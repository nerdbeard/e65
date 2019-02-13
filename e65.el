;;; e65-bus

(defstruct e65-bus devices)

(defun e65-add-device (bus device)
  (assert (not (e65-device-bus device)))
  (push device (e65-bus-devices bus))
  (setf (e65-device-bus device) bus))

(defun e65-bus-emit (bus &rest signals)
  (dolist (receiver (e65-bus-devices bus))
    (funcall (e65-device-signal-function receiver) receiver signals)))

;;; e65-device

(defstruct e65-device
  bus
  ;; Device-specific use
  state
  ;; Transforms address space and implements chip select logic
  (map-function (lambda (device addr) nil))
  ;; Receives write ops via transformed address
  (write-function (lambda (device addr data) nil))
  ;; Read requests
  (read-function (lambda (device addr) nil))
  ;; Other signal lines
  (signal-function (lambda (device signals) nil)))

;; Transmit a byte from a device to the other devices on the same bus
;; if their `map-function` is interested in `addr`.
(defun e65-device-write (device addr data)
  (let ((bus (e65-device-bus device)))
    (dolist (target (e65-bus-devices bus))
      (and (not (eq device target))
           (let ((addr (funcall (e65-device-map-function target)
                                target addr)))
             (and addr (funcall (e65-device-write-function target)
                                target addr data)))))))

;; Allow device to read from the bus by delegating to every
;; interested device attached to the bus except the reader
(defun e65-device-read (device addr)
  (let ((bus (e65-device-bus device))
        values)
    (dolist (read-device (e65-bus-devices bus))
      (and (not (eq device read-device))
           (let* ((addr (funcall (e65-device-map-function read-device)
                                 read-device addr))
                  (data (and addr (funcall (e65-device-read-function read-device)
                                           read-device addr))))
             (and data (push data values)))))
    (cond
     ((not values) #x0000)              ; Or is it #xffff?
     ((cdr values) (error "Bus conflict; multiple responses to single read"))
     (t (car values)))))

;; Emit a signal to all other devices
(defun e65-device-emit (device signals)
  (dolist (receiver (e65-bus-devices (e65-device-bus device)))
    (and (not (eq receiver device))
         (funcall (e65-device-signal-function receiver) receiver signals))))

;;; e65-6264 SRAM

(defun e65-6264-write (device addr data)
  (aset (e65-device-state device)
        (logand addr #x1fff)            ; Only 13 address pins
        (logand data #xff)))

(defun e65-6264-read (device addr)
  (aref (e65-device-state device)
        (logand addr #x1fff)))          ; Only 13 address pins

(defun make-e65-6264 (map-function)
  (make-e65-device :state (make-vector #x2000 #x00)
                   :map-function map-function
                   :write-function 'e65-6264-write
                   :read-function 'e65-6264-read))

(defun e65-display-bank (data)
  (switch-to-buffer "e65-6264 contents")
  (delete-region (point-min) (point-max))
  (dotimes (i (length data))
    (when (= 0 (mod i 16))
      (insert (format "%04x: " i)))
    (insert (format "%02x " (aref data i)))
    (when (= 0 (mod (1+ i) 16))
      (insert "\n"))
    (when (= 0 (mod (1+ i) 256))
      (insert "\n"))))

;;; e65-rom

(defun make-e65-rom-from-file (filename map-function)
  (let ((data (vector (with-temp-buffer
                        (insert-file-contents-literally filename)
                        (buffer-string)))))
    (make-e65-rom data map-function)))

(defun make-e65-rom (data map-function)
  (make-e65-device :state data
                   :map-function map-function
                   :write-function (lambda (device addr data) nil)
                   :read-function (lambda (device addr)
                                    (aref (e65-device-state device) addr))))

;;; e65-6502

(defun make-e65-6502 ()
  (make-e65-device
   :state (make-nes/cpu :interrupt (make-nes/interrupt))
   :signal-function #'e65-6502--signal))

(defun e65-6502--reset (device)
  (let ((state (e65-device-state device)))
    ;; Three cycles of suppressed PUSHes
    ;; ...
    ;; Copy RESET vector to PC
    (setf (nes/cpu-register->pc
           (nes/cpu->register
            (e65-device-state device)))
          (logior (e65-device-read device #xfffc)
                  (lsh (e65-device-read device #xfffd) 8)))))

(defun e65-6502--signal (device signals)
  (cond
   ((member :reset signals)
    (e65-6502--reset device))
   ((member :phi0 signals)
    ;; Occlude nes/cpu--bus-read and nes/cpu--bus-write
    (cl-letf (((symbol-function 'nes/cpu--bus-read)
               (lambda (C addr) (e65-device-read device addr)))
              ((symbol-function 'nes/cpu--bus-write)
               (lambda (C addr data) (e65-device-write device addr data))))
      (nes/cpu-step (e65-device-state device))))))

;;; Scratch

(defun e65-bus-tick (bus)
  (e65-bus-emit bus :phi0))

(defun e65-scratch ()
  (let ((bus (make-e65-bus))
        (cpu (make-e65-6502))
        (ram (make-e65-6264
              (lambda (device addr)
                (and (> #x2000 addr) addr))))
        (rom (make-e65-rom
              (e65-compile-rom)
              (lambda (device addr)
                (and (<= #xe000 addr)
                     (logand #x1fff addr))))))
    ;; Install CPU
    (e65-add-device bus cpu)
    ;; Install 8K RAM at 0x0000-0x1fff
    (e65-add-device bus ram)
    ;; Install a ROM in upper 8K
    (e65-add-device bus rom)
    ;; Crank it
    (e65-bus-emit bus :reset)
    (e65-init-trace cpu)
    (dotimes (i 32)
      (e65-bus-tick bus)
      (e65-trace cpu))
    (e65-display-bank (e65-device-state ram))))

(defun e65-init-trace (cpu)
  (switch-to-buffer "e65-trace")
  (delete-region (point-min) (point-max))
  (e65-trace cpu))

(defun e65-trace (cpu)
  (switch-to-buffer "e65-trace")
  (goto-char (point-max))
  (let ((r (nes/cpu->register (e65-device-state cpu))))
    (insert (format "PC:%04x A:%02x X:%02x Y:%02x SP:%04x SR:%02x\n"
                    (nes/cpu-register->pc r)
                    (nes/cpu-register->acc r)
                    (nes/cpu-register->idx-x r)
                    (nes/cpu-register->idx-y r)
                    (nes/cpu-register->sp r)
                    (logior (lsh (if (nes/cpu-register->sr-negative r)  1 0) 7)
                            (lsh (if (nes/cpu-register->sr-overflow r)  1 0) 6)
                            (lsh (if (nes/cpu-register->sr-reserved r)  1 0) 5)
                            (lsh (if (nes/cpu-register->sr-break r)     1 0) 4)
                            (lsh (if (nes/cpu-register->sr-decimal r)   1 0) 3)
                            (lsh (if (nes/cpu-register->sr-interrupt r) 1 0) 2)
                            (lsh (if (nes/cpu-register->sr-zero r)      1 0) 1)
                            (lsh (if (nes/cpu-register->sr-carry r)     1 0) 0))))))

(defun e65-compile-rom ()
  (setq comfy-mem (make-vector #x10000 0))
  (comfy-init)
  ;; I think we are building from 0xffff downward, so the first byte
  ;; to emit is the low byte of the reset vector IIRC followed by NMI
  ;; and IRQ in some order or another.  But, we don't have the
  ;; addresses of our routines yet, so just emit place holder.
  (comfy-compile '(seq 0 0 0 0 0 0) 0 0)
  (let ((interupt-vector (comfy-compile 'resume 0 0)) ; resume on all interupts
        (reset-vector (comfy-genbr 0)))               ; loop forever
    (comfy-ra reset-vector reset-vector)
    (let ((il (logand #xff interupt-vector))
          (ih (lsh interupt-vector -8))
          (rl (logand #xff reset-vector))
          (rh (lsh reset-vector -8)))
      (aset comfy-mem #xfffa il)        ; NMI
      (aset comfy-mem #xfffb ih)
      (aset comfy-mem #xfffc rl)        ; RESET
      (aset comfy-mem #xfffd rh)
      (aset comfy-mem #xfffe il)        ; IRQ
      (aset comfy-mem #xffff ih)))
  ;; Return last 8Kb as rom image
  (subseq comfy-mem #xe000))
