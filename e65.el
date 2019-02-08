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


(defun e65-display-6264 (ram)
  (switch-to-buffer "e65-6264 contents")
  (delete-region (point-min) (point-max))
  (let ((data (e65-device-state ram)))
    (dotimes (i #x2000)
      (when (= 0 (mod i 16))
        (insert (format "%04x: " i)))
      (insert (format "%02x " (aref data i)))
      (when (= 0 (mod (1+ i) 16))
        (insert "\n"))
      (when (= 0 (mod (1+ i) 256))
        (insert "\n")))))

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
    (setf (e65-6502-state-pc state)
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
        (ram (make-e65-6264
              (lambda (device addr)
                (and (> #x2000 addr) addr)))))
    ;; Install CPU
    (e65-add-device bus (make-e65-6502))
    
    ;; Install 8K RAM at 0x0000-0x1fff
    (e65-add-device bus ram)
    ;; Install a ROM in upper 8K
    (e65-add-device bus (make-e65-rom
                         (make-vector #x2000 #x00)
                         (lambda (device addr)
                           (and (<= #xe000 addr)
                                (logand #x01ff addr)))))
    ;; Crank it
    (dotimes (i 3) (e65-bus-tick bus))
    ;; Show the output; note the result of executing three BRK
    ;; instructions in the stack in page one!
    (e65-display-6264 ram)))
