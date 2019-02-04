(defstruct e65-bus devices)

(defun e65-add-device (bus device)
  (assert (not (e65-device-bus device)))
  (push device (e65-bus-devices bus))
  (setf (e65-device-bus device) bus))

(defun e65-bus-emit (bus &rest signals)
  (dolist (receiver (e65-bus-devices bus))
    (funcall (e65-device-signal-function receiver) receiver signals)))

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
      (and (not (= device target))
           (let ((addr (funcall (e65-device-map-function target)
                                target addr)))
             (and addr (funcall (e65-device-write-function target)
                                addr data)))))))

;; Allow device to read from the bus by delegating to every
;; interested device attached to the bus except the reader
(defun e65-device-read (device addr)
  (let ((bus (e65-device-bus device))
        values)
    (dolist (read-device (e65-bus-devices bus))
      (and (not (= device read-device))
           (let* ((addr (funcall (e65-device-map-function read-device)
                                 read-device addr))
                  (data (and addr (funcall (e65-device-read-function
                                            read-device) addr))))
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

(defun e65-6264-write (device addr data)
  (aset (e65-6264-state device)
        (logand addr #x01ff)            ; Only 13 address pins
        (logand data #xff)))

(defun e65-6264-read (device addr)
  (aref (e65-6264-state device)
        (logand addr #x01ff)))          ; Only 13 address pins

(defun make-e65-6264 (map-function)
  (make-e65-device :state (make-vector #x01ff #x00)
                   :map-function map-function
                   :write-function 'e65-6264-write
                   :read-function 'e65-6264-read))

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

(defun e65-scratch ()
  (let ((bus (make-e65-bus)))
    ;; Install 8K RAM at 0x0000-0x1fff
    (e65-add-device bus (make-e65-6264
                         (lambda (addr)
                           (and (> #x2000 addr) addr))))
    ;; Install a ROM in upper 8K
    (e65-add-device bus (make-e65-rom-from-file
                         "random"
                         (lambda (addr)
                           (and (<= #xe000 addr)
                                (logand #x01ff addr)))))
    ;; Crank it
    (e65-bus-emit bus :phi0)))
