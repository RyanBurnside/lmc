;;;; lmc.lisp
;;;; implementation taken from http://povinelli.eece.mu.edu/teaching/eece2710/lmc.html#A%20Sample%20LMC%20Program

(in-package #:lmc)

(defparameter *mailboxes* (make-array 100 :initial-element 0))
(defparameter *inbox* '())  ; FIFO BASKET OF SLIPS
(defparameter *outbox* '()) ; FIFO BASKET OF SLIPS

(defparameter *calculator* 0)
(defparameter *instruction-counter* 0)
(defparameter *negative-flag* nil)
(defparameter *finished* nil)

(defun mailbox-from-opcode (opcode)
  "Given an opcode aka 560 returns mailbox 60."
  (assert (<= 0 opcode 99))
  (mod opcode 100))

(defun do-opcode (num)
  "Performs an action given an instruction.
Where O is the operation and XX is the mailbox index.
In the form of OXX."
  (if (<= 0 num 999)
      (let ((mailbox (mailbox-from-opcode num)))
        (cond
          ((zerop num) (stop))
          ((<= 100 num 199) (add mailbox))
          ((<= 200 num 299) (sub mailbox))
          ((<= 300 num 399) (sto mailbox))
          ((<= 400 num 499) (sta mailbox))
          ((<= 500 num 599) (lod mailbox))
          ((<= 600 num 699) 'B)
          ((<= 700 num 799) 'BZ)
          ((<= 800 num 899) 'BP)
          ((= 910 num) 'READ)
          ((= 902 num) 'PRINT)))
      (format t "ERROR opcode ~a is not between 0 and 999!" num)))

(defun set-mailbox-f (mailbox value)
  "Setter for the *mailboxes* array."
  (setf (aref *mailboxes* mailbox) value))

(defun get-mailbox (mailbox)
  "Getter for the *mailboxes* array."
  (aref *mailboxes* mailbox))

;;; Opcode routines

(defun stop ()
  "Stops the Computer - the Little Man rests."
  (setf *finished* t))

(defun add (mailbox)
  "Adds the contents of mailbox xx to the calculator display."
  (incf *calculator* (get-mailbox mailbox)))

(defun sub (mailbox)
  "Subtracts the contents of mailbox xx from the calculator display."
  (decf *calculator* (get-mailbox mailbox))
  (setf *negative-flag* (minusp *calculator*)))

(defun sto (mailbox)
  "Stores the calculator value into mailbox xx."
  (set-mailbox-f mailbox *calculator*))

(defun opcode-part (opcode)
  "Returns the 100s version of an opcode."
  (floor (/ opcode 100)))

(defun sta (mailbox)
  "Stores the address portion of the calculator value (last 2 digits)
into the address portion of the instruction in mailbox xx."
  (let ((calc-mailbox (mailbox-from-opcode *calculator*))
        (mailbox-opcode-part (get-mailbox mailbox)))
    (set-mailbox-f mailbox (+ calc-mailbox mailbox-opcode-part))))

(defun lod (mailbox)
  "Loads the contents of mailbox xx into the calculator."
  (setf *calculator* (get-mailbox mailbox)))
