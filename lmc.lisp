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

(defun opcodep (num)
  "Determines if the opcode is legal."
  (or (zerop num)
      (<= 100 num 899)
      (member '(910 902))))

(defun do-opcode (num)
  "Performs an action given an instruction.
Where O is the operation and XX is the mailbox index.
In the form of OXX. Steps 4 and 5 in the instructions."
  (if (opcodep num)
      (let ((mailbox (mailbox-from-opcode num)))
        (incf *instruction-counter*)
        (cond
          ((zerop num) (stop))
          ((<= 100 num 199) (add mailbox))
          ((<= 200 num 299) (sub mailbox))
          ((<= 300 num 399) (sto mailbox))
          ((<= 400 num 499) (sta mailbox))
          ((<= 500 num 599) (lod mailbox))
          ((<= 600 num 699) (b mailbox))
          ((<= 700 num 799) (bz mailbox))
          ((<= 800 num 899) (bp mailbox))
          ((= 910 num) (red))
          ((= 902 num) (prt))))
      (format t "Bad opcode: ~a. Ignored." num)))

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

(defun b (mailbox)
  "This instruction sets the instruction counter to the number xx,
thus effectively branching to mailbox xx See the note for instruction
BP"
  (setf *instruction-count* mailbox))

(defun bz (mailbox)
  "IF the calculator value is zero, THEN set the instruction counter
to the number xx, thus effectively branching to mailbox xx. See the
 note for instruction BP"
  (when (zerop *calculator*)
    (setf *instruction-count* mailbox)))w

(defun bp (mailbox)
  "IF the calculator value is positive, THEN set the instruction
 counter to the number xx, thus effectively branching to mailbox xx.
 NOTE: zero is considered positive."
  (when (>= *calculator* 0)
    (setf *instruction-count* mailbox)))

(defun red ()
  (setf *calculator* (pop *inbox*)))

(defun prt ()
  (setf *outbox*
        (reverse (cons *calculator* (reverse *outbox*)))))

(defun main ()
  (while (not *finished*)
         ;; Goto mailbox with instruction counter
         (let ((mailbox-value (get-mailbox *instruction-counter*)))
           (do-opcode mailbox-value))))
