;;;; lmc.lisp
;;;; implementation taken from http://povinelli.eece.mu.edu/teaching/eece2710/lmc.html#A%20Sample%20LMC%20Program

(in-package #:lmc)

;; TODO get rid of this global crap make the LMC contained

(defparameter *mailboxes* (make-array 100 :initial-element 0))
(defparameter *inbox* '())
(defparameter *outbox* '())
(defparameter *calculator* 0)
(defparameter *instruction-counter* 0)
(defparameter *negative-flag* nil)
(defparameter *finished* nil)

(defun reset ()
  (setf *mailboxes* (make-array 100 :initial-element 0)
        *inbox* '()
        *outbox* '()
        *calculator* 0
        *instruction-counter* 0
        *negative-flag* nil
        *finished* nil))

(reset)

(defun mailbox-from-opcode (opcode)
  "Given an opcode aka 560 returns mailbox 60."
  (mod opcode 100))

(defun opcodep (num)
  "Determines if the opcode is legal."
  (or (<= 100 num 899)
      (member num '(0 901 902))))

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
          ((= num 901) (red))
          ((= num 902) (prt))))
      (progn
        (format t "Bad opcode: ~a. Halting!." num)
        (setf *finished* t))))


(defun set-mailbox-f (mailbox value)
  "Setter for the *mailboxes* array."
  (setf (aref *mailboxes* mailbox) value))

(defun get-mailbox (mailbox)
  "Getter for the *mailboxes* array."
  (aref *mailboxes* mailbox))

;;; Opcode routines

(defun button ()
  "Starts the Computer - pokes little man in the ribs.."
  (setf *finished* nil))

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
    (setf *instruction-count* mailbox)))

(defun bp (mailbox)
  "IF the calculator value is positive, THEN set the instruction
 counter to the number xx, thus effectively branching to mailbox xx.
 NOTE: zero is considered positive."
  (when (>= *calculator* 0)
    (setf *instruction-count* mailbox)))

(defun red ()
  "Read"
  (setf *calculator* (pop *inbox*)))

(defun prt ()
  "print calculator value to outbox."
  (setf *outbox* (reverse (cons *calculator* (reverse *outbox*)))))

(defun main ()
  "Driver that processes initial input state."
  (loop :until *finished*
        :do (do-opcode (get-mailbox *instruction-counter*))))

(defun load-boxes (start ops)
  "Given a starting index loads all ops in sequentially."
  (loop :for i :from start
        :for op :in ops
        :do (setf (aref *mailboxes* i) op)))

(defun pretty-print-boxes ()
  "Print the boxes in a 10 x 10 table."
  (loop :with line
        :for i :below (length *mailboxes*) :by 10
        :do (setf line (coerce (subseq *mailboxes* i (+ i 9)) 'list))
            (format t "~{~4d~}~%" line)))

(defun sample-program ()
  (reset)
  ;; We put 2 ops to be added in the inbox (rather than in boxes)
  (setf *inbox* `(100 200))
  ;; Enter the operations into the boxes
  (load-boxes 0 `(901 306 901 106 902 000))
  (pretty-print-boxes)
  ;; Run the main program
  (main)
  ;; verify that the sum is correct
  (print *outbox*))
