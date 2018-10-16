(in-package #:rogue-test)

(defun in-range (n origin range)
  (<= origin n (+ origin range)))

(defun p (x y)
  (list x y))

(defun x (pos)
  (first pos))

(defun (setf x) (value seq)
  (setf (first seq) value))

(defun y (pos)
  (second pos))

(defun (setf y) (value seq)
  (setf (second seq) value))

(defun pos+ (a b)
  (list (+ (x a) (x b))
	(+ (y a) (y b))))

(defun pos- (a b)
  (list (- (x a) (x b))
	(- (y a) (y b))))

(defun w (dim)
  (first dim))

(defun (setf w) (value seq)
  (setf (first seq) value))

(defun h (dim)
  (second dim))

(defun (setf h) (value seq)
  (setf (second seq) value))

(defun half-w (dims)
  (truncate (w dims) 2))

(defun half-h (dims)
  (truncate (h dims) 2))

(defmacro compose-pred (n-args &rest predicates)
  (let ((args (loop for i from 0 to (1- n-args)
	      	    collect (gensym))))
    `#'(lambda ,args
	 (and ,@(loop for pred in predicates
		      collect `(funcall ,pred ,@args))))))

(defun empty-p (sequence)
  (zerop (length sequence)))

(defun positive-p (number)
  (> 0 number))

(defun negative-p (number)
  (< 0 number))

;; (defun prog-or (&rest forms)
;;   "evaluate forms in order, then return with non-nil if at least one of the forms has returned non-nil"
;;   )

(defun direction-delta (direction)
  (case direction
    (:up (list 0 -1))
    (:down (list 0 1))
    (:left (list -1 0))
    (:right (list 1 0))
    (:up-left (list -1 -1))
    (:up-right (list 1 -1))
    (:bot-left (list -1 1))
    (:bot-right (list 1 1))
    (:nop (list 0 0))))

(defun direction-opposite (direction)
  (case direction
    (:up :down)
    (:down :up)
    (:left :right)
    (:right :left )
    (:up-left :bot-right)
    (:up-right :bot-left)
    (:bot-left :up-right )
    (:bot-right :up-left)))

(defvar *cardinal* '(:up :down :left :right))
(defvar *diagonal* '(:up-left :bot-right :up-right :bot-left))
(defvar *dirs-nonzero* (append *cardinal* *diagonal*))
(defvar *dirs* (append (list :nop) *dirs-nonzero*))

(defun pos+dir (pos dir)
  (pos+ pos (direction-delta dir)))

(defun pos-dir (pos dir)
  (pos- pos (direction-delta dir)))

(defun random-range (a b)
  (if (= a b) a
      (progn (assert (< a b))
	     (+ (random (1+ (- b a))) a))))
