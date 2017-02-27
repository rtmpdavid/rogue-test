(in-package #:rogue-test)

(defun in-range (n origin range)
  (<= origin n (+ origin range)))

(defun x (pos)
  (first pos))

(defun (setf x) (value seq)
  (setf (first seq) value))

(defun y (pos)
  (second pos))

(defun (setf y) (value seq)
  (setf (second seq) value))

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

(defun clamp (n a b)
  (max (min n b) a))

(defmacro compose (n-args &rest predicates)
  (let ((args (loop for i from 0 to (1- n-args)
	      	    collect (gensym))))
    `#'(lambda ,args
	 (and ,@(loop for pred in predicates
		      collect `(funcall ,pred ,@args))))))

(defun empty-p (sequence)
  (zerop (length sequence)))
