(in-package #:rogue-test)

(defun in-range (n origin range)
  (<= origin n (+ origin range)))

(defun x (pos)
  (first pos))

(defun y (pos)
  (second pos))

(defun w (dim)
  (first dim))

(defun h (dim)
  (second dim))


(defmacro compose (n-args &rest predicates)
  (let ((args (loop for i from 0 to (1- n-args)
	      	    collect (gensym))))
    `#'(lambda ,args
	 (and ,@(loop for pred in predicates
		      collect `(funcall ,pred ,@args))))))
