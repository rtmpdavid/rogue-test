(in-package #:rogue-test)

;;; Dice rolla'
;;; I want to be able to type in liek 2d4+3 and get something meaningful
;;; riiiight
;;; 
;;; (roll 2d4+3+4d6)
;;; (roll "2d4+3+4d6)
;;; (roll 2d4 34d6)
;;; #D2d4+3+4d6

(defun transform-die (die)
  (if (integerp die) die
      (append
       (list '+ (car die))
       (loop for i from 1 to (car die)
	     collect (list 'random (cdr die))))))

(defun parse-die (die)
  (let ((split (split-sequence:split-sequence #\d die)))
    (if (= (length split) 1) (parse-integer (car split))
	(if (> (length split) 2) (error (format nil "~a is not a valid die" die))
	    (cons (if (empty-p (first split)) 1
		      (parse-integer (first split)))
		  (parse-integer (second split)))))))

(defmacro roll (dice)
  `(+ ,@(loop for die in (split-sequence:split-sequence #\+ (string-downcase (symbol-name dice)))
	      collect (transform-die (parse-die die)))))


(defun roll-die (die)
  (if (integerp die) die
      (+ (car die)
	 (loop for i from 1 to (car die)
	       summing (random (cdr die))))))

(defun roll-dice (dice)
  (loop for die in (split-sequence:split-sequence #\+ dice)
	summing (roll-die (parse-die die))))
