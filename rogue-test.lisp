;;;; rogue-test.lisp

(in-package #:rogue-test)

;;; "rogue-test" goes here. Hacks and glory await!

#|
(ql:quickload :rogue-test)
(rogue-test::start-with-swank)
|#

(defvar *ticks* 0)

(defvar *screen* nil)
(defparameter *player* nil)
(defparameter *camera* nil)
(defparameter *entities* nil)

(defun player-p (entity)
  (eq *player* entity))

(defmacro continuable (&body body)
  `(restart-case (progn ,@body)
     (continue () :report "Continuable: Continue")))

(defparameter *player* nil)
(defparameter *camera* nil)
(defparameter *entities* nil)

(defun char+ (char n)
  (code-char (+ (char-code char) n)))

(defun selector-n-to-char (n)
  (if (< n 32) (char+ #\a n)
      (char+ #\A n)))

(defun selector-char-to-n (char page things)
  (when (or (upper-case-p char) (lower-case-p char))
    (let ((n (+ (- (char-code char) (if (upper-case-p char) (char-code #\A)
					(char-code #\a)))
		(* page (- (getmaxy *screen*) 2)))))
      (when (< n (length things))
	n))))

(defun draw-selector (things begin string)
  (clear)
  (move 0 0)
  (addstr string)
  (when (> begin 0) (addstr " [ - scroll up; "))
  (when (> (- (length things) begin) (- (getmaxy *screen*) 2))
    (addstr "] - scroll down"))
  (loop for thing in (nthcdr begin things)
	for i from 1 to (- (getmaxy *screen*) 3)
	do (move i 0)
	   (addstr (format nil "~a - ~a"
			   (selector-n-to-char (1- i))
			   thing)))
  (move (- (getmaxy *screen*) 2) 0)
  (addstr "Space - cancel")
  (refresh))

(defun selector-loop (things string cancel)
  (let ((page 0)
	(selection nil))
    (loop with input
	  until selection
	  finally (return selection)
	  do (draw-selector things (* page (- (getmaxy *screen*) 2)) string)
	     (setf input (code-char (getch)))
	     (case input
	       (#\[ (when (> page 0)
		      (decf page)))
	       (#\] (when (> (- (length things) (* page (getmaxy *screen*)))
			     (- (getmaxy *screen*) 2))
		      (incf page)))
	       (#\Space (progn (setf *echo* cancel)
			  (return nil)))
	       (t (setf selection
			(selector-char-to-n input page things)))))))

(defmacro selector ((things selection &optional
					(question-prompt "select thing")
					(cancel "Cancel")
					(empty-echo "nothing to select")
					(force-display nil)) &body body)
  (let ((question (gensym))
	(empty (gensym))
	(len (gensym)))
    `(let ((,len (length ,things))
	   (,question ,question-prompt)
	   (,empty ,empty-echo))
       (let ((selection (cond ((= 0 ,len) (prog1 nil
					    (setf *echo* ,empty)))
			      (,force-display (selector-loop ,things ,question ,cancel))
			      ((= 1 ,len) 0)
			      (t (selector-loop ,things ,question ,cancel)))))
	 (when ,selection
	   ,@body)))))

(defun entity-selector (things &optional (string "select") (force-display nil))
  (selector ((mapcar #'entity-uniq-name things) selection
	     (format nil "What to ~a?" string) "Cancel"
	     (format nil "Nothing to ~a" string) force-display)
    (elt things selection)))

(defun y-or-n-prompt (string)
  (setf *echo* (format nil "Really ~a? (y/n)" string))
  (refresh-screen)
  (char= #\y (code-char (getch))))

(defun direction-prompt (string)
  (let ((answer nil))
    (loop until answer
	  do (setf *echo* (format nil "In which direction to ~a? (space to cancel)" string))
	     (refresh-screen)
	     (let* ((input (getch))
		    (parsed (parse-movement input)))
	       (if parsed (setf answer parsed)
		   (when (char= (code-char input) #\Space)
		     (setf answer :cancel)))))
    (when (direction-p answer) answer)))

(defun draw-field (screen)
  (let* ((w (getmaxx screen))
	 (h (getmaxy screen)))
    (loop for i from (- 5 (mod (entity-x *camera*) 5)) to (1- w) by 5
	  do (loop for j from (- 5 (1+ (mod (entity-y *camera*) 5))) to (1- h) by 5
		   do (move j i)
		      (addch (char-code #\.))))))

(defun draw-entities (screen)
  (let* ((w (getmaxx screen))
	 (h (getmaxy screen))
	 (x (- (entity-x *camera*) (truncate w 2)))
	 (y (- (entity-y *camera*) (truncate h 2))))
    (loop for entity in *entities*
	  do (entity-draw entity *camera* (list w h)))))

(defparameter *echo* nil)
(defvar *action* :step)

(defun refresh-screen ()
  (clear)
  (draw-level *level* (entity-position *camera*) (list (getmaxx *screen*) (getmaxy *screen*)))
  (draw-entities *screen*)
  (move 0 0)
  (addstr (format nil "~a: ~a/~a ATK:~a"
		  (entity-uniq-name *player*)
		  (entity-hp *player*)
		  (entity-hp-max *player*)
		  (actor-attack *player*)))
  ;; (move (- (getmaxy *screen*) 2) 0)
    (move (1- (getmaxy *screen*)) 0)
  (when *echo*
    (addstr *echo*)    
    (setf *echo* nil))
  (refresh))

(defvar *level* nil)

(defun main-step ()  
  (loop for entity in *entities*
	do (entity-step entity))
  (refresh-screen)
  (incf *ticks*))

(defun main-loop ()
  (noecho)
  (start-color)
  (init-pair 1 color_blue color_red)
  (init-pair 2 color_white color_black)
  (init-pair 3 color_black color_red)
  (init-pair 4 color_red color_black)
  (setf *player* (make-instance 'player :name "david" :char #\@
				:attack "2d8+5"))
  (setf *camera* *player*)
  (push *player* *entities*)
  (push  (make-instance 'monster :name "goblin" :char #\g :x 10 :behaviour :chaser) *entities*)
  (setf *ticks* 0)
  (setf *level* (generate-topology 2))
  (loop
    (continuable (main-step))))

(defun start-with-swank ()
  (swank-loader:init)
  (let ((port 4005))
    (handler-case (swank:create-server :port port :style :spawn :dont-close t)
      (SB-BSD-SOCKETS:ADDRESS-IN-USE-ERROR () (swank:create-server :port (incf port) :style :spawn :dont-close t))))
  (setf *screen* (initscr))
  (setf *entities* nil)
  (main-loop))


(trivial-signal::signal-handler-bind ((:winch (lambda (signo)
						(refresh-screen)))))
