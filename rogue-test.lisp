;;;; rogue-test.lisp

(in-package #:rogue-test)

;;; "rogue-test" goes here. Hacks and glory await!

#|
(ql:quickload :rogue-test)
(rogue-test::start-with-swank)
|#

(defvar *screen*)

(defmacro continuable (&body body)
  `(restart-case (progn ,@body)
     (continue () :report "Continuable: Continue")))

(defun parse-movement (input)
  (case (code-char input)
    (#\h :left)
    (#\j :down)
    (#\k :up)
    (#\l :right)
    (#\y :up-left)
    (#\u :up-right)
    (#\b :bot-left)
    (#\n :bot-right)
    (#\. :nop)))

(defun movement-p (input)
  (member (code-char input) (list #\h #\j #\k #\l #\.)))

(defun action-p (input)
  (member (code-char input) (list #\p #\A)))

(defun direction-p (thing)
  (member thing (list :up :down :left :right :up-left :up-right :bot-left :bot-right :nop)))

(deftype direction ()
  '(satisfies direction-p))

(defclass entity ()
  ((position-x :accessor ent-x
	       :initform 0
	       :initarg :x)
   (position-y :accessor ent-y
	       :initform 0
	       :initarg :y)
   (representation :accessor ent-char
		   :initform #\0
		   :initarg :char
		   :type (or character nil))
   (visible :accessor ent-visible
	    :initform t
	    :initarg :visible
	    :type boolean)
   (name ;; :allocation :class
    :accessor ent-name
    :initarg :name
    :initform "Base entity")
   (id :accessor ent-id)))

(defclass pushable (entity)
  ((name :initform "Pushable entity")))

(defclass boulder (pushable)
  ((name :initform "Boulder")))

(defun pushable-p (entity)
  (typep entity 'pushable))

(defclass destructable (entity)
  ((name :initform "Destructable entity")   
   (hitpoints-max :initform 100
		  :initarg :hp
		  :accessor ent-hp-max)
   (hitpoints :initform nil
	      :initarg :hp-current
	      :accessor ent-hp)
   (took-damage :initform nil
		:accessor ent-took-damage)))

(defmethod initialize-instance :after ((entity destructable) &rest args)
  (declare (ignore args))
  (when (not (ent-hp entity))
    (setf (ent-hp entity) (ent-hp-max entity))))

(defgeneric take-damage (destructable damage))
(defmethod take-damage ((destructable destructable) damage)
  (if (zerop (ent-hp destructable)) nil
      (setf (ent-hp destructable) (max 0 (- (ent-hp destructable) damage))
	    (ent-took-damage destructable) t)))

(defun destructable-p (entity)
  (typep entity 'destructable))

(defun nop ())

(defclass actor (entity)
  ((name :initform "Actor")
   (attack :initform 10
	   :initarg :attack
	   :accessor actor-attack)))

(defun actor-p (entity)
  (typep entity 'actor))

(defgeneric alive-p (actor))
(defmethod alive-p ((actor actor))
  (typecase actor
    (destructable (not (zerop (ent-hp actor))))
    (t nil)))

(defgeneric enemy-p (actor-a actor-b))
(defmethod enemy-p :around (a b)
  (if (eq a b) nil
      (call-next-method a b)))
(defmethod enemy-p ((actor-a actor) (actor-b actor))
  nil)

(defclass monster (actor pushable destructable)
  ((name :initform "Monster")
   (behaviour :initform nil
	      :initarg :behaviour
	      :accessor behaviour)
   (fraction :initform nil
	     :initarg :fraction
	     :accessor fraction)
   (target :initform nil
	   :initarg :target
	   :accessor target)))

(defun monster-p (entity)
  (typep entity 'monster))

(defmethod take-damage ((monster monster) damage)
  (let ((val (call-next-method)))
    (when (and val (not (alive-p monster)))
      (setf (behaviour monster) :nop))
    val))

(defun ent-distance (a b)
  (abs (sqrt (+ (expt (- (ent-x a) (ent-x b)) 2)
		(expt (- (ent-y a) (ent-y b)) 2)))))

(defun find-target (monster)
  (let ((new-target (first (sort (remove-if-not #'(lambda (a) (enemy-p monster a)) *entities*)
				 #'(lambda (a b) (< (ent-distance monster a)
						    (ent-distance monster b)))))))
    (setf (target monster) new-target)
    (when (and (monster-p new-target)
	       (not (target new-target)))
      (setf (target new-target) monster))))

(defun find-targets ()
  (let ((actors (remove-if-not #'actor-p *entities*)))
    (loop for actor in actors
	  do (when (and (monster-p actor)
			(not (target actor))))
	     (let ((new-target (first (sort (remove-if-not #'(lambda (a) (enemy-p actor a)) actors)
					    #'(lambda (a b) (< (ent-distance actor a)
							       (ent-distance actor b)))))))
	       (setf (target actor) new-target)
	       (when (and (monster-p new-target)
			  (not (target new-target)))
		 (setf (target new-target) actor))))))

(defmethod entity-step ((monster monster))
  (case (behaviour monster)
    (:chaser
     (with-slots (target) monster
       (when (and target (alive-p target))
	 (let ((delta (list (- (ent-x target) (ent-x monster))
			    (- (ent-y target) (ent-y monster)))))
	   (if (adjacent-p monster target)
	       (entity-interact monster target :attack nil nil)
	       (move-entity monster (list (+ (ent-x monster) (signum (first delta)))
					  (+ (ent-y monster) (signum (second delta)))))))
	 (when (and (not (alive-p target))
		    (eq monster *camera*))
	   (push (ent-char target) *kills*)))       
       (when (or (and target
		      (not (alive-p target)))
		 (not target))
	 (find-target monster)))
     

     

     ;; (if (find-if #'(lambda (a)
     ;; 		      (and (alive-p a)
     ;; 			   (enemy-p monster a)))
     ;; 		  (adjacent-entities monster))
     ;; 	 (entity-interact monster (car (adjacent-entities monster))
     ;; 			  :attack nil nil)
     ;; 	 ;; (let ((enemies (sort (remove monster (remove-if-not
     ;; 	 ;; 				       #'(lambda (a)
     ;; 	 ;; 					   (and (alive-p a)
     ;; 	 ;; 						(enemy-p monster a)))
     ;; 	 ;; 				       *entities*))
     ;; 	 ;; 		      #'(lambda (a b) (< (ent-distance monster a)
     ;; 	 ;; 					 (ent-distance monster b))))))
     ;; 	 ;;   (when enemies
     ;; 	 ;;     (let ((delta (list (- (ent-x (first enemies)) (ent-x monster))
     ;; 	 ;; 			(- (ent-y (first enemies)) (ent-y monster)))))
     
     ;; 	 ;;       (move-entity monster (list (+ (ent-x monster) (signum (first delta)))
     ;; 	 ;; 				  (+ (ent-y monster) (signum (second delta))))))))
     ;; 	 )
     )
    (:nop nil)))

(defclass player (actor pushable destructable)
  ((ent-char :initform #\@)))

(defun player-p (entity)
  (eq *player* entity))

(defun parse-action (input)
  (case (code-char input)
    (#\p (let ((direction (direction-prompt "push")))
	   (when direction
	     (let ((target (entity-selector (entity-at-direction *player* direction))))
	       (when target
		 (entity-interact *player* target :push direction nil))))))
    (#\A (let ((targets (adjacent-entities *player*)))
	   (let ((selection (entity-selector targets "attack")))
	     (when selection (entity-interact *player* selection :attack nil nil)))))))

(defmethod entity-step ((player player))
  ;; (let ((input (getch)))
  ;;   (cond
  ;;     ((movement-p input) (move-entity player (parse-movement input)))
  ;;     ((action-p input) (parse-action input))))
  )

(defmethod enemy-p ((a player) (b monster))
  t)

(defmethod enemy-p ((a monster) (b player))
  t)

(defmethod enemy-p ((a monster) (b monster))
  (or (not (fraction a))
      (not (fraction b))
      (not (eq (fraction a)
	       (fraction b)))))

(defgeneric ent-uniq-name (entity))
(defmethod ent-uniq-name ((entity entity))
  (format nil "~a №~a" (ent-name entity) (ent-id entity)))
(defmethod ent-uniq-name ((entity player))
  (ent-name entity))

(defparameter *player* nil)
(defparameter *camera* nil)
(defparameter *entities* nil)

(defun adjacent-p (entity-a entity-b)
  (and (<= (abs (- (ent-x entity-a) (ent-x entity-b))) 1)
       (<= (abs (- (ent-y entity-a) (ent-y entity-b))) 1)))

(defun adjacent-entities (entity)
  (loop for ent in *entities*
	if (and (not (eq entity ent))
		(<= (abs (- (ent-x ent) (ent-x entity))) 1)
		(<= (abs (- (ent-y ent) (ent-y entity))) 1))
	  collect ent))

(defparameter entity-counters (make-hash-table))

(defgeneric entity-step (entity))
(defmethod entity-step ((entity entity))
  ;; (move-entity entity (elt '(:up :down :left :right :nop) (random 5)))  
  )

(defmethod initialize-instance :after ((entity entity) &rest args)
  (declare (ignore args))
  (setf (ent-id entity) (gethash (type-of entity) entity-counters 0))
  (incf (gethash (type-of entity) entity-counters 0)))

(defgeneric entity-interact (entity-a entity-b interaction value from-move))
(defmethod entity-interact (a b interaction value from-move)
  (declare (ignore from-move))
  (when (player-p a)
    (setf *echo* (format nil "Do not know how a ~s can ~a a ~s"
			 (ent-uniq-name a)
			 interaction
			 (ent-uniq-name b))))
  t)

(defmethod entity-interact ((a entity) (b entity) (interaction (eql :step)) value from-move)
  (declare (ignore from-move))
  (when (player-p a)
    (setf *echo* (format nil "Cannot move ~a into ~a"
			 (ent-uniq-name a) (ent-uniq-name b))))
  nil)

(defmethod entity-interact ((a actor) (b destructable) (interaction (eql :step)) value from-move)
  (entity-interact a b :attack value from-move))

(defmethod entity-interact ((a actor) (b pushable) (interaction (eql :step)) value from-move)
  (entity-interact a b :push value t) t)

(defmethod entity-interact ((a actor) (b actor) (interaction (eql :step)) value from-move)
  (when (enemy-p a b) (entity-interact a b :attack value t)) t)

(defmethod entity-interact ((a actor) (b destructable) (interaction (eql :attack)) value from-move)
  (declare (ignore value)
	   (ignore from-move))  
  (take-damage b (random (actor-attack a))))

(defmethod entity-interact ((a player) (b actor) (interaction (eql :attack)) value from-move)
  (let ((rv (call-next-method)))
    (if rv (setf *echo* (format nil "Attack ~a for ~a hp! (~a/~a)"
				(ent-uniq-name b)
				rv
				(ent-hp b)
				(ent-hp-max b)))
	(setf *echo* (format nil "~a is destroyed!"
			     (ent-uniq-name b))))))

(defmethod entity-interact ((a player) (b actor) (interaction (eql :attack)) value from-move) 
  (if (or (enemy-p a b)
	  (y-or-n-prompt (format nil "attack ~a" (ent-uniq-name b))))
      (call-next-method)))

(defmethod entity-interact ((a entity) (b pushable) (interaction (eql :push)) value from-move)
  (if (entity-at-direction b value)
      (progn (when (player-p a)
	       (setf *echo* (format nil "Cannot push ~a into ~a"
				    (ent-uniq-name b)
				    (ent-uniq-name (car (entity-at-direction b value))))))
	     nil)
      (progn (move-entity b value)
	     (when from-move
	       (move-entity a value))	     
	     t)))

(defmethod entity-interact ((a entity) (b boulder) (interaction (eql :step)) value from-move)
  (call-next-method a b :push value t))

(defun direction-delta (direction)
  (case direction
    (:up (list 0 -1))
    (:down (list 0 1))
    (:left (list -1 0))
    (:right (list 1 0))
    (:up-left (mapcar #'+ (mapcar #'direction-delta (list :up :left))))
    (:up-right (mapcar #'+ (mapcar #'direction-delta (list :up :right))))
    (:bot-left (mapcar #'+ (mapcar #'direction-delta (list :bot :left))))
    (:bot-right (mapcar #'+ (mapcar #'direction-delta (list :bot :right))))
    (:nop (list 0 0))))

(defun entity-move-result (entity value)
  (typecase value
    (sequence (list (first value) (second value)))
    (direction (mapcar #'+ (list (ent-x entity) (ent-y entity))
		       (direction-delta value)))))

(defun action-player-applicable-p (action)
  (case action
    (:step nil)
    (:push nil)
    (t nil)))

(defun entity-position (x y)
  (loop for ent in *entities*
	if (and (= (ent-x ent) x)
		(= (ent-y ent) y))
	  return ent))

(defun entities-position (x y)
  (loop for ent in *entities*
	if (and (= (ent-x ent) x)
		(= (ent-y ent) y))
	  collect ent))

(defun entity-at-direction (entity direction)
  (let ((coords (entity-move-result entity direction)))
    (entities-position (first coords) (second coords))))

(defun move-entity (entity value)
  (let* ((move-result (entity-move-result entity value))
	 (targets (delete entity (entities-position (first move-result) (second move-result)))))
    (if (not targets)
	(setf (ent-x entity) (first move-result)
	      (ent-y entity) (second move-result))	
	(if (= (length targets) 1) (entity-interact entity (car targets) :step value t)
	    (let ((selection
		    (if (eq entity *player*) (entity-selector targets "interract with")
			(random (length targets)))))
	      (when selection
		(entity-interact entity selection :Step value t)))))))

(defun in-range (thing a b)
  (<= a thing b))

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

(defun entity-selector (things &optional (string "select"))
  (if (null things) (progn (setf *echo* (format nil "Nothing to ~a" string))
			   nil)
      (let ((selection (selector-loop (mapcar #'ent-uniq-name things)
				      (format nil "What to ~a?" string))))
	(if selection (elt things selection)
	    (prog1 nil (setf *echo* "Cancel"))))))

(defun selector-loop (things &optional (string "select thing"))
  (case (length things)
    (0 nil)
    (1 0)
    (t (let ((page 0)
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
		    (#\Space (return nil))
		    (t (setf selection
			     (selector-char-to-n input page things)))))))))

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
    (loop for i from (- 5 (mod (ent-x *camera*) 5)) to (1- w) by 5
	  do (loop for j from (- 5 (1+ (mod (ent-y *camera*) 5))) to (1- h) by 5
		   do (move j i)
		      (addch (char-code #\.))))))

(defun draw-entities (screen)
  (let* ((w (getmaxx screen))
	 (h (getmaxy screen))
	 (x (- (ent-x *camera*) (truncate w 2)))
	 (y (- (ent-y *camera*) (truncate h 2))))
    (loop for entity in *entities*
	  if (and (in-range (ent-x entity) x (1- (+ x w)))
		  (in-range (ent-y entity) y (1- (+ y h))))
	    do (move (- (+ (ent-y entity) (truncate h 2)) (ent-y *camera*))
		     (- (+ (ent-x entity) (truncate w 2)) (ent-x *camera*)))
	       (if (destructable-p entity)
		   (cond
		     ((not (alive-p entity)) (attron (color-pair 3)))
		     ((ent-took-damage entity) (attron (color-pair 4)))
		     (t (attron (color-pair 2))))
		   (attron (color-pair 2)))
	       (addch (char-code (ent-char entity))))))

(defparameter *echo* nil)
(defvar *action* :step)

(defun refresh-screen ()
  (clear)
  (draw-field *screen*)
  (draw-entities *screen*)
  (move 0 0)
  (addstr (format nil "~a: ~a/~a ATK:~a"
		  (ent-uniq-name *player*)
		  (ent-hp *player*)
		  (ent-hp-max *player*)
		  (actor-attack *player*)))
  (attron (color-pair 2))
  ;; (move (- (getmaxy *screen*) 2) 0)
    (move (1- (getmaxy *screen*)) 0)
  (addstr (coerce *kills* 'string))
  ;; (when *echo*
  ;;   (addstr *echo*)    
  ;;   (setf *echo* nil))
  ;; (addstr (format nil "~a ~a" (length *entities*) (incf pstep)))
  (refresh))

(defvar pstep 0)
(defvar *kills* nil)

(defun main-step ()
  (when (not (remove-if-not #'(lambda (ent)
			    (enemy-p *camera* ent)) *entities*))
    (push #\Space *kills*)
    (dotimes (i (1+ (random 50)))     
      (let ((char (elt "0123456789 " (random (length "0123456789")))))
	(push (make-instance 'monster :char char
				      :fraction (if (char= char #\Space) 'spess 
						    (random 310))
				      :attack 100
				      :hp 1000
				      :behaviour :chaser
				      :x (- (random 100) 50)
				      :y (- (random 100) 50))
	      *entities*))))
  (sleep 0.05)
  (setf *entities* (delete-if-not #'alive-p *entities*))
  (loop for entity in *entities*
	do (entity-step entity))
  (refresh-screen)
  (loop for entity in *entities*
	if (destructable-p entity)
	  do (setf (ent-took-damage entity) nil)))

(defun main-loop ()
  (noecho)
  (start-color)
  (init-pair 1 color_blue color_red)
  (init-pair 2 color_white color_black)
  (init-pair 3 color_black color_red)
  (init-pair 4 color_red color_black)
  ;; (setf *player* (make-instance 'player))
  ;; (setf (slot-value *player* 'name) "david")
  ;; (setf *camera* *player*)
  ;; (push *player* *entities*)
  (loop
    (continuable (main-step))))

(defun start-with-swank ()
  (swank-loader:init)
  (let ((port 4005))
    (handler-case (swank:create-server :port port :style :spawn :dont-close t)
      (SB-BSD-SOCKETS:ADDRESS-IN-USE-ERROR () (swank:create-server :port (incf port) :style :spawn :dont-close t))))
  (setf *screen* (initscr))
  (setf *entities* nil)
  (dotimes (i 1)
    (push (make-instance 'monster :char #\G
				  :fraction 0
				  :name "Number eater"
				  :attack 1000
				  :hp 100000
				  :behaviour :chaser				
				  :x (random 1)
				  :y (random 1))
	  *entities*))
  (setf *camera* (first *entities*))
  (setf *player* *camera*)
  (setf *kills* nil)  
  (main-loop))


(trivial-signal::signal-handler-bind ((:winch (lambda (signo)
						(refresh-screen)))))
