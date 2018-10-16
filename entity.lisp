(in-package #:rogue-test)

;;; Base entity
(defclass entity ()
  ((position-x :accessor entity-x
	       :initform 0
	       :initarg :x)
   (position-y :accessor entity-y
	       :initform 0
	       :initarg :y)
   (representation :accessor entity-char
		   :initform #\0
		   :initarg :char
		   :type (or character nil))
   (visible :accessor entity-visible
	    :initform t
	    :initarg :visible
	    :type boolean)
   (name ;; :allocation :class
    :accessor entity-name
    :initarg :name
    :initform "Base entity")
   (id :accessor entity-id)
   (creation-time :accessor entity-created
		  :initarg :creation-time
		  :initform *ticks*)))

(defmethod initialize-instance :after ((entity entity) &rest args)
  (declare (ignore args))
  (setf (entity-id entity) (gethash (type-of entity) entity-counters 0))
  (incf (gethash (type-of entity) entity-counters 0)))

(defparameter entity-counters (make-hash-table))

;;; Class definitions

(defclass pushable (entity)
  ((name :initform "Pushable entity")))

(defclass boulder (entity)
  ((name :initform "Boulder")))

"Something an actor can fit into their inventory"
(defclass item (entity)
  ((name :initform "Thing")))

(defclass destructable (entity)
  ((name :initform "Destructable entity")   
   (hitpoints-max :initform 100
		  :initarg :hp
		  :accessor entity-hp-max)
   (hitpoints :initform nil
	      :initarg :hp-current
	      :accessor entity-hp)
   (took-damage :initform nil
		:accessor entity-took-damage)))

(defclass actor (entity)
  ((name :initform "Actor")
   (inventory :initform nil
	      :initarg :inventory
	      :accessor actor-inventory)
   (kills :initform nil
	  :initarg :kills
	  :accessor actor-kills)
   (attack :initform "1d6"
	   :initarg :attack
	   :accessor actor-attack)))

(defclass corpse (item)
  ((actor :initform nil
	  :accessor corpse-actor
	  :initarg :actor)
   (representation :initform #\X)
   (rotten :initform nil
	   :accessor corpse-rotten
	   :initarg :rotten)
   (decomposition-time :initform nil
		       :accessor corpse-rots-at
		       :initarg :decomposes-at)))

(defclass mortal (actor destructable)
  ())

(defclass monster (mortal pushable)
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

(defclass player (mortal pushable)
  ((entity-char :initform #\@)))

(defun pushable-p (entity)
  (typep entity 'pushable))

;;; Common

(defun entity-position (entity)
  (declare (type entity entity))
  (list (entity-x entity)
	(entity-y entity)))

(defun (setf entity-position) (position entity)
  (declare (type entity entity))
  (setf (entity-x entity) (first position)
	(entity-y entity) (second position)))

(defun entities-position (x y)
  (loop for entity in *entities*
	if (and (= x (entity-x entity))
		(= y (entity-y entity)))
	  collect entity))

;;; Predicates

(defun entity-p (thing)
  (typep thing 'entity))

(defun pushable-p (entity)
  (typep entity 'pushable))

(defun destructable-p (entity)
  (typep entity 'destructable))

(defun item-p (entity)
  (typep entity 'item))

(defun actor-p (entity)
  (typep entity 'actor))

(defun monster-p (entity)
  (typep entity 'monster))

;;; Generics definitions

(defgeneric entity-draw (entity camera screen-dimensions)
  (:documentation "Draws the entity"))

;;; This should return a unique, user-friendly name of an entity
(defgeneric entity-uniq-name (entity)
  (:documentation "Returns a unique, user-friendly name of an entity"))

;;; This applies amount damage to an entity and does any side effects of that
(defgeneric take-damage (destructable amount)
  (:documentation "Deal damage to an entity, and do all side-effects"))

(defgeneric regain-hp (destructable amount)
  (:documentation "Reagain some hp, and do all side-effects"))

;;;This returns t if an actor entity is alive.
(defgeneric alive-p (actor)) 		

;;;This returns t if two actor entities are enemies
(defgeneric enemy-p (actor-a actor-b))

;;;This method gets called for each entity every step
(defgeneric entity-step (entity)
  (:documentation "This will be called each step of the simulation for every entity "))

(defgeneric edible-p (actor entity)
  (:documentation "User the to figure out if something is edible for an actor"))

;;; An interaction generic method
;;; Describes everything an entity can directly do to an another entity
(defgeneric entity-interact (entity-a entity-b interaction value from-move)
  (:documentation "Describes any interaction an entity can have with another entity"))

;;; Make a corpse from an actor

(defun make-corpse (actor &key
			    (creation-t *ticks*)
			    (decomposition-t 100)
			    (rotten nil))
  (make-instance 'corpse
		 :x (entity-x actor)
		 :y (entity-y actor)
		 :actor actor
		 :rotten rotten
		 :decomposes-at (+ creation-t decomposition-t)
		 :creation-time creation-t))

;;; Method definitions

;;; entity-draw
;;; before: checks if entity is actually on the screen
(defmethod entity-draw :around (entity camera screen-dimensions)
  (when (and (in-range (entity-x entity)
		       (- (entity-x camera) (/ (w screen-dimensions) 2))
		       (w screen-dimensions))
	     (in-range (entity-y entity)
		       (- (entity-y camera) (/ (h screen-dimensions) 2))
		       (h screen-dimensions)))
    (call-next-method)))

;;; generic: draws entity character at entity position relevant to the camera
(defmethod entity-draw (entity camera screen-dimensions)
  (move (+ (- (entity-y entity) (entity-y camera)) (truncate (h screen-dimensions) 2))
	(+ (- (entity-x entity) (entity-x camera)) (truncate (w screen-dimensions) 2)))
  (addch (char-code (entity-char entity))))

(defmethod initialize-instance :after ((entity destructable) &rest args) 
  (declare (ignore args))
  (when (not (entity-hp entity))
    (setf (entity-hp entity) (entity-hp-max entity))))

;;; Step

;;; Generic entity
(defmethod entity-step ((entity entity)))

;;; Corpse
(defmethod entity-step ((corpse corpse))
  )

;;; Monster
(defmethod entity-step ((monster monster))
  (case (behaviour monster)
    (:chaser
     (with-slots (target) monster
       (when (and target (alive-p target))
	 (let ((delta (list (- (entity-x target) (entity-x monster))
			    (- (entity-y target) (entity-y monster)))))
	   (if (adjacent-p monster target)
	       (entity-interact monster target :attack nil nil)
	       (move-entity monster (list (+ (entity-x monster) (signum (first delta)))
					  (+ (entity-y monster) (signum (second delta)))))))
	 (when (and (not (alive-p target))
		    (eq monster *camera*))
	   (push (entity-char target) *kills*)))       
       (when (or (and target
		      (not (alive-p target)))
		 (not target))
	 (find-target monster))))
    (:nop nil)))

;;; Player

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
  (member (code-char input) (list #\h #\j #\k #\l #\y #\u #\b #\n #\.)))

(defun action-p (input)
  (member (code-char input) (list #\p #\A #\g #\d #\i #\e)))

(defun direction-p (thing)
  (member thing (list :up :down :left :right :up-left :up-right :bot-left :bot-right :nop)))

(deftype direction ()
  '(satisfies direction-p))

(defmethod entity-step ((player player))
  (let ((input (getch)))
    (cond
      ((movement-p input) (move-entity player (parse-movement input)))
      ((action-p input) (parse-action input)))))



(defmethod edible-p ((a actor) (b entity))
  nil)

(defmethod edible-p ((a monster) (b corpse))
  t)

(defmethod edible-p ((a player) (b corpse))
  t)

;;; Movement

(defun entities-at-direction (entity direction)
  (let ((coords (entity-move-result entity direction)))
    (entities-position (first coords) (second coords))))

(defun entity-move-result (entity value)
  (typecase value
    (sequence (list (first value) (second value)))
    (direction (mapcar #'+ (list (entity-x entity) (entity-y entity))
		       (direction-delta value)))))

;;; Take damage

(defmethod take-damage ((destructable destructable) damage)
  (if (zerop (entity-hp destructable)) nil
      (setf (entity-hp destructable) (max 0 (- (entity-hp destructable) damage))
	    (entity-took-damage destructable) t)))

(defmethod take-damage ((mortal mortal) damage)
  (let ((val (call-next-method)))
    (when (and val (not (alive-p mortal)))
      (let ((corpse (make-corpse mortal :decomposition-t (entity-hp-max mortal))))
	(setf (elt *entities* (position mortal *entities*)) corpse)))
    val))

(defmethod regain-hp ((destructable destructable) amount)
  (incf (entity-hp destructable) amount)
  (when (> (entity-hp destructable)
	   (entity-hp-max destructable))
    (setf (entity-hp destructable) (entity-hp-max destructable))))

(defmethod alive-p ((actor actor))
  (typecase actor
    (destructable (not (zerop (entity-hp actor))))
    (t nil)))

(defmethod alive-p ((corpse corpse))
  nil)

(defmethod enemy-p :around (a b)
  (if (eq a b) nil
      (call-next-method a b)))
(defmethod enemy-p ((actor-a actor) (actor-b actor))
  nil)

(defun entity-distance (a b)
  (abs (sqrt (+ (expt (- (entity-x a) (entity-x b)) 2)
		(expt (- (entity-y a) (entity-y b)) 2)))))

(defun find-target (monster)
  (let ((new-target (first (sort (remove-if-not #'(lambda (a) (enemy-p monster a))
						(remove-if-not #'actor-p *entities*))
				 #'(lambda (a b) (< (entity-distance monster a)
						    (entity-distance monster b)))))))
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
					    #'(lambda (a b) (< (entity-distance actor a)
							       (entity-distance actor b)))))))
	       (setf (target actor) new-target)
	       (when (and (monster-p new-target)
			  (not (target new-target)))
		 (setf (target new-target) actor))))))

(defun parse-action (input)
  (case (code-char input)
    ;; Push something
    (#\p (let ((direction (direction-prompt "push")))
	   (when direction
	     (let ((target (entity-selector (entities-at-direction *player* direction))))
	       (when target
		 (entity-interact *player* target :push direction nil))))))
    ;; Attack something
    (#\A (let ((targets (adjacent-entities *player*)))
	   (let ((selection (entity-selector targets "attack")))
	     (when selection (entity-interact *player* selection :attack nil nil)))))
    ;; Pick something up
    (#\g (let ((targets (remove-if-not #'item-p (entities-at-direction *player* :nop))))
	   (let ((thing (entity-selector targets "pick up")))
	     (when thing
	       (setf *entities* (delete thing *entities*))
	       (push thing (actor-inventory *player*))
	       (setf *echo* (format nil "Picked ~a up." (entity-uniq-name thing)))))))
    ;; Drop something on the ground
    (#\d (let ((thing (entity-selector (actor-inventory *player*) "pick up")))
	   (when thing
	     (setf (actor-inventory *player*) (delete thing (actor-inventory *player*)))
	     (setf (entity-position thing) (entity-position *player*))
	     (push thing *entities*)
	     (setf *echo* (format nil "Dropped ~a on the ground." (entity-uniq-name thing))))))
    ;; Look at something in the inventory
    (#\i (let ((thing (entity-selector (actor-inventory *player*) "look at" t)))
	   (when thing
	     (setf *echo* (format nil "You look at ~a suggestively."
				  (entity-uniq-name thing))))))
    ;; Eat something
    (#\e (let ((thing (entity-selector (actor-inventory *player*) "eat")))
	   (when thing	     
	     (entity-interact *player* thing :eat nil nil))))))

(defmethod enemy-p ((a player) (b monster))
  t)

(defmethod enemy-p ((a monster) (b player))
  t)

(defmethod enemy-p ((a monster) (b monster))
  (or (not (fraction a))
      (not (fraction b))
      (not (eq (fraction a)
	       (fraction b)))))

;;; Unique name

(defmethod entity-uniq-name ((entity entity))
  (format nil "~a №~a" (entity-name entity) (entity-id entity)))

(defmethod entity-uniq-name ((corpse corpse))
  (format nil "a ~:[~;rotten~]corpse of ~a" (corpse-rotten corpse) (entity-uniq-name (corpse-actor corpse))))

(defmethod entity-uniq-name ((entity player))
  (entity-name entity))

(defun adjacent-p (entity-a entity-b)
  (and (<= (abs (- (entity-x entity-a) (entity-x entity-b))) 1)
       (<= (abs (- (entity-y entity-a) (entity-y entity-b))) 1)))

(defun adjacent-entities (entity)
  (loop for ent in *entities*
	if (and (not (eq entity ent))
		(<= (abs (- (entity-x ent) (entity-x entity))) 1)
		(<= (abs (- (entity-y ent) (entity-y entity))) 1))
	  collect ent))

(defun action-player-applicable-p (action)
  (case action
    (:step nil)
    (:push nil)
    (t nil)))

(defun entities-at-direction (entity direction)
  (let ((coords (entity-move-result entity direction)))
    (entities-position (first coords) (second coords))))

(defun move-entity (entity value)
  (let* ((move-result (entity-move-result entity value))
	 (targets (remove-if #'item-p
		   (delete entity
			   (entities-position (first move-result) (second move-result))))))
    (if (not targets)
	(setf (entity-x entity) (first move-result)
	      (entity-y entity) (second move-result))	
	(let ((selection (if (eq entity *player*) (entity-selector targets "interract with")
			     (random (length targets)))))
	  (when selection
	    (entity-interact entity selection :Step value t))))))
