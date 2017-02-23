;;; interact.lisp - various interactions between entities

(in-package :rogue-test)

(defmethod entity-interact (a b interaction value from-move)
  (declare (ignore from-move))
  (when (player-p a)
    (setf *echo* (format nil "Do not know how a ~s can ~a a ~s"
			 (entity-uniq-name a)
			 interaction
			 (entity-uniq-name b))))
  t)

;;; steps

(defmethod entity-interact ((a entity) (b entity) (interaction (eql :step)) value from-move)
  (declare (ignore from-move))
  (when (player-p a)
    (setf *echo* (format nil "Cannot move ~a into ~a"
			 (entity-uniq-name a) (entity-uniq-name b))))
  nil)

(defmethod entity-interact ((a entity) (b item) (interaction (eql :step)) value from-move)
  (declare (ignore a b interaction value from-move))
  nil)

(defmethod entity-interact ((a actor) (b destructable) (interaction (eql :step)) value from-move)
  (entity-interact a b :attack value from-move))

(defmethod entity-interact ((a actor) (b pushable) (interaction (eql :step)) value from-move)
  (entity-interact a b :push value t) t)

(defmethod entity-interact ((a actor) (b actor) (interaction (eql :step)) value from-move)
  (when (enemy-p a b) (entity-interact a b :attack value t)) t)

(defmethod entity-interact ((a entity) (b boulder) (interaction (eql :step)) value from-move)
  (call-next-method a b :push value t))

;;; attack

(defmethod entity-interact ((a actor) (b destructable) (interaction (eql :attack)) value from-move)
  (declare (ignore value)
	   (ignore from-move))
  (let ((damage (roll-dice (actor-attack a))))
    (take-damage b damage)
    damage))

(defmethod entity-interact :after ((a actor) (b actor) (interaction (eql :attack)) value from-move)
  (declare (ignore value)
	   (ignore from-move)
	   (ignore interaction))
  (when (not (alive-p b))
    (push b (actor-kills a))))

(defmethod entity-interact ((a player) (b mortal) (interaction (eql :attack)) value from-move)
  (let ((rv (call-next-method)))
    (if rv (setf *echo* (format nil "Attack ~a for ~a hp~:[ (~a/~a)~;, destroying it!~]"
				(entity-uniq-name b)
				rv
				(not (alive-p b))
				(entity-hp b)
				(entity-hp-max b))))))

(defmethod entity-interact ((a player) (b actor) (interaction (eql :attack)) value from-move) 
  (if (or (enemy-p a b)
	  (y-or-n-prompt (format nil "attack ~a" (entity-uniq-name b))))
      (call-next-method)))

;;; push

(defmethod entity-interact ((a entity) (b pushable) (interaction (eql :push)) value from-move)
  (if (entities-at-direction b value)
      (progn (when (player-p a)
	       (setf *echo* (format nil "Cannot push ~a into ~a"
				    (entity-uniq-name b)
				    (entity-uniq-name (car (entities-at-direction b value))))))
	     nil)
      (progn (move-entity b value)
	     (when from-move
	       (move-entity a value))	     
	     t)))

;;; eat

(defmethod entity-interact ((a actor) (b entity) (interaction (eql :eat)) value from-move)
  (when (edible-p a b)
    (when (alive-p (push b (actor-kills a))))
    (setf *entities* (delete b *entities*))))

