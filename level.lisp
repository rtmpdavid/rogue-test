(in-package :rogue-test)

(defparameter dim-roll :3d4+3)

(defun mapref (map position)
  (aref map (x position) (y position)))

(defun (setf mapref) (value map position)
  (setf (aref map (x position) (y position)) value))

(defun pos-in-range (position level)
  (and (in-range (x position) 0 (1- (array-dimension level 0)))
       (in-range (y position) 0 (1- (array-dimension level 1)))))

;;; Styles:
;;; dungeon - your normal run-of-the-mill dungeon room: a bunch of rooms connected by tunnels

(defmacro loop-in-map ((position map &optional (offset nil offset-p) (dimensions nil dimensions-p))
		       &body body)
  (let ((range (gensym))
	(begin (gensym))
	(x (gensym))
	(y (gensym)))
    `(let* ((,begin ,(if offset-p offset '(p 0 0)))
	    (,range (pos+ ,begin ,(if dimensions-p dimensions `(pos- (array-dimensions ,map) (list 1 1))))))
       (loop for ,x from (x ,begin) to (w ,range)
	     do (loop for ,y from (y ,begin) to (h ,range)
		      do (let ((,position (list ,x ,y)))
			   ,@body))))))

(defun draw-level (level cam-pos scr-dims) 
  (loop-in-map (pos level (p 0 0) scr-dims)
    (when  (pos-in-range pos level)
      (let ((sym (case (mapref level pos)
		   (:room #\.)
		   (:wall #\W)
		   (:maze #\#)
		   (:maze-filled #\~)
		   (:dbg #\$)
		   (:door #\=)
		   (t #\ ))))
	(move (+ (- (y pos) (y cam-pos)) (truncate (h scr-dims) 2))
	      (+ (- (x pos) (x cam-pos)) (truncate (w scr-dims) 2)))
	(addch (char-code sym))))))

(defmacro with-level-copy ((copy level) &body body)
  `(let ((,copy (copy-array ,level)))
     ,@body))

;; (defun cast-rays (level position &optional (max-distance nil max-distance-p))
;;   )

(defun paint-room (level room)
  (let ((position (getf room :pos))
	(size (pos- (getf room :size) (p 1 1))))
    (loop-in-map (pos level position size)
      (if (or (= (x pos) (x position))
	      (= (y pos) (y position))
	      (= (x pos) (+ (x position) (w size)))
	      (= (y pos) (+ (y position) (h size))))
	  (setf (mapref level pos) :wall)
	  (setf (mapref level pos) :room)))))

(defun gen-rooms (amount)
  (flet ((room-collide-p (a b)
	   (let* ((a-a (getf a :pos))
		  (a-b (getf a :size))
		  (b-a (getf b :pos))
		  (b-b (getf b :size)))
	     (and (or (in-range (x a-a) (x b-a) (x b-b))
		      (in-range (x a-b) (x b-a) (x b-b))
		      (in-range (x b-a) (x a-a) (x a-b))
		      (in-range (x b-b) (x a-a) (x a-b)))
		  
		  (or (in-range (y a-a) (y b-a) (y b-b))
		      (in-range (y a-b) (y b-a) (y b-b))
		      (in-range (y b-a) (y a-a) (y a-b))
		      (in-range (y b-b) (y a-a) (y a-b)))))))
    (let ((rooms nil)
	  (min-x 0)
	  (min-y 0)
	  (max-x 0)
	  (max-y 0)
	  (o-x 0)
	  (o-y 0)
	  (e-x 0)
	  (e-y 0))
      (loop for size = (p (roll-dice dim-roll) (roll-dice dim-roll))
	    for pos = (list (- (random-range o-x e-x) (half-w size))
			    (- (random-range o-y e-y) (half-h size)))
	    for room = (list :pos pos :size size :n (length rooms))
	    for i from 0
	    if (notany #'(lambda (room-b) (room-collide-p room room-b)) rooms)
	      do (progn (push room rooms)
			(setf max-x (max max-x (+ (x pos) (w size))))
			(setf max-y (max max-y (+ (y pos) (h size))))
			(setf min-x (min min-x (x pos)))
			(setf min-y (min min-y (y pos))))
	    else
	      do (progn (= i 100)
			(setf i 0)
			(decf o-x)
			(decf o-y)
			(incf e-x)
			(incf e-y))
	    until (zerop (- (length rooms) amount)))
      (loop for room in rooms
	    do (setf (getf room :pos) (pos- (getf room :pos) (p min-x min-y))))
      (let ((level (make-array (pos- (p max-x max-y) (p min-x min-y)) :initial-element nil)))
	(list :level level :rooms rooms)))))

(defun print-level (level)
  (loop for y from 0 to (1- (array-dimension level 1))
  	do (loop for x from 0 to (1- (array-dimension level 0))
  		 do (let ((sym (case (aref level x y)
				 (:room #\.)
				 (:wall #\W)
				 (:maze #\#)
				 (:maze-filled #\~)
				 (:dbg #\$)
				 (:door #\=)
				 (t #\ ))))
		      (format t "~a" sym)))
  	   (terpri)))

(defun find-at (level position directions &rest values)
  (loop for dir in directions
	for pos = (pos+ position (direction-delta dir))
	if (and (pos-in-range pos level)
		(find (mapref level pos) values))
	  collect pos))

(defun count-at (level position directions value)  
  (let ((rv 0))
    (loop for dir in directions
	 for pos = (pos+dir position dir)
	 if (and (pos-in-range pos level)
		 (equal value (mapref level pos)))
	   do (incf rv)
	      (list value pos dir))
    rv))

(defun dir-in-range (level position dir)
  (let ((pos (pos+dir position dir)))
    (and (in-range (x pos) 0 (1- (array-dimension level 0)))
	 (in-range (y pos) 0 (1- (array-dimension level 1))))))

(defun can-branch-maze (level position dir)
  (let ((pos (pos+dir position dir)))
    (and (pos-in-range pos level)
	 (not (mapref level pos))
	 (zerop (count-at level pos *dirs* :room))
	 (case (count-at level position *cardinal* :maze)
	   (0 (= (count-at level pos *diagonal* :maze) 0))
	   (1 (and (= (count-at level pos *cardinal* :maze) 1)
		   (mapref level position)))
	   (2 (and (every #'(lambda (diag-pos)
			      (find diag-pos (find-at level position *cardinal* :maze)
				    :test #'equal))
			  (find-at level pos *diagonal* :maze))

	       (= (count-at level pos *cardinal* :maze) 1)
		   (mapref level position)))))))

(defun direction-pool (level position)
  (remove-if-not #'(lambda (dir) (can-branch-maze level position dir)) *cardinal*))

(defun choose-direction (level pos)
  (let ((pool (direction-pool level pos)))
    (when pool (random-elt pool))))

(defun gen-pasage (level position)
  (let ((pos (copy-seq position))
	(branch-targets nil))
    (loop for dir = (choose-direction level pos)
	  do (dbg-print-pos level pos)
	  while dir
	  do (loop for pool = (direction-pool level pos)
		   do (setf (mapref level pos) :maze)
		   while (and (roll-above :d1000 250)
			      (can-branch-maze level pos dir))		   
		   do (when (> (length pool) 1)
			(push (list pos pool) branch-targets))		      
		      (setf pos (pos+dir pos dir))))
    (loop while branch-targets
	  for target = (random-elt branch-targets)
	  do (mapcar #'(lambda (dir)
			 (gen-pasage level (pos+dir (first target) dir)))
		     (second target))
	     (setf branch-targets (delete target branch-targets)))))

(defun gen-passages (level)
  (loop-in-map (pos level)
    (when (and (direction-pool level pos)
	       (not (mapref level pos)))
      (gen-pasage level pos))))

(defun dbg-print-pos (level pos)
  (let ((tmp (alexandria:copy-array level)))
    (setf (mapref tmp pos) :dbg)
    (print-level tmp)
    (sleep 0.1)
    (terpri)))

(defun pos-in-room (position room)
  (let ((room-position (getf room :pos))
	(size (getf room :size)))
    (and (in-range (x position) (x room-position) (w size))
	 (in-range (y position) (y room-position) (h size)))))

(defun corner-p (level position)
  (and (= (count-at level position *cardinal* :wall) 2)
       (= (count-at level position *diagonal* :room) 1)
       (= (count-at level position *cardinal* :room) 0)))

(defun walk-maze (start room rooms level)
  (with-level-copy (tmp (getf level :level))
   (let ((connections (make-hash-table))
	 (deadends nil)
	 (branches nil)
	 (pos (first (find-at tmp start *cardinal* :maze)))
	 (dist 0))
     (loop while pos
	   until (eq (mapref tmp pos) :maze-filled)
	   for next-pos = (find-at tmp pos *cardinal* :maze)
	   do (setf (mapref tmp pos) :maze-filled)
	      (let ((adj (first (find-at tmp pos *cardinal* :wall))))
		(when (and adj
			   (not (pos-in-room adj room))
			   (not (corner-p tmp adj))
			   (= (count-at tmp pos *cardinal* :room) 0))
		  (let ((room-b (find-if #'(lambda (room)
					     (pos-in-room adj room))
					 rooms)))
		    (let ((val (gethash (getf room-b :n) connections)))
		      (when (or (not val)
				(< dist (getf val :dist)))
			(setf (gethash (getf room-b :n) connections)
			      (list :rooms (sort (list (getf room :n) (getf room-b :n)) #'<)
				    :dist dist
				    :start start
				    :end adj))))))
		(incf dist))
	   if next-pos
	     do (progn (setf pos (car next-pos))		       
		       (loop for branch in (cdr next-pos)
			     do (push (list branch dist) branches)))
	   else 
	     do (progn
		  (setf deadends (adjoin pos deadends :test #'equal))
		  (when branches
		    (setf pos (first (car branches))
			  dist (second (car branches))
			  branches (cdr branches)))))
     (list :connections (alexandria:hash-table-values connections) :deadends deadends))))

(defun gen-connections (level)
  (let ((connections (make-hash-table :test #'equal))
	(deadends nil))
   (loop for room in (getf level :rooms)
	 for pos = (getf room :pos)
	 for size = (getf room :size)
	 for x = (1+ (x pos))
	 for y = (y pos)
	 for i from 0
	 do (flet ((walk ()
		     (let ((result (walk-maze (list x y) room (remove room (getf level :rooms)) level)))
		       (loop for con in (getf result :connections)			    
			     for val = (gethash (getf con :rooms) connections)
			     if (or (not val)
				    (< (getf con :dist) (getf val :dist)))
			       do (setf (gethash (getf con :rooms) connections) con))
		       (loop for deadend in (getf result :deadends)
		       	     do (setf deadends (adjoin deadend deadends :test #'equalp))))))
	      (loop do (incf x)
		    while (< (- x (x pos)) (1- (w size)))
		    do (walk))
	      (loop do (incf y)
		    while (< (- y (y pos)) (1- (h size)))
		    do (walk))
	      (loop do (decf x)
		    while (> (- x (x pos)) 0)
		    do (walk))
	      (loop do (decf y)
		    while (> (- y (y pos)) 0)
		    do (walk))))
    (loop for con in (alexandria:hash-table-values connections)
	  if (= (count-at (getf level :level) (getf con :start) *cardinal* :door) 0)
	    do (setf (mapref (getf level :level) (getf con :start)) :door)
	  if (= (count-at (getf level :level) (getf con :end) *cardinal* :door) 0)
	    do (setf (mapref (getf level :level) (getf con :end)) :door))
    deadends))

(defun remove-deadends (level deadends)
  (loop for pos in deadends
	do (loop while (and (= (count-at level pos *cardinal* :maze) 1)
			    (= (count-at level pos *cardinal* :door) 0))
		 do (dbg-print-pos level pos)
		    (setf (mapref level pos) nil
			  pos (first (find-at level pos *cardinal* :maze))))))

(defun add-random-doors (level)
  (declare (ignore level))
  ;; (loop-in-map (pos level)
  ;;   (if (and (not (mapref level pos))
  ;; 	     (= (count-at level pos *cardinal* :maze) 2)
  ;; 	     (= (count-at level pos *cardinal* :door) 0)
  ;; 	     (or (= (count-at level pos '(:left :right) :maze) 2)
  ;; 		 (= (count-at level pos '(:up :down) :maze) 2))
  ;; 	     (toss-coin)
  ;; 	     (toss-coin)
  ;; 	     (toss-coin))
  ;; 	(setf (mapref level pos) :maze)))
  )

(defun gen-labyrinth (level)
  (gen-passages (getf level :level))
  ;; remove-deadends (getf level :level)
  (gen-connections level)
  (print-level (getf level :level))
  ;; (add-random-doors (getf level :level))
  )

(defun generate-dungeon (n-rooms)
  (let* ((level (gen-rooms n-rooms)))
    (loop for room in (getf level :rooms)
	  do (paint-room (getf level :level) room))
    (gen-labyrinth level)
    ;; (print-level (getf level :level))
    (getf level :level)))

(defun generate-topology (number-of-rooms &optional (style :dungeon))
  (case style
    (:dungeon (generate-dungeon number-of-rooms))))

;; (defun gen-level (number-of-rooms challenge-rating style))
