;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rtm-model.lisp
;;;   - implementation of a RTM model that works online and offline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rtm-lisp-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(defun get-current-time-string ()
  "Returns a rtm-valid date, such as:  2006-05-07T10:26:21Z"
  (multiple-value-bind (sec min hour day month year weekday daylight-p zone) (get-decoded-time)
    (declare (ignore weekday daylight-p))
    (format nil "~d-~2,'0:d-~2,'0:dT~2,'0:d:~2,'0:d:~2,'0:dZ~2,'0:d" year month day hour min sec zone)))

(defun from-rtm-type (type value)
  (case type
    (bool (string= "1" value))
    (t value)))

(defun to-rtm-type (type value)
  (case type
    (bool (if value "1" "0"))
    (t value)))

(defmacro awhen (it &body action)
  `(let ((it ,it))
     (when it
       ,@action)))

(defun find-by-criteria (criteria criteria-selector bucket)
  (find criteria bucket :key criteria-selector :test #'string=))

(defun find-by-id (id-string bucket)
  (find-by-criteria id-string #'get-id bucket))

(defun cdrassoc (key alist)
  (cdr (assoc key alist)))

(defun chain (fn 1stargs finalarg)
  ;; usefull for the following example:
  ;; (cdrassoc :rrule
  ;; 	  (cdrassoc :taskseries
  ;; 		    (cdrassoc :list
  ;; 			      '((:list . ((:taskseries . ((:rrule . "ola")))))))))
  ;; (chain #'cdrassoc '(:rrule :taskseries :list) '((:list . ((:taskseries . ((:rrule . "ola")))))))
  (if 1stargs
      (funcall fn (first 1stargs) (chain fn (rest 1stargs) finalarg))
      finalarg))

(defmacro cdrassocchain (keylist alist)
  `(chain #'(lambda (x y) (cdr (assoc x y))) ,keylist ,alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object model for the RTM entities

;;; User information
(defclass rtm-user-info ()
  ((id                :accessor get-id                 :initarg :id)
   (username          :accessor get-username           :initarg :username)
   (fullname          :accessor get-fullname           :initarg :fullname)
   (permissions       :accessor get-permissions        :initarg :permissions)
   (contacts          :accessor get-contacts           :initarg :contacts           :initform nil)
   (contact-groups    :accessor get-contact-groups     :initarg :contact-groups     :initform nil)
   (locations         :accessor get-locations          :initarg :locations          :initform nil)
   (task-lists        :accessor get-task-lists         :initarg :task-lists         :initform nil)
   (default-task-list :accessor get-default-task-lists :initarg :default-task-lists :initform nil)))

(defvar *rtm-user-info* nil)
(setf *rtm-user-info* (make-instance 'rtm-user-info))  


(defclass rtm-object ()
  ((dirty :accessor is-dirty :initarg :dirty :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Contacts
(defclass contact (rtm-object)
  ((id         :accessor get-id       :initarg :id)
   (username   :accessor get-username :initarg :username)
   (full-name  :accessor get-fullname :initarg :fullname)))

(defmethod rtm-delete-contact ((c contact))
  (make-offline-task (rtm:rtm-api-contacts-delete (get-id c))
		     (setf (get-contacts *rtm-user-info*)
			   (remove-if #'(lambda (x) (string= (get-id x) (get-id x)))
				      (get-contacts *rtm-user-info*)))))

(defun rtm-add-contact (username-or-email)
  (let ((temp-id (make-offline-id "contact")))
    (make-offline-task
     ;; remotely:
     (let* ((alist (rtm:rtm-api-contacts-add username-or-email))
	    (offline-contact (find-by-id temp-id (get-contacts *rtm-user-info*))))
       (setf (get-id offline-contact) (cdrassoc :id alist)
	     (get-fullname offline-contact) (cdrassoc :fullname alist)))
     ;; locally:
     (let ((new-contact (make-instance 'contact
				       :id temp-id
				       :username username-or-email
				       :fullname "(pending sync)")))
       (pushnew new-contact
		(get-contacts *rtm-user-info*)
		:key #'get-id
		:test #'string=)
       new-contact))))

(defun get-contact-list ()
  (declare (special *rtm-user-info*))
  (get-contacts *rtm-user-info*))

(defun refresh-contact-list ()
  (declare (special *rtm-user-info*))
  (let ((contacts (mapcar
		   #'(lambda (c)
		       (make-instance 'contact
				      :username   (cdrassoc :username c)
				      :fullname   (cdrassoc :fullname c)
				      :id (cdrassoc :id c)))
		   (rtm:rtm-api-contacts-get-list))))
    (setf (get-contacts *rtm-user-info*) contacts)))

(defun list-contacts ()
  (format t "DEPRECATED: list-contacts is now get-contact-list or refresh-contact-list.")
  (let ((contacts (mapcar
		   #'(lambda (c)
		       (make-instance 'contact
				      :username   (cdrassoc :username c)
				      :fullname   (cdrassoc :fullname c)
				      :id (cdrassoc :id c)))
		   (rtm:rtm-api-contacts-get-list))))
    (setf (get-contacts *rtm-user-info*) contacts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Groups (of contacts)
(defclass contact-group (rtm-object)
  ((id       :accessor get-id       :initarg :id)
   (name     :accessor get-name     :initarg :name)
   (contacts :accessor get-contacts :initarg :contacts :initform nil)))

(defmethod rtm-delete-contact-group ((g contact-group))
  (make-offline-task
   (rtm:rtm-api-groups-delete (get-id g))
   (setf (get-contact-groups *rtm-user-info*)
	 (remove-if (lambda (x) (string= (get-id x) (get-id g)))
		    (get-contact-groups *rtm-user-info*)))))

(defun rtm-add-contact-group (name)
  (declare (special *rtm-user-info*))
  (let ((temp-id (make-offline-id "contactgroup")))
    (make-offline-task
     ;;remotely:
     (let* ((alist (rtm:rtm-api-groups-add name))
	    (offline-group (find-by-id temp-id (get-contact-groups *rtm-user-info*))))
       (setf (get-id       offline-group) (cdrassoc :id alist)
	     (get-fullname offline-group) (cdrassoc :fullname alist)))
     ;; locally:
     (let ((new-group (make-instance 'contact-group
				     :id temp-id
				     :name name)))
       (pushnew new-group
		(get-contact-groups *rtm-user-info*)
		:key #'get-id
		:test #'string=)
       new-group))))

(defmethod rtm-add-contact-to-group ((c contact) (g contact-group))
  (make-offline-task
   (rtm:rtm-api-groups-add-contact (get-id g) (get-id c))
   (pushnew c (get-contacts g) :key #'get-id :test #'string=)))


(defmethod rtm-remove-contact-from-group ((c contact) (g contact-group))
  (make-offline-task
   (rtm:rtm-api-groups-remove-contact (get-id g) (get-id c))
   (setf (get-contacts g)
	 (remove-if (lambda (x) (string= (get-id x) (get-id c)))
		    (get-contacts g)))))

(defun get-contact-group-list ()
  (declare (special *rtm-user-info*))
  (get-contacts *rtm-user-info*))

(defun refresh-contact-group-list ()
  (declare (special *rtm-user-info*))
  (unless (get-contacts *rtm-user-info*)
    (list-contacts))
  (let ((groups
	 (mapcar
	  #'(lambda (g)
	      (make-instance 'contact-group
			     :id       (cdrassoc :id g)
			     :name     (cdrassoc :name g)
			     :contacts (mapcar
					#'(lambda (c-alist)
					    (find-by-id (cdrassoc :id (cdr c-alist))
							(get-contacts *rtm-user-info*)))
					(cdrassoc :contacts g))))
	  (rtm:rtm-api-groups-get-list))))
    (setf (get-contact-groups *rtm-user-info*) groups)))


(defun list-contact-groups ()
  "Fetches contacts, then fetches groups and sorts contacts into them."
  (format t "DEPRECATED: list-contact-groups is now get-contact-group-list or refresh-contact-group-list.")
  (unless (get-contacts *rtm-user-info*)
    (list-contacts))
  (let ((groups
	 (mapcar
	  #'(lambda (g)
	      (make-instance 'contact-group
			     :id       (cdrassoc :id g)
			     :name     (cdrassoc :name g)
			     :contacts (mapcar
					#'(lambda (c-alist)
					    (find-by-id (cdrassoc :id (cdr c-alist))
							(get-contacts *rtm-user-info*)))
					(cdrassoc :contacts g))))
	  (rtm:rtm-api-groups-get-list))))
    (setf (get-contact-groups *rtm-user-info*) groups)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists (of tasks)
(defclass task-list (rtm-object)
  ((id         :accessor get-id         :initarg :id)
   (name       :accessor get-name       :initarg :name)
   (archived   :accessor is-archived    :initarg :archived :initform nil)
   (smart      :accessor is-smart       :initarg :smart  :initform nil)
   (position   :accessor get-position   :initarg :position  :initform "0")
   (locked     :accessor is-locked      :initarg :locked  :initform nil)
   (deleted    :accessor is-deleted    :initarg :deleted :initform nil)
   (sort-order :accessor get-sort-order :initarg :sort-order :initform "")
   (filter     :accessor get-filter     :initarg :filter :initform "")
   (tasks      :accessor get-tasks      :initarg :tasks :initform nil)))

(defun rtm-add-task-list (name &optional (filter ""))
  (declare (special *rtm-user-info*))
  (let ((temp-id (make-offline-id "tasklist")))
    (make-offline-task
     (awhen (find-by-id temp-id (get-task-lists *rtm-user-info*))
       ;; otherwise, we have already deleted it, so don't even bother! :d
       (let* ((alist (rtm:rtm-api-lists-add name filter))
	      (offline-task-list it))
	 (setf (get-id       offline-task-list) (cdrassoc :id  alist)
	       (is-archived  offline-task-list) (from-rtm-type 'bool (cdrassoc :archived alist))
	       (is-smart     offline-task-list) (from-rtm-type 'bool (cdrassoc :smart    alist))
	       (is-deleted   offline-task-list) (from-rtm-type 'bool (cdrassoc :deleted  alist))
	       (is-locked    offline-task-list) (from-rtm-type 'bool (cdrassoc :locked   alist))
	       (get-position offline-task-list) (cdrassoc :position alist))))
     (let ((new-list
	    (make-instance 'task-list
			   :id     temp-id
			   :name   name
			   :smart  (not (null filter))
			   :filter filter)))
       (pushnew new-list
		(get-task-lists *rtm-user-info*)
		:key #'get-id
		:test #'string=)
       new-list))))

(defmethod rtm-delete-task-list ((tl task-list))
  (declare (special *rtm-user-info*))
  (make-offline-task
   (rtm:rtm-api-lists-delete (get-id tl))
   (progn
     (setf (get-task-lists *rtm-user-info*)
	   (remove-if (lambda (x) (string= (get-id x) (get-id tl)))
		      (get-task-lists *rtm-user-info*)))
     (setf (get-deleted tl) t))))

(defmethod rtm-archive-task-list ((tl task-list))
  (make-offline-task
   (rtm:rtm-api-lists-archive (get-id tl))
   (setf (is-archived tl) t)))

(defmethod rtm-unarchive-task-list ((tl task-list))
  (make-offline-task
   (rtm:rtm-api-lists-unarchive (get-id tl))
   (setf (is-archived tl) nil)))

(defmethod rtm-set-default-task-list ((tl task-list))
  (declare (special *rtm-user-info*))
  (make-offline-task
   (rtm:rtm-api-lists-set-default-list (get-id tl))
   (setf (get-default-task-lists *rtm-user-info*) tl)))

(defmethod rtm-change-task-list-name ((tl task-list) name)
  (make-offline-task
   (rtm:rtm-api-lists-set-name (get-id tl) name)
   (setf (get-name tl) name)))

(defun get-task-lists-list ()
  (declare (special *rtm-user-info*))
  (get-task-lists *rtm-user-info*))

(defun refresh-task-lists-list ()
  (declare (special *rtm-user-info*))
  (let ((task-lists
	 (mapcar (lambda (l)
		   (make-instance 'task-list
				  :name       (cdrassoc :name l)
				  :id         (cdrassoc :id l)
				  :position   (cdrassoc :position l)
				  :sort-order (cdrassoc :sort_order l)
				  :archived   (from-rtm-type 'bool (cdrassoc :archived l))
				  :smart      (from-rtm-type 'bool (cdrassoc :smart l))
				  :locked     (from-rtm-type 'bool (cdrassoc :locked l))
				  :deleted    (from-rtm-type 'bool (cdrassoc :deleted l))
				  :tasks      nil))
		 (rtm:rtm-api-lists-get-list))))
    (setf (get-task-lists *rtm-user-info*) task-lists)
    task-lists))

(defun rtm-list-task-lists ()
  (declare (special *rtm-user-info*))
  (format t "DEPRECATED: rtm-list-task-lists is now get-task-lists or refresh-task-lists-list.")
  (let ((task-lists
	 (mapcar (lambda (l)
		   (make-instance 'task-list
				  :name       (cdrassoc :name l)
				  :id         (cdrassoc :id l)
				  :position   (cdrassoc :position l)
				  :sort-order (cdrassoc :sort_order l)
				  :archived   (from-rtm-type 'bool (cdrassoc :archived l))
				  :smart      (from-rtm-type 'bool (cdrassoc :smart l))
				  :locked     (from-rtm-type 'bool (cdrassoc :locked l))
				  :deleted    (from-rtm-type 'bool (cdrassoc :deleted l))
				  :tasks      nil))
		 (rtm:rtm-api-lists-get-list))))
    (setf (get-task-lists *rtm-user-info*) task-lists)
    task-lists))



(defun get-count-for-list-named (task-list-name &key filter-complete)
  "Fetch count for badges, by name."
  (declare (special *rtm-user-info*))
  (let ((l (get-tasks (find-by-criteria task-list-name #'get-name (get-task-lists *rtm-user-info*)))))
    (length (if filter-complete
		(mapcan (lambda (x) (when (string= "" (rtm:get-completed x)) (list x))) l)
		l))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Locations
(defclass location (rtm-object)
  ((name      :accessor get-name      :initarg :name)
   (id        :accessor get-id        :initarg :id)
   (longitude :accessor get-longitude :initarg :longitude)
   (latitude  :accessor get-latitude  :initarg :latitude)
   (zoom      :accessor get-zoom      :initarg :zoom)
   (address   :accessor get-address   :initarg :address)
   (viewable  :accessor is-viewable   :initarg :viewable)))

(defmethod get-location-url ((location location) &key (zoom 19))
   (format nil "http://maps.google.com/?q=~a@~a,~a&t=h&z=~a"
	   (rtm:get-name location)
	   (rtm:get-latitude location)
	   (rtm:get-longitude location)
	   zoom))

(defun get-location-list ()
  (declare (special *rtm-user-info*))
  (get-locations *rtm-user-info*))

(defun refresh-location-list ()
  (declare (special *rtm-user-info*))
  (let ((locations
	 (mapcar
	  #'(lambda (l)
	      (make-instance 'location
			     :id        (cdrassoc :id l)
			     :name      (cdrassoc :name l)
			     :longitude (cdrassoc :longitude l)
			     :latitude  (cdrassoc :latitude l)
			     :zoom      (cdrassoc :zoom l)
			     :address   (cdrassoc :address l)
			     :viewable  (from-rtm-type 'bool (cdrassoc :viewable l))))
	  (rtm:rtm-api-locations-get-list))))
    (setf (get-locations *rtm-user-info*) locations)))

(defun list-locations ()
  (format t "DEPRECATED: list-locations is now get-location-list or refresh-location-list.")
  (let ((locations
	 (mapcar
	  #'(lambda (l)
	      (make-instance 'location
			     :id        (cdrassoc :id l)
			     :name      (cdrassoc :name l)
			     :longitude (cdrassoc :longitude l)
			     :latitude  (cdrassoc :latitude l)
			     :zoom      (cdrassoc :zoom l)
			     :address   (cdrassoc :address l)
			     :viewable  (from-rtm-type 'bool (cdrassoc :viewable l))))
	  (rtm:rtm-api-locations-get-list))))
    (setf (get-locations *rtm-user-info*) locations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Task
(defclass task (rtm-object)
  ((list-id       :accessor get-list-id       :initarg :list-id)
   (taskseries-id :accessor get-taskseries-id :initarg :taskseries-id :initform "")
   (task-id       :accessor get-id            :initarg :task-id)
   (created       :accessor get-created       :initarg :created :initform "")
   (modified      :accessor get-modified      :initarg :modified :initform "")
   (name          :accessor get-name          :initarg :name)
   (source        :accessor get-source        :initarg :source :initform "")
   (url           :accessor get-url           :initarg :url :initform "")
   (location-id   :accessor get-location-id   :initarg :location-id :initform "")
   (tags          :accessor get-tags          :initarg :tags :initform nil)
   (recurrence    :accessor get-recurrence    :initarg :recurrence :initform '((:every) (:after)))
   (participants  :accessor get-participants  :initarg :participants :initform nil)
   (notes         :accessor get-notes         :initarg :notes :initform nil)
   (due           :accessor get-due           :initarg :due :initform "")
   (has-due-time  :accessor has-due-time      :initarg :has-due-time :initform nil)
   (added         :accessor get-added         :initarg :added :initform "")
   (completed     :accessor get-completed     :initarg :completed :initform "")
   (deleted       :accessor get-deleted       :initarg :deleted :initform "")
   (priority      :accessor get-priority      :initarg :priority :initform "N")
   (postponed     :accessor get-postponed     :initarg :postponed :initform "")
   (estimate      :accessor get-estimate      :initarg :estimate :initform "")))

(defmethod get-location ((task task))
  (declare (special *rtm-user-info*))
  (find-by-id (get-location-id task) (get-locations *rtm-user-info*)))

(defmethod get-list ((task task))
  (declare (special *rtm-user-info*))
  (find-by-id (get-list-id task) (get-task-lists *rtm-user-info*)))

(defmethod rtm-list-tasks-on-list ((wanted-list task-list) &key filter last-sync exclude-completed)
  (reverse
   (apply
    #'append
    (mapcar
     #'(lambda (list)
	 (mapcan
	  #'(lambda (taskseries)
	      (when (and (cdrassoc :task taskseries) ;; Make sure we have no empty taskseries, useless to us...
			 (or (not exclude-completed)
			     (string= "" (cdrassocchain '(:completed :task) taskseries)))) ;; filter, if needed, completed
		(list (let* ((task (cdrassoc :task taskseries))
			     (participants
			      (mapcar #'(lambda (c)
					  (let ((c (cdr c)))
					    (make-instance 'contact
							   :id       (cdrassoc :id c)
							   :username (cdrassoc :username c)
							   :fullname (cdrassoc :fullname c))))
				      (cdrassoc :participants taskseries)))
			     (notes (mapcar #'(lambda (n)
						(let ((n (cdr n)))
						  (make-instance 'note
								 :id       (cdrassoc :id n)
								 :created  (cdrassoc :created n)
								 :modified (cdrassoc :modified n)
								 :title    (cdrassoc :title n)
								 :contents (cdrassoc :$t n))))
					    (cdrassoc :notes taskseries)))
			     (tags (mapcar #'(lambda (tag) (cdr tag))
					   (cdrassoc :tags taskseries)))
			     (recurrence (let ((rrule (cdrassoc :rrule taskseries)))
					   `((:every . ,(when (from-rtm-type 'bool
									     (cdrassoc :every rrule))
							      (cdrassoc :$t rrule)))
					     (:after . ,(when (from-rtm-type 'bool
									     (cdrassoc :after rrule))
							      (cdrassoc :$t rrule))))))
			     (task-id (cdrassoc :id task))
			     (new-task (or (find-by-id task-id (get-tasks wanted-list))
					   (make-instance 'task :task-id task-id))))
			(setf (slot-value new-task 'list-id) (cdrassoc :id list))
			(setf (slot-value new-task 'taskseries-id) (cdrassoc :id taskseries))
			(setf (slot-value new-task 'created) (cdrassoc :created taskseries))
			(setf (slot-value new-task 'modified) (cdrassoc :modified taskseries))
			(setf (slot-value new-task 'name) (cdrassoc :name taskseries))
			(setf (slot-value new-task 'source) (cdrassoc :source taskseries))
			(setf (slot-value new-task 'url) (cdrassoc :url taskseries))
			(setf (slot-value new-task 'location-id) (cdrassoc :location_id taskseries))
			(setf (slot-value new-task 'tags) tags)
			(setf (slot-value new-task 'recurrence) recurrence)
			(setf (slot-value new-task 'participants) participants)
			(setf (slot-value new-task 'notes) notes)
			(setf (slot-value new-task 'due) (cdrassoc :due task))
			(setf (slot-value new-task 'has-due-time) (from-rtm-type 'bool (cdrassoc :has_due_time task)))
			(setf (slot-value new-task 'added) (cdrassoc :added task))
			(setf (slot-value new-task 'completed) (cdrassoc :completed task))
			(setf (slot-value new-task 'deleted) (cdrassoc :deleted task))
			(setf (slot-value new-task 'priority) (cdrassoc :priority task))
			(setf (slot-value new-task 'postponed) (from-rtm-type 'bool (cdrassoc :postponed task)))
			(setf (slot-value new-task 'estimate) (cdrassoc :estimate task))
			(mapcar #'(lambda (n) (setf (get-task n) new-task)) (get-notes new-task))
			new-task))))
	  (let ((taskseries (cdrassoc :taskseries list)))
	    (if (atom (caar taskseries))
		(list taskseries)
		taskseries))))
     (rtm:rtm-api-tasks-get-list (get-id wanted-list)
				 :filter filter
				 :last-sync last-sync)))))

(defun rtm-refresh-all-lists (&key smart-only exclude-completed)
  (dolist (l (get-task-lists *rtm-user-info*))
    (unless (and smart-only (not (is-smart l))) 
      (rtm-refresh-list l :exclude-completed exclude-completed))))

(defmethod rtm-refresh-list ((list task-list) &key exclude-completed)
  (setf (get-tasks list) (rtm-list-tasks-on-list list :exclude-completed exclude-completed))
  (awhen (find-by-id (get-id list) (get-task-lists-list))
    (unless (eq list it)
      (setf (get-tasks it) (get-tasks list)))))

(defmethod rtm-refresh-list ((list-id simple-base-string) &key exclude-completed)
  (rtm-refresh-list (find-by-id list-id (get-task-lists *rtm-user-info*))
		     :exclude-completed exclude-completed))

;; when RTM API supports reading of smartlist criteria, i could parse it and compute their value again, offline.
;; for now, a remote update is required.

(defmethod rtm-add-task ((list task-list) name &optional (parse-date-in-name-p nil))
  (let ((temp-id (make-offline-id "task")))
    (when (is-smart list)
      (setf list (find-by-criteria "Inbox" #'get-name (get-task-lists-list))))
    (make-offline-task
     ;;remote task
     ;;check whether the task is still on this list:
     (awhen (find-by-id temp-id (get-tasks list))
       (let* ((alist (cdrassoc :list (rtm:rtm-api-tasks-add (get-id list)
							    name
							    (to-rtm-type 'bool parse-date-in-name-p))))
	      (taskseries (cdrassoc :taskseries alist))
	      (task (cdrassoc :task taskseries))
	      (task-id (cdrassoc :id task)))
	 
	 ;; update the stub id so that further changes on that task can be modified correctly:
	 (let ((offline-task it))
	   (setf (get-id offline-task) task-id
		 (get-taskseries-id offline-task) (cdrassoc :id taskseries)))
	 
	 ;; To reflect smartlists, one should update them:
	 (rtm-refresh-all-lists :smart-only t)))
     
     
     ;;local task
     (let ((offline-task (make-instance 'task
					:list-id (get-id list)
					:name    name
					:task-id temp-id)))
       (pushnew offline-task
		(get-tasks list)
		:key #'get-id
		:test #'string=)))))

(defmethod find-real-task ((task task))
  "usefull for when you have a task that may no longer be the same instance after a refresh."
  (find-by-id (get-id task)
	      (get-tasks (find-by-id (get-list-id task)
				     (get-task-lists *rtm-user-info*)))))

(defmethod rtm-add-tags ((task task) (tags list))
  ;; capture the new tags set and pass it to the next two closures:
  (let ((new-tags (set-difference tags (get-tags task) :test #'string=)))
    (make-offline-task
     ;;remote
     (rtm:rtm-api-tasks-add-tags (get-list-id task)
				 (get-taskseries-id task)
				 (get-id task)
				 new-tags)
     ;;local
     (setf (get-tags task)
	   (append (get-tags task) new-tags)))))

(defmethod rtm-remove-tags ((task task) (tags list))
  (unless (intersection tags (get-tags task) :test #'string=)
    (make-offline-task
     (rtm:rtm-api-tasks-remove-tags (get-list-id task)
				    (get-taskseries-id task)
				    (get-id task)
				    tags)
     (setf (get-tags task)
	   (set-difference (get-tags task) tags)))))

(defmethod rtm-change-task-tags ((task task) (tags list))
  (let* ((task-tags (get-tags task))
	 (task-tags-cdr (cdr task-tags))
	 (desired-task-tags (if (stringp task-tags-cdr) task-tags task-tags-cdr)))
    (unless (equal tags desired-task-tags)
      (make-offline-task
       (rtm:rtm-api-tasks-set-tags (get-list-id task)
				   (get-taskseries-id task)
				   (get-id task)
				   tags)
       (setf (get-tags task) (list tags))))))


(defmethod rtm-change-task-tags ((task task) (tags simple-base-string))
  (let ((strings (split-tokens tags)))
    (rtm-change-task-tags task strings)))


(defmethod rtm-complete-task ((task task))
  (when (string= "" (get-completed task)) ;; else it's already completed.
    (make-offline-task
     (let ((result
	    (rtm:rtm-api-tasks-complete (get-list-id task)
					(get-taskseries-id task)
					(get-id task))))
       (setf (get-completed task)
	     (cdrassocchain '(:completed :task :taskseries :list) result)))
     (setf (get-completed task) (get-current-time-string))))) 


(defmethod rtm-uncomplete-task ((task task))
  (unless (string= "" (get-completed task)) ;; it's completed
    (make-offline-task
     (rtm:rtm-api-tasks-uncomplete (get-list-id task)
				   (get-taskseries-id task)
				   (get-id task))
     (setf (get-completed task) ""))))


(defmethod rtm-delete-task ((task task))
  (let ((list-id (get-list-id task)))

    (make-offline-task

     (unless (or (string= "" (get-taskseries-id task))
		 (offline-id-p (get-id task))) ;; otherwise there is no such task online, yet!
       (let ((result
	      (rtm:rtm-api-tasks-delete list-id
					(get-taskseries-id task)
					(get-id task))))
	 (setf (get-deleted task)
	       (cdrassocchain '(:deleted :task :taskseries :list) result))
	 ;; (rtm-refresh-list list-id)
	 ;; To reflect smartlists, one should recalculate all lists again:
	 (rtm-refresh-all-lists :smart-only t)))
     
     (awhen (find-by-id list-id (get-task-lists-list))
       (setf (get-tasks it)
	     (remove (get-id task)
		     (get-tasks it)
		     :key #'get-id
		     :test #'string=))))))

(defmethod rtm-move-task-priority ((task task) move-up-p)
  (make-offline-task

   (let ((result
	  (rtm:rtm-api-tasks-move-priority (get-list-id task)
					   (get-taskseries-id task)
					   (get-id task)
					   (if move-up-p "up" "down"))))
     (setf (get-priority task)
	   (cdrassocchain '(:priority :task :taskseries :list) result)))
   
   (let* ((priorities '("N" "1" "2" "3"))
	  (current-priority-index (position (get-priority task) priorities :test #'string=))
	  (new-tentative-index (funcall (if move-up-p #'+ #'-) current-priority-index 1))
	  (new-index (cond ((> new-tentative-index 4) 4)
			   ((< new-tentative-index 0) 0)
			   (t new-tentative-index)
			   )))
     (setf (get-priority task) (nth new-index priorities)))))

(defmethod rtm-move-task-to-list ((task task) (list task-list))
  (unless (equal (get-id list) (get-list-id task))
    (make-offline-task

     (rtm:rtm-api-tasks-move-to (get-list-id task)
				(get-id list)
				(get-taskseries-id task)
				(get-id task))
     
     (let ((old-list (find-by-id (get-list-id task)
				 (get-task-lists *rtm-user-info*))))
       (setf (get-list-id task) (get-id list))
       (when old-list
	 (setf (get-tasks old-list)
	       (delete-if (lambda (x) (string= (get-id x) (get-id task)))
			  (get-tasks old-list))))
       (pushnew task (get-tasks list) :key #'get-id :test #'string=)))))

(defmethod rtm-postpone-task ((task task))
  (make-offline-task
   (let ((result
	  (rtm:rtm-api-tasks-postpone (get-list-id task)
				      (get-taskseries-id task)
				      (get-id task))))
     
     (setf (get-due task)
	   (cdrassocchain '(:due :task :taskseries :list) result)))
   (progn
     (setf (get-postponed task) t)
     ;; TODO: add a day to the due date
     )))

(defmethod rtm-change-task-due-date ((task task) due-date &key (has-due-time-p t) parse-p)
  (unless (string= (rtm:get-due task) due-date)
    (setf has-due-time-p (if (string= "" due-date) nil has-due-time-p))
    (make-offline-task
     (let ((result
	    (rtm:rtm-api-tasks-set-due-date (get-list-id task)
					    (get-taskseries-id task)
					    (get-id task)
					    due-date
					    (to-rtm-type 'bool has-due-time-p)
					    (to-rtm-type 'bool parse-p))))
       ;; update to reflect parsed date/times:
       (setf (get-due task) 
	     (cdrassocchain '(:due :task :taskseries :list) result)))
     (progn
       (setf (get-due task) due-date
	     (has-due-time task) has-due-time-p)))))

(defmethod rtm-change-task-estimate ((task task) estimate)
  (unless (equal estimate (get-estimate task))
    (make-offline-task
     (let ((result
	    (rtm:rtm-api-tasks-set-estimate (get-list-id task)
					    (get-taskseries-id task)
					    (get-id task)
					    estimate)))
       (setf (get-estimate task)
	     (cdrassocchain '(:estimate :task :taskseries :list) result)))
     
     (setf (get-estimate task) estimate))))

(defmethod rtm-change-task-location ((task task) (location list))
  (when (null location)
    (make-offline-task
     (rtm:rtm-api-tasks-set-location (get-list-id task)
				     (get-taskseries-id task)
				     (get-id task)
				     "")
     (setf (get-location-id task) nil))))

(defmethod rtm-change-task-location ((task task) (location location))
  (unless (or (null location)                                   ;; there must be a location
	      (equal (get-location-id task) (get-id location))) ;; differing than the one in task
    (make-offline-task
     (rtm:rtm-api-tasks-set-location (get-list-id task)
				     (get-taskseries-id task)
				     (get-id task)
				     (get-id location))
     (setf (get-location-id task) (get-id location)))))

(defmethod rtm-change-task-location ((task task) (location-name simple-base-string))
  ;; if the location name string exists, use its location obj. else, create it first
  (awhen (find-by-criteria location-name #'get-name (get-locations *rtm-user-info*))
    (rtm-change-task-location task it)))



(defmethod rtm-change-task-name ((task task) name)
  (unless (string= (get-name task) name)
    (make-offline-task
     (rtm:rtm-api-tasks-set-name (get-list-id task)
				 (get-taskseries-id task)
				 (get-id task)
				 name)
     (setf (get-name task) name))))

(defmethod rtm-change-task-priority ((task task) priority)
  (unless (equal priority (get-priority task))
    (make-offline-task
     (rtm:rtm-api-tasks-set-priority (get-list-id task)
				     (get-taskseries-id task)
				     (get-id task)
				     priority)
     (setf (get-priority task)
	   priority))))

(defmethod rtm-change-task-recurrence ((task task) recurrence)
  (make-offline-task
   (let* ((result (rtm:rtm-api-tasks-set-recurrence (get-list-id task)
						    (get-taskseries-id task)
						    (get-id task)
						    recurrence))
	  (rrule (cdrassocchain '(:rrule :taskseries :list) result)))
     (setf (get-recurrence task)
	   `((:every . ,(when (from-rtm-type 'bool
					     (cdrassoc :every rrule))
			      (cdrassoc :$t rrule)))
	     (:after . ,(when (from-rtm-type 'bool
					     (cdrassoc :after rrule))
			      (cdrassoc :$t rrule))))))

   (setf (get-recurrence task) '((:every . t)
				 (:after . t))))) ;; TODO: make this recurrence value valid.

(defmethod rtm-change-task-url ((task task) url)
  (unless (equal url (get-url task))
    (make-offline-task
     (rtm:rtm-api-tasks-set-url (get-list-id task)
				(get-taskseries-id task)
				(get-id task)
				url)
     (setf (get-url task)
	   url))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note
(defclass note (rtm-object)
  ((id       :accessor get-id       :initarg :id)
   (created  :accessor get-created  :initarg :created)
   (modified :accessor get-modified :initarg :modified)
   (task     :accessor get-task     :initarg :task)
   (title    :accessor get-title    :initarg :title)
   (contents :accessor get-contents :initarg :contents)))

(defmethod rtm-add-note-to-task ((task task) title text)
  (let ((temp-id (make-offline-id "note")))
    (make-offline-task
     ;;remote
     (let ((note (find-by-id temp-id (get-notes task)))
	   (alist (rtm:rtm-api-tasks-notes-add
		   (get-list-id task)
		   (get-taskseries-id task)
		   (get-id task)
		   title
		   text)))
       (setf (get-id note) (cdrassoc :id alist)
	     (get-created note) (cdrassoc :created alist)
	     (get-modified note) (cdrassoc :modified alist)
	     (get-title note) (cdrassoc :title alist)
	     (get-contents note) (cdrassoc :$T alist)))
     ;;local
     (let ((offline-note
	    (make-instance 'note
			   :id temp-id
			   :created (get-current-time-string) 
			   :modified (get-current-time-string)
			   :task task
			   :title title
			   :contents text)))
       (pushnew offline-note (get-notes task) :key #'get-id :test #'string=)
       offline-note))))


(defmethod rtm-delete-note ((n note))
  (make-offline-task
   (rtm:rtm-api-tasks-notes-delete (get-id n))
   (setf (get-notes (get-task n))
	 (remove-if (lambda (x) (string= (get-id x) (get-id n)))
		    (get-notes (get-task n))))))

(defmethod rtm-edit-note ((n note) new-title new-text)
  (make-offline-task
   (let ((alist (rtm:rtm-api-tasks-notes-edit (get-id n) new-title new-text)))
     (setf (get-modified n) (cdrassoc :modified alist))
     n)
   (progn
     (setf (get-title n)    new-title
	   (get-contents n) new-text)
     n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object initialization

(defun init-rtm ()
  "Assumes we already got a token or checked the current one."
  (let ((result (make-instance 'rtm-user-info)))
    (setf *rtm-user-info* result
	  (get-id         *rtm-user-info*) (cdrassoc :id       rtm:*rtm-api-user*)
	  (get-username   *rtm-user-info*) (cdrassoc :username rtm:*rtm-api-user*)
	  (get-fullname   *rtm-user-info*) (cdrassoc :fullname rtm:*rtm-api-user*))
    result))

(defun refresh-rtm (&key exclude-completed)
  "Reloads data from the network"
  (refresh-contact-list)
  (refresh-contact-group-list)
  (refresh-location-list)
  (refresh-task-lists-list)
  (rtm-refresh-all-lists :exclude-completed exclude-completed))


;;; Tests:
;; (rtm-list-task-lists)

;; (rtm-add-note-to-task (nth 0 (rtm-list-tasks-on-list (nth 0 (get-task-lists *rtm-user-info*)))
;; 	      "aaa" "eeee")

;; (rtm-delete-note (nth 0 (get-notes (nth 0 (rtm-list-tasks-on-list (nth 4 (get-task-lists *rtm-user-info*)))))))

;; (rtm-edit-note (nth 0 (get-notes (nth 0 (rtm-list-tasks-on-list (nth 4 (get-task-lists *rtm-user-info*))))))
;; 	       "yadda" "weeee!")

;;(init-rtm)



#|
The MIT License

Copyright (c) 2008, 2009, 2010 Edgar Gonçalves

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#
