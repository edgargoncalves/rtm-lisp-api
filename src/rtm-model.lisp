(in-package :rtm-lisp-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(defun from-rtm-type (type value)
  (case type
    (bool (string= "1" value))
    (t value)))

(defun to-rtm-type (type value)
  (case type
    (bool (if value "1" "0"))
    (t value)))

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

;; (cdrassocchain '(:rrule :taskseries :list) '((:list . ((:taskseries . ((:rrule . "ola")))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object model for the RTM entities

;;; User information
(defclass rtm-user-info ()
  ((id             :accessor get-id             :initarg :id)
   (username       :accessor get-username       :initarg :username)
   (fullname       :accessor get-fullname       :initarg :fullname)
   (permissions    :accessor get-permissions    :initarg :permissions)
   (contacts       :accessor get-contacts       :initarg :contacts       :initform nil)
   (contact-groups :accessor get-contact-groups :initarg :contact-groups :initform nil)
   (locations      :accessor get-locations      :initarg :locations      :initform nil)
   (task-lists     :accessor get-task-lists     :initarg :task-lists     :initform nil)))

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
  (rtm:rtm-api-contacts-delete (get-id c))
  (delete-if #'(lambda (x) (string= (get-id x) (get-id x)))
	     (get-contacts *rtm-user-info*)))

(defun rtm-add-contact (username-or-email)
  (let* ((alist (rtm:rtm-api-contacts-add username-or-email))
	 (new-contact 
	  (make-instance 'contact
			 :id (cdrassoc :id alist)
			 :username (cdrassoc :username alist)
			 :fullname (cdrassoc :fullname alist))))
    (pushnew new-contact
	     (get-contacts *rtm-user-info*)
	     :key #'get-id
	     :test #'string=)
    new-contact))

(defun list-contacts ()
  (let ((contacts (mapcar
		   #'(lambda (c)
		       (make-instance 'contact
				      :username   (cdrassoc :username c)
				      :fullname   (cdrassoc :fullname c)
				      :id (cdrassoc :id c)))
		   (rtm:rtm-api-contacts-get-list))))
    (setf (get-contacts *rtm-user-info*)
	  contacts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Groups (of contacts)
(defclass contact-group (rtm-object)
  ((id       :accessor get-id       :initarg :id)
   (name     :accessor get-name     :initarg :name)
   (contacts :accessor get-contacts :initarg :contacts :initform nil)))

(defmethod rtm-delete-contact-group ((g contact-group))
  (rtm:rtm-api-groups-delete (get-id g))
  (delete-if (lambda (x) (string= (get-id x) (get-id g)))
	     (get-contact-groups *rtm-user-info*)))

(defun rtm-add-contact-group (name)
  (let* ((alist (rtm:rtm-api-groups-add name))
	 (new-group
	  (make-instance 'contact-group
			 :id       (cdrassoc :id alist)
			 :name     (cdrassoc :name alist))))
    (pushnew new-group
	     (get-contact-groups *rtm-user-info*)
	     :key #'get-id
	     :test #'string=)
    new-group))

(defmethod rtm-add-contact-to-group ((c contact) (g contact-group))
  (rtm:rtm-api-groups-add-contact (get-id g) (get-id c))
  (pushnew c (get-contacts g) :key #'get-id :test #'string=))

(defmethod rtm-remove-contact-from-group ((c contact) (g contact-group))
  (rtm:rtm-api-groups-remove-contact (get-id g) (get-id c))
  (delete-if (lambda (x) (string= (get-id x) (get-id c)))
	     (get-contacts g)))

(defun list-contact-groups ()
  "Fetches contacts, then fetches groups and sorts contacts into them."
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

;;(list-contact-groups)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists (of tasks)
(defclass task-list (rtm-object)
  ((id         :accessor get-id         :initarg :id)
   (name       :accessor get-name       :initarg :name)
   (archived   :accessor is-archived    :initarg :archived)
   (smart      :accessor is-smart       :initarg :smart)
   (position   :accessor get-position   :initarg :position)
   (locked     :accessor is-locked      :initarg :locked)
   (deleted    :accessor get-deleted    :initarg :deleted)
   (sort-order :accessor get-sort-order :initarg :sort-order)
   (tasks      :accessor get-tasks      :initarg :tasks :initform nil)))

(defun rtm-add-task-list (name &optional filter)
  (let* ((alist (rtm:rtm-api-lists-add name (or filter "")))
	 (new-list
	  (make-instance 'task-list
			 :id       (cdrassoc :id alist)
			 :name     (cdrassoc :name alist)
			 :smart    (from-rtm-type 'bool (cdrassoc :smart    alist))
			 :deleted  (from-rtm-type 'bool (cdrassoc :deleted  alist))
			 :locked   (from-rtm-type 'bool (cdrassoc :locked   alist))
			 :position (from-rtm-type 'bool (cdrassoc :position alist))
			 :archived (from-rtm-type 'bool (cdrassoc :archived alist)))))
    (pushnew new-list
	     (get-task-lists *rtm-user-info*)
	     :key #'get-id
	     :test #'string=)
    
    new-list))

(defmethod rtm-delete-task-list ((tl task-list))
  (rtm:rtm-api-lists-delete (get-id tl))
  (setf (get-deleted tl) t)
  (let ((x (delete-if (lambda (x) (string= (get-id x) (get-id tl)))
		      (get-task-lists *rtm-user-info*))))
    (declare (ignore x))
    ;; (rtm-refresh-all-lists)
    ))

(defmethod rtm-archive-task-list ((tl task-list))
  (rtm:rtm-api-lists-archive (get-id tl))
  (setf (is-archived tl) t))

(defmethod rtm-unarchive-task-list ((tl task-list))
  (rtm:rtm-api-lists-unarchive (get-id tl))
  (setf (is-archived tl) nil))

(defmethod rtm-set-default-task-list ((tl task-list))
  (rtm:rtm-api-lists-set-default-list (get-id tl)))

(defmethod rtm-change-task-list-name ((tl task-list) name)
  (rtm:rtm-api-lists-set-name (get-id tl) name)
  (setf (get-name tl) name)
  ;; (rtm-refresh-all-lists)
  )

(defun rtm-list-task-lists ()
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

(defun list-locations ()
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

;; (list-locations)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Task
(defclass task (rtm-object)
  ((list-id       :accessor get-list-id       :initarg :list-id)
   (taskseries-id :accessor get-taskseries-id :initarg :taskseries-id)
   (task-id       :accessor get-id            :initarg :task-id)
   (created       :accessor get-created       :initarg :created)
   (modified      :accessor get-modified      :initarg :modified)
   (name          :accessor get-name          :initarg :name)
   (source        :accessor get-source        :initarg :source)
   (url           :accessor get-url           :initarg :url)
   (location-id   :accessor get-location-id   :initarg :location-id)
   (tags          :accessor get-tags          :initarg :tags :initform nil)
   (recurrence    :accessor get-recurrence    :initarg :recurrence :initform '((:every) (:after)))
   (participants  :accessor get-participants  :initarg :participants :initform nil)
   (notes         :accessor get-notes         :initarg :notes :initform nil)
   (due           :accessor get-due           :initarg :due)
   (has-due-time  :accessor has-due-time      :initarg :has-due-time)
   (added         :accessor get-added         :initarg :added)
   (completed     :accessor get-completed     :initarg :completed)
   (deleted       :accessor get-deleted       :initarg :deleted)
   (priority      :accessor get-priority      :initarg :priority)
   (postponed     :accessor get-postponed     :initarg :postponed)
   (estimate      :accessor get-estimate      :initarg :estimate)))

(defmethod get-location ((task task))
  (find-by-id (get-location-id task) (get-locations *rtm-user-info*)))

(defmethod get-list ((task task))
  (find-by-id (get-list-id task) (get-task-lists *rtm-user-info*)))

(defun rtm-list-tasks-on-list (wanted-list &key filter last-sync)
  (reverse
   (apply
    #'append
    (mapcar
     #'(lambda (list)
	 (mapcan
	  #'(lambda (taskseries)
	      (when (cdrassoc :task taskseries) ;; Make sure we have no empty taskseries, useless to us...
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
			     (new-task
			      (make-instance 'task
					     :list-id       (cdrassoc :id list)
					     :taskseries-id (cdrassoc :id taskseries)
					     :task-id       (cdrassoc :id task)
					     :created       (cdrassoc :created taskseries)
					     :modified      (cdrassoc :modified taskseries)
					     :name          (cdrassoc :name taskseries)
					     :source        (cdrassoc :source taskseries)
					     :url           (cdrassoc :url taskseries)
					     :location-id   (cdrassoc :location-id taskseries)
					     :tags          tags
					     :recurrence    recurrence
					     :participants  participants
					     :notes         notes
					     :due           (cdrassoc :due task)
					     :has-due-time  (from-rtm-type 'bool (cdrassoc :has_due_time task))
					     :added         (cdrassoc :added task)
					     :completed     (cdrassoc :completed task)
					     :deleted       (cdrassoc :deleted task)
					     :priority      (cdrassoc :priority task)
					     :postponed     (from-rtm-type 'bool (cdrassoc :postponed task))
					     :estimate      (cdrassoc :estimate task))))
			(mapcar #'(lambda (n) (setf (get-task n) new-task)) (get-notes new-task))
			new-task))))
	  (let ((taskseries (cdrassoc :taskseries list)))
	    (if (atom (caar taskseries))
		(list taskseries)
		taskseries))))
     (rtm:rtm-api-tasks-get-list (get-id wanted-list)
				 :filter filter
				 :last-sync last-sync)))))

(defun rtm-refresh-all-lists ()
  (dolist (l (get-task-lists *rtm-user-info*))
    (rtm-refresh-list l)))

(defmethod rtm-refresh-list ((list task-list))
  (setf (get-tasks list) (rtm-list-tasks-on-list list)))

(defmethod rtm-refresh-list ((list-id simple-base-string))
  (rtm-refresh-list (find-by-id list-id (get-task-lists *rtm-user-info*))))


;;(rtm-refresh-all-lists)

(defmethod rtm-add-task ((list task-list) name &optional (parse-date-in-name-p nil))
  (let* ((alist (cdrassoc :list (rtm:rtm-api-tasks-add (get-id list) name (to-rtm-type 'bool parse-date-in-name-p))))
	 (taskseries (cdrassoc :taskseries alist))
	 (task (cdrassoc :task taskseries))
	 (task-id (cdrassoc :id task)))
    ;; To reflect smartlists, one should recalculate all lists again:
    (rtm-refresh-list list)
    (find-by-id task-id (get-tasks list))))

(defmethod rtm-add-tags ((task task) (tags list))
  (let ((new-tags (set-difference tags (get-tags task) :test #'string=)))
    (rtm:rtm-api-tasks-add-tags (get-list-id task)
				(get-taskseries-id task)
				(get-id task)
				new-tags)
    (setf (get-tags task)
	  (append (get-tags task) new-tags))))

(defmethod rtm-remove-tags ((task task) (tags list))
  (unless (intersection tags (get-tags task) :test #'string=)
    (rtm:rtm-api-tasks-remove-tags (get-list-id task)
				   (get-taskseries-id task)
				   (get-id task)
				   tags)
    (setf (get-tags task)
	  (set-difference (get-tags task) tags))))

(defmethod rtm-change-task-tags ((task task) (tags list))
  (let* ((task-tags (get-tags task))
	 (task-tags-cdr (cdr task-tags))
	 (desired-task-tags (if (stringp task-tags-cdr) task-tags task-tags-cdr)))
  (unless (equal tags desired-task-tags)
    (rtm:rtm-api-tasks-set-tags (get-list-id task)
				(get-taskseries-id task)
				(get-id task)
				tags)
    (setf (get-tags task) (list tags))
    task)))


(defmethod rtm-change-task-tags ((task task) (tags simple-base-string))
  (let ((strings (split-tokens tags)))
    (rtm-change-task-tags task strings)))



(defmethod rtm-complete-task ((task task))
  (when (string= "" (get-completed task)) ;; else it's already completed.
    (let ((result
	   (rtm:rtm-api-tasks-complete (get-list-id task)
				       (get-taskseries-id task)
				       (get-id task))))
      (setf (get-completed task)
	    (cdrassocchain '(:completed :task :taskseries :list) result))))
  task)

(defmethod rtm-uncomplete-task ((task task))
  (unless (string= "" (get-completed task)) ;; it's completed
    (rtm:rtm-api-tasks-uncomplete (get-list-id task)
				  (get-taskseries-id task)
				  (get-id task))
    (setf (get-completed task) nil))
  task)


(defmethod rtm-delete-task ((task task))
  (let* ((list-id (get-list-id task))
	 (result
	  (rtm:rtm-api-tasks-delete list-id
				    (get-taskseries-id task)
				    (get-id task))))
    (setf (get-deleted task)
	  (cdrassocchain '(:deleted :task :taskseries :list) result))
    (rtm-refresh-list list-id)
    task))

(defmethod rtm-move-task-priority ((task task) move-up-p)
  (let ((result
	 (rtm:rtm-api-tasks-move-priority (get-list-id task)
					  (get-taskseries-id task)
					  (get-id task)
					  (if move-up-p "up" "down"))))
    (setf (get-priority task)
	  (cdrassocchain '(:priority :task :taskseries :list) result))))

(defmethod rtm-move-task-to-list ((task task) (list task-list))
  (unless (equal (get-id list) (get-list-id task))
    (rtm:rtm-api-tasks-move-to (get-list-id task)
			       (get-id list)
			       (get-taskseries-id task)
			       (get-id task))
    (let ((old-list (find-by-id (get-list-id task)
				(get-task-lists *rtm-user-info*))))
      (setf (get-list-id task) (get-id list))
      (setf (get-tasks old-list)
	    (delete-if (lambda (x) (string= (get-id x) (get-id task)))
		       (get-tasks old-list)))
      (pushnew task (get-tasks list) :key #'get-id :test #'string=))))

(defmethod rtm-postpone-task ((task task))
  (let ((result
	 (rtm:rtm-api-tasks-postpone (get-list-id task)
				     (get-taskseries-id task)
				     (get-id task))))
    (setf (get-postponed task) t)
    (setf (get-due task)
	  (cdrassocchain '(:due :task :taskseries :list) result))
    task))

(defmethod rtm-change-task-due-date ((task task) due-date &key (has-due-time-p t) parse-p)
  (let ((result
	 (rtm:rtm-api-tasks-set-due-date (get-list-id task)
					 (get-taskseries-id task)
					 (get-id task)
					 due-date
					 (to-rtm-type 'bool has-due-time-p)
					 (to-rtm-type 'bool parse-p))))
    (setf (get-due task)
	  (cdrassocchain '(:due :task :taskseries :list) result))
    (setf (has-due-time task)
	  has-due-time-p)
    task))

(defmethod rtm-change-task-estimate ((task task) estimate)
  (unless (equal estimate (get-estimate task))
    (let ((result
	   (rtm:rtm-api-tasks-set-estimate (get-list-id task)
					   (get-taskseries-id task)
					   (get-id task)
					   estimate)))
      (setf (get-estimate task)
	    (cdrassocchain '(:estimate :task :taskseries :list) result))))
  task)

(defmethod rtm-change-task-location ((task task) (location location))
  (unless (equal (get-location-id task) (get-id location))
    (rtm:rtm-api-tasks-set-location (get-list-id task)
				    (get-taskseries-id task)
				    (get-id task)
				    (get-id location))
    (setf (get-location-id task)
	  (get-id location)))
  task)

(defmethod rtm-change-task-location ((task task) (location list))
  (unless (or location (equal (get-location-id task) ""))
    (rtm:rtm-api-tasks-set-location (get-list-id task)
				    (get-taskseries-id task)
				    (get-id task)
				    "")
    (setf (get-location-id task) ""))
  task)

(defmethod rtm-change-task-location ((task task) (location-name simple-base-string))
  ;; if the location name string exists, use its location obj. else, create it first
  (let ((location (find-by-criteria location-name #'get-name (get-locations *rtm-user-info*))))
    (if (and location (not (equal (get-id location) (get-location-id task))))
	(rtm-change-task-location task location)
	;; (format t "Location ~s does not exist on your RTM.~%" location-name)
	)
    task))



(defmethod rtm-change-task-name ((task task) name)
  (unless (string= (get-name task) name)
    (rtm:rtm-api-tasks-set-name (get-list-id task)
				(get-taskseries-id task)
				(get-id task)
				name)
    (setf (get-name task)
	  name))
  task)

(defmethod rtm-change-task-priority ((task task) priority)
  (unless (equal priority (get-priority task))
    (rtm:rtm-api-tasks-set-priority (get-list-id task)
				    (get-taskseries-id task)
				    (get-id task)
				    priority)
    (setf (get-priority task)
	  priority))
  task)

(defmethod rtm-change-task-recurrence ((task task) recurrence)
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
  task)

(defmethod rtm-change-task-url ((task task) url)
  (unless (equal url (get-url task))
    (rtm:rtm-api-tasks-set-url (get-list-id task)
			       (get-taskseries-id task)
			       (get-id task)
			       url)
    (setf (get-url task)
	  url))
  task)


;; TODO: make each *rtm-user-info* update save the last check time, and
;; TODO: only fetch recent changes.

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
  (let ((alist (rtm:rtm-api-tasks-notes-add
		 (get-list-id task)
		 (get-taskseries-id task)
		 (get-id task)
		 title
		 text)))
    (let ((new-note (make-instance 'note
				   :id (cdrassoc :id alist)
				   :created (cdrassoc :created alist)
				   :modified (cdrassoc :modified alist)
				   :task task
				   :title (cdrassoc :title alist)
				   :contents (cdrassoc :$T alist))))
    (pushnew new-note (get-notes task) :key #'get-id :test #'string=)
    new-note)))



(defmethod rtm-delete-note ((n note))
  (when n
    (rtm:rtm-api-tasks-notes-delete (get-id n))
    (delete-if (lambda (x) (string= (get-id x) (get-id n)))
	       (get-notes (get-task n)))))

(defmethod rtm-edit-note ((n note) new-title new-text)
  (let ((alist (rtm:rtm-api-tasks-notes-edit (get-id n) new-title new-text)))
    (format t "~s~%" alist)    
    (setf (get-title    n) new-title
	  (get-contents n) new-text
	  (get-modified n) (cdrassoc :modified alist))
    n))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object initialization

(defun init-rtm ()
  "Assumes we already got a token or checked the current one."
  (let ((result (make-instance 'rtm-user-info)))
    (setf *rtm-user-info*                      result
	  (get-id             *rtm-user-info*) (cdrassoc :id       rtm:*rtm-api-user*)
	  (get-username       *rtm-user-info*) (cdrassoc :username rtm:*rtm-api-user*)
	  (get-fullname       *rtm-user-info*) (cdrassoc :fullname rtm:*rtm-api-user*)
	  (get-contacts       *rtm-user-info*) (list-contacts)
	  (get-contact-groups *rtm-user-info*) (list-contact-groups)
	  (get-locations      *rtm-user-info*) (list-locations)
	  (get-task-lists     *rtm-user-info*) (rtm-list-task-lists))
    (rtm-refresh-all-lists)
    result))


;;; Tests:
;; (rtm-list-task-lists)

;; (rtm-add-note-to-task (nth 0 (rtm-list-tasks-on-list (nth 0 (get-task-lists *rtm-user-info*)))
;; 	      "aaa" "eeee")

;; (rtm-delete-note (nth 0 (get-notes (nth 0 (rtm-list-tasks-on-list (nth 4 (get-task-lists *rtm-user-info*)))))))

;; (rtm-edit-note (nth 0 (get-notes (nth 0 (rtm-list-tasks-on-list (nth 4 (get-task-lists *rtm-user-info*))))))
;; 	       "yadda" "weeee!")

;;(init-rtm)
