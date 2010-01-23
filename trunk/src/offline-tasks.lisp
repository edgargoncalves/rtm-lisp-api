;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; offline-tasks.lisp
;;;   - support for producing offline tasks:
;;;       -> local forms are executed immediately;
;;;       -> remote/online forms are stored on `OFFLINE-BUFFER';
;;;       -> sync those forms tentatively with `OFFLINE-REMOTE-SYNC-ALL'.
;;;   - requires ccl's locks and processes (threads).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:offline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Offline work support

(defclass offline-task ()
  ((online-form :accessor get-online-form :initarg :online-form)
   (local-form  :accessor get-local-form  :initarg :local-form)
   (undo-form   :accessor get-undo-form   :initarg :undo-form)
   (time        :accessor get-time        :initarg :time))
  (:documentation "`ONLINE-FORM' is a list that, when evaluated, updates the remote state.
`UNDO-FORM' is a lambda form that must be applied in order for the local state to be reverted (in case remote update fails)."))

(defvar offline-buffer (list))
(defvar offline-state-needs-remote-sync nil)
(defparameter *buffer-lock* (ccl:make-lock))
(defparameter *sync-lock*   (ccl:make-lock))


(defmethod perform-remote-task ((task offline-task))
  (declare (special offline-buffer))
  (handler-case (progn
		  ;;TODO - try to remove this. large grain lock can be tough...
		  (funcall (get-online-form task))
		  (ccl:with-lock-grabbed (*buffer-lock*)
		    (setf offline-buffer (remove task offline-buffer))
		    (unless offline-buffer
		      (setf offline-state-needs-remote-sync nil))))
    ;; in case a network error occurs, don't do anything.
    ;; in case of a rtm error, we should engage in a need-to-undo state (error)
    ;; (rtm-error (condition)
    ;;   (error condition))
    (error (condition)
      (format t "Can't perform online task: ~s~%" condition)
      (error condition))))
    
(defmethod undo-task ((task offline-task))
  (eval (get-undo-form task)))

(defmacro make-offline-task (online-form local-form &optional undo-form &key (sync-immediatly t) (run-online-only nil))
  `(let ((task (make-instance 'offline-task
			      :online-form #'(lambda () ,online-form)
			      :local-form  #'(lambda () ,local-form)
			      :undo-form   #'(lambda () ,undo-form))))
     (declare (special offline-state-needs-remote-sync offline-buffer))

     (if ,run-online-only
	 (progn ,local-form ,online-form)
	 (progn
	   (ccl:with-lock-grabbed (*buffer-lock*)
	     (push task offline-buffer))
	   (let ((offline-result ,local-form))
	     (setf offline-state-needs-remote-sync t)
	     (when ,sync-immediatly
	       (offline-remote-sync-all))
	     offline-result)))))


(defun offline-remote-sync-all ()
  (ccl:process-run-function
   "run-online-task"
   #'(lambda ()
       (declare (special offline-buffer))
       ;; wait on a free lock before we can proceed:
       (ccl:with-lock-grabbed (*sync-lock*)
	 (let ((task-list (reverse offline-buffer)))
	   ;; offline-buffer items will be deleted, so we have another copy of this list.
	   (dolist (task task-list)
	     (perform-remote-task task)))))))

(defun make-offline-id (type)
  (format nil "offline-~s" (gensym type)))

(defun offline-id-p (id)
  (when (stringp id)
    (let ((position (search "offline-" id)))
      (and position (= position 0)))))

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
