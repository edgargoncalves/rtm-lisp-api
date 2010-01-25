
(asdf:operate 'asdf:load-op 'drakma)   ;; HTTP client
(asdf:operate 'asdf:load-op 'cl-json)  ;; JSON parser
(asdf:operate 'asdf:load-op 'ironclad) ;; Encryption library

(defpackage #:rtm-lisp-api-offline
  (:nicknames #:offline)
  (:use :cl)
  (:export
   #:perform-remote-task
   #:undo-task
   #:offline-remote-sync-all
   #:make-offline-task
   #:make-offline-id
   #:offline-id-p))


(defpackage #:rtm-lisp-api
  (:nicknames #:rtm)
  (:use :cl #:drakma #:json #:offline)
  (:export

   ;; Object names:
   #:rtm-user-info
   #:contact
   #:contact-group
   #:task-list
   #:task
   #:location
   #:note

   ;; Variables:
   #:rtm-api-key
   #:rtm-api-shared-secret
   #:rtm-api-endpoint
   #:rtm-api-auth-endpoint

   ;; Dynamic variables
   #:*last-transaction*
   #:*rtm-api-current-frob*
   #:*rtm-api-token*
   #:*rtm-api-perms*
   #:*rtm-api-user*

   ;; Functions 
   #:compute-rtm-api-sig
   #:rtm-api-timelines-create

   #:with-timeline

   #:rtm-api-transactions-undo
   #:rtm-api-call-method
   #:rtm-api-invalid-frob-p
   #:rtm-api-get-frob
   #:rtm-api-build-auth-url

   #:rtm-api-get-token
   #:rtm-api-check-token
   #:rtm-api-contacts-add
   #:rtm-api-contacts-delete
   #:rtm-api-contacts-get-list
   #:rtm-api-groups-add
   #:rtm-api-groups-add-contact
   #:rtm-api-groups-delete
   #:rtm-api-groups-get-list
   #:rtm-api-groups-remove-contact
   #:rtm-api-lists-add
   #:rtm-api-lists-archive
   #:rtm-api-lists-delete
   #:rtm-api-lists-get-list
   #:rtm-api-lists-set-default-list
   #:rtm-api-lists-set-name
   #:rtm-api-lists-unarchive
   #:rtm-api-locations-get-list
   #:rtm-api-reflection-get-method-info
   #:rtm-api-reflection-get-methods
   #:rtm-api-settings-get-list
   #:rtm-api-tasks-add
   #:rtm-api-tasks-add-tags
   #:rtm-api-tasks-complete
   #:rtm-api-tasks-delete
   #:rtm-api-tasks-get-list
   #:rtm-api-tasks-move-priority
   #:rtm-api-tasks-move-to
   #:rtm-api-tasks-postpone
   #:rtm-api-tasks-remove-tags
   #:rtm-api-tasks-set-due-date
   #:rtm-api-tasks-set-estimate
   #:rtm-api-tasks-set-location
   #:rtm-api-tasks-set-name
   #:rtm-api-tasks-set-priority
   #:rtm-api-tasks-set-recurrence
   #:rtm-api-tasks-set-tags
   #:rtm-api-tasks-set-url
   #:rtm-api-tasks-uncomplete
   #:rtm-api-tasks-notes-add
   #:rtm-api-tasks-notes-delete
   #:rtm-api-tasks-notes-edit
   #:rtm-api-test-echo
   #:rtm-api-test-login
   #:rtm-api-time-convert
   #:rtm-api-time-parse
   #:rtm-api-timezones-get-list

   ;; model functions
   #:init-rtm
   #:refresh-rtm
   ;; model variables
   #:*rtm-user-info*

   #:rtm-user-info
   #:get-id                 
   #:get-username           
   #:get-fullname           
   #:get-permissions        
   #:get-contacts           
   #:get-contact-groups     
   #:get-locations          
   #:get-task-lists         
   #:get-default-task-lists

   #:rtm-refresh-list
   #:rtm-refresh-all-lists

   #:contact
   #:rtm-delete-contact
   #:rtm-add-contact
   #:get-contact-list
   #:refresh-contact-list
   #:list-contacts
   #:contact-group
   #:rtm-delete-contact-group
   #:rtm-add-contact-group
   #:rtm-add-contact-to-group
   #:rtm-remove-contact-from-group
   #:get-contact-group-lists
   #:refresh-contact-group-lists
   #:list-contact-groups

   #:task-list
   #:get-name      
   #:is-archived   
   #:is-smart      
   #:get-position  
   #:is-locked     
   #:is-deleted    
   #:get-sort-order
   #:get-filter    
   #:get-tasks

   #:rtm-add-task-list
   #:rtm-delete-task-list
   #:rtm-archive-task-list
   #:rtm-unarchive-task-list
   #:rtm-set-default-task-list
   #:rtm-change-task-list-name
   #:get-task-lists-list
   #:refresh-task-lists-list
   #:rtm-list-task-lists
   #:get-count-for-list-named

   #:location
   #:get-name     
   #:get-id       
   #:get-longitude
   #:get-latitude 
   #:get-zoom     
   #:get-address
   #:get-location-url
   #:get-location-image-url
   #:is-viewable
   #:get-location-list
   #:refresh-location-list
   #:list-locations

   #:task
   #:get-list-id       
   #:get-taskseries-id 
   #:get-created       
   #:get-modified      
   #:get-name          
   #:get-source        
   #:get-url           
   #:get-location-id   
   #:get-tags          
   #:get-recurrence    
   #:get-participants  
   #:get-notes         
   #:get-due           
   #:has-due-time      
   #:get-added         
   #:get-completed     
   #:get-deleted       
   #:get-priority      
   #:get-postponed     
   #:get-estimate      
   #:get-location
   #:get-list
   #:rtm-list-tasks-on-list
   #:rtm-refresh-all-lists
   #:rtm-refresh-list
   #:rtm-refresh-list
   #:rtm-add-task
   #:rtm-add-tags
   #:rtm-remove-tags
   #:rtm-change-task-tags
   #:rtm-complete-task
   #:rtm-uncomplete-task
   #:rtm-delete-task
   #:rtm-move-task-priority
   #:rtm-move-task-to-list
   #:rtm-postpone-task
   #:rtm-change-task-due-date
   #:rtm-change-task-estimate
   #:rtm-change-task-location
   #:rtm-change-task-name
   #:rtm-change-task-priority
   #:rtm-change-task-recurrence
   #:rtm-change-task-url

   #:note
   #:rtm-add-note-to-task
   #:rtm-delete-note
   #:rtm-edit-note
   #:get-task
   #:get-title
   #:get-contents))
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
