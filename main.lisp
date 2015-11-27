;;;;
;;;; Copyright (c) 2015 Kamen Tomov, All Rights Reserved
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

;;;; main.lisp

(defpackage pipicha
  (:use 
   #:common-lisp
   #:kamo-lib)
  (:export
   #:server-join
   #:host-join
   #:welcome
   #:refused-name-unavailable
   #:peer-join
   #:update-host
   #:send-chat-message
   #:bye

   #:chat-join
   #:chat-message
   #:chat-leave

   #:available-chat-commands
   #:env
   #:logwrite
   #:netloc
   #:net-ip
   #:net-port
   #:net-path
   #:peer
   #:server
   #:*world*
   #:*home*
   #:my-addr
   #:my-network-address
   #:parse-address
   #:get-node
   #:instantiate-node
   #:cleanup
   #:peer-data
   #:message
   #:chat-repl
   #:rmt-response
   #:call-type
   #:blocking-call
   #:non-blocking-call
   #:+blocking+
   #:+non-blocking+))

(in-package :pipicha)

(defclass env () ())

(defvar *world*)
(defvar *home* (make-instance 'env)) 

(defgeneric get-node (env &optional address))
(defgeneric instantiate-node (env type &key port path))
(defgeneric cleanup (env node))
(defgeneric available-chat-commands (env))
(defmethod available-chat-commands ((env env))
  (list "join room" "quit" "help"))

(defclass netloc ()
  ((ip :initarg :address :accessor net-ip)
   (port :initarg :port :accessor net-port)
   (path :initarg :path :accessor net-path)))

(defgeneric my-network-address (env netloc)
  (:documentation "Use my-address instead. Before that implement the corresponding method"))

(defgeneric my-addr (netloc))
(defmethod my-addr ((netloc netloc)) (my-network-address *world* netloc))

(defclass node (netloc)
  ((name :initarg :name :accessor node-name)
   (timestamp :initform (get-universal-time) :accessor node-timestamp)))

(defclass server (node)
  ((chat-rooms :initform (make-hash-table :test #'equal)
               :accessor chat-rooms)))

(defclass peer-data ()
  ((address :initarg :address :accessor peer-data-address)
   (name :initarg :name :accessor peer-data-name)))

(defmethod ms:class-persistant-slots ((self peer-data))
  '(address name))

(defclass peer (node)
  ((room-name :initarg :room-name :accessor peer-room-name)
   (messages :initform nil :accessor peer-messages)
   (peers :initform nil :accessor peer-peers)
   (server :initform nil :accessor peer-server)))

(defclass message ()
  ((text :initarg :text :accessor message-text)
   (author :initarg :author :accessor message-author)
   (created :initform (get-universal-time) :accessor message-created)))

(defmethod ms:class-persistant-slots ((self message))
  '(text author created))

(defmacro rmt-response (code)
  (assert (find code '(:response-forwarded :response-created
                       :response-ok :response-removed)))
  code)

(defclass call-type () ())
(defclass blocking-call (call-type) ())
(defclass non-blocking-call (call-type) ())
(defvar +blocking+ (make-instance 'blocking-call))
(defvar +non-blocking+ (make-instance 'non-blocking-call))

;;;;;;;;;;;;;;;;;;
; REMOTE INTERFACE
;;;;;;;;;;;;;;;;;;

(defmacro react (form response)
  `(progn ,form ,response))

(defgeneric server-join (env call-type desination myself my-name chat-room))
(defmethod server-join ((home env) (call-type call-type) (mykingdom server)
                        annoyer annoyers-name room-name)
  "Inform the room host about a peer willing to join the room. If room
  doesn't exist - create it and assign the peer for host."
  (multiple-value-bind (return-value chat-room-host)
      (multiple-value-bind (chat-room-host existing-p)
          (gethash room-name (chat-rooms mykingdom))
        (if existing-p
            (progn
              (host-join *world* call-type chat-room-host
                         (my-addr mykingdom) annoyer annoyers-name)
              (values (rmt-response :response-forwarded) chat-room-host))
            (progn
              (setf (gethash room-name (chat-rooms mykingdom)) annoyer)
              (values (rmt-response :response-created) annoyers-name))))
    (react (display-server-join mykingdom return-value
                                annoyers-name room-name chat-room-host)
           return-value)))

(defgeneric display-server-join (myself response annoyers-name chat-room host))
(defmethod display-server-join ((myself server) response annoyers-name chat-room host)
  (case response
    (:response-created
     (format t "Room `~a' created. Host is `~a'.~%" chat-room host))
    (:response-forwarded
     (format t "`~a' is joining room `~a'. Peer `~a' is the host there.~%"
             annoyers-name chat-room host))
    (otherwise
     (format t "Error while asking host `~a' to add user `~a' to room `~a'.~%"
             host annoyers-name chat-room))))

(defgeneric host-join (where call-type destination myself annoyer annoyers-name))
(defmethod host-join ((home env) (call-type call-type) (mykingdom peer)
                      the-big-annoyer someone someones-name)
  (let ((my-peers (peer-peers mykingdom))
        (my-name (node-name mykingdom)))
    (flet ((duplicated-name ()
             (or (equal someones-name my-name)
                 (find-if #'(lambda (name)
                              (equal someones-name name))
                          my-peers :key #'peer-data-name))))
      (if (duplicated-name)
          (refused-name-unavailable *world* +non-blocking+ someone
                                    (my-addr mykingdom) (my-addr mykingdom))
          (progn
            (mapcar #'(lambda (p)
                        (peer-join *world* +non-blocking+ (peer-data-address p)
                                   (my-addr mykingdom) someone someones-name))
                    my-peers)
            (peer-join home +non-blocking+ mykingdom nil someone someones-name)
            (welcome *world* +non-blocking+ someone (my-addr mykingdom)
                     my-name my-peers (peer-messages mykingdom))))))
  (rmt-response :response-forwarded))
  
(defgeneric welcome (where call-type destination myself my-name
                     my-peers my-chat-history))
(defmethod welcome ((home env) (call-type call-type) (mykingdom peer)
                    annoyer name peers chat-history)
  (setf (peer-peers mykingdom)
        (push (make-instance 'peer-data :address annoyer :name name)
              peers))
  (and chat-history (setf (peer-messages mykingdom) chat-history))
  (react (display-welcome mykingdom chat-history)
         (rmt-response :response-ok)))

(defgeneric display-welcome (myself chat-history))
(defmethod display-welcome ((mykingdom peer) chat-history)
  (format
   t
   "*** ~a, welcome to room `~a'. There is/are `~a' peers here beside you.~%"
          (node-name mykingdom) (peer-room-name mykingdom)
          (length (peer-peers mykingdom)))
  (if (null chat-history)
      (format t "~at's quite a lonely place.~%"
              (if (> (length (peer-peers mykingdom)) 2)
                  "*** Nevertheless i " "*** I")))
  (mapcar #'(lambda (message)
              (display-chat-message mykingdom message))
          (reverse chat-history)))

(defgeneric refused-name-unavailable (where call-type destination
                                      myself messenger-to-kill))
(defmethod refused-name-unavailable ((home env) (call-type call-type)
                                     (mykingdom peer) myself annoyer)
  (react (display-refused-name-unavailable mykingdom)
         (rmt-response :response-ok)))

(defgeneric display-refused-name-unavailable (myself))
(defmethod display-refused-name-unavailable ((myself peer))
  (format t "*** I'm afraid name `~a' is already taken.~%" (node-name myself)))

(defgeneric peer-join (where call-type destination myself annoyer annoyers-name))
(defmethod peer-join ((home env) (call-type call-type)
                      (mykingdom peer) annoyer someone someones-name)
  (declare (ignore annoyer))
  (push (make-instance 'peer-data :address someone :name someones-name)
        (peer-peers mykingdom))
  (react (display-peer-join mykingdom someones-name)
         (rmt-response :response-ok)))

(defgeneric display-peer-join (myself name))
(defmethod display-peer-join ((myself peer) name)
  (format t "*** `~a' has just joined the party at `~a'~%"
          name (peer-room-name myself)))

(defgeneric update-host (where call-type destination myself newboss of-room))
(defmethod update-host ((home env) (call-type call-type) (mykingdom server)
                        annoyer newboss chat-room)
  (multiple-value-bind (return-code)
      (if newboss
          (progn (setf (gethash chat-room (chat-rooms mykingdom)) newboss)
                 (rmt-response :response-created))
          (progn (remhash chat-room (chat-rooms mykingdom))
                 (rmt-response :response-removed)))
    (react (display-update-host mykingdom return-code annoyer newboss chat-room)
           return-code)))

(defgeneric display-update-host (myself test annoyer newboss chat-room))
(defmethod display-update-host ((myself server) response annoyer newboss chat-room)
  (format t "`~a' from room `~a' asked for a management change.~%" annoyer chat-room)
  (if (eq response :response-created)
      (format t "Not surprisingly the new host is `~a'.~%" newboss)
      (write-line "The room has been blown up.")))

(defgeneric send-chat-message (where call-type destination myself msg))
(defmethod send-chat-message ((home env) (call-type call-type)
                              (myself peer) annoyer msg)
  (let ((new-msg (make-instance 'message :text msg :author annoyer)))
    (push new-msg (peer-messages myself))
    (react (and (equal (type-of call-type) 'call-type) ;a hack: fixme
                (display-chat-message
                 myself new-msg
                 (if (equal (my-addr myself) annoyer)
                     (node-name myself))))
           (rmt-response :response-ok))))

(defgeneric display-chat-message (myself message &optional name))
(defmethod display-chat-message ((myself peer) (message message) &optional name)
  (multiple-value-bind (sec min hour)
      (decode-universal-time (message-created message))
    (declare (ignore sec))
    (format t "~2,'0D:~2,'0D ~a: ~a~%" hour min
            (or name (find-peer-data-name myself (message-author message)))
            (message-text message))))

(defgeneric bye (where call-type destination myself server))
(defmethod bye ((home env) (call-type call-type) (mykingdom peer) wimp server)
  (let ((response
         (if server
             (progn (setf (peer-server mykingdom) server)
                    (update-host *world* +blocking+ server (my-addr mykingdom) 
                                 (my-addr mykingdom) (peer-room-name mykingdom))
                    (rmt-response :response-forwarded))
             (rmt-response :response-ok))))
    (react
     (progn (display-bye mykingdom response (find-peer-data-name mykingdom wimp))
            (setf (peer-peers mykingdom)
                  (remove wimp (peer-peers mykingdom)
                          :test #'equal :key #'peer-data-address)))
     response)))

(defgeneric display-bye (myself test name))
(defmethod display-bye ((myself peer) response name)
  (format t "*** ~a `~a' has quit. "
          (if (> (length (peer-peers myself)) 2) "Luckily" "Sadly")
          name)
  (case response
    (:response-ok (write-line "S/He won't be missed so much."))
    (:response-forwarded (write-line "Now we will need a new host."))
    (otherwise (write-line "Unfortunatelly our request to become host has failed."))))


;;;;;;;;;;;;;;;;;
; LOCAL INTERFACE
;;;;;;;;;;;;;;;;;

(defgeneric chat-join (myself server chat-room))
(defmethod chat-join ((mykingdom peer) server chat-room)
  (setf (peer-room-name mykingdom) chat-room)
  (let ((resp
         (case (server-join *world* +blocking+ server (my-addr mykingdom)
                            (node-name mykingdom) chat-room)
           (:response-created
            (progn (setf (peer-server mykingdom) server)
                   mykingdom))
           (:response-forwarded mykingdom)
           (otherwise nil))))
    (setf (node-timestamp mykingdom) (get-universal-time))
    resp))

(defgeneric chat-message (myself msg))
(defmethod chat-message ((mykingdom peer) msg)
  "Sends the message to the peers. Push the message to itself"
  (send-chat-message *home* +blocking+ mykingdom (my-addr mykingdom) msg)
  (mapcar #'(lambda (p)
              (send-chat-message *world* +non-blocking+
                                 (peer-data-address p) (my-addr mykingdom) msg))
          ;fixme check the responses and return depending on overal success
          (peer-peers mykingdom)))

(defgeneric chat-leave (myself))
(defmethod chat-leave ((mykingdom peer))
  (let* ((server (peer-server mykingdom))
         (next-host
          (if server ; if I'm the host
              (let ((election (car (last (peer-peers mykingdom)))))
                (if election 
                    (peer-data-address election) ; I nominate the next host
                    (progn                       ; I ask the server to remove the room
                      (update-host *world* +non-blocking+ server
                                   (my-addr mykingdom) nil (peer-room-name mykingdom))
                      nil))))))
    (mapcar #'(lambda (p)
                (let* ((paddress (peer-data-address p))
                       (serverp (and (equal next-host paddress) server))) ;Tell them if they 
                                                                          ;need to be a host
                  (bye *world* (if serverp +blocking+ +non-blocking+)
                       paddress (my-addr mykingdom) serverp)))
            (peer-peers mykingdom))
    (setf (peer-peers mykingdom) nil     ;Questionable. If not I'd be able to send 
                                         ;messages to the peers.
          (peer-server mykingdom) nil))  ;Questionable.
  mykingdom) 

;;;;;;;;;;;
; INTERFACE
;;;;;;;;;;;

(defgeneric display-help (myself))
(defmethod display-help ((myself peer))
  (write-line "*** You can chat or you can do:")
  (mapcar #'(lambda (cmd)
              (format t "*** /~a~%" cmd))
          (available-chat-commands *world*)))

(defgeneric display-chat-command-error (myself))
(defmethod display-chat-command-error ((myself peer))
  (write-line "Use \help."))

(defgeneric display-zero-text-warning (myself))
(defmethod display-zero-text-warning ((myself peer))
  (write-line "*** Blank line - ignoring ..."))

(defgeneric display-chat-hello (myself))
(defmethod display-chat-hello ((myself peer))
  (write-line "*** Hi there...")
  (let ((name)
        (server))
    (macrolet
        ((helper1 (welcome-msg insist-msg &body body)
           `(progn
              (write-line (format nil "*** ~a: " ,welcome-msg))
              (loop for line = (read-line nil nil nil t)
                 do (if (zerop (length line))
                        (write-line (format nil "*** ~a? " ,insist-msg))
                        (progn ,@body))))))
      (helper1 "Select a nickname" "Nickname"
               (progn (setf name line) (return)))
      `(helper1 "Server (type 'old' to use the current one.)" "Server"
               (if (setf server
                         (cond
                           ((equal line "new") (my-addr (instantiate-node *world* 'server)))
                           ((equal line "old") nil)
                           (t (ignore-errors (parse-address *world* line)))))
                   (return)))
      (write-line "*** Enjoy P2P chat!")
      (display-help myself)
      (values name server))))

(defgeneric display-one-room-limit (myself))
(defmethod display-one-room-limit ((myself peer))
  (write-line "*** Quit first and then you can join another room."))

;;;;;;;;;;;;;
; OTHER STUFF
;;;;;;;;;;;;;

(defgeneric parse-address (where str))

(defgeneric find-peer-data-address (myself name))
(defmethod find-peer-data-address ((myself peer) name)
  (if (equal (node-name myself) name)
      (my-addr myself)
      (let ((res (find name (peer-peers myself) :key #'peer-data-name :test #'equal)))
        (if res (peer-data-address res)))))

(defgeneric find-peer-data-name (myself address))
(defmethod find-peer-data-name ((myself peer) address)
  (if (equal (my-addr myself) address)
      (node-name myself)
      (let ((res (find address (peer-peers myself) :key #'peer-data-address :test #'equal)))
        (if res (peer-data-name res)))))

(defun comprehend-command (line)
  (if (and line (not (equal line "")) (eq (aref line 0) #\/))
      (let* ((split-line (split-by-one-char line))
             (cmd (car split-line)))
        (flet ((verify-command (str &optional argc)
                 (and (equal str cmd)
                      (if argc 
                          (= (length split-line) argc)
                          t))))
          (cons
           (cond ((verify-command "/help" 1) 1)
                 ((verify-command "/quit" 1) 3)
                 ((verify-command "/join" 2) 4)
                 ((verify-command "/sudoadd" 2) 5)
                 ((verify-command "/sudojoin" 3) 6)
                 ((verify-command "/sudomsg") 7)
                 ((verify-command "/sudoquit" 2) 9)
                 (t 0))
           (cdr split-line))))
      (list 16 line)))

(defun chat-repl (myself server)
  (let ((local-nodes))
    (flet ((find-local-peer-by-name (name)
             (if (equal (node-name myself) name)
                 myself
                 (find name local-nodes :key #'node-name :test #'equal)))
           (userher (name)
             (get-node *world* (find-peer-data-address myself name)))
           (local-add (node)
             (push node local-nodes) node)
           (local-del (node)
             (setf local-nodes
                   (remove (my-addr node) local-nodes :test #'equal :key #'my-addr))
             node))
      (multiple-value-bind (peer-name-chosen desired-server)
          (display-chat-hello myself)
        (setf (node-name myself) peer-name-chosen
              server (or desired-server server))
        (loop for line = (read-line)
           for cmd = (comprehend-command line)
           do (case (car cmd)
                (1 (display-help myself))   ;help
                (3 (progn (chat-leave myself) ;quit
                          (cleanup *world* myself)
                          (return)))
                (4 (progn               ;join
                     (if (slot-boundp myself 'room-name)
                         (display-one-room-limit myself)
                         (chat-join myself server (second cmd)))))
                (16 (if (zerop (length line)) ;msg
                        (display-zero-text-warning myself)
                        (chat-message myself line))) 
                (5 (setf (node-name (local-add (instantiate-node *world* 'peer))) ;sudoadd
                         (second cmd)))
                (6 (chat-join (find-local-peer-by-name (second cmd)) ;sudojoin
                              server (third cmd)))
                (7 (if (zerop (length (third cmd))) ;sudomsg
                       (display-zero-text-warning (userher (second cmd)))
                       (chat-message (userher (second cmd)) (concatenate-list (cddr cmd)))))
                (9 (cleanup *world* (local-del (chat-leave (userher (second cmd)))))) ;sudoquit
                (0 (display-chat-command-error myself)))))))) ;error

(defmethod print-object ((ne netloc) s)
  (format s "#<netloc ~a:~a/~a>" (net-ip ne) (net-port ne) (net-path ne)))

(defmethod print-object ((ne server) s)
  (let ((ut (get-universal-time)))
    (format s "#<server:~a, Name:~a, Created:~a, Rooms: "
            (my-addr ne)
            (and (slot-boundp ne 'name) (node-name ne))
            (- ut -3 (node-timestamp ne)))
    (maphash #'(lambda (key val)
                 (format s "~%#<chat-room:~a host:~a> " key val))
             (chat-rooms ne))))

(defmethod print-object ((ne peer) s)
  (let ((ut (get-universal-time)))
    (format s "#<peer:~a, Name:~a, Created:~a, Room:~a, Server:~a, Peers:~a, Messages:~a>"
            (my-addr ne)
            (and (slot-boundp ne 'name) (node-name ne))
            (- ut -3 (node-timestamp ne))
            (and (slot-boundp ne 'room-name) (peer-room-name ne))
            (peer-server ne)
            (and (slot-boundp ne 'peers) (peer-peers ne))
            (and (slot-boundp ne 'messages) (peer-messages ne)))))  

(defmethod print-object ((m message) s)
  (format s "#<message ~a: ~a>"
          (message-author m) (message-text m)))

(defmethod print-object ((p peer-data) s)
  (format s "#<peer-data: ~a name: ~a>"
            (peer-data-address p) (peer-data-name p)))

