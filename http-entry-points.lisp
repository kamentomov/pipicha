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

;;;; http-entry-points.lisp

(defpackage http-entry-points
  (:use 
   #:common-lisp
   #:by-http
   #:ip-interfaces
   #:pipicha)
  (:export))

(in-package :http-entry-points)

; start one server and many peers like this:
; (server-main '(anything 1234))
; (http-entry-points::peer-main '(anything "lo" "23413" "http://localhost:1234/chat-services"))

(defun ip-address-by-device (dev)
  (let ((res (find dev (get-ip-interfaces) :key #'ip-interface-name :test #'equal)))
    (if res (ip-interface-address res)
        (error (format nil "Network device `~a' is not associated with an address" dev)))))

;;; SERVER

(defun server-start (port ip)
  (instantiate-node
   (setf *world* (make-instance 'http))
   'server :port port :path "chat-services" :ip ip)
  (let ((quitcmd "bye"))
    (write-line
     (format nil "server-url: `~a'." (my-addr (get-node *world*))))
    (write-line
     (format nil "You can type `~a' to close (works if idle)." quitcmd))
    (loop for line = (read-line nil nil nil t)
       do (if (and (equal line quitcmd) (zerop (hash-table-count (pipicha::chat-rooms (get-node *world*)))))
              (progn (cleanup *world* (get-node *world*))
                     (return))))))

(defun show-usage-server (app)
  (format t "~a `network device name like eth0' `port to listen at' ~%" app))

(defun server-main (args)
  (if (= (length args) 3)
      (server-start
       (parse-integer (nth 2 args))
       (ip-address-by-device (nth 1 args)))
      (show-usage-server (nth 0 args))))

;;; PEER

(defun peer-start (port server ip)
  (chat-repl
   (instantiate-node 
    (setf *world* (make-instance 'http))
    'peer :port port :path "peer-chats" :ip ip)
   server))

(defun show-usage-peer (app)
  (format t "~a `network device name like eth0' `port to listen at' `server-url (check your server)'~%" app))

(defun peer-main (args)
  (if (= (length args) 4)
      (peer-start (parse-integer (nth 2 args))
                  (nth 3 args)
                  (ip-address-by-device (nth 1 args)))
      (show-usage-peer (nth 0 args))))
