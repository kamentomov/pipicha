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

;;;; by-http.lisp

(defpackage by-http
  (:use 
   #:common-lisp
   #:pipicha
   #:hunchentoot
   #:kamo-lib)
  (:export
   #:http))

(in-package :by-http)
(declaim (optimize (debug 3)))
;; (setf *SHOW-LISP-ERRORS-P* t
;;       *CATCH-ERRORS-P* nil)
;; (setf drakma:*header-stream* *standard-output*)
(defclass http (env) ())

(defmethod my-network-address ((world http) (myguts netloc))
  (format nil "http://~a:~a/~a"
          (net-ip myguts)
          (net-port myguts) (net-path myguts)))

(defmethod parse-address ((world http) (address string))
  address)

(defun encode-param (param)
  (format nil "~s" (ms:marshal param)))

(defun dispatcher (procedure destination source &rest others)
;(dispatcher 'test "http://localhost:1234/chat-procedures" "http://dir.bg" "L2" 3 nil)
;(dispatcher 'server-join "http://localhost:1234/chat-services" "http://127.0.0.1/" "L2" 3 nil)
  (let ((params
         `(("proc" . ,(encode-param procedure)) ("source" . ,(encode-param source))
           ,@(mapcar #'(lambda (el)
                         (cons (format nil "~a" (gentemp)) (encode-param el)))
                     others))))
    (decode-web-response
     (multiple-value-bind (body code)
         (net.aserve.client::do-http-request destination :query params)
       ;(drakma:http-request destination :parameters params :preserve-uri t)
       (declare (ignore body))
       code))))

(let ((lst `((:response-ok . 200)
             (:response-forwarded . 303)
             (:response-created . 201)
             (:response-removed . 202))))
  (defun decode-web-response (val)
    (car
     (find val lst
           :key #'(lambda (val)
                    (cdr val)))))
  (defun encode-web-response (val)
    (cdr (assoc val lst))))
  
(defun decode-param (param)
  (cond
    ((equal param "") nil)
    (t (ms:unmarshal (read-from-string param)))))

(defun decode-params (assoc-lst &optional res)
  ;(decode-params  '(("c"."http://localhost:12345/chat-services") ("e"."f") ("g"."h") ("i"."NIL")))
  (if assoc-lst
      (let ((val (cdr (pop assoc-lst))))
        (decode-params assoc-lst (cons (decode-param val) res)))
      (reverse res)))

(let ((node)
      (acceptor)
      (ct (make-instance 'call-type)))
  (defmethod instantiate-node ((env http) type &key port path ip)
    (setf node (make-instance type :port port :path path)
          (net-ip node) (vector-to-string ip))
    (start (setf acceptor
                 (make-instance 'easy-acceptor
                                :access-log-destination  nil
                                :message-log-destination nil
                                :port port)))
    (publish-remote-handler path)
    node)
  (defun handle-remote-request ()
    (setf
     (return-code*)
     (encode-web-response 
      (let ((params (decode-params (get-parameters *request*))))
        (apply (symbol-function (find-symbol (symbol-name (first params)) :pipicha))
               *home* ct node (second params) (cddr params)))))
    "")
  (defmethod get-node ((world http) &optional address)
    (declare (ignore address))
    node)
  (defmethod cleanup ((world http) (node netloc))
    (stop acceptor)))

(defun publish-remote-handler (path)
  (setq *dispatch-table*
        (list (create-prefix-dispatcher
               (format nil "/~a" path)
               'handle-remote-request))))

;;;;;;;;;;;;;;;;;;
; REMOTE INTERFACE
;;;;;;;;;;;;;;;;;;

(defmacro prepare-dispatch (procedure no-of-params)
  (let ((params (loop for i to (1- no-of-params)
                   collect (gentemp))))
    `(defmethod ,procedure ((world http) (call-type call-type) dest myself ,@params)
       (dispatcher ',procedure dest ;call-type:fix-me
                   (my-addr (get-node *world*)) ,@params))))

(prepare-dispatch pipicha:server-join 2)
(prepare-dispatch pipicha:host-join 2)
(prepare-dispatch pipicha:welcome 3)
(prepare-dispatch pipicha:refused-name-unavailable 1)
(prepare-dispatch pipicha:peer-join 2)
(prepare-dispatch pipicha:update-host 2)
(prepare-dispatch pipicha:send-chat-message 1)
(prepare-dispatch pipicha:bye 1)



