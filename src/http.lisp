(in-package #:zledger)

(declaim #.*compile-declaration*)

(defvar *http-server* nil)

(defcomponent http (logger config)
  (:start
   (setf *http-server* (make-http-server))
   (log:info "HTTP server listening to ~a:~d"
             (hunchentoot:acceptor-address *http-server*)
             (hunchentoot:acceptor-port *http-server*))
   (hunchentoot:start *http-server*))
  (:stop
   (hunchentoot:stop *http-server* :soft t)))

(defhandler healthcheck (:method :get :path "/ping")
  (with-json-response (200)
    (st-json:jso "pong" t
                 "requests_headers"
                 (loop for (k . v) in (hunchentoot:headers-in*)
                       collect (st-json:jso (string k) (string v))))))

(defhandler user-by-id (:method :get :path "/users/:id/:version")
  (with-path-parameters (id version)
    (with-json-response (200)
      (st-json:jso "users" (st-json:as-json-bool nil) "id" id "version" version))))
