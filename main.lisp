(ql:quickload "usocket")
(ql:quickload "cl-ppcre")

(defun render-template (file)
  (let ((f (open file :if-does-not-exist nil)))
    (when f
      (let ((f-str (format nil "~a~%" (read-line f))))
	(close f)
	f-str))))

(defvar *app* (make-instance 'app))
(add-route *app* "/" 'root-view)

(defun root-view (request)
  (render-template "/base.html"))

(defparameter *http-200-ok* "HTTP/1.1 200 OK")
(defparameter *http-400-bad-request* "HTTP/1.1 400 Bad Request")
(defparameter *http-404-not-found* "HTTP/1.1 404 Not Found")

(defconstant localhost "127.0.0.1")

(defgeneric add-route(obj rule view)
  (:documentation "creates rule for view"))
(defgeneric get-view(obj rule)
  (:documentation "returns associated procedure with rule"))
(defgeneric parse-request (obj http-str)
  (:documentation "parses http and returns a request obj"))
(defgeneric response-string (obj)
  (:documentation "returns string based on http response obj"))
(defgeneric exec-view (app request)
  (:documentation "executes view and returns reponse obj"))

;;TODO a bunch of shit

(defclass app ()
  ((views
    :initarg :views
    :initform nil
    )))

(defmethod add-route((app-obj app) rule view)
  (acons 'rule view (slot-value app-obj 'views)))

(defmethod get-view((app-obj app) rule)
  (assoc 'rule (slot-value app-obj 'views)))

(defmethod exec-view ((app-obj app) (request-obj request))
  "executes view procedure and passes 
   request-obj as the parameter"

  (let ((response-obj (make-instance 'response))
	(rule (slot-value request-obj 'file)))

    (if rule ;;if rule exists
	;;execute view and bind
	;;its return value to view-ret
      (let ((view-ret (funcall (get-view app-obj rule) request-obj)))
	;;if view-ret is a response obj, return it
	(cond ((equal (type-of view-ret) 'response) view-ret)
	      ((equal (type-of view-ret)   'string)
	        (setf (slot-value response-obj 'file) view-ret)
	         response-obj)
	      (t (error "Respose is all wack"))))
     ;;else
     (progn 
       (setf (slot-value response-obj 'message) *http-404-not-found*)
       response-obj))))

(defclass request ()
  ((method
    :initarg :method
    :initform nil)
   (file
    :initarg :file
    :initform nil)
   (host
    :initarg :host
    :initform nil)
   (content-type
    :initarg :content-type
    :initform nil)
   (content
    :initarg :content
    :initform nil)))

(defmethod parse-request((request-obj request) request)
  (format t "~a~%" request)

  (let ((request-list (cl-ppcre:split "\\s" request)))

    (setf (slot-value request-obj 'method) (first request-list))
    (setf (slot-value request-obj 'file) (second request-list))

    (when (> (list-length request-list) 3) nil)))
      
(defclass response ()
  ((message
    :initarg :message
    :initform nil)
   (content
    :initarg :content
    :initform nil)))

(defmethod response-string ((response-obj response))
  (format nil "~a~%Content-Type: text/html~%~%~a~%"
	  (slot-value response-obj 'message)
	  (slot-value response-obj 'content)))

(defun run-server (app ipaddr portno)
  (usocket:with-socket-listener (socket ipaddr portno)
    (usocket:wait-for-input socket)
	(let* ((connection (usocket:socket-accept socket))
	       (request (read-line (usocket:socket-stream connection)))
	       (request-obj (make-instance 'request)))
	 (format t "It works")
	 (parse-request request-obj request)
	  (let ((out (response-string (exec-view app request-obj))))
	   (server-output connection out)))))


;; outputs into the server
(defun server-output (connection message)
     (format (usocket:socket-stream connection) "~a~%" message)
     (force-output (usocket:socket-stream connection)))
