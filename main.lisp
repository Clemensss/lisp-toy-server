(ql:quickload "usocket")
(ql:quickload "cl-ppcre")

(defvar *http-response* "HTTP/1.1 200 OK~%Content-Type: text/html~%~%a~%")
(defconstant localhost "127.0.0.1")

(defgeneric add-route(obj rule view)
  (:documentation "creates rule for view"))
(defgeneric get-view(obj rule)
  (:documentation "returns associated procedure with rule"))

(defgeneric parse-request (obj request)
  (:documentation "parses request and returns a request obj"))

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


(defun exec-view (app-obj request-obj)
  "executes view procedure and passes 
   request-obj as the parameter"
  (let ((rule (slot-value request-obj 'file)))
    (funcall (get-view app-obj rule) request-obj)))

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

(defmethod parse-request ((request-obj request) request)
  (format t "~a~%" request)

  (let ((request-list (cl-ppcre:split "\\s" request)))

    (setf (slot-value request-obj 'method) (first request-list))
    (setf (slot-value request-obj 'file) (second request-list))

    (when (> (list-length request-list) 3) 0)))
      
(defclass response ()
  ((message
    :initarg :message
    :initform (error "No message specified"))
   (content
    :initarg :content
    :initform nil)))


(defun run-server (app ipaddr portno)
  (usocket:with-socket-listener (socket ipaddr portno)
    (usocket:wait-for-input socket)
	(let* ((connection (usocket:socket-accept socket))
	       (request (read-line (usocket:socket-stream connection)))
	       (request-obj (make-instance 'request)))
	(parse-request request-obj request)
	(exec-view app request-obj))))

;; outputs into the server
(defun server-output (connection message)
     (format (usocket:socket-stream connection) "~a~%" message)
     (force-output (usocket:socket-stream connection)))
