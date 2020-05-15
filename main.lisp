(ql:quickload "usocket")
(ql:quickload :cl-interpol)

(defmacro views-route(route view &optional (views-var '*views*))
  `(acons ',route ,view ,views-var))

(defvar localhost "127.0.0.1")
(defvar *http-response* "HTTP/1.1 200 OK~%Content-Type: text/html~%~%Hello world!~%")

(defun split (string char)
  "Returns a list of substrings of string
  divided by ONE space each.
  Note: Two consecutive spaces will be seen as
  if there were an empty string between them."
  (loop for i = 0 then (1+ j)
	as j = (position char string :start i)
	collect (subseq string i j)
	while j)) 

;;returns function according to the http request method
(defun response-get (request)
(defmacro ret-method-fun (method)
  `(intern (format nil "~a-response" ,method)))
(defun parse-request (request)
  (split request "\n")))
(defun handle-request (request)
  (format t "~a~%" request)
  (let* ((request-list (parse-request request))
	 (method (car request-list))
	 (reponse-fun (ret-method-fun method)))
    (response-fun request-list))

(defun create-server (ipaddr portno)
  (usocket:with-socket-listener (socket ipaddr portno)
  (usocket:wait-for-input socket)
  (usocket:with-connected-socket (connection (usocket:socket-accept socket))
    (unwind-protect 
	 (progn
	    ;;(server-output connection "Where are here")
	    (let ((request (read-line (usocket:socket-stream connection))))
		(format t "~a~%" (parse-request request))))
		  
		;;(response (handle-request request)))
	   ;; (server-output connection response)))
      (progn
	(format t "Closing sockets~%")
	(usocket:socket-close connection)
	(usocket:socket-close socket)(return))))))

;; outputs into the server
(defun server-output (connection message)
     (format (usocket:socket-stream connection) message)
     (force-output (usocket:socket-stream connection)))
