(ql:quickload "usocket")

(defparameter localhost "127.0.0.1")

(defun split (string char)
  "Returns a list of substrings of string
  divided by ONE space each.
  Note: Two consecutive spaces will be seen as
  if there were an empty string between them."
  (loop for i = 0 then (1+ j)
	as j = (position char string :start i)
	collect (subseq string i j)
	while j)) 

(defun handle-request (request)
  (format t "~a~%" request)
  (let ((request-list (split request #\Space)))
    (if(equal(car request-list) "GET")
       "OK GET METHOD\n"
     "WHAT\n")))

(defun create-server (ipaddr portno)
  (usocket:with-socket-listener (socket ipaddr portno)
  (usocket:wait-for-input socket)
  (usocket:with-connected-socket (connection (usocket:socket-accept socket))
    (server-output connection "Where are here")
    (let* ((request (read-line (usocket:socket-stream connection)))
	   (response (handle-request request)))
      (server-output connection response)))))

;; outputs into the server
(defun server-output (connection message)
     (format (usocket:socket-stream connection) message)
     (force-output (usocket:socket-stream connection)))
