(load "~/quicklisp/setup.lisp")
(require :clack)
(require :ningle)
(load "test")
(load "make-html")

(defvar *app* (make-instance 'ningle:<app>))


(defun home ()
	(format nil
					(concatenate 'string 
											 "<form action=\"/result\" method=\"POST\">"
											 "<textarea id=\"inputText\" name=\"q\" rows=\"10\" cols=\"30\" style=\"width:460px;height:300px\">"
											 "</textarea></br>"
											 "<input type=\"submit\" value=\"チェックする\" />"
											 "</form>")))



(setf (ningle:route *app* "/")
			(home))

(setf (ningle:route *app* "/result" :method :POST)
			#'(lambda (params)
					(let ((q (getf params :|q|)))
						(result-html
							(print-result
								(in-thread
									q
									:livedoor t))))))

(clack:clackup *app*)

