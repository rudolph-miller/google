(defun matchText (txt)
	(concatenate 
		'string
		"<span class=\"matchText\">"
		txt
		"</span>"))

(defun noMatchText (txt)
	(concatenate 
		'string
		"<span class=\"noMatchText\">"
		txt
		"</span>"))

;;;map lst -> (cadr item) = t or nil
(defun match (lst)
	(mapcar
		#'(lambda (item)
				(if (cadr item)
					(matchText (car item))
					(noMatchText (car item))))
		lst))

(defun style ()
	(concatenate 'string 
							 "<style type=\"text/css\">"
							 ".matchText {background-color:#e6bbc4;}"
							 ".noMatchText {}"
							 "</style>"))

(defun result-html (lst)
	(let ((html-list (match lst)))
		(apply #'concatenate 'string 
					 (style)
					 (append (list "<span class=\"result\">") html-list (list "</span>")))))


