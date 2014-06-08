(load "vars")
(load "util")
(load "longest")

(defvar *dict* (make-hash-table :test #'equal))
(defvar *dict-pos* "utf-8/")

;;;destructive
;;;just count word
;; or you can set it just list and use find instead of gethash
(defun count-word (word hash)
  (declare (hash-table hash)
		   (optimize (speed 3) (safety 0) (debug 0) (space 0)))
	(setf (gethash word hash) (length word)))

(defun search-engine (q &key (livedoor nil) (yahoo nil))
	(let ((s (make-string-output-stream)))
	(sb-ext:run-program "/usr/bin/python" (cond
																					(yahoo  (list "test.py" q "yahoo"))
																					(livedoor (list "test.py" q "livedoor"))
																					(t (list "test.py" q)))
											:output s)
	(get-output-stream-string s)))

;;;pickup "..." form strings
(defun pick-double-quotation (str)
	(let ((result nil))
		(declare (list result))
		(loop
			for beg = (position #\" str)
			while beg
			for end = (position #\" str :start (1+ beg))
			while end
			do (setf result (cons (subseq str (1+ beg) end) result))
			do (setf str (subseq str (1+ end))))
		(nreverse result)))

(defun get-result (q &key (livedoor nil) (yahoo nil))
	(let* ((sea (search-engine q :livedoor livedoor :yahoo yahoo))
				 (dq (pick-double-quotation sea))
				 (lst (coerce sea 'list))
				 (result nil) (flag nil) (acc nil))
		(if (or (find "との一致はありません。" dq :test #'equal)
						(find "に一致するウェブページは見つかりませんでした。" dq :test #'equal))
			0
			(progn
				(loop for chr in lst
							while chr
							do (if (eql #\件 chr)
									 (progn (setq result acc)
													(setq flag nil)))
							do (if flag
									 (push chr acc))
							do (if (eql #\約 chr)
									 (setq flag 't acc nil)))
				(if (null result) (setq result (list #\0)))
				(read-from-string 
					(coerce (remove-not-number (nreverse result)) 'string))))))
;;;numbers -> 48-57
(defun remove-not-number (lst)
	(remove-if
		#'(lambda (chr)
				(not
					(and (<= 48 (char-code chr)) (<= (char-code chr) 57))))
		lst))

(defun wget (url)
	(let ((file-name (concatenate 'string (namestring (car (directory "./tmp"))) (gen-unique-id)))
				(result ""))
		(declare (string file-name result))
		(labels ((sub (cha)
									(with-open-file (f file-name :direction :input :external-format cha)
										(loop
											for line = (read-line f nil)
											while line
											do (setf result (concatenate 'string result line))))))
			(sb-ext:run-program "/sw/bin/wget" (list url "-O" file-name) :output nil)
			(handler-case
				(sub :utf-8)
				(sb-int:stream-decoding-error (c) 
																			(handler-case
																				(sub :cp932)
																				(sb-int:stream-decoding-error (c)
																																			(sub :euc-jp))))))
		result))

;;;generate unique id by universal time
(defun gen-unique-id ()
	(the string (write-to-string (the integer (get-universal-time)))))

(defun gen-word-list (str)
	(mapcar #'car 
					(longest-search str)))

(defun list-n-gram (n lst hash)
	(declare (fixnum n)
					 (list lst)
					 (hash-table hash)
					 (optimize (speed 3) (safety 0)))
	(loop
		for word = (slice-list n lst)
		while word
		do (setf lst (cdr lst))
		do (count-word word hash)
		while (>= (length lst) n))
	hash)

(defun slice-list (n lst)
	(declare (fixnum n)
					 (list lst)
					 (optimize (speed 3) (safety 0)))
	(labels ((sub (n lst acc)
								(if (<= n 0)
									(nreverse acc)
									(sub (1- n) (cdr lst) (cons (car lst) acc)))))
		(sub n lst nil)))


(defun list-n-to-m-gram (n m  lst &optional
													 (hash (make-hash-table :test #'equal)))
	(declare (fixnum n m)
					 (hash-table hash))
	(loop
		for i from n to m
		do (list-n-gram i lst hash))
	(the hash-table hash))

(defun analyze (s &key (url nil))
	(let ((str (if (null url)
							 s
							 (remove-not-jp (remove-comment (wget url))))))
		(print (length str))
		(list-n-to-m-gram 2 4
											(gen-word-list str))))

(defun read-file (file)
	(with-open-file (input file :direction :input)
		(let ((result nil))
			(loop
				for chr = (read-char input nil)
				while chr
				do (if (not (or
											(eql chr #\NewLine)
											(eql chr #\Space)
											(eql chr #\Tab)))
						 (push chr result)))
			(concatenate 'string (nreverse result)))))

(defun get-sentence (str)
	(split-in-words
		'(#\、 #\。 #\「 #\」 #\, #\. #\')
		str))

(defun split-in-words (key-list str)
	(declare (list key-list)
					 (string str)
					 (optimize (speed 3) (safety 0)))
	(let ((result nil))
		(declare (list result))
		(loop
			for pos = (let ((m (apply #'min (mapcar 
																				#'(lambda (key)
																						(let ((pos (position key str)))
																							(if pos
																								pos
																								most-positive-fixnum)))
																				key-list))))
									(if (not (eql m most-positive-fixnum))
										m nil))
			do (setq result (cons (subseq str 0 pos) result))
			while pos
			do (setq str (subseq str (1+ pos))))
		(nreverse result)))

(defun sentence-n-to-m-gram (n m str)
	(let ((lst (get-sentence str))
				(result (make-hash-table :test #'equal)))
		(loop
			for item in lst
			do (list-n-to-m-gram n m (gen-word-list item) result))
		result))

(defun print-hash (hash)
	(maphash #'(lambda (key val)
							 (format t "~a: ~a~%" key val))
					 hash))


(defun make-key-list (str)
	(declare (string str)
					 (optimize (speed 3) (safety 0) (debug 0)))
	(let ((lst (mapcar #'gen-word-list (get-sentence str))))
		(declare (list lst))
		(labels ((sub (lst1)
									(declare (list lst1))
									(let ((result nil)
												(flag nil)
												(acc nil))
										(declare (list result acc)
														 (boolean flag))
										(loop
											for item in lst1
											for len = (if (eql (length item) 1)
																	(let ((code (char-code (character item))))
																		(if (and (< 12353 code)(< code 12447))
																			;;;Hiragana 12353-12447
																			1 2)) 2)
											do (cond
													 ((and (eql len 1) (not flag))
														(setq flag t)
														(push item acc))
													 ((and (eql len 1) flag)
														(push item acc))
													 ((and (> len 1) (not flag))
														(push item acc))
													 ((and (> len 1) flag)
														(push (apply #'concatenate 'string (nreverse acc)) result)
														(setq flag nil)
														(setq acc (list item)))))
										(if (not (null result))
											(if (not (null acc))
												(nreverse (cons (apply #'concatenate 'string (nreverse acc)) result))
												(nreverse result))
											(apply #'concatenate 'string (nreverse acc))))))
			;(remove-if #'(lambda (item) (< (length item) 2))
								 (flatten (mapcar #'sub lst)))))

(defun flatten (lst)
	(declare (list lst)
					 (optimize (speed 3) (safety 0)))
	(labels ((iter (lst acc)
								 (if (null lst)
									 acc
									 (if (atom (car lst))
										 (iter (cdr lst) (cons (car lst) acc))
										 (iter (cdr lst) (append (iter (car lst) nil) acc))))))
		(nreverse (iter lst nil))))


(defun in-thread (str &key (url nil) (file nil) (yahoo nil) (livedoor nil))
	(let ((threads)
				(result (make-hash-table :test #'equal))
				(key-list 
					(make-key-list
						(cond
							(url (wget str))
							(file (read-file str))
							(t str)))))
		(maphash 
			#'(lambda (key val)
					(let ((str (apply #'concatenate 'string key)))
						(push 
							(sb-thread:make-thread 
								#'(lambda ()
										(setf
											(gethash key result)
											(list 
												(get-result str :yahoo yahoo :livedoor livedoor)
												(length str)))))
							threads)))
			(list-n-to-m-gram
				1 4
				key-list))
		(loop
			for thread in threads
			do (sb-thread:join-thread thread))
		(list result key-list)))

(defun print-result (hash-list)
	(let ((dict (make-hash-table :test #'equal))
				(hash (car hash-list))
				(key-list (cadr hash-list)))
		(maphash 
			#'(lambda (keys val)
					(mapc #'(lambda (key)
											(if (not (zerop (car val)))
											(let ((value (/ (car val) (cadr val)))
														(score (gethash key dict)))
												(if (null score)
													(setf (gethash key dict) value)
													(setf (gethash key dict) (min value score))))))
									keys))
			hash)
		(print-hash hash)
		(print
			(mapcar
			#'(lambda (key)
					(let ((score (gethash key dict)))
						(if (and (not (null score)) (>= (/ 3 2) score))
						(list key t)
						(list key nil))))
			key-list))))

