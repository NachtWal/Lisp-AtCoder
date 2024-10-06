(defun skipSpaces (x)
  (if (string= x "")
      (error "から文字"))
  (let ((y (char x 0)))
    (case y
      (#\Space (skipSpaces (subseq x 1)))
      (otherwise x))))
(defun readString (x)
  (labels ((readWord (word rest)
	     (if (string= rest "")
		 word
		 (if (string= (subseq rest 0 1) " ")
		      word
		      (readWord (if (string= word "")
				    (subseq rest 0 1)
				    (concatenate 'string word (subseq rest 0 1)))
				(subseq rest 1))))))
    (readWord "" x)))
(defun splitString (x)
  (labels ((splitLoop (stringlist rest)
	     (handler-case
		 (progn
		   (setq rest (skipSpaces rest))
		   (if (null stringlist)
		       (setq stringlist
			     (list (readString rest)))
		       (setq stringlist
			     (append stringlist (list (readString rest)))))
		   (splitLoop stringlist (subseq rest (length (car (last stringlist))))))
	       (error ()
		 stringlist))))
      (splitLoop nil x)))

(defun Welcome-to-Atcoder ()
  (let* ((a (parse-integer (read-line)))
	 (bc (read-line))
	 (s (read-line))
	 (bclist (splitString bc))
	 (bcInt (mapcar #'parse-integer bclist)))
    (princ (+ a (first bcInt) (second bcInt)))
    (princ " ")
    (princ s)(terpri)))
(Welcome-to-Atcoder)
