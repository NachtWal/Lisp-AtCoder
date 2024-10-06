#|先頭文字から空白があれば取り除く処理。
  例："  AB" -> "AB", "AB" -> "AB"
  最終文字まで到達したらerrorを投げる。|#
(defun skipSpaces (x)
  (if (string= x "")
      (error "から文字"))
  (let ((y (char x 0)))
    (case y
      (#\Space (skipSpaces (subseq x 1)))
      (otherwise x))))
#|次の空白文字までの文字列を読む。
  例："AB CD" -> "AB", " AB CD" -> ""
  |#
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
#|空白区切りで文字列をリストにする。
  例："AB CD" -> '("AB" "CD"), "AB   CD" -> '("AB", "CD")
  |#
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

(let* ((input (splitString (read-line)))
       (ab (mapcar #'parse-integer input)))
  (if (evenp (reduce #'* ab))
      (write-line "Even")
      (write-line "Odd")))
