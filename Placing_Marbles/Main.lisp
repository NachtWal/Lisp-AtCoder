(let ((s123 (read-line)))
  (print
    (reduce #'+ 
        (mapcar (lambda (x) (- (char-code x) (char-code #\0))) 
            (coerce s123 'list)))))