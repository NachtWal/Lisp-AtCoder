(defvar a (read))
(defvar b (read))
(defvar c (read))
(defvar x (read))

(princ 
  (labels ((smallLoop (total i j k)
                       (if (<  c k)
                           total
                           (if (= x (+ (* 500 i) (* 100 j) (* 50 k)))
                               (smallLoop (+ total 1) i j (+ k 1))
                               (smallLoop total i j (+ k 1)))))
           (middleloop (total i j k)
                        (if (< b j)
                            total
                            (middleLoop (smallLoop total i j k) i (+ j 1) k)))
           (largeLoop (total i j k)
                       (if (< a i)
                            total
                            (largeLoop (middleLoop total i j k) (+ i 1) j k))))
    (largeLoop 0 0 0 0)))(terpri)