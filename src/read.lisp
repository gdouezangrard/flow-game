(defun read-map (map)
  "Given a valid map file, return a map."
  (with-open-file (stream map)
                  (labels ((read-file (&optional (l '()))
                                      (let ((c (read-char stream nil)))
                                        (cond ((null c) l)
                                              ((eq c #\-) (read-line stream nil) (read-file))
                                              (t (read-file (append l (list c)))))))
                           (get-size (l &optional (i '()) (j '()))
                                     (cond ((digit-char-p (car l)) (get-size (cdr l) (append i (list (car l))) j))
                                           ((eq (car l) #\Space) (get-size (cdr l) '() (append j (list (apply #'concatenate 'string (mapcar #'string i))))))
                                           ((eq (car l) #\Newline) (append (list (mapcar #'parse-integer (append j (list (apply #'concatenate 'string (mapcar #'string i)))))) l))
                                           (t (get-size (cdr l) i j))))
                           (get-map (l &optional (a '()))
                                    (cond ((null l) a)
                                          ((not (characterp (car l))) (get-map (cdr l) (cons (car l) a)))
                                          ((eq (car l) #\Newline) (get-map (cdr l) a))
                                          ((and (characterp (car l)) (digit-char-p (car l) 16)) (get-map (cdr l) (cons (digit-char-p (car l) 16) a)))
                                          (t (get-map (cdr l) (cons (car l) a))))))
                          (reverse (get-map (get-size (read-file)))))))

(defun make-grid (l &optional (acc '()) (i 0))
  "Build a grid from a file parsed list."
  (cond ((null l) acc)
        (t (make-grid (cdr l) (append acc (list (list `(,i ,(cond ((or (eq (car l) #\#) (eq (car l) #\+)) 0) (t (car l))) ,(cond ((eq (car l) #\+) 0) (t nil)) ,(not (or (eq (car l) #\#) (eq (car l) #\+)))) '()))) (1+ i)))))