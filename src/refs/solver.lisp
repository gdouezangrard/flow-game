#!/usr/local/bin/clisp

(load "map")
(defparameter *size* (car (read-map "maps/map.txt")))
(defparameter *map* (make-grid (cdr (read-map "maps/map.txt"))))
(defun position-visited-neighboors (c s)
  "Returns the visited neighboors of a position"
  (cond ((null s) nil)
        ((equal c (caar s)) (cadar s))
        (t (position-visited-neighboors c (cdr s)))))

(defun is-visited-position (c s)
  "Decides if a position c belongs to the visited list s"
  (cond ((null s) nil)
        ((equal (caar s) c) t)
        (t (is-visited-position c (cdr s)))))

(defun len (size)
  (* (car size) (cadr size)))

;;;;;;;;;;; POTENTIAL ERROR HERE
(defun choose-direction (l s size)
  "Returns the next direction given a list of visited s and a grid l"
  (if (or (<= (length s) 1)
          (is-in (initially-colored l) (caar (last s))))
      0
      (let* ((len (len size))
             (end (caar (last s)))
             (before-end (caar (last (butlast s))))
             (difference (- end before-end)))
            (cond ((equal difference (* -1 len)) 0)
                  ((equal difference 1) 1)
                  ((equal difference len) 2)
                  ((equal difference -1) 3)
                  (t 0)))))

(defun position-is-obstacle (l c s i size)
  "Decides if the position c is an obstacle, s is the list of visited positions"
  (let ((prime (initially-colored l))
        (len (len size)))
    (or (>= c len)
        (and (is-in prime c)
             (not (equal (position-to-color l c) i)))
        (is-visited-position c s))))

;;;;;;;;;; POTENTIAL ERROR HERE
(defun any-obstacle (l c s i size)
  "Decides if the next potential position is an obstacle"
  (let* ((prime (initially-colored l))
         (neighboors (position-neighboors (id-to-position c size) size))
         (direction (choose-direction l s size))
         ; (y (print direction))
         (potential-next (nth direction neighboors))
         (visited (position-visited-neighboors c s)))
    (if (or (position-is-obstacle l potential-next s i size)
            (is-in visited potential-next))
        t
        nil)))

(defun any-unmarked-path (l c s i size)
  "Looks for an unmarked path"
  (let* ((neighboors (position-neighboors (id-to-position c size) size))
         (direction (choose-direction l s size))
         (potential-paths (list-without-nth neighboors (1+ direction)))
         (visited (position-visited-neighboors c s)))
    (labels ((choose-unmarked-path (k cp pp v vi) ; "k: the grid, cp: current position, pp: list of potential paths, v: visited positions list"
                                   (cond ((endp pp) (len size))
                                         ((and (not (position-is-obstacle k (car pp) v i size)) (not (is-in vi (car pp)))) (car pp))
                                         (t (choose-unmarked-path k cp (cdr pp) v vi)))))
            (choose-unmarked-path l c potential-paths s visited))))

(defun move-forward (l c s size)
  "Moves forward"
  (let* ((neighboors (position-neighboors (id-to-position c size) size))
         (direction (choose-direction l s size))
         (last-position (car (last s)))
         (rest-of-list (butlast s))
         (next-position (nth direction neighboors)))
    (append (append rest-of-list
                    (list (append (list (car last-position))
                                  (list (append (car (cdr last-position))
                                                (list next-position))))))
            (list (list next-position ())))))

(defun turn-to-unmarked-path (l c s i size)
  "Turns to unmarked path"
  (let ((len (len size))
        (unmarked-path (any-unmarked-path l c s i size))
        (last-position (car (last s)))
        (rest-of-list (butlast s)))
    (if (< unmarked-path len)
        (append (append rest-of-list
                        (list (append (list (car last-position))
                                      (list (append (car (cdr last-position))
                                                    (list unmarked-path))))))
                (list (list unmarked-path ()))))))

(defun turn-back (s)
  "Turns one cell back"
  (butlast s))

(defun color-found (l i s)
  "Detect if a color is found"
  (if (and (= (position-to-color l (car (car (last s))))
              i)
           (not (= (color-to-position l i)
                   (car (car (last s))))))
      t
      nil))

(defun solve (l size)
  "solves a grid l"
  (let* ((colors (color-list l))
         (start-position (color-to-position l (nth 0 colors)))
         (len (len size))
         (prime (initially-colored l)))
    (labels ((solve-aux (grid start visited color all-colors all-primes)
                        (if (color-found grid color visited);;si je trouve une couleur
                            (if (= color (car (last all-colors)))
                                visited;; si c'est la dernière couleur, c'est terminé !
                                (let ((new-visited-4 (append visited
                                                             (list (list (color-to-position grid (1+ color)) ())))));; sinon je passe à une nouvelle couleur
                                  (solve-aux grid (color-to-position grid (1+ color)) new-visited-4 (1+ color) all-colors all-primes)))
                            (if (any-obstacle grid (car (car (last visited))) visited color size);;si la couleur n'est pas trouvé je cherche un obstacle
                                (if (< (any-unmarked-path grid (car (car (last visited))) visited color size) len);; si obstacle, chercher une chemin non marqué
                                    (let ((new-visited-1 (turn-to-unmarked-path grid (car (car (last visited))) visited color size)))
                                      (solve-aux grid start new-visited-1 color all-colors all-primes))
                                    (if (is-in all-primes (car (car (last visited))))
                                        (let ((new-visited-5 (turn-back (turn-back visited))))
                                          (solve-aux grid (color-to-position grid (1- color)) new-visited-5 (1- color) all-colors all-primes))
                                        (let ((new-visited-2 (turn-back visited)))
                                          (solve-aux grid start new-visited-2 color all-colors all-primes))))
                                (let ((new-visited-3 (move-forward grid (car (car (last visited))) visited size)))
                                  (solve-aux grid start new-visited-3 color all-colors all-primes))))))
            (solve-aux l start-position (list (list start-position ())) (nth 0 colors) colors prime))))
(defun final-solution (l size)
  "final solution"
  (let* ((s (solve l size)))
    (labels ((adjust-solution (sl i res)
	     ""
	     (cond ((endp sl) res)
		   ((equal nil (car (cdr (car sl))))
		    (let* ((bl (butlast res))
			   (q (last res))
			   (color (car (car q)))
			   (path (car (cdr (car q))))
			   (new-pos (car (car sl)))
			   (new-path (append path (list (id-to-position new-pos size))))
			   (new-res (append bl (list (append (list color) (list new-path)))))
			   (final-res (append new-res (list (append (list (1+ i) ()))))))
		      (adjust-solution (cdr sl) (1+ i) final-res)))
		   (t (let* ((bl (butlast res))
			     (q (last res))
			     (color (car (car q)))
			     (path (car (cdr (car q))))
			     (new-pos (car (car sl)))
			     (new-path (list (append path (list (id-to-position new-pos size)))))
			     (new-res (append bl (list (append (list color) new-path)))))
			     
			(adjust-solution (cdr sl) i new-res))))))
      (butlast (adjust-solution s 1 '((1 ())))))))
(print (final-solution *map* *size*))
;;(print (solve *map* *size*))
;;(print (car (cdr (car '((1 (3 4)))))))
;; (defparameter sl (solve *map* *size*))
;; (defparameter res '((1 ()))) 
;; (print (append (butlast res) (list (append (list (car (car (last res)))) (list (append (car (cdr (car (last res)))) (list (car (car sl)))))))))