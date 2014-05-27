(load "read")
(load "map")

; (defun color-to-position (l c)
;   "returns the first position with color c"
;   (cond ((endp l) nil)
;         ((= (car (cdr (car (car l)))) c) (car (car (car l))))
;         (t (color-to-position (cdr l) c))))
; ;;(color-to-position g 1)

; (defun position-to-color (l p)
;   "returns the color of position p"
;   (cond ((endp l) nil)
;         ((= (car (car (car l))) p) (car (cdr (car (car l)))))
;         (t (position-to-color (cdr l) p))))
; ;;(position-to-color g 16)

; (defun color-list (l)
;   "returns the color list of a grid"
;   (cdr (sort (remove-duplicates (mapcar #'cadar l)) #'<)))
; ;;(color-list g)ye

(defun elmt-in-list (l i)
  "returns true if i is in l, i is an enteger"
  (cond ((endp l) nil)
        ((= (car l) i) t)
        (t (elmt-in-list (cdr l) i))))
;;(elmt-in-list `nil 15)

(defun prime-list (l)
  "returns the prime positions: positions initially colored"
  (cond ((endp l) nil)
        ((not (= 0 (car (cdr (car (car l)))))) (append (list (car (car (car l)))) (prime-list (cdr l))))
        (t (prime-list (cdr l)))))
;;(prime-list g)

; (defun list-without-nth (list n)
;   (append (subseq list 0 (1- n)) (nthcdr n list)))
; ;;(list-without-nth `(1 2 4 5 6) 2)

(defun same-color (l p1 p2)
  "decides wether p1 and p2 have the same color or not"
  (if (equal (position-to-color l p1) (position-to-color l p2)) t nil))
;;(same-color g 12 5)

(defun grid-size (l)
  "returns the gris size"
  (labels ((grid-size-bis (l)
                          (if (endp l)
                              0
                              (+ 1 (grid-size-bis (cdr l))))))
          (isqrt (grid-size-bis l))))
;;(grid-size g)

(defun  position-neighboors (l c)
  "returns the neighboors of c"
  (let* ((size (grid-size l))
         (limit (* size size)))
    (cond ((= (mod c size) 0) 
           (cond ((= c 0) (list limit 1 size limit))
                 ((= c (- (* size size) size)) (list (- c size) (1+ c) limit limit))
                 (t (list (- c size) (1+ c) (+ c size) limit))))
          ((= (mod c size) (1- size))
           (cond ((= c (1- size)) (list limit limit (+ c size) (1- c)))
                 ((= c (1- (* size size))) (list (- c size) limit limit (1- c)))
                 (t (list (- c size) limit (+ c size) (1- c)))))
          ((and (> c 0) (< c (1- size)))
           (list limit (1+ c) (+ c size) (1- c)))
          ((and (> c (- (* size size) size)) (< c (1- (* size size))))
           (list (- c size) (1+ c) limit (1- c)))
          (t (list (- c size) (1+ c) (+ c size) (1- c))))))
;;(position-neighboors g 1)

;;format of visited
;;s = ((1 (2 5)) (2 (3 6))) 

(defun position-visited-neighboors (c s)
  "returns the visited neighboors of a position"
  (cond ((endp s) nil)
        ((= c (car (car s))) (car (cdr (car s))))
        (t (position-visited-neighboors c (cdr s)))))
;;(position-vited-neighboors 5 `((1 (2 5)) (2 (1)) (5 (3 6))))

(defun position-is-visited (c s)
  "decides if a position c belongs to visited list s"
  (cond ((endp s) nil)
        ((= (car (car s)) c) t)
        (t (position-is-visited c (cdr s)))))
;;(position-is-visited 3 `((1 (2 5)) (2 (1)) (5 (3 6))))

(defun choose-direction (l s)
  "returns the next direction given a list of visited s and a grid l"
  (if (or (<= (length s) 1) (elmt-in-list (prime-list l) (car (car (last s))))) 0
      (let* ((end (car (car (last s))))
             (before-end (car (car (last (butlast s)))))
             (difference (- end before-end))
             (size (grid-size l)))
        (cond ((= difference (* -1 size)) 0)
              ((= difference 1) 1)
              ((= difference size) 2)
              ((= difference -1) 3)))))
;;(choose-direction g `((1 (4))))

(defun position-is-obstacle (l c s i)
  "decides if the position c is an obstacle, s is the list of visited positions"
  (let ((prime (prime-list l))
        (size (grid-size l)))
    (if (or (>= c (* size size)) (and (elmt-in-list prime c) (not (= (position-to-color l c) i))) (position-is-visited c s)) t nil)))
;;(position-is-obstacle g 8 `((10 (9)) (9 ())) 1)

(defun any-obstacle (l c s i)
  "decides if the next potential position is an obstacle"
  (let* ((neighboors (position-neighboors l c))
         (direction (choose-direction l s))
         (potential-next (nth direction neighboors))
         (visited (position-visited-neighboors c s)))
    (if (or (position-is-obstacle l potential-next s i) (elmt-in-list visited potential-next))
        t
        nil)))
;;(any-obstacle g 9 `((10 (9)) (9 ())) 1)

(defun any-unmarked-path (l c s i)
  "looks for an unmarked path"
  (let* ((neighboors (position-neighboors l c))
         (direction (choose-direction l s))
         (potential-paths (list-without-nth neighboors (1+ direction)))
         (size (grid-size l))
         (visited (position-visited-neighboors c s)))
    (labels ((choose-unmarked-path (k cp pp v vi)
                                   "k: the grid, cp: current position, pp: list of potential paths, v: visited positions list"
                                   (cond ((endp pp) (* size size))
                                         ((and (not (position-is-obstacle k (car pp) v i)) (not (elmt-in-list vi (car pp)))) (car pp))
                                         (t (choose-unmarked-path k cp (cdr pp) v vi)))))
            (choose-unmarked-path l c potential-paths s visited))))
;;(any-unmarked-path g 9 `((10 (9)) (9 ())) 1)

(defun move-forward (l c s)
  "moves forward"
  (let* ((neighboors (position-neighboors l c))
         (direction (choose-direction l s))
         (last-position (car (last s)))
         (rest-of-list (butlast s))
         (next-position (nth direction neighboors)))
    (append (append rest-of-list (list (append (list (car last-position)) (list (append (car (cdr last-position)) (list next-position)))))) (list (list next-position ())))))  
;;(move-forward g 2 `((1 (4 5)) (2 (6))))

(defun turn-to-unmarked-path (l c s i)
  "turns to unmarked path"
  (let ((size (grid-size l))
        (unmarked-path (any-unmarked-path l c s i))
        (last-position (car (last s)))
        (rest-of-list (butlast s)))
    (if (< unmarked-path (* size size))
        (append (append rest-of-list (list (append (list (car last-position)) (list (append (car (cdr last-position)) (list unmarked-path)))))) (list (list unmarked-path ()))))))
;;(turn-to-unmarked-path g 2 `((1 (4 5)) (2 ())))

(defun turn-back (s)
  "turns back"
  (butlast s))

(defun color-found (l i s)
  ""
  (if (and (= (position-to-color l (car (car (last s)))) i) (not (= (color-to-position l i) (car (car (last s)))))) t nil))
;;(color-found g 1 `((8 ())))

; (defun id-to-position (id size)
;   "Converts a cell id to a position (x,y)"
;   (cons (mod id (cadr size)) (cons (floor (/ id (car size))) nil)))

(defun solve (l)
  "solves a grid l"
  (let* ((colors (color-list l))
         (start-position (color-to-position l (nth 0 colors)))
         (size (grid-size l))
         (prime (prime-list l)))
    (labels ((solve-aux (grid start visited color all-colors all-primes)
                        ""
                        (if (color-found grid color visited);;si je trouve une couleur 
                            (if (= color (car (last all-colors))) visited;; si c'est la dernière couleur, c'est terminé !
                                (let ((new-visited-4 (append visited (list (list (color-to-position grid (1+ color)) ())))));; sinon je passe à une nouvelle couleur 
                                  (solve-aux grid (color-to-position grid (1+ color)) new-visited-4 (1+ color) all-colors all-primes))) 
                            (if (any-obstacle grid (car (car (last visited))) visited color);;si la couleur n'est pas trouvé je cherche un obstacle
                                (if (< (any-unmarked-path grid (car (car (last visited))) visited color) (* size size));; si obstacle, chercher une chemin non marqué
                                    (let ((new-visited-1 (turn-to-unmarked-path grid (car (car (last visited))) visited color)))
                                      (solve-aux grid start new-visited-1 color all-colors all-primes))
                                    (if (elmt-in-list all-primes (car (car (last visited))))
                                        (let ((new-visited-5 (turn-back (turn-back visited))))
                                          (solve-aux grid (color-to-position grid (1- color)) new-visited-5 (1- color) all-colors all-primes))
                                        (let ((new-visited-2 (turn-back visited)))
                                          (solve-aux grid start new-visited-2 color all-colors all-primes))))
                                (let ((new-visited-3 (move-forward grid (car (car (last visited))) visited)))
                                  (solve-aux grid start new-visited-3 color all-colors all-primes))))))
            (solve-aux l start-position (list (list start-position ())) (nth 0 colors) colors prime))))

(defun final-solution (read)
  "final solution"
  (let* ((l (make-grid (cdr read)))
         (size (car read))
         (s (solve l)))
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