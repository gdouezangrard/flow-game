(defun row-id (map)
  "Returns the id of the first map cell"
  (nth 0 (caar map)))

(defun row-color (map)
  "Returns the color of the first map cell"
  (nth 1 (caar map)))

(defun field-color (map)
  "Returns the color of the a map cell"
  (nth 1 (car map)))

(defun row-primary (map)
  "Returns the primary state of the first map cell"
  (nth 3 (caar map)))

(defun position-to-id (position size)
  "Converts position (x,y) to unique a id"
  (+ (car position) (* (cadr size) (cadr position))))

(defun id-to-position (id size)
  "Converts a cell id to a position (x,y)"
  (cons (mod id (cadr size)) (cons (floor (/ id (car size))) nil)))

(defun color-to-position (map c)
  "Returns the first position with color c"
  (cond ((null map) nil)
        ((equal (row-color map) c) (row-id map))
        (t (color-to-position (cdr map) c))))

(defun position-to-color (map p)
  "Returns the color of position p"
  (cond ((null map) nil)
        ((equal (row-id map) p) (row-color map))
        (t (position-to-color (cdr map) p))))

(defun is-primary (map c)
  "Returns T if c is primary, nil else"
  (cond ((null map) nil)
        ((equal (row-id map) c) (row-primary map))
        (t (is-primary (cdr map) c))))

(defun color-to-prime-position (map c)
  "Returns the first prime position with color c"
  (cond ((null map) nil)
        ((and (equal (row-color map) c) (row-primary map)) (row-id map))
        (t (color-to-prime-position (cdr map) c))))

(defun color-list (map)
  "Returns the color list of a grid"
  (cdr (sort (remove-duplicates (mapcar #'field-color map)) #'<)))

(defun is-in (l i)
  "Returns true if i is in l"
  (cond ((null l) nil)
        ((equal (car l) i) t)
        (t (is-in (cdr l) i))))

(defun initially-colored (map &optional (acc nil))
  "Returns the primary positions"
  (cond ((null map) acc)
        ((not (zerop (row-color map))) (initially-colored (cdr map) (append acc (list (row-id map)))))
        (t (initially-colored (cdr map) acc))))

(defun list-without-nth (l n)
  "Returns the list l without its nth element"
  (append (subseq l 0 (1- n)) (nthcdr n l)))

(defun have-same-color (l p1 p2)
  "Returns a boolean corresponding to whether p1 and p2 have the same color or not"
  (if (equal (position-to-color l p1) (position-to-color l p2)) t nil))

(defun is-valid-position (position size)
  "Check if position is valid (belongs to the map)"
  (and (>= (car position) 0)
       (>= (cadr position) 0)
       (< (car position) (car size))
       (< (cadr position) (cadr size))))

(defun position-iterate (p i j)
  "Intermediate fonction to iterate from a position"
  (cons (+ (car p) i) (cons (+ (cadr p) j) nil)))

; (defun position-neighboors (p size)
;   "Returns the list of valid p's neightboors (as in valid in the grid)"
;   (mapcar (lambda (e) (if (is-valid-position (position-iterate p (car e) (cadr e)) size)
;                           (position-to-id (position-iterate p (car e) (cadr e)) size)
;                           (* (car size) (cadr size)))) '((0 -1) (1 0) (0 1) (-1 0))))