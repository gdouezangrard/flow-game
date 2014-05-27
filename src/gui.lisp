(load "ltk/ltk")
(load "solver_3")

(defun path-row-color (path)
  "Get color from path"
  (caar path))

(defun path-row-cells (path)
  "Get cells from path"
  (cadar path))

(defun path-row-cells-from-color (path color)
  "Get cell from path and color"
  (cond ((null path) nil)
        ((equal (path-row-color path) color) (path-row-cells path))
        (t (path-row-cells-from-color (cdr path) color))))

(defun is-in-path (path c)
  "Check if cell is in path"
  (cond ((null path) nil)
        ((is-in (path-row-cells path) c) (path-row-color path))
        (t (is-in-path (cdr path) c))))

(defun downgrade (row cell &optional (acc nil))
  "Remove all cells in the path after 'cell'"
  (cond ((null row) (append acc (list cell)))
        ((equal (car row) cell) (append acc (list cell)))
        (t (downgrade (cdr row) cell (append acc (list (car row)))))))

(defun downgrade-minus-1 (row cell &optional (acc nil))
  "Remove all cells in the path after and including 'cell'"
  (cond ((null row) acc)
        ((equal (car row) cell) acc)
        (t (downgrade-minus-1 (cdr row) cell (append acc (list (car row)))))))

(defun add-to-path (path color c &optional (acc nil))
  "Add cell c to path by its color"
  (cond ((null path) (append acc (list (list color (list c)))))
        ((equal (path-row-color path) color) (append acc (list (list color (downgrade (path-row-cells path) c))) (cdr path)))
        (t (add-to-path (cdr path) color c (append acc (list (car path)))))))

(defun add-to-path-partial-erase (path color c &optional (acc nil))
  "Add cell c to path with downgrade"
  (cond ((null path) (append acc (list (list color (list c)))))
        ((equal (path-row-color path) color) (append acc (list (list color (downgrade-minus-1 (path-row-cells path) c))) (cdr path)))
        (t (add-to-path-partial-erase (cdr path) color c (append acc (list (car path)))))))

(defun add-to-path-erase (path color c &optional (acc nil))
  "Add cell c to path with override"
  (cond ((null path) (append acc (list (list color (list c)))))
        ((equal (path-row-color path) color) (append acc (list (list color (list c))) (cdr path)))
        (t (add-to-path-erase (cdr path) color c (append acc (list (car path)))))))

(defun add-to-ltkpath (ltkpath draw color &optional (acc nil))
  "Add to ltkpath"
  (cond ((null ltkpath) (append acc (list (list color (list draw)))))
        ((equal (path-row-color ltkpath) color) (append acc (list (list color (list draw))) (cdr ltkpath)))
        (t (add-to-ltkpath (cdr ltkpath) draw color (append acc (list (car ltkpath)))))))

(defun get-cell (evt factor)
  "Get cell clicked"
  (list (floor (ltk::event-x evt) factor) (floor (ltk::event-y evt) factor)))

(defun move-is-valid (path cell-color cell size)
  "Check if move is valid"
  (let ((pcell (car (last (path-row-cells-from-color path cell-color)))))
    (cond ((equal cell pcell) nil)
          ((or (>= (car cell) (car size)) (< (car cell) 0) (< (cadr cell) 0) (>= (cadr cell) (cadr size))) nil)
          ((equal (car cell) (car pcell)) (cond ((or (equal (cadr cell) (1- (cadr pcell))) (equal (cadr cell) (1+ (cadr pcell)))) t)
                                                (t nil)))
          ((equal (cadr cell) (cadr pcell)) (cond ((or (equal (car cell) (1- (car pcell))) (equal (car cell) (1+ (car pcell)))) t)
                                                  (t nil)))
          (t nil))))

(defun draw-line (canvas color positions)
  "Draw a line interpolating points in 'positions'"
  (let ((line (ltk::create-line canvas positions)))
    (ltk::itemconfigure canvas line "fill" color)
    (ltk::itemconfigure canvas line "width" 20)
    line))

(defun linearize (l &optional (acc nil))
  "Linearize a list"
  (cond ((null l) acc)
        (t (linearize (cdr l) (append acc (list (caar l) (cadar l)))))))

(defun redraw-path (canvas ltkpath path color factor colors)
  "Redraw a path by its color"
  (let ((old (car (path-row-cells-from-color ltkpath color))))
    (ltk::itemconfigure canvas old "state" :hidden)
    (if (>= (length (path-row-cells-from-color path color)) 2)
        (draw-line canvas (nth color colors) (mapcar (lambda (e) (+ (* e factor) (/ factor 2))) (linearize (path-row-cells-from-color path color)))))))

(defun erase-paths (canvas ltkpath)
  "Erase all drawn paths"
  (cond ((null ltkpath) nil)
        (t (progn (ltk::itemconfigure canvas (caadar ltkpath) "state" :hidden)
                  (erase-paths canvas (cdr ltkpath))))))

(defun redraw-all (canvas path factor colors)
  "Redraw the entire grid"
  (cond ((null path) nil)
        (t (progn (draw-line canvas (nth (caar path) colors) (mapcar (lambda (e) (+ (* e factor) (/ factor 2))) (linearize (cadar path))))
                  (redraw-all canvas (cdr path) factor colors)))))

(defun validate (map color cells path size)
  "Validate a path"
  (cond ((null cells) nil)
        (t (and (not (equal (car cells) (car (last cells)))) (is-primary map (position-to-id (car cells) size)) (is-primary map (position-to-id (car (last cells)) size))))))

(defun check-valid (map path size)
  "Validate a grid"
  (cond ((null path) t)
        (t (if (validate map (caar path) (cadar path) path size)
               (check-valid map (cdr path) size)
               nil))))

(defun bindings (canvas map factor size colors bclear bcheck path)
  "Bindings for the GUI"
  (let* ((down nil)
         (last-cell nil)
         (ltkpath nil)
         (last-color nil)
         (last-was-primary nil))
    (ltk::bind bclear "<ButtonRelease-1>" (lambda (evt)
                                            (erase-paths canvas ltkpath)
                                            (setf ltkpath nil)
                                            (setf path nil)))
    (ltk::bind bcheck "<ButtonRelease-1>" (lambda (evt)
                                            (if (and (equal (length path) (length (color-list map))) (check-valid map path size) (not (null path)))
                                                (ltk::message-box "Solution valide !" "Correction" :ok :info)
                                                (ltk::message-box "Solution incorrecte !" "Correction" :ok :info))))
    (ltk::bind canvas "<ButtonPress-1>"
               (lambda (evt)
                 (setf down t)
                 (let ((cell (get-cell evt factor)))
                   (if (not (or (>= (car cell) (car size)) (< (car cell) 0) (< (cadr cell) 0) (>= (cadr cell) (cadr size))))
                   (cond ((is-primary map (position-to-id cell size))
                          (let ((color (position-to-color map (position-to-id cell size))))
                            (setf path (add-to-path-erase path color cell))
                            (setf last-cell cell)
                            (setf last-color color)
                            (setf ltkpath (add-to-ltkpath ltkpath (redraw-path canvas ltkpath path last-color factor colors) color))))
                         ((is-in-path path cell)
                          (let ((color (is-in-path path cell)))
                            (setf path (add-to-path path color cell))
                            (setf last-color color)
                            (setf last-cell cell)
                            (setf ltkpath (add-to-ltkpath ltkpath (redraw-path canvas ltkpath path last-color factor colors) last-color))))))
                 (print path))))
    (ltk::bind canvas "<ButtonRelease-1>" (lambda (evt)
                                            (declare (ignore evt))
                                            (setf down nil)
                                            (setf last-was-primary nil)
                                            (setf last-cell nil)
                                            (setf last-color nil)))
    (ltk::bind canvas "<Motion>"
               (lambda (evt)
                 (when down
                   (let ((cell (get-cell evt factor)))
                     (if (and (move-is-valid path last-color cell size) (not last-was-primary))
                         (if (is-in-path path cell)
                             (let ((color (is-in-path path cell)))
                               (if (equal color last-color)
                                   (progn (setf path (add-to-path path color cell))
                                          (setf last-cell cell)
                                          (setf ltkpath (add-to-ltkpath ltkpath (redraw-path canvas ltkpath path last-color factor colors) last-color)))
                                   (if (not (is-primary map (position-to-id cell size)))
                                   (progn (setf path (add-to-path-partial-erase path color cell))
                                          (setf path (add-to-path path last-color cell))
                                          (setf ltkpath (add-to-ltkpath ltkpath (redraw-path canvas ltkpath path color factor colors) color))
                                          (setf ltkpath (add-to-ltkpath ltkpath (redraw-path canvas ltkpath path last-color factor colors) last-color))
                                          (setf last-cell cell)))))
                             (if (is-primary map (position-to-id cell size))
                                 (let ((color (position-to-color map (position-to-id cell size))))
                                   (if (equal color last-color)
                                       (progn (setf path (add-to-path path color cell))
                                              (setf last-cell nil)
                                              (setf last-was-primary t)
                                              (setf ltkpath (add-to-ltkpath ltkpath (redraw-path canvas ltkpath path last-color factor colors) last-color)))))
                                 (progn (setf path (add-to-path path last-color cell))
                                        (setf last-cell cell)
                                        (setf ltkpath (add-to-ltkpath ltkpath (redraw-path canvas ltkpath path last-color factor colors) last-color))))))))))))

(defun draw-rectangle (canvas x y factor)
  "Draw initial grid rectangles"
  (let* ((rectangle (ltk::create-rectangle canvas (* x factor) (* y factor) (* (1+ x) factor) (* (1+ y) factor))))
    (ltk::itemconfigure canvas rectangle "outline" :white)
    (ltk::itemconfigure canvas rectangle "fill" "")))

(defun draw-grid (canvas size factor)
  "Draw initial grid"
  (loop for i from 0 to (1- (car size)) do
        (loop for j from 0 to (1- (cadr size)) do
              (draw-rectangle canvas i j factor))))

(defun draw-primary (canvas map size factor colors)
  "Draw primary cells"
  (cond ((null map) nil)
        ((row-primary map) (progn (draw-circle canvas (id-to-position (row-id map) size) 50 10 (nth (row-color map) colors) factor)
                                  (draw-primary canvas (cdr map) size factor colors)))
        (t (draw-primary canvas (cdr map) size factor colors))))

(defun draw-circle (canvas position radius padding color factor)
  "Intermediate circle drawing"
  (let* ((x (car position))
         (y (cadr position))
         (oval (ltk::create-oval canvas (+ (* x factor) (/ padding 2)) (+ (* y factor ) (/ padding 2)) (+ (* x factor) (- radius (/ padding 2))) (+ (* y factor) (- radius (/ padding 2))))))
    (ltk::itemconfigure canvas oval "fill" color)
    (ltk::itemconfigure canvas oval "outline" "")))

(defun gui (read &optional (path nil))
  "Main graphical user interface"
  (setf ltk::*wish-args* '("-name" "Flow Game"))
  (setf ltk:*debug-tk* nil)
  (ltk::with-ltk ()
                 (let* ((map (make-grid (cdr read)))
                        (size (car read))
                        (factor 50)
                        (width (- (* (car size) factor) 1))
                        (height (- (* (cadr size) factor) 1))
                        (colors '(nil :red :blue :yellow :green :orange :pink :pink :brown :purple :magenta :cyan :white))
                        (ic (make-instance 'ltk::canvas :width width :height height :background :grey10))
                        (canvas (ltk::canvas ic))
                        (bclear (make-instance 'ltk::button
                                               :master nil
                                               :text "Clear"))
                        (bcheck (make-instance 'ltk::button
                                               :master nil
                                               :text "Check")))
                   (draw-grid canvas size factor)
                   (draw-primary canvas map size factor colors)
                   (ltk::pack canvas :fill :both)
                   (if (not (null path))
                       (progn (redraw-all canvas path factor colors))
                       (progn (bindings canvas map factor size colors bclear bcheck path)
                              (ltk::pack bclear :side :left)
                              (ltk::pack bcheck :side :right))))))