(load "gui")

(defun help-message ()
  "Display a general help message."
  (format t "--------------~%")
  (format t "Usage: ./flow.lisp [mode] map-file~%")
  (format t "modes:~%")
  (format t "  solve : solve the grid (can take a while)")
  (format t "  play : you can play on the grid"))

(defun rf-commandline (args)
  "Invoke actions corresponding to command line arguments."
  (cond ((null args) (help-message))
        ((equal (length args) 2) (cond ((equal (car args) "play") (gui (read-map (cadr args))))
                                       ((equal (car args) "solve") (gui (read-map (cadr args)) (final-solution (read-map (cadr args)))))))
        (t (help-message))))

(rf-commandline *args*)