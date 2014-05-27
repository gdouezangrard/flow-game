#!/usr/local/bin/clisp

(load "gui")

(car (read-map "../maps/map.txt")) ; -> (4 4)
(make-grid (cdr (read-map "../maps/map.txt"))) ; -> Valid map

(defparameter *size* (car (read-map "../maps/map.txt")))
(defparameter *map* (make-grid (cdr (read-map "../maps/map.txt"))))

;; map
(row-id *map*) ; -> 0
(row-color *map*) ; -> 0
(row-primary *map*) ; -> nil
(position-to-id '(1 2) *size*) ; -> 9
(id-to-position 9 *size*) ; -> (1 2)
(color-to-position *map* 1) ; -> 1
(position-to-color *map* 16) ; -> 2
(color-to-prime-position *map* 1) ; -> 1
(color-list *map*) ; -> (1 2 3 4)
(is-in '(2 15 3) 15) ; -> T
(initially-colored *map*) ; -> (1 3 5 7 8 12 14 15)
(list-without-nth '(0 1 2 3 4) 3) ; -> (0 1 2 4)
(have-same-color *map* 3 5) ; -> T
(is-valid-position '(3 3) *size*) ; -> T
(is-valid-position '(3 4) *size*) ; -> NIL
(position-iterate '(3 3) 1 -1) ; -> (4 2)
(position-neighboors '(1 0) *size*) ; -> (11 14)
(mapcar (lambda (e) (id-to-position e *size*)) (position-neighboors '(3 3) *size*)) ; -> ((3 2) (2 3))
(position-visited-neighboors 5 '((1 (2 5)) (2 (1)) (5 (3 6)))) ; -> (3 6)
(is-visited-position 3 '((1 (2 5)) (2 (1)) (5 (3 6)))) ; -> T
(len *size*) ; -> 16

; solver
(choose-direction *map* '((1 (4))) *size*) ; -> 0
(position-is-obstacle *map* 8 `((10 (9)) (9 ())) 1 *size*) ; -> NIL
(any-obstacle *map* 9 '((10 (9)) (9 ())) 1 *size*) ; -> T
(any-unmarked-path *map* 9 `((10 (9)) (9 ())) 1 *size*) ; -> 13
(move-forward *map* 2 '((1 (4 5)) (2 (6))) *size*) ; -> ((1 (4 5)) (2 (6 6)) (6 NIL))
(turn-to-unmarked-path *map* 2 `((1 (4 5)) (2 ())) 1 *size*)
(color-found *map* 1 `((8 ())))
; (solve *map* *size*)

; gui
(is-in-path '((1 ((2 2) (3 3) (5 6))) (2 ((6 2) (4 5)))) '(6 2)) ; -> 2
(downgrade '((1 2) (2 5) (3 1)) '(1 2)) ; -> ((1 2))
(downgrade nil '(1 2)) ; -> ((1 2))
(add-to-path '((1 ((2 2) (3 3) (5 6))) (2 ((6 2) (4 5)))) 2 '(6 2)) ; -> ((1 ((2 2) (3 3) (5 6))) (2 ((6 2))))
(add-to-path (add-to-path-partial-erase '((1 ((1 0))) (3 ((1 1) (2 1))) (2 ((0 3) (1 3) (1 2) (2 2) (3 2) (3 1)))) 2 '(2 2)) 3 '(2 2)) ; -> ((1 ((2 2) (3 3) (5 6))) (2 ((6 2))))
(add-to-path-erase '((1 ((2 2) (3 3) (5 6))) (2 ((6 2) (4 5)))) 2 '(6 2)) ; -> ((1 ((2 2) (3 3) (5 6))) (2 ((6 2))))
(linearize '((1 2) (3 5) (5 6))) ; -> '(1 2 3 5 5 6)