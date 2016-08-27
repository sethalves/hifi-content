#!/usr/bin/env guile
!#

;; https://github.com/mkeeter/ao

;; (add-to-load-path (string-append (dirname (current-filename)) "/../bind/guile"))
(add-to-load-path "/home/seth/src/ao/bind/guile")

(use-modules (ao bind))
(ao-init-guile)
(use-modules (ao shapes) (ao transforms) (ao csg) (ao export))

;; (define bbox (cube '(-0.020986 -0.675867 -0.053568) '(0.020237 0.630563 0.162057)))
(define bbox (cube '(-0.020986 -0.675867 -0.053568) '(0.020237 0.630563 0.162057)))
;; '(-1 -1 -1) '(1 1 1)
;; (ao-show bbox)
(ao-export-mesh bbox "bow_collision_hull.obj" '(-0.020986 -0.675867 -0.053568) '(0.020237 0.630563 0.162057) 10)
