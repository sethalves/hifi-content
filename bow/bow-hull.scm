#!/usr/bin/env guile
!#

;; https://github.com/mkeeter/ao

;; (add-to-load-path (string-append (dirname (current-filename)) "/../bind/guile"))
;; (add-to-load-path "/home/seth/src/ao/bind/guile") ;; this makes (current-filename) return #f for some reason.  use relative path
(add-to-load-path "../../ao/bind/guile")

;; (use-modules (ao bind))
(use-modules (ao sys libao))

(ao-init-guile)
;; (use-modules (ao shapes) (ao transforms) (ao csg) (ao export))
(use-modules (ao sys operators) (ao sys user) (ao shapes) (ao csg) (ao transforms))

;; (define bbox (cube '(-0.020986 -0.675867 -0.053568) '(0.020237 0.630563 0.162057)))
(define bbox (cube '(-0.023 -0.40 -0.06) '(0.023 0.40 0.08)))
;; '(-1 -1 -1) '(1 1 1)
;; (ao-show bbox)
(ao-export-mesh bbox "bow_collision_hull.obj" 2 (list -0.023 -0.675867 -0.053568) (list 0.023 0.630563 0.182057) 2)
