#!/usr/bin/env guile
!#

(add-to-load-path "../../ao/bind/guile")

(use-modules (ao bind))

(ao-init-guile)
(use-modules (ao operators) (ao user) (ao export)
             (ao shapes) (ao csg) (ao transforms))


(define hull-length 12)
(define hull-width 8)
(define hull-thickness 0.25)
(define hull-wall-height 2)

;;;;;;;;;;;;;;;;;

(define hull-half-length (/ hull-length 2.0))
(define hull-half-width (/ hull-width 2.0))
(define hull-half-diff (- hull-half-length hull-half-width))
(define H (* hull-length 2))
(define L (- H))

;;;;;;;;;;;;;;;;;

(define outer-hull (intersection (sphere (list 0 0 (- hull-half-diff)) hull-half-length)
                                 (sphere (list 0 0 hull-half-diff) hull-half-length)))

(define inner-hull-space (intersection (sphere (list 0 0 (- hull-half-diff)) (- hull-half-length hull-thickness))
                                       (sphere (list 0 0 hull-half-diff) (- hull-half-length hull-thickness))))

(define inner-floor (intersection inner-hull-space
                                  (extrude-z (rectangle (list L 0) (list H H)) L H)))

(define full-hull (difference outer-hull inner-floor))

(define boat-hull
  (difference
   full-hull
   (extrude-z (rectangle (list L hull-wall-height) (list H H)) L H)
   ))


(ao-export-mesh boat-hull "boat-hull.obj" (list L L L) (list H H H) 11.0)
