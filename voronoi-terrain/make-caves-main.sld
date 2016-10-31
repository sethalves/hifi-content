(define-library (make-caves-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme r5rs)
          (srfi 1)
          (snow assert)
          (foldling command-line)
          (seth cout)
          (seth math-3d)
          (seth scad-model))
  (begin

    (define (read-arcs input-filename)
      (let ((in-port (open-input-file input-filename)))
        (let loop ((arcs '()))
          (let ((line (read-line in-port)))
            (if (eof-object? line)
                (begin
                  (close-port in-port)
                  arcs)
                (let* ((line-port (open-input-string line))
                       (radius (read line-port))
                       (p0 (read line-port))
                       (p1 (read line-port))
                       (p2 (read line-port)))
                  (loop (cons (list radius p0 p1 p2) arcs))))))))


    (define (split-on-x-axis points)
      ;; split any polygon lines that cross the x axis
      (let loop ((points (reverse (cons (car points) (reverse points))))
                 (output '()))
        (cond ((null? points)
               ;; shouldn't happen
               (snow-assert #f)
               points)
              ((null? (cdr points)) (reverse output))
              (else
               (let ((p0 (car points))
                     (p1 (cadr points)))
                 (cond ((or (and (< (vector2-x p0) 0) (> (vector2-x p1) 0))
                            (and (> (vector2-x p0) 0) (< (vector2-x p1) 0)))
                        (let* ((dx (- (vector2-x p1) (vector2-x p0)))
                               (dy (- (vector2-y p1) (vector2-y p0)))
                               ;; y = mx + b
                               (m (/ dy dx))
                               (b (- (vector2-y p0) (* m (vector2-x p0)))))
                          (loop (cdr points) (cons (vector 0 b) (cons p0 output)))))
                       (else
                        (loop (cdr points) (cons p0 output)))))))))



    (define (main-program)

      (define (usage why)
        (cerr why "\n")
        (cerr "make-caves [arguments] input-filename")
        (exit 1))

      (let* ((args (parse-command-line `((-?) (-h))))
             (input-filename #f)
             (extra-arguments '()))
        (for-each
         (lambda (arg)
           (case (car arg)
             ((-? -h) (usage ""))
             ((--)
              (set! extra-arguments (cdr arg)))))
         args)

        (if (not (= (length extra-arguments) 1))
            (usage "give input-filename as a command-line argument"))
        (set! input-filename (car extra-arguments))

        (let ((arcs (read-arcs input-filename)))
          (write-scad-file
           (fold
            append
            '()
            (map
             (lambda (arc)
               (let* ((radius (list-ref arc 0)) ;; of extruded circle
                      (p0 (list-ref arc 1)) ;; 3 points
                      (p1 (list-ref arc 2)) ;;  on circumference
                      (p2 (list-ref arc 3)) ;;  of the circle
                      ;; circle center in 3-space
                      (center (circle-center-from-circumference-points p0 p1 p2))
                      ;; radius of extrusion circle
                      (offset (vector3-length (vector3-diff center p0)))
                      (center->p0 (point-diff p0 center))
                      (center->p2 (point-diff p2 center))
                      (normal (vector3-normalize (cross-product center->p0 center->p2)))
                      ;; how many degrees to sweep the exstrusion
                      (sweep (angle-between-vectors center->p0 center->p2))
                      (tilt (rotation-between-vectors (vector 0.0 0.0 1.0) normal))
                      (spin (angle-between-vectors
                             (vector3-rotate (vector 1.0 0.0 0.0) tilt)
                             center->p0
                             normal))
                      (spin-quat (rotation-quaternion normal spin))
                      (upright-spin (angle-between-vectors normal (vector 0.0 1.0 0.0)))
                      (extrude-to-place (quaternion-normalize (multiply-quaternions spin-quat tilt)))
                      (extrude-to-place-inverse (quaternion-conjugate extrude-to-place))

                      (unrotated-cave-shape-points (list (vector -12 0)
                                                         (vector 12 0)
                                                         (vector 12 5)
                                                         (vector 10 8)
                                                         (vector 6 12)
                                                         (vector 3 14)
                                                         (vector -3 14)
                                                         (vector -6 12)
                                                         (vector -10 8)
                                                         (vector -12 5)))
                      (cave-shape-points
                       (map (lambda (p)
                              (let* ((a (atan (vector2-y p) (vector2-x p)))
                                     (a-up (+ a upright-spin))
                                     (len (vector2-length p))
                                     )
                                (vector (* (cos a-up) len)
                                        (* (sin a-up) len))
                                ))
                            unrotated-cave-shape-points))
                      ;; (make-cap
                      ;;  (lambda (angle)
                      ;;    (make-scad-translate
                      ;;     (vector (* (cos angle) offset)
                      ;;             (* (sin angle) offset)
                      ;;             0)
                      ;;     (make-scad-rotate
                      ;;      (vector pi/2 0 angle)
                      ;;      (make-scad-sphere (* radius 1.008) 24)))))

                      (make-cap
                       (lambda (angle)

                         ;; (cerr "IN: " cave-shape-points "\n")
                         ;; (cerr "OUT: " (split-on-x-axis cave-shape-points) "\n")

                         (make-scad-translate
                          (vector (* (cos angle) offset) (* (sin angle) offset) 0)
                          (make-scad-rotate
                           ;; (rotation-between-vectors normal (vector 0 1 0))
                           (combine-rotations
                            (rotation-quaternion (vector 1 0 0) (- pi/2))
                            extrude-to-place-inverse)

                          (make-scad-rotate-extrude
                           365 10
                           (make-scad-polygon
                            (filter (lambda (p) (>= (vector2-x p) 0))
                                    (split-on-x-axis unrotated-cave-shape-points)
                                    )
                            3))
                          )
                         )))

                      ;; (make-cap
                      ;;  (lambda (angle)
                      ;;    (make-scad-rotate
                      ;;     (vector (- pi/2) 0 0)
                      ;;     (make-scad-rotate-extrude
                      ;;      365 10
                      ;;      (make-scad-polygon
                      ;;       (filter (lambda (p) (>= (vector2-x p) 0))
                      ;;               (split-on-x-axis unrotated-cave-shape-points)
                      ;;               )
                      ;;       3)))))
                      )
                 (let* (;; (cave-shape (make-scad-circle radius 24))
                        (cave-shape (make-scad-polygon cave-shape-points 3))
                        (trans-cave-shape (make-scad-translate
                                           (vector offset 0 0)
                                           (list cave-shape))))
                   (list
                    (make-scad-translate
                     center
                     (make-scad-rotate
                      (quaternion->euler~zyx extrude-to-place)
                      (list
                       ;; circle segment
                       (make-scad-rotate-extrude sweep 10 (list trans-cave-shape))
                       ;; sphere around p0
                       (make-cap 0.0)
                       ;; sphere around p2
                       (make-cap sweep)


                       ;; XXX
                       ;; (make-scad-translate (vector 0 0 0) (make-scad-sphere 3 12))
                       ;; (make-scad-translate (vector 0 0 20) (make-scad-sphere 3 12))
                       ;; (make-scad-translate (vector 0 0 40) (make-scad-sphere 3 12))


                       )
                      )
                     )

                    ;; (make-scad-translate p0 (make-cap 0.0))
                    ;; (make-scad-translate p2 (make-cap sweep))


                    ;; XXX center sphere
                    ;; (make-scad-translate center (make-scad-sphere 5 12))
                    ;; (make-scad-translate p0 (make-scad-sphere 3 12))
                    ;; (make-scad-translate p1 (make-scad-sphere 3 12))
                    ;; (make-scad-translate p2 (make-scad-sphere 3 12))

                    ;; XXX normal
                    ;; (make-scad-translate (vector3-sum center (vector3-scale normal 5)) (make-scad-sphere 3 12))
                    ;; (make-scad-translate (vector3-sum center (vector3-scale normal 10)) (make-scad-sphere 3 12))
                    ;; (make-scad-translate (vector3-sum center (vector3-scale normal 15)) (make-scad-sphere 3 12))

                    ))))
             arcs))
           (current-output-port)))))

    ))
