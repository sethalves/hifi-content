(define-library (make-caves-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme read)
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
               (let* ((radius (list-ref arc 0))
                      (p0 (list-ref arc 1))
                      (p1 (list-ref arc 2))
                      (p2 (list-ref arc 3))
                      (center (circle-center-from-circumference-points p0 p1 p2))
                      (offset (vector3-length (vector3-diff center p0)))
                      (center->p0 (point-diff p0 center))
                      (center->p1 (point-diff p1 center))
                      (center->p2 (point-diff p2 center))
                      (p0->p2 (point-diff p2 p0))
                      ;; (normal-a (vector3-normalize (cross-product center->p1 center->p2)))
                      ;; (normal-b (vector3-normalize (cross-product center->p2 center->p1)))
                      ;; (normal (if (> (vector3-y normal-a) (vector3-y normal-b))
                      ;;             normal-a normal-b))
                      (normal (vector3-normalize (cross-product center->p0 center->p2)))
                      (sweep (angle-between-vectors center->p0 center->p2))
                      (up (vector 0.0 1.0 0.0))
                      ;; (spin (angle-between-vectors (vector 1.0 0.0 0.0)
                      ;;                              (vector (vector3-x center->p0)
                      ;;                                      0.0
                      ;;                                      (vector3-z center->p0))
                      ;;                              (vector 0.0 1.0 0.0)))
                      ;; (tilt (angle-between-vectors
                      ;;        (vector 0.0 1.0 0.0)
                      ;;        normal)

                      ;;        ;; (vector 0.0 0.0 -1.0)

                      ;;        ;; (vector (vector3-x p0->p2)
                      ;;        ;;         (vector3-y p0->p2)
                      ;;        ;;         0.0)


                      ;;        ;; (vector3-rotate p0->p2 (euler->quaternion (vector 0 (- spin) 0)))

                      ;;        ;; (vector 1.0 0.0 0.0)
                      ;;        )

                      (tilt (rotation-between-vectors (vector 0.0 0.0 1.0) normal))
                      (spin (angle-between-vectors
                             (vector3-rotate (vector 1.0 0.0 0.0) tilt)
                             center->p0
                             normal))
                      (spin-quat (rotation-quaternion normal spin))
                      )

                      ;; (rotation (vector
                      ;;            (if (> (vector3-y normal) 0) tilt (- tilt))
                      ;;            spin 0))
                      ;;)

                 (cerr "--> " arc " : " center "\n")

                 (let* ((circle (make-scad-circle radius 10))
                        (trans-circle (make-scad-translate
                                       (vector offset 0 0)
                                       (list circle))))
                   (list
                    ;; sphere around p0
                    (make-scad-translate p0 (make-scad-sphere radius 12 12 12))
                    ;; arc
;                    (make-scad-translate
;                     p0
;                     (make-scad-rotate
;                      (vector 0 spin 0)
;                      (make-scad-rotate
;                       (vector (- tilt) 0 0)
;                       (make-scad-translate
;                        (vector (- offset) 0 0)
;                        (make-scad-rotate
;                         (vector (- pi/2) 0 0)
;                         (make-scad-rotate-extrude sweep 10 (list trans-circle))
;                         )
;                        )
;                       )
;                      )
;                     )


                    (make-scad-translate
                     center
;                      (make-scad-rotate
;                       (quaternion->euler (multiply-quaternions tilt spin-quat))

                     (make-scad-rotate
                      ;; (quaternion->euler~zyx tilt)
                      (quaternion->euler~zyx (multiply-quaternions spin-quat tilt))
                      (list
                       (make-scad-rotate-extrude sweep 10 (list trans-circle))
                       (make-scad-translate (vector 0 0 0) (make-scad-sphere 3 12 12 12))
                       (make-scad-translate (vector 0 0 20) (make-scad-sphere 3 12 12 12))
                       (make-scad-translate (vector 0 0 40) (make-scad-sphere 3 12 12 12))
                       )
                      )
;                     )
                   )

                     ;; XXX center sphere
                     (make-scad-translate center (make-scad-sphere 5 12 12 12))
                     ;; XXX p2 sphere
                     (make-scad-translate p1 (make-scad-sphere 5 12 12 12))
                     ;; XXX normal
                     (make-scad-translate (vector3-sum center (vector3-scale normal 5)) (make-scad-sphere 3 12 12 12))
                     (make-scad-translate (vector3-sum center (vector3-scale normal 10)) (make-scad-sphere 3 12 12 12))
                     (make-scad-translate (vector3-sum center (vector3-scale normal 15)) (make-scad-sphere 3 12 12 12))

                    ;; sphere around p2
                    (make-scad-translate p2 (make-scad-sphere radius 12 12 12))))))
             arcs))
           (current-output-port)))))

    ))
