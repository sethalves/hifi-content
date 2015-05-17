#! /bin/sh
#| -*- scheme -*-
exec csi -include-path /usr/local/share/scheme -s $0 "$@"
|#
(use r7rs)
(include "snow/assert.sld")
(include "snow/input-parse.sld")
(include "foldling/command-line.sld")
(include "seth/cout.sld")
(include "seth/math-3d.sld")
;; (include "seth/obj-model.sld")

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (foldling command-line)
        ;; (seth obj-model)
        (seth math-3d)
        (seth cout)
        )


(define fn 8)

(define (make-segment base-width base-length position rotation tree-definition depth skip-trunk skip-leaves port output-type)

  ;; output an openscad translate command
  (define (translate v)
    (display (format "translate([~a, ~a, ~a])\n"
                     (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))
             port))

  ;; output an openscad rotate command
  (define (rotate v)
    (let ((eu-rot (radians->degrees (quaternion->euler~zyx v))))
      (display (format "rotate([~a, ~a, ~a])\n"
                       (vector-ref eu-rot 0)
                       (vector-ref eu-rot 1)
                       (vector-ref eu-rot 2))
               port)))

  (cond
   ((= (string-length tree-definition) 0) #t) ;; done
   ((or (eqv? #\o (string-ref tree-definition 0))
        (eqv? #\O (string-ref tree-definition 0)))
    (cond ((not skip-leaves)
           ;; leaf ball
           (translate position)
           (rotate rotation)
           (if (eqv? #\o (string-ref tree-definition 0))
               (display (format "resize([10, 2.5, 10]) sphere(r = 1, $fn=~a);\n" fn) port)
               (display (format "resize([10, 8, 10]) sphere(r = 2, $fn=~a);\n" fn) port))
           (display "\n" port))))
   (else
    ;; branch
    (let* ((my-length (- base-length depth))
           (my-thickness (/ base-width (+ depth 1.)))
           (child-thickness (/ base-width (+ depth 2.)))
           (tip-offset (vector3-rotate (vector 0 my-length 0) rotation))
           (tip (vector3-sum position tip-offset))
           (new-child (lambda (r) ;; r is euler radians
                        (make-segment base-width base-length tip
                                      (combine-rotations (euler->quaternion r)
                                                         rotation)
                                      (substring tree-definition 1)
                                      (+ depth 1)
                                      skip-trunk skip-leaves
                                      port output-type))))

      (cond ((not skip-trunk)
             (translate position)
             (rotate rotation)
             (display (format "rotate([-90, 0, 0])\n") port)
             (display (format "cylinder(h = ~a, r1 = ~a, r2 = ~a, center = false, $fn=~a);\n\n"
                              my-length my-thickness child-thickness fn)
                      port)))
      (cond
       ((eq? output-type 'hull) #t) ;; don't recurse
       ((eqv? #\i (string-ref tree-definition 0))
        ;; (new-child (degrees->radians (vector 0 30 0)))
        (new-child (degrees->radians (vector 0 0 0)))
        )
       ((eqv? #\y (string-ref tree-definition 0))
        ;; (new-child (degrees->radians (vector (+ 15 (* 5 depth)) -40 120)))
        ;; (new-child (degrees->radians (vector (- -15 (* 5 depth)) 40 120)))
        ;;
        ;; (new-child (degrees->radians (vector (+ 15 (* 5 depth)) -40 0)))
        ;; (new-child (degrees->radians (vector (- -15 (* 5 depth)) 40 0)))
        ;;
        ;; (let* ((r0 (rotation-quaternion-d (vector 1 0 0) (+ 15 (* 5 depth))))
        ;;        (r1 (rotation-quaternion-d (vector 1 0 0) (- -15 (* 5 depth))))
        ;;        (r0~ (combine-rotations (rotation-quaternion-d (vector 0 1 0) -40) r0))
        ;;        (r1~ (combine-rotations (rotation-quaternion-d (vector 0 1 0) 40) r1))
        ;;        ;; (r0~~ (combine-rotations (rotation-quaternion-d (vector 0 0 1) 120) r0~))
        ;;        ;; (r1~~ (combine-rotations (rotation-quaternion-d (vector 0 0 1) 120) r1~))
        ;;        )
        ;;   (new-child (quaternion->euler r0~))
        ;;   (new-child (quaternion->euler r1~))
        ;;   )
        ;;
        ;; (new-child (degrees->radians (vector (+ 15 (* 5 depth)) 40 0)))
        ;; (new-child (degrees->radians (vector (- -15 (* 5 depth)) 40 0)))
        ;;
        (let* ((r0 (rotation-quaternion-d (vector 0 1 0) 120))
               (r1 (rotation-quaternion-d (vector 0 1 0) 120))
               (r0~ (combine-rotations r0 (rotation-quaternion-d (vector 0 0 1) -40)))
               (r1~ (combine-rotations r1 (rotation-quaternion-d (vector 0 0 1) 40)))
               (r0~~ (combine-rotations r0~ (rotation-quaternion-d (vector 1 0 0) (+ 15 (* 5 depth)))))
               (r1~~ (combine-rotations r1~ (rotation-quaternion-d (vector 1 0 0) (- -15 (* 5 depth)))))
               )
          (new-child (quaternion->euler r0~~))
          (new-child (quaternion->euler r1~~))
          )

        )
       ((eqv? #\x (string-ref tree-definition 0))
        (let* ((r0 (rotation-quaternion-d (vector 1 0 0) (+ 50 (* 5 depth))))
               (r1 (combine-rotations r0 (rotation-quaternion-d (vector 0 1 0) 120)))
               (r2 (combine-rotations r0 (rotation-quaternion-d (vector 0 1 0) -120))))
          (new-child (quaternion->euler r0))
          (new-child (quaternion->euler r1))
          (new-child (quaternion->euler r2))))
       (else
        (error "unknown tree definition character: "
               (string (string-ref tree-definition 0)))))))))

(define (main-program)
  (define (usage why)
    (cout why "\n")
    (cout "L-system-tree [arguments] output.scm\n")
    (cout "   The output file should already exist, it will be appended to.\n")
    (cout "   -p --position x y z           Set the position of the trunk\n")
    (cout "   -r --rotation x y z           Set the rotation of the trunk\n")
    (cout "   -w --base-width n             Thickness of truck.  default 10\n")
    (cout "   -h --hull                     output obj collision hull\n")
    (cout "   -o --output filename          File to write to\n")
    (cout "   -t --tree ixyo                Define the shape of the tree\n")
    (cout "      i = no branching\n")
    (cout "      y = one becomes 2\n")
    (cout "      x = one becomes 3\n")
    (cout "      o = thin leaf ball\n")
    (cout "      O = fat leaf ball\n")
    (exit 1))


  (let* ((args (parse-command-line
                `(((-p --position -r --rotation) x y z)
                  ((-w --base-width) width)
                  ((-h --hull))
                  (-t tree-definition)
                  (-o output-file)
                  ((--skip-trunk --skip-leaves))
                  (-?) (-h))))
         (pos zero-vector)
         (rot zero-vector)
         (base-width 2.0)
         (base-length 10.0)
         (output-as-hull #f)
         (tree-definition "iixyyyO")
         (skip-trunk #f)
         (skip-leaves #f)
         (output-file #f)
         (output-port (current-output-port))
         (extra-arguments '()))

    (for-each
     (lambda (arg)
       (case (car arg)
         ((-? -h) (usage ""))
         ((-p --position)
          (set! pos (vector (string->number (list-ref args 1))
                            (string->number (list-ref args 2))
                            (string->number (list-ref args 3)))))
         ((-r --rotation)
          (set! rot (vector (string->number (list-ref args 1))
                            (string->number (list-ref args 2))
                            (string->number (list-ref args 3)))))
         ((--skip-trunk)
          (set! skip-trunk #t))
         ((--skip-leaves)
          (set! skip-leaves #t))
         ((-w)
          (set! base-width (string->number (cadr arg))))
         ((-h --hull)
          (set! output-as-hull #t))
         ((-t)
          (set! tree-definition (cadr arg)))
         ((-o)
          (if output-file (usage "give -o only once"))
          (set! output-file (cadr arg)))
         ((--)
          (set! extra-arguments (cdr arg))
          )))
     args)

    (if output-file
        (set! output-port (open-output-file output-file)))

    (make-segment base-width base-length pos
                  (euler->quaternion (degrees->radians rot))
                  tree-definition 0
                  skip-trunk skip-leaves
                  output-port
                  (cond (output-as-hull 'hull)
                        (else 'normal)))
    ))

(main-program)
