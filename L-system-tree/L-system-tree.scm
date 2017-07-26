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

(cond-expand
 (chibi
  (import (chibi match)))
 (chicken
  (import (matchable))))



;;;;;;; scad output

(define fn 16)

(define (scad-translate translation thunk)
  ;; output an openscad translate command
  (string-append
   (format "translate([~a, ~a, ~a]) {\n"
           (vector-ref translation 0)
           (vector-ref translation 1)
           (vector-ref translation 2))
   (thunk)
   (format "}\n")))

(define (scad-rotate rotation thunk)
  ;; output an openscad rotate command
  (let ((eu-rot (radians->degrees (quaternion->euler~zyx rotation))))
    (string-append
     (format "rotate([~a, ~a, ~a]) {\n"
             (vector-ref eu-rot 0)
             (vector-ref eu-rot 1)
             (vector-ref eu-rot 2))
     (thunk)
     (format "}\n"))))

(define (scad-scale scale thunk)
  ;; output an openscad scale command
  (string-append
   (format "scale([~a, ~a, ~a]) {\n"
           (vector-ref scale 0)
           (vector-ref scale 1)
           (vector-ref scale 2))
   (thunk)
   (format "}\n")))

(define (scad-transform translation rotation scale thunk)
  (scad-translate translation
                  (lambda ()
                    (scad-rotate rotation
                                 (lambda ()
                                   (scad-scale scale
                                               thunk))))))

(define (scad-sphere . maybe-radius)
  (let ((radius (if (null? maybe-radius) 1.0 (car maybe-radius))))
    (format "sphere(r = ~a, $fn=~a);\n" radius fn)))

(define (scad-cylinder base-radius top-radius length)
  (string-append
   (format "rotate([-90, 0, 0])\n")
   (format "cylinder(h = ~a, r1 = ~a, r2 = ~a, center = false, $fn=~a);\n"
           length base-radius top-radius fn)))



;;;;;;; povray output


(define current-translation (make-parameter (vector 0 0 0)))
(define current-rotation (make-parameter (euler->quaternion (vector 0 0 0))))
(define current-scale (make-parameter (euler->quaternion (vector 1 1 1))))

(define (pov-ray-translate translation thunk)
   (parameterize ((current-translation translation))
                 (thunk)))

(define (pov-ray-rotate rotation thunk)
  (parameterize ((current-rotation rotation))
                (thunk)))

(define (pov-ray-scale scale thunk)
   (parameterize ((current-scale scale))
                 (thunk)))

(define (pov-ray-transform translation rotation scale thunk)
  (pov-ray-translate translation
                  (lambda ()
                    (pov-ray-rotate rotation
                                 (lambda ()
                                   (pov-ray-scale scale
                                               thunk))))))

(define (pov-ray-transform-string)
  (let ((translation (current-translation))
        (rotation (radians->degrees (quaternion->euler~zyx (current-rotation))))
        (scale (current-scale)))
    (string-append
     (format "    scale<~a,~a,~a>\n"
             (vector-ref scale 0)
             (vector-ref scale 1)
             (vector-ref scale 2))
     (format "    rotate<~a,~a,~a>\n"
             (vector-ref rotation 0)
             (vector-ref rotation 1)
             (vector-ref rotation 2))
     (format "    translate<~a,~a,~a>\n"
             (vector-ref translation 0)
             (vector-ref translation 1)
             (vector-ref translation 2)))))


(define (pov-ray-sphere . maybe-radius)
  (let ((radius (if (null? maybe-radius) 1.0 (car maybe-radius))))
    (string-append
     (format "sphere{<0,0,0>, ~a\n" radius)
     (pov-ray-transform-string)

     "    texture{ pigment{color rgb<0,1.0,0>}"
     "             normal {bumps 0.75 scale 0.015}"
     "    }"

     (format "}\n"))))


(define (pov-ray-cylinder base-radius top-radius length)
  (string-append
   (format "cone{<0,0,0>,~a,<0,~a,0>,~a\n" base-radius length top-radius)
   (pov-ray-transform-string)

   "    texture{ pigment{color rgb<0.65,0.16,0.16>}"
   "             normal {bumps 0.75 scale 0.015}"
   "    }"


   (format "}\n")))


;;;;;;;;;;;;;;;;;;;;
;;
;; tree generation
;;
;;;;;;;;;;;;;;;;;;;;

(define (make-segment base-width base-length position rotation scale tree-definition
                      depth skip-trunk skip-leaves port output-type
                      transform sphere cylinder)
  (cond
   ((= (string-length tree-definition) 0) #t) ;; done
   ((or (eqv? #\o (string-ref tree-definition 0))
        (eqv? #\O (string-ref tree-definition 0)))
    (cond ((not skip-leaves)
           ;; leaf ball
           (display
            (transform position
                       rotation
                       (if (eqv? #\o (string-ref tree-definition 0))
                           (vector (* 8 scale) (* 2.5 scale) (* 8 scale))
                           (vector (* 8 scale) (* 6 scale) (* 8 scale)))
                       sphere)
            port)
           (display "\n" port))))
   (else
    ;; branch
    (let* ((my-length (* (- base-length depth) scale))
           (my-thickness (* (/ base-width (+ depth 1.)) scale))
           (child-thickness (* (/ base-width (+ depth 2.)) scale))
           (new-child (lambda (r) ;; r is euler radians
                        (let* ((child-rotation (combine-rotations (euler->quaternion r) rotation))
                               (child-back-up (vector3-rotate (vector 0 0.4 0) child-rotation))
                               (tip-offset (vector3-rotate (vector 0 my-length 0) rotation))
                               (tip (vector3-diff
                                     (vector3-sum position tip-offset)
                                     child-back-up)))
                          (make-segment base-width base-length tip
                                        child-rotation
                                        scale
                                        (substring tree-definition 1)
                                        (+ depth 1)
                                        skip-trunk skip-leaves
                                        port output-type transform sphere cylinder)))))

      (cond ((not skip-trunk)
             (display
              (transform position
                         rotation
                         (vector 1 1 1)
                         (lambda ()
                           (cylinder my-thickness child-thickness my-length)))
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
          ))

       ((eqv? #\x (string-ref tree-definition 0))
        (let* ((r0 (rotation-quaternion-d (vector 1 0 0) (+ 50 (* 5 depth))))
               (r1 (combine-rotations r0 (rotation-quaternion-d (vector 0 1 0) 120)))
               (r2 (combine-rotations r0 (rotation-quaternion-d (vector 0 1 0) -120))))
          (new-child (quaternion->euler r0))
          (new-child (quaternion->euler r1))
          (new-child (quaternion->euler r2))))

       ((eqv? #\f (string-ref tree-definition 0))
        (let* ((r0 (rotation-quaternion-d (vector 1 0 0) (+ 50 (* 5 depth))))
               (r1 (combine-rotations r0 (rotation-quaternion-d (vector 0 1 0) 120))))
          (new-child (quaternion->euler r0))
          (new-child (quaternion->euler r1))))

       (else
        (error "unknown tree definition character: "
               (string (string-ref tree-definition 0)))))))))


;;;;;;;;;;;;;;;;;;;;
;;
;; main program
;;
;;;;;;;;;;;;;;;;;;;;


(define (main-program)
  (define (usage why)
    (cout why "\n")
    (cout "L-system-tree [arguments] output.scm\n")
    (cout "   The output file should already exist, it will be appended to.\n")
    (cout "   -p --position x y z           Set the position of the trunk\n")
    (cout "   -r --rotation x y z           Set the rotation of the trunk\n")
    (cout "   -s --scale s                  Scale size of tree\n")
    (cout "   -w --base-width n             Thickness of truck.  default 10\n")
    (cout "   -h --hull                     output obj collision hull\n")
    (cout "   -o --output filename          File to write to\n")
    (cout "   -v --povray                   output in pov-ray format\n")
    (cout "   -t --tree ixyo                Define the shape of the tree\n")
    (cout "   -d --delete filename          subtract out scad file from output\n")
    (cout "   --fn n                        Set OpenSCAD's $fn variable\n")
    (cout "      i = no branching\n")
    (cout "      y = one becomes 2\n")
    (cout "      x = one becomes 3\n")
    (cout "      o = thin leaf ball\n")
    (cout "      O = fat leaf ball\n")
    (exit 1))


  (let* ((args (parse-command-line
                `(((-p --position -r --rotation) x y z)
                  ((-s --scale) s)
                  ((-w --base-width) width)
                  ((-h --hull))
                  (-t tree-definition)
                  ((-o --output) output-file)
                  ((-d --delete) subtract-file)
                  (--fn fn)
                  ((--skip-trunk --skip-leaves -v --povray --pov-ray))
                  (-?) (-h))))
         (pos zero-vector)
         (rot zero-vector)
         (scale 1.0)
         (base-width 2.0)
         (base-length 10.0)
         (output-as-hull #f)
         (tree-definition "iixyyyO")
         (skip-trunk #f)
         (skip-leaves #f)
         (output-file #f)
         (subtract-file #f)
         (output-povray #f)
         (output-port (current-output-port))
         (extra-arguments '()))

    (for-each
     (lambda (arg)
       (case (car arg)
         ((-? -h) (usage ""))
         ((-p --position)
          (set! pos (vector (string->number (list-ref arg 1))
                            (string->number (list-ref arg 2))
                            (string->number (list-ref arg 3)))))
         ((-r --rotation)
          (set! rot (vector (string->number (list-ref arg 1))
                            (string->number (list-ref arg 2))
                            (string->number (list-ref arg 3)))))
         ((-s --scale)
          (set! scale (string->number (cadr arg))))
         ((--fn)
          (set! fn (string->number (cadr arg))))
         ((--skip-trunk)
          (set! skip-trunk #t))
         ((--skip-leaves)
          (set! skip-leaves #t))
         ((-v --povray --pov-ray)
          (set! output-povray #t))
         ((-w)
          (set! base-width (string->number (cadr arg))))
         ((-h --hull)
          (set! output-as-hull #t))
         ((-t)
          (set! tree-definition (cadr arg)))
         ((-o --output)
          (if output-file (usage "give -o only once"))
          (set! output-file (cadr arg)))
         ((-d --delete)
          (if subtract-file (usage "give -d only once"))
          (set! subtract-file (cadr arg)))
         ((--)
          (set! extra-arguments (cdr arg))
          )))
     args)

    (if output-file
        (set! output-port (open-output-file output-file)))


    (cond (subtract-file
           (cout "difference() {\n"
                 "union() {\n\n"
                 output-port)))

    (make-segment base-width base-length pos
                  (euler->quaternion (degrees->radians rot))
                  scale
                  tree-definition 0
                  skip-trunk skip-leaves
                  output-port
                  (cond (output-as-hull 'hull)
                        (else 'normal))

                  (if output-povray pov-ray-transform scad-transform)
                  (if output-povray pov-ray-sphere scad-sphere)
                  (if output-povray pov-ray-cylinder scad-cylinder)
                  )

    (cond (subtract-file
           (cout "}\n"
                 "include <" subtract-file ">\n"
                 "}\n"
                 output-port)
           ))
    ))

(main-program)
