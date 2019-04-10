(define-library (tree-tile-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (scheme cxr)
          (scheme inexact)
          (srfi 1)
          (srfi 27)
          (srfi 29)
          (srfi 69)
          (srfi 95)
          (snow assert)
          (foldling command-line)
          (seth cout)
          (seth strings)
          (seth math-3d)
          (seth raster)
          (seth image)
          (seth pbm)
          (seth model-3d)
          (seth obj-model)
          (seth scad-model)
          )
  (begin

    (define-record-type <tree>
      (make-tree position yaw scale model-index)
      tree?
      (position tree-position tree-set-position!)
      (yaw tree-yaw tree-set-yaw!)
      (scale tree-scale tree-set-scale!)
      (model-index tree-model-index tree-set-model-index!))


    (define (minimum-distance2 trees tree)
      (let loop ((trees trees)
                 (min-dx2 #f))
        (if (null? trees) min-dx2
            (let* ((other-tree (car trees))
                   (dx (distance-between-points-squared (tree-position tree) (tree-position other-tree))))
              (loop (cdr trees)
                    (if (or (not min-dx2) (< dx min-dx2)) dx min-dx2))))))


    (define (main-program)
      (let* ((tree-count 100)
             (tile-x-size 200)
             (tile-z-size 200)
             (random-i 14) ;; arbitrary, done for repeatability
             (random-j 3) ;; same as i
             (random-source (make-random-source))
             (random-integer (random-source-make-integers random-source))
             (tree-models (vector (make-empty-model) (make-empty-model) (make-empty-model)))
             (tree-dimensions (vector #f #f #f))
             (tile-model (make-empty-model))
             )

        (random-source-pseudo-randomize! random-source random-i random-j)

        (cerr "loading...\n")
        (read-obj-model-file "tree0.obj" (vector-ref tree-models 0))
        (read-obj-model-file "tree1.obj" (vector-ref tree-models 1))
        (read-obj-model-file "tree2.obj" (vector-ref tree-models 2))

        (do ((i 0 (+ i 1)))
            ((= i 3) #t)
          (vector-set! tree-dimensions i (model-dimensions (vector-ref tree-models i))))

        (cerr "placing trees...\n")
        (let loop ((trees '())
                   (count 0))
          (if (< count tree-count)
              (let* ((position (vector (random-integer tile-x-size) 0 (random-integer tile-z-size)))
                     (yaw (degrees->radians (random-integer 360)))
                     (scale (+ (* (/ (random-integer 100) 100.0) 0.5) 0.75))
                     (model-index (random-integer 3))
                     (candidate-tree (make-tree position yaw scale model-index))
                     (min-dx2 (minimum-distance2 trees candidate-tree))
                     (min-dx (if min-dx2 (sqrt min-dx2) #f))
                     )


                (cout count " " model-index " " min-dx2 "\n")

                (model-append! tile-model
                               (vector-ref tree-models (tree-model-index candidate-tree))
                               (lambda (v)
                                 (let* ((scale-m (matrix-scaling (tree-scale candidate-tree)))
                                        (rot-q (euler->quaternion (vector 0 (tree-yaw candidate-tree) 0)))
                                        (rot-m (quaternion->matrix rot-q))
                                        (trans-m (matrix-translation position))
                                        (m (matrix-A*B trans-m (matrix-A*B rot-m scale-m))))
                                   (vertex-transform m v))))


                (loop (cons candidate-tree trees) (+ count 1)))))

        (let* ((output-filename (string-append "tree-tile-" (number->string 0) ".obj"))
               (output-port (open-output-file output-filename)))
          (write-obj-model tile-model output-port)
          (close-output-port output-port))
        ))
    ))
