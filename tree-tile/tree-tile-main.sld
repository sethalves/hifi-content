(define-library (tree-tile-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (scheme cxr)
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

    (define (main-program)
      (cerr "loading...\n")
      (let* ((tree-count 10)
             (tile-x-size 1000)
             (tile-z-size 1000)
             (random-i 12) ;; arbitrary, done for repeatability
             (random-j 3) ;; same as i
             (random-source (make-random-source))
             (random-integer (random-source-make-integers random-source))
             ;;
             (tree0-model (make-empty-model))
             (tree1-model (make-empty-model))
             (tree2-model (make-empty-model))
             (tree-models (vector tree0-model tree1-model tree2-model))
             ;;
             )

        (random-source-pseudo-randomize! random-source random-i random-j)

        (cerr "loading trees...\n")

        (read-obj-model-file "tree0.obj" tree0-model)
        (read-obj-model-file "tree1.obj" tree1-model)
        (read-obj-model-file "tree2.obj" tree2-model)

        (cerr "done loading.\n")


        (let loop ((tree-positions '())
                   (count 0))
          (if (< count tree-count)
              (let ((x-pos (random-integer tile-x-size))
                    (z-pos (random-integer tile-z-size))
                    (scale (+ (* (/ (random-integer 100) 100.0) 0.5) 0.75)))
                (cout "(" x-pos " " z-pos " " scale "\n")
                (loop tree-positions (+ count 1)))))

        ))
    ))

