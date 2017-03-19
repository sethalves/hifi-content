


(define-library (pnm-to-height-map-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (scheme cxr)
          (srfi 1)
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
          )
  (begin

    (define epsilon 0.0001)


    (define (main-program)
      (define (usage why)
        (cerr why "\n")
        (cerr "voronoi-terrain [arguments] pnm-input-filename")
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
            (usage "give pnm input-filename"))
        (set! input-filename (car extra-arguments))


        (let*  ((img (ppm-file->image input-filename)))
          (if (not img)
              (cerr "failed to read pbm from " input-filename "\n")
              (let ((height (raster-height img))
                    (width (raster-width img)))
                (do ((y (- height 1) (- y 1))) ;; make image axis be lower left
                    ((= y -1) #t)
                  (do ((x 0 (+ x 1)))
                      ((= x width) #t)
                    (let* ((pixel (raster-get-pixel img x y))
                           (red (pixel-red pixel)))
                      (if (< red 255)
                          (cout x " " red " " y "\n")))))
                #t)))))

    ))
