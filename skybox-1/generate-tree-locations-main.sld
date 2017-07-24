(define-library (generate-tree-locations-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme inexact)
          (scheme process-context)
          (srfi 27)
          (seth cout)
          (seth math-3d)
          )

  (begin

    (define (main-program)
      (let* ((random-i 12)
             (random-j 3)
             (random-source (make-random-source))
             (random-integer (random-source-make-integers random-source))
             (border-gap 200)
             )

        ;; do this so the results are repeatable...
        (random-source-pseudo-randomize! random-source random-i random-j)

        (let loop ((count 0))
          (let* ((a (/ (random-integer 180) pi)) ;; a and d chose tree location
                 (d (+ (random-integer 500) 40))
                 (x (* (cos a) d))
                 (y 0.0)
                 (z (* (sin a) d))
                 (r (random-integer 360)) ;; rotation
                 (w (+ (random-integer 3) 3))
                 (s (+ (random-integer 2) 2)) ;; size
                )
            (cond ((>= count 400)
                   ;; done
                   #t)
                  (else
                   ;; keep this one
                   (cout (exact (round x)) " "
                         (exact (round z)) " "
                         (exact (round y)) " "
                         (exact (round r)) " "
                         (exact (round s)) " "
                         (exact (round w)) "\n")
                   (loop (+ count 1))))))
        ))
    ))
