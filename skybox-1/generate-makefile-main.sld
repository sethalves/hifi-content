(define-library (generate-makefile-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme inexact)
          (scheme process-context)
          (srfi 13)
          (srfi 27)
          (srfi 29)
          (seth cout)
          (seth math-3d)
          )

  (begin

    (define (pad n)
      (string-pad (number->string n) 2 #\0))

    (define (main-program)
      (let loop ((i 0))
        (cond ((= i 10) #t)
              (else
               ;; trees2-row0.pnm: trees12.pnm blank.pnm
               (cout (format "trees~a-row0.pnm: trees~a.pnm blank.pnm\n" i (pad (* i 6))))
               ;; pnmcat -lr blank.pnm $^ blank.pnm > $@
               (cout (format "\tpnmcat -lr blank.pnm $^ blank.pnm > $@\n\n"))

               ;; trees2-row1.pnm: trees13.pnm trees14.pnm trees15.pnm trees16.pnm
               (cout (format "trees~a-row1.pnm: trees~a.pnm trees~a.pnm trees~a.pnm trees~a.pnm\n"
                             i (pad (+ (* i 6) 1)) (pad (+ (* i 6) 2)) (pad (+ (* i 6) 3)) (pad (+ (* i 6) 4))))
               ;; pnmcat -lr $^ > $@
               (cout (format "\tpnmcat -lr $^ > $@\n\n"))

               ;; trees2-row2.pnm: trees17.pnm blank.pnm
               (cout (format "trees~a-row2.pnm: trees~a.pnm blank.pnm\n" i (pad (+ (* i 6) 5))))
               ;; pnmcat -lr blank.pnm $^ blank.pnm > $@
               (cout (format "\tpnmcat -lr blank.pnm $^ blank.pnm > $@\n\n"))

               ;; trees-skybox-2.pnm: trees2-row0.pnm trees2-row1.pnm trees2-row2.pnm
               (cout (format "trees-skybox-~a.pnm: trees~a-row0.pnm trees~a-row1.pnm trees~a-row2.pnm\n" i i i i))
               ;; pnmcat -tb $^ > $@
               (cout (format "\tpnmcat -tb $^ > $@\n\n\n\n"))

               (loop (+ i 1)))))

      (let loop ((v 0))
        (cond ((>= v 10) #t)
              (else
               (cout (format "trees~a.png trees~a.png trees~a.png trees~a.png trees~a.png trees~a.png \\\n"
                             (pad (+ (* v 6) 0))
                             (pad (+ (* v 6) 1))
                             (pad (+ (* v 6) 2))
                             (pad (+ (* v 6) 3))
                             (pad (+ (* v 6) 4))
                             (pad (+ (* v 6) 5))))
               (loop (+ v 1)))))

      (cout ": trees.pov trees.ini\n")
      (cout "\tpovray +W${WIDTH} +H${HEIGTH} trees.ini\n")
      )

    ))
