#! /bin/bash
#| -*- scheme -*-
exec gosh -r7 "$0" "$@"
|#

(import (scheme base)
        (scheme write)
        (scheme inexact))

(let* ((pi 3.1415)
       (image-size 480)
       (center (/ image-size 2.0))
       (offset (- (/ image-size 2.0) 60))
       (x-fudge -10)
       (y-fudge 10))
  (display "\tconvert -pointsize 40 -fill black \\")
  (newline)
  (let loop ((angle 0)
             (n 3))
    (cond ((> angle (* 2 pi)) #t)
          (else
           (display "\t\t-draw 'text ")
           (let ((x (exact (round (+ (+ (* (cos angle) offset) center) x-fudge)))))
             (display (if (or (> n 9) (= n 0)) (- x 20) x)))
           (display ",")
           (display (exact (round (+ (+ (* (sin angle) offset) center) y-fudge))))
           (display " \"")
           (display (if (= n 0) 12 n))
           (display "\"")
           (display "' \\")
           (newline)
           (loop (+ angle (/ pi 6.0))
                 (modulo (+ n 1) 12)))))
  (display "\t\t$< $@")
  (newline)
  )
