#! /bin/bash
#| -*- scheme -*-
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
SOURCE="$(readlink "$SOURCE")"
[[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
exec gosh \
-e '(set! *load-suffixes* (cons ".sld" *load-suffixes*))' \
-e "(append! *load-path* (list \"$DIR\" \".\"))" \
-r7 "$0" "$@"
|#


(import (scheme base)
        (scheme write)
        (srfi 60)
        (seth cout))

(define (n->bits n)
  (let loop ((n n) ;; decimal number
             (b 1) ;; bit value
             (i 0) ;; bit index
             (result '())) ;; list of "on" indexes
    (cond ((= n 0) result)
          ((> (bitwise-and n b) 0)
           (loop (- n b)
                 (* b 2)
                 (+ i 1)
                 (cons i result)))
          (else
           (loop n
                 (* b 2)
                 (+ i 1)
                 result)))))


(define centers (make-vector 64 #t))
(for-each
 (lambda (n) (vector-set! centers n #f))
 (list 5 10 48)) ;; no center-circle for straight runs

(let loop ((n 1))
  (cond ((<= n (+ 32 16 8 4 2 1))
         (cout "models/wires-" n ".stl: wires.scad common.scad\n")
         (cout "\tmkdir -p models\n")
         (cout "\topenscad ")
         (for-each (lambda (bit) (cout "-Dsegment_" bit "=1 ")) (n->bits n))
         (if (vector-ref centers n) (cout "-Dcenter=1 "))
         (cout "-o $@ $<\n\n")
         (loop (+ n 1)))))
