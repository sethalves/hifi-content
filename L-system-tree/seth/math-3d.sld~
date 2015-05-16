
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; utilites for doing 3d math
;;
;; all the matrix stuff in here is row-major, meaning the
;; vector that holds other vectors holds rows.
;;

(define-library (seth math-3d)
  (export pi
          pi*2
          pi/2
          number->pretty-string
          vector-max
          delete-from-vector
          range
          average
          +f
          fmod
          vector2-x
          vector2-y
          vector3-x
          vector3-y
          vector3-z
          vector3->4
          vector4->3
          point-x
          point-y
          point-z
          line-a
          line-b
          make-line
          quat-s
          quat-x
          quat-y
          quat-z
          quaternion
          atan2
          quaternion->euler
          euler->quaternion
          rotation-quaternion
          rotation-quaternion-d
          zero-rotation
          zero-vector
          parse-vector-full
          parse-vector
          radians->degrees
          degrees->radians
          vector2-sum
          vector3-sum
          vector-list-sum
          vector3-diff
          point-diff
          vector2-diff
          vector3-abs
          vector2-scale
          vector3-scale
          vector-scale
          almost=
          vector3-equal?
          vector2-average
          vector3-average
          vector3-normalize
          quaternion-equal?
          quaternion-conjugate
          multiply-quaternions
          vector3-rotate
          combine-rotations
          dot-product
          vector3-magnitude
          cross-product
          distance-between-points
          distance-from-point-to-plane
          line-plane-intersection
          segment-plane-intersection
          triangle-plane-intersection
          triangle-is-degenerate?
          triangle-normal
          angle-between-vectors
          quaternion-normalize
          quaternion->matrix
          vector-numbers->strings
          triangle-number
          matrix-new
          matrix-width
          matrix-height
          matrix-ref
          matrix-set!
          matrix-row
          matrix-col
          vector->column-vector
          column-vector->vector
          matrix-map
          vector-minor
          matrix-minor
          matrix-determinant
          matrix-transpose
          matrix-A*B
          matrix-*
          matrix-translation
          matrix-rotation-x
          matrix-rotation-y
          matrix-rotation-z
          matrix-scaling
          matrix->string
          matrix-print
          circle-center-from-circumference-points-2d
          matrix-squash-z
          matrix-unsquash-z
          circle-center-from-circumference-points
          sigmoid
          mm->in
          in->mm
          clamp-number
          normalize-angle
          normalize-angle+
          angle-diff
          bounding-box2-is-type?
          bounding-box2-x0 bounding-box2-set-x0!
          bounding-box2-y0 bounding-box2-set-y0!
          bounding-box2-x1 bounding-box2-set-x1!
          bounding-box2-y1 bounding-box2-set-y1!
          bounding-box2-copy
          bounding-box2-=?
          bounding-box2-add-point

          )
  (import (scheme base)
          (scheme write)
          (scheme inexact)
          (scheme char)
          (srfi 1)
          (srfi 13)
          (seth cout)
          )


  (begin

    (define pi 3.14159265358979323846)
    (define pi*2 (* pi 2.0))
    (define pi/2 (/ pi 2.0))


    (define (number->pretty-string v places)
      ;; I didn't want to write this.  Why did I have to write this?
      ;; number->string will return scientific notation on some platforms.
      (define n->s (vector "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (define (first-power v)
        (let loop ((p 1))
          (if (> (expt 10 (inexact p)) v) (- p 1)
              (loop (+ p 1)))))
      (define (next-digit v p)
        (let loop ((v v)
                   (result 0))
          (let ((x (expt 10 (inexact p))))
            (cond ((< v x) result)
                  (else
                   (loop (- v x) (+ result 1)))))))
      (define (do-loop v)
        (let loop ((p (if (>= v 0) (first-power v) (first-power (- v))))
                   (result "")
                   (v v))
          (cond ((< v (expt 10 (inexact (- places)))) result)
                ((= v 0) result)
                (else
                 (let* ((n (next-digit v p))
                        (next-v (- v (* n (expt 10 (inexact p))))))
                   (loop (- p 1)
                         (if (and (> next-v 0) (= p 0))
                             (string-append result (vector-ref n->s n) ".")
                             (string-append result (vector-ref n->s n)))
                         next-v))))))
      (let ((result (if (< v 0)
                        (string-append "-" (do-loop (- v)))
                        (do-loop v))))
        (cond ((equal? result "") "0")
              ((equal? result "-") "0")
              (else result))))


    (define (vector-max v)
      (let loop ((i 0)
                 (m #f))
        (cond ((= i (vector-length v)) m)
              ((eq? m #f) (loop (+ i 1) (vector-ref v i)))
              ((> (vector-ref v i) m) (loop (+ i 1) (vector-ref v i)))
              (else (loop (+ i 1) m)))))


    (define (delete-from-vector v n)
      ;; copy vector, skip over entry n
      (let* ((v-len (vector-length v))
             (ret (make-vector (- v-len 1) 0)))
        (let loop ((i 0))
          (cond ((= i v-len) ret)
                ((= i n) (loop (+ i 1)))
                ((< i n)
                 (vector-set! ret i (vector-ref v i))
                 (loop (+ i 1)))
                (else ;; (> i n)
                 (vector-set! ret (- i 1) (vector-ref v i))
                 (loop (+ i 1)))))))


    (define (range low high)
      (let loop ((n low)
                 (ret (list)))
        (if (= n high) (reverse ret)
            (loop (+ n 1) (cons n ret)))))


    (define (average v0 v1) (/ (+ v0 v1) 2.0))


    (define (+f a b . limits-oa)
      ;; add two numbers, either of which may be #f.  keep the answer
      ;; within limits, if provided.
      (let* ((limits (cond ((null? limits-oa) (list #f #f))
                           ((null? (cdr limits-oa)) (list (car limits-oa) #f))
                           (else limits-oa)))
             (min-value (car limits))
             (max-value (cadr limits))
             (v (cond ((not a) #f)
                      ((not b) #f)
                      (else (+ a b)))))
        (cond ((not v) #f)
              ((and min-value (< v min-value)) min-value)
              ((and max-value (> v max-value)) max-value)
              (else v))))


    (define (fmod v m)
      (cond ((and (vector? v) (= (vector-length v) 3))
             (vector (fmod (vector3-x v) m)
                     (fmod (vector3-y v) m)
                     (fmod (vector3-z v) m)))
            ((and (> v 0.0) (> v m)) (fmod (- v m) m))
            ((and (< v 0.0) (< v (- m))) (fmod (+ v m) m))
            (else v)))

    (define (vector2-x v) (vector-ref v 0))
    (define (vector2-y v) (vector-ref v 1))

    (define (vector3-x v) (vector-ref v 0))
    (define (vector3-y v) (vector-ref v 1))
    (define (vector3-z v) (vector-ref v 2))

    (define (vector3->4 v)
      ;; converts a vector3 into a 4 member column vector
      (vector (vector (vector-ref v 0))
              (vector (vector-ref v 1))
              (vector (vector-ref v 2))
              (vector 1)))

    (define (vector4->3 v)
      ;; converts a 4 member column vector into a 3 member vector
      (vector (vector-ref (vector-ref v 0) 0)
              (vector-ref (vector-ref v 1) 0)
              (vector-ref (vector-ref v 2) 0)))


    (define point-x vector3-x)
    (define point-y vector3-y)
    (define point-z vector3-z)


    (define (line-a line) (vector-ref line 0))
    (define (line-b line) (vector-ref line 1))
    (define (make-line point-a point-b)
      (vector point-a point-b))


    (define (quat-s q) (vector-ref q 0))
    (define (quat-x q) (vector-ref q 1))
    (define (quat-y q) (vector-ref q 2))
    (define (quat-z q) (vector-ref q 3))

    (define (quaternion s x y z) (vector s x y z))


    (define (atan2 y x)
      (cond ((> y 0.) (cond ((> x 0.) (atan (/ y x)))
                            ((< x 0.) (- pi (atan (/ (- y) x))))
                            ((= x 0.) (/ pi 2.))))
            ((< y 0.) (cond ((> x 0.) (- (* 2 pi) (atan (/ (- y) x))))
                            ((< x 0.) (+ (atan (/ y x)) pi))
                            ((= x 0.) (* pi 1.5))))
            ((= y 0.) (cond ((> x 0.) 0)
                            ((< x 0.) pi)
                            ((= x 0.) #f))))) ;; error


    (define (wrong-quaternion->euler q)
      ;; convert a quaternion into rotations about x, y, and z axis.
      ;; theta = elevation angle
      ;; phi = bank angle
      ;; psi = heading angle
      (let ((s (quat-s q))
            (x (quat-x q))
            (y (quat-y q))
            (z (quat-z q)))
        (let ((m11 (+ (* 2 s s) (* 2 x x) -1))
              (m12 (+ (* 2 x y) (* 2 s z)))
              (m13 (- (* 2 x z) (* 2 s y)))
              (m23 (+ (* 2 y z) (* 2 s x)))
              (m33 (+ (* 2 s s) (* 2 z z) -1)))
          (let ((psi (atan2 m12 m11))
                (theta (asin (- m13)))
                (phi (atan2 m23 m33)))
            (vector phi (if (< (cos theta) 0.) (+ theta pi) theta) psi)))))



    (define (quaternion->euler r)
      (let* ((rs (quat-s r))
             (rx (quat-x r))
             (ry (quat-y r))
             (rz (quat-z r))
             (tx (* rx rx))
             (ty (* ry ry))
             (tz (* rz rz))
             (ts (* rs rs))
             (m (+ tx ty tz ts)))
        (if (almost= m 0 0.00001) (vector 0 0 0)
            (let* ((n (* 2 (+ (* ry rs) (* rx rz))))
                   (p (- (* m m) (* n n))))
              (cond ((> p 0)
                     (vector (atan2 (* 2 (- (* rx rs) (* ry rz)))
                                    (+ (- tx) (- ty) tz ts))
                             (atan2 n (sqrt p))
                             (atan2 (* 2 (- (* rz rs) (* rx ry)))
                                    (+ tx (- ty) (- tz) ts))))
                    ((> n 0)
                     (vector 0 (/ pi 2) (atan2 (+ (* rz rs) (* rx ry))
                                               (+ .5 (- tx) (- tz)))))
                    (else
                     (vector 0 (- (/ pi 2)) (atan2 (+ (* rz rs) (* rx ry))
                                                   (+ .5 (- tx) (- tz))))))))))

    ;; http://rpgstats.com/wiki/index.php?title=LibraryRotationFunctions
    ;; vector RotToEuler(rotation r)
    ;; {
    ;;     rotation t=<r.x*r.x,r.y*r.y,r.z*r.z,r.s*r.s>;
    ;;     float m=(t.x + t.y + t.z + t.s);
    ;;     if(m==0) return <0,0,0>;
    ;;     float n=2*(r.y*r.s + r.x*r.z);
    ;;     float p=m*m-n*n;
    ;;     if(p>0)
    ;;         return < llAtan2(2.0 * (r.x*r.s - r.y*r.z),(-t.x - t.y + t.z + t.s)),
    ;;                  llAtan2(n,llSqrt(p)), llAtan2(2.0 * (r.z*r.s - r.x*r.y),( t.x - t.y - t.z + t.s))>;
    ;;     else if(n>0)
    ;;         return < 0, PI_BY_TWO, llAtan2((r.z*r.s + r.x*r.y), 0.5 - t.x - t.z)>;
    ;;     else
    ;;         return < 0, -PI_BY_TWO, llAtan2((r.z*r.s + r.x*r.y), 0.5 - t.x - t.z)>;
    ;; }



    (define (euler->quaternion eu)
      (let* ((v (vector3-scale eu 0.5))
             (ax (sin (vector3-x v)))
             (aw (cos (vector3-x v)))
             (by (sin (vector3-y v)))
             (bw (cos (vector3-y v)))
             (cz (sin (vector3-z v)))
             (cw (cos (vector3-z v))))
        (vector (- (* aw bw cw) (* ax by cz))
                (+ (* aw by cz) (* ax bw cw))
                (- (* aw by cw) (* ax bw cz))
                (+ (* aw bw cz) (* ax by cw))
                )))

    (define (euler->quaternion-1 eu)
      ;; this gives the same results as euler->quaternion, but it's easier to
      ;; understand what's happening.  the vector is rotated around the x, y, and
      ;; then z axis, but the axis rotate also.  the y rotation isn't around
      ;; <0, 1, 0>, it's around <0, 1, 0> after it's been rotated around the x
      ;; axis.
      (let* ((x-rot (rotation-quaternion (vector 1 0 0) (vector3-x eu)))
             (y-axis (vector3-rotate (vector 0 1 0) x-rot))
             (y-rot (rotation-quaternion y-axis (vector3-y eu)))
             (z-axis (vector3-rotate (vector 0 0 1)
                                     (combine-rotations x-rot y-rot)))
             (z-rot (rotation-quaternion z-axis (vector3-z eu))))
        (combine-rotations x-rot y-rot z-rot)))


    (define (euler->quaternion-2 eu)
      ;; don't use this one, it's bogus.
      (combine-rotations
       (rotation-quaternion (vector 1 0 0) (vector3-x eu))
       (rotation-quaternion (vector 0 1 0) (vector3-y eu))
       (rotation-quaternion (vector 0 0 1) (vector3-z eu))))



    ;; rotation EulerToRotFast(vector v)
    ;; {
    ;;     v/=2;
    ;;     float ax=llSin(v.x);
    ;;     float aw=llCos(v.x);
    ;;     float by=llSin(v.y);
    ;;     float bw=llCos(v.y);
    ;;     float cz=llSin(v.z);
    ;;     float cw=llCos(v.z);
    ;;     return <aw*by*cz + ax*bw*cw,
    ;;             aw*by*cw - ax*bw*cz,
    ;;             aw*bw*cz + ax*by*cw,
    ;;             aw*bw*cw - ax*by*cz>;
    ;; }

    (define (rotation-quaternion axis angle)
      (let* ((unit-axis (vector3-normalize axis))
             (sine-half-angle (sin (/ angle 2.0)))
             (cosine-half-angle (cos (/ angle 2.0))))
        (quaternion
         cosine-half-angle
         (* sine-half-angle (vector3-x unit-axis))
         (* sine-half-angle (vector3-y unit-axis))
         (* sine-half-angle (vector3-z unit-axis)))))

    (define (rotation-quaternion-d axis angle)
      (rotation-quaternion axis (degrees->radians angle)))

    (define zero-rotation (quaternion 1. 0. 0. 0.))
    (define zero-vector (vector 0. 0. 0.))

    ;; XXX make this deal with vectors of any length

    (define (parse-vector-full entire-str)
      ;; parse a string like <-4.0, 3  , 20>
      ;;        < xxx , yyy , zzz >
      ;; state: 0 111 1 222 2 333 3
      ;;
      ;; returns (list #(x,y,z) (list 'reason characters details ...)

      (define number-characters (list #\- #\. #\0 #\1 #\2 #\3 #\4 #\5
                                      #\6 #\7 #\8 #\9 #\e #\E))
      (define (string-tack str c)
        ;; add a character to the end of str
        (string-append str (string c)))

      (let loop ((str entire-str)
                 (x "")
                 (y "")
                 (z "")
                 (state 0))
        (if (= (string-length str) 0)
            ;; indicate the string was too short
            (list #f (list 'too-short (- (string-length entire-str)
                                         (string-length str))))
            (let* ((c (string-ref str 0))
                   (rest (substring str 1 (string-length str)))
                   (chars-examined
                    (- (string-length entire-str) (string-length rest))))
              (case state
                ((0) (cond ((char-whitespace? c) (loop rest x y z state))
                           ((eqv? c #\<) (loop rest x y z 1))
                           (else
                            (list #f (list 'unexpected-character
                                           chars-examined c state)))))
                ((1 2) (cond ((char-whitespace? c) (loop rest x y z state))
                             ((memv c number-characters)
                              (if (= state 1)
                                  (loop rest (string-tack x c) y z state)
                                  (loop rest x (string-tack y c) z state)))
                             ((eqv? c #\,) (loop rest x y z (+ state 1)))
                             (else
                              (list #f (list 'unexpected-character
                                             chars-examined c state)))))
                ((3) (cond ((char-whitespace? c) (loop rest x y z state))
                           ((memv c number-characters)
                            (loop rest x y (string-tack z c) state))
                           ((eqv? c #\>) ;; done?
                            (let ((xn (string->number x))
                                  (yn (string->number y))
                                  (zn (string->number z)))
                              (cond ((and xn yn zn)
                                     (list (vector (string->number x)
                                                   (string->number y)
                                                   (string->number z))
                                           (list 'success chars-examined)))
                                    (else
                                     (list #f (list 'non-number
                                                    chars-examined x y z))))))
                           (else (list #f (list 'unexpected-character
                                                chars-examined c state))))))))))

    (define (parse-vector str)
      ;; return vector or #f if parsing fails.
      (let* ((r (parse-vector-full str))
             (v (car r))
             (reason-and-details (cadr r))
             (reason (car reason-and-details))
             (characters-examined (cadr reason-and-details))
             (details (cddr reason-and-details)))
        (cond ((eq? reason 'success) v)
              (else #f))))


    (define (radians->degrees r)
      (cond ((number? r) (* (/ r pi*2) 360.0))
            ((vector? r) (list->vector (radians->degrees (vector->list r))))
            ((list? r) (map radians->degrees r))
            (else
             (error "radians->degrees can't process: " r))))
    (define (degrees->radians d)
      (cond ((number? d) (/ (* d pi*2) 360.0))
            ((vector? d) (list->vector (degrees->radians (vector->list d))))
            ((list? d) (map degrees->radians d))
            (else
             (error "degrees->radians can't process: " d))))


    (define (vector2-sum v0 v1)
      ;; add up vectors
      (vector (+ (vector2-x v0) (vector2-x v1))
              (+ (vector2-y v0) (vector2-y v1))))


    ;; (define (vector3-sum v0 v1)
    ;;   ;; add up vectors
    ;;   (vector (+ (vector3-x v0) (vector3-x v1))
    ;;           (+ (vector3-y v0) (vector3-y v1))
    ;;           (+ (vector3-z v0) (vector3-z v1))))

    (define (vector3-sum . vs)
      ;; add up vectors
      (vector (apply + (map vector3-x vs))
              (apply + (map vector3-y vs))
              (apply + (map vector3-z vs))))



    (define (vector-list-sum lst)
      (define (vls lst running-total)
        (cond ((null? lst) running-total)
              (else (vls (cdr lst) (vector3-sum (car lst) running-total)))))
      (vls lst (vector 0.0 0.0 0.0)))


    (define (vector3-diff v0 v1)
      ;; subtract vectors
      (vector (- (vector3-x v0) (vector3-x v1))
              (- (vector3-y v0) (vector3-y v1))
              (- (vector3-z v0) (vector3-z v1))))

    (define point-diff vector3-diff)


    (define (vector2-diff v0 v1)
      ;; subtract vectors
      (vector (- (vector3-x v0) (vector3-x v1))
              (- (vector3-y v0) (vector3-y v1))))


    (define (vector3-abs v)
      (vector (abs (vector3-x v))
              (abs (vector3-y v))
              (abs (vector3-z v))))

    (define (vector2-scale v n)
      (vector (* (vector2-x v) n)
              (* (vector2-y v) n)))

    (define (vector3-scale v n)
      (vector (* (vector3-x v) n)
              (* (vector3-y v) n)
              (* (vector3-z v) n)))

    (define (vector-scale v n)
      (vector-map (lambda (entry) (* entry n)) v))

    (define (almost= a b tolerance)
      (<= (abs (- a b)) tolerance))

    (define (vector3-equal? v0 v1)
      ;; return #t if v0 and v1 are the same or almost the same
      (> 0.000001 (vector-max (vector3-abs (vector3-diff v0 v1)))))

    (define (vector2-average v0 v1)
      (vector2-scale (vector2-sum v0 v1) 0.5))

    ;; (define (vector3-average v0 v1)
    ;;   (vector3-scale (vector3-sum v0 v1) 0.5))


    (define (vector3-average . vs)
      (vector3-scale (apply vector3-sum vs) (/ 1.0 (length vs))))


    (define (vector3-normalize v)
      (let ((d (sqrt (+ (* (vector3-x v) (vector3-x v))
                        (* (vector3-y v) (vector3-y v))
                        (* (vector3-z v) (vector3-z v))))))

        (vector (/ (vector3-x v) d)
                (/ (vector3-y v) d)
                (/ (vector3-z v) d))))


    (define (quaternion-equal? q0 q1)
      ;; return #t if q0 and q1 are the same or almost the same
      (and (< (vector-max (vector (abs (- (quat-s q0) (quat-s q1)))
                                  (abs (- (quat-x q0) (quat-x q1)))
                                  (abs (- (quat-y q0) (quat-y q1)))
                                  (abs (- (quat-z q0) (quat-z q1)))))
              0.000001)))


    (define (quaternion-conjugate q)
      ;; http://www.sjbrown.co.uk/?article=quaternions
      ;; ~q = (w, -x, -y, -z)
      (quaternion (quat-s q)
                  (- (quat-x q))
                  (- (quat-y q))
                  (- (quat-z q))))


    (define (multiply-quaternions q1 q2)
      ;; [w1*w2 - v1.v2, (w1*v2 + w2*v1  +  v1 x v2)]
      ;;
      ;; (Q1 * Q2).w = (w1w2 - x1x2 - y1y2 - z1z2)
      ;; (Q1 * Q2).x = (w1x2 + x1w2 + y1z2 - z1y2)
      ;; (Q1 * Q2).y = (w1y2 - x1z2 + y1w2 + z1x2)
      ;; (Q1 * Q2).z = (w1z2 + x1y2 - y1x2 + z1w2
      ;;
      ;; q1q2 = [w1w2 - v1.v2, w1v2 + w2v1 + v1^v2]
      ;;
      ;; combine the rotations of two quaternions into one
      (let* ((w1 (quat-s q1))
             (v1 (vector (quat-x q1) (quat-y q1) (quat-z q1)))
             (w2 (quat-s q2))
             (v2 (vector (quat-x q2) (quat-y q2) (quat-z q2)))
             (s (- (* w1 w2) (dot-product v1 v2)))
             (v (vector3-sum (vector3-sum (vector3-scale v2 w1)
                                          (vector3-scale v1 w2))
                             (cross-product v1 v2))))
        (quaternion s (vector3-x v) (vector3-y v) (vector3-z v))))


    (define (vector3-rotate v q)
      ;; rotate a vector with a quaternion
      ;; [0, v'] = q.[0, v].~q
      (let ((r (multiply-quaternions
                q
                (multiply-quaternions
                 (quaternion 0.0 (vector3-x v) (vector3-y v) (vector3-z v))
                 (quaternion-conjugate q)))))
        (vector (vector-ref r 1)
                (vector-ref r 2)
                (vector-ref r 3))))


    (define (combine-rotations . rs)
      ;; apply rotations in order.
      (let loop ((r zero-rotation)
                 (rrs (reverse rs)))
        (cond ((null? rrs) r)
              ((not (car rrs)) (loop r (cdr rrs)))
              (else (loop (multiply-quaternions r (car rrs))
                          (cdr rrs))))))


    (define (dot-product v0 v1)
      ;; (+ (* (vector3-x v0) (vector3-x v1))
      ;;    (* (vector3-y v0) (vector3-y v1))
      ;;    (* (vector3-z v0) (vector3-z v1)))
      ;;
      ;; (vector-apply + (vector-map (lambda (m0 m1) (* m0 m1)) v0 v1))
      ;;
      (let loop ((result 0)
                 (index 0))
        (cond ((= index (vector-length v0)) result)
              (else
               (loop (+ result (* (vector-ref v0 index)
                                  (vector-ref v1 index)))
                     (+ index 1))))))


    (define (vector3-magnitude v0)
      (sqrt (dot-product v0 v0)))


    (define (cross-product v0 v1)
      (vector
       (- (* (vector3-y v0) (vector3-z v1)) (* (vector3-z v0) (vector3-y v1)))
       (- (* (vector3-z v0) (vector3-x v1)) (* (vector3-x v0) (vector3-z v1)))
       (- (* (vector3-x v0) (vector3-y v1)) (* (vector3-y v0) (vector3-x v1)))))



    (define (distance-between-points a b)
      (let ((dx (- (point-x a) (point-x b)))
            (dy (- (point-y a) (point-y b)))
            (dz (- (point-z a) (point-z b))))
        (sqrt (+ (* dx dx)
                 (* dy dy)
                 (* dz dz)))))


    (define (distance-from-point-to-plane P plane)
      ;; P is a vector of size 3.
      ;; plane is #(#(point) #(normal-vector))
      ;; http://softsurfer.com/Archive/algorithm_0104/algorithm_0104.htm
      (let* ((plane-point (vector-ref plane 0))
             (plane-normal (vector-ref plane 1))
             (sn (- (dot-product plane-normal (vector3-diff P plane-point))))
             (sd (dot-product plane-normal plane-normal))
             (sb (/ sn sd))
             ;; B is the point on the plane from which the plane's normal
             ;; passes through P
             (B (vector3-sum P (vector3-scale plane-normal sb)))
             (d (vector3-diff P B)))
        (vector3-magnitude d)))


    (define (line-plane-intersection S P)
      ;; S is #(#point #point)
      ;; P is a plane: #(#(point) #(normal))
      (let* (;; segment is on line: S0 + u * V where 0 <= u <= 1.0
             (S0 (vector-ref S 0))
             (S1 (vector-ref S 1))
             (V (vector3-diff S1 S0))
             (r (vector-ref P 0)) ;; point on plane
             (n (vector-ref P 1)) ;; plane normal
             (dnV (dot-product n V)))
        (if (eqv? dnV 0.0) #f
            (let ((u (/ (dot-product n (vector3-diff r S0)) dnV)))
              (vector3-sum S0 (vector3-scale V u))))))


    (define (segment-plane-intersection S P)
      ;; S is #(#point #point)
      ;; P is a plane: #(#(point) #(normal))
      (let* (;; segment is on line: S0 + u * V where 0 <= u <= 1.0
             (S0 (vector-ref S 0))
             (S1 (vector-ref S 1))
             (V (vector3-diff S1 S0))
             (r (vector-ref P 0)) ;; point on plane
             (n (vector-ref P 1)) ;; plane normal
             (dnV (dot-product n V)))
        (if (eqv? dnV 0.0) #f
            (let ((u (/ (dot-product n (vector3-diff r S0)) dnV)))
              (if (or (< u 0.0) (> u 1.0))
                  #f
                  (vector3-sum S0 (vector3-scale V u)))))))


    (define (triangle-plane-intersection T P)
      ;; T is #(P0 P1 P2), the P's are vectors of length 3
      ;; P is a plane: #(#(point) #(normal))
      ;; returns #f or a segment.
      (let* ((i0 (segment-plane-intersection
                  (vector (vector-ref T 0) (vector-ref T 1)) P))
             (i1 (segment-plane-intersection
                  (vector (vector-ref T 1) (vector-ref T 2)) P))
             (i2 (segment-plane-intersection
                  (vector (vector-ref T 2) (vector-ref T 0)) P))
             (intersections (filter (lambda (x) x) (list i0 i1 i2))))
        (if (= (length intersections) 2)
            (list->vector intersections)
            #f)))


    (define (triangle-is-degenerate? T)
      (or (vector3-equal? (vector-ref T 0) (vector-ref T 1))
          (vector3-equal? (vector-ref T 1) (vector-ref T 2))
          (vector3-equal? (vector-ref T 2) (vector-ref T 0))))


    (define (triangle-normal T)
      ;; T is #(P0 P1 P2), the P's are vectors of length 3
      ;; assumes right-hand rule.
      (vector3-normalize
       (cross-product (vector3-diff (vector-ref T 1) (vector-ref T 0))
                      (vector3-diff (vector-ref T 2) (vector-ref T 0)))))


    (define (angle-between-vectors v0 v1 . axis)
      ;; return the angle (positive or negative) that rotates
      ;; v0 to v1 around axis or (v0 X v1) if axis is not provided
      (let* ((aa (acos (dot-product (vector3-normalize v0)
                                    (vector3-normalize v1))))
             (xp (if (null? axis)
                     (cross-product (vector3-normalize v0) (vector3-normalize v1))
                     (car axis)))
             ;;
             (rot+ (rotation-quaternion xp aa))
             (rot- (rotation-quaternion xp (- aa)))
             ;;
             (v0-rot+ (vector3-rotate v0 rot+))
             (v0-rot- (vector3-rotate v0 rot-))
             ;;
             (dx+ (distance-between-points v1 v0-rot+))
             (dx- (distance-between-points v1 v0-rot-)))
        (if (< dx+ dx-) aa (- aa))))


    (define (quaternion-normalize q)
      (let ((magnitude (sqrt (dot-product q q))))
        (vector-map (lambda (entry) (/ entry magnitude)) q)))


    (define (quaternion->matrix q)
      ;; http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToMatrix/index.htm
      (let* ((q-norm (quaternion-normalize q))
             (w (quat-s q-norm))
             (x (quat-x q-norm))
             (y (quat-y q-norm))
             (z (quat-z q-norm)))
        (matrix-* (vector (vector    w  (- z)   y (- x))
                          (vector    z     w (- x)(- y))
                          (vector (- y)    x    w (- z))
                          (vector    x     y    z    w))

                  (vector (vector    w  (- z)    y  x)
                          (vector    z     w  (- x) y)
                          (vector (- y)    x     w  z)
                          (vector (- x) (- y) (- z) w)))))



    (define (quaternion->matrix-alt q)
      ;; http://web.archive.org/web/20041029003853/http:/www.j3d.org/matrix_faq/matrfaq_latest.html#Q54
      (let* ((q-norm (quaternion-normalize q))
             (w (quat-s q-norm))
             (x (quat-x q-norm))
             (y (quat-y q-norm))
             (z (quat-z q-norm))
             (xx (* x x))
             (xy (* x y))
             (xz (* x z))
             (xw (* x w))
             (yy (* y y))
             (yz (* y z))
             (yw (* y w))
             (zz (* z z))
             (zw (* z w))
             (m11 (- 1 (* 2 (+ yy zz))))
             (m12 (* 2 (- xy zw)))
             (m13 (* 2 (+ xz yw)))
             (m21 (* 2 (+ xy zw)))
             (m22 (- 1 (* 2 (+ xx zz))))
             (m23 (* 2 (- yz xw)))
             (m31 (* 2 (- xz yw)))
             (m32 (* 2 (+ yz xw)))
             (m33 (- 1 (* 2 (+ xx yy))))
             )
        (vector (vector m11 m12 m13 0)
                (vector m21 m22 m23 0)
                (vector m31 m32 m33 0)
                (vector   0   0   0 1))))


    (define (vector-numbers->strings v)
      (list->vector
       (map (lambda (x)
              (number->pretty-string x 2))
            (vector->list v))))


    (define (triangle-number v)
      ;; http://en.wikipedia.org/wiki/Triangular_number
      (apply + (range 0 (+ v 1))))



    ;; matrix is a vector of vectors.
    ;; (vector (vector 1 2 3)
    ;;         (vector 4 5 6)
    ;;         (vector 7 8 9))


    (define (matrix-new row-count col-count)
      (let ((rows (make-vector row-count 0)))
        (do ((i 0 (+ i 1)))
            ((= i row-count) rows)
          (vector-set! rows i (make-vector col-count 0)))))

    (define (matrix-width m)
      (vector-length (vector-ref m 0)))

    (define (matrix-height m)
      (vector-length m))


    (define (matrix-ref m i j)
      ;; indexing is from 0, not 1
      (vector-ref (vector-ref m i) j))


    (define (matrix-set! m i j value)
      ;; indexing is from 0, not 1
      (vector-set! (vector-ref m i) j value))


    (define (matrix-row m i)
      (vector-ref m i))


    (define (matrix-col m j)
      (vector-map (lambda (v) (vector-ref v j)) m))

    (define (vector->column-vector v)
      (vector-map (lambda (entry) (vector entry)) v))

    (define (column-vector->vector v)
      (vector-map (lambda (row) (vector-ref row 0)) v))


    (define (matrix-map f m)
      (vector-map (lambda (row) (vector-map f row)) m))


    (define (vector-minor v i)
      ;; copy vector, skip over i'th entry
      (delete-from-vector v i))


    (define (matrix-minor m i j)
      (let* ((row-count (vector-length m))
             (col-count (vector-length (vector-ref m 0)))
             (minor (make-vector (- row-count 1))))
        (let loop ((row-index 0))
          (cond ((= row-index row-count) minor)
                ((= row-index i) (loop (+ row-index 1)))
                ((< row-index i)
                 (let* ((orig-row (vector-ref m row-index))
                        (minor-row (vector-minor orig-row j)))
                   (vector-set! minor row-index minor-row)
                   (loop (+ row-index 1))))
                (else ;; (> row-index i)
                 (let* ((orig-row (vector-ref m row-index))
                        (minor-row (vector-minor orig-row j)))
                   (vector-set! minor (- row-index 1) minor-row)
                   (loop (+ row-index 1))))))))


    (define (matrix-determinant m)
      ;; determinant of a square matrix
      (let ((n (vector-length m)))
        (cond ((< n 1) (error "can't get determinant of empty matrix"))
              ((= n 1) (matrix-ref m 0 0))
              ((= n 2) (- (* (matrix-ref m 0 0) (matrix-ref m 1 1))
                          (* (matrix-ref m 1 0) (matrix-ref m 0 1))))
              (else
               (let loop ((det 0)
                          (j1 0))
                 (cond ((= j1 n) det)
                       (else
                        (loop (+ det
                                 (* (expt -1 j1)
                                    (matrix-ref m 0 j1)
                                    (matrix-determinant (matrix-minor m 0 j1))))
                              (+ j1 1)))))))))



    (define (matrix-transpose m)
      (let* ((ret-height (matrix-width m))
             (ret-width (matrix-height m))
             (ret (matrix-new ret-height ret-width)))
        (do ((i 0 (+ i 1)))
            ((= i (matrix-height m)) ret)
          (do ((j 0 (+ j 1)))
              ((= j (matrix-width m)) #t)
            (matrix-set! ret j i (matrix-ref m i j))))))


    (define (matrix-A*B m0 m1)
      (let* ((m0-height (matrix-height m0))
             (m1-width (matrix-width m1))
             (ret (matrix-new m0-height m1-width)))
        (do ((i 0 (+ i 1)))
            ((= i m0-height) ret)
          (do ((j 0 (+ j 1)))
              ((= j m1-width) #t)
            (matrix-set! ret i j (dot-product (matrix-col m1 j)
                                              (matrix-row m0 i)))))))



    (define (matrix-* . ms)
      (cond ((= (length ms) 1) (car ms))
            (else
             (matrix-A*B (car ms) (apply matrix-* (cdr ms))))))


    (define (matrix-translation v)
      (vector (vector 1 0 0 (vector3-x v))
              (vector 0 1 0 (vector3-y v))
              (vector 0 0 1 (vector3-z v))
              (vector 0 0 0 1)))


    (define (matrix-rotation-x a)
      (vector (vector 1       0           0 0)
              (vector 0 (cos a) (- (sin a)) 0)
              (vector 0 (sin a)     (cos a) 0)
              (vector 0       0           0 1)))

    (define (matrix-rotation-y a)
      (vector (vector     (cos a) 0 (sin a) 0)
              (vector           0 1       0 0)
              (vector (- (sin a)) 0 (cos a) 0)
              (vector           0 0       0 1)))

    (define (matrix-rotation-z a)
      (vector (vector (cos a) (- (sin a)) 0 0)
              (vector (sin a)     (cos a) 0 0)
              (vector       0           0 1 0)
              (vector       0           0 0 1)))

    (define (matrix-scaling v)
      (vector (vector v 0 0 0)
              (vector 0 v 0 0)
              (vector 0 0 v 0)
              (vector 0 0 0 1)))

    (define (matrix->string m)

      (let ((result ""))
        (define (out . strs)
          (if (null? strs) #t
              (begin
                (set! result (string-append result (car strs)))
                (apply out (cdr strs)))))
        (cond
         ((and (vector? m) (vector? (vector-ref m 0)))
          (let* ((m-strs (matrix-map
                          (lambda (entry) (number->pretty-string entry 2))
                          m))
                 (col-width (lambda (j)
                              (apply max
                                     (map string-length
                                          (vector->list (matrix-col m-strs j))))))
                 (total-width (+ (apply +
                                        (map col-width
                                             (range 0 (matrix-width m))))
                                 (* 2 (matrix-width m))
                                 -2)
                              ))
            (out "+-" (make-string total-width #\space) "-+\n")
            (do ((i 0 (+ i 1)))
                ((= i (matrix-height m)) #t)
              (out "|")
              (do ((j 0 (+ j 1)))
                  ((= j (matrix-width m)) #t)
                (out " " (string-pad (matrix-ref m-strs i j)
                                     (col-width j)) " "))
              (out "|\n"))
            (out "+-" (make-string total-width #\space) "-+\n")))
         ((vector? m)
          (out "[" (string-join (map (lambda (entry)
                                       (number->pretty-string entry 2))
                                     (vector->list m)) " ") "]"))
         (else
          (let ((p (open-output-string)))
            (write m p)
            (get-output-string p))))
        result))


    (define (matrix-print m port)
      (cout (matrix->string m) port))


    (define (circle-center-from-circumference-points-2d p1 p2 p3)
      ;; given 3 2D points, find circle center
      ;; http://home.att.net/~srschmitt/circle3pts.html
      (let* ((x1 (vector-ref p1 0))
             (y1 (vector-ref p1 1))
             (x2 (vector-ref p2 0))
             (y2 (vector-ref p2 1))
             (x3 (vector-ref p3 0))
             (y3 (vector-ref p3 1))
             (m (vector (vector #f #f #f 1)
                        (vector (+ (* x1 x1) (* y1 y1)) x1 y1 1)
                        (vector (+ (* x2 x2) (* y2 y2)) x2 y2 1)
                        (vector (+ (* x3 x3) (* y3 y3)) x3 y3 1)))
             (m11 (matrix-determinant (matrix-minor m 0 0)))
             (m12 (matrix-determinant (matrix-minor m 0 1)))
             (m13 (matrix-determinant (matrix-minor m 0 2))))
        (vector (/ (* 0.5 m12) m11)
                (/ (* -0.5 m13) m11))))


    (define (matrix-squash-z a b c)
      ;; return a transformation matrix that will rotate the plane
      ;; containing the 3 points such that it lines up with the x,y plane
      (let* ((ab (point-diff b a))
             (ac (point-diff c a))
             (up (vector 0. 0. 1.))
             (plane-normal (cross-product ab ac))
             (normal-x-up (cross-product plane-normal up))
             (spin (angle-between-vectors plane-normal
                                          up
                                          normal-x-up))
             (rot (rotation-quaternion normal-x-up spin)))
        (matrix-* (quaternion->matrix rot)
                  (matrix-translation (vector3-scale a -1)))))


    (define (matrix-unsquash-z a b c)
      ;; return the inverse of what matrix-squash-z returns.
      (let* ((ab (point-diff b a))
             (ac (point-diff c a))
             (up (vector 0. 0. 1.))
             (plane-normal (cross-product ab ac))
             (normal-x-up (cross-product plane-normal (vector 0 0 1)))
             (spin (angle-between-vectors plane-normal
                                          up
                                          normal-x-up))
             (rot (rotation-quaternion normal-x-up (- spin))))
        (matrix-* (matrix-translation a)
                  (quaternion->matrix rot))))


    (define (circle-center-from-circumference-points p1 p2 p3)
      ;; project the points onto a plane, then call the 2d version.
      ;; reverse the projection to get the result.
      (let* ((trans (matrix-squash-z p1 p2 p3))
             (untrans (matrix-unsquash-z p1 p2 p3))
             (p1_ (vector4->3 (matrix-* trans (vector3->4 p1))))
             (p2_ (vector4->3 (matrix-* trans (vector3->4 p2))))
             (p3_ (vector4->3 (matrix-* trans (vector3->4 p3)))))
        (let* ((c_ (circle-center-from-circumference-points-2d p1_ p2_ p3_))
               (c (matrix-* untrans (vector (vector (vector-ref c_ 0))
                                            (vector (vector-ref c_ 1))
                                            (vector 0)
                                            (vector 1)))))
          (vector4->3 c))))


    (define (sigmoid v)
      ;; gnuplot> plot 1 / (1 + exp (-x))
      (/ 1.0 (+ 1 (exp (- v)))))



    (define (mm->in v)
      (cond ((vector? v)
             (vector-map (lambda (e) (* e (/ 1.0 25.4))) v))
            (else
             (* v (/ 1.0 25.4)))))


    (define (in->mm v)
      (cond ((vector? v)
             (vector-map (lambda (e) (* e 25.4)) v))
            (else
             (* v 25.4))))


    (define (clamp-number in low high)
      (cond ((and low (< in low)) low)
            ((and high (> in high)) high)
            (else in)))

    (define (normalize-angle ang)
      (let loop ((result ang))
        (cond ((< result (- pi))
               (loop (+ result pi*2)))
              ((>= result pi)
               (loop (- result pi*2)))
              (else
               result))))


    (define (normalize-angle+ ang)
      (let loop ((result ang))
        (cond ((< result 0)
               (loop (+ result pi*2)))
              ((>= result pi*2)
               (loop (- result pi*2)))
              (else
               result))))


    (define (angle-diff ang0 ang1)
      ;; return positive or negative radians that can be added to
      ;; ang1 to reach ang0.  the return value will be between -PI and PI
      (normalize-angle (- (normalize-angle ang0) (normalize-angle ang1))))



    (define-record-type <bounding-box2>
      (bounding-box2-new x0 y0 x1 y1)
      bounding-box2-is-type?
      (x0 bounding-box2-x0 bounding-box2-set-x0!)
      (y0 bounding-box2-y0 bounding-box2-set-y0!)
      (x1 bounding-box2-x1 bounding-box2-set-x1!)
      (y1 bounding-box2-y1 bounding-box2-set-y1!))


    (define (bounding-box2-copy bb)
      (bounding-box2-new
       (bounding-box2-x0 bb)
       (bounding-box2-y0 bb)
       (bounding-box2-x1 bb)
       (bounding-box2-y1 bb)))


    (define (bounding-box2-=? bb0 bb1)
      (and (= (bounding-box2-x0 bb0) (bounding-box2-x0 bb1))
           (= (bounding-box2-y0 bb0) (bounding-box2-y0 bb1))
           (= (bounding-box2-x1 bb0) (bounding-box2-x1 bb1))
           (= (bounding-box2-y1 bb0) (bounding-box2-y1 bb1))))


    (define (bounding-box2-add-point bb p)
      (let ((x (vector2-x p))
            (y (vector2-y p))
            (result #f))
        (cond ((eq? (bounding-box2-x0 bb) #f)
               (bounding-box2-set-x0! bb x)
               (bounding-box2-set-y0! bb y)
               (bounding-box2-set-x1! bb x)
               (bounding-box2-set-y1! bb y)
               #t)
              (else
               (cond ((< x (bounding-box2-x0 bb))
                      (bounding-box2-set-x0! bb x)
                      (set! result #t)))
               (cond ((< y (bounding-box2-y0 bb))
                      (bounding-box2-set-y0! bb y)
                      (set! result #t)))
               (cond ((> x (bounding-box2-x1 bb))
                      (bounding-box2-set-x1! bb x)
                      (set! result #t)))
               (cond ((> y (bounding-box2-y1 bb))
                      (bounding-box2-set-y1! bb y)
                      (set! result #t)))
               result))))

    ))
