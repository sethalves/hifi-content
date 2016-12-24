(define-library (rocks-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme process-context)
          (srfi 27)
          (foldling command-line)
          (seth cout)
          (seth model-3d)
          (seth math-3d)
          (seth obj-model))
  (begin
    (define (main-program)
      (define (usage why)
        (cerr why "\n")
        (cerr "rocks [arguments] obj-output-file\n")
        (cerr "  --random-i              pseudo-random number generator seed\n")
        (cerr "  --random-j              pseudo-random number generator seed\n")
        (cerr "  --size                  maximum dimension of output\n")
        (exit 1))

      (let* ((args (parse-command-line `((-?) (-h)
                                         (--random-i random-i)
                                         (--random-j random-j)
                                         (--size size))))
             (output-filename #f)
             (point-count #f)
             (random-i #f)
             (random-j #f)
             (size #f)
             (extra-arguments '()))
        (for-each
         (lambda (arg)
           (case (car arg)
             ((-? -h) (usage ""))
             ((--random-i)
              (set! random-i (string->number (cadr arg))))
             ((--random-j)
              (set! random-j (string->number (cadr arg))))
             ((--size)
              (set! size (string->number (cadr arg))))             ((--)
              (set! extra-arguments (cdr arg)))))
         args)

        (if (not random-i) (set! random-i 0))
        (if (not random-j) (set! random-j 0))
        (if (not size) (set! size 10))

        (if (not (= (length extra-arguments) 2))
            (usage "give obj output-filename and point-count"))
        (set! output-filename (car extra-arguments))
        (set! point-count (exact (string->number (cadr extra-arguments))))

        (let* ((model (make-empty-model))
               (mesh (make-mesh #f '()))
               (random-source (make-random-source))
               (random-integer (random-source-make-integers random-source))
               (reso 5.0)
               (rand-max (exact (* size reso)))
               (half-rand-max (/ rand-max 2.0))
               (output-handle
                (if (equal? output-filename "-")
                    (current-output-port)
                    (open-output-file output-filename))))
          (random-source-pseudo-randomize! random-source random-i random-j)
          (model-prepend-mesh! model mesh)
          (let loop ((i 0))
            (cond ((= i point-count)

                   (do ((a 0 (+ a 1)))
                       ((= a i) a)
                     (do ((b 0 (+ b 1)))
                         ((= b i) b)
                       (do ((c 0 (+ c 1)))
                           ((= c i) c)
                         (cond ((= a b) #t)
                               ((= b c) #t)
                               ((= a c) #t)
                               (else
                                (mesh-append-face! model mesh
                                                   (make-face model
                                                              (vector (make-face-corner a 'unset 'unset)
                                                                      (make-face-corner b 'unset 'unset)
                                                                      (make-face-corner c 'unset 'unset))
                                                              #f)))))))

                   ;; (let face-loop ((a 2))
                   ;;   (cond ((= a point-count) #t)
                   ;;         (else
                   ;;          (mesh-append-face! model mesh
                   ;;                             (make-face model
                   ;;                                        (vector (make-face-corner a 'unset 'unset)
                   ;;                                                (make-face-corner (- a 1) 'unset 'unset)
                   ;;                                                (make-face-corner (- a 2) 'unset 'unset))
                   ;;                                        #f))
                   ;;          (face-loop (+ a 1)))))

                   #t
                   )
                  (else
                   (let* ((point-3d (vector (inexact (- (random-integer rand-max) half-rand-max))
                                            (inexact (- (random-integer rand-max) half-rand-max))
                                            (inexact (- (random-integer rand-max) half-rand-max))))
                          (point-3d-str (vector-map number->string (vector3-scale point-3d (/ 1.0 reso)))))
                     (model-append-vertex! model point-3d-str)
                     ;; (if (> i 1)
                     ;;     (mesh-append-face! model mesh
                     ;;                        (make-face model
                     ;;                                   (vector (make-face-corner 0 'unset 'unset)
                     ;;                                           (make-face-corner 1 'unset 'unset)
                     ;;                                           (make-face-corner i 'unset 'unset))
                     ;;                                   #f)))
                     (loop (+ i 1))))))



          (write-obj-model model output-handle)
          (if (not (equal? output-filename "-"))
              (close-output-port output-handle)))))))
