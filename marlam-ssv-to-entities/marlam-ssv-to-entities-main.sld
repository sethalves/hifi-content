(define-library (marlam-ssv-to-entities-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme inexact)
          (scheme process-context)
          (srfi 69)
          (srfi 13)
          (srfi 14)
          (srfi 27)
          (foldling command-line)
          (seth cout)
          (seth model-3d)
          (seth math-3d)
          (seth scad-model)
          (seth obj-model))
  (begin

    (define (get-registration-M model)
      (let* ((aa-box (model-aa-box model))
             (center (aa-box-center aa-box))
             (dimensions (model-dimensions model)))
        (matrix-translation (vector3-scale center -1.0))))

    (define (get-registration-point model)
      (let* ((aa-box (model-aa-box model))
             (center (aa-box-center aa-box))
             (dimensions (model-dimensions model)))
        (vector
         (+ (/ (- (vector3-x center)) (vector3-x dimensions)) 0.5)
         (+ (/ (- (vector3-y center)) (vector3-y dimensions)) 0.5)
         (+ (/ (- (vector3-z center)) (vector3-z dimensions)) 0.5))))


    (define (json-vec3 v)
      (string-append
       "{ x: " (number->pretty-string (vector3-x v))
       ", y: " (number->pretty-string (vector3-y v))
       ", z: " (number->pretty-string (vector3-z v))
       " }"))

    (define (json-quat q)
      (string-append
       "{ w: " (number->pretty-string (quat-w q))
       ", x: " (number->pretty-string (quat-x q))
       ", y: " (number->pretty-string (quat-y q))
       ", z: " (number->pretty-string (quat-z q))
       " }"))


    (define caching-read-obj-model-file #f)


    (define (main-program)
      (define (usage why)
        (cerr why "\n")
        (cerr "marlam-ssv-to-entities [arguments] ssv-input-file\n")
        (cerr "  --scad                output scad rather than js\n")
        (cerr "  --set <doodad-set>    limit output to named doodad-set\n")
        (cerr "  --one-each            output each model once\n")
        (exit 1))

      (let ((cached-filename #f)
            (cached-model #f))
        (set! caching-read-obj-model-file (lambda (filename)
                                            (cond ((not (equal? filename cached-filename))
                                                   (set! cached-filename filename)
                                                   (set! cached-model (read-obj-model-file filename))))
                                            cached-model)))

      (let* ((args (parse-command-line `((-?) (-h)
                                         (--scad)
                                         (--one-each)
                                         (--set doodad-set)
                                         )))
             (input-filename #f)
             (doodad-set #f)
             (output-scad #f)
             (one-each #f)
             (extra-arguments '()))
        (for-each
         (lambda (arg)
           (case (car arg)
             ((-? -h) (usage ""))
             ((--scad)
              (if output-scad (usage "give --output-scad only once"))
              (set! output-scad #t))
             ((--one-each)
              (if output-scad (usage "give --one-each only once"))
              (set! one-each #t))
             ((--set)
              (if doodad-set (usage "give --set only once"))
              (set! doodad-set (cadr arg)))
             ((--)
              (set! extra-arguments (cdr arg)))))
         args)

        (if (not (= (length extra-arguments) 1))
            (usage "give semicolon-separated-value input-filename"))
        (set! input-filename (car extra-arguments))

        (let* ((local-top "/home/seth/Desktop/wow-exports/wow-exports/world/wmo/azeroth/buildings/hearthglen/")
               (http-top "http://headache.hungry.com/~seth/hifi/wow/wow-exports/world/wmo/azeroth/buildings/hearthglen/")
               (hifi-scale-adjustment 0.75)
               ;; (base-position (vector 69.28839874267578 23.184999465942383 -6.469799995422363))
               (base-position (vector 76.198974609375 22.947805404663086 -10.013736724853516))
               (base-rotation (vector 0.6427862644195557 0 0.7660181522369385 0))
               (base-rotation-M (matrix-rotation-quaternion (vector 1.0 0.0 0.0 0.0)))
               (base-model (read-obj-model-file (string-append local-top "hearthglen_townhall_nowall.obj")))
               (base-reg-M (get-registration-M base-model))
               (base-trans-M (matrix-translation base-position))
               (base-scale-M (matrix-scaling hifi-scale-adjustment))
               ;; (base-M (matrix-* base-trans-M base-rotation-M base-scale-M )) ;; base-reg-M))
               (base-M base-trans-M)
               (one-each-hash (make-hash-table))

               (in-hndl (open-input-file input-filename)))
          (read-line in-hndl) ;; dump the first line

          (cerr "base-position: " base-position "\n")
          (cerr "base-reg: " (get-registration-point base-model) "\n")
          ;; (matrix-print base-reg-M (current-output-port))

          (let loop ((line (read-line in-hndl)))
            (cond ((eof-object? line) #t)
                  (else
                   (let* ((line-split (string-tokenize line (char-set-complement (string->char-set ";"))))
                          (line-parts (list->vector line-split))
                          (model-filename (vector-ref line-parts 0))
                          (model-basename (substring model-filename 0 (- (string-length model-filename) 4)))
                          (model-hullname (string-append model-basename "-hull.obj"))
                          (px (string->number (vector-ref line-parts 1)))
                          (py (string->number (vector-ref line-parts 2)))
                          (pz (string->number (vector-ref line-parts 3)))
                          (position (vector px pz py))
                          (rw (string->number (vector-ref line-parts 4)))
                          (rx (string->number (vector-ref line-parts 5)))
                          (ry (string->number (vector-ref line-parts 6)))
                          (rz (string->number (vector-ref line-parts 7)))
                          (rotation
                           (combine-rotations
                            (rotation-quaternion (vector 1 0 0) 0)
                            (rotation-quaternion (vector 0 1 0) 0)
                            (rotation-quaternion (vector 0 0 1) (- pi/2))
                            (vector rw rx ry rz)
                            ))
                          (scale (* (string->number (vector-ref line-parts 8)) hifi-scale-adjustment))
                          (this-lines-doodad-set (vector-ref line-parts 9))

                          (doodad-model (caching-read-obj-model-file (string-append local-top model-filename)))
                          (dimensions (vector3-scale (model-dimensions doodad-model) scale))
                          (reg (get-registration-point doodad-model))

                          ;; (rot-M (quaternion->matrix (vector rw rx ry rz)))
                          ;; (scale-M (matrix-scaling scale))

                          ;; (M (matrix-* base-trans-M rot-M scale-M))
                          ;; (ok (matrix-print M (current-error-port)))
                          ;; (ok1 (matrix-print (vector3->4 (vector px py pz)) (current-error-port)))
                          ;; (final-pos (vector4->3 (matrix-* (vector3->4 (vector px py pz)) M)))


                          )

                     (cond ((and (or (not one-each) (not (hash-table-exists? one-each-hash model-filename)))
                                 (or (not doodad-set) (equal? this-lines-doodad-set doodad-set)))

                            (hash-table-set! one-each-hash model-filename #t)

                            ;; (cerr position " --> " (matrix-transform-position3 base-M position) "\n")
                            ;; (cerr (vector-ref (matrix-transpose (matrix-A*B base-M (vector3->4 position))) 0) "\n")
                            ;; (matrix-print (matrix-transpose (matrix-A*B base-M (vector3->4 position))) (current-error-port))
                            ;; (cerr this-lines-doodad-set "\n")

                            (cout "Entities.addEntity({\n")
                            (cout "    type: \"Model\",\n")
                            (cout "    modelURL: \"" (string-append http-top model-filename) "\",\n")
                            (cout "    position: " (json-vec3
                                                    (matrix-transform-position3 base-M position)
                                                    ;; (vector3-sum base-position position)
                                                    ) ",\n")
                            (cout "    rotation: " (json-quat rotation) ",\n")
                            (cout "    name: \"" "wow-import" "\",\n")
                            (cout "    collisionsWillMove: true,\n")
                            (cout "    dynamic: true,\n")
                            (cout "    velocity: " (json-vec3 (vector 0 -1 0)) ",\n")
                            ;; (cout "    lifetime: 60,\n")
                            (cout "    collisionless: false,\n")
                            (cout "    gravity: " (json-vec3 (vector 0 -1 0)) ",\n")

                            ;; (cout "    shapeType: \"box\",\n")
                            (cout "    shapeType: \"compound\",\n")
                            (cout "    compoundShapeURL: \"" (string-append http-top model-hullname "\",\n"))

                            (cout "    registrationPoint: " (json-vec3 reg) ",\n")
                            (cout "    dimensions: " (json-vec3 dimensions) ",\n")
                            (cout "});\n"))))

                   (loop (read-line in-hndl)))))
          (close-input-port in-hndl))))))