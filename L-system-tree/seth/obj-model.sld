(define-library (seth obj-model)
  (export read-obj-model
          read-obj-model-file
          write-obj-model)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (scheme cxr)
          (scheme process-context)
          (srfi 13)
          (srfi 29)
          (srfi 69)
          (snow assert)
          (snow input-parse)
          (seth cout)
          (seth strings)
          (seth math-3d)
          (seth model-3d))
  (cond-expand
   (chicken (import (extras)))
   (else))

  (begin

    (define (parse-index index-string)
      ;; return a zero-based index value
      (snow-assert (string? index-string))
      (cond ((equal? index-string "") 'unset)
            (else
             (let ((result (- (string->number index-string) 1)))
               (snow-assert (>= result 0))
               result))))


    (define (unparse-index index)
      (snow-assert (or (number? index) (eq? index 'unset)))
      (snow-assert (or (eq? index 'unset) (>= index 0)))
      ;; return a one-based index string
      (cond ((eq? index 'unset) "")
            (else
             (number->string (+ index 1)))))


    (define (parse-face-corner face-corner-string)
      (snow-assert (string? face-corner-string))
      (let* ((parts (string-split face-corner-string #\/))
             (parts-length (length parts)))
        (cond ((= parts-length 1)
               (make-face-corner (parse-index (car parts)) 'unset 'unset))
              ((= parts-length 2)
               (make-face-corner (parse-index (car parts))
                                 (parse-index (cadr parts)) 'unset))
              ((= parts-length 3)
               (make-face-corner (parse-index (car parts))
                                 (parse-index (cadr parts))
                                 (parse-index (caddr parts))))
              (else
               (error "bad face-vertex string" face-corner-string)))))


    (define (unparse-face-corner face-corner)
      (snow-assert (face-corner? face-corner))
      (if (and (eq? (face-corner-texture-index face-corner) 'unset)
               (eq? (face-corner-normal-index face-corner) 'unset))
          (unparse-index (face-corner-vertex-index face-corner))
          (format "~a/~a/~a"
                  (unparse-index (face-corner-vertex-index face-corner))
                  (unparse-index (face-corner-texture-index face-corner))
                  (unparse-index (face-corner-normal-index face-corner)))))



    (define (read-face nt)
      (let face-loop ((face-vertex-indices '()))
        (let ((face-vertex-index (nt)))
          (cond
           ((equal? face-vertex-index "")
            (reverse face-vertex-indices))
           (else
            (face-loop
             (cons face-vertex-index
                   face-vertex-indices)))))))


    (define (read-mesh model mesh-name material vertex-index-start texture-index-start normal-index-start inport)
      (snow-assert (model? model))
      (snow-assert (input-port? inport))
      (snow-assert (or (material? material) (not material)))
      (let ((mesh (make-mesh #f '())))
        (define (done result next-mesh-name next-material)
          (snow-assert (or (material? material) (not material)))
          (cond ((not (null? (mesh-faces mesh)))
                 (if mesh-name (mesh-set-name! mesh mesh-name))
                 (mesh-set-faces! mesh (reverse (mesh-faces mesh)))
                 (model-prepend-mesh! model mesh)))
          (values result next-mesh-name next-material))
        (let loop ((material material))
          (let ((line (read-line inport)))
            (cond ((eof-object? line) (done #f #f material))
                  (else
                   ;; consume the next line
                   (let* ((line-trimmmed (string-trim-both line)))
                     (cond ((= (string-length line-trimmmed) 0) (loop material))
                           ((eqv? (string-ref line-trimmmed 0) #\#) (loop material))
                           (else
                            (let* ((line-hndl
                                    (open-input-string line-trimmmed))
                                   (nt (lambda ()
                                         (next-token '(#\space)
                                                     '(#\space *eof*)
                                                     #\# line-hndl)))
                                   (first-token (nt)))
                              (cond ((equal? first-token "v")
                                     (let* ((x (nt)) (y (nt)) (z (nt)))
                                       (model-prepend-vertex! model x y z)
                                       (loop material)))
                                    ((equal? first-token "vt")
                                     (let* ((x (string->number (nt)))
                                            (y (string->number (nt))))
                                       (model-append-texture-coordinate!
                                        model (vector x y))
                                       (loop material)))
                                    ((equal? first-token "vn")
                                     (let* ((x (nt)) (y (nt)) (z (nt)))
                                       (model-prepend-normal! model x y z)
                                       (loop material)))
                                    ((equal? first-token "f")
                                     (mesh-prepend-face!
                                      model mesh
                                      (map parse-face-corner (read-face nt))
                                      material
                                      vertex-index-start texture-index-start
                                      normal-index-start inport)
                                     (loop material))
                                    ((equal? first-token "g")
                                     (let ((next-mesh-name (nt)))
                                       (done #t
                                             (if (eof-object? next-mesh-name)
                                                 #f
                                                 next-mesh-name)
                                             material)))
                                    ((equal? first-token "mtllib")
                                     (let ((material-library (nt)))
                                       (model-set-material-libraries!
                                        model
                                        (cons material-library (model-material-libraries model)))
                                       (loop material)))
                                    ((equal? first-token "usemtl")
                                     (let* ((next-material-name (nt))
                                            (next-material (model-get-material-by-name model next-material-name)))
                                       (loop next-material)))
                                    (else
                                     (loop material)))))))))))))


    (define (read-obj-model inport . maybe-model)
      (snow-assert (input-port? inport))
      (let* ((model (if (null? maybe-model)
                        (make-model '() '() '() '() '() (make-hash-table))
                        (car maybe-model)))
             (vertex-index-start (length (model-vertices model)))
             (texture-index-start (length (model-texture-coordinates model)))
             (normal-index-start (length (model-normals model))))
        (snow-assert (model? model))

        (model-set-meshes! model (reverse (model-meshes model)))
        (model-set-vertices! model (reverse (model-vertices model)))
        (model-set-normals! model (reverse (model-normals model)))

        (let loop ((mesh-name #f)
                   (material #f))
          (let-values (((continue next-model-name next-material)
                        (read-mesh model mesh-name
                                   material
                                   vertex-index-start
                                   texture-index-start
                                   normal-index-start
                                   inport)))
            (cond (continue (loop next-model-name next-material))
                  (else
                   (model-set-meshes! model (reverse (model-meshes model)))
                   (model-set-vertices! model (reverse (model-vertices model)))
                   (model-set-normals! model (reverse (model-normals model)))
                   model))))))


    (define (read-obj-model-file input-file-name . maybe-model)
      (snow-assert (string? input-file-name))
      (let ((model (if (null? maybe-model)
                       (make-model '() '() '() '() '() (make-hash-table))
                       (car maybe-model))))
        (snow-assert (model? model))
        (read-obj-model (open-input-file input-file-name) model)))


    (define (write-obj-model model port)
      (snow-assert (model? model))
      (snow-assert (output-port? port))

      (for-each
       (lambda (material-library)
         (display (format "mtllib ~a\n" material-library) port))
       (model-material-libraries model))
      (newline port)

      (for-each
       (lambda (vertex)
         (display (format "v ~a ~a ~a\n"
                          (vector3-x vertex)
                          (vector3-y vertex)
                          (vector3-z vertex)) port))
       (model-vertices model))
      (newline port)

      (for-each
       (lambda (coord)
         (display (format "vt ~a ~a\n"
                          (number->pretty-string (vector2-x coord) 4)
                          (number->pretty-string (vector2-y coord) 4)) port))
       (model-texture-coordinates model))
      (newline port)

      (for-each
       (lambda (normal)
         (display (format "vn ~a ~a ~a\n"
                          (vector3-x normal)
                          (vector3-y normal)
                          (vector3-z normal)) port))
       (model-normals model))

      (let loop ((meshes (model-meshes model))
                 (nth 1)
                 (material #f))
        (cond ((null? meshes) #t)
              (else 
               (let ((mesh (car meshes))
                     (current-material material))
                 (if (mesh-name mesh)
                     (display (format "\ng ~a\n" (mesh-name mesh)) port)
                     (display (format "\ng mesh-~a\n" nth) port))
                 (for-each
                  (lambda (face)
                    (snow-assert (face? face))

                    (let ((next-material (face-material face)))
                      (cond ((not (eq? next-material current-material))
                             (display (format "usemtl ~a\n" (material-name next-material)) port)
                             (set! current-material next-material)))
                      (display (format "f ~a"
                                       (string-join
                                        (vector->list
                                         (vector-map unparse-face-corner (face-corners face)))))
                               port)
                      (newline port)))
                  (mesh-faces mesh))
                 (loop (cdr meshes) (+ nth 1) current-material))))))

    ))
