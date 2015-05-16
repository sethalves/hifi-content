(define-library (seth obj-model)
  (export make-model
          make-empty-model
          read-obj-model
          read-obj-model-file
          compact-obj-model
          fix-face-winding
          add-simple-texture-coordinates
          model-aa-box
          model-dimensions
          model-max-dimension
          model-texture-aa-box
          scale-model
          size-model
          translate-model
          write-obj-model

          aa-box-low-corner aa-box-set-low-corner!
          aa-box-high-corner aa-box-set-high-corner!
          )
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
          (seth math-3d))
  (cond-expand
   (chicken (import (extras)))
   (else))

  (begin

    ;; a model has a list of vertices and of normals. A model also has a list of meshes.
    (define-record-type <model>
      (make-model meshes vertices texture-coordinates normals)
      model?
      (meshes model-meshes model-set-meshes!)
      (vertices model-vertices model-set-vertices!)
      (texture-coordinates model-texture-coordinates model-set-texture-coordinates!)
      (normals model-normals model-set-normals!))

    (define (make-empty-model)
      (make-model '() '() '() '()))

    ;; a mesh is a group of triangles defined by indexing into the model's vertexes
    (define-record-type <mesh>
      (make-mesh name faces)
      mesh?
      (name mesh-name mesh-set-name!)
      (faces mesh-faces mesh-set-faces!))

    ;; a face corner is a vertex being used as part of the definition for a face
    (define-record-type <face-corner>
      ;; indexes here are zero based
      (make-face-corner~ vertex-index texture-index normal-index)
      face-corner?
      (vertex-index face-corner-vertex-index face-corner-set-vertex-index!)
      (texture-index face-corner-texture-index face-corner-set-texture-index!)
      (normal-index face-corner-normal-index face-corner-set-normal-index!))


    (define (make-face-corner vertex-index texture-index normal-index)
      (snow-assert (integer? vertex-index))
      (snow-assert (or (integer? texture-index) (eq? texture-index 'unset)))
      (snow-assert (or (integer? normal-index) (eq? normal-index 'unset)))
      (make-face-corner~ vertex-index texture-index normal-index))


    (define-record-type <aa-box>
      (make-aa-box~ low-corner high-corner)
      aa-box?
      (low-corner aa-box-low-corner aa-box-set-low-corner!)
      (high-corner aa-box-high-corner aa-box-set-high-corner!))


    (define (make-aa-box initial-low initial-high)
      (make-aa-box~
       (if (> (vector-length initial-low) 2)
           initial-low
           (vector (vector2-x initial-low)
                   (vector2-y initial-low)
                   0))
       (if (> (vector-length initial-high) 2)
           initial-high
           (vector (vector2-x initial-high)
                   (vector2-y initial-high)
                   0))))


    (define (model-clear-texture-coordinates! model)
      (model-set-texture-coordinates! model '()))

    (define (model-append-texture-coordinate! model v)
      (snow-assert (model? model))
      (snow-assert (vector? v))
      (snow-assert (= (vector-length v) 2))
      (model-set-texture-coordinates!
       model (reverse (cons v (reverse (model-texture-coordinates model))))))


    (define (aa-box-add-point! aa-box p)
      (let ((prev-low (aa-box-low-corner aa-box))
            (prev-high (aa-box-high-corner aa-box)))
        (aa-box-set-low-corner!
         aa-box
         (vector (min (vector-ref prev-low 0) (vector-ref p 0))
                 (min (vector-ref prev-low 1) (vector-ref p 1))
                 (if (> (vector-length p) 2)
                     (min (vector-ref prev-low 2) (vector-ref p 2))
                     (vector-ref prev-low 2))))
        (aa-box-set-high-corner!
         aa-box
         (vector (max (vector-ref prev-high 0) (vector-ref p 0))
                 (max (vector-ref prev-high 1) (vector-ref p 1))
                 (if (> (vector-length p) 2)
                     (max (vector-ref prev-high 2) (vector-ref p 2))
                     (vector-ref prev-high 2))))))


    (define (face-corner->vertex model face-corner)
      (snow-assert (model? model))
      (snow-assert (face-corner? face-corner))
      (let ((index (face-corner-vertex-index face-corner)))
        (if (eq? index 'unset) #f
            (vector-map string->number (list-ref (model-vertices model) index)))))


    (define (face-corner->normal model face-corner)
      (snow-assert (model? model))
      (snow-assert (face-corner? face-corner))
      (let ((index (face-corner-normal-index face-corner)))
        (if (eq? index 'unset) #f
            (vector-map string->number (list-ref (model-normals model) index)))))
      

    ;; a face is a vector of face-corners
    (define (face? face)
      (cond ((not (vector? face)) #f)
            (else
             (let loop ((face (vector->list face)))
               (cond ((null? face) #t)
                     ((face-corner? (car face)) (loop (cdr face)))
                     (else #f))))))


    (define (for-each-mesh model proc)
      (for-each proc (model-meshes model)))


    ;; call op with and replace every face in model.  op should accept mesh
    ;; and face and return face
    (define (operate-on-faces model op)
      (for-each-mesh
       model
       (lambda (mesh)
         (mesh-set-faces!
          mesh
          (map
           (lambda (face) (op mesh face))
           (mesh-faces mesh))))))


    ;; call op with and replace every face corner.  op should accept mesh and
    ;; face and face-corner and return face-corner.
    (define (operate-on-face-corners model op)
      (operate-on-faces
       model
       (lambda (mesh face)
         (vector-map
          (lambda (face-corner)
            (op mesh face face-corner))
          face))))



    ;; a face includes a vector of indexes into the models vertices.  turn
    ;; a face into a vector of vertices.
    (define (face->vertices model face)
      (vector-map
       (lambda (face-corner)
         (face-corner->vertex model face-corner))
       face))


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


    (define (model-prepend-mesh! model mesh)
      (snow-assert (model? model))
      (snow-assert (mesh? mesh))
      (model-set-meshes! model (cons mesh (model-meshes model))))


    (define (string-split str tester)
      (snow-assert (string? str))
      (snow-assert (procedure? tester))
      (let loop ((chars (string->list str))
                 (result '())
                 (results '()))
        (cond ((null? chars)
               (reverse (cons (list->string (reverse result)) results)))
              ((tester (car chars))
               (loop (cdr chars)
                     '()
                     (cons (list->string (reverse result)) results)))
              (else
               (loop (cdr chars)
                     (cons (car chars) result)
                     results)))))


    (define (parse-face-corner face-corner-string)
      (snow-assert (string? face-corner-string))
      (let* ((parts
              (string-split face-corner-string (lambda (c) (eqv? c #\/))))
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


    (define (shift-face-indices face vertex-index-start texture-index-start normal-index-start)
      (snow-assert (face? face))
      (snow-assert (number? vertex-index-start))
      (snow-assert (number? texture-index-start))
      (snow-assert (number? normal-index-start))
      (vector-map
       (lambda (corner)
         (face-corner-set-vertex-index!
          corner (+ (face-corner-vertex-index corner) vertex-index-start))
         (if (not (eq? (face-corner-texture-index corner) 'unset))
             (face-corner-set-texture-index!
              corner (+ (face-corner-texture-index corner) texture-index-start)))
         (if (not (eq? (face-corner-normal-index corner) 'unset))
             (face-corner-set-normal-index!
              corner (+ (face-corner-normal-index corner) normal-index-start)))
         )
       face))


    (define (model-prepend-vertex! model x y z)
      (snow-assert (model? model))
      (snow-assert (string? x))
      (snow-assert (string? y))
      (snow-assert (string? z))
      (model-set-vertices! model (cons (vector x y z) (model-vertices model))))


    (define (model-prepend-normal! model x y z)
      (snow-assert (model? model))
      (snow-assert (string? x))
      (snow-assert (string? y))
      (snow-assert (string? z))
      (model-set-normals! model (cons (vector x y z) (model-normals model))))


    (define (mesh-prepend-face! mesh face-corner-strings
                               vertex-index-start texture-index-start
                               normal-index-start inport)
      (snow-assert (mesh? mesh))
      (snow-assert (list? face-corner-strings))
      (let* ((parsed-corners (map parse-face-corner face-corner-strings))
             (face (list->vector parsed-corners)))
        (shift-face-indices face vertex-index-start texture-index-start normal-index-start)
        (mesh-set-faces! mesh (cons face (mesh-faces mesh)))))


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


    (define (read-mesh model mesh-name vertex-index-start texture-index-start normal-index-start inport)
      (snow-assert (model? model))
      (snow-assert (input-port? inport))
      (let ((mesh (make-mesh #f '())))
        (define (done result next-mesh-name)
          (cond ((not (null? (mesh-faces mesh)))
                 (if mesh-name (mesh-set-name! mesh mesh-name))
                 (mesh-set-faces! mesh (reverse (mesh-faces mesh)))
                 (model-prepend-mesh! model mesh)))
          (values result next-mesh-name))
        (let loop ()
          (let ((line (read-line inport)))
            (cond ((eof-object? line) (done #f #f))
                  (else
                   ;; consume the next line
                   (let* ((line-trimmmed (string-trim-both line)))
                     (cond ((= (string-length line-trimmmed) 0) (loop))
                           ((eqv? (string-ref line-trimmmed 0) #\#) (loop))
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
                                       (loop)))
                                    ((equal? first-token "vt")
                                     (let* ((x (string->number (nt)))
                                            (y (string->number (nt))))
                                       (model-append-texture-coordinate!
                                        model (vector x y))
                                       (loop)))
                                    ((equal? first-token "vn")
                                     (let* ((x (nt)) (y (nt)) (z (nt)))
                                       (model-prepend-normal! model x y z)
                                       (loop)))
                                    ((equal? first-token "f")
                                     (mesh-prepend-face!
                                      mesh (read-face nt) vertex-index-start
                                      texture-index-start normal-index-start inport)
                                     (loop))
                                    ((equal? first-token "g")
                                     (let ((next-mesh-name (nt)))
                                       (done #t
                                             (if (eof-object? next-mesh-name)
                                                 #f
                                                 next-mesh-name))))
                                    (else
                                     (loop)))))))))))))


    (define (read-obj-model inport . maybe-model)
      (snow-assert (input-port? inport))
      (let* ((model (if (null? maybe-model)
                        (make-model '() '() '() '())
                        (car maybe-model)))
             (vertex-index-start (length (model-vertices model)))
             (texture-index-start (length (model-texture-coordinates model)))
             (normal-index-start (length (model-normals model))))
        (snow-assert (model? model))

        (model-set-meshes! model (reverse (model-meshes model)))
        (model-set-vertices! model (reverse (model-vertices model)))
        (model-set-normals! model (reverse (model-normals model)))

        (let loop ((mesh-name #f))
          (let-values (((continue next-model-name)
                        (read-mesh model mesh-name
                                   vertex-index-start
                                   texture-index-start
                                   normal-index-start
                                   inport)))
            (cond (continue (loop next-model-name))
                  (else
                   (model-set-meshes! model (reverse (model-meshes model)))
                   (model-set-vertices! model (reverse (model-vertices model)))
                   (model-set-normals! model (reverse (model-normals model)))
                   model))))))


    (define (read-obj-model-file input-file-name . maybe-model)
      (snow-assert (string? input-file-name))
      (let ((model (if (null? maybe-model) #f (car maybe-model))))
        (snow-assert (model? model))
        (read-obj-model (open-input-file input-file-name) model)))


    (define (write-obj-model model port)
      (snow-assert (model? model))
      (snow-assert (output-port? port))
      (for-each
       (lambda (vertex)
         (display (format "v ~a ~a ~a"
                          (vector3-x vertex)
                          (vector3-y vertex)
                          (vector3-z vertex)) port)
         (newline port))
       (model-vertices model))
      (newline port)

      (for-each
       (lambda (coord)
         (display (format "vt ~a ~a"
                          (exact (round (vector2-x coord)))
                          (exact (round (vector2-y coord)))) port)
         (newline port))
       (model-texture-coordinates model))
      (newline port)

      (for-each
       (lambda (normal)
         (display (format "vn ~a ~a ~a"
                          (vector3-x normal)
                          (vector3-y normal)
                          (vector3-z normal)) port)
         (newline port))
       (model-normals model))

      (let loop ((meshes (model-meshes model))
                 (nth 1))
        (cond ((null? meshes) #t)
              (else 
               (let ((mesh (car meshes)))
                 (if (mesh-name mesh)
                     (display (format "\ng ~a\n" (mesh-name mesh)) port)
                     (display (format "\ng mesh-~a\n" nth) port))
                 (for-each
                  (lambda (face)
                    (display (format "f ~a"
                                     (string-join
                                      (vector->list
                                       (vector-map unparse-face-corner face))))
                             port)
                    (newline port))
                  (mesh-faces mesh))
                 (loop (cdr meshes) (+ nth 1)))))))



    (define (compact-obj-model model)
      ;; fix a model that has repeated points

      (define (remap-index index orig-vector new-ht)
        (snow-assert (or (eq? index 'unset) (number? index)))
        (snow-assert (vector? orig-vector))
        (snow-assert (hash-table? new-ht))
        (cond ((equal? index 'unset) 'unset)
              (else
               (let* ((old-value (vector-ref orig-vector index))
                      (old-key (string-join (vector->list old-value))))
                 (hash-table-ref new-ht old-key)))))

      (snow-assert (model? model))

      (let ((original-vertices (list->vector (model-vertices model)))
            (new-vertices '())
            (vertex-ht (make-hash-table))
            (vertex-n 0)
            (original-normals (list->vector (model-normals model)))
            (new-normals '())
            (normal-ht (make-hash-table))
            (normal-n 0))

        (for-each
         (lambda (vertex)
           (let ((key (string-join (vector->list vertex))))
             (cond ((not (hash-table-exists? vertex-ht key))
                    (hash-table-set! vertex-ht key vertex-n)
                    (set! vertex-n (+ vertex-n 1))
                    (set! new-vertices (cons vertex new-vertices))))))
         (model-vertices model))

        (for-each
         (lambda (normal)
           (let ((key (string-join (vector->list normal))))
             (cond ((not (hash-table-exists? normal-ht key))
                    (hash-table-set! normal-ht key normal-n)
                    (set! normal-n (+ normal-n 1))
                    (set! new-normals (cons normal new-normals))))))
         (model-normals model))

        (model-set-vertices! model (reverse new-vertices))
        (model-set-normals! model (reverse new-normals))

        (operate-on-face-corners
         model
         (lambda (mesh face face-corner)
           (make-face-corner
            (remap-index (face-corner-vertex-index face-corner)
                         original-vertices vertex-ht)
            (face-corner-texture-index face-corner)
            (remap-index (face-corner-normal-index face-corner)
                         original-normals normal-ht))))
        ))


    (define (fix-face-winding model)
      (snow-assert (model? model))
      ;; obj files should have counter-clockwise points.  If we read normals
      ;; and the ordering on the points that define a face suggest that
      ;; the normal is more than 90 degrees off of the read normal,
      ;; flip the ordering of the points in the face.
      (operate-on-faces
       model
       (lambda (mesh face)
         (snow-assert (mesh? mesh))
         (snow-assert (face? face))
         (let ((vertices (face->vertices model face)))
           ;; a face might be defined by more than 3 vertices.  if so, punt.
           (cond ((= (vector-length vertices) 3)
                  (let ((normals
                         `(,(face-corner->normal model (vector-ref face 0))
                           ,(face-corner->normal model (vector-ref face 1))
                           ,(face-corner->normal model (vector-ref face 2))))
                        (vertex-0 (vector-ref vertices 0))
                        (vertex-1 (vector-ref vertices 1))
                        (vertex-2 (vector-ref vertices 2)))
                    (if (memq 'unset normals)
                        ;; if any of the normals are missing don't try.
                        face
                        ;; we have 3 vertices with 3 normals.
                        ;; average the normals
                        (let* ((normal (apply vector3-average normals))
                               ;; get cross-product of triangle sides
                               (diff-1-0 (vector3-diff vertex-1 vertex-0))
                               (diff-2-0 (vector3-diff vertex-2 vertex-0))
                               (cross (cross-product diff-1-0 diff-2-0))
                               ;; angle between implied normal and read one
                               (angle-between
                                (angle-between-vectors
                                 normal cross (cross-product normal cross))))
                          (cond ((> (abs angle-between) pi/2)
                                 ;; the angles don't agree, so flip the face
                                 (vector (vector-ref face 0)
                                         (vector-ref face 2)
                                         (vector-ref face 1)))
                                ;; the angles agree, leave it alone.
                                (else face))))))
                 ;; not 3 vertices in face, pass it back unchanged.
                 (else face))))))



    (define (add-simple-texture-coordinates model scale)
      ;; transform each face to the corner of some supposed texture and decide
      ;; on reasonable texture coords for the face.
      (snow-assert (model? model))
      ;; erase all the existing texture coordinates
      (model-clear-texture-coordinates! model)
      ;; set new texture coordinates for every face
      (operate-on-faces
       model
       (lambda (mesh face)
         (snow-assert (mesh? mesh))
         (snow-assert (face? face))
         (let ((vertices (face->vertices model face)))
           (snow-assert (> (vector-length vertices) 2))
           (let* ((vertex-0 (vector-ref vertices 0))
                  (vertex-1 (vector-ref vertices 1))
                  (vertex-last
                   (vector-ref vertices (- (vector-length vertices) 1)))
                  ;; pick x and y axis.  x is along the line from vertex-0
                  ;; to vertex-1.  y is perpendicular to the x axis and to
                  ;; the normal of the triangle.
                  (x-axis (vector3-normalize (vector3-diff vertex-1 vertex-0)))
                  (diff-last-0 (vector3-diff vertex-last vertex-0))
                  (normal (cross-product x-axis diff-last-0))
                  (y-axis (vector3-normalize (cross-product normal x-axis)))
                  ;; vertex-transformer takes a vertex in 3 space and maps
                  ;; it into the coordinate system defined by the axis
                  ;; we defined, above.
                  (vertex-transformer
                   (lambda (vertex x-offset)
                     (let* ((dv (vector3-diff vertex vertex-0))
                            (x (+ (dot-product x-axis dv) x-offset))
                            (y (dot-product y-axis dv)))
                       (vector2-scale (vector x y) scale))))
                  (first-tex-v (vertex-transformer vertex-0 0.0))
                  (last-tex-v (vertex-transformer vertex-last 0.0))
                  (offset (- (min (vector2-x first-tex-v) (vector2-x last-tex-v)))))

             (vector-for-each
              (lambda (face-corner)
                (snow-assert (face-corner? face-corner))
                (let* ((vertex (face-corner->vertex model face-corner)))
                  (model-append-texture-coordinate!
                   model (vertex-transformer vertex offset))
                  (let ((index (length (model-texture-coordinates model))))
                    (face-corner-set-texture-index! face-corner index))))
              face)

             face)))))


    (define (model-aa-box model)
      (cond ((null? (model-vertices model)) #f)
            (else
             (let* ((p0 (vector-map string->number (car (model-vertices model))))
                    (aa-box (make-aa-box p0 p0)))
               ;; insert all of the model's vertices into the axis-aligned bounding box
               (for-each
                (lambda (p)
                  (aa-box-add-point! aa-box (vector-map string->number p)))
                (model-vertices model))
               aa-box))))


    (define (model-dimensions model)
      (let* ((aa-box (model-aa-box model))
             (low (aa-box-low-corner aa-box))
             (high (aa-box-high-corner aa-box)))
        (vector (- (vector-ref high 0) (vector-ref low 0))
                (- (vector-ref high 1) (vector-ref low 1))
                (- (vector-ref high 2) (vector-ref low 2)))))


    (define (model-max-dimension model)
      (vector-max (model-dimensions model)))


    (define (model-texture-aa-box model)
      (cond ((null? (model-texture-coordinates model)) #f)
            (else
             (let* ((p0 (car (model-texture-coordinates model)))
                    (aa-box (make-aa-box p0 p0)))
               ;; insert all of the model's texture-coordinates into the axis-aligned bounding box
               (for-each
                (lambda (p)
                  (aa-box-add-point! aa-box p))
                (model-texture-coordinates model))
               aa-box))))


    (define (scale-model model scaling-factor)
      (model-set-vertices!
       model
       (map
        (lambda (vertex)
          (vector-map
           (lambda (p)
             (number->pretty-string (* (string->number p) scaling-factor) 6))
           vertex))
        (model-vertices model))))


    (define (size-model model desired-max-dimension)
      (let ((max-dimension (model-max-dimension model)))
        (scale-model model (/ (inexact desired-max-dimension) (inexact max-dimension)))))


    (define (translate-model model by-offset)
      (model-set-vertices!
       model
       (map
        (lambda (vertex)
          (let ((fvertex (vector-map string->number vertex)))
            (vector-map number->string (vector3-sum fvertex by-offset))))
        (model-vertices model))))

    
    ))
