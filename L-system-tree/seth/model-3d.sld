(define-library (seth model-3d)
  (export
   ;; models
   make-model
   make-empty-model
   model?
   model-meshes model-set-meshes!
   model-vertices model-set-vertices!
   model-texture-coordinates model-set-texture-coordinates!
   model-normals model-set-normals!
   model-material-libraries model-set-material-libraries!
   model-materials model-set-materials!
   model-prepend-mesh!
   model-get-material-by-name
   model-aa-box
   model-dimensions
   model-max-dimension
   model-texture-aa-box
   model-clear-texture-coordinates!
   model-append-texture-coordinate!
   model-prepend-vertex!
   model-append-vertex!
   model-prepend-normal!
   model-append-normal!
   ;; materials
   make-material
   material?
   material-name material-set-name!
   add-simple-texture-coordinates
   set-all-faces-to-material
   clear-material-libraries
   add-material-library
   add-material-libraries
   ;; meshes
   make-mesh
   mesh?
   mesh-name mesh-set-name!
   mesh-faces mesh-set-faces!
   mesh-prepend-face!
   mesh-append-face!
   ;; face-corners
   make-face-corner
   face-corner?
   face-corner-vertex-index face-corner-set-vertex-index!
   face-corner-texture-index face-corner-set-texture-index!
   face-corner-normal-index face-corner-set-normal-index!
   face-corner->vertex
   face-corner->normal
   ;; faces
   make-face
   face?
   face-corners face-set-corners!
   face-material face-set-material!
   face->vertices
   fix-face-winding
   shift-face-indices
   ;; mapers/iterators
   for-each-mesh
   operate-on-faces
   operate-on-face-corners
   ;; utilities
   compact-obj-model
   scale-model
   size-model
   translate-model
   )

  (import (scheme base)
          (scheme file)
          (scheme write)
          (scheme cxr)
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
      (make-model meshes vertices texture-coordinates normals material-libraries materials)
      model?
      (meshes model-meshes model-set-meshes!) ;; list of meshes
      (vertices model-vertices model-set-vertices!) ;; list of vertices
      (texture-coordinates model-texture-coordinates model-set-texture-coordinates!)
      (normals model-normals model-set-normals!)
      (material-libraries model-material-libraries model-set-material-libraries!)
      (materials model-materials model-set-materials!))

    (define (make-empty-model)
      (make-model '() '() '() '() '() (make-hash-table)))


    ;; a material is a reference into an external .mtl file
    (define-record-type <material>
      (make-material name)
      material?
      (name material-name material-set-name!))

    ;; look up a material by name.  if this is the first reference to it, create it.
    (define (model-get-material-by-name model material-name)
      (snow-assert (model? model))
      (snow-assert (or (string? material-name) (not material-name)))
      (cond ((not material-name) #f)
            (else
             (let ((materials (model-materials model)))
               (cond ((hash-table-exists? materials material-name)
                      (hash-table-ref materials material-name))
                     (else
                      (let ((new-material (make-material material-name)))
                        (hash-table-set! materials material-name new-material)
                        new-material)))))))



    ;; a mesh is a group of triangles defined by indexing into the model's vertexes
    (define-record-type <mesh>
      (make-mesh name faces)
      mesh?
      (name mesh-name mesh-set-name!)
      (faces mesh-faces mesh-set-faces~!))

    (define (mesh-set-faces! mesh faces)
      (snow-assert (mesh? mesh))
      (snow-assert (list? faces))
      (for-each
       (lambda (face)
         (snow-assert (face? face)))
       faces)
      (mesh-set-faces~! mesh faces))



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


    ;; a face is a list of face corners and a material
    (define-record-type <face>
      (make-face~ corners material)
      face?
      (corners face-corners face-set-corners!) ;; corners is a vector
      (material face-material face-set-material!))


    (define (make-face model corners material)
      (snow-assert (model? model))
      (snow-assert (vector? corners))
      (vector-for-each
       (lambda (corner)
         (snow-assert (face-corner? corner)))
       corners)
      (snow-assert (or (material? material) (not material)))
      (make-face~ corners material))



    (define (model-clear-texture-coordinates! model)
      (model-set-texture-coordinates! model '()))


    (define (model-append-texture-coordinate! model v)
      (snow-assert (model? model))
      (snow-assert (vector? v))
      (snow-assert (= (vector-length v) 2))
      (model-set-texture-coordinates!
       model (reverse (cons v (reverse (model-texture-coordinates model))))))



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
        (if (eq? index 'unset) 'unset
            (vector-map string->number (list-ref (model-normals model) index)))))


    (define (for-each-mesh model proc)
      (for-each proc (model-meshes model)))


    ;; call op with and replace every face in model.  op should accept mesh
    ;; and face and return face
    (define (operate-on-faces model op)
      (for-each-mesh
       model
       (lambda (mesh)
         (snow-assert (mesh? mesh))
         (mesh-set-faces!
          mesh
          (map
           (lambda (face) (op mesh face))
           (mesh-faces mesh))))))


    ;; call op with and replace every face corner.  op should accept mesh and
    ;; face and face-corner and return face-corner.
    (define (operate-on-face-corners model op)
      (snow-assert (model? model))
      (operate-on-faces
       model
       (lambda (mesh face)
         (snow-assert (mesh? mesh))
         (snow-assert (face? face))
         (face-set-corners!
          face
          (vector-map
           (lambda (face-corner)
             (snow-assert (face-corner? face-corner))
             (let ((op-result (op mesh face face-corner)))
               (snow-assert (face-corner? op-result))
               op-result))
           (face-corners face)))
         face)))


    ;; a face includes a vector of indexes into the models vertices.  turn
    ;; a face into a vector of vertices.
    (define (face->vertices model face)
      (snow-assert (model? model))
      (snow-assert (face? face))
      (vector-map
       (lambda (face-corner)
         (snow-assert (face-corner? face-corner))
         (face-corner->vertex model face-corner))
       (face-corners face)))


    (define (model-prepend-mesh! model mesh)
      (snow-assert (model? model))
      (snow-assert (mesh? mesh))
      (model-set-meshes! model (cons mesh (model-meshes model))))



    (define (shift-face-indices face vertex-index-start texture-index-start normal-index-start)
      (snow-assert (face? face))
      (snow-assert (integer? vertex-index-start))
      (snow-assert (integer? texture-index-start))
      (snow-assert (integer? normal-index-start))
      (vector-map
       (lambda (corner)
         (snow-assert (face-corner? corner))
         (face-corner-set-vertex-index!
          corner (+ (face-corner-vertex-index corner) vertex-index-start))
         (if (not (eq? (face-corner-texture-index corner) 'unset))
             (face-corner-set-texture-index!
              corner (+ (face-corner-texture-index corner) texture-index-start)))
         (if (not (eq? (face-corner-normal-index corner) 'unset))
             (face-corner-set-normal-index!
              corner (+ (face-corner-normal-index corner) normal-index-start))))
       (face-corners face)))


    (define (model-prepend-vertex! model x y z)
      (snow-assert (model? model))
      (snow-assert (string? x))
      (snow-assert (string? y))
      (snow-assert (string? z))
      (model-set-vertices! model (cons (vector x y z) (model-vertices model))))



    (define (model-append-vertex! model v)
      ;; returns the index of the new vertex
      (snow-assert (model? model))
      (snow-assert (vector? v))
      (snow-assert (= (vector-length v) 3))
      (snow-assert (string? (vector-ref v 0)))
      (snow-assert (string? (vector-ref v 1)))
      (snow-assert (string? (vector-ref v 2)))
      (model-set-vertices! model (reverse (cons v (reverse (model-vertices model)))))
      (- (length (model-vertices model)) 1))


    (define (model-prepend-normal! model x y z)
      (snow-assert (model? model))
      (snow-assert (string? x))
      (snow-assert (string? y))
      (snow-assert (string? z))
      (model-set-normals! model (cons (vector x y z) (model-normals model))))


    (define (model-append-normal! model v)
      ;; returns the index of the new normal
      (snow-assert (model? model))
      (snow-assert (vector? v))
      (snow-assert (= (vector-length v) 3))
      (snow-assert (string? (vector-ref v 0)))
      (snow-assert (string? (vector-ref v 1)))
      (snow-assert (string? (vector-ref v 2)))
      (model-set-normals! model (reverse (cons v (reverse (model-normals model)))))
      (- (length (model-normals model)) 1))


    (define (mesh-prepend-face! model mesh face-corners material
                                vertex-index-start texture-index-start
                                normal-index-start inport)
      (snow-assert (mesh? mesh))
      (snow-assert (list? face-corners))
      (let ((face (make-face model (list->vector face-corners) material)))
        (shift-face-indices face vertex-index-start texture-index-start normal-index-start)
        (mesh-set-faces! mesh (cons face (mesh-faces mesh)))))


    (define (mesh-append-face! model mesh face)
      (snow-assert (model? model))
      (snow-assert (mesh? mesh))
      (snow-assert (face? face))
      (mesh-set-faces! mesh (reverse (cons face (reverse (mesh-faces mesh))))))


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
           (snow-assert (mesh? mesh))
           (snow-assert (face? face))
           (snow-assert (face-corner? face-corner))

           (make-face-corner
            (remap-index (face-corner-vertex-index face-corner)
                         original-vertices vertex-ht)
            (face-corner-texture-index face-corner) ;; XXX compact these also
            (remap-index (face-corner-normal-index face-corner)
                         original-normals normal-ht))
           ))
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
         (let ((vertices (face->vertices model face))
               (corners (face-corners face)))
           ;; a face might be defined by more than 3 vertices.  if so, punt.
           (cond ((= (vector-length vertices) 3)
                  (let ((normals
                         `(,(face-corner->normal model (vector-ref corners 0))
                           ,(face-corner->normal model (vector-ref corners 1))
                           ,(face-corner->normal model (vector-ref corners 2))))
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
                                 (face-set-corners!
                                  face
                                  (vector (vector-ref corners 0)
                                          (vector-ref corners 2)
                                          (vector-ref corners 1)))
                                 face)
                                ;; the angles agree, leave it alone.
                                (else face))))))
                 ;; not 3 vertices in face, pass it back unchanged.
                 (else face))))))


    (define (pick-u-v-axis~ vertices)
      (snow-assert (vector? vertices))
      (snow-assert (> (vector-length vertices) 2))
      (let* ((vertex-0 (vector-ref vertices 0))
             (vertex-1 (vector-ref vertices 1))
             (vertex-last (vector-ref vertices (- (vector-length vertices) 1)))
             ;; pick u and v axis.  u is along the line from vertex-0
             ;; to vertex-1.  v is perpendicular to the u axis and to
             ;; the normal of the triangle.
             (u-axis (vector3-normalize (vector3-diff vertex-1 vertex-0)))
             (diff-last-0 (vector3-diff vertex-last vertex-0))
             (normal (cross-product u-axis diff-last-0))
             (v-axis (vector3-normalize (cross-product normal u-axis))))
        (values u-axis v-axis)))


    (define (pick-u-v-axis vertices)
      (snow-assert (vector? vertices))
      (snow-assert (> (vector-length vertices) 2))
      (let* ((vertex-0 (vector-ref vertices 0))
             (vertex-1 (vector-ref vertices 1))
             (vertex-last (vector-ref vertices (- (vector-length vertices) 1)))
             (diff-1-0 (vector3-diff vertex-1 vertex-0))
             (diff-last-0 (vector3-diff vertex-last vertex-0))
             (normal (cross-product diff-1-0 diff-last-0))

             (sx (vector3-normalize #(1 0 0)))
             (sy (vector3-normalize #(0 1 0)))
             (sz (vector3-normalize #(0 0 1)))
             (best-normal-axis (worst-aligned-vector normal (list sx sy sz)))

             (center (vector3-scale
                      (apply vector3-sum (vector->list vertices))
                      (/ 1.0 (vector-length vertices))))
             (plane-intersection (triangle-plane-intersection vertices (vector center best-normal-axis)))

             )
        (cond ((not plane-intersection)
               (pick-u-v-axis~ vertices))

              (else
               (let* ((u0 (vector-ref plane-intersection 0))
                      (u1 (vector-ref plane-intersection 1))
                      (u-direction (vector3-diff u1 u0))
                      (u-axis (vector3-normalize u-direction))
                      (v-axis (vector3-normalize (cross-product normal u-axis))))
                 (values u-axis v-axis))))))


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
           (let-values (((u-axis v-axis) (pick-u-v-axis vertices)))
             (let* ((vertex-0 (vector-ref vertices 0))
                    ;; vertex-transformer takes a vertex in 3 space and maps
                    ;; it into the coordinate system defined by the axis
                    ;; we defined, above.
                    (vertex-transformer
                     (lambda (vertex x-offset)
                       (let* ((dv (vector3-diff vertex vertex-0))
                              (x (+ (dot-product u-axis dv) x-offset))
                              (y (dot-product v-axis dv)))
                         (vector2-scale (vector x y) scale))))
                    (vertex-transformer-no-offset
                     (lambda (vertex) (vertex-transformer vertex 0.0)))
                    ;; figure out how much we have to slide this face over
                    ;; in order to have all the uv coord be positive.
                    (un-offset-uvs
                     (vector->list
                      (vector-map vertex-transformer-no-offset vertices)))
                    (offset
                     (- (apply min (map vector2-x un-offset-uvs)))))

               (vector-for-each
                (lambda (face-corner)
                  (snow-assert (face-corner? face-corner))
                  (let ((index (length (model-texture-coordinates model)))
                        (vertex (face-corner->vertex model face-corner)))
                    (model-append-texture-coordinate!
                     model (vertex-transformer vertex offset))
                    (face-corner-set-texture-index! face-corner index)))
                (face-corners face)))))
         face)))


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
      (snow-assert (model? model))
      (snow-assert (vector? by-offset))
      (snow-assert (= (vector-length by-offset) 3))
      (model-set-vertices!
       model
       (map
        (lambda (vertex)
          (let ((fvertex (vector-map string->number vertex)))
            (vector-map number->string (vector3-sum fvertex by-offset))))
        (model-vertices model))))


    (define (set-all-faces-to-material model material-name)
      (snow-assert (model? model))
      (snow-assert (string? material-name))
      (let ((material (model-get-material-by-name model material-name)))
        (operate-on-faces
         model
         (lambda (mesh face)
           (face-set-material! face material)
           face))))


    (define (clear-material-libraries model)
      (snow-assert (model? model))
      (model-set-material-libraries! model '()))


    (define (add-material-library model material-library-name)
      (snow-assert (model? model))
      (snow-assert (string? material-library-name))
      (cond ((member material-library-name (model-material-libraries model))
             #t)
            (else
             (model-set-material-libraries!
              model
              (cons material-library-name (model-material-libraries model))))))


    (define (add-material-libraries model material-library-names)
      (snow-assert (model? model))
      (for-each
       (lambda (material-library-name)
         (add-material-library model material-library-name))
       material-library-names))


    ))
