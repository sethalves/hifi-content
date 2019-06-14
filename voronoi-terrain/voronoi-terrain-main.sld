

;; http://www.voronoi.com/wiki/index.php?title=Main_Page


(define-library (voronoi-terrain-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (scheme cxr)
          (srfi 1)
          (srfi 69)
          (srfi 95)
          (snow assert)
          (foldling command-line)
          (seth cout)
          (seth strings)
          (seth math-3d)
          (seth raster)
          (seth image)
          (seth pbm)
          (seth graph)
          (seth model-3d)
          (seth obj-model)
          (seth scad-model)
          (seth octree)
          )
  (begin

    (define epsilon 0.0001)

    (define-record-type <voronoi-graph-data>
      (make-voronoi-graph-data point index status)
      voronoi-graph-data?
      (point voronoi-graph-data-point voronoi-graph-data-set-point!)
      (index voronoi-graph-data-index voronoi-graph-data-set-index!)
      (status voronoi-graph-data-status voronoi-graph-data-set-status!))

    (define (image-fat-line! img pxl x0 y0 x1 y1 channels)
      (image-line! img pxl x0 y0 x1 y1 channels)
      (image-line! img pxl (- x0 1) y0 (- x1 1) y1 channels)
      (image-line! img pxl (+ x0 1) y0 (+ x1 1) y1 channels)
      (image-line! img pxl x0 (- y0 1) x1 (- y1 1) channels)
      (image-line! img pxl x0 (+ y0 1) x1 (+ y1 1) channels))


    (define (polygon->points polygon)
      (map (lambda (node)
             (voronoi-graph-data-point (node-value node)))
           polygon))


    (define (points-average points)
      (vector2-scale
       (fold vector2-sum (vector 0 0) points)
       (/ 1.0 (length points))))


    (define (show-lines lines width height)
      (let ((image (raster-new width height (vector 255 255 255 255))))
        (for-each
         (lambda (line)
           (let ((x0 (exact (round (vector-ref (car line) 0))))
                 (y0 (exact (round (vector-ref (car line) 1))))
                 (x1 (exact (round (vector-ref (cadr line) 0))))
                 (y1 (exact (round (vector-ref (cadr line) 1)))))
             ;; (image-fat-line! image (vector 0 0 0 255) x0 y0 x1 y1 rgba)
             (image-line! image (vector 0 0 0 255) x0 y0 x1 y1 rgba)
             ))
         lines)

        (image->ppm image (current-output-port))))


    (define (point-loop->lines points)
      (let loop ((points points)
                 (lines '()))
        (cond ((null? points) lines)
              ((null? (cdr points)) lines)
              (else (let ((point-a (car points))
                          (point-b (cadr points)))
                      (loop (cdr points)
                            (cons (list point-a point-b) lines)))))))


    (define (show-polygons polygons width height)
      ;; polygon is a list of graph nodes
      (let loop ((polygons polygons)
                 (lines '()))
        (if (null? polygons)
            (show-lines lines width height)
            (let* ((polygon (car polygons))
                   (points (polygon->points polygon))
                   (center (points-average points))
                   ;; squish all the points toward the center, a little
                   (off-points (map
                                (lambda (point)
                                  (let* ((point->center (vector2-diff center point))
                                         (point->center-n (vector2-normalize point->center))
                                         (offset (vector2-scale point->center-n 2.0)))
                                    (vector2-sum point offset)))
                                points))
                   (off-points-loop (cons (last off-points) off-points))
                   (new-lines (point-loop->lines off-points-loop)))
              (loop (cdr polygons) (append lines new-lines))))))


    (define (show-graph graph width height)
      (let ((lines (map
                    (lambda (edge)
                      (let* ((node-a (edge-start-node edge))
                             (node-b (edge-end-node edge))
                             (data-a (node-value node-a))
                             (data-b (node-value node-b))
                             (point-a (voronoi-graph-data-point data-a))
                             (point-b (voronoi-graph-data-point data-b)))
                        (list point-a point-b)))
                    (graph-edges graph))))
        (show-lines lines width height)))


    (define (find-face-for-point model octree p previous-face)
      ;; put an vertical ray through the top of the model and find which face is below the xz point
      (let ((segment (vector (vector (vector2-x p) 2000 (vector2-y p))
                             (vector (vector2-x p) -2000 (vector2-y p)))))
        (if (and previous-face
                 (segment-triangle-intersection segment (face->vertices model previous-face)))
            previous-face
            (let loop ((octree-parts (octree-ray-intersection octree segment)))
              (if (null? octree-parts) #f
                  (let face-loop ((faces (octree-contents (car octree-parts))))
                    (if (null? faces) (loop (cdr octree-parts))
                        (let ((vertices (face->vertices model (car faces))))
                          (if (segment-triangle-intersection segment vertices)
                              (car faces)
                              (face-loop (cdr faces)))))))))))


    (define (write-surface-texture model width height output-x-size output-z-size height-function)
      (let* ((output-image-width 256)
             (output-image-height 256)
             (octree (model->octree model (model-aa-box model)))
             (img (raster-new output-image-width output-image-height (vector 255 255 255 255)))
             (image-x->model-x (lambda (img-x) (/ (* img-x output-x-size) output-image-width)))
             (image-y->model-z (lambda (img-y) (/ (* img-y output-z-size) output-image-height)))
             (previous-normal #f)
             (previous-face #f)
             )
        (do ((y 0 (+ y 1)))
            ((= y output-image-height) #t)
          (let ((model-z (image-y->model-z y)))
            (do ((x 0 (+ x 1)))
                ((= x output-image-width) #t)
              ;; (cerr "x=" x ", y=" y "\n")
              (let* ((model-x (image-x->model-x x))
                     (face (find-face-for-point model octree (vector model-x model-z) previous-face))
                     (normal (if (and previous-normal (eq? face previous-face))
                                 previous-normal
                                 (if face (face->average-normal model face) #f)))
                     (flat (if normal (> (vector3-y normal) 0.999) #f)))
                (set! previous-normal normal)
                (set! previous-face face)
                (if flat
                    ;; (raster-set-pixel! img x y (vector 153 76 0 255))
                    (raster-set-pixel! img x y (vector 0 76 0 255))
                    (raster-set-pixel! img x y (vector 0 150 0 255)))
                ;; (if normal (raster-set-pixel! img x y (vector (* (- 1.0 (vector3-y normal)) 255)
                ;;                                               76
                ;;                                               0
                ;;                                               255)))
                ))))

        ;; (cerr "normal = " (find-normal-for-point model (vector 50 50)) "\n")
        (image->ppm img (current-output-port))))


    (define (output-caves model width height output-x-size output-z-size height-function)

      )


    (define (point-x-< p0 p1)
      (if (almost= (vector2-x p0) (vector2-x p1) epsilon)
          (< (vector2-y p0) (vector2-y p1))
          (< (vector2-x p0) (vector2-x p1))))

    (define (point-x-> p0 p1)
      (if (almost= (vector2-x p0) (vector2-x p1) epsilon)
          (> (vector2-y p0) (vector2-y p1))
          (> (vector2-x p0) (vector2-x p1))))

    (define (point-y-< p0 p1)
      (if (almost= (vector2-y p0) (vector2-y p1) epsilon)
          (< (vector2-x p0) (vector2-x p1))
          (< (vector2-y p0) (vector2-y p1))))

    (define (point-y-> p0 p1)
      (if (almost= (vector2-y p0) (vector2-y p1) epsilon)
          (> (vector2-x p0) (vector2-x p1))
          (> (vector2-y p0) (vector2-y p1))))

    (define (lines->points lines)
      (let loop ((lines lines)
                 (points (list)))
        (if (null? lines)
            points
            (let ((p0 (caar lines))
                  (p1 (cadar lines)))
              (loop (cdr lines)
                    (cons p1 (cons p0 points)))))))


    (define (points->unique-points points)
      (let ((x-sorted-points (sort points point-x-<)))
        (let loop ((unique-points (list))
                   (x-sorted-points x-sorted-points))
          (cond ((null? x-sorted-points) unique-points)
                ((null? (cdr x-sorted-points))
                 (cons (car x-sorted-points) unique-points))
                (else
                 (let ((p0 (car x-sorted-points))
                       (p1 (cadr x-sorted-points)))
                   (if (vector2-almost-equal? p0 p1 epsilon)
                       (loop unique-points (cdr x-sorted-points))
                       (loop (cons p0 unique-points)
                             (cdr x-sorted-points)))))))))

    (define (add-corners points width height)
      (cons (vector 0 0)
            (cons (vector 0 height)
                  (cons (vector width 0)
                        (cons (vector width height)
                              points)))))

    (define (lines->unique-points+corners lines width height)
      (let* ((points (lines->points lines)))
        (points->unique-points (add-corners points width height))))


    (define (setup-x-almost-= value)
      (lambda (point)
        (almost= (vector2-x point) value epsilon)))

    (define (setup-y-almost-= value)
      (lambda (point)
        (almost= (vector2-y point) value epsilon)))


    (define (discover-edge-points lines width height)
      (let* ((points (lines->unique-points+corners lines width height))
             ;; down along left edge
             (left-points (filter (setup-x-almost-= 0.0) points))
             (left-points-sorted (sort left-points point-y-<))
             ;; left to right along bottom
             (bottom-points (filter (setup-y-almost-= height) points))
             (bottom-points-sorted (sort bottom-points point-x-<))
             ;; up along right edge
             (right-points (filter (setup-x-almost-= width) points))
             (right-points-sorted (sort right-points point-y->))
             ;; right to left along the top
             (top-points (filter (setup-y-almost-= 0.0) points))
             (top-points-sorted (sort top-points point-x->)))
        (values left-points-sorted bottom-points-sorted
                right-points-sorted top-points-sorted)))


    (define (make-edge-lines left-edge-points bottom-edge-points
                             right-edge-points top-edge-points)
      ;; (cerr "all: " points "\n")
      ;; (cerr "left: " left-points-sorted "\n")
      ;; (cerr "top: " top-points-sorted "\n")
      ;; (cerr "right: " right-points-sorted "\n")
      ;; (cerr "bottom: " bottom-points-sorted "\n")

      ;; put them all in a loop
      (let ((sorted-edge-points (append left-edge-points
                                        bottom-edge-points
                                        right-edge-points
                                        top-edge-points
                                        ;; and close loop
                                        (list (car left-edge-points)))))
        (let loop ((edges (list))
                   (sorted-edge-points sorted-edge-points))
          (cond ((null? sorted-edge-points) edges)
                ((null? (cdr sorted-edge-points)) edges)
                (else
                 (let ((p0 (car sorted-edge-points))
                       (p1 (cadr sorted-edge-points)))
                   (if (vector2-almost-equal? p0 p1 epsilon)
                       (loop edges (cdr sorted-edge-points))
                       (loop (cons (list p0 p1) edges)
                             (cdr sorted-edge-points)))))))))



    (define (tug-lines-ends-to-points in-lines points)
      (map
       (lambda (line)
         (let loop ((points points)
                    (line line))
           (cond ((null? points) line)
                 (else
                  (let ((point (car points)))
                    ;; (cerr "tug point: " point "\n")
                    (cond ((and (vector2-almost-equal? (car line) point epsilon)
                                (vector2-almost-equal? (cadr line) point epsilon))
                           ;; (cerr "a\n")
                           (loop (cdr points) (list point point)))
                          ((vector2-almost-equal? (car line) point epsilon)
                           ;; (cerr "b\n")
                           (loop (cdr points) (list point (cadr line))))
                          ((vector2-almost-equal? (cadr line) point epsilon)
                           ;; (cerr "c\n")
                           (loop (cdr points) (list (car line) point)))
                          (else
                           ;; (cerr "d\n")
                           (loop (cdr points) line))))))))
       in-lines))


    (define (edge-angle node-0 node-1)
      (let* ((p0 (voronoi-graph-data-point (node-value node-0)))
             (p1 (voronoi-graph-data-point (node-value node-1)))
             (delta (vector2-diff p1 p0)))
        (atan2 (vector2-y delta) (vector2-x delta))))


    (define (find-face-edge node-a node-b)
      ;; look for an edge that joins node-b some node
      ;; other than node-a and is a sharper left-turn
      ;; than the others.
      (let loop ((b-edges (node-edges node-b))
                 (leftest-angle 0)
                 (face-edge #f))
        (if (null? b-edges)
            (begin
              ;; (cerr "find-face-edge: "
              ;;       (voronoi-graph-data-point (node-value node-a)) " --> "
              ;;       (voronoi-graph-data-point (node-value node-b)) " --> "
              ;;       (if face-edge
              ;;           (voronoi-graph-data-point (node-value (edge-other-node face-edge node-b)))
              ;;           #f) " -- "
              ;;       leftest-angle "\n")
              face-edge)
            (let* ((bc-edge (car b-edges))
                   (node-c (edge-other-node bc-edge node-b))
                   (b-rest (cdr b-edges)))
              (if (eq? node-a node-c)
                  ;; don't go backwards
                  (loop b-rest leftest-angle face-edge)
                  ;; see if this edge is best
                  (let* ((angle-ab (edge-angle node-a node-b))
                         (angle-bc (edge-angle node-b node-c))
                         (angle-change
                          (and angle-ab angle-bc
                               (normalize-angle (- angle-bc angle-ab)))))
                    (cond ((not angle-change)
                           (loop b-rest leftest-angle face-edge))
                          ((< angle-change 0)
                           ;; a right turn
                           (loop b-rest leftest-angle face-edge))
                          ((> angle-change pi)
                           ;; a spin-too-far left turn
                           (loop b-rest leftest-angle face-edge))
                          ((> angle-change leftest-angle)
                           ;; best left-turn so far
                           (loop b-rest angle-change bc-edge))
                          (else
                           ;; a left turn, but not as sharp as
                           ;; another one we've already seen
                           (loop b-rest leftest-angle face-edge)))))))))

    (define (point->corner model mesh material point)
      (let* ((point-index (model-append-vertex! model (make-vertex-from-point/color point #f))))
        (make-face-corner point-index 'unset 'unset)))


    (define (add-faces model mesh path-nodes)
      (let* ((corners (map (lambda (node)
                             (make-face-corner
                              (voronoi-graph-data-index (node-value node))
                              'unset ;; texture
                              'unset)) ;; normal
                           path-nodes))
             (corners-count (length corners))
             (material #f))
        (let ((face (make-face model (list->vector corners) material)))
          (cond ((< corners-count 3) #f)
                ((= corners-count 3)
                 (if (not (model-contains-equivalent-face model face epsilon))
                     (mesh-append-face! model mesh face)))
                (else
                 (let* ((center-vertex (face->center-vertex model face))
                        (center-corner
                         (point->corner model mesh material center-vertex))
                        (last-corner (last corners)))
                   (let loop ((corners (cons last-corner corners)))
                     (cond ((null? corners) #t)
                           ((null? (cdr corners)) #t)
                           (else
                            (let* ((corner-b (car corners))
                                   (corner-c (cadr corners))
                                   (subface (make-face model (vector center-corner corner-b corner-c)
                                                       material)))
                              (if (not (model-contains-equivalent-face model subface epsilon))
                                  (mesh-append-face! model mesh subface))
                              (loop (cdr corners))))))))))))

    (define (face-search start-node path-nodes node-a node-b)
      (cond
       ((eq? start-node node-b)
        path-nodes
        ;; (add-faces model mesh path-nodes)
        )
       (else
        (let ((face-edge (find-face-edge node-a node-b)))
          (cond (face-edge
                 (face-search start-node
                              (cons node-b path-nodes)
                              node-b
                              (edge-other-node face-edge node-b)))
                (else #f))))))


    (define (find-polygons graph)
      (let ((polygons '()))
        (let loop ((nodes (graph-nodes graph)))
          (cond ((null? nodes) polygons)
                (else
                 (let* ((node (car nodes))
                        (data (node-value node)))
                   (for-each
                    (lambda (edge)
                      (let ((face (face-search node (list node) node
                                               (edge-other-node edge node))))
                        (if face (set! polygons (cons face polygons)))))
                    (node-edges node))
                   (loop (cdr nodes))))))))



    (define (graph->model graph polygons multiplier height-function)
      (let ((model (make-empty-model))
            (mesh (make-mesh #f '())))
        (model-prepend-mesh! model mesh)

        ;; fill in the indexes of the nodes
        (for-each
         (lambda (node)
           (let* ((data (node-value node))
                  (point (voronoi-graph-data-point data))
                  ;; y axis is up
                  (point-3d (vector (vector2-x point)
                                    (height-function point)
                                    (vector2-y point)))
                  (point-3d-scaled
                   (vector (* (vector3-x point-3d) (vector3-x multiplier))
                           (* (vector3-y point-3d) (vector3-y multiplier))
                           (* (vector3-z point-3d) (vector3-z multiplier))))
                  (vtx (make-vertex-from-point/color point-3d-scaled #f))
                  (index (model-append-vertex! model vtx)))
             (voronoi-graph-data-set-index! data index)))
         (graph-nodes graph))

        ;; search for polygons in the graph
        (for-each (lambda (polygon)
                    (add-faces model mesh polygon))
                  polygons)

        model))



    (define (line-segments->graph in-lines)
      (let* ((graph (make-graph))
             (points (points->unique-points (lines->points in-lines)))
             (lines (tug-lines-ends-to-points in-lines points))
             (nodes (map
                     (lambda (point)
                       (let ((data (make-voronoi-graph-data point #f #f)))
                         (make-node graph data)))
                     points))
             (nodes-hash (make-hash-table)))

        ;; (cerr "--------------- points --------\n")
        ;; (for-each (lambda (point) (cerr point "\n")) points)

        ;; create a hash-table to go from points to graph nodes
        (for-each
         (lambda (node)
           (let ((point (voronoi-graph-data-point (node-value node))))
             (hash-table-set! nodes-hash point node)))
         nodes)



        ;; (cerr "---------------- lines -------\n")
        ;; (let loop ((lines lines)
        ;;            (in-lines in-lines))
        ;;   (if (null? lines) #t
        ;;       (begin
        ;;         (cerr (car in-lines) "  -->  " (car lines) "\n")
        ;;         (loop (cdr lines) (cdr in-lines)))))


        ;; (cerr "---------------- hash keys -------\n")
        ;; (cerr (hash-table-keys nodes-hash) "\n")

        ;; (cerr "---------------- nodes -------\n")
        ;; (for-each (lambda (node-key)
        ;;             (cerr (voronoi-graph-data-point (node-value (hash-table-ref nodes-hash node-key))) "\n"))
        ;;           (hash-table-keys nodes-hash))


        ;; (cerr "---------------- edges -------\n")

        ;; make a graph edge for each line
        (for-each
         (lambda (line)
           (let ((node-a (hash-table-ref nodes-hash (car line)))
                 (node-b (hash-table-ref nodes-hash (cadr line))))
             ;; (cerr (voronoi-graph-data-point (node-value node-a)) " --> "
             ;;       (voronoi-graph-data-point (node-value node-b)) " : "
             ;;       (eq? node-a node-b) "\n")

             (if (not (eq? node-a node-b))
                 (connect-nodes graph node-a node-b))))
         lines)


        graph))


    (define (fill-area model mesh base points)
      ;; fill in a fan-shape by making a series of triangles
      ;; that all have `base` as the first point
      ;; (cerr points "\n")
      (let* ((material #f)
             (base-corner (point->corner model mesh material base))
             (point-corners
              (map (lambda (point)
                     (point->corner model mesh material point))
                   points)))
        (let loop ((point-corners point-corners))
          (cond ((null? point-corners) #t)
                ((null? (cdr point-corners)) #t)
                (else
                 (let* ((corner-a (car point-corners))
                        (corner-b (cadr point-corners))
                        (face (make-face
                               model (vector corner-b corner-a base-corner)
                               material)))
                   (mesh-append-face! model mesh face)
                   (loop (cdr point-corners))))))))


    (define (close-model model width height height-function multiplier
                         left-edge-points bottom-edge-points
                         right-edge-points top-edge-points)
      ;; make a box around the lower part of the model so it's
      ;; closed/water-tight
      (define (2d->3d points)
        (map (lambda (point)
               (vector
                (* (vector2-x point) (vector3-x multiplier))
                (* (height-function point) (vector3-y multiplier))
                (* (vector2-y point) (vector3-z multiplier))))
             points))
      (let ((mesh (car (model-meshes model)))
            (width-s (* (vector3-x multiplier) width))
            (height-s (* (vector3-z multiplier) height)))
        (fill-area model mesh
                   (vector 0 0 (/ height-s 2.0))
                   (append (list (vector 0 0 0))
                           (2d->3d left-edge-points)
                           (list (vector 0 0 height-s))))
        (fill-area model mesh
                   (vector (/ width-s 2.0) 0 height-s)
                   (append (list (vector 0 0 height-s))
                           (2d->3d bottom-edge-points)
                           (list (vector width-s 0 height-s))))
        (fill-area model mesh
                   (vector width-s 0 (/ height-s 2.0))
                   (append (list (vector width-s 0 height-s))
                           (2d->3d right-edge-points)
                           (list (vector width-s 0 0))))
        (fill-area model mesh
                   (vector (/ width-s 2.0) 0 0)
                   (append (list (vector width-s 0 0))
                           (2d->3d top-edge-points)
                           (list (vector 0 0 0))))
        ;; add bottom
        (fill-area model mesh
                   (vector (/ width-s 2.0) 0 (/ height-s 2.0))
                   (list (vector 0 0 0)
                         (vector 0 0 (/ height-s 2.0))
                         (vector 0 0 height-s)
                         (vector (/ width-s 2.0) 0 height-s)
                         (vector width-s 0 height-s)
                         (vector width-s 0 (/ height-s 2.0))
                         (vector width-s 0 0)
                         (vector (/ width-s 2.0) 0 0)
                         (vector 0 0 0)))))

    (define (read-points points-input-filename)
      (let ((points-input-port (open-input-file points-input-filename)))
        (let loop ((points '()))
          (let ((line (read-line points-input-port)))
            (if (eof-object? line)
                (begin
                  (close-port points-input-port)
                  points)
                (let* ((line-port (open-input-string line))
                       (x (read line-port))
                       (y (read line-port))
                       (z (read line-port)))
                  (loop (cons (vector x y z) points))))))))


    (define (read-lines lines-input-filename)
      (let ((lines-input-port (open-input-file lines-input-filename)))
        (let loop ((lines '()))
          (let ((line (read-line lines-input-port)))
            (if (eof-object? line)
                (begin
                  (close-port lines-input-port)
                  lines)
                (let* ((line-port (open-input-string line))
                       (x0 (read line-port))
                       (y0 (read line-port))
                       (x1 (read line-port))
                       (y1 (read line-port))
                       (v0 (vector x0 y0))
                       (v1 (vector x1 y1)))
                  (if (not (vector2-equal? v0 v1))
                      (loop (cons (list v0 v1) lines))
                      (loop lines))))))))


    (define (make-height-function width height multiplier
                                  output-x-size output-y-size output-z-size
                                  points)
      (lambda (point)
        ;; find closest 2 points
        (define (closer? a-vec b-vec)
          (let ((a (cond ((not a-vec) #f)
                         ((= (vector-length a-vec) 2) a-vec)
                         (else (vector (vector3-x a-vec) (vector3-z a-vec)))))
                (b (cond ((not b-vec) #f)
                         ((= (vector-length b-vec) 2) b-vec)
                         (else (vector (vector3-x b-vec) (vector3-z b-vec))))))
            (cond ((not a) #f)
                  ((not b) #t)
                  (else
                   (< (vector3-length (vector2-diff a point))
                      (vector3-length (vector2-diff b point)))))))

        (let loop ((points points)
                   (closest #f)
                   (next-closest #f))
          (if (null? points)
              (let* ((closest-dx
                      (vector3-length (vector2-diff closest point)))
                     (next-closest-dx
                      (vector3-length (vector2-diff next-closest point)))
                     (total-dx (+ closest-dx next-closest-dx))
                     (closest-ratio (if (not (= total-dx 0.0))
                                        (/ closest-dx total-dx)
                                        0.5))
                     (next-closest-ratio (if (not (= total-dx 0.0))
                                             (/ next-closest-dx total-dx)
                                             0.5)))
                (* (vector3-y multiplier)
                   (+ (* (vector3-y closest) closest-ratio)
                      (* (vector3-y next-closest) next-closest-ratio))))

              (let ((p (car points))
                    (rest (cdr points)))
                (cond ((closer? p closest)
                       (loop rest p closest))
                      ((closer? p next-closest)
                       (loop rest closest p))
                      (else
                       (loop rest closest next-closest))))))))


    (define (main-program)
      (define (usage why)
        (cerr why "\n")
        (cerr "voronoi-terrain [arguments] lines-input-file points-input-file")
        (cerr "    --obj                      output an obj file\n")
        (cerr "    --pnm                      output a pnm file\n")
        (cerr "    --scad                     output an openscad file\n")
        (cerr "    --texture                  output green and brown texture\n")
        (cerr "    --caves                    output a negative openscad file\n")
        (cerr "    --input-width w            width of output\n")
        (cerr "    --input-height h           height of output\n")
        (cerr "    --output-x-size x-size     width of output\n")
        (cerr "    --output-y-size y-size     height of output\n")
        (cerr "    --output-z-size z-size     depth of output\n")
        (exit 1))

      (let* ((args (parse-command-line `((--obj)
                                         (--pnm)
                                         (--scad)
                                         (--caves)
                                         (--texture)
                                         ((--input-width) width)
                                         ((--input-height) height)
                                         ((--output-x-size) width)
                                         ((--output-y-size) width)
                                         ((--output-z-size) depth)
                                         (-?) (-h))))
             (output-obj #f)
             (output-pnm #f)
             (output-scad #f)
             (output-caves #f)
             (output-texture #f)
             (width #f)
             (height #f)
             (output-x-size #f)
             (output-y-size #f)
             (output-z-size #f)
             (lines-input-filename #f)
             (points-input-filename #f)
             (extra-arguments '())
             )
        (for-each
         (lambda (arg)
           (case (car arg)
             ((-? -h) (usage ""))
             ((--obj)
              (if (or output-obj output-pnm output-scad output-texture)
                  (usage "give only one of: --obj --pnm --scad --texture --caves"))
              (set! output-obj #t))
             ((--pnm)
              (if (or output-obj output-pnm output-scad output-texture)
                  (usage "give only one of: --obj --pnm --scad --texture --caves"))
              (set! output-pnm #t))
             ((--scad)
              (if (or output-obj output-pnm output-scad output-texture)
                  (usage "give only one of: --obj --pnm --scad --texture --caves"))
              (set! output-scad #t))
             ((--caves)
              (if (or output-obj output-pnm output-caves output-texture)
                  (usage "give only one of: --obj --pnm --caves --texture --caves"))
              (set! output-caves #t))
             ((--texture)
              (if (or output-obj output-pnm output-scad output-texture)
                  (usage "give only one of: --obj --pnm --scad --texture --caves"))
              (set! output-texture #t))             ((--input-width)
              (set! width (string->number (cadr arg))))
             ((--input-height)
              (set! height (string->number (cadr arg))))
             ((--output-x-size)
              (set! output-x-size (string->number (cadr arg))))
             ((--output-y-size)
              (set! output-y-size (string->number (cadr arg))))
             ((--output-z-size)
              (set! output-z-size (string->number (cadr arg))))
             ((--)
              (set! extra-arguments (cdr arg)))))
         args)

        (if (not (= (length extra-arguments) 2))
            (usage "give both lines-input-file and points-input-file"))
        (set! lines-input-filename (car extra-arguments))
        (set! points-input-filename (cadr extra-arguments))

        (if (not width) (set! width 100))
        (if (not height) (set! height 100))

        (if (not output-x-size) (set! output-x-size width))
        (if (not output-z-size) (set! output-z-size height))
        (if (not output-y-size)
            (set! output-y-size (max output-x-size output-z-size)))

        (cerr "input-width=" width " input-height=" height "\n")

        (let* ((lines (read-lines lines-input-filename))
               (points (read-points points-input-filename)))
          (let-values (((left-edge-points
                         bottom-edge-points
                         right-edge-points
                         top-edge-points)
                        (discover-edge-points lines width height)))
            (let* ((edge-lines (make-edge-lines left-edge-points
                                                bottom-edge-points
                                                right-edge-points
                                                top-edge-points))
                   (lines-and-edges (append lines edge-lines)))
              (let* ((graph (line-segments->graph lines-and-edges))
                     (multiplier (vector (/ output-x-size width)
                                         (/ output-y-size 255.0)
                                         (/ output-z-size height)))
                     (height-function (make-height-function
                                       width height
                                       multiplier
                                       output-x-size output-y-size output-z-size
                                       points))
                     (polygons (find-polygons graph))
                     (model (graph->model graph polygons
                                          multiplier
                                          height-function))
                     (close-model (lambda()
                                    (close-model model width height height-function
                                                 multiplier
                                                 left-edge-points
                                                 bottom-edge-points
                                                 right-edge-points
                                                 top-edge-points)
                                    (operate-on-faces model (lambda (mesh face)
                                                              (face-set-normals! model face)
                                                              face))
                                    (compact-obj-model model)
                                    (fix-face-winding! model))))
                (cond

                 (output-obj
                  ;; output a model
                  (close-model)
                  (write-obj-model model (current-output-port)))

                 (output-pnm
                  ;; output a png
                  ;; (show-graph graph width height))
                  (show-polygons polygons width height))

                 (output-scad
                  ;; output an openscad file
                  (close-model)
                  (write-scad-file
                   (list (model->scad-polyhedron model))
                   (current-output-port)))

                 (output-texture
                  ;; output a surface texure for the terrain
                  (write-surface-texture model width height output-x-size output-z-size height-function))

                 (output-caves
                  ;; output a negative openscad file to define caves
                  (close-model)
                  (write-caves-model  model width height output-x-size output-z-size height-function))

                 (else
                  ;; else complain
                  (usage "give only one of: --obj --pnm --scad --texture --caves")))))))))


    ))
