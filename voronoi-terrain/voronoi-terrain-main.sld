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

    (define (show-lines lines width height)
      (let ((image (raster-new width height (vector 255 255 255 255))))
        (for-each
         (lambda (line)
           (cout line "\n" (current-error-port))
           (let ((x0 (exact (round (vector-ref (car line) 0))))
                 (y0 (exact (round (vector-ref (car line) 1))))
                 (x1 (exact (round (vector-ref (cadr line) 0))))
                 (y1 (exact (round (vector-ref (cadr line) 1)))))
             (image-fat-line! image (vector 0 0 0 255) x0 y0 x1 y1 rgba)
             ))
         lines)

        (image->ppm image (current-output-port))
        ))


    (define (point-x-< p0 p1)
      (if (= (vector2-x p0) (vector2-x p1))
          (< (vector2-y p0) (vector2-y p1))
          (< (vector2-x p0) (vector2-x p1))))

    (define (point-x-> p0 p1)
      (if (= (vector2-x p0) (vector2-x p1))
          (> (vector2-y p0) (vector2-y p1))
          (> (vector2-x p0) (vector2-x p1))))

    (define (point-y-< p0 p1)
      (if (= (vector2-y p0) (vector2-y p1))
          (< (vector2-x p0) (vector2-x p1))
          (< (vector2-y p0) (vector2-y p1))))

    (define (point-y-> p0 p1)
      (if (= (vector2-y p0) (vector2-y p1))
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


    (define (discover-edges lines width height)
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
             (top-points-sorted (sort top-points point-x->))
             ;; put them all in a loop
             (sorted-edge-points (append left-points-sorted
                                         bottom-points-sorted
                                         right-points-sorted
                                         top-points-sorted
                                         ;; and close loop
                                         (list (car left-points-sorted)))))

        ;; (cout "all: " points "\n" (current-error-port))
        ;; (cout "left: " left-points-sorted "\n" (current-error-port))
        ;; (cout "top: " top-points-sorted "\n" (current-error-port))
        ;; (cout "right: " right-points-sorted "\n" (current-error-port))
        ;; (cout "bottom: " bottom-points-sorted "\n" (current-error-port))
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


    (define (tug-line-ends-to-points in-lines points)
      (map
       (lambda (line)
         (let loop ((points points)
                    (line line))
           (cond ((null? points) line)
                 (else
                  (let ((point (car points)))
                    (cond ((vector2-almost-equal? (car line) point epsilon)
                           (loop (cdr points) (list point (cadr line))))
                          ((vector2-almost-equal? (cadr line) point epsilon)
                           (loop (cdr points) (list (car line) point)))
                          (else
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
            face-edge
            (let* ((bc-edge (car b-edges))
                   (node-c (edge-other-node bc-edge node-b)))
              (if (eq? node-a node-c)
                  ;; don't go backwards
                  (loop (cdr b-edges) leftest-angle face-edge)
                  ;; see if this edge is best
                  (let* ((angle-ab (edge-angle node-a node-b))
                         (angle-bc (edge-angle node-b node-c))
                         (angle-change (- angle-bc angle-ab))
                         (b-rest (cdr b-edges)))
                    (cond ((> angle-change pi)
                           ;; a spin-too-far right turn
                           (loop b-rest leftest-angle face-edge))
                          ((< angle-change 0)
                           ;; a right turn
                           (loop b-rest leftest-angle face-edge))
                          ((> angle-change leftest-angle)
                           ;; best so far
                           (loop b-rest angle-bc bc-edge))
                          (else
                           (loop b-rest leftest-angle face-edge)))))))))


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
                 (if (not (model-contains-face model face))
                     (mesh-append-face! model mesh face)))
                (else
                 (let* ((center-vertex (face->center-vertex model face))
                        (center-vertex-s (vector-map number->string center-vertex))
                        (center-index
                         (model-append-vertex! model center-vertex-s))
                        (center-corner (make-face-corner
                                        center-index 'unset 'unset))
                        (last-corner (last corners)))
                   (let loop ((corners (cons last-corner corners)))
                     (cond ((null? corners) #t)
                           ((null? (cdr corners)) #t)
                           (else
                            (let* ((corner-b (car corners))
                                   (corner-c (cadr corners))
                                   (subface (make-face model (vector center-corner
                                                                     corner-b
                                                                     corner-c)
                                                       material)))
                              (if (not (model-contains-face model subface))
                                  (mesh-append-face! model mesh subface))
                              (loop (cdr corners))))))))))))

    (define (face-search model mesh start-node path-nodes node-a node-b)
      (cond
       ;; ((eq? (voronoi-graph-data-status (node-value node-b)) 'searched) #t)
       ((eq? start-node node-b)
        (add-faces model mesh (reverse path-nodes)))
       (else
        (let ((face-edge (find-face-edge node-a node-b)))
          (cond (face-edge
                 (face-search model mesh
                              start-node
                              (cons node-b path-nodes)
                              node-b
                              (edge-other-node face-edge node-b)))
                (else #t))))))


    (define (graph->model graph height-function)
      (let ((model (make-empty-model))
            (mesh (make-mesh #f '())))
        (model-prepend-mesh! model mesh)

        ;; fill in the indexes of the nodes
        (for-each
         (lambda (node)
           (let* ((data (node-value node))
                  (point (voronoi-graph-data-point data))
                  (point-3d (vector (number->string (vector2-x point))
                                    (number->string (vector2-y point))
                                    (number->string (height-function point))))
                  (index (model-append-vertex! model point-3d)))
             (voronoi-graph-data-set-index! data index)))
         (graph-nodes graph))

        ;; search for faces
        (let loop ((nodes (graph-nodes graph)))
          (cond ((null? nodes) #t)
                (else
                 (let* ((node (car nodes))
                        (data (node-value node)))
                   (for-each
                    (lambda (edge)
                      (face-search model mesh
                                   node (list node) node
                                   (edge-other-node edge node)))
                    (node-edges node))
                   (voronoi-graph-data-set-status! data 'searched)
                   (loop (cdr nodes))))))
        (operate-on-faces model (lambda (mesh face)
                                  (face-set-normals! model face)
                                  face))
        model))



    (define (line-segments->graph in-lines)
      (let* ((graph (make-graph))
             (points (points->unique-points (lines->points in-lines)))
             (lines (tug-line-ends-to-points in-lines points))
             (nodes (map
                     (lambda (point)
                       (let ((data (make-voronoi-graph-data point #f #f)))
                         (make-node graph data)))
                     points))
             (nodes-hash (make-hash-table)))

        ;; create a hash-table to go from points to graph nodes
        (for-each
         (lambda (node)
           (let ((point (voronoi-graph-data-point (node-value node))))
             (hash-table-set! nodes-hash point node)))
         nodes)

        ;; make a graph edge for each line
        (for-each
         (lambda (line)
           (let ((node-a (hash-table-ref nodes-hash (car line)))
                 (node-b (hash-table-ref nodes-hash (cadr line))))
             (connect-nodes graph node-a node-b)))
         lines)

        graph))


    (define (main-program)
      (define (usage why)
        (cout why "\n" (current-error-port))
        (cout "voronoi-terrain [arguments]" (current-error-port))
        (cout "    --obj         output an obj file\n" (current-error-port))
        (cout "    --pnm         output a pnm file\n" (current-error-port))
        (cout "    --width w     width of output\n" (current-error-port))
        (cout "    --height h    height of output\n" (current-error-port))
        (exit 1))


      (let* ((args (parse-command-line `((--obj)
                                         (--pnm)
                                         ((--width) width)
                                         ((--height) height)
                                         (-?) (-h))))
             (output-obj #f)
             (output-pnm #f)
             (width #f)
             (height #f)
             (extra-arguments '())
             (height-function (lambda (point)
                                1.0
                                ;; (/ (vector2-x point) (+ (vector2-y point) 1))
                                ))
             )
        (for-each
         (lambda (arg)
           (case (car arg)
             ((-? -h) (usage ""))
             ((--obj)
              (if (or output-obj output-pnm) (usage "give only one of --obj or --pnm"))
              (set! output-obj #t))
             ((--pnm)
              (if (or output-obj output-pnm) (usage "give only one of --obj or --pnm"))
              (set! output-pnm #t))
             ((--width)
              (set! width (string->number (cadr arg))))
             ((--height)
              (set! height (string->number (cadr arg))))
             ((--)
              (set! extra-arguments (cdr arg)))))
         args)

        ;; TODO -- set width and height from command-line
        (if (not width) (set! width 100))
        (if (not height) (set! height 100))

        (cout "width=" width " height=" height "\n" (current-error-port))

        (let loop ((lines '()))
          (let ((line (read-line)))
            (if (eof-object? line)
                (let* ((edge-lines (discover-edges lines width height))
                       (lines-and-edges (append lines edge-lines))
                       (graph (line-segments->graph lines-and-edges))
                       (model (graph->model graph height-function)))
                  ;; (cout "---\n" (current-error-port))
                  ;; (cout "edge lines: " edge-lines "\n" (current-error-port))
                  ;; (cout "---\n" (current-error-port))
                  (cond
                   (output-obj (write-obj-model model (current-output-port)))
                   (output-pnm (show-lines lines-and-edges width height))
                   (else (usage "give one of --obj or --pnm"))))
                (let* ((line-port (open-input-string line))
                       (x0 (read line-port))
                       (y0 (read line-port))
                       (x1 (read line-port))
                       (y1 (read line-port)))
                  (loop (cons (list (vector x0 y0) (vector x1 y1))
                              lines))))))))

    ))
