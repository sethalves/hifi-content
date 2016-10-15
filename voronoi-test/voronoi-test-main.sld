(define-library (voronoi-test-main)
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
          (foldling command-line)
          (seth cout)
          (seth strings)
          (seth math-3d)
          (seth raster)
          (seth image)
          (seth pbm)
          (seth graph)
          )
  (begin

    (define epsilon 0.0001)

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
                   (loop (cons (list p0 p1) edges)
                         (cdr sorted-edge-points))))))))


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


    (define (point->key point)
      (string-append (number->string (vector2-x point)) "/" (number->string (vector2-y point))))

    (define (extract-faces in-lines)
      (let* ((graph (make-graph))
             (points (points->unique-points (lines->points in-lines)))
             (lines (tug-line-ends-to-points in-lines points))
             (nodes (map (lambda (point) (make-node graph point)) points))
             (nodes-hash (make-hash-table)))

        ;; create a hash-table to go from points to graph nodes
        (for-each
         (lambda (node)
           (let ((node-point (node-value node)))
             (hash-table-set! nodes-hash (point->key node-point) node)))
         nodes)

        ;; make a graph edge for each line
        (for-each
         (lambda (line)
           (let ((node-a (hash-table-ref nodes-hash (point->key (car line))))
                 (node-b (hash-table-ref nodes-hash (point->key (cadr line)))))
             (connect-nodes graph node-a node-b)))
         lines)

        #t))


    (define (main-program)
      ;; XXX read width and height from command-line
      (define width 100)
      (define height 100)

      (let loop ((lines '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              (let* ((edge-lines (discover-edges lines width height))
                     (lines-and-edges (append lines edge-lines)))
                (cout "---\n" (current-error-port))
                (show-lines lines-and-edges width height)
                (extract-faces lines-and-edges))
              (let* ((line-port (open-input-string line))
                     (x0 (read line-port))
                     (y0 (read line-port))
                     (x1 (read line-port))
                     (y1 (read line-port)))
                (loop (cons (list (vector x0 y0) (vector x1 y1))
                            lines)))))))

    ))
