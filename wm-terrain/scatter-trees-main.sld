
(define-library (scatter-trees-main)
  (export main-program)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (scheme cxr)
          (srfi 1)
          (srfi 27)
          (srfi 29)
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
          (seth model-3d)
          (seth obj-model)
          (seth scad-model)
          )
  (begin

    (define (main-program)
      (cerr "loading...\n")
      (let* (
             (tree-count 800)
             ;; (tree-count 10)
             (tree-y-fudge (vector 0 -0.5 0)) ;; try to keep trunks in the ground on steep hills
             (water-level -26) ;; no trees below here
             (random-i 12) ;; arbitrary, done for repeatability
             (random-j 3) ;; same as i
             (random-source (make-random-source))
             (random-integer (random-source-make-integers random-source))
             ;;
             (terrain-model (make-empty-model))
             (terrain-model (read-obj-model-file "terrain-textured.obj" terrain-model))
             ;; (terrain-center (vector -416 0.49 -549.9))
             (terrain-center (vector -80 35 148))
             (tower-center (vector 0 0 0))
             (initial-bb (model-aa-box terrain-model))
             (dimensions (vector3-diff (aa-box-high-corner initial-bb) (aa-box-low-corner initial-bb)))
             (half-dimensions (vector3-scale dimensions 0.5))
             ;;
             (tree0-model (make-empty-model))
             (tree1-model (make-empty-model))
             (tree2-model (make-empty-model))
             (tree-models (vector tree0-model tree1-model tree2-model))
             ;;
             (x-slices 6)
             (z-slices 6)
             (slices-hash (make-hash-table))
             (slice-x-size (/ (vector3-x dimensions) x-slices))
             (slice-z-size (/ (vector3-z dimensions) z-slices))
             )
        (define (slice-indices->slice-key x z)
          (string-append (number->string x) "," (number->string z)))

        (define (position->slice-key pos)
          (let* ((pos-in-dimensions (vector3-sum (vector3-diff pos terrain-center) half-dimensions))
                 (ratio (vector (/ (vector3-x pos-in-dimensions) (vector3-x dimensions))
                                0
                                (/ (vector3-z pos-in-dimensions) (vector3-z dimensions)))))
            (slice-indices->slice-key (exact (floor (* (vector3-x ratio) x-slices)))
                                      (exact (floor (* (vector3-z ratio) z-slices))))))


        (random-source-pseudo-randomize! random-source random-i random-j)

        (cerr "dimensions=" dimensions "\n")


        (cerr "loading trees...\n")

        (read-obj-model-file "tree0.obj" tree0-model)
        (read-obj-model-file "tree1.obj" tree1-model)
        (read-obj-model-file "tree2.obj" tree2-model)

        (cerr "done loading.\n")

        (do ((x-slice 0 (+ x-slice 1)))
            ((= x-slice x-slices) #t)
          (do ((z-slice 0 (+ z-slice 1)))
              ((= z-slice z-slices) #t)
            ;; (cerr "setting hash table: " (slice-indices->slice-key x-slice z-slice) "\n")
            (hash-table-set! slices-hash (slice-indices->slice-key x-slice z-slice) (make-empty-model))))


        ;; slide the terrain-model into hifi world-space
        ;; (translate-model terrain-model (vector3-diff terrain-center (vector3-scale dimensions 0.5)))
        (translate-model terrain-model terrain-center)

        (let* ((aa-box (model-aa-box terrain-model))
               (octree (model->octree terrain-model aa-box)))

          (cerr "aa-box=" (aa-box-low-corner aa-box) " " (aa-box-high-corner aa-box) "\n")
          (cerr "half-dimensions=" half-dimensions "\n")

          (let loop ((count 0))
            (cond ((>= count tree-count) #t)
                  (else
                   (let* ((xrnd-on-mdl (- (random-integer (exact (round (vector3-x dimensions))))
                                          (vector3-x half-dimensions)))
                          (zrnd-on-mdl (- (random-integer (exact (round (vector3-z dimensions))))
                                          (vector3-z half-dimensions)))
                          (p (vector3-sum (vector xrnd-on-mdl 0 zrnd-on-mdl) terrain-center))
                          (distance-from-tower (vector3-length (vector3-diff p tower-center))))
                     (cond
                      ((< distance-from-tower 100)
                       ;; don't put random trees near tower
                       (loop count))
                      (else
                       (let* ((ray0 (vector (vector3-x p)
                                            (vector3-y (aa-box-high-corner aa-box))
                                            (vector3-z p)))
                              (ray1 (vector (vector3-x p)
                                            (vector3-y (aa-box-low-corner aa-box))
                                            (vector3-z p)))
                              (face (ray-cast terrain-model octree (vector ray0 ray1))))
                         (cond (face
                                (let* ((root-point (vector3-sum (segment-triangle-intersection
                                                                 (vector ray0 ray1)
                                                                 (face->vertices terrain-model face))
                                                                tree-y-fudge)))
                                  (cond ((> (vector3-y (vector3-diff root-point tree-y-fudge)) water-level)
                                         ;; this is a good place for a tree.  generate the rest of its parameters
                                         (let ((r (random-integer 360)) ;; rotation
                                               (w (random-integer 3))
                                               (s (/ (+ (random-integer 15) 5) 10.0)))
                                           (cout (vector3-x root-point) " "
                                                 (vector3-z root-point) " "
                                                 (vector3-y root-point) " "
                                                 (exact (round r)) " "
                                                 s " "
                                                 (exact (+ (round w) 3)) "\n")

                                           (model-append! (hash-table-ref slices-hash (position->slice-key root-point))
                                                          (vector-ref tree-models w)
                                                          (lambda (v)
                                                            (let* ((scale-m (matrix-scaling s))
                                                                   (rot-q (euler->quaternion
                                                                           (vector 0 (degrees->radians r) 0)))
                                                                   (rot-m (quaternion->matrix rot-q))
                                                                   (trans-m (matrix-translation root-point))
                                                                   (m (matrix-A*B trans-m (matrix-A*B rot-m scale-m))))
                                                              (vertex-transform m v))))


                                           (loop (+ count 1))))
                                        (else
                                         (loop count)))))
                               (else
                                (cerr "no face at " p "\n")
                                (loop count))))))
                     ))))


          (let* ((rez-script-filename "rez-combined-trees.js")
                 (rez-script-port (open-output-file rez-script-filename)))
            (do ((x-slice 0 (+ x-slice 1)))
                ((= x-slice x-slices) #t)
              (do ((z-slice 0 (+ z-slice 1)))
                  ((= z-slice z-slices) #t)
                (let* ((output-filename (string-append "combined-trees-"
                                                       (number->string x-slice) "-"
                                                       (number->string z-slice) ".obj"))
                       (output-port (open-output-file output-filename))
                       (output-model (hash-table-ref slices-hash (slice-indices->slice-key x-slice z-slice)))
                       (output-model-bb (model-aa-box output-model))
                       (output-model-center (if output-model-bb
                                                (aa-box-center output-model-bb)
                                                (vector 0 0 0)))
                       (output-model-dimensions
                        (if output-model-bb
                            (vector3-diff (aa-box-high-corner output-model-bb)
                                          (aa-box-low-corner output-model-bb))
                            (vector 1 1 1))))
                  (cerr "writing " output-filename " : "
                        (if output-model-bb output-model-center #f)
                        "\n")
                  (if output-model-bb
                      (cout (string-append
                             (format "Entities.addEntity({\n")
                             (format "    name: \"combined trees ~a ~a\",\n" x-slice z-slice)
                             (format "    type: \"Model\",\n")
                             (format "    modelURL: \"http://headache.hungry.com/~~seth/hifi/wm-terrain/~a.gz\",\n"
                                     output-filename)
                             (format "    position: { x: ~a, y: ~a, z: ~a },\n"
                                     (vector3-x output-model-center)
                                     (vector3-y output-model-center)
                                     (vector3-z output-model-center))
                             (format "    dimensions: { x: ~a, y: ~a, z: ~a },\n"
                                     (vector3-x output-model-dimensions)
                                     (vector3-y output-model-dimensions)
                                     (vector3-z output-model-dimensions))
                             (format "    lifetime: 120,\n")
                             (format "    collisionless: true\n")
                             (format "});\n\n"))
                            rez-script-port))

                  (write-obj-model output-model output-port)
                  (close-output-port output-port))))
            (close-output-port rez-script-port)))))
    ))
