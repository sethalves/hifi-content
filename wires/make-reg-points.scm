#! /bin/bash
#| -*- scheme -*-
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
SOURCE="$(readlink "$SOURCE")"
[[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
exec gosh \
-e '(set! *load-suffixes* (cons ".sld" *load-suffixes*))' \
-e "(append! *load-path* (list \"$DIR\" \".\"))" \
-r7 "$0" "$@"
|#


(import (scheme base)
        (scheme write)
        (srfi 29)
        (seth obj-model)
        (seth model-3d)
        (seth math-3d)
        (seth cout))

(cout "wireRegistrationPoints = [\n")
(cout "    { x: 0.5, y: 0.5, z: 0.5 },\n")

(let loop ((n 1))
  (cond ((<= n (+ 32 16 8 4 2 1))
         (let* ((model (make-empty-model))
                (filename (string-append "models/wires-" (number->string n) ".obj")))
           (read-obj-model-file filename model)
           (let* ((aa-box (model-aa-box model))
                  (dimensions (vector3-diff (aa-box-high-corner aa-box) (aa-box-low-corner aa-box)))
                  (center (aa-box-center aa-box))
                  (reg (vector
                        (+ (/ (- (vector3-x center)) (vector3-x dimensions)) 0.5)
                        (+ (/ (- (vector3-y center)) (vector3-y dimensions)) 0.5)
                        (+ (/ (- (vector3-z center)) (vector3-z dimensions)) 0.5))))
             (cout (format "    { x: ~a, y: ~a, z: ~a },\n" (vector3-x reg) (vector3-y reg) (vector3-z reg)))))
         (loop (+ n 1)))))

(cout "];\n\n")

(cout "module.exports = wireRegistrationPoints;\n")
