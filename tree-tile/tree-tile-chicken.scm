#! /bin/sh
#| -*- scheme -*-
exec csi -include-path /usr/local/share/scheme -s $0 "$@"
|#

(use r7rs)

(include "srfi/60.sld")
(include "snow/assert.sld")
(include "snow/bytevector.sld")
(include "snow/bignum.sld")
(include "snow/input-parse.sld")
(include "snow/binio.sld")
(include "snow/random.sld")
(include "seth/cout.sld")
(include "seth/raster.sld")
(include "seth/image.sld")
(include "seth/pbm.sld")
(include "seth/math-3d.sld")
(include "seth/strings.sld")
(include "seth/graph.sld")
(include "seth/octree.sld")
(include "seth/model-3d.sld")
(include "seth/obj-model.sld")
(include "seth/scad-model.sld")
(include "foldling/command-line.sld")
(include "tree-tile-main.sld")

(import (scheme base)
        (tree-tile-main))
(main-program)
