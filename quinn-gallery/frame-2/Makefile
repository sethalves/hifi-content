#
#
#

all: frame-2.obj

frame-2-edges.stl: frame-2.scad
	openscad -D edges=1 -D canvas=0 -o $@ $^

frame-2-edges.obj: frame-2-edges.stl
	wavefront-obj-tool -n -c $^ -o $@ -S frame_edges

frame-2.obj: frame-2-edges.obj
	wavefront-obj-tool -n -L frame-2.mtl -c $^ -o $@

clean:
	rm -f *~ frame-2-canvas.obj frame-2-canvas.stl frame-2-edges.obj frame-2-edges.stl frame-2.obj
