#
#
#

all: frame-1.obj

frame-1-edges.stl: frame-1.scad
	openscad -D edges=1 -D canvas=0 -o $@ $^

frame-1-edges.obj: frame-1-edges.stl
	wavefront-obj-tool -n -c $^ -o $@ -S frame_edges

frame-1.obj: frame-1-edges.obj
	wavefront-obj-tool -n -L frame-1.mtl -c $^ -o $@

clean:
	rm -f *~ frame-1-canvas.obj frame-1-canvas.stl frame-1-edges.obj frame-1-edges.stl frame-1.obj
