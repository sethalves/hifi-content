#
#
#

all: frame-0.obj

frame-0-edges.stl: frame-0.scad
	openscad -D edges=1 -D canvas=0 -o $@ $^

frame-0-edges.obj: frame-0-edges.stl
	wavefront-obj-tool -n -c $^ -o $@ -S frame_edges

frame-0.obj: frame-0-edges.obj
	wavefront-obj-tool -n -L frame-0.mtl -c $^ -o $@

clean:
	rm -f *~ frame-0-canvas.obj frame-0-canvas.stl frame-0-edges.obj frame-0-edges.stl frame-0.obj
