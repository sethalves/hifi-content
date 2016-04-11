
all: dropper.obj

dropper.stl: dropper.scad
	openscad -o $@ $^

dropper.obj: dropper.stl
	wavefront-obj-tool -n -c $^ -o $@ -L dropper.mtl -S dropper_mtl

clean:
	rm -f *~ dropper.stl dropper.obj

upload: dropper.obj dropper.mtl
	scp $^ headache:public_html/hifi/
