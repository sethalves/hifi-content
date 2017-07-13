#
#
#

WAVEFRONT_OBJ_TOOL=wavefront-obj-tool

all: openscad-export-handedness-test.stl openscad-export-handedness-test.obj


openscad-export-handedness-test.stl: openscad-export-handedness-test.scad
	openscad -o $@ $^


openscad-export-handedness-test.obj: openscad-export-handedness-test.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c $^ -o $@

clean:
	rm -f *~ openscad-export-handedness-test.stl openscad-export-handedness-test.obj
