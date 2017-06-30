#
#
#

# these are related to outer_radius and inner_radius in clock.scad -- (* (- 0.5 0.05) 2)
CLOCK_FACE_TEXTURE_WIDTH=0.9
CLOCK_FACE_TEXTURE_HEIGHT=0.9


all: clock.obj


clock.stl: clock.scad
	openscad -o $@ $^

clock-untextured.obj: clock.stl
	wavefront-obj-tool -n $^ -o $@ -L clock.mtl -S clock_body_mtl

clock.obj: clock-untextured.obj
	wavefront-obj-tool -n -c $^ -o $@ \
		-T -M $(CLOCK_FACE_TEXTURE_WIDTH) $(CLOCK_FACE_TEXTURE_HEIGHT) \
		-L clock.mtl -S clock_face_mtl

upload: clock.obj clock.mtl clock-face.png
	mkdir -p clock
	cp $^ clock/
	rsync --delete -avP clock/ headache.hungry.com:public_html/hifi/clock/

clean:
	rm -f *~ clock.stl clock-untextured.obj clock.obj
	rm -rf clock
