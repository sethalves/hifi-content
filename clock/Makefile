#
#
#

# these are related to outer_radius and inner_radius in clock.scad -- (* (- 0.5 0.05) 2)
CLOCK_FACE_TEXTURE_WIDTH=0.9
CLOCK_FACE_TEXTURE_HEIGHT=0.9
INNER_RADIUS=0.45
HALF_DEPTH=0.04


all: clock.obj


clock.stl: clock.scad
	openscad -o $@ $^

clock-untextured.obj: clock.stl
	wavefront-obj-tool -n $^ -o $@ -L clock.mtl -S clock_body_mtl

clock-offset.obj: clock-untextured.obj
	wavefront-obj-tool -n -c $^ -o $@ \
		-T -M $(CLOCK_FACE_TEXTURE_WIDTH) $(CLOCK_FACE_TEXTURE_HEIGHT) \
		-L clock.mtl -S clock_face_mtl

clock.obj: clock-offset.obj
	wavefront-obj-tool -n -c $^ -o $@ -t -$(INNER_RADIUS) $(HALF_DEPTH) -$(INNER_RADIUS)

upload: clock.obj clock.mtl clock-face.png
	mkdir -p clock
	cp $^ clock/
	rsync --delete -avP clock/ headache.hungry.com:public_html/hifi/clock/

clean:
	rm -f *~ clock.stl clock-untextured.obj clock-offset.obj clock.obj
	rm -rf clock
