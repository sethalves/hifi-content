#
#
#

# these are related to outer_radius and inner_radius in clock.scad -- (* (- 0.5 0.05) 2)
CLOCK_FACE_TEXTURE_WIDTH=0.9
CLOCK_FACE_TEXTURE_HEIGHT=0.9
INNER_RADIUS=0.45
HALF_DEPTH=0.04

LIBS='(foldling command-line)' '(seth obj-model)' '(seth scad-model)' '(srfi 27)'


all: clock.obj hour-hand.obj minute-hand.obj


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

hour-hand.scad: make-clock-hand-gauche.scm make-clock-hand.sld
	./$< --length 0.25 > $@

hour-hand.stl: hour-hand.scad
	openscad -o $@ $^

hour-hand.obj: hour-hand.stl
	wavefront-obj-tool -n -c $^ -o $@

minute-hand.scad: make-clock-hand-gauche.scm make-clock-hand.sld
	./$< --length 0.4 > $@

minute-hand.stl: minute-hand.scad
	openscad -o $@ $^

minute-hand.obj: minute-hand.stl
	wavefront-obj-tool -n -c $^ -o $@

upload: clock.obj clock.mtl clock-face.png rez-clock.js hour-hand.obj minute-hand.obj clock.js
	mkdir -p clock
	cp $^ clock/
	rsync --delete -avP clock/ headache.hungry.com:public_html/hifi/clock/

libs:
	snow2 install $(LIBS)

link-libs: very-clean
	snow2 -s \
		-p 'http://foldling.org/snow2/index.scm' \
		-p '../../snow2-packages/seth' \
		-p '../../snow2-packages/snow' \
		-p '../../seth-snow2-misc' \
		install $(LIBS)


clean:
	rm -f *~ clock.stl clock-untextured.obj clock-offset.obj clock.obj
	rm -f hour-hand.scad hour-hand.stl hour-hand.obj
	rm -f minute-hand.scad minute-hand.stl minute-hand.obj
	rm -rf clock

very-clean: clean
	rm -rf seth foldling srfi snow
