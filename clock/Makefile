#
#
#

ATP_CLIENT=../../hifi/build/tools/atp-client/atp-client
WAVEFRONT_OBJ_TOOL=wavefront-obj-tool


# these are related to outer_radius and inner_radius in clock.scad -- (* (- 0.5 0.05) 2)
CLOCK_FACE_TEXTURE_WIDTH=0.9
CLOCK_FACE_TEXTURE_HEIGHT=0.9
INNER_RADIUS=0.45
HALF_DEPTH=0.04

LIBS='(foldling command-line)' '(seth obj-model)' '(seth scad-model)' '(srfi 27)'


all: clock.obj hour-hand.obj minute-hand.obj clock-face.png


clock.stl: clock.scad
	openscad -o $@ $^

clock-untextured.obj: clock.stl
	$(WAVEFRONT_OBJ_TOOL) -n $^ -o $@ -L clock.mtl -S clock_body_mtl

clock-offset.obj: clock-untextured.obj
	$(WAVEFRONT_OBJ_TOOL) $^ -o $@ \
		-U -M $(CLOCK_FACE_TEXTURE_WIDTH) $(CLOCK_FACE_TEXTURE_HEIGHT) \
		-L clock.mtl -S clock_face_mtl

clock.obj: clock-offset.obj
	$(WAVEFRONT_OBJ_TOOL) -n -c $^ -o $@ -t -$(INNER_RADIUS) $(HALF_DEPTH) -$(INNER_RADIUS)

hour-hand.scad: make-clock-hand-gauche.scm make-clock-hand.sld
	./$< --length 0.25 > $@

hour-hand.stl: hour-hand.scad
	openscad -o $@ $^

hour-hand.obj: hour-hand.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c $^ -o $@

# $(WAVEFRONT_OBJ_TOOL) --hull -n -c $^ -o $@

minute-hand.scad: make-clock-hand-gauche.scm make-clock-hand.sld
	./$< --length 0.4 > $@

minute-hand.stl: minute-hand.scad
	openscad -o $@ $^

minute-hand.obj: minute-hand.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c $^ -o $@

clock-face-blank.png:
	pbmmake -white 480 480 | pnmtopng > $@

clock-face.png: clock-face-blank.png
	convert -pointsize 40 -fill black \
		-draw 'text 320,94 "1"' \
		-draw 'text 386,160 "2"' \
		-draw 'text 410,250 "3"' \
		-draw 'text 386,340 "4"' \
		-draw 'text 320,406 "5"' \
		-draw 'text 230,430 "6"' \
		-draw 'text 140,406 "7"' \
		-draw 'text 74,340 "8"' \
		-draw 'text 50,250 "9"' \
		-draw 'text 54,160 "10"' \
		-draw 'text 120,94 "11"' \
		-draw 'text 210,70 "12"' \
		$< $@

clock.obj.gz: clock.obj
	rm -f $@
	gzip -9 $<

hour-hand.obj.gz: hour-hand.obj
	rm -f $@
	gzip -9 $<

minute-hand.obj.gz: minute-hand.obj
	rm -f $@
	gzip -9 $<


upload-atp: clock.obj.gz clock.mtl clock-face.png hour-hand.obj.gz minute-hand.obj.gz clock.js
	- $(ATP_CLIENT) -T clock.obj.gz atp://eschatology/clock/clock.obj.gz
	- $(ATP_CLIENT) -T clock.mtl atp://eschatology/clock/clock.mtl
	- $(ATP_CLIENT) -T clock-face.png atp://eschatology/clock/clock-face.png
	- $(ATP_CLIENT) -T hour-hand.obj.gz atp://eschatology/clock/hour-hand.obj.gz
	- $(ATP_CLIENT) -T minute-hand.obj.gz atp://eschatology/clock/minute-hand.obj.gz
	- $(ATP_CLIENT) -T clock.js atp://eschatology/clock/clock.js

upload: clock.obj.gz clock.mtl clock-face.png rez-clock.js hour-hand.obj.gz minute-hand.obj.gz clock.js
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
	rm -f clock.obj.gz hour-hand.obj.gz minute-hand.obj.gz
	rm -f hour-hand.stl hour-hand.obj
	rm -f minute-hand.stl minute-hand.obj
	rm -rf clock
	rm -f clock-face-blank.png clock-face.png
	rm -f clock.md clock.html

very-clean: clean
	rm -rf seth foldling srfi snow
