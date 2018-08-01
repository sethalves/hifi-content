#
#
#

ATP_CLIENT=../../hifi/build/tools/atp-client/atp-client
WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
SCHEME_LIBS='(srfi 60)' '(seth cout)' '(seth obj-model)' '(srfi 29)'

%.obj: %.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c $^ -o $@


WIRE_MODELS=$(foreach nth,1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63,models/wires-${nth}.obj)

COMPONENT_MODELS=models/battery.obj models/or-gate.obj models/and-gate.obj


all: $(WIRE_MODELS) $(COMPONENT_MODELS)


libs:
	snow2 install $(SCHEME_LIBS)


include wires.make

wires.make: make-makefile.scm
	./make-makefile.scm > wires.make


models/battery.stl: battery.scad common.scad
	mkdir -p models
	openscad -o $@ $<


models/or-gate.stl: or-gate.scad common.scad
	mkdir -p models
	openscad -o $@ $<


models/and-gate.stl: and-gate.scad common.scad
	mkdir -p models
	openscad -o $@ $<


wire-reg-points.js: make-reg-points.scm $(WIRE_MODELS)
	./make-reg-points.scm > $@


# upload-atp:
# 	- $(ATP_CLIENT) -T $$I atp://eschatology/wires/$I


SCRIPTS=wires-shared.js wire-reg-points.js add-wire.js wires-controller.js

upload: $(WIRE_MODELS) $(COMPONENT_MODELS) $(SCRIPTS) wire-add-tool.json
	rm -rf wires
	mkdir -p wires/models
	cp $(WIRE_MODELS) $(COMPONENT_MODELS) wires/models/
	cp	$(SCRIPTS) wire-add-tool.json wires/
	rsync --delete -avP wires/ headache.hungry.com:public_html/hifi/wires/


clean:
	rm -f *~
	rm -rf models
	rm -rf wires

very-clean: clean
	rm -rf seth srfi snow wires.make wire-reg-points.js
