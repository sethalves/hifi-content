#
#
#

ATP_CLIENT=../../hifi/build/tools/atp-client/atp-client
WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
SCHEME_LIBS='(seth cout)' '(srfi 60)'

%.obj: %.stl
	$(WAVEFRONT_OBJ_TOOL) -c $^ -o $@


WIRE_MODELS=$(foreach nth,1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63,models/wires-${nth}.obj)


all: $(WIRE_MODELS)


libs:
	snow2 install $(SCHEME_LIBS)


include wires.make

wires.make: make-makefile.scm
	./make-makefile.scm > wires.make


# upload-atp:
# 	- $(ATP_CLIENT) -T $$I atp://eschatology/wires/$I


upload: $(WIRE_MODELS) wires-shared.js add-wire.js wire-add-tool.json
	rm -rf wires
	mkdir -p wires/models
	cp $(WIRE_MODELS) wires/models/
	cp	wires-shared.js add-wire.js wire-add-tool.json wires/
	rsync --delete -avP wires/ headache.hungry.com:public_html/hifi/wires/


clean:
	rm -f *~
	rm -rf models

very-clean: clean
	rm -rf seth srfi wires.make
