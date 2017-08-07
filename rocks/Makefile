#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

rock-%-no-txtr.obj: rock-%-points.obj
	- $(VHACD) -g -i $< -o $@

rock-%.obj: rock-%-no-txtr.obj
	wavefront-obj-tool -m 0.8 -S rock_mtl -L rock.mtl $< -o $@


VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
LIBS='(foldling command-line)' '(seth obj-model)' '(seth scad-model)' '(srfi 27)'
OUTPUTS=rock-0.obj rock-1.obj rock-2.obj rock-3.obj crystal-0.obj crystal-1.obj compound-crystal-0.obj

all: $(OUTPUTS)


rock-0-points.obj:
	./rocks-gauche.scm --random-i 50 --random-j 20 $@ 12

rock-1-points.obj:
	./rocks-gauche.scm --random-i 2 --random-j 8 $@ 12

rock-2-points.obj:
	./rocks-gauche.scm --sphere --random-i 12 --random-j 3 $@ 200

rock-3-points.obj:
	./rocks-gauche.scm --size 10 --random-i 200 --random-j 20 $@ 20

crystal-0.obj:
	./rocks-gauche.scm --crystal --size 10 $@ 6

crystal-0.scad:
	./rocks-gauche.scm --scad --crystal --size 10 $@ 6

crystal-1.obj:
	./rocks-gauche.scm --crystal --size 8 $@ 6

crystal-1.scad:
	./rocks-gauche.scm --scad --crystal --size 8 $@ 6

compound-crystal-0.stl: compound-crystal-0.scad crystal-0.scad crystal-1.scad
	openscad -o $@ $<

compound-crystal-0.obj: compound-crystal-0.stl
	$(WAVEFRONT_OBJ_TOOL) -c -n $< -o $@

libs:
	snow2 install $(LIBS)

link-libs: very-clean
	snow2 -s \
		-p 'http://foldling.org/snow2/index.scm' \
		-p '../../snow2-packages/seth' \
		-p '../../snow2-packages/snow' \
		-p '../../seth-snow2-misc' \
		install $(LIBS)


upload: $(OUTPUTS) rock.jpg rock.mtl
	mkdir -p rocks
	cp $^ rocks/
	rsync --delete -avP rocks/ headache.hungry.com:public_html/hifi/rocks/


clean:
	rm -f *~
	rm -rf rocks
	rm -f rock-0-points.obj rock-0-no-txtr.obj rock-0.obj
	rm -f rock-1-points.obj rock-1-no-txtr.obj rock-1.obj
	rm -f rock-2-points.obj rock-2-no-txtr.obj rock-2.obj
	rm -f rock-3-points.obj rock-3-no-txtr.obj rock-3.obj
	rm -f crystal-0.obj crystal-0.scad
	rm -f crystal-1.obj crystal-1.scad
	rm -f compound-crystal-0.stl compound-crystal-0.obj

very-clean: clean
	rm -rf seth snow srfi foldling
