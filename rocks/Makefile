#
#
#

rock-%.obj: rock-%-points.obj
	$(VHACD) -g -i $< -o $@



VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
LIBS='(foldling command-line)' '(seth obj-model)' '(srfi 27)'

all: rock-0.obj rock-1.obj rock-2.obj


rock-0-points.obj:
	./rocks-gauche.scm --random-i 50 --random-j 20 $@ 12

rock-1-points.obj:
	./rocks-gauche.scm --random-i 2 --random-j 8 $@ 12

rock-2-points.obj:
	./rocks-gauche.scm --random-i 12 --random-j 3 $@ 12



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
	rm -f *~
	rm -f rock-0-points.obj rock-0.obj
	rm -f rock-1-points.obj rock-1.obj
	rm -f rock-2-points.obj rock-2.obj

very-clean: clean
	rm -rf seth snow srfi foldling
