#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
LIBS='(foldling command-line)' '(seth obj-model)' '(seth scad-model)' '(srfi 27)'

all:



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
	mkdir -p marlam-ssv-to-entities
	cp $^ marlam-ssv-to-entities/
	rsync --delete -avP marlam-ssv-to-entities/ headache.hungry.com:public_html/hifi/marlam-ssv-to-entities/


clean:
	rm -f *~


very-clean: clean
	rm -rf seth snow srfi foldling
