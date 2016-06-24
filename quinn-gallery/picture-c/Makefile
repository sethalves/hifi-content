#
#
#

all: picture-c.obj frame-0.mtl

../frame-0/frame-0.obj ../frame-0/canvas.obj:
	(cd ../frame-0 && make)

frame-0.mtl: ../frame-0/frame-0.mtl
	(cp ../frame-0/frame-0.mtl ./)

picture-c.obj: ../frame-0/frame-0.obj ../frame-0/canvas.obj
	wavefront-obj-tool -n -L frame-0.mtl -c $^ -o $@

upload: all
	rsync -avP --exclude Makefile --exclude *~ ./ headache.hungry.com:public_html/hifi/quinn-gallery/picture-c/

clean:
	rm -f *~ picture-c.obj frame-0.obj canvas.obj frame-0.mtl
