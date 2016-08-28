#
#
#

all: bow_collision_hull.obj

bow_collision_hull.obj:
	./bow-hull.scm

upload: bow_collision_hull.obj bow.js
	mkdir -p export
	cp $^ export/
	rsync -avP --delete export/ headache.hungry.com:public_html/hifi/bow/

clean:
	rm -f *~
	rm -f bow_collision_hull.obj
	rm -rf export/
