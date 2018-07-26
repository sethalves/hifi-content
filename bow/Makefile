#
#
#

all: bow_collision_hull.obj

upload: bow_collision_hull.obj bow.js bow-deadly.fbx
	mkdir -p bow
	cp -a $^ bow/
	rsync -avP --delete bow/ headache.hungry.com:public_html/hifi/bow/

clean:
	rm -f *~
	rm -rf bow/
