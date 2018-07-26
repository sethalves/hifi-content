#
#
#

all: bow_collision_hull.obj

upload: \
		arrow.fbx \
		arrow-sparkle.png \
		Arrow_impact1.L.wav \
		bow_collision_hull.obj \
		bow-deadly.fbx \
		Bow_draw.1.L.wav \
		bow.js \
		bow.json \
		newarrow_collision_hull.obj \
		newarrow_textured.fbx \
		notch.wav \
		String_release2.L.wav
	mkdir -p bow
	cp -a $^ bow/
	rsync -avP --delete bow/ headache.hungry.com:public_html/hifi/bow/

clean:
	rm -f *~
	rm -rf bow/
