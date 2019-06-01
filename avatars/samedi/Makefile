#
#
#

all:


upload:
	rm -rf samedi
	mkdir -p samedi
	cp export/avatar.fst samedi/
	cp export/samedi.fbx samedi/
	cp export/textures/* samedi/
	ln -s . samedi/textures
	rsync --delete -avP samedi/ headache.hungry.com:public_html/hifi/avatars/Samedi/


clean:
	rm -f *~
	rm -rf samedi
