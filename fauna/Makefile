#
#
#

all:


upload: ray/manta.fbx ray/manta.jpg ray/manta_normal.jpg jellyfish/Jellyfish.gltf
	rm -rf fauna
	mkdir -p fauna
	cp $^ fauna/
	rsync --delete -avP fauna/ headache.hungry.com:public_html/hifi/fauna/


clean:
	rm -f *~
	rm -rf fauna
