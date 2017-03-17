#
#
#

all:

upload: voxel-to-mesh-test.js voxel-to-mesh-test.svg
	mkdir -p voxel-to-mesh-test
	cp $^ voxel-to-mesh-test/
	rsync -avP voxel-to-mesh-test/ headache.hungry.com:public_html/hifi/voxel-to-mesh-test/

clean:
	rm -f *~
	rm -rf voxel-to-mesh-test
