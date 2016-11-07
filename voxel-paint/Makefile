#
#
#

all:


upload: voxel-paint-init.js
	rm -rf voxel-paint
	mkdir -p voxel-paint
	cp $^ voxel-paint/
	rsync --delete -avP voxel-paint/ headache.hungry.com:public_html/hifi/voxel-paint/


clean:
	rm -f *~
	rm -rf voxel-paint
