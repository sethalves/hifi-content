#
#
#

all:


upload: voxel-paint-init.js voxel-paint-brush.js voxel-paint-eraser.js unitBoxTransparent.fbx
	rm -rf voxel-paint
	mkdir -p voxel-paint
	cp $^ voxel-paint/
	rsync --delete -avP voxel-paint/ headache.hungry.com:public_html/hifi/voxel-paint-2/


clean:
	rm -f *~
	rm -rf voxel-paint
