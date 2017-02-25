#
#
#

all:

TEXTURES=\
textures/color-0-0-0.png \
textures/color-0-0-ff.png \
textures/color-0-ff-0.png \
textures/color-0-ff-ff.png \
textures/color-ff-0-0.png \
textures/color-ff-0-ff.png \
textures/color-ff-ff-0.png \
textures/color-ff-ff-ff.png


t: ${TEXTURES}


textures/color-%.png:
mkdir -p textures
ppmmake "rgb:`echo $@ | sed 's/textures.color-//' | sed 's/.png//' | sed 'sx-x/xg'`" 1 1 | /usr/bin/pnmtopng > $@

#	ppmmake rgb:0/0/0 1 1 |	/usr/bin/pnmtopng > $@


upload: voxel-paint-init.js voxel-paint-brush.js voxel-paint-eraser.js unitBoxTransparent.fbx voxel-paint-shared.js ${TEXTURES}
rm -rf voxel-paint
mkdir -p voxel-paint
cp voxel-paint-init.js voxel-paint-brush.js voxel-paint/
cp voxel-paint-eraser.js unitBoxTransparent.fbx voxel-paint-shared.js voxel-paint/
mkdir -p voxel-paint/textures
cp ${TEXTURES} voxel-paint/textures
rsync --delete -avP voxel-paint/ headache.hungry.com:public_html/hifi/voxel-paint-5/


clean:
rm -f *~
rm -rf voxel-paint
rm -rf textures
