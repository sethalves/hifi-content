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


upload: ${TEXTURES} voxel-paint-tool.js voxel-paint-palette.js voxel-paint-shared.js textures/grass.png textures/wood.png \
	textures/dirt.png brushSet.json models/painter_Palette2.fbx models/arrows.fbx models/palette.fbx \
	models/painter_Palette.fbx models/painter_Palette_L.fbx models/studioFloor.fbx models/smallBrush.fbx \
	models/smallpaintRag.fbx models/largepaintRag.fbx models/unitBoxTransparent.fbx models/largeBrush.fbx \
	models/medpaintRag.fbx models/medBrush.fbx voxel-paint-init.js voxel-paint-aether.js handyAttacher.js
	rm -rf voxel-paint
	mkdir -p voxel-paint
	cp brushSet.json handyAttacher.js voxel-paint-aether.js voxel-paint-init.js voxel-paint/
	cp voxel-paint-palette.js voxel-paint-shared.js voxel-paint-tool.js voxel-paint/
	mkdir -p voxel-paint/models
	cp models/*.fbx voxel-paint/models/
	mkdir -p voxel-paint/textures
	cp textures/*.png voxel-paint/textures/
	rsync --delete -avP voxel-paint/ headache.hungry.com:public_html/hifi/voxel-paint-6/


clean:
	rm -f *~
	rm -rf voxel-paint
	rm -f textures/color-*png
