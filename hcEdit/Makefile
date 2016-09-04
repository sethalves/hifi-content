#
#
#

EXPORT_FILES=genericTool.js toolMaterials.mtl \
	horizontalTranslation.js \
	rotation.js \
	verticalTranslation.js \
	hangPicture.js hangPictureTool.obj hangPictureIcon.png

###

all: hangPictureTool.obj

base-tool.stl: base-tool.scad
	openscad -o $@ $^

base-tool.obj: base-tool.stl
	wavefront-obj-tool -n $^ -o $@ -S toolHandleMaterial


###

hangPicture-icon.obj: tool-icon.obj.m4
	m4 -DMTL=hangPictureIconMaterial $^ > $@

hangPicture-icon-positioned.obj: hangPicture-icon.obj
	wavefront-obj-tool -t 0 0.03 0 $^ > $@

hangPictureTool.obj: base-tool.obj hangPicture-icon-positioned.obj
	wavefront-obj-tool -n -L toolMaterials.mtl -c $^ -o $@

###

upload: all
	rm -rf export
	mkdir export
	cp -a $(EXPORT_FILES) export/
	rsync -avP export/ headache.hungry.com:public_html/hifi/hcEdit/

clean:
	rm -f *~ base-tool.stl base-tool.obj
	rm -f hangPicture-icon.obj hangPicture-icon-positioned.obj hangPictureTool.obj
