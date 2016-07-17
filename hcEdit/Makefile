#
#
#

%.obj: %.stl
	wavefront-obj-tool -n $^ -o $@



all:


upload:
	scp \
		genericTool.js \
		horizontalTranslation.js \
		rotation.js \
		verticalTranslation.js \
		hangPicture.js \
	headache:public_html/hifi/hcEdit

unupload:
	scp headache:public_html/hifi/hcEdit/genericTool.js ./
	scp headache:public_html/hifi/hcEdit/horizontalTranslation.js ./
	scp headache:public_html/hifi/hcEdit/rotation.js ./
	scp headache:public_html/hifi/hcEdit/verticalTranslation.js ./
	scp headache:public_html/hifi/hcEdit/hangPicture.js ./
