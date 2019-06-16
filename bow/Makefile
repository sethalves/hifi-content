#
#
#

OVEN=../../hifi/build/tools/oven/oven
BAKEDDIR=baked


all: ${BAKEDDIR}/bow-deadly/baked ${BAKEDDIR}/arrow/baked

${BAKEDDIR}/bow-deadly/baked: bow-deadly.fbx
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

${BAKEDDIR}/arrow/baked: arrow.fbx
	${OVEN} -i $^ -o ${BAKEDDIR} -t model


UNBAKED_FILES= \
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

upload: ${UNBAKED_FILES} ${BAKEDDIR}/bow-deadly/baked ${BAKEDDIR}/arrow/baked
	mkdir -p bow
	rsync -avP ${BAKEDDIR}/bow-deadly/baked/ bow/
	rsync -avP ${BAKEDDIR}/arrow/baked/ bow/
	cp -a ${UNBAKED_FILES} bow/
	rsync -avP --delete bow/ headache.hungry.com:public_html/hifi/bow/

clean:
	rm -f *~
	rm -rf bow/
	rm -rf ${BAKEDDIR}
