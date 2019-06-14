#
#

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@


OVEN=../../hifi/build/tools/oven/oven
BAKEDDIR=baked
BAKEDOUTPUT=${BAKEDDIR}/lamp-post/baked

all: ${BAKEDOUTPUT}

${BAKEDOUTPUT}: lamp-post.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

upload: lamp-post.obj.gz Lamp_Post_export.mtl Diffuse_Map.jpg Specular_map.jpg lamp.js
	rm -rf lamp
	mkdir -p lamp
	rsync -avP ${BAKEDOUTPUT}/ lamp/
	cp $^ lamp/
	rsync --delete -avP lamp/ headache.hungry.com:public_html/hifi/lamp/


clean:
	rm -f *~
	rm -rf lamp
	rm -rf ${BAKEDDIR}
