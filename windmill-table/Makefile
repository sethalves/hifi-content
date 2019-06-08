#
#
#


OVEN=../../hifi/build/tools/oven/oven
BAKEDDIR=baked
BAKEDOUTPUT=${BAKEDDIR}/table2_re-oriented/baked

FILES=\
${BAKEDOUTPUT}/table2_re-oriented.baked.fbx \
${BAKEDOUTPUT}/table2_re-oriented.baked.fst \
${BAKEDOUTPUT}/table2_re-oriented.baked.json

all: ${FILES}

${FILES}: table2_re-oriented.fbx
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

upload: ${FILES}
	rsync --delete -avP ${BAKEDOUTPUT}/ headache.hungry.com:public_html/hifi/windmill-table/

clean:
	rm -rf *~ ${BAKEDDIR}
