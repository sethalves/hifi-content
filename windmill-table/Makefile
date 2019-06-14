#
#
#

OVEN=../../hifi/build/tools/oven/oven
BAKEDDIR=baked

BAKEDOUTPUT=${BAKEDDIR}/table2_re-oriented/baked
INPUT_FILE=table2_re-oriented.fbx
PUBLISHED_NAME=windmill-table

#
#
#

all: ${BAKEDOUTPUT}

${BAKEDOUTPUT}: ${INPUT_FILE}
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

upload: ${BAKEDOUTPUT}
	rsync --delete -avP ${BAKEDOUTPUT}/ headache.hungry.com:public_html/hifi/${PUBLISHED_NAME}/

clean:
	rm -rf *~ ${BAKEDDIR}
