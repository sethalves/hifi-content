#
#
#

OVEN=../../hifi/build/tools/oven/oven
BAKEDDIR=baked

BAKEDOUTPUT=${BAKEDDIR}/hearthglen_townhall_nowall/baked
INPUT_FILE=hearthglen_townhall_nowall.obj
PUBLISHED_NAME=hearthglen-townhall

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
