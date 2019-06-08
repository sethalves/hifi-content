#
#
#


OVEN=../../hifi/build/tools/oven/oven
BAKEDDIR=baked
BAKEDOUTPUT=${BAKEDDIR}/hearthglen_townhall_nowall/baked

FILES=\
${BAKEDOUTPUT}/hearthglen_townhall_nowall.baked.fbx \
${BAKEDOUTPUT}/hearthglen_townhall_nowall.baked.fst \
${BAKEDOUTPUT}/hearthglen_townhall_nowall.baked.json


all: ${FILES}

${FILES}: hearthglen_townhall_nowall.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

upload: ${FILES}
	rsync --delete -avP ${BAKEDOUTPUT}/ headache.hungry.com:public_html/hifi/hearthglen-townhall/

clean:
	rm -rf *~ ${BAKEDDIR}
