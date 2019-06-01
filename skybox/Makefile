#
#
#

# size of one segment.  the final image will be of size: (WIDTH * 4, HEIGTH * 3)
# WIDTH=900
# HEIGTH=900

WIDTH=2048
HEIGTH=2048

OVEN=../../hifi/build/tools/oven/oven

BAKEDDIR=baked

%.pnm: %.png
	pngtopnm $^ > $@

%.png: %.pnm
	pnmtopng $^ > $@


DAY_RESULTS=\
  ${BAKEDDIR}/trees-skybox.png \
  ${BAKEDDIR}/trees-skybox_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT.ktx \
  ${BAKEDDIR}/trees-skybox_COMPRESSED_SRGB8_ETC2.ktx \
  ${BAKEDDIR}/trees-skybox.ktx \
  ${BAKEDDIR}/trees-skybox.texmeta.json

TWILIGHT_RESULTS=\
  ${BAKEDDIR}/trees-skybox-twilight.png \
  ${BAKEDDIR}/trees-skybox-twilight_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT.ktx \
  ${BAKEDDIR}/trees-skybox-twilight_COMPRESSED_SRGB8_ETC2.ktx \
  ${BAKEDDIR}/trees-skybox-twilight.ktx \
  ${BAKEDDIR}/trees-skybox-twilight.texmeta.json

NIGHT_RESULTS=\
  ${BAKEDDIR}/trees-skybox-night.png \
  ${BAKEDDIR}/trees-skybox-night_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT.ktx \
  ${BAKEDDIR}/trees-skybox-night_COMPRESSED_SRGB8_ETC2.ktx \
  ${BAKEDDIR}/trees-skybox-night.ktx \
  ${BAKEDDIR}/trees-skybox-night.texmeta.json

###

all: ${DAY_RESULTS} ${TWILIGHT_RESULTS} ${NIGHT_RESULTS} grass.png

blank.pnm:
	pbmmake -black ${WIDTH} ${HEIGTH} > $@


trees.pov: make-trees-pov.sh
	./make-trees-pov.sh $@


### day

trees-day.pov: skybox-header.pov trees.pov
	cat $^ > $@

trees-day0.png trees-day1.png trees-day2.png trees-day3.png trees-day4.png trees-day5.png: trees.ini trees-day.pov
	povray +W${WIDTH} +H${HEIGTH} $<

trees-row0.pnm: trees-day0.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-row1.pnm: trees-day1.pnm trees-day2.pnm trees-day3.pnm trees-day4.pnm
	pnmcat -lr $^ > $@

trees-row2.pnm: trees-day5.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-skybox.pnm: trees-row0.pnm trees-row1.pnm trees-row2.pnm
	pnmcat -tb $^ > $@

grass.png: trees-day5.png
	cp $^ $@

${DAY_RESULTS}: trees-skybox.png
	mkdir -p ${BAKEDDIR}
	${OVEN} -i trees-skybox.png -o ${BAKEDDIR} -t skybox

### twilight

trees-twilight.pov: skybox-twilight-header.pov trees.pov
	cat $^ > $@

trees-twilight0.png trees-twilight1.png trees-twilight2.png trees-twilight3.png trees-twilight4.png trees-twilight5.png: \
	trees-twilight.ini trees-twilight.pov
	povray +W${WIDTH} +H${HEIGTH} $<

trees-twilight-row0.pnm: trees-twilight0.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-twilight-row1.pnm: trees-twilight1.pnm trees-twilight2.pnm trees-twilight3.pnm trees-twilight4.pnm
	pnmcat -lr $^ > $@

trees-twilight-row2.pnm: trees-twilight5.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-skybox-twilight.pnm: trees-twilight-row0.pnm trees-twilight-row1.pnm trees-twilight-row2.pnm
	pnmcat -tb $^ > $@

${TWILIGHT_RESULTS}: trees-skybox-twilight.png
	mkdir -p ${BAKEDDIR}
	${OVEN} -i trees-skybox-twilight.png -o ${BAKEDDIR} -t skybox


### night

trees-night.pov: skybox-night-header.pov trees.pov
	cat $^ > $@

trees-night0.png trees-night1.png trees-night2.png trees-night3.png trees-night4.png trees-night5.png: \
	trees-night.ini trees-night.pov
	povray +W${WIDTH} +H${HEIGTH} $<

trees-night-row0.pnm: trees-night0.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-night-row1.pnm: trees-night1.pnm trees-night2.pnm trees-night3.pnm trees-night4.pnm
	pnmcat -lr $^ > $@

trees-night-row2.pnm: trees-night5.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-skybox-night.pnm: trees-night-row0.pnm trees-night-row1.pnm trees-night-row2.pnm
	pnmcat -tb $^ > $@

${NIGHT_RESULTS}: trees-skybox-night.png
	mkdir -p ${BAKEDDIR}
	${OVEN} -i trees-skybox-night.png -o ${BAKEDDIR} -t skybox


###

clean:
	rm -f *~ *.pnm *.png
	rm -f tree.pov trees.pov trees-day.pov trees-twilight.pov trees-night.pov
	rm -f *.pov-state
	rm -rf ${BAKEDDIR}
	rm -rf skybox

# upload: trees-skybox.png trees-skybox-twilight.png trees-skybox-night.png grass.png cycle-sky.js
# 	scp $^ headache:public_html/hifi/

upload: ${DAY_RESULTS} ${TWILIGHT_RESULTS} ${NIGHT_RESULTS} grass.png cycle-sky.js
	mkdir -p skybox
	cp $^ skybox/
	rsync --delete -avP skybox/ headache.hungry.com:public_html/hifi/skybox/
