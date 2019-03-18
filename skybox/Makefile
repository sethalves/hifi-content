#
#
#

# size of one segment.  the final image will be of size: (WIDTH * 4, HEIGTH * 3)
WIDTH=900
HEIGTH=900

%.pnm: %.png
	pngtopnm $^ > $@

%.png: %.pnm
	pnmtopng $^ > $@


###

all: trees-skybox.png trees-skybox-twilight.png # trees-skybox-night.png

blank.pnm:
	pbmmake -black ${WIDTH} ${HEIGTH} > $@

### day

trees.pov: skybox-header.pov make-trees-pov.sh
	./make-trees-pov.sh $< $@

trees0.png trees1.png trees2.png trees3.png trees4.png trees5.png: trees.pov trees.ini
	povray +W${WIDTH} +H${HEIGTH} trees.ini

trees-row0.pnm: trees0.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-row1.pnm: trees1.pnm trees2.pnm trees3.pnm trees4.pnm
	pnmcat -lr $^ > $@

trees-row2.pnm: trees5.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-skybox.pnm: trees-row0.pnm trees-row1.pnm trees-row2.pnm
	pnmcat -tb $^ > $@

grass.png: trees5.png
	cp $^ $@


### twilight

trees-twilight.pov: skybox-twilight-header.pov make-trees-pov.sh
	./make-trees-pov.sh $< $@

trees-twilight0.png trees-twilight1.png trees-twilight2.png trees-twilight3.png trees-twilight4.png trees-twilight5.png: \
	trees-twilight.pov trees-twilight.ini
	povray +W${WIDTH} +H${HEIGTH} trees-twilight.ini

trees-twilight-row0.pnm: trees-twilight0.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-twilight-row1.pnm: trees-twilight1.pnm trees-twilight2.pnm trees-twilight3.pnm trees-twilight4.pnm
	pnmcat -lr $^ > $@

trees-twilight-row2.pnm: trees-twilight5.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-skybox-twilight.pnm: trees-twilight-row0.pnm trees-twilight-row1.pnm trees-twilight-row2.pnm
	pnmcat -tb $^ > $@


###

clean:
	rm -f *~ *.pnm *.png
	rm -f tree.pov trees.pov trees-twilight.pov
	rm -f *.pov-state

upload: trees-skybox.png grass.png # skybox.png
	scp $^ headache:public_html/hifi/
