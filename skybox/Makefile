#
#
#

# size of one segment.  the final image will be of size: (WIDTH * 4, HEIGTH * 3)
WIDTH=400
HEIGTH=400

%.pnm: %.png
	pngtopnm $^ > $@

%.png: %.pnm
	pnmtopng $^ > $@


###

all: skybox.png

clouds-with-shadows0.png clouds-with-shadows1.png clouds-with-shadows2.png clouds-with-shadows3.png clouds-with-shadows4.png clouds-with-shadows5.png: clouds-with-shadows.pov clouds-with-shadows.ini
	povray +W${WIDTH} +H${HEIGTH} clouds-with-shadows.ini

blank.pnm:
	pbmmake -black ${WIDTH} ${HEIGTH} > $@

row0.pnm: clouds-with-shadows0.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

row1.pnm: clouds-with-shadows1.pnm clouds-with-shadows2.pnm clouds-with-shadows3.pnm clouds-with-shadows4.pnm
	pnmcat -lr $^ > $@

row2.pnm: clouds-with-shadows5.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@


skybox.pnm: row0.pnm row1.pnm row2.pnm
	pnmcat -tb $^ > $@


###

clean:
	rm -f *~ *.pnm *.png

upload: skybox.png
	scp $^ headache:public_html/hifi/
