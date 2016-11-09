#
#
#


WAVEFRONT_OBJ_TOOL=../../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm


all: $(NAME).obj $(FRAME).mtl

$(FRAME)-edges.stl: ../$(FRAME)/$(FRAME).scad
	openscad -D width=$(WIDTH) -D height=$(HEIGHT) -o $@ $^

$(FRAME)-edges.obj: $(FRAME)-edges.stl
	${WAVEFRONT_OBJ_TOOL} -n -c $^ -o $@ -S frame_edges

$(FRAME).obj: $(FRAME)-edges.obj
	${WAVEFRONT_OBJ_TOOL} -n -L $(FRAME).mtl -c $^ -o $@

$(FRAME).mtl: ../$(FRAME)/$(FRAME).mtl
	(cp ../$(FRAME)/$(FRAME).mtl ./)

canvas.obj: ../$(FRAME)/canvas.obj.m4
	m4 -DHALFWIDTH=`echo $(WIDTH)/2 | bc -l` -DHALFHEIGHT=`echo $(HEIGHT)/2 | bc -l` $^ > $@

$(NAME).obj: $(FRAME).obj canvas.obj
	${WAVEFRONT_OBJ_TOOL} -n -c $^ -o $@

export: all
	mkdir -p ../export/$(NAME)/
	cp $(NAME).obj $(FRAME).mtl canvas.png ../export/$(NAME)/

clean:
	rm -f *~
	rm -f $(FRAME)-edges.stl $(FRAME)-edges.obj $(FRAME).obj
	rm -f $(NAME).obj $(FRAME).obj canvas.obj $(FRAME).mtl canvas.obj
