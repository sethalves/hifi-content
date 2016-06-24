
all: $(NAME).obj $(FRAME).mtl

../$(FRAME)/$(FRAME).obj ../$(FRAME)/canvas.obj:
	(cd ../$(FRAME) && make)

$(FRAME).mtl: ../$(FRAME)/$(FRAME).mtl
	(cp ../$(FRAME)/$(FRAME).mtl ./)

$(NAME).obj: ../$(FRAME)/$(FRAME).obj ../$(FRAME)/canvas.obj
	wavefront-obj-tool -n -L $(FRAME).mtl -c $^ -o $@

upload: all
	rsync -avP --exclude Makefile --exclude *~ ./ headache.hungry.com:public_html/hifi/quinn-gallery/$(NAME)/

clean:
	rm -f *~ $(NAME).obj $(FRAME).obj canvas.obj $(FRAME).mtl
