ATP_CLIENT=../../hifi/build/tools/atp-client/atp-client
WAVEFRONT_OBJ_TOOL=wavefront-obj-tool


all: controller-tests-table.obj

controller-tests-table.stl: controller-tests-table.scad
	openscad -o $@ $^

controller-tests-table.obj: controller-tests-table.stl
	$(WAVEFRONT_OBJ_TOOL) -c -C 0.01 -n $^ -o $@ --color 0.54 0.27 0.07

controller-tests-table.obj.gz: controller-tests-table.obj
	rm -f $@
	gzip -9 $<

upload: controller-tests-table.obj.gz setup-controller-tests.js color-cube.obj
	mkdir -p controller-tests
	cp $^ controller-tests/
	rsync --delete -avP controller-tests/ headache.hungry.com:public_html/hifi/controller-tests/

clean:
	rm -f *~
	rm -f controller-tests-table.obj controller-tests-table.obj.gz controller-tests-table.stl
	rm -rf controller-tests
