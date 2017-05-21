#
#
#

# VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
N=-n

%.obj: %.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@


table-hull-parts/%.stl: table.scad
	mkdir -p table-hull-parts
	openscad -D output_table_hull=1 -D n="`basename -s .stl $@`" -o $@ $^

drawer-hull-parts/%.stl: table.scad
	mkdir -p drawer-hull-parts
	openscad -D output_drawer_hull=1 -D n="`basename -s .stl $@`" -o $@ $^

TARGETS=table.obj table-hull.obj table-drawer.obj table-drawer-hull.obj table.mtl build-table.js


TABLE_HULL_PARTS=$(foreach nth,0 1 2 3 4 5 6 7 8 9,table-hull-parts/${nth}.obj)
DRAWER_HULL_PARTS=$(foreach nth,0 1 2 3 4,drawer-hull-parts/${nth}.obj)


all: ${TARGETS}

#
# visual table model
#

table.stl: table.scad
	openscad -D output_table_visual=1 -o $@ $^

table.obj: table.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S table_wood_mtl

#
# visual drawer model
#

table-drawer.stl: table.scad
	openscad -D output_drawer_visual=1 -o $@ $^

table-drawer.obj: table-drawer.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S table_wood_mtl

# table-drawer-hull.obj: table-drawer.obj
# 	$(VHACD) -g -i $^ -o $@ --maxvertices 64 --concavity 0.001 --resolution 400000 --depth 30

table-drawer-hull.obj: ${DRAWER_HULL_PARTS}
	wavefront-obj-tool ${N} -c $^ -o $@


#
# collision hull made of convex parts
#

table-hull.obj: ${TABLE_HULL_PARTS}
	wavefront-obj-tool ${N} -c $^ -o $@

#
#
#

upload: ${TARGETS}
	mkdir -p table
	cp $^ table/
	rsync --delete -avP table/ headache.hungry.com:public_html/hifi/table/

clean:
	rm -f *~
	rm -f table.stl table.obj table-hull.obj
	rm -f table-drawer.stl table-drawer.obj table-drawer-hull.obj
	rm -rf table-hull-parts
	rm -rf drawer-hull-parts
	rm -rf table
