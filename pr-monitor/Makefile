#
#
#

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@

WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
OPENSCAD=openscad-nightly

UPLOADS=pr-utils.js pr-monitor.js pr-monitor-es.js status-page.js rez-status-page.js bezel.obj.gz bowl.obj.gz

all: bezel.obj.gz bowl.obj.gz

bezel.stl: bezel.scad
	$(OPENSCAD) -o $@ $^

bezel.obj: bezel.stl
	$(WAVEFRONT_OBJ_TOOL) -n $^ --color 0.2 0.6 0.2 -o $@


bowl.stl: bowl.scad
	$(OPENSCAD) -o $@ $^

bowl.obj: bowl.stl
	$(WAVEFRONT_OBJ_TOOL) -n $^ --color 1.0 0.6 0.0 -o $@


upload: $(UPLOADS)
	rm -rf pr-monitor
	mkdir -p pr-monitor
	cp $^ pr-monitor/
	rsync --delete -avP pr-monitor/ headache.hungry.com:public_html/hifi/pr-monitor/


s3: $(UPLOADS)
	s3cmd put pr-utils.js s3://hifi-content/seth/pr-monitor/pr-utils.js
	s3cmd put pr-monitor.js s3://hifi-content/seth/pr-monitor/pr-monitor.js
	s3cmd put pr-monitor-es.js s3://hifi-content/seth/pr-monitor/pr-monitor-es.js
	s3cmd put status-page.js s3://hifi-content/seth/pr-monitor/status-page.js
	s3cmd put bezel.obj.gz s3://hifi-content/seth/pr-monitor/bezel.obj.gz
	s3cmd put bowl.obj.gz s3://hifi-content/seth/pr-monitor/bowl.obj.gz
	s3cmd put rez-status-page.js s3://hifi-content/seth/pr-monitor/rez-status-page.js


clean:
	rm -f *~
	rm -rf pr-monitor
	rm -f bezel.obj bezel.stl
	rm -f bowl.obj bowl.stl
