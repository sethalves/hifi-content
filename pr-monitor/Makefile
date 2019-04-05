#
#
#

WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
OPENSCAD=openscad-nightly

all: bezel.obj

bezel.stl: bezel.scad
	$(OPENSCAD) -o $@ $^

bezel.obj: bezel.stl
	$(WAVEFRONT_OBJ_TOOL) -n $^ --color 0.2 0.3 0.2 -o $@


upload: pr-utils.js pr-monitor.js pr-monitor-es.js status-page.js bezel.obj rez-status-page.js
	rm -rf pr-monitor
	mkdir -p pr-monitor
	cp $^ pr-monitor/
	rsync --delete -avP pr-monitor/ headache.hungry.com:public_html/hifi/pr-monitor/


s3: pr-monitor.js pr-monitor-es.js pr-utils.js
	s3cmd put pr-utils.js s3://hifi-content/seth/pr-monitor/pr-utils.js
	s3cmd put pr-monitor.js s3://hifi-content/seth/pr-monitor/pr-monitor.js
	s3cmd put pr-monitor-es.js s3://hifi-content/seth/pr-monitor/pr-monitor-es.js
	s3cmd put status-page.js s3://hifi-content/seth/pr-monitor/status-page.js
	s3cmd put bezel.obj s3://hifi-content/seth/pr-monitor/bezel.obj
	s3cmd put rez-status-page.js s3://hifi-content/seth/pr-monitor/rez-status-page.js


clean:
	rm -f *~
	rm -rf pr-monitor
	rm -f bezel.obj bezel.stl
