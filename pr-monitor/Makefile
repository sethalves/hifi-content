#
#
#

all:


upload: pr-monitor.js pr-monitor-es.js pr-utils.js
	rm -rf pr-monitor
	mkdir -p pr-monitor
	cp $^ pr-monitor/
	rsync --delete -avP pr-monitor/ headache.hungry.com:public_html/hifi/pr-monitor/


s3: pr-monitor.js pr-monitor-es.js pr-utils.js
	s3cmd put pr-monitor.js s3://hifi-content/seth/pr-monitor/pr-monitor.js
	s3cmd put pr-monitor-es.js s3://hifi-content/seth/pr-monitor/pr-monitor-es.js
	s3cmd put pr-utils.js s3://hifi-content/seth/pr-monitor/pr-utils.js

clean:
	rm -f *~
	rm -rf pr-monitor
