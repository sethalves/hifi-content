#
#
#

all:


upload: pr-monitor.js pr-monitor-es.js pr-utils.js
	rm -rf pr-monitor
	mkdir -p pr-monitor
	cp $^ pr-monitor/
	rsync --delete -avP pr-monitor/ headache.hungry.com:public_html/hifi/pr-monitor/


clean:
	rm -f *~
	rm -rf pr-monitor
