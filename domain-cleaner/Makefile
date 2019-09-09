#
#
#

FILES=\
    domainCleaner.app.json \
    appData/domain-cleaner.js \
	appData/domain-cleaner.svg \
	appData/ui/domain-cleaner.html \
	appData/ui/css/domainCleaner_ui.css \
	appData/ui/images/loadingSpinner.svg \
	appData/ui/js/domainCleaner_ui.js



all:

upload: $(FILES)
	rm -rf domain-cleaner
	mkdir -p domain-cleaner/appData/ui/css
	mkdir -p domain-cleaner/appData/ui/images
	mkdir -p domain-cleaner/appData/ui/js
	cp domainCleaner.app.json domain-cleaner/domainCleaner.app.json
	cp appData/domain-cleaner.js domain-cleaner/appData/domain-cleaner.js
	cp appData/domain-cleaner.svg domain-cleaner/appData/domain-cleaner.svg
	cp appData/ui/domain-cleaner.html domain-cleaner/appData/ui/domain-cleaner.html
	cp appData/ui/css/domainCleaner_ui.css domain-cleaner/appData/ui/css/domainCleaner_ui.css
	cp appData/ui/images/loadingSpinner.svg domain-cleaner/appData/ui/images/loadingSpinner.svg
	cp appData/ui/js/domainCleaner_ui.js domain-cleaner/appData/ui/js/domainCleaner_ui.js
	rsync --delete -avP domain-cleaner/ seth@headache.hungry.com:public_html/hifi/domain-cleaner/

clean:
	rm -f *~
	rm -rf domain-cleaner
