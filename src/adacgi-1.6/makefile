# This makefile automates generates the CGI demo programs and
# compressed files.

VERSION=1.6

# By default, compile all of the demo programs:

all: demo minimal search test_cookie test_send test_encode


demo: FORCE
	gnatmake -q demo.adb

minimal: FORCE
	gnatmake -q minimal.adb

search: FORCE
	gnatmake -q search.adb

test_cookie: FORCE
	gnatmake -q test_cookie.adb

test_send: FORCE
	gnatmake -q test_send.adb

test_encode: FORCE
	gnatmake -q test_encode.adb

# "FORCE" just forces the depending rule to always be run.
# In particular, we always want to invoke gnatmake, which then checks to see if
# anything needs recompiling.

FORCE:


clean:
	/bin/rm -f demo getdemo minimal search test_get test_cookie \
                   test_encode test_send *.o *.ali *.BAK


# "make distribute" makes the files for re-distribution.
# Note that the "distribute" rule explicitly requires GNU tar, since
# GNU tar has the "--name-prefix" option.  See the "Software Release Practice
# HOWTO" at http://www.linuxdoc.org if you need to eliminate GNU dependencies.

# Switch to this (need to anyway for zip files):
#        @ls $(SRC) | sed s:^:foobar-$(VERS)/: >MANIFEST
#        @(cd ..; ln -s foobar foobar-$(VERS))
#        (cd ..; tar -czvf foobar/foobar-$(VERS).tar.gz `cat foobar/MANIFEST`)
#        @(cd ..; rm foobar-$(VERS))



distribute: clean
	# weblint -e bad-link -e upper-case -e mailto-link -x netscape *.html
	echo "Note: only distributing filenames matching [a-zA-Z]*"
	chmod ugo+r [a-zA-Z]*
	chmod g-w [a-zA-Z]*
	rm -f adacgi-*.zip adacgi-*.tar.gz *.BAK *.rpm
	# tar --name-prefix='adacgi-$(VERSION)/' -cvf - [a-zA-Z]* > adacgi.tar
	cd ..; \
	   tar -cvf - adacgi-$(VERSION)/[a-zA-Z]* > adacgi-$(VERSION).tar ; \
	   gzip --best adacgi-$(VERSION).tar
	cd ..; zip -9 adacgi-$(VERSION).zip adacgi-$(VERSION)/[a-zA-Z]*
	mv ../adacgi-$(VERSION).tar.gz .
	mv ../adacgi-$(VERSION).zip .
	chmod ugo+r *.zip *.tar.gz
	chmod g-w   *.zip *.tar.gz
	# uuencode cgi.tar.gz cgi.tar.gz > cgi.tar.gz.uu
	# uuencode cgi.zip cgi.zip > cgi.zip.uu
	# more readme *.html *.htm *.ads *.adb makefile > cgi.email
	echo "If all has gone well, run 'make RPM' as root."

RPM:
	cp adacgi-$(VERSION).tar.gz /usr/src/redhat/SOURCES
	cp adacgi.spec /usr/src/redhat/SPECS
	cd /usr/src/redhat/SPECS; rpm -ba adacgi.spec
	cp /usr/src/redhat/RPMS/i386/adacgi-*.i386.rpm /usr/src/redhat/SRPMS/adacgi-*.src.rpm .
	chown --reference=. *.rpm
	chmod g-w *

# i386 version goes into /usr/src/redhat/RPMS/i386/adacgi-1.4-1.i386.rpm
# Source RPM in SRPMS.
# /usr/doc/adacgi-1.4  (and its sample subdirectory)
# /usr/lib/gnat/adainclude gets the files cgi.ad{b,s} (source)
# /usr/lib/gnat/adalib/cgi.o   is the resulting binary

