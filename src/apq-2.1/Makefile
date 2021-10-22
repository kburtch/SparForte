# $Id: Makefile,v 1.2 2005/02/11 02:59:41 ken Exp $
# Warren W. Gay VE3WWG
#
# Licensed under the ACL (Ada Community License)
# or GPL2 :

DISTDIR=apq-$(VERSION)
DISTFILE=apq-$(VERSION).tar.gz

#DBG=-g
OPTZ=-O1

CC=gcc

ZOPTS=$(DBG) $(OPTZ)
COPTS=-Wall $(ZOPTS)
AOPTS=-gnata -gnatf -gnato -gnatwp $(ZOPTS)

.c.o:
	$(CC) -c $(COPTS) $(ZOPTS) $(PG_INCL) $(MY_INCL) $< -o $*.o

all:	setup$(HAVE_MY) mysql$(HAVE_MY) postgresql$(HAVE_PG) libapq.a finish

tmysql::
	rm -f tmysql mysql.trc
	gnatmake -gnatf tmysql -largs -lapq $(MY_LIBS)


setup0: # MySQL support is not being included
mysql0: # MySQL support is not being included
postgresql0: # PostgreSQL is not being included


setup1:	mysql_xcr mysql_xcr2 mysql_xcr3 mysql_incl
	chmod u+x mysql_xcr mysql_xcr2 mysql_xcr3 mysql_incl mysql_xty mysql_linker_options

mysql1: apq-mysql.ads c_mysql.o
	ADA_INCLUDE_PATH=. ADA_OBJECTS_PATH=. gnatmake -c $(AOPTS) comp_mysql

c_mysql.o: c_mysql.c
	$(CC) -c $(COPTS) $(ZOPTS) $(MY_INCL) c_mysql.c -o c_mysql.o

apq-mysql.ads: apq-mysql.ads-in mysql_generr.c mysql_xcr2 mysql_xcr3
	@rm -f mysql_errmsg.h mysql_generr
	./mysql_xcr >mysql_errmsg.h
	./mysql_xty >mysql_type_codes.h
	$(CC) $(COPTS) $(MY_INCL) mysql_generr.c -o mysql_generr
	$(CC) $(COPTS) $(MY_INCL) mysql_gentyp.c -o mysql_gentyp
	$(CC) $(COPTS) $(MY_INCL) mysqlopts.c -o mysql_opts
	./mysql_opts >opts.tmp
	./mysql_generr | sort -k1,1n | sed '/ER_ERROR_FIRST/d;/CR_UNKNOWN_ERROR/d;/ER_ERROR_LAST/d;/CR_ERROR_LAST/d;/CR_MARIADB_LAST_ERROR/d;/CR_MYSQL_LAST_ERROR/d'  | ./mysql_xcr2 >gen.tmp
	./mysql_gentyp | sort -k1,1n | ./mysql_xcr3
	#KB
	rm -f mysql_generr mysql_errmsg.h mysql_gentyp mysql_type_codes.h gen.tmp opts.tmp mysql_opts

postgresql1: numeric.o notices.o
	ADA_INCLUDE_PATH=. ADA_OBJECTS_PATH=. gnatmake -c $(AOPTS) comp_pg

libapq.a::
	@rm -f libapq.a
	ar cr libapq.a apq.o $(PG_OBJS) $(MY_OBJS)
	ar tv libapq.a

finish:
	@echo "--"
	@echo "Now do 'make install' to install the library"
	@echo
	@echo "NOTE: Remove any prior versions of the APQ library"
	@echo "and package, if you have it installed. You will"
	@echo "need to remove those packages and libraries"
	@echo "manually."
	@echo
	@echo "You may 'make uninstall' if you are about to"
	@echo "reinstall APQ version $(VERSION). This will not"
	@echo "work for prior versions of APQ however."
	@echo

install: libapq.a
	HAVE_PG=$(HAVE_PG) HAVE_MY=$(HAVE_MY) ./install_lib
	@echo "--"
	@echo "For fun do:"
	@echo "  1. cd ./eg2"
	@echo "  2. make"
	@echo "  3. follow instructions given."
	@echo "--"

uninstall:
	HAVE_PG=1 HAVE_MY=1 ./install_lib uninstall

reinstall: uninstall clobber all install

clean:
	rm -f *.o *.ali *.core b~*.ad[sb] errs.t libapq.a mysql.trc comp_pg comp_mysql

clobber: clean
	rm -f mysql_errmsg.h mysql_type_codes.h mysql_generr Makeincl tmysql apq-mysql.ads
	rm -f libapq.a GNUmakefile
	rm -fr ./release $(DISTFILE)
	cd ./eg; $(MAKE) clobber
	rm -f apq-1.93-win32-2.7.1.tar.gz

manual: apq-manual.pdf

apq-manual.pdf: binding2.ps
	rm -f apq-manual.ps apq-manual.pdf
	cp binding2.ps apq-manual.ps
	ps2pdf apq-manual.ps

distribution: manual
	./build_release
	@mv release $(DISTDIR)
	tar czf $(DISTFILE) $(DISTDIR)
	@rm -fr $(DISTDIR)
	@ls -l $(DISTFILE)

#binary-distributions:
#	./build_release build-win32-1.93-2.7.1
#	@mv release apq-1.93-win32-2.7.1
#	tar czf apq-1.93-win32-2.7.1.tar.gz apq-1.93-win32-2.7.1
#	rm -fr apq-1.93-win32-2.7.1
#	ls -l apq-1.93-win32-2.7.1.tar.gz

# End $Source: /home/cvsroot/bush/src/apq-2.1/Makefile,v $
