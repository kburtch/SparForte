# APQ Test Makefile
# $Id: Makefile,v 1.2 2005/02/11 02:59:46 ken Exp $
# Warren W. Gay VE3WWG
#
# Under the ACL (Ada Community License)
# See ../ACL.txt

CC=gcc
COPTS=-c -Wall
AOPTS=$(DBG) -gnato -gnata -gnatf

.c.o:
	$(CC) $(COPTS) $<

deflt:
	@echo
	@echo "If you have PostgreSQL installed do:"
	@echo "  1. make create_pg"
	@echo "  2. make check_pg"
	@echo "  3. make release_pg"
	@echo
	@echo "And/or if you have MySQL installed do:"
	@echo "  1. make create_my"
	@echo "  2. make check_my"
	@echo "  3. make release_my"
	@echo
	@echo "*** WARNING ***"
	@echo
	@echo "If you have databases named apq_eg that"
	@echo "you do not want dropped, then do not run"
	@echo "these tests!!!"
	@echo
	@echo "*** END WARNING ***"
	@echo

db:	getpwent getgrent

database:
	@echo "*** DROPPING PostgreSQL DATABASE apq_eg ***"
	@echo "Press RETURN to proceed:"
	@read reply
	-dropdb apq_eg >/dev/null 2>&1
	createdb apq_eg

db_mysql:
	@echo "*** DROPPING MySQL DATABASE apq_eg ***"
	@echo "Press RETURN to proceed:"
	@read reply
	-mysqladmin --force drop apq_eg
	mysqladmin create apq_eg

getpwent: getpwent.o
	$(CC) getpwent.o -o getpwent
	./getpwent >passwd

getgrent: getgrent.o
	$(CC) getgrent.o -o getgrent
	./getgrent >group

files: db
	./getpwent >passwd
	./getgrent >group

create_pg: files database create_pwdb_main
	./create_pwdb_main
	@echo "--"
	@echo "do 'make check_pg' to test the created database."
	@echo "do 'make release_pg' to release the test database."

check_pg: check_pwdb
	./check_pwdb
	@echo "--"
	@echo "do 'make release_pg' to release the test database."

create_my: files db_mysql create_mysql_prog
	./create_mysql
	@echo "--"
	@echo "do 'make check_my' to test the created database."
	@echo "do 'make release_my' to release the test database."

check_my: check_mysql_prog
	./check_mysql
	@echo "--"
	@echo "do 'make release_my' to release the test database."

release_pg:
	@echo "*** DROPPING PostgreSQL DATABASE apq_eg ***"
	@echo "Press RETURN to proceed:"
	@read reply
	dropdb apq_eg
	rm -f passwd group

release_my:
	@echo "*** DROPPING MySQL DATABASE apq_eg ***"
	@echo "Press RETURN to proceed:"
	@read reply
	-mysqladmin --force drop apq_eg

create_pwdb_main::
	gnatmake $(AOPTS) create_pwdb_main

check_pwdb::
	gnatmake $(AOPTS) check_pwdb

create_mysql_prog::
	gnatmake $(AOPTS) create_mysql

check_mysql_prog::
	gnatmake $(AOPTS) check_mysql

clean:
	rm -f *.ali b~*.ad[sb] *.o errs.t

clobber: clean
	rm -f create_pwdb_main check_pwdb getpwent passwd getgrent group
	rm -f create_mysql check_mysql

# End $Source: /home/cvsroot/bush/src/apq-2.1/eg2/Makefile,v $
