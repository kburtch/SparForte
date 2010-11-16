# APQ Test Makefile
# $Id: Makefile,v 1.2 2005/02/11 02:59:46 ken Exp $
# Warren W. Gay VE3WWG
#
# Under the ACL (Ada Community License)
# See ../ACL.txt

#DBG=-g
CC=gcc
COPTS=-c -Wall
AOPTS=$(DBG) -gnato -gnata -gnatf

.c.o:
	$(CC) $(COPTS) $<

all:	create_pwdb_main check_pwdb
	@echo "--"
	@echo "do 'make create' to create the database."
	@echo "do 'make check' to test the created database."
	@echo "do 'make release' to release the test database."
	@echo
	@echo "*** WARNING ***"
	@echo
	@echo "If you have databases named apq_eg that"
	@echo "you do not want dropped, then do not run"
	@echo "these tests!!!"
	@echo
	@echo "*** END WARNING ***"
	@echo

db:	getpwent getgrent database

database:
	@echo "*** DROPPING PostgreSQL DATABASE apq_eg ***"
	@echo "Press RETURN to proceed:"
	@read reply
	-dropdb apq_eg >/dev/null 2>&1
	createdb apq_eg

getpwent: getpwent.o
	$(CC) getpwent.o -o getpwent
	./getpwent >passwd

getgrent: getgrent.o
	$(CC) getgrent.o -o getgrent
	./getgrent >group

create:	db create_pwdb_main
	./getpwent >passwd
	./getgrent >group
	./create_pwdb_main
	@echo "--"
	@echo "do 'make check' to test the created database."
	@echo "do 'make release' to release the test database."

check:	check_pwdb
	./check_pwdb
	@echo "--"
	@echo "do 'make release' to release the test database."

release:
	@echo "*** DROPPING PostgreSQL DATABASE apq_eg ***"
	@echo "Press RETURN to proceed:"
	@read reply
	dropdb apq_eg
	rm -f passwd group

create_pwdb_main::
	gnatmake $(AOPTS) create_pwdb_main

check_pwdb::
	gnatmake $(AOPTS) check_pwdb

clean:
	rm -f *.ali b~*.ad[sb] *.o errs.t

clobber: clean
	rm -f create_pwdb_main check_pwdb getpwent passwd getgrent group

# End $Source: /home/cvsroot/bush/src/apq-2.1/eg/Makefile,v $
