
all: gen_constants c_bdb.o bdb_constants.ads db

gen_constants:
	gcc -DBDB4 -o gen_constants -Wall -fstack-protector -O1 -D_FORTIFY_SOURCE=2 gen_constants.c

c_bdb.o: c_bdb.c
	gcc -O3 -DBDB4 -Wall -fstack-protector -O1 -D_FORTIFY_SOURCE=2 -c c_bdb.c

bdb_constants.ads: gen_constants
	./gen_constants > bdb_constants.ads

db: db.adb
	gnatmake -c -gnatVaep -gnateEeAeEeF db.adb
	gnatbind -x db.ali
	gnatlink db.ali -ldb c_bdb.o

clean:
	rm -f *.o *.ali db bdb_constants.ads gen_constants
	rm -f __db* txn/__db* nbs

distclean: clean
	rm  -f *~ *.tmp *.bak

