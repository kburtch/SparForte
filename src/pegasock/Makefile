# A simple makefile
all:
	cc -c -O2 c_tinyserve.c
	cc -c -O2 c_os.c
	gnatmake -c -I../ -gnatfoN tester.adb
	gnatbind -x -I../ tester.ali
	gnatlink tester.ali c_os.o c_tinyserve.o -I../

test:
	cc -c -O2 c_tinyserve.c
	cc -c -O2 c_os.c
	gnatmake -c -I../ -gnatfao tester.adb
	gnatbind -x -I../ tester.ali
	gnatlink tester.ali c_os.o c_tinyserve.o
	./tester
	rm test.txt

speedtest:
	cc -c -O2 c_tinyserve.c
	cc -c -O2 c_os.c
	gnatmake -c -I../ -gnatfoN speedtester.adb
	gnatbind -x -I../ speedtester.ali
	gnatlink speedtester.ali c_os.o c_tinyserve.o
	time ./speedtester

backofftest:
	cc -c -O2 c_tinyserve.c
	cc -c -O2 c_os.c
	gnatmake -c -I../ -gnatfoN backofftester.adb
	gnatbind -x -I../ backofftester.ali
	gnatlink backofftester.ali c_os.o c_tinyserve.o
	time ./backofftester

tinyservetest:
	cc -c -O2 c_tinyserve.c
	cc -c -O2 c_os.c
	gnatmake -c -I../ -gnatfoN tinyservetester.adb
	gnatbind -x -I../ tinyservetester.ali
	gnatlink tinyservetester.ali c_os.o c_tinyserve.o
	time ./tinyservetester

clean:
	rm *.o *.ali tester test.txt tinyservetester

