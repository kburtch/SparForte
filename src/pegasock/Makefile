# A simple makefile
all:
	cc -c -O2 -Wall -Werror -fstack-protector -D_FORTIFY_SOURCE=2 -c c_tinyserve.c
	cc -c -O2 -Wall -fstack-protector -D_FORTIFY_SOURCE=2 c_errno.c
	gnatmake -c -I../ -gnatfoN  -gnatVaep -gnateEeEeF tester.adb
	gnatbind -x -I../ tester.ali
	gnatlink tester.ali c_errno.o c_tinyserve.o -I../

test:
	cc -c -O2 -Wall -Werror -fstack-protector -D_FORTIFY_SOURCE=2 -c c_tinyserve.c
	cc -c -O2 -Wall -Werror -fstack-protector -D_FORTIFY_SOURCE=2 c_errno.c
	gnatmake -c -I../ -gnatfao  -gnatVaep -gnateEeEeF tester.adb
	gnatbind -x -I../ tester.ali
	gnatlink tester.ali c_errno.o c_tinyserve.o
	./tester
	rm test.txt

speedtest:
	cc -c -O2 -Wall -Werror -fstack-protector -D_FORTIFY_SOURCE=2 -c c_tinyserve.c
	cc -c -O2 -Wall -Werror -fstack-protector -D_FORTIFY_SOURCE=2 c_errno.c
	gnatmake -c -I../ -gnatfoN speedtester.adb
	gnatbind -x -I../ speedtester.ali
	gnatlink speedtester.ali c_errno.o c_tinyserve.o
	time ./speedtester

backofftest:
	cc -c -O2 -Wall -Werror -fstack-protector -D_FORTIFY_SOURCE=2 -c c_tinyserve.c
	cc -c -O2 -Wall -Werror -fstack-protector -D_FORTIFY_SOURCE=2 c_errno.c
	gnatmake -c -I../ -gnatfoN backofftester.adb
	gnatbind -x -I../ backofftester.ali
	gnatlink backofftester.ali c_errno.o c_tinyserve.o
	time ./backofftester

tinyservetest:
	cc -c -O2 -Wall -Werror -fstack-protector -D_FORTIFY_SOURCE=2 -c c_tinyserve.c
	cc -c -O2 -Wall -Werror -fstack-protector -D_FORTIFY_SOURCE=2 c_errno.c
	gnatmake -c -I../ -gnatfoN tinyservetester.adb
	gnatbind -x -I../ tinyservetester.ali
	gnatlink tinyservetester.ali c_errno.o c_tinyserve.o
	time ./tinyservetester

clean:
	-rm *.o *.ali
	test -f tester && rm tester || :
	test -f test.txt && test.txt || :
	test -f tinyservetester && tinyservetester || :

