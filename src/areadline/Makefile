# A simple makefile
all:
	gnatmake -c -O2 -gnatfoN rl.adb
	gnatbind -x rl.ali
	gnatlink rl.ali -L/lib/x86_64-linux-gnu/ -l:libreadline.so.6 -l:libhistory.so.6 -lcurses
clean:
	rm -f *.o *.ali rl

