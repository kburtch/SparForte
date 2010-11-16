# $Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/Makefile,v 1.2 2005/02/11 02:59:36 ken Exp $
# Warren W. Gay VE3WWG
#
# Protected under the GPL

# ERRNO_H=/usr/include/errno.h

CC=	gcc
COPTS=	-c -Wall -D`uname -s` -DRELEASE="\"`uname -r`\"" -DHARDWARE="\"`uname -m`\""

.c.o:
	$(CC) $(CFLAGS) $(COPTS) $<

all:	wc-host.ads cglue.o
	rm -f hostconst.o

hostconst:
	$(CC) $(CFLAGS) $(COPTS) hostconst.c
	$(CC) hostconst.o -o hostconst

wc-host.ads: hostconst
	./hostconst 1>wc-host.ads 2>wc-host.adb

clean:
	rm -f *.o *.ali hostconst hostconst.o cglue.obj cglue.o wc-host.ads

clobber: clean
	rm -f wc-hosts.ads

# End
