# $Header: /home/cvsroot/bush/src/ADAVOX-0.51/Makefile,v 1.2 2005/02/11 02:59:33 ken Exp $
# Top Level Makefile for adavox
# Warren W. Gay VE3WWG
# 
# Protected under the GPL

# Default Settings :
#
# To override, example: $ make CPU=486 INSTDIR=/opt/local/bin
#
INSTDIR=/usr/local/bin
CPU=pentiumpro			
OPTZ=
GNATOPTS=-gnato -gnata -gnatn -q -c -i

all:
	(cd ./wc   ; $(MAKE) -$(MAKEFLAGS) all)
	(cd ./main ; $(MAKE) -$(MAKEFLAGS) INSTDIR=$(INSTDIR) CPU=$(CPU) OPTZ=$(OPTZ) all)

clean:
	(cd ./wc   ; $(MAKE) -$(MAKEFLAGS) clean)
	(cd ./main ; $(MAKE) -$(MAKEFLAGS) clean)

clobber:
	(cd ./wc   ; $(MAKE) -$(MAKEFLAGS) clobber)
	(cd ./main ; $(MAKE) -$(MAKEFLAGS) clobber)

install:
	(cd ./main ; $(MAKE) -$(MAKEFLAGS) INSTDIR=$(INSTDIR) install)

test:
	gnatmake hello
	./hello

# End
