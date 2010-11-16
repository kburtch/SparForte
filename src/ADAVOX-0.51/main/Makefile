# $Header: /home/cvsroot/bush/src/ADAVOX-0.51/main/Makefile,v 1.2 2005/02/11 02:59:35 ken Exp $
# Warren W. Gay VE3WWG
#
# Protected under the GPL

#
# OVERRIDE THIS IF YOU WANT THE EXECUTABLE TO GO SOMEWHERE ELSE
#
INSTDIR=/usr/local/bin

#
# MODIFY THE CPU= LINE OUT IF YOU ARE NOT RUNNING ON A PENTIUM
#
#CPU=pentiumpro # -m486

#
# MODIFY THE NEXT LINES WHEN DEBUGGING (NB: Do not use -O1 or higher)
#
OPTZ=	# -O # -g
GNATOPTS=-gnato -gnata

INC=	-I../bc -I../wc
LIB=	-largs ../wc/cglue.o

COPTS=	-Wall $(OPTZ) -march=$(CPU)
AOPTS=  $(INC) -gnatf $(GNATOPTS)
AOPTZ=  $(OPTZ) -march=$(CPU)

CC=     gcc

.c.o:
	$(CC) -c $(CFLAGS) $(COPTS) -Wall $<

all:	adavox

adavox:	adavox.adb main.adb main.ads # and so much more ;-)
	gnatmake $(AOPTS) $(AOPTZ) adavox $(LIB)

clean:
	rm -f *.o *.ali core b~* *.s adavox

clobber: clean 
	rm -f adavox

install: adavox
	install -o 0 -g 0 -cpm 555 adavox $(INSTDIR)/
	chmod u+s $(INSTDIR)/adavox	# Setuid permits realtime priority
	@ls -l $(INSTDIR)/adavox

# End
