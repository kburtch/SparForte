/* $Id: getgrent.c,v 1.2 2005/02/11 02:59:46 ken Exp $
 * Warren W. Gay VE3WWG
 *
 * Licensed under the ACL (Ada Community License)
 */
#include <stdio.h>
#include <sys/types.h>
#include <grp.h>

int
main(int argc,char **argv) {
	struct group *gr;
	char **mp;

	while ( (gr = getgrent()) != NULL ) {
		printf("%s:%s:%d:",
			gr->gr_name,
			gr->gr_passwd,
			gr->gr_gid);
		for ( mp=gr->gr_mem; *mp != NULL; ++mp ) {
			if ( mp != gr->gr_mem )
				putchar(',');
			fputs(*mp,stdout);
		}
		putchar('\n');
	}

	return 0;
}

/* End $Source: /home/cvsroot/bush/src/apq-2.1/eg2/getgrent.c,v $ */
