/* $Id: getpwent.c,v 1.2 2005/02/11 02:59:46 ken Exp $
 * Warren W. Gay VE3WWG
 *
 * Licensed under the ACL (Ada Community License)
 */
#include <stdio.h>
#include <sys/types.h>
#include <pwd.h>

int
main(int argc,char **argv) {
	struct passwd *pw;

	while ( (pw = getpwent()) != NULL ) {
		printf("%s:%s:%d:%d:%s:%s:%s\n",
			pw->pw_name,
			pw->pw_passwd,
			pw->pw_uid,
			pw->pw_gid,
			pw->pw_gecos,
			pw->pw_dir,
			pw->pw_shell);
	}

	return 0;
}

/* End $Source: /home/cvsroot/bush/src/apq-2.1/eg/getpwent.c,v $ */
