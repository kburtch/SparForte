/* $Id: cglue.c,v 1.2 2005/02/11 02:59:36 ken Exp $
 * Warren W. Gay VE3WWG
 *
 * C glue, which is linked with the Ada code :
 */
static const char rcsid[] = "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/cglue.c,v 1.2 2005/02/11 02:59:36 ken Exp $";

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>

#if HAVE_SCHED
#include <memory.h>
#endif

#include "config.h"

int
fetch_error(void) {
    return errno;
}

void
store_error(int err) {
    errno = err;
}

#if HAVE_SCHED

int
set_sched_policy(int policy,int priority) {
	struct sched_param param;
	int pri_max, pri_min;

	/*
	 * Check if the priority is within range :
	 */
	pri_max = sched_get_priority_max(policy);
	pri_min = sched_get_priority_min(policy);
	if ( priority > pri_max || priority < pri_min )
		return ERANGE;

	/*
	 * Change the scheduling priority :
	 */ 
	memset(&param,0,sizeof param);
	param.sched_priority = priority;

	if ( sched_setscheduler(0,policy,&param) != 0 )
		return errno;	/* Failed */

	return 0;		/* Successful */
}

#endif

/*
 * If a setuid program, we want to go back to
 * unpriviledged mode for safety.
 */
void
squash_root() {

	if ( geteuid() == 0 )
		setuid(getuid());               /* Be ourself; don't stay as root! */
}
	
/*
 * End $Source: /home/cvsroot/bush/src/ADAVOX-0.51/wc/cglue.c,v $
 */
