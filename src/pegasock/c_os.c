/* C code for functions not covered in bush_os.ads          */
/* Part of BUSH                                             */
/*                                                          */
/* I didn't want to resort to C, but I need to use stat()   */
/* and manually declaring the structure in Ada is extremely */
/* unportable between UNIXes -- KB                          */
//
//  CVS: $Id: c_os.c,v 1.6 2005/08/31 15:10:44 ken Exp $

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <time.h>
#include <errno.h>

/* C PEGASOCK ERRNO                                         */
/*                                                          */
/* Patch for GCC 3.x / GNAT 5.x which won't import errno.   */

int C_pegasock_errno( void ) {
    return errno;
}

/* C PEGASOCK RESET ERRNO                                   */
/*                                                          */
/* Patch for GCC 3.x / GNAT 5.x which won't import errno.   */

void C_pegasock_reset_errno( void ) {
     errno = 0;
}

