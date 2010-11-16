/* $Id: config.h,v 1.2 2005/02/11 02:59:36 ken Exp $
 *
 * (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
 *
 * Protected under the GNU GPL License
 */
#ifndef FreeBSD
#define HAVE_FreeBSD 	0
#else
#define HAVE_FreeBSD 	1
#define PLATFORM 	"FreeBSD"
#include <sched.h>
#define HAVE_SCHED	1
#endif 

#ifndef NetBSD
#define HAVE_NetBSD 	0
#else
#define HAVE_NetBSD 	1
#define PLATFORM 	"NetBSD"
#include <sched.h>
#define HAVE_SCHED	1
#endif 

#ifndef OpenBSD
#define HAVE_OpenBSD 	0
#else
#define HAVE_OpenBSD 	1
#define PLATFORM 	"OpenBSD"
#include <sched.h>
#define HAVE_SCHED	1
#endif 

#if HAVE_FreeBSD | HAVE_NetBSD | HAVE_OpenBSD
#define HAVE_BSD	1
#else
#define HAVE_BSD	0
#endif

#ifndef Linux
#define HAVE_LINUX	0
#else
#define PLATFORM 	"LINUX"
#include <sched.h>
#define HAVE_SCHED	1
#define HAVE_LINUX	1
#endif

#ifndef HAVE_SCHED
#define HAVE_SCHED	0
#endif

#if HAVE_BSD		/* Any BSD */
#include <sys/soundcard.h>
#endif

#if HAVE_LINUX
#include <linux/soundcard.h>
#endif

/* End config.h */
