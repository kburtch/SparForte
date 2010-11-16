
#ifdef __vxworks
#include "ioLib.h"
#include "dosFsLib.h"
#ifndef __RTP__
# include "nfsLib.h"
#endif
#include "selectLib.h"
#include "vxWorks.h"
#endif

#ifdef IN_RTS
#define POSIX
#include "tconfig.h"
#include "tsystem.h"
#include <fcntl.h>
#include <sys/stat.h>
#ifdef VMS
#include <unixio.h>
#endif
//#else
//#include "config.h"
//#include "system.h"
#endif

#include <time.h>
#include <errno.h>

#if defined (sun) && defined (__SVR4) && !defined (__vxworks)
/* The declaration is present in <time.h> but conditionalized
   on a couple of macros we don't define.  */
extern struct tm *localtime_r(const time_t *, struct tm *);
#endif

/* This value is returned as the time zone offset when a valid value
   cannot be determined. It is simply a bizarre value that will never
   occur. It is 3 days plus 73 seconds (offset is in seconds). */

long __bush_invalid_tzoff = 259273;

/* Definition of __gnat_localtime_r used by a-calend.adb */

#if defined (__EMX__) || defined (__MINGW32__)

#ifdef CERT

/* For the Cert run times on native Windows we use dummy functions
   for locking and unlocking tasks since we do not support multiple
   threads on this configuration (Cert run time on native Windows). */

void dummy (void) {}

void (*Lock_Task) ()   = &dummy;
void (*Unlock_Task) () = &dummy;

#else

#define Lock_Task system__soft_links__lock_task
extern void (*Lock_Task) (void);

#define Unlock_Task system__soft_links__unlock_task
extern void (*Unlock_Task) (void);

#endif

/* Reentrant localtime for Windows and OS/2. */

extern struct tm *
__bush_localtime_tzoff (const time_t *, struct tm *, long *);

struct tm *
__bush_localtime_tzoff (const time_t *timer, struct tm *tp, long *off)
{
                                                                             745,0-1       80%
  DWORD dwRet;
  struct tm *tmp;
  TIME_ZONE_INFORMATION tzi;

  (*Lock_Task) ();
  tmp = localtime (timer);
  memcpy (tp, tmp, sizeof (struct tm));
  dwRet = GetTimeZoneInformation (&tzi);
  *off = tzi.Bias;
  if (tp->tm_isdst > 0)
    *off = *off + tzi.DaylightBias;
  *off = *off * -60;
  (*Unlock_Task) ();
  return tp;
}

#else
#if defined (__Lynx__) && defined (___THREADS_POSIX4ad4__)

/* As of LynxOS 3.1.0a patch level 040, LynuxWorks changes the
   prototype to the C library function localtime_r from the POSIX.4
   Draft 9 to the POSIX 1.c version. Before this change the following
   spec is required. Only use when ___THREADS_POSIX4ad4__ is defined,
   the Lynx convention when building against the legacy API. */

extern struct tm *
__bush_localtime_tzoff (const time_t *, struct tm *, long *);

struct tm *
__bush_localtime_tzoff (const time_t *timer, struct tm *tp, long *off)
{
  /* Treat all time values in GMT */
  localtime_r (tp, timer);
  *off = 0;
  return NULL;
}

#else
#if defined (VMS)

/* __bush_localtime_tzoff is not needed on VMS */

#else
/* All other targets provide a standard localtime_r */

extern struct tm *
__bush_localtime_tzoff (const time_t *, struct tm *, long *);

struct tm *
__bush_localtime_tzoff (const time_t *timer, struct tm *tp, long *off)
{
   localtime_r (timer, tp);

/* AIX, HPUX, SGI Irix, Sun Solaris */
#if defined (_AIX) || defined (__hpux__) || defined (sgi) || defined (sun)
  /* The contents of external variable "timezone" may not always be
     initialized. Instead of returning an incorrect offset, treat the local
     time zone as 0 (UTC). The value of 28 hours is the maximum valid offset
     allowed by Ada.Calendar.Time_Zones. */
  if ((timezone < -28 * 3600) || (timezone > 28 * 3600))
    *off = 0;
  else
  {
    *off = (long) -timezone;
    if (tp->tm_isdst > 0)
      *off = *off + 3600;
   }
/* Lynx - Treat all time values in GMT */
#elif defined (__Lynx__)
  *off = 0;

/* VxWorks */
#elif defined (__vxworks)
#include <stdlib.h>
{
  /* Try to read the environment variable TIMEZONE. The variable may not have
     been initialize, in that case return an offset of zero (0) for UTC. */
  char *tz_str = getenv ("TIMEZONE");

  if ((tz_str == NULL) || (*tz_str == '\0'))
    *off = 0;
  else
  {
    char *tz_start, *tz_end;

    /* The format of the data contained in TIMEZONE is N::U:S:E where N is the
       name of the time zone, U are the minutes difference from UTC, S is the
       start of DST in mmddhh and E is the end of DST in mmddhh. Extracting
       the value of U involves setting two pointers, one at the beginning and
       one at the end of the value. The end pointer is then set to null in
       order to delimit a string slice for atol to process. */
    tz_start = index (tz_str, ':') + 2;
    tz_end = index (tz_start, ':');
    tz_end = '\0';

    /* The Ada layer expects an offset in seconds */
    *off = atol (tz_start) * 60;
  }
}

/* Darwin, Free BSD, Linux, Tru64, where there exists a component tm_gmtoff
   in struct tm */
#elif defined (__APPLE__) || defined (__FreeBSD__) || defined (linux) ||\
     (defined (__alpha__) && defined (__osf__)) || defined (__GLIBC__)
  *off = tp->tm_gmtoff;

/* All other platforms: Treat all time values in GMT */
#else
  *off = 0;
#endif
   return NULL;
}

#endif
#endif
#endif



