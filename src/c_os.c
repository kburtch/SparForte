/* C code for functions not covered in spar_os.ads          */
/* Part of SparForte                                        */
/*                                                          */
/* I didn't want to resort to C, but I need to use stat()   */
/* and manually declaring the structure in Ada is extremely */
/* unportable between UNIXes -- KB                          */

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>
#include <time.h>
#include <errno.h>

/* C ERRNO                                                  */
/*                                                          */
/* Patch for GCC 3.x / GNAT 5.x which won't import errno.   */

int C_errno( void ) {
    return errno;
}

/* C RESET ERRNO                                            */
/*                                                          */
/* Patch for GCC 3.x / GNAT 5.x which won't import errno.   */

void C_reset_errno( void ) {
     errno = 0;
}

/* C WEXITSTATUS                                            */
/*                                                          */
/* Call macro WEXITSTATUS to get the status from an waitpid */
/* result.  Returns 0 if killed or 255 otherwise.           */

int C_WEXITSTATUS( int waitpid_status ) {
  if ( WIFEXITED( waitpid_status ) ) {
     return WEXITSTATUS( waitpid_status );
  } else if ( WIFSIGNALED( waitpid_status ) ) {
     return 0;
  } else {
     return 255;
  }
}

/* IS EXECUTABLE FILE                                       */
/*                                                          */
/* True if file exists, is regular, and is executable       */

int C_is_executable_file( char * path ) {
  struct stat info;
  int result = 0;
  if ( stat( path, &info ) == 0 ) {
     if ( info.st_mode & S_IFREG ) {
        if ( getuid() == info.st_uid ) {
           result = (info.st_mode & S_IXUSR ) > 0;
        } else if ( getgid() == info.st_gid ) {
           result = (info.st_mode & S_IXGRP ) > 0;
        } else {
           result = (info.st_mode & S_IXOTH ) > 0;
        }
     }
  }
  return result;
}

/* IS EXECUTABLE                                            */
/*                                                          */
/* True if file or special file exists and is executable    */

int C_is_executable( char * path ) {
  struct stat info;
  int result = 0;
  if ( stat( path, &info ) == 0 ) {
     if ( getuid() == info.st_uid ) {
        result = (info.st_mode & S_IXUSR ) > 0;
     } else if ( getgid() == info.st_gid ) {
        result = (info.st_mode & S_IXGRP ) > 0;
     } else {
        result = (info.st_mode & S_IXOTH ) > 0;
     }
  }
  return result;
}

/* IS READABLE FILE                                         */
/*                                                          */
/* True if file exists, is regular, and is executable       */

int C_is_readable_file( char * path ) {
  struct stat info;
  int result = 0;
  if ( stat( path, &info ) == 0 ) {
     if ( info.st_mode & S_IFREG ) {
        if ( getuid() == info.st_uid ) {
           result = (info.st_mode & S_IRUSR ) > 0;
        } else if ( getgid() == info.st_gid ) {
           result = (info.st_mode & S_IRGRP ) > 0;
        } else {
           result = (info.st_mode & S_IROTH ) > 0;
        }
     }
  }
  return result;
}

/* IS READABLE                                              */
/*                                                          */
/* True if file exists and is readable.                     */

int C_is_readable( char * path ) {
  struct stat info;
  int result = 0;
  if ( stat( path, &info ) == 0 ) {
     if ( getuid() == info.st_uid ) {
        result = (info.st_mode & S_IRUSR ) > 0;
     } else if ( getgid() == info.st_gid ) {
        result = (info.st_mode & S_IRGRP ) > 0;
     } else {
        result = (info.st_mode & S_IROTH ) > 0;
     }
  }
  return result;
}

/* IS WAITING FILE                                          */
/*                                                          */
/* True if file exists, is regular, readable and not empty  */

int C_is_waiting_file( char * path ) {
  struct stat info;
  int result = 0;
  if ( stat( path, &info ) == 0 ) {
     if ( info.st_mode & S_IFREG ) {
        if ( info.st_size > 0 ) {
           if ( getuid() == info.st_uid ) {
              result = (info.st_mode & S_IRUSR ) > 0;
           } else if ( getgid() == info.st_gid ) {
              result = (info.st_mode & S_IRGRP ) > 0;
           } else {
              result = (info.st_mode & S_IROTH ) > 0;
           }
        }
     }
  }
  return result;
}

/* IS INCLUDABLE FILE                                       */
/*                                                          */
/* True if file exists, is regular, readable, not empty and */
/* not world writable                                       */

int C_is_includable_file( char * path ) {
  struct stat info;
  int result = 0;
  if ( stat( path, &info ) == 0 ) {
     if ( info.st_mode & S_IFREG ) {
        if ( info.st_size > 0 ) {
           if ( getuid() == info.st_uid ) {
              result = (info.st_mode & S_IRUSR ) > 0;
           } else if ( getgid() == info.st_gid ) {
              result = (info.st_mode & S_IRGRP ) > 0;
           } else {
              result = (info.st_mode & S_IROTH ) > 0;
           }
           if ( result ) {
              result = (info.st_mode & S_IWOTH ) == 0;
           }
        }
     }
  }
  return result;
}

/* FILE LENGTH                                              */
/*                                                          */
/* Return file length as reported by stat().                */

int C_file_length( char * path ) {
  struct stat info;
  int result = -1;
  if ( stat( path, &info ) == 0 )
     result = info.st_size;
  return result;
}

/* FILE MODIFY TIME                                         */
/*                                                          */
/* Return file modify time as reported by stats().          */

void C_file_modify_time( char * path, int *year, int *month, int *day, int *sec ) {
  struct stat info;
  struct tm *timestruct;

  *year = -1;
  if ( stat( path, &info ) == 0 ) {
     timestruct = localtime( &info.st_mtime );
     *year = timestruct->tm_year+1900;
     *month = timestruct->tm_mon+1;
     *day = timestruct->tm_mday;
     *sec = timestruct->tm_sec+
            timestruct->tm_min*60+
            timestruct->tm_hour*60*60;
  }
}

/* FILE CHANGE TIME                                         */
/*                                                          */
/* Return file change time as reported by stats().          */

void C_file_change_time( char * path, int *year, int *month, int *day, int *sec ) {
  struct stat info;
  struct tm *timestruct;

  *year = -1;
  if ( stat( path, &info ) == 0 ) {
     timestruct = localtime( &info.st_ctime );
     *year = timestruct->tm_year+1900;
     *month = timestruct->tm_mon+1;
     *day = timestruct->tm_mday;
     *sec = timestruct->tm_sec+
            timestruct->tm_min*60+
            timestruct->tm_hour*60*60;
  }
}

/* FILE ACCESS TIME                                         */
/*                                                          */
/* Return file access time as reported by stats().          */

void C_file_access_time( char * path, int *year, int *month, int *day, int *sec ) {
  struct stat info;
  struct tm *timestruct;

  *year = -1;
  if ( stat( path, &info ) == 0 ) {
     timestruct = localtime( &info.st_atime );
     *year = timestruct->tm_year+1900;
     *month = timestruct->tm_mon+1;
     *day = timestruct->tm_mday;
     *sec = timestruct->tm_sec+
            timestruct->tm_min*60+
            timestruct->tm_hour*60*60;
  }
}

/* DAY OF WEEK                                              */
/*                                                          */
/* Return the day of week                                   */

void C_day_of_week( int * wday, int year, int month, int day ) {
  time_t result = -1;
  struct tm timestruct;
  struct tm *timestruct2;

  *wday = -1;
  timestruct.tm_year = year-1900;
  timestruct.tm_mon  = month-1;
  timestruct.tm_mday = day;
  timestruct.tm_sec  = 0;
  timestruct.tm_min  = 0;
  timestruct.tm_hour = 0;
  result = mktime( &timestruct );
  timestruct2 = localtime( &result );
  *wday = timestruct2->tm_wday+1;

}

// SIGNAL HANDLING

int *sigchld_flag = 0;         // Address of Ada boolean variable
int *sigint_flag = 0;          // Address of Ada boolean variable
int *sigwinch_flag = 0;        // Address of Ada boolean variable
int *sigpipe_flag = 0;         // Address of Ada boolean variable

void sigint_handler( int sig ) {
  *sigint_flag = 1;
}

int C_install_sigint_handler( int *flag_address ) {
  static unsigned int handler_installed = 0;
  sigset_t signalmask;
  struct sigaction old_sigint;
  struct sigaction sa;
  int res = 0;

  sigint_flag = flag_address;                    // where to flag occurence
  sigfillset( &signalmask );                     // block all during handling
  sa.sa_handler = sigint_handler;                // what to do on signal
  sa.sa_mask    = signalmask;                    // mask all signals
  sa.sa_flags   = 0;                             // trap zombie children
  res = sigaction( SIGINT, &sa, &old_sigint );   // setup signal trap
  if ( res == 0 )                                // OK?
     handler_installed = 1;                      // mark as installed
  return handler_installed;
}

void sigchld_handler( int sig ) {
  *sigchld_flag = 1;
}

int C_install_sigchld_handler( int *flag_address ) {
  static unsigned int handler_installed = 0;
  sigset_t signalmask;
  struct sigaction old_sigchld;
  struct sigaction sa;
  int res = 0;

  sigchld_flag = flag_address;                   // where to flag occurence
  sigfillset( &signalmask );                     // block all during handling
  sa.sa_handler = sigchld_handler;               // what to do on signal
  sa.sa_mask    = signalmask;                    // mask all signals
  sa.sa_flags   = 0;                             // trap zombie children
  res = sigaction( SIGCHLD, &sa, &old_sigchld ); // setup signal trap
  if ( res == 0 )                                // OK?
     handler_installed = 1;                      // mark as installed
  return handler_installed;
}

void sigwinch_handler( int sig ) {
  *sigwinch_flag = 1;
}

int C_install_sigwinch_handler( int *flag_address ) {
  static unsigned int handler_installed = 0;
  sigset_t signalmask;
  struct sigaction old_sigwinch;
  struct sigaction sa;
  int res = 0;

  sigwinch_flag = flag_address;                  // where to flag occurence
  sigfillset( &signalmask );                     // block all during handling
  sa.sa_handler = sigwinch_handler;              // what to do on signal
  sa.sa_mask    = signalmask;                    // mask all signals
  sa.sa_flags   = 0;                             // trap zombie children
  res = sigaction( SIGWINCH, &sa, &old_sigwinch ); // setup signal trap
  if ( res == 0 )                                // OK?
     handler_installed = 1;                      // mark as installed
  return handler_installed;
}

void sigpipe_handler( int sig ) {
  *sigpipe_flag = 1;
}

int C_install_sigpipe_handler( int *flag_address ) {
  static unsigned int handler_installed = 0;
  sigset_t signalmask;
  struct sigaction old_sigpipe;
  struct sigaction sa;
  int res = 0;

  sigpipe_flag = flag_address;                   // where to flag occurence
  sigfillset( &signalmask );                     // block all during handling
  sa.sa_handler = sigpipe_handler;               // what to do on signal
  sa.sa_mask    = signalmask;                    // mask all signals
  sa.sa_flags   = 0;                             // trap zombie children
  res = sigaction( SIGPIPE, &sa, &old_sigpipe ); // setup signal trap
  if ( res == 0 )                                // OK?
     handler_installed = 1;                      // mark as installed
  return handler_installed;
}

// SDL TESTING
// FREEBSD possibly should be __FreeBSD__
#ifdef FREEBSD
#include <SDL.h>
#include <SDL_video.h>
#endif
#ifdef __APPLE__
#include <SDL.h>
#include <SDL_video.h>
#endif
#ifdef __CYGWIN__
#include <SDL/SDL.h>
#include <SDL/SDL_video.h>
#endif
#ifdef __linux__
#include <SDL/SDL.h>
#include <SDL/SDL_video.h>
#endif


/* ------------------------------------------------------------------------ */
/* GET PIXELS ADDRESS                                                       */
/*                                                                          */
/* Address_To_Access_Conversions returns the wrong value in                 */
/* GCC 3.x.  Use C to get the correct system.address.                       */
/* ------------------------------------------------------------------------ */

int *get_pixels_address( SDL_Surface *screen ) {
    return (int *) screen->pixels;
}


/* ------------------------------------------------------------------------ */
/* GET FORMAT ADDRESS                                                       */
/*                                                                          */
/* Address_To_Access_Conversions returns the wrong value in                 */
/* GCC 3.x.  Use C to get the correct system.address.                       */
/* ------------------------------------------------------------------------ */

int *get_format_address( SDL_Surface *screen ) {
    return (int *) screen->format;
}

/* ------------------------------------------------------------------------ */
/* SDL EXT(ension) PLOT                                                     */
/*                                                                          */
/* Set a single pixel in the current SDL video mode.  Although this could   */
/* be done in Ada, this version avoids the messiness of address-access      */
/* conversions, array bounds and SDL endian macros.                         */
/* ------------------------------------------------------------------------ */

void SDL_EXT_plot( SDL_Surface *screen, Sint16 x, Sint16 y, Uint32 colour,
  int pen_mode ) {
  Uint8  *ubuff8;
  Uint16 *ubuff16;
  Uint32 *ubuff32;
  Uint8 c1;
  Uint8 c2;
  Uint8 c3;
  Uint8 p1;
  Uint8 p2;
  Uint8 p3;
  switch (screen->format->BytesPerPixel) {
  case 1: // Indexed (palette of 256 colours)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + x;
    switch (pen_mode) {
    case 0: // pen_mode_invert
      *ubuff8 = (Uint8) colour ^ *ubuff8;
      break;
    case 5: // pen_mode_off
      break;
    default: /* pen_mode_copy */
      *ubuff8 = (Uint8) colour;
    }
    break;
  case 2: // Hi colour (Video card dependent components)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*2);
    ubuff16 = (Uint16*) ubuff8;
    switch (pen_mode) {
    case 0: // pen_mode_invert:
      *ubuff16 = (Uint16) colour ^ *ubuff16;
      break;
    case 1: // pen_mode_brighten:
      c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
      c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
      c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
      p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
      p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
      p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
      *ubuff16 =
         ((( (Uint16) c1 + (Uint16) p1) << screen->format->Rshift ) &
            screen->format->Rmask) +
         ((( (Uint16) c2 + (Uint16) p2) << screen->format->Gshift ) &
            screen->format->Gmask) +
         ((( (Uint16) c3 + (Uint16) p3) << screen->format->Bshift ) &
            screen->format->Bmask );
      break;
    case 2: // pen_mode_darken:
      c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
      c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
      c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
      p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
      p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
      p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
      *ubuff16 =
         ((( (Uint16) c1 - (Uint16) p1) << screen->format->Rshift ) &
            screen->format->Rmask) +
         ((( (Uint16) c2 - (Uint16) p2) << screen->format->Gshift ) &
            screen->format->Gmask) +
         ((( (Uint16) c3 - (Uint16) p3) << screen->format->Bshift ) &
            screen->format->Bmask );
      break;
    case 3: // pen_mode_average
      c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
      c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
      c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
      p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
      p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
      p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
      *ubuff16 =
         ((( ( (Uint16) c1 + (Uint16) p1) / 2) << screen->format->Rshift ) &
            screen->format->Rmask) +
         ((( ( (Uint16) c2 + (Uint16) p2) / 2) << screen->format->Gshift ) &
            screen->format->Gmask) +
         ((( ( (Uint16) c3 + (Uint16) p3) / 2) << screen->format->Bshift ) &
            screen->format->Bmask );
      break;
    case 5: // pen_mode_off
      break;
    default: /* pen_mode_copy */
      *ubuff16 = (Uint16) colour;
    }
    break;
  case 3: // True Colour (8-bit components, endian dependent)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*3);
    if (SDL_BYTEORDER == SDL_LIL_ENDIAN ) {
       c1 = (colour & 0xFF0000) >> 16;
       c2 = (colour & 0x00FF00) >> 8;
       c3 = colour & 0x0000FF;
    } else {
       c3 = (colour & 0xFF0000) >> 16;
       c2 = (colour & 0x00FF00) >> 8;
       c1 = colour & 0x0000FF;
    }
    switch (pen_mode) {
    case 0: // pen_mode_invert:
      *ubuff8 = *ubuff8 ^ c3;
      ubuff8[1] = ubuff8[1] ^ c2;
      ubuff8[2] = ubuff8[2] ^ c1;
      break;
    case 1: // pen_mode_brighten:
      *ubuff8 = *ubuff8 + c3;
      ubuff8[1] = ubuff8[1] + c2;
      ubuff8[2] = ubuff8[2] + c1;
      break;
    case 2: // pen_mode_darken:
      *ubuff8 = *ubuff8 - c3;
      ubuff8[1] = ubuff8[1] - c2;
      ubuff8[2] = ubuff8[2] - c1;
      break;
    case 3: // pen_mode_average
      *ubuff8 = (*ubuff8 + c3) / 2;
      ubuff8[1] = (ubuff8[1] + c2) / 2;
      ubuff8[2] = (ubuff8[2] + c1) / 2;
      break;
    case 5: // pen_mode_off
      break;
    default: /* pen_mode_copy */
      *ubuff8 = c3;
      ubuff8[1] = c2;
      ubuff8[2] = c1;
    }
    break;
  case 4: // Direct Colour (3 x 8-bit palette index)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*4);
    ubuff32 = (Uint32 *) ubuff8;
    switch (pen_mode) {
    case 0: // pen_mode_invert:
      *ubuff32 = *ubuff32 ^ colour;
      break;
    case 1: // pen_mode_brighten: (doesn't work right)
      *ubuff32 = *ubuff32 + colour;
      break;
    case 2: // pen_mode_darken: (doesn't work right)
      *ubuff32 = *ubuff32 - colour;
      break;
    case 3: // pen_mode_average (doesn't work right)
      *ubuff32 = (*ubuff32 + colour)/2;
      break;
    case 5: // pen_mode_off
      break;
    default: /* pen_mode_copy */
      *ubuff32 = colour;
    }
    break;
  default:
     fprintf( stderr, "SDL_Plot unknown mode\n" );
  }
}

/* ------------------------------------------------------------------------ */
/* SDL EXT(ension) HLINE                                                    */
/*                                                                          */
/* Draw a horizontal line in current SDL video mode.  Although this could   */
/* be done in Ada, this version avoids the messiness of address-access      */
/* conversions, array bounds and SDL endian macros.                         */
/* ------------------------------------------------------------------------ */

void SDL_EXT_hline( SDL_Surface *screen, Sint16 x, Sint16 x2, Sint16 y, Uint32 colour, int pen_mode ) {
  Uint8  *ubuff8 = NULL;
  Uint16 *ubuff16 = NULL;
  Uint32 *ubuff32 = NULL;
  Uint8 c1;
  Uint8 c2;
  Uint8 c3;
  Uint8 p1;
  Uint8 p2;
  Uint8 p3;
  Sint16 x1 = x+1;
  // Sint16 y1 = y;
  // Uint8 colour8 = (Uint8) colour;
  Uint16 colour16= (Uint16) colour;
  switch (screen->format->BytesPerPixel) {
  case 1: // Indexed (palette of 256 colours)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + x;
    switch (pen_mode) {
    case 0: // pen_mode_invert
      *ubuff8 = (Uint8) colour ^ *ubuff8;
      break;
    case 5: // pen_mode_off
      break;
    default: /* pen_mode_copy */
      *ubuff8 = (Uint8) colour;
    }
    break;
  case 2: // Hi colour (Video card dependent components)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*2);
    ubuff16 = (Uint16*) ubuff8;
    switch (pen_mode) {
    case 0: // pen_mode_invert:
      *ubuff16 = (Uint16) colour ^ *ubuff16;
      break;
    case 1: // pen_mode_brighten:
      c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
      c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
      c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
      p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
      p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
      p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
      *ubuff16 =
         ((( (Uint16) c1 + (Uint16) p1) << screen->format->Rshift ) &
            screen->format->Rmask) +
         ((( (Uint16) c2 + (Uint16) p2) << screen->format->Gshift ) &
            screen->format->Gmask) +
         ((( (Uint16) c3 + (Uint16) p3) << screen->format->Bshift ) &
            screen->format->Bmask );
      break;
    case 2: // pen_mode_darken:
      c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
      c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
      c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
      p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
      p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
      p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
      *ubuff16 =
         ((( (Uint16) c1 - (Uint16) p1) << screen->format->Rshift ) &
            screen->format->Rmask) +
         ((( (Uint16) c2 - (Uint16) p2) << screen->format->Gshift ) &
            screen->format->Gmask) +
         ((( (Uint16) c3 - (Uint16) p3) << screen->format->Bshift ) &
            screen->format->Bmask );
      break;
    case 3: // pen_mode_average
      c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
      c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
      c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
      p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
      p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
      p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
      *ubuff16 =
         ((( ( (Uint16) c1 + (Uint16) p1) / 2) << screen->format->Rshift ) &
            screen->format->Rmask) +
         ((( ( (Uint16) c2 + (Uint16) p2) / 2) << screen->format->Gshift ) &
            screen->format->Gmask) +
         ((( ( (Uint16) c3 + (Uint16) p3) / 2) << screen->format->Bshift ) &
            screen->format->Bmask );
      break;
    case 5: // pen_mode_off
      break;
    default: /* pen_mode_copy */
      *ubuff16 = (Uint16) colour;
    }
    break;
  case 3: // True Colour (8-bit components, endian dependent)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*3);
    if (SDL_BYTEORDER == SDL_LIL_ENDIAN ) {
       c1 = (colour & 0xFF0000) >> 16;
       c2 = (colour & 0x00FF00) >> 8;
       c3 = colour & 0x0000FF;
    } else {
       c3 = (colour & 0xFF0000) >> 16;
       c2 = (colour & 0x00FF00) >> 8;
       c1 = colour & 0x0000FF;
    }
    switch (pen_mode) {
    case 0: // pen_mode_invert:
      *ubuff8 = *ubuff8 ^ c3;
      ubuff8[1] = ubuff8[1] ^ c2;
      ubuff8[2] = ubuff8[2] ^ c1;
      break;
    case 1: // pen_mode_brighten:
      *ubuff8 = *ubuff8 + c3;
      ubuff8[1] = ubuff8[1] + c2;
      ubuff8[2] = ubuff8[2] + c1;
      break;
    case 2: // pen_mode_darken:
      *ubuff8 = *ubuff8 - c3;
      ubuff8[1] = ubuff8[1] - c2;
      ubuff8[2] = ubuff8[2] - c1;
      break;
    case 3: // pen_mode_average
      *ubuff8 = (*ubuff8 + c3) / 2;
      ubuff8[1] = (ubuff8[1] + c2) / 2;
      ubuff8[2] = (ubuff8[2] + c1) / 2;
      break;
    case 5: // pen_mode_off
      break;
    default: /* pen_mode_copy */
      *ubuff8 = c3;
      ubuff8[1] = c2;
      ubuff8[2] = c1;
    }
    break;
  case 4: // Direct Colour (3 x 8-bit palette index)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*4);
    ubuff32 = (Uint32 *) ubuff8;
    switch (pen_mode) {
    case 0: // pen_mode_invert:
      *ubuff32 = *ubuff32 ^ colour;
      break;
    case 1: // pen_mode_brighten: (doesn't work right)
      *ubuff32 = *ubuff32 + colour;
      break;
    case 2: // pen_mode_darken: (doesn't work right)
      *ubuff32 = *ubuff32 - colour;
      break;
    case 3: // pen_mode_average (doesn't work right)
      *ubuff32 = (*ubuff32 + colour)/2;
      break;
    case 5: // pen_mode_off
      break;
    default: /* pen_mode_copy */
      *ubuff32 = colour;
    }
    break;
  default:
     fprintf( stderr, "SDL_EXT_hline canvas type\n" );
  }
  for ( x=x1;x<=x2;x++ ) {
      switch (screen->format->BytesPerPixel) {
      case 1:
        ubuff8++;
        switch (pen_mode) {
        case 0: // pen_mode_invert
          *ubuff8 = (Uint8) colour ^ *ubuff8;
          break;
        case 5: // pen_mode_off
          break;
        default: /* pen_mode_copy */
          *ubuff8 = (Uint8) colour;
        }
        break;
      case 2:
        ubuff16++;
        switch (pen_mode) {
        case 0: // pen_mode_invert:
          *ubuff16 = (Uint16) colour ^ *ubuff16;
          break;
        case 1: // pen_mode_brighten:
          c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
          c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
          c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
          p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
          p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
          p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
          *ubuff16 =
             ((( (Uint16) c1 + (Uint16) p1) << screen->format->Rshift ) &
                screen->format->Rmask) +
             ((( (Uint16) c2 + (Uint16) p2) << screen->format->Gshift ) &
                screen->format->Gmask) +
             ((( (Uint16) c3 + (Uint16) p3) << screen->format->Bshift ) &
                screen->format->Bmask );
          break;
        case 2: // pen_mode_darken:
          c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
          c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
          c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
          p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
          p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
          p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
          *ubuff16 =
             ((( (Uint16) c1 - (Uint16) p1) << screen->format->Rshift ) &
                screen->format->Rmask) +
             ((( (Uint16) c2 - (Uint16) p2) << screen->format->Gshift ) &
                screen->format->Gmask) +
             ((( (Uint16) c3 - (Uint16) p3) << screen->format->Bshift ) &
                screen->format->Bmask );
          break;
        case 3: // pen_mode_average
          c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
          c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
          c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
          p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
          p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
          p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
          *ubuff16 =
             ((( ( (Uint16) c1 + (Uint16) p1) / 2) << screen->format->Rshift ) &
                screen->format->Rmask) +
             ((( ( (Uint16) c2 + (Uint16) p2) / 2) << screen->format->Gshift ) &
                screen->format->Gmask) +
             ((( ( (Uint16) c3 + (Uint16) p3) / 2) << screen->format->Bshift ) &
                screen->format->Bmask );
          break;
        case 5: // pen_mode_off
          break;
        default: /* pen_mode_copy */
          *ubuff16 = colour16;
        }
        break;
      case 3:
        ubuff8 = ubuff8 + 3;
        if (SDL_BYTEORDER == SDL_LIL_ENDIAN ) {
           c1 = (colour & 0xFF0000) >> 16;
           c2 = (colour & 0x00FF00) >> 8;
           c3 = colour & 0x0000FF;
        } else {
           c3 = (colour & 0xFF0000) >> 16;
           c2 = (colour & 0x00FF00) >> 8;
           c1 = colour & 0x0000FF;
        }
        switch (pen_mode) {
        case 0: // pen_mode_invert:
          *ubuff8 = *ubuff8 ^ c3;
          ubuff8[1] = ubuff8[1] ^ c2;
          ubuff8[2] = ubuff8[2] ^ c1;
          break;
        case 1: // pen_mode_brighten:
          *ubuff8 = *ubuff8 + c3;
          ubuff8[1] = ubuff8[1] + c2;
          ubuff8[2] = ubuff8[2] + c1;
          break;
        case 2: // pen_mode_darken:
          *ubuff8 = *ubuff8 - c3;
          ubuff8[1] = ubuff8[1] - c2;
          ubuff8[2] = ubuff8[2] - c1;
          break;
        case 3: // pen_mode_average
          *ubuff8 = (*ubuff8 + c3) / 2;
          ubuff8[1] = (ubuff8[1] + c2) / 2;
          ubuff8[2] = (ubuff8[2] + c1) / 2;
          break;
        case 5: // pen_mode_off
          break;
        default: /* pen_mode_copy */
          *ubuff8 = c3;
          ubuff8[1] = c2;
          ubuff8[2] = c1;
        }
        break;
      case 4:
        ubuff32++;
        switch (pen_mode) {
        case 0: // pen_mode_invert:
          *ubuff32 = *ubuff32 ^ colour;
          break;
        case 1: // pen_mode_brighten: (doesn't work right)
          *ubuff32 = *ubuff32 + colour;
          break;
        case 2: // pen_mode_darken: (doesn't work right)
          *ubuff32 = *ubuff32 - colour;
          break;
        case 3: // pen_mode_average (doesn't work right)
          *ubuff32 = (*ubuff32 + colour)/2;
          break;
        case 5: // pen_mode_off
          break;
        default: /* pen_mode_copy */
          *ubuff32 = colour;
        }
	break;
      default:
         fprintf( stderr, "SDL_EXT_hline unknown mode\n" );
      } /* case */
  } /* for */
} /* hline */


/* ------------------------------------------------------------------------ */
/* SDL EXT(ension) VLINE                                                    */
/*                                                                          */
/* Draw a vertical line in current SDL video mode.  Although this could     */
/* be done in Ada, this version avoids the messiness of address-access      */
/* conversions, array bounds and SDL endian macros.                         */
/* ------------------------------------------------------------------------ */

void SDL_EXT_vline( SDL_Surface *screen, Sint16 x, Sint16 y1, Sint16 y2, Uint32 colour, int pen_mode ) {
  Uint8  *ubuff8 = NULL;
  Uint16 *ubuff16 = NULL;
  Uint32 *ubuff32 = NULL;
  Uint8 c1;
  Uint8 c2;
  Uint8 c3;
  Uint8 p1;
  Uint8 p2;
  Uint8 p3;
  Sint16 y = y;
  // Uint8 colour8 = (Uint8) colour;
  Uint16 colour16= (Uint16) colour;
  for ( y=y1;y<=y2;y++ ) {
      switch (screen->format->BytesPerPixel) {
      case 1: // Indexed (palette of 256 colours)
        ubuff8 = (Uint8*) screen->pixels;
        ubuff8 += (y * screen->pitch) + x;
        break;
      case 2: // Hi colour (Video card dependent components)
        ubuff8 = (Uint8*) screen->pixels;
        ubuff8 += (y * screen->pitch) + (x*2);
        ubuff16 = (Uint16*) ubuff8;
        break;
      case 3: // True Colour (8-bit components, endian dependent)
        ubuff8 = (Uint8*) screen->pixels;
        ubuff8 += (y * screen->pitch) + (x*3);
        break;
      case 4: // Direct Colour (3 x 8-bit palette index)
        ubuff8 = (Uint8*) screen->pixels;
        ubuff8 += (y * screen->pitch) + (x*4);
        ubuff32 = (Uint32 *) ubuff8;
        break;
      default:
         fprintf( stderr, "SDL_EXT_vline canvas type\n" );
      }
      switch (screen->format->BytesPerPixel) {
      case 1:
        switch (pen_mode) {
        case 0: // pen_mode_invert
          *ubuff8 = (Uint8) colour ^ *ubuff8;
          break;
        case 5: // pen_mode_off
          break;
        default: /* pen_mode_copy */
          *ubuff8 = (Uint8) colour;
        }
        break;
      case 2:
        switch (pen_mode) {
        case 0: // pen_mode_invert:
          *ubuff16 = (Uint16) colour ^ *ubuff16;
          break;
        case 1: // pen_mode_brighten:
          c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
          c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
          c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
          p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
          p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
          p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
          *ubuff16 =
             ((( (Uint16) c1 + (Uint16) p1) << screen->format->Rshift ) &
                screen->format->Rmask) +
             ((( (Uint16) c2 + (Uint16) p2) << screen->format->Gshift ) &
                screen->format->Gmask) +
             ((( (Uint16) c3 + (Uint16) p3) << screen->format->Bshift ) &
                screen->format->Bmask );
          break;
        case 2: // pen_mode_darken:
          c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
          c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
          c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
          p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
          p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
          p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
          *ubuff16 =
             ((( (Uint16) c1 - (Uint16) p1) << screen->format->Rshift ) &
                screen->format->Rmask) +
             ((( (Uint16) c2 - (Uint16) p2) << screen->format->Gshift ) &
                screen->format->Gmask) +
             ((( (Uint16) c3 - (Uint16) p3) << screen->format->Bshift ) &
                screen->format->Bmask );
          break;
        case 3: // pen_mode_average
          c1 = ( (*ubuff16 & screen->format->Rmask) >> screen->format->Rshift);
          c2 = ( (*ubuff16 & screen->format->Gmask) >> screen->format->Gshift);
          c3 = ( (*ubuff16 & screen->format->Bmask) >> screen->format->Bshift);
          p1 = ( ((Uint16) colour & screen->format->Rmask) >> screen->format->Rshift);
          p2 = ( ((Uint16) colour & screen->format->Gmask) >> screen->format->Gshift);
          p3 = ( ((Uint16) colour & screen->format->Bmask) >> screen->format->Bshift);
          *ubuff16 =
             ((( ( (Uint16) c1 + (Uint16) p1) / 2) << screen->format->Rshift ) &
                screen->format->Rmask) +
             ((( ( (Uint16) c2 + (Uint16) p2) / 2) << screen->format->Gshift ) &
                screen->format->Gmask) +
             ((( ( (Uint16) c3 + (Uint16) p3) / 2) << screen->format->Bshift ) &
                screen->format->Bmask );
          break;
        case 5: // pen_mode_off
          break;
        default: /* pen_mode_copy */
          *ubuff16 = colour16;
        }
        break;
      case 3:
        if (SDL_BYTEORDER == SDL_LIL_ENDIAN ) {
           c1 = (colour & 0xFF0000) >> 16;
           c2 = (colour & 0x00FF00) >> 8;
           c3 = colour & 0x0000FF;
        } else {
           c3 = (colour & 0xFF0000) >> 16;
           c2 = (colour & 0x00FF00) >> 8;
           c1 = colour & 0x0000FF;
        }
        switch (pen_mode) {
        case 0: // pen_mode_invert:
          *ubuff8 = *ubuff8 ^ c3;
          ubuff8[1] = ubuff8[1] ^ c2;
          ubuff8[2] = ubuff8[2] ^ c1;
          break;
        case 1: // pen_mode_brighten:
          *ubuff8 = *ubuff8 + c3;
          ubuff8[1] = ubuff8[1] + c2;
          ubuff8[2] = ubuff8[2] + c1;
          break;
        case 2: // pen_mode_darken:
          *ubuff8 = *ubuff8 - c3;
          ubuff8[1] = ubuff8[1] - c2;
          ubuff8[2] = ubuff8[2] - c1;
          break;
        case 3: // pen_mode_average
          *ubuff8 = (*ubuff8 + c3) / 2;
          ubuff8[1] = (ubuff8[1] + c2) / 2;
          ubuff8[2] = (ubuff8[2] + c1) / 2;
          break;
        case 5: // pen_mode_off
          break;
        default: /* pen_mode_copy */
          *ubuff8 = c3;
          ubuff8[1] = c2;
          ubuff8[2] = c1;
        }
        break;
      case 4:
        switch (pen_mode) {
        case 0: // pen_mode_invert:
          *ubuff32 = *ubuff32 ^ colour;
          break;
        case 1: // pen_mode_brighten: (doesn't work right)
          *ubuff32 = *ubuff32 + colour;
          break;
        case 2: // pen_mode_darken: (doesn't work right)
          *ubuff32 = *ubuff32 - colour;
          break;
        case 3: // pen_mode_average (doesn't work right)
          *ubuff32 = (*ubuff32 + colour)/2;
          break;
        case 5: // pen_mode_off
          break;
        default: /* pen_mode_copy */
          *ubuff32 = colour;
        }
	break;
      default:
         fprintf( stderr, "SDL_EXT_vline unknown pen mode\n" );
      } /* case */
  } /* for */
} /* vline */


/* ------------------------------------------------------------------------ */
/* SDL EXT(ension) RAW PIXEL                                                */
/*                                                                          */
/* Get a single pixel in the current SDL video mode.  Although this could   */
/* be done in Ada, this version avoids the messiness of address-access      */
/* conversions, array bounds and SDL endian macros.                         */
/* ------------------------------------------------------------------------ */

Uint32 SDL_EXT_raw_pixel( SDL_Surface *screen, SDL_Surface *target, Sint16 x, Sint16 y ) {
  Uint8  *ubuff8;
  Uint16 *ubuff16;
  Uint32 *ubuff32;
  Uint8 c1;
  Uint8 c2;
  Uint8 c3;
  switch (screen->format->BytesPerPixel) {
  case 1: // Indexed (palette of 256 colours)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + x;
    SDL_GetRGB( *ubuff8, screen->format, &c1, &c2, &c3 );
    return SDL_MapRGB( target->format, c1, c2, c3 );
  case 2: // Hi colour (Video card dependent components)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*2);
    ubuff16 = (Uint16*) ubuff8;
    SDL_GetRGB( *ubuff16, screen->format, &c1, &c2, &c3 );
    return SDL_MapRGB( target->format, c1, c2, c3 );
  case 3: // True Colour (8-bit components, endian dependent)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*3);
    c1 = *ubuff8;
    c2 = ubuff8[1];
    c3 = ubuff8[2];
    if (SDL_BYTEORDER == SDL_LIL_ENDIAN ) {
       SDL_GetRGB( ( (Uint32)(c1) << 16 ) + ( (Uint32)(c2) << 8 ) + (Uint32)(c3), screen->format, &c1, &c2, &c3 );
    return SDL_MapRGB( target->format, c1, c2, c3 );
    } else {
       SDL_GetRGB( ( (Uint32)(c3) << 16 ) + ( (Uint32)(c2) << 8 ) + (Uint32)(c1), screen->format, &c1, &c2, &c3 );
    return SDL_MapRGB( target->format, c1, c2, c3 );
    }
  case 4: // Direct Colour (3 x 8-bit palette index)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*4);
    ubuff32 = (Uint32 *) ubuff8;
    SDL_GetRGB( *ubuff32, screen->format, &c1, &c2, &c3 );
    return SDL_MapRGB( target->format, c1, c2, c3 );
  }
  fprintf( stderr, "SDL_EXT_pixel unknown mode\n" );
  return 0;
} /* SDL EXT raw pixel */


/* ------------------------------------------------------------------------ */
/* SDL EXT(ension) PIXEL                                                    */
/*                                                                          */
/* Get the RGB values for a pixel in the current SDL video mode.  There is  */
/* no clipping.  Surface must be locked.                                    */
/* ------------------------------------------------------------------------ */

void SDL_EXT_pixel( SDL_Surface *screen, Sint16 x, Sint16 y, Uint8 *red, Uint8 *green, Uint8 *blue ) {
  Uint8  *ubuff8;
  Uint16 *ubuff16;
  Uint32 *ubuff32;
  switch (screen->format->BytesPerPixel) {
  case 1: // Indexed (palette of 256 colours)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + x;
    SDL_GetRGB( *ubuff8, screen->format, red, green, blue );
    break;
  case 2: // Hi colour (Video card dependent components)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*2);
    ubuff16 = (Uint16*) ubuff8;
    SDL_GetRGB( *ubuff16, screen->format, red, green, blue );
    break;
  case 3: // True Colour (8-bit components, endian dependent)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*3);
    if (SDL_BYTEORDER == SDL_LIL_ENDIAN ) {
       *blue = *ubuff8;
       *green = ubuff8[1];
       *red = ubuff8[2];
    } else {
       *red = *ubuff8;
       *green = ubuff8[1];
       *blue = ubuff8[2];
    }
    break;
  case 4: // Direct Colour (3 x 8-bit palette index)
    ubuff8 = (Uint8*) screen->pixels;
    ubuff8 += (y * screen->pitch) + (x*4);
    ubuff32 = (Uint32 *) ubuff8;
    SDL_GetRGB( *ubuff32, screen->format, red, green, blue );
    break;
  default:
     fprintf( stderr, "SDL_EXT_pixel unknown mode\n" );
  }
} /* SDL EXT pixel */

char debug_sdl_error_char( int i ) {
  return SDL_GetError()[i];
}


/* ------------------------------------------------------------------------ */
/* SDL EXT(ension) WINDOW TITLE                                             */
/*                                                                          */
/* Set window title without an icon.  Easier to do this from C than Ada     */
/* since we need a null C pointer.                                          */
/* ------------------------------------------------------------------------ */

void SDL_EXT_window_title( char * title ) {
  SDL_WM_SetCaption( title, NULL );
} /* SDL EXT window title */


/* ------------------------------------------------------------------------ */
/* SDL EXT(ension) SAVE BMP                                                 */
/*                                                                          */
/* Call SDL_SaveBMP (a C macro).                                            */
/* ------------------------------------------------------------------------ */

int SDL_EXT_save_bmp( SDL_Surface *surface, char *path ) {
  return SDL_SaveBMP( surface, path );
} /* SDL EXT save bmp */


/* ------------------------------------------------------------------------ */
/* SDL EXT(ension) GET PIXEL MASKS                                          */
/*                                                                          */
/* Get pixel masks for this hardware for use in creating new software       */
/* surfaces.                                                                */
/* ------------------------------------------------------------------------ */

void SDL_EXT_get_pixel_masks( int res, Uint32 *Rmask, Uint32 *Gmask, Uint32 *Bmask, Uint32 *Amask ) {

#if SDL_BYTEORDER == SDL_BIG_ENDIAN
    *Rmask = 0xff000000;
    *Gmask = 0x00ff0000;
    *Bmask = 0x0000ff00;
    *Amask = 0x000000ff;
#else
    *Rmask = 0x000000ff;
    *Gmask = 0x0000ff00;
    *Bmask = 0x00ff0000;
    *Amask = 0xff000000;
#endif
}

/* end of c_os.c */
