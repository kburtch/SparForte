// FREEBSD possibly should be __FreeBSD__
#ifdef __FreeBSD__
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

#include <stdio.h>

#undef main // Workaround for Darwin

int main() {
  int palsize = sizeof( SDL_Palette );

  if ( palsize == 16 ) {               // looks like 64-bit alignment?
     printf( "%s\n", "64" );
  } else if ( palsize == 8 ) {         // looks like 32-bit alignment?
     printf( "%s\n", "32" );
  } else if ( palsize == 24 ) {        // Darwin, looks like 64-bit alignment?
     printf( "%s\n", "64" );
  } else {                             // unknown
     printf( "%s\n", "0" );
  }

  return 0;
}
