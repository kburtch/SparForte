/* -------------------------------------------- */
/* scripting.c                                  */
/*                                              */
/* An example of using BUSH as a scripting      */
/* language for a C program.                    */
/* -------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

int main() {
  char i_string[255];
  int i;
  FILE *f;

  /* assign a value to i */

  i = 5;

  /* export i */

  sprintf( i_string, "i_string=%d", i );     // convert i to a string
  if ( putenv( i_string ) != 0 )              // add i to the environment
     printf( "putenv i_string failed: %s\n", strerror( errno ) );

  /* Create the script to run */

  f = fopen( "scripting_example.bush", "w" );
  fprintf( f, "%s\n", "pragma restriction( no_external_commands );" );
  fprintf( f, "%s\n", "pragma ada_95;" );
  fprintf( f, "%s\n", "procedure scripting_example is" );
  fprintf( f, "%s\n", "i : integer := numerics.value( i_string );" );
  fprintf( f, "%s\n", "begin" );
  fprintf( f, "%s\n", "put_line( i * 2 );" );
  fprintf( f, "%s\n", "end scripting_example;" );
  fclose( f );

  /* Run the script.  If successful, delete script */

  if ( system( "../bush scripting_example.bush" ) != 0 ) {
     printf( "Oh, no.  There was an error in the script\n" );
  } else {
     unlink( "scripting_example.bush" );
  }
  return 0;
}

