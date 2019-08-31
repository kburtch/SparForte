/* C code for functions not covered in spar_os.ads          */
/* Part of SparForte                                        */
/*                                                          */
/* I didn't want to resort to C, but I need to use stat()   */
/* and manually declaring the structure in Ada is extremely */
/* unportable between UNIXes -- KB                          */

#include <string.h>
#include <pcre.h>

void C_pcre( const char *regex, const char* str, char *errmsg, size_t errmax,
   int *result ) {
   pcre *regex_ptr;
   char no_error = '\0';
   const char *errmsg_ptr = &no_error;
   int erroffset = 0;            // location of error in pattern
   int res = 0;

   /* For Perl-compatible Regular Expressions, there are UTF-8, 16
    * and 32 variants and this may also affect the parameters.
    * for here, this is UTF-8 (even though, at this time,
    * SparForte is Latin-1 only).
    */

  *result = 0;
  *errmsg = no_error;

   regex_ptr = pcre_compile(regex, 0, &errmsg_ptr, &erroffset, NULL );
   if (regex_ptr != NULL ) {
      // This should not typically fail.  It does not return an
      // error message, just a negative error code.
      res = pcre_exec( regex_ptr, NULL, str, strlen(str), 0, 0, NULL, 0);
      if ( res >= 0 ) {
         *result = 1;
      } else if ( res == PCRE_ERROR_NULL ) {
         strncpy( errmsg, "pcre_exec failed: null error", 255 );
      } else if ( res == PCRE_ERROR_BADOPTION ) {
         strncpy( errmsg, "pcre_exec failed: bad option", 255 );
      } else if ( res == PCRE_ERROR_BADMAGIC ) {
         strncpy( errmsg, "pcre_exec failed: bad magic number", 255 );
      } else if ( res == PCRE_ERROR_UNKNOWN_NODE ) {
         strncpy( errmsg, "pcre_exec failed: unknown_node", 255 );
      } else if ( res == PCRE_ERROR_NOMEMORY ) {
         strncpy( errmsg, "pcre_exec failed: no memory", 255 );
      } else if ( res != PCRE_ERROR_NOMATCH ) {
         strncpy( errmsg, "pcre_exec failed", 255 );
         *errmsg = no_error;
      }
      pcre_free( regex_ptr );
   } else {
      strncpy( errmsg, errmsg_ptr, 255 );
   }
   // Safety precaution to prevent buffer overruns.
   errmsg[255] = '\0';
}

/* end of c_pcre.c */
