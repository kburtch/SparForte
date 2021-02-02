#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

// completion functions (as defined in Ada)

extern char * Ada_executable_word_generator(const char *text, int state);
extern char * Ada_assignment_word_generator(const char *text, int state);
extern char * Ada_variable_word_generator(const char *text, int state);
extern char * Ada_parameter_word_generator(const char *text, int state);
extern char * Ada_git_word_generator(const char *text, int state);
extern char * Ada_ipset_word_generator(const char *text, int state);
extern char * Ada_svn_word_generator(const char *text, int state);
extern char * Ada_yum_word_generator(const char *text, int state);
extern char * Ada_apt_word_generator(const char *text, int state);
extern char * Ada_docker_word_generator(const char *text, int state);
extern char * Ada_k8s_word_generator(const char *text, int state);

// types of completions

const int completion_type_none       = -1;
const int completion_type_default    =  0;
const int completion_type_command    =  1;
const int completion_type_assignment =  2;
const int completion_type_parameter  =  3;
const int completion_type_variable   =  4;
const int completion_type_git        =  5;
const int completion_type_svn        =  6;
const int completion_type_ipset      =  7;
const int completion_type_yum        =  8;
const int completion_type_apt        =  9;
const int completion_type_docker     = 10;
const int completion_type_k8s        = 11;

/**
 *  SPARFORTE COMPLETION
 *
 * This function is the callback for GNU readline which tries to
 * determine which word generation function to call in Ada.  The
 * word generators produce the list of completion words.
 */

char ** sparforte_completion( const char *text, int start, int end ) {
    char **matches;
    int  text_idx      = -9999;
    int  text_word_cnt = -9999;
    //int  text_first    = -9999;
    int  in_squote     = -9999;
    int  in_dquote     = -9999;
    int  in_bquote     = -9999;
    int  in_whspace    = -9999;
    int  in_backslash  = -9999;
    char ch;
    // Commands with autocomplete of subcommands
    const char git_str[5]    = "git ";
    const char ipset_str[7]  = "ipset ";
    const char svn_str[5]    = "svn ";
    const char yum_str[5]    = "yum ";
    const char apt_str[5]    = "apt ";
    const char docker_str[8] = "docker ";
    const char kubernetes_str[9] = "kubectl ";
    const char oc_str[4]     = "oc ";

    int completion_type = completion_type_none;

    // start with no matches

    matches = (char **)NULL;

    // Assignment completion

    text_word_cnt = 0;
    //text_first = 0;
    text_idx = 0;
    in_squote  = 0;
    in_dquote  = 0;
    in_bquote  = 0;
    in_whspace = 0;
    in_backslash = 0;

    // Count the number of words on the line
    // Note: process substitution not included in word counting

    if ( !((start == 0) && (end == 0)) ) {
      while ( text_idx < start ) {

        ch = rl_line_buffer[text_idx];
        if ( ch == '`' ) {
           if ( (!in_squote) && (!in_backslash) ) { // backquote
              in_bquote = !in_bquote;
           }
           in_backslash = 0;
           in_whspace = 0;
        } else if ( ch == '"' ) { // double quote
           if ( (!in_squote) && (!in_backslash) ) {
              in_dquote = !in_dquote;
           }
           in_backslash = 0;
           in_whspace = 0;
        } else if ( ch == '\'' ) { // single quote
           if ( (!in_dquote) && (!in_backslash) ) {
              in_squote = !in_squote;
           }
           in_backslash = 0;
           in_whspace = 0;
        } else if ( ch=='\\' ) {
           if ( (!in_squote) && (!in_backslash) ) {
              in_backslash = 1;
           } else {
              in_backslash = 0;
           }
        } else if ( (ch==' ') || (ch=='\t') ) { // whitespace
           if ( (!in_whspace) && (!in_squote) && (!in_dquote) && (!in_bquote) && (!in_backslash) ) {
              text_word_cnt++;
//printf( "%s\n", "word at ws" ); // DEBUG
           }
           in_backslash = 0;
           in_whspace = 1;
        } else if ( ch == ':' ) {
           if ( (!in_squote) && (!in_dquote) && (!in_bquote) && (!in_backslash) ) {
              if ( text_idx < end ) {
                 if ( rl_line_buffer[text_idx+1] == '=' ) {
                    completion_type = completion_type_assignment;
                 }
              }
           }
           in_backslash = 0;
           in_whspace = 0;
        } else {  // other character
           in_backslash = 0;
           in_whspace = 0;
        } // if
        text_idx++;
      } // while
      text_word_cnt++;
//printf( "%s\n", "word at end" ); // DEBUG
    } // if not empty

/*
    printf("%s %d\n", "start          ", start ); // DEBUG
    printf("%s %d\n", "end            ", end ); // DEBUG
    printf("%s %d\n", "text_idx       ", text_idx ); // DEBUG
    printf("%s %d\n", "text_first     ", text_first ); // DEBUG
    printf("%s %d\n", "text_word_count", text_word_cnt ); // DEBUG
    printf("%s %d\n", "in_dquote      ", in_dquote ); // DEBUG
    printf("%s %d\n", "in_squote      ", in_squote ); // DEBUG
    printf("%s %d\n", "in_bquote      ", in_bquote ); // DEBUG
    printf("%s %d\n", "in_whspace     ", in_bquote ); // DEBUG
*/

    // Is this a command?
    // Is there nothing but whitespace in front of it?  idx will be -1.

    text_idx = start - 1;

    // Skip whitespace

    while ( (text_idx > -1) &&
            ( rl_line_buffer[text_idx] == ' ' ||
              rl_line_buffer[text_idx] == '\t' ) ) {
      text_idx--;
    }

    // What if it's a single or double quoted command?

    if ( (text_idx >= 0) &&
        (rl_line_buffer[text_idx] == '"' ||
         rl_line_buffer[text_idx] == '\'')) {
      text_idx--;
      while (text_idx > -1 &&
            ( rl_line_buffer[text_idx] == ' ' ||
              rl_line_buffer[text_idx] == '\t' ) ) {
        text_idx--;
      }
    }

    //printf("%s %d\n", "first is?      ", text_idx ); // DEBUG

    // If it's the first word or if it follows a pipe symbol,
    // treat it as a command (though it could be an assignment).

    if (text_idx < 0) {
       completion_type = completion_type_command;
    } else if ( rl_line_buffer[text_idx] == '|' ) {
       completion_type = completion_type_command;
    } else if ( rl_line_buffer[text_idx] == '$' ) {
       completion_type = completion_type_variable;
    } else if ( rl_line_buffer[text_idx] == '{' ) {
      if (text_idx > 0) {
         if ( rl_line_buffer[text_idx-1] == '$' ) {
            completion_type = completion_type_variable;
         }
      }
    } else if ( rl_line_buffer[text_idx] == '(' ) {
      if (text_idx > 0) {
         if ( rl_line_buffer[text_idx-1] == '$' ) {
            completion_type = completion_type_command;
         }
      }
    }

    // Git subcommand completion: a special case.

    if (text_word_cnt == 2) {
       if (completion_type == completion_type_none) {
          if ( strncmp( rl_line_buffer, git_str, strlen(git_str) ) == 0 ) {
             completion_type = completion_type_git;
          }
       }

       // Svn subcommand completion: a special case.

       if (completion_type == completion_type_none) {
          if ( strncmp( rl_line_buffer, svn_str, strlen(svn_str) ) == 0 ) {
             completion_type = completion_type_svn;
          }
       }

       // Ipset subcommand completion: a special case.

       if (completion_type == completion_type_none) {
          if ( strncmp( rl_line_buffer, ipset_str, strlen(ipset_str) ) == 0 ) {
             completion_type = completion_type_ipset;
          }
       }

       // Yum subcommand completion: a special case.

       if (completion_type == completion_type_none) {
          if ( strncmp( rl_line_buffer, yum_str, strlen(yum_str) ) == 0 ) {
             completion_type = completion_type_yum;
          }
       }

       // Apt subcommand completion: a special case.

       if (completion_type == completion_type_none) {
          if ( strncmp( rl_line_buffer, apt_str, strlen(apt_str) ) == 0 ) {
             completion_type = completion_type_apt;
          }
       }

       // Docker subcommand completion: a special case.

       if (completion_type == completion_type_none) {
          if ( strncmp( rl_line_buffer, docker_str, strlen(docker_str) ) == 0 ) {
             completion_type = completion_type_docker;
          }
       }

       // Kubernetes subcommand completion: a special case.

       if (completion_type == completion_type_none) {
          if ( strncmp( rl_line_buffer, kubernetes_str, strlen(kubernetes_str) ) == 0 ) {
             completion_type = completion_type_k8s;
          }
       }

       // Openshift subcommand completion: a special case.

       if (completion_type == completion_type_none) {
          if ( strncmp( rl_line_buffer, oc_str, strlen(oc_str) ) == 0 ) {
             completion_type = completion_type_k8s;
          }
       }

	} // word count 2

    // If no choice was made, fall back to some kind of parameter.
    // Note: this does not distinguish between an AdaScript
    // or Bourne shell command so anything that might apply
    // will be shown.

    if (completion_type == completion_type_none) {
       completion_type = completion_type_parameter;
    }

    // Perform completion match search
    // By default, do not attempt default file matching.
    // C does not allow switch statement using run-time values.

    rl_attempted_completion_over = 1;

    if (completion_type == completion_type_command) {
       matches = rl_completion_matches(text, Ada_executable_word_generator);
    } else if (completion_type == completion_type_assignment) {
       matches = rl_completion_matches(text, Ada_assignment_word_generator);
    } else if (completion_type == completion_type_parameter) {
       matches = rl_completion_matches(text, Ada_parameter_word_generator);
    } else if (completion_type == completion_type_git) {
       matches = rl_completion_matches(text, Ada_git_word_generator);
    } else if (completion_type == completion_type_ipset) {
       matches = rl_completion_matches(text, Ada_ipset_word_generator);
    } else if (completion_type == completion_type_svn) {
       matches = rl_completion_matches(text, Ada_svn_word_generator);
    } else if (completion_type == completion_type_yum) {
       matches = rl_completion_matches(text, Ada_yum_word_generator);
    } else if (completion_type == completion_type_apt) {
       matches = rl_completion_matches(text, Ada_apt_word_generator);
    } else if (completion_type == completion_type_variable) {
       matches = rl_completion_matches(text, Ada_variable_word_generator);
    } else if (completion_type == completion_type_docker) {
       matches = rl_completion_matches(text, Ada_docker_word_generator);
    } else if (completion_type == completion_type_k8s) {
       matches = rl_completion_matches(text, Ada_k8s_word_generator);
    } else {
       // if something goes wrong, case completion_type_default will
       // trigger the GNU readline built-in completion.
       matches = ( (char **) NULL );
       rl_attempted_completion_over = 0;
    }

    return matches;
}


/**
 *  C STRDUP (called from Ada)
 *
 * A binding to the strdup() function because readline frees strings
 * allocated by STRDUP.  This is here for portability but might be
 * better defined in the spar_os package.
 */

char *C_strdup( const char *s ) {
	return strdup( s );
}


/**
 *  BACKSLASH CHECK
 *
 * Taken from article on tab completion.  The solution must be recursive
 * as backslashes can escape other backslashes.
 */

int backslash_check( char *line, int index ) {
    return (
        index > 0 &&
        line[index - 1] == '\\' &&
        !backslash_check(line, index - 1)
    );
}


/**
 *  C INIT READLINE (called from Ada)
 *
 * Initialize the GNU readline library.
 */

void C_init_readline() {
  // This identifies SparForte configurations in inputrc files

  rl_readline_name = "SparForte";

  // enable history

  using_history();

  // Default comment in AdaScript

  rl_variable_bind( "comment-begin", "--" );

  // Tilde expansion (not sure if this is necessary)
  rl_variable_bind( "expand-tilde", "on" );

  // This defines the quote characters for breaking up a line
  // for completion.

  rl_completer_quote_characters = "\"'";

  // The alternative SparForte function to match words for
  // completion.

  rl_attempted_completion_function = sparforte_completion;

  // Detecting a backslash escape
  // Remove backslash from word break characters.

  rl_completer_word_break_characters = " ";
  rl_char_is_quoted_p = &backslash_check;
}


/**
 *  C READLINE (called from Ada)
 *
 * Read a line of text.  ada_buffer will have the line of text, which must
 * be later deallocated by free_readline.
 */

void C_readline( char *term, char *prompt, char **ada_buffer, int keep_history) {
  char *buffer = NULL;

  // rl_terminal_name should be set before each call so that TERM env var
  // changes take effect.  Also, a failure to set this at all will cause
  // readline to default to horizontal scrolling mode.

  rl_terminal_name = term;

  // If we are keeping history, we are the command prompt.  At the
  // prompt, enable completion and blinking parenthesis matching.

  if (keep_history) {
     // Blink the matching parenthesis
     rl_variable_bind( "blink-matching-paren", "on" );
     // Do completions
     rl_variable_bind( "disable-completion", "off" );
  } else {
     // Do not blink the matching parenthesis
     rl_variable_bind( "blink-matching-paren", "off" );
     // Do not do completions
     rl_variable_bind( "disable-completion", "on" );
  }

  // Read. Add non-empty lines to gnu readline's history

  buffer = readline( prompt );
  if (buffer) {
     *ada_buffer = buffer;
     if ( strlen(buffer) > 0 ) {
        add_history( buffer );
     }
  }
}


/**
 *  C FREE READLINE (called from Ada)
 *
 * Deallocate the string in the ada_buffer.
 */

void C_free_readline(char *ada_buffer) {
  free( ada_buffer);
}


