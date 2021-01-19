#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

// sudo apt-get install libreadline-dev
// gcc -o rl rl.c -lreadline

// Bash readline support is 4500 lines of code.

// completion functions (as defined in Ada)

extern char * Ada_executable_word_generator(const char *text, int state);
extern char * Ada_assignment_word_generator(const char *text, int state);
extern char * Ada_parameter_word_generator(const char *text, int state);
extern char * Ada_git_word_generator(const char *text, int state);
extern char * Ada_ipset_word_generator(const char *text, int state);
extern char * Ada_svn_word_generator(const char *text, int state);
extern char * Ada_yum_word_generator(const char *text, int state);

const int completion_type_none       = -1;
const int completion_type_default    = 0;
const int completion_type_command    = 1;
const int completion_type_assignment = 2;
const int completion_type_parameter  = 3;
const int completion_type_git        = 4;
const int completion_type_svn        = 5;
const int completion_type_ipset      = 6;
const int completion_type_yum        = 7;
const int completion_type_apt        = 8;

//char ** sparforte_completion( const char *text, int start, int end, int qc, int compflags) {
char ** sparforte_completion( const char *text, int start, int end ) {
    char **matches;
    int  text_idx;
    int  assign_idx;
    int  git_idx;
    const char git_str[5] = "git ";
    const char ipset_str[7] = "ipset ";
    const char svn_str[5] = "svn ";
    const char yum_str[5] = "yum ";
    const char apt_str[5] = "apt ";

    int completion_type = completion_type_none;

    // printf( "executable_completion\n" ); // DEBUG

    // start with no matches

    matches = (char **)NULL;

    // Assignment completion

    // Is this a command?
    // Is there nothing but whitespace in front of it?  idx will be -1.

    text_idx = start - 1;
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

    // If it's the first word or if it follows a pipe symbol,
    // treat it as a command (though it could be an assignment).

    if (text_idx < 0) {
       completion_type = completion_type_command;
    } else if ( rl_line_buffer[text_idx] == '|' ) {
       completion_type = completion_type_command;
    }

    // Assignment completion

    if (completion_type == completion_type_none) {
       assign_idx = end - 1;
       while ( (assign_idx > -1) && (rl_line_buffer[assign_idx] != ':') ) {
          assign_idx--;
       }
       if (assign_idx >= 0) {
          if (rl_line_buffer[assign_idx+1] == '=') {
             completion_type = completion_type_assignment;
          }
       }
    }

    // Git subcommand completion: a special case.

    if (completion_type == completion_type_none) {
       git_idx = 0;
       while ( (git_idx<strlen(git_str) ) &&
          ( rl_line_buffer[git_idx] == git_str[git_idx]) ) {
            git_idx++;
       }
       if (git_idx==strlen(git_str)) {
          completion_type = completion_type_git;
       }
    }

    // Svn subcommand completion: a special case.

    if (completion_type == completion_type_none) {
       git_idx = 0;
       while ( (git_idx<strlen(svn_str) ) &&
          ( rl_line_buffer[git_idx] == svn_str[git_idx]) ) {
            git_idx++;
       }
       if (git_idx==strlen(svn_str)) {
          completion_type = completion_type_svn;
       }
    }

    // Ipset subcommand completion: a special case.

    if (completion_type == completion_type_none) {
       git_idx = 0;
       while ( (git_idx<strlen(ipset_str) ) &&
          ( rl_line_buffer[git_idx] == ipset_str[git_idx]) ) {
            git_idx++;
       }
       if (git_idx==strlen(ipset_str)) {
          completion_type = completion_type_ipset;
       }
    }

    // Yum subcommand completion: a special case.

    if (completion_type == completion_type_none) {
       git_idx = 0;
       while ( (git_idx<strlen(yum_str) ) &&
          ( rl_line_buffer[git_idx] == yum_str[git_idx]) ) {
            git_idx++;
       }
       if (git_idx==strlen(yum_str)) {
          completion_type = completion_type_yum;
       }
    }

    // Apt subcommand completion: a special case.

    if (completion_type == completion_type_none) {
       git_idx = 0;
       while ( (git_idx<strlen(apt_str) ) &&
          ( rl_line_buffer[git_idx] == apt_str[git_idx]) ) {
            git_idx++;
       }
       if (git_idx==strlen(apt_str)) {
          completion_type = completion_type_apt;
       }
    }

    // If no choice was made, fall back to GNU readline default.

    if (completion_type == completion_type_none) {
       completion_type = completion_type_default;
    }

    // Debug

    //if ( (matches == 0) && (is_command) ) {
    //   printf( "[command?]\n");
    //}

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
       matches = rl_completion_matches(text, Ada_git_word_generator); //////
    } else { // case completion_type_default:
       matches = ( (char **) NULL );
       rl_attempted_completion_over = 0;
    }

    return matches;
}

char *C_strdup( const char *s ) {
	return strdup( s );
}

void C_init_readline() {
  // This identifies SparForte configurations in inputrc files

  rl_readline_name = "SparForte";

  // enable history

  using_history();

}

void C_readline( char *term, char *prompt, char **ada_buffer) {
  char *buffer = NULL;

  // This defines the quote characters for breaking up a line
  // for completion.

  rl_completer_quote_characters = "\"'";

  // The alternative SparForte function to match words for
  // completion.

  rl_attempted_completion_function = sparforte_completion;

  // rl_terminal_name should be set before each call so that TERM env var
  // changes take effect.

  // RL_PROMPT_START_IGNORE (001) and RL_PROMPT_END_IGNORE (002)

  buffer = readline( prompt );
  if (buffer) {
     // printf("You entered: %s\n", buffer);
     *ada_buffer = buffer;
     // add non-empty lines to history
     if ( strlen(buffer) > 0 ) {
        add_history( buffer );
     }
  }
}

void C_free_readline(char *ada_buffer) {
  free( ada_buffer);
}


