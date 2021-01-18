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
extern char * Ada_git_word_generator(const char *text, int state);
extern char * Ada_parameter_word_generator(const char *text, int state);

//char ** sparforte_completion( const char *text, int start, int end, int qc, int compflags) {
char ** sparforte_completion( const char *text, int start, int end ) {
    char **matches;
    int  text_idx;
    int  is_command;
    int  assign_idx;
    int  is_assign;
    int  is_git;
    int  git_idx;
    const char git_str[5] = "git ";
    //char ch;

    // printf( "executable_completion\n" ); // DEBUG

    // start with no matches

    matches = (char **)NULL;

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

    is_command = 0;
    if (text_idx < 0) {
       is_command = 1;
    } else if ( rl_line_buffer[text_idx] == '|' ) {
       is_command = 1;
    }

    // Is it an assignment

    is_assign = 0;
    if (!is_command) {
       assign_idx = end - 1;
       while ( (assign_idx > -1) && (rl_line_buffer[assign_idx] != ':') ) {
          assign_idx--;
       }
       if (assign_idx >= 0) {
          is_assign = (rl_line_buffer[assign_idx+1] == '=');
       }
    }

    // Is it a Git command?

    is_git = 0;
    if ((!is_command) && (!is_assign)) {
       git_idx = 0;
       while ( (git_idx<strlen(git_str) ) &&
          ( rl_line_buffer[git_idx] == git_str[git_idx]) ) {
            git_idx++;
       }
       if (git_idx==strlen(git_str)) {
          is_git = 1;
       }
    }

    // Debug

    //if ( (matches == 0) && (is_command) ) {
    //   printf( "[command?]\n");
    //}

    // Perform completion match search
    // By default, do not attempt default file matching.

    rl_attempted_completion_over = 1;

    if (is_command) {
       matches = rl_completion_matches(text, Ada_executable_word_generator);
    } else if (is_assign) {
       matches = rl_completion_matches(text, Ada_assignment_word_generator);
    } else if (is_git) {
       matches = rl_completion_matches(text, Ada_git_word_generator);
    } else {
       matches = rl_completion_matches(text, Ada_parameter_word_generator);
       // If not a command, Use the default file matching.
       // matches = ( (char **) NULL );
       // rl_attempted_completion_over = 0;
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


