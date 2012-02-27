#!/usr/local/bin/spar

pragma annotate( summary, "printargs" );
pragma annotate( description, "Retrieve the list of command-line arguments given to the program." );
pragma annotate( description, "Example command line: " );
pragma annotate( description, "myprogram -c 'alpha beta' -h 'gamma'" );
pragma annotate( see_also, "http://rosettacode.org/wiki/Command-line_arguments" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure printargs is
begin
   put_line( "The command is '" & command_line.command_name & "'" );
   for Arg in 1..command_line.argument_count loop
      put( "Argument" ) @ (Arg ) @ ( " is '" ) @
         ( command_line.argument(Arg) ) @ ("'");
      new_line;
   end loop;
end printargs;

-- VIM editor formatting instructions
-- vim: ft=spar

