#!/usr/local/bin/spar

pragma annotate( summary, "printargs" )
              @( description, "Retrieve the list of command-line arguments given to the program." )
              @( description, "Example command line: " )
              @( description, "myprogram -c 'alpha beta' -h 'gamma'" )
              @( category, "tutorials" )
              @( author, "Ken O. Burtch" )
              @( see_also, "http://rosettacode.org/wiki/Command-line_arguments" );
pragma license( unrestricted );

pragma software_model( nonstandard );
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

