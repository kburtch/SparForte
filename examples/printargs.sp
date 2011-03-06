#!/usr/local/bin/bush

-- A common interview test from Rosetta Code for testing basic programming
-- skills.

pragma annotate( "printargs" );
pragma annotate( "" );
pragma annotate( "Retrieve the list of command-line arguments given to the program." );
pragma annotate( "Example command line: " );
pragma annotate( "myprogram -c 'alpha beta' -h 'gamma'" );
pragma annotate( "translated by Ken O. Burtch" );

procedure printargs is
begin
   put_line( "The command is '" & command_line.command_name & "'" );
   for Arg in 1..command_line.argument_count loop
      put( "Argument" ) @ (Arg ) @ ( " is '" ) @
         ( command_line.argument(Arg) ) @ ("'");
      new_line;
   end loop;
end printargs;

