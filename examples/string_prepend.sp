#!/usr/local/bin/spar

pragma annotate( summary, "string_prepend" )
       @( description, "Create a string variable equal to any text value." )
       @( description, "" )
       @( description, "Prepend the string variable with another string " )
       @( description, "literal." )
       @( description, "" )
       @( description, "If your language supports any idiomatic ways to " )
       @( description, "do this without referring to the variable twice " )
       @( description, "in one expression, include such solutions." )
       @( description, "" )
       @( description, "To illustrate the operation, show the content of " )
       @( description, "the variable." )
       @( category, "tutorials" )
       @( author, "Ken O. Burtch" )
       @( see_also, "http://rosettacode.org/wiki/String_prepend" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure string_prepend is
  world : constant string := "World!";
  hello : constant string := "Hello ";
  s : string;
begin
  -- Using concatenation
  s := world;
  s := hello & @;
  ? s;

  -- Using strings library
  s := world;
  s := strings.insert( @, 1, hello );
  ? s;

  command_line.set_exit_status( 0 );
end string_prepend;

-- VIM editor formatting instructions
-- vim: ft=spar

