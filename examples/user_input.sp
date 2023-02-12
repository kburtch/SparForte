#!/usr/local/bin/spar

pragma annotate( summary, "get_string" )
       @( description, "Input a string and the integer 75000 from the text console." )
       @( category, "tutorials" )
       @( see_also, "https://rosettacode.org/wiki/User_input/Text" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure user_input is
  s : unbounded_string;
  i : integer;
begin
  s := get_line;
  i := integer( numerics.value( get_line ) );
  ? s @ i;
exception when others =>
  put_line( standard_error, "the value is not valid" );
end user_input;

-- VIM editor formatting instructions
-- vim: ft=spar
