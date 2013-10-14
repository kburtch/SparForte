#!/usr/local/bin/spar

pragma annotate( summary, "incstr" );
pragma annotate( description, "Increment an integer number in a string" );
pragma annotate( see_also, "http://rosettacode.org/wiki/Increment_a_numerical_string" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure incstr is
   s : string := "12345";
begin
   s := strings.trim( strings.image( integer( numerics.value( s ) + 1 ) ), trim_end.both ) ;
   ? s;
end incstr;

-- VIM editor formatting instructions
-- vim: ft=spar

