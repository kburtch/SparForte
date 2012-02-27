#!/usr/local/bin/spar

pragma annotate( summary, "radices" );
pragma annotate( description, "This task requires parsing of such a string (which may" );
pragma annotate( description, "be assumed to contain nothing else) using the" );
pragma annotate( description, "language's built-in facilities if possible. Parsing of" );
pragma annotate( description, "decimal strings is required, parsing of other formats" );
pragma annotate( description, "is optional but should be shown (i.e., if the language" );
pragma annotate( description, "can parse in base-19 then that should be illustrated)." );
pragma annotate( see_also, "http://rosettacode.org/wiki/Non-decimal_radices/Input" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure radices is
begin
  ? numerics.value( "16#ABCF123#" );
  ? numerics.value( "8#7651#" );
  ? numerics.value( "2#1010011010#" );
  ? numerics.value( "16#F.FF#E+2" );
end radices;

-- VIM editor formatting instructions
-- vim: ft=spar

