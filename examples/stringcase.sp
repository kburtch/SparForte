#!/usr/local/bin/spar

pragma annotate( summary, "stringcase" );
pragma annotate( description, "Take the string 'alphaBETA', and demonstrate how to" );
pragma annotate( description, "convert it to UPPER-CASE and lower-case. Use the" );
pragma annotate( description, "default encoding of a string literal or plain ASCII if" );
pragma annotate( description, "there is no string literal in your language. Show any" );
pragma annotate( description, "additional case conversion functions (e.g. swapping" );
pragma annotate( description, "case, capitalizing the first letter, etc.) that may be" );
pragma annotate( description, "included in the library of your language. " );
pragma annotate( see_also, "http://rosettacode.org/wiki/String_case" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure stringcase is
  s : constant string := "alphaBETA";
begin
  ? strings.to_upper( s );
  ? strings.to_lower( s );
  ? strings.to_proper( s );
end stringcase;

-- VIM editor formatting instructions
-- vim: ft=spar

