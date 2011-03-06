#!/usr/local/bin/bush

pragma annotate( "stringcase" );
pragma annotate( "" );
pragma annotate( "Take the string 'alphaBETA', and demonstrate how to" );
pragma annotate( "convert it to UPPER-CASE and lower-case. Use the" );
pragma annotate( "default encoding of a string literal or plain ASCII if" );
pragma annotate( "there is no string literal in your language. Show any" );
pragma annotate( "additional case conversion functions (e.g. swapping" );
pragma annotate( "case, capitalizing the first letter, etc.) that may be" );
pragma annotate( "included in the library of your language. " );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/String_case" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure stringcase is
  s : constant string := "alphaBETA";
begin
  ? strings.to_upper( s );
  ? strings.to_lower( s );
  ? strings.to_proper( s );
end stringcase;

