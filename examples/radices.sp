#!/usr/local/bin/bush

pragma annotate( "radices" );
pragma annotate( "" );
pragma annotate( "This task requires parsing of such a string (which may" );
pragma annotate( "be assumed to contain nothing else) using the" );
pragma annotate( "language's built-in facilities if possible. Parsing of" );
pragma annotate( "decimal strings is required, parsing of other formats" );
pragma annotate( "is optional but should be shown (i.e., if the language" );
pragma annotate( "can parse in base-19 then that should be illustrated)." );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Non-decimal_radices/Input" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure radices is
begin
  ? numerics.value( "16#ABCF123#" );
  ? numerics.value( "8#7651#" );
  ? numerics.value( "2#1010011010#" );
  ? numerics.value( "16#F.FF#E+2" );
end radices;

