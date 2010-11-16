#!/usr/local/bin/bush

-- From Rosetta Code

pragma annotate( "charcode" );
pragma annotate( "" );
pragma annotate( "Given a character value in your language, print its code (could be" );
pragma annotate( "ASCII code, Unicode code, or whatever your language uses). For example," );
pragma annotate( "the character 'a' (lowercase letter A) has a code of 97 in ASCII (as" );
pragma annotate( "well as Unicode, as ASCII forms the beginning of Unicode). Conversely," );
pragma annotate( "given a code, print out the corresponding character. " );
pragma annotate( "by Ken O. Burtch");

pragma restriction( no_external_commands );

procedure charcode is
  code : natural := 97;
  char : character := 'a';
begin
  put_line( "character code" & strings.image( code ) & " = character " & strings.val( code ) );
  put_line( "character " & char & " = character code" & strings.image( numerics.pos( char ) ) );
end charcode;

