#!/usr/local/bin/spar

pragma annotate( summary, "charcode" );
pragma annotate( description, "Given a character value in your language, print its code (could be" );
pragma annotate( description, "ASCII code, Unicode code, or whatever your language uses). For example," );
pragma annotate( description, "the character 'a' (lowercase letter A) has a code of 97 in ASCII (as" );
pragma annotate( description, "well as Unicode, as ASCII forms the beginning of Unicode). Conversely," );
pragma annotate( description, "given a code, print out the corresponding character. " );
pragma annotate( see_also, "http://rosettacode.org/wiki/Character_codes" );
pragma annotate( author, "Ken O. Burtch");
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure charcode is
  code : natural := 97;
  char : character := 'a';
begin
  put_line( "character code" & strings.image( code ) & " = character " & strings.val( code ) );
  put_line( "character " & char & " = character code" & strings.image( numerics.pos( char ) ) );
end charcode;

-- VIM editor formatting instructions
-- vim: ft=spar

