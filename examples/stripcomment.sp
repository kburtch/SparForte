#!/usr/local/bin/bush

pragma annotate( "stripcomment" );
pragma annotate( "" );
pragma annotate( "The task is to remove text that follow any of a set of" );
pragma annotate( "comment markers, (in these examples either a hash or a" );
pragma annotate( "semicolon) from a string or input line." );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Strip_comments_from_a_string" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure stripcomment is
  line : string := get_line;
  eol  : natural := 0;
  ch   : character;
begin
  for i in 1..strings.length( line ) loop
    ch := strings.element( line, i );
    exit when ch = '#' or ch = ';';
    eol := i;
  end loop;
  if eol > 0 then
     ? strings.slice( line, 1, eol );
  end if;
end stripcomment;

