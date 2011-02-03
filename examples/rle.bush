#!/usr/local/bin/bush

pragma annotate( "rle" );
pragma annotate( "" );
pragma annotate( "Given a string containing uppercase characters (A-Z)," );
pragma annotate( "compress repeated 'runs' of the same character by" );
pragma annotate( "storing the length of that run, and provide a function to" );
pragma annotate( "reverse the compression. The output can be anything, as" );
pragma annotate( "long as you can recreate the input with it." );
pragma annotate( "" );
pragma annotate( "Example:" );
pragma annotate( "Input: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW" );
pragma annotate( "Output: 12W1B12W3B24W1B14W" );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Run-length_encoding" );
pragma annotate( "by Ken O. Burtch" );

procedure rle is

  function to_rle( s : string ) return string is
  begin
    if strings.length( s ) = 0 then
       return "";
    end if;
    declare
      result : string;
      code   : character;
      prefix : string;
      first  : natural := 1;
      index  : natural := 1;
    begin
      while index <= strings.length( s ) loop
        first := index;
        index := @+1;
        code := strings.element( s, positive(first) );
        while index <= strings.length( s ) loop
          exit when code /= strings.element( s, positive(index) );
          index := @+1;
          exit when index-first = 99;
        end loop;
        prefix := strings.trim( strings.image( index - first ), trim_end.left ); 
        result := @ & prefix & code;
      end loop;
      return result;
    end;
  end to_rle;

  function from_rle( s : string ) return string is
  begin
    if strings.length( s ) = 0 then
       return "";
    end if;
    declare
      result : string;
      index  : positive := 1;
      prefix : string;
      code : character;
    begin
      loop
        prefix := "" & strings.element( s, index );
        index := @+1;
        if strings.is_digit( strings.element( s, index ) ) then
          prefix := @ & strings.element( s, index );
          index := @+1;
        end if;
        code := strings.element( s, index );
        index := @+1;
        result := @ & ( numerics.value( prefix ) * code );
        exit when natural(index) > strings.length( s );
      end loop;
      return result;
    end;
  end from_rle;

begin
  ? to_rle( "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW" );
  ? from_rle( "12W1B12W3B24W1B14W");
end rle;

