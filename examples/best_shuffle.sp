#!/usr/local/bin/spar

pragma annotate( summary, "best_shuffle" )
       @( description, "Shuffle the characters of a string in such a" )
       @( description, "way that as many of the character values are" )
       @( description, "in a different position as possible. Print" )
       @( description, "the result as follows: original string," )
       @( description, "shuffled string, (score). The score gives the" )
       @( description, "number of positions whose character value" )
       @( description, "did not change." )
       @( category, "algorithms" )
       @( author, "Ken O. Burtch" )
       @( see_also, "http://rosettacode.org/wiki/Best_shuffle" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure best_shuffle is

  -- Shuffle the characters in a string.  Do not swap identical characters

  function shuffle( s : string ) return string is
    t : string := s;
    tmp : character;
  begin
    for i in 1..strings.length(s) loop
       for j in 1..strings.length(s) loop
         if i /= j and strings.element( s, i ) /= strings.element( t, j ) and strings.element( s, j ) /= strings.element( t, i ) then
            tmp := strings.element( t, i );
            t := strings.overwrite( t, i, strings.element( t, j ) & "" );
            t := strings.overwrite( t, j, tmp & "" );
         end if;
       end loop;
    end loop;
    return t;
  end shuffle;

  must_stop : boolean := false;

begin

  while not must_stop loop
    declare
      original : constant string := get_line;
      shuffled : constant string := shuffle( original );
      score : natural := 0;
   begin
      if original = "" then
         must_stop;
      end if;

      -- determine the score for the shuffled string

      for i in 1..strings.length( original ) loop
         if strings.element( original, i ) = strings.element( shuffled, i ) then
            score := @+1;
         end if;
      end loop;
      put_line( original & ", " & shuffled & ", (" &
          strings.image( score ) & " )" );

   end;
  end loop;

end best_shuffle;

-- VIM editor formatting instructions
-- vim: ft=spar
