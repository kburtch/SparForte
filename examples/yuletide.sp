#!/usr/local/bin/spar

pragma annotate( summary, "yuletide" );
pragma annotate( description, "A company decides that whenever Xmas falls on a Sunday they will give their" );
pragma annotate( description, "workers all extra paid holidays so that, together with any public holidays," );
pragma annotate( description, "workers will not have to work the following week (between the 25th of" );
pragma annotate( description, "December and the first of January)." );
pragma annotate( description, "");
pragma annotate( description, "In what years between 2008 and 2121 will the 25th of December be a Sunday?" );
pragma annotate( description, "");
pragma annotate( description, "Using any standard date handling libraries of your programming language;" );
pragma annotate( description, "compare the dates calculated with the output of other languages to discover" );
pragma annotate( description, "any anomalies in the handling of dates which may be due to, for example," );
pragma annotate( description, "overflow in types used to represent dates/times similar to y2k type" );
pragma annotate( description, "problems. ");
pragma annotate( category, "puzzles" );
pragma annotate( see_also, "http://rosettacode.org/wiki/Day_of_the_week" );
pragma annotate( author, "Ken O. Burtch ");
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure yuletide is
begin
   for Year in 2008..2121 loop
      if calendar.day_of_week ( calendar.time_of (Year, 12, 25, 0)) = 1 then
         put_line( "Christmas " & strings.image( Year ) & " is on a Sunday" );
      end if;
   end loop;
end yuletide;

-- VIM editor formatting instructions
-- vim: ft=spar

