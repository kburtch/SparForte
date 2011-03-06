#!/usr/local/bin/bush

-- From Rosetta Code

pragma annotate("yuletide");
pragma annotate("");
pragma annotate("A company decides that whenever Xmas falls on a Sunday they will give their" );
pragma annotate("workers all extra paid holidays so that, together with any public holidays," );
pragma annotate("workers will not have to work the following week (between the 25th of" );
pragma annotate("December and the first of January)." );
pragma annotate("");
pragma annotate("In what years between 2008 and 2121 will the 25th of December be a Sunday?" );
pragma annotate("");
pragma annotate("Using any standard date handling libraries of your programming language;" );
pragma annotate("compare the dates calculated with the output of other languages to discover" );
pragma annotate("any anomalies in the handling of dates which may be due to, for example," );
pragma annotate("overflow in types used to represent dates/times similar to y2k type" );
pragma annotate("problems. ");
pragma annotate("translated by Ken O. Burtch ");

pragma restriction( no_external_commands );

procedure yuletide is
begin
   for Year in 2008..2121 loop
      if calendar.day_of_week ( calendar.time_of (Year, 12, 25, 0)) = 1 then
         put_line( "Christmas " & strings.image( Year ) & " is on a Sunday" );
      end if;
   end loop;
end yuletide;

