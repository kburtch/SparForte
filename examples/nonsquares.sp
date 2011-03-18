#!/usr/local/bin/bush

pragma annotate( "nonsquares" );
pragma annotate( "" );
pragma annotate( "Show that the following remarkable formula gives the" );
pragma annotate( "sequence of non-square natural numbers: n +" );
pragma annotate( "floor(1/2 + sqrt(n)).  Print out the values for n in" );
pragma annotate( "the range 1 to 22.  Show that no squares occur for n" );
pragma annotate( "less than one million." );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Sequence_of_non-squares" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure nonsquares is
 
   function is_non_square (n : positive) return positive is
   begin
      return n + positive (numerics.rounding(numerics.sqrt (long_float (n))));
   end is_non_square;
 
   i : positive;
begin
   for n in 1..22 loop -- First 22 non-squares
      put (strings.image (is_non_square (n)));
   end loop;
   new_line;
   for n in 1..1_000_000 loop -- Check first million of
      i := is_non_square (n);
      if i = positive (numerics.rounding(numerics.sqrt (long_float (i))))**2 then
         put_line ("Found a square:" & strings.image (n));
      end if;
   end loop;
end nonsquares;
