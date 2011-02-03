#!/usr/local/bin/bush

pragma annotate( "monte" );
pragma annotate( "" );
pragma annotate( "A Monte Carlo Simulation is a way of approximating the" );
pragma annotate( "value of a function where calculating the actual value is" );
pragma annotate( "difficult or impossible. It uses random sampling to define" );
pragma annotate( "constraints on the value and then makes a sort of 'best" );
pragma annotate( " guess.'" );
pragma annotate( "" );
pragma annotate( "Write a function to run a simulation like this with a" );
pragma annotate( "variable number of random points to select. Also, show the" );
pragma annotate( "results of a few different sample sizes. For software" );
pragma annotate( "where the number pi is not built-in, we give pi to a couple" );
pragma annotate( "of digits: 3.141592653589793238462643383280 " );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Monte_Carlo_methods" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

procedure monte is

pragma restriction( no_external_commands );
 
   function pi (throws : positive) return float is
      inside : natural := 0;
   begin
      for throw in 1..throws loop
         if numerics.random ** 2 + numerics.random ** 2 <= 1.0 then
            inside := @ + 1;
         end if;
      end loop;
      return 4.0 * float (inside) / float (throws);
   end pi;

begin
   ? "      1_000:" & strings.image (pi (      1_000))
   @ "     10_000:" & strings.image (pi (     10_000))
   @ "    100_000:" & strings.image (pi (    100_000))
   @ "  1_000_000:" & strings.image (pi (  1_000_000));
end monte;

