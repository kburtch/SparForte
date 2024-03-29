#!/usr/local/bin/spar

pragma annotate( summary, "monte" )
              @( description, "A Monte Carlo Simulation is a way of approximating the" )
              @( description, "value of a function where calculating the actual value is" )
              @( description, "difficult or impossible. It uses random sampling to define" )
              @( description, "constraints on the value and then makes a sort of 'best" )
              @( description, "guess.'" )
              @( description, "" )
              @( description, "Write a function to run a simulation like this with a" )
              @( description, "variable number of random points to select. Also, show the" )
              @( description, "results of a few different sample sizes. For software" )
              @( description, "where the number pi is not built-in, we give pi to a couple" )
              @( description, "of digits: 3.141592653589793238462643383280 " )
              @( category, "algorithms" )
              @( see_also, "http://rosettacode.org/wiki/Monte_Carlo_methods" )
              @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure monte is

   function pi_estimate (throws : positive) return float is
      inside : natural := 0;
   begin
      for throw in 1..throws loop
         if numerics.random ** 2 + numerics.random ** 2 <= 1.0 then
            inside := @ + 1;
         end if;
      end loop;
      return 4.0 * float (inside) / float (throws);
   end pi_estimate;

begin
   ? "      1_000:" & strings.image (pi_estimate (      1_000))
   @ "     10_000:" & strings.image (pi_estimate (     10_000))
   @ "    100_000:" & strings.image (pi_estimate (    100_000))
   @ "  1_000_000:" & strings.image (pi_estimate (  1_000_000));
end monte;

-- VIM editor formatting instructions
-- vim: ft=spar

