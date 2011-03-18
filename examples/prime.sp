#!/usr/local/bin/bush

pragma annotate( "prime" );
pragma annotate( "" );
pragma annotate( "Write a boolean function that tells whether a given" );
pragma annotate( "integer is prime. Remember that 1 and all" );
pragma annotate( "non-positive numbers are not prime. " );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Primality_by_trial_division" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure prime is

function is_prime(item : positive) return boolean is
   result : boolean := true;
   test : natural;
begin
   if item /= 2 and item mod 2 = 0 then
      result := false;
   else
      test := 3;
      while test < natural( numerics.sqrt( item ) ) loop
         if natural(item) mod test = 0 then
            result := false;
            exit;
         end if;
         test := @ + 2;
      end loop;
  end if;
  return result;
end is_prime;

  number : positive;
  result : boolean;

begin
  number := 6;
  result   := is_prime( number );
  put( number ) @ ( " : " ) @ ( result );
  new_line;

  number := 7;
  result   := is_prime( number );
  put( number ) @ ( " : " ) @ ( result );
  new_line;

  number := 8;
  result   := is_prime( number );
  put( number ) @ ( " : " ) @ ( result );
  new_line;
end prime;

