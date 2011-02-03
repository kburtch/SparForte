#!/usr/local/bin/bush

pragma annotate( "perfect" );
pragma annotate( "" );
pragma annotate( "In mathematics, a perfect number is a positive integer" );
pragma annotate( "that is the sum of its proper positive divisors, that is," );
pragma annotate( "the sum of the positive divisors excluding the number" );
pragma annotate( "itself." );
pragma annotate( "" );
pragma annotate( "http://en.wikipedia.org/wiki/Perfect_number" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure perfect is

  function is_perfect( n : positive ) return boolean is
    sum : natural := 0;
  begin
    for i in 1..n-1 loop
      if n mod i = 0 then
         sum := @+i;
      end if;
    end loop;
    return sum = natural( n );
  end is_perfect;

  number : positive;
  result : boolean;
begin
  number := 6;
  result   := is_perfect( number );
  put( number ) @ ( " : " ) @ ( result );
  new_line;

  number := 18;
  result   := is_perfect( number );
  put( number ) @ ( " : " ) @ ( result );
  new_line;

  number := 28;
  result   := is_perfect( number );
  put( number ) @ ( " : " ) @ ( result );
  new_line;

end perfect;

