#!/usr/local/bin/spar

pragma annotate( summary, "sumsq" )
              @( description, "Write a program to find the sum of squares " )
              @( description, "of a numeric vector.  The program should " )
              @( description, "work on a zero-length vector (with an " )
              @( description, "answer of 0). " )
              @( see_also, "http://rosettacode.org/wiki/Sum_of_squares" )
              @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure sumsq is

type float_array is array(1..6) of float;

function sum_of_squares( x : in out float_array; len : positive ) return float is
  total : float := 0.0;
begin
  for i in arrays.first(x)..len loop
    total := @ + x(i)**2;
  end loop;
  return total;
end sum_of_squares;

  fa : float_array := (3.0, 1.0, 4.0, 1.0, 5.0, 9.0);

begin
  -- Zero length (result zero)
  ? sum_of_squares(fa, 0);

  -- Non-zero length (result 133)
  ? sum_of_squares(fa, 6);
end sumsq;

-- VIM editor formatting instructions
-- vim: ft=spar

