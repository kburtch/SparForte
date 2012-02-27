#!/usr/local/bin/spar

pragma annotate( summary, "bitarith" );
pragma annotate( description, "Write a routine to perform a bitwise AND, OR, and XOR on" );
pragma annotate( description, "two integers, a bitwise NOT on the first integer, a left" );
pragma annotate( description, "shift, right shift, right arithmetic shift, left rotate," );
pragma annotate( description, "and right rotate. All shifts and rotates should be done on" );
pragma annotate( description, "the first integer with a shift/rotate amount of the second" );
pragma annotate( description, "integer." );
pragma annotate( see_also, "http://rosettacode.org/wiki/Bitwise_operations" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure bitarith is
  A : natural := 255;
  B : natural := 170;
  X : natural := 128;
  N : natural := 1;
begin
  put( "A and B = " ) @ (A and B); new_line;
  put( "A or  B = " ) @ (A or  B); new_line;
  put( "A xor B = " ) @ (A xor B); new_line;
  new_line;
  put( "A << B = " ) @ ( numerics.shift_left( X, N ) ); new_line;
  put( "A >> B = " ) @ ( numerics.shift_right( X, N ) ); new_line;
  put( "A >>> B = " ) @ ( numerics.shift_right_arithmetic( X, N ) ); new_line;
  put( "A rotl B = " ) @ ( numerics.rotate_left( X, N ) ); new_line;
  put( "A rotr B = " ) @ ( numerics.rotate_right( X, N ) ); new_line;
end bitarith;

-- VIM editor formatting instructions
-- vim: ft=spar

