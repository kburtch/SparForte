#!/usr/local/bin/bush

pragma annotate( "Write a routine to perform a bitwise AND, OR, and XOR on" );
pragma annotate( "two integers, a bitwise NOT on the first integer, a left" );
pragma annotate( "shift, right shift, right arithmetic shift, left rotate," );
pragma annotate( "and right rotate. All shifts and rotates should be done on" );
pragma annotate( "the first integer with a shift/rotate amount of the second" );
pragma annotate( "integer." );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Bitwise_operations" );
pragma annotate( "" );
pragma annotate( "Translated by ken O. Burtch" );

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
