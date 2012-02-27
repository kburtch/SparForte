#!/usr/local/bin/spar

pragma annotate( summary, "loopsbreak" );
pragma annotate( description, "Show a loop which prints random numbers (each number newly" );
pragma annotate( description, "generated each loop) from 0 to 19 (inclusive). If a number is" );
pragma annotate( description, "10, stop the loop after printing it, and do not generate any" );
pragma annotate( description, "further numbers. Otherwise, generate and print a second random" );
pragma annotate( description, "number before restarting the loop. If the number 10 is never" );
pragma annotate( description, "generated as the first number in a loop, loop forever. " );
pragma annotate( see_also, "http://rosettacode.org/wiki/Loops/Break" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure arraysloop is
  a : positive;
  b : positive;
begin
  loop
    a := numerics.rnd( 20 );
    put_line( strings.image( a ) );
    exit when a = 10;
    b := numerics.rnd( 20 );
    put_line( strings.image( b ) );
  end loop;
end arraysloop;

-- VIM editor formatting instructions
-- vim: ft=spar

