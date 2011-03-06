#!/usr/local/bin/bush

pragma annotate( "loopsbreak" );
pragma annotate( "" );
pragma annotate( "Show a loop which prints random numbers (each number newly" );
pragma annotate( "generated each loop) from 0 to 19 (inclusive). If a number is" );
pragma annotate( "10, stop the loop after printing it, and do not generate any" );
pragma annotate( "further numbers. Otherwise, generate and print a second random" );
pragma annotate( "number before restarting the loop. If the number 10 is never" );
pragma annotate( "generated as the first number in a loop, loop forever. " );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Loops/Break" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

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

