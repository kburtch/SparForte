#!/usr/local/bin/bush

-- A common interview test from Rosetta Code for testing basic programming
-- skills.

pragma annotate( "aplusb" );
pragma annotate( "" );
pragma annotate( "A+B - in programming contests, classic problem, which is given so" );
pragma annotate( "contestants can gain familiarity with online judging system being used. " );
pragma annotate( "A+B is one of few problems on contests, which traditionally lacks fabula." );
pragma annotate( "Given 2 integer numbers, A and B. One needs to find their sum. " );
pragma annotate( "translated by Ken O. Burtch" );

procedure aplusb is
  l : string;
  a : integer;
  b : integer;
begin
  -- get( a ) @ ( b ) doesn't work in Bush since there's no integer_io.get()
  l := get_line;
  a := numerics.value( strings.field( l, 1, ' ' ) );
  b := numerics.value( strings.field( l, 2, ' ' ) );
  ? a+b;
end aplusb;

