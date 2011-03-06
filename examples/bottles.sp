#!/usr/local/bin/bush

-- A common interview test from Rosetta Code for testing basic programming
-- skills.

pragma annotate( "bottles" );
pragma annotate( "" );
pragma annotate( "In this puzzle, write code to print out the entire '99 bottles of beer" );
pragma annotate( "on the wall' song." );
pragma annotate( "translated by Ken O. Burtch" );

pragma restriction( no_external_commands );

 procedure Bottles is
 begin
    for X in reverse 1..99 loop
       ? strings.image( X ) & " bottles of beer on the wall";
       ? strings.image( X ) & " bottles of beer";
       ? "Take one down, pass it around";
       ? strings.image( X-1 ) & " bottles of beer on the wall" @ "";
    end loop;
 end Bottles;
