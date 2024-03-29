#!/usr/local/bin/spar

pragma annotate( summary, "bottles" );
pragma annotate( description, "In this puzzle, write code to print out the entire '99 bottles of beer" );
pragma annotate( description, "on the wall' song.  A common interview test from Rosetta Code for" );
pragma annotate( description, "testing basic programming skills." );
pragma annotate( category, "entertainment" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

 procedure Bottles is
 begin
    for X in reverse 1..99 loop
       ? strings.image( X ) & " bottles of beer on the wall";
       ? strings.image( X ) & " bottles of beer";
       ? "Take one down, pass it around";
       ? strings.image( integer(X-1) ) & " bottles of beer on the wall" @ "";
    end loop;
 end Bottles;

-- VIM editor formatting instructions
-- vim: ft=spar

