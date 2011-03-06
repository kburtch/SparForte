#!/usr/local/bin/bush

pragma annotate( "Increment an integer number in a string" );
pragma annotate( "http://rosettacode.org/wiki/Increment_a_numerical_string" );
pragma annotate( "" );
pragma annotate( "Translated by ken O. Burtch" );

procedure incstr is
   s : string := "12345";
begin
   s := strings.trim( strings.image( numerics.value( s ) + 1 ), trim_end.both ) ;
   ? s;
end incstr;
