#!/usr/local/bin/spar

pragma annotate( summary, "arraysum" )
       @( description, "Compute the sum and product of an array of integers." )
       @( see_also, "http://rosettacode.org/wiki/Sum_and_product_of_an_array" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure arraysum is
  type int_array is array(1..10) of integer;
  arr : constant int_array := (1,2,3,4,5,6,7,8,9,10 );
begin
  ? stats.sum( arr );
  declare
     product : integer := 1;
  begin
     for i in arrays.first( arr )..arrays.last( arr ) loop
         product := @ * arr(i);
     end loop;
     ? product;
  end;
end arraysum;

-- VIM editor formatting instructions
-- vim: ft=spar

