#!/usr/local/bin/spar

-- arraysum as an unstructured script (no main program)

-- Test data

myarr : constant array(1..10) of integer := (1,2,3,4,5,6,7,8,9,10 );

-- Sum of the array

? stats.sum( myarr );

-- Product of the array

product := 1;
for i in arrays.first( myarr )..arrays.last( myarr ) loop
    product := @ * myarr(i);
end loop;
? product;

-- VIM editor formatting instructions
-- vim: ft=spar

