i : integer := 8;
u : universal_numeric copies i; -- universal type greater range than integer
pragma assert( u = i );

