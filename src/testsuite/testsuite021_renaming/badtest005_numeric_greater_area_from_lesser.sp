i : integer := 8;
u : universal_numeric renames i; -- universal type greater range than integer
pragma assert( u = i );

