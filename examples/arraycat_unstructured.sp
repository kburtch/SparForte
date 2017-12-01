#!/usr/local/bin/spar

-- arraycat as an unstructured script (no main program)

type arrayOf3 is array(1..3) of integer;
a1 : constant arrayOf3 := (1, 2, 3);
a2 : constant arrayOf3 := (4, 5, 6);
type arrayOf6 is array(1..6) of integer;
a3 : arrayOf6;
p  : natural := arrays.first(a3);

-- In SparForte, & only works on strings and there's no indefinite ranges
-- or array slicing.  We have to do this the hard way, one element at a
-- time.

for i in arrays.first(a1)..arrays.last(a1) loop
    a3(p) := a1(i);
    p := @+1;
end loop;
for i in arrays.first(a2)..arrays.last(a2) loop
    a3(p) := a2(i);
    p := @+1;
end loop;
-- show the array
for i in arrays.first(a3)..arrays.last(a3) loop
    put( a3(i) );
end loop;
new_line;

-- VIM editor formatting instructions
-- vim: ft=spar

