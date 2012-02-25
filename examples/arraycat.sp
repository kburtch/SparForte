#!/usr/local/bin/spar

pragma annotate( summary, "arraycat" );
pragma annotate( description, "Show how to concatenate two arrays in your language." );
pragma annotate( description, "From a Rosetta Code example" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure arraycat is
  type arrayOf3 is array(1..3) of integer;
  a1 : arrayOf3 := (1, 2, 3);
  a2 : arrayOf3 := (4, 5, 6);
  type arrayOf6 is array(1..6) of integer;
  a3 : arrayOf6;
  p  : natural := arrays.first(a3);
begin
  -- In SparForte 1, array support is limited.  & only works on strings
  -- and there's no indefinite ranges.  We have to do this the hard way.
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
end arraycat;

-- VIM editor formatting instructions
-- vim: ft=spar

