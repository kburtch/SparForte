type r1 is record
  i : integer;
end record;
r : r1;
r.i := 1;
s : string;
records.to_json( s, r ); -- string not json string

