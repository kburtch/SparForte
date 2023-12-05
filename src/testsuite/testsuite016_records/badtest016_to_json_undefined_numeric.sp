type r1 is record
  e : integer;
end record;
r : r1;
js : json_string;
records.to_json( js, r ); -- r.e is undefined
? r.e;
env js;
