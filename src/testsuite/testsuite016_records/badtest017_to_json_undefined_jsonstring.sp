type r1 is record
  e : json_string;
end record;
r : r1;
js : json_string;
records.to_json( js, r ); -- r.e is undefined
? r.e;
env js;
