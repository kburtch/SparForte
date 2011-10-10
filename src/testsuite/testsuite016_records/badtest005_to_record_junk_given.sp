type enum is ( e1, e2, e3 );
type r1 is record
  e : enum;
end record;
r : r1;
r.e := e2;
js : json_string := "foobar";
records.to_record( r, js ); -- foobar isn't a json string

