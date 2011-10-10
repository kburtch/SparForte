type r1 is record
  b : boolean;
end record;
r : r1;
r.b := false;
js : json_string := "{" & ASCII.Quotation & "foobar" & ASCII.Quotation & ":true}";
records.to_record( r, js ); -- no such field

