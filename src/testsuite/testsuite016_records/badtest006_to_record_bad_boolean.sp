type r1 is record
  b : boolean;
end record;
r : r1;
r.b := false;
js : json_string := "{" & ASCII.Quotation & "b" & ASCII.Quotation & ":foobar}";
records.to_record( r, js ); -- foobar isn't true/false

