type r1 is record
  a : boolean;
  b : boolean;
end record;
r : r1;
r.b := false;
js : json_string := "{" & ASCII.Quotation & "a" & ASCII.Quotation & ":true}";
records.to_record( r, js ); -- one fields when two expected

