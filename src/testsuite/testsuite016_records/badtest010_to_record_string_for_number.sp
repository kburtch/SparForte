type r1 is record
  i : integer;
end record;
r : r1;
r.i := 0;
js : json_string := "{" & ASCII.Quotation & "i" & ASCII.Quotation & ":" &
   ASCII.Quotation & "foobar" & ASCII.Quotation & "}";
records.to_record( r, js ); -- i is string but number expected

