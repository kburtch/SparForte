type enum is ( e1, e2, e3 );
type r1 is record
  e : enum;
end record;
r : r1;
r.e := e2;
js : json_string := "{" & ASCII.Quotation & "e" & ASCII.Quotation & ":-1}";
records.to_record( r, js ); -- enum index out of bounds

