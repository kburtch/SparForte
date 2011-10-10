type r1 is record
  b : boolean;
end record;
r : r1;
r.b := false;
js : json_string := "{" & ASCII.Quotation & "a" & ASCII.Quotation & ":true," &
  ASCII.Quotation & "b" & ASCII.Quotation & ":false}";
records.to_record( r, js ); -- two fields when one expected

