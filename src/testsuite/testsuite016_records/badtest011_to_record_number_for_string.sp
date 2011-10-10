type r1 is record
  s : string;
end record;
r : r1;
r.s := "";
js : json_string := "{" & ASCII.Quotation & "s" & ASCII.Quotation & ":1}";
records.to_record( r, js ); -- s is number but string expected

