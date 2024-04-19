procedure t is
  type testenum is (red, green, blue);
  type testrec is record
     colour : testenum;
  end record;
  tr : testrec;
  js : json_string;
begin
  js := "{" & ASCII.Quotation & "colour" & ASCII.Quotation & ": }";
  records.to_record( tr, js ); -- illegal JSON: no field name
  env tr.colour;
end t;


