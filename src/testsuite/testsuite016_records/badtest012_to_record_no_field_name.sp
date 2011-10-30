type json_rec9 is record
    i : json_string;
end record;
jr9 : json_rec9;
js : json_string := "{[3,4]}";

records.to_record( jr9, js ); -- illegal JSON: no field name

