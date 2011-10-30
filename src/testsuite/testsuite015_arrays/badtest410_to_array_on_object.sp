js : json_string := "{[1,2],[3,4]}";

type json_arr9 is array(1..2) of json_string;
ja9 : json_arr9;

arrays.to_array( ja9, js ); -- illegal JSON

