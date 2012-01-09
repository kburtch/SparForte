type int_array is array(1..1) of integer;
a : int_array;
js : json_string := "[" & ASCII.Quotation & "foo" & ASCII.Quotation & "]";
arrays.to_array( a, js ); -- integer but string supplied

