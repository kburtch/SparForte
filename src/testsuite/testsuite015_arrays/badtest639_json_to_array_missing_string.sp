js: json_string;
a : array(1..2) of string;

js := "[ " & ASCII.Quotation & "string1" & ASCII.Quotation &
             ", " &
             ASCII.Quotation & -- "string2" & ASCII.Quotation &
             " ]";
arrays.to_array( a, js ); -- missing value

