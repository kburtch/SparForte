js : json_string;
type arrtype is array(1..2) of boolean;

a : arrtype;

arrays.to_json( js, a ); -- bool values are undefined
