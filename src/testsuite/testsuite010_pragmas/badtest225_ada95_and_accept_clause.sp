# This should fail

pragma ada_95;
type t is new integer
accept --not allowed
  null;
end accept;

a : t := 1;
put_line( a );

