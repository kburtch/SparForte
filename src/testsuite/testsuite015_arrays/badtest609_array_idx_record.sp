type array_type is array(1..5) of integer;
i : integer;
a : array_type;
type rt is record
   i : integer;
end record;
r : rt;
i := a(r);

