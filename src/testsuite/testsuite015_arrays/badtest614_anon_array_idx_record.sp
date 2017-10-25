i : integer;
type rt is record
  i : integer;
end record;
r : rt;
a : array(r ..r) of integer;
i := a(1);
r.i := 1;

