-- This should fail

type rt is record
  i : integer;
end record;

r : rt;

r.i := 1;

echo ${r.i};

