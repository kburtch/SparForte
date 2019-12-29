type rt is record
  i : integer;
end record;
r : rt;
r : rt := (1); -- this should fail
? r.i;
