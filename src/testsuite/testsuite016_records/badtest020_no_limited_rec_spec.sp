type rt is record
  i : integer;
end record;
r : limited rt;
r : limited rt := (1); -- this should fail
? r.i;
