type rt is record
  i : integer;
end record;
r : constant rt;
? r.i; -- this should fail
r : constant rt := (1);
? r.i;
