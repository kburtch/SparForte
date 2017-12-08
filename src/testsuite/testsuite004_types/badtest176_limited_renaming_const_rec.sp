procedure ren is
  type rectype is constant record
       i : integer := 1;
  end record;

  r : rectype;
  r2 : limited rectype renames r;
  i : integer;
begin
  i := r2.i; -- this should be limited (no factor)
end ren;

-- vim: ft=spar

