procedure p is
  type rec_type is record
       i : integer;
       j : integer;
  end record;

  r : rec_type := (1); -- expected two
begin
  ?r.i;
endp;

