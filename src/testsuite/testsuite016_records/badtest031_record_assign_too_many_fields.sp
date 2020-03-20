procedure p is
  type rec_type is record
       i : integer;
  end record;

  r : rec_type := (1,2); -- expected one
begin
  ?r.i;
endp;

