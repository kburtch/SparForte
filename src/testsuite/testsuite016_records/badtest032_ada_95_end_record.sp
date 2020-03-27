pragma ada_95;
procedure p is
  type rec_type is record
       i : integer;
  end rec_type;  -- pragma ada_95 not allowed

  r : rec_type := (1);
begin
  ?r.i;
end p;

