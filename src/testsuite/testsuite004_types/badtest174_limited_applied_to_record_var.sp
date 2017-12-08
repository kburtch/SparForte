procedure p is
  type record_type is record
      i : integer;
  end record;
  r : limited record_type;
begin
  ? r.i;
end p;

