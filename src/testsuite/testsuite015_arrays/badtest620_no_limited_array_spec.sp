procedure t is
  type atype is array(1..1) of integer;
  c7 : limited atype;
  c7 : limited atype := (1);
begin
  ? c7(1);
end t;

