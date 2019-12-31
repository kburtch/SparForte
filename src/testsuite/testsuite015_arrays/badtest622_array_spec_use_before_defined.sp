procedure t is
  type atype is array(1..1) of integer;
  c7 : constant atype;
  b  : integer := c7(1); -- c7 is not defined
  c7 : constant atype := (1);
begin
  ? c7(1);
  ? b;
end t;

