procedure t is
  type a is array (1..1) of exception; -- exception arrays are not allowed
begin
  null;
end t;

