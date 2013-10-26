procedure t is
begin
  null;
  exception when true => -- true is not an exception
    null;
end t;

