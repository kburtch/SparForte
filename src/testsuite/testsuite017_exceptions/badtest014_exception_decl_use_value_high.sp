procedure t is
  e : exception with "foo" use 256; -- value too high
begin
  null;
end t;

