procedure t is
  global : integer := 0;

  function fun1 return integer is
  begin
     global := 1;
     return 5;
  end fun1;

  i : integer;
begin
  i := global + fun1; -- if expr changes, it is a side-effect
end t;

