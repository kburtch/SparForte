procedure p is
  -- out parameter not allowed
  function out_func ( x : out integer ) return integer is
  begin
    x := 5;
  end out_func;
begin
  null;
end p;
