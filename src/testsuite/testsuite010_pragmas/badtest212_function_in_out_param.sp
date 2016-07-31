pragma ada_95;
procedure p is
  -- out parameter not allowed
  function out_func ( x : in out integer ) return integer is
  begin
    x := 5;
  end out_func;
begin
  null;
end p;
