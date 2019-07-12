procedure p is

  function forward_func1c( param1c : in out integer ) return integer;

  function forward_func1c( param1c : in integer ) return integer is
  begin
    return param1c;
  end forward_func1c;

begin
  ? forward_func1c( 1 );
end p;
