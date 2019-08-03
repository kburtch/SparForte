procedure p is

  function forward_func1a( param1a : integer ) return integer;

  function forward_func1a( param1 : integer ) return integer is
  begin
    return param1;
  end forward_func1a;

begin
  ? forward_func1a( 1 );
end p;
