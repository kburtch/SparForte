procedure p is

  function forward_func1d return integer;

  function forward_func1d( param1d : integer ) return integer is
  begin
    return param1d;
  end forward_func1d;

begin
  ? forward_func1d( 1 );
end p;
