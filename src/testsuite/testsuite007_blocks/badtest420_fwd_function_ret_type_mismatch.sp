procedure p is

  function forward_func1f( param1f : integer ) return string;

  function forward_func1f( param1f : integer ) return integer is
  begin
    return param1f;
  end forward_func1f;

begin
  ? forward_func1f( 1 );
end p;
