procedure p is

  function forward_func1b( param1b : integer ) return integer;

  function forward_func1b( param1b : string ) return integer is
  begin
    return numerics.value( param1b );
  end forward_func1b;

begin
  ? forward_func1b( 1 );
end p;
