procedure p is

  function forward_func1b( param1b : integer; param2b : integer ) return integer;

  function forward_func1b( param1b : integer; param2b : string ) return integer is
  begin
    ? param2b;
    return numerics.value( param1b );
  end forward_func1b;

begin
  ? forward_func1b( 1 );
end p;
