procedure p is

  procedure proc2c( params21 : integer; param2c : in out integer );

  procedure proc2c( params21 : integer; param2c : in integer ) is
  begin
    put_line( param21 );
    put_line( param2c );
  end proc2c;

begin
  proc2c( 1, 2 );
end p;
