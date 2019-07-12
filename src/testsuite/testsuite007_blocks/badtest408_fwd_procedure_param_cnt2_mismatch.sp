procedure p is

  procedure proc2d( params21 : integer; param2 : integer );

  procedure proc2d is
  begin
    put_line( param21 );
    put_line( param2 );
  end proc2d;

begin
  proc2d( 1, 2 );
end p;
