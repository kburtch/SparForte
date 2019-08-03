procedure p is

  procedure proc2a( params21 : integer; param22 : integer );

  procedure proc2a( params21 : integer; param2 : integer ) is
  begin
    put_line( param21 );
    put_line( param22 );
  end proc2a;

begin
  proc2a( 1, 2 );
end p;
