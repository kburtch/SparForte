procedure p is

  procedure proc2b( params21 : integer; param2b : integer );

  procedure proc2b( params21 : integer; param2b : string ) is
  begin
    put_line( param21 );
    put_line( param2b );
  end proc2b;

begin
  proc2b( 1, 2 );
end p;
