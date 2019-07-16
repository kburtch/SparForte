procedure p is

  procedure proc1b( param1b : integer );

  procedure proc1b( param1b : string ) is
  begin
    put_line( param1b );
  end proc1b;

begin
  proc1b( 15 );
end p;
