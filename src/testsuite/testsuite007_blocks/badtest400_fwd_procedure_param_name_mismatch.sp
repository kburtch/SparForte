procedure p is

  procedure proc1a( param1a : integer );

  procedure proc1a( param1 : integer ) is
  begin
    put_line( param1 );
  end proc1a;

begin
  proc1a( 15 );
end p;
