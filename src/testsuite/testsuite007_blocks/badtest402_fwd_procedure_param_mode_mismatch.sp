procedure p is

  procedure proc1c( param1c : in out integer );

  procedure proc1c( param1c : in integer ) is
  begin
    put_line( param1c );
  end proc1c;

begin
  proc1c( 15 );
end p;
