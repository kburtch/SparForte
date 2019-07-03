procedure t2 is

  procedure proc0;

  procedure proc0 is
  begin
    put_line( "zero" );
  end proc0;

  procedure proc1( param1 : integer );

  procedure proc1( param1 : integer ) is
  begin
    put_line( param1 );
  end proc1;

  --procedure proc1a( param1a : integer );

  --procedure proc1a( param1 : integer ) is
  --begin
  --  put_line( param1 );
  --end proc1a;

  --procedure proc1b( param1b : integer );

  --procedure proc1b( param1b : string ) is
  --begin
  --  put_line( param1b );
  --end proc1b;

  --procedure proc1c( param1c : in out integer );

  --procedure proc1c( param1c : in integer ) is
  --begin
  --  put_line( param1c );
  --end proc1c;

  --procedure proc1d( param1 : integer );

  --procedure proc1d is
  --begin
  --  put_line( param1 );
  --end proc1d;

  --procedure proc1e( param1 : integer );

  --procedure proc1e( param1 : integer );

  procedure proc2( param21 : integer; param22 : integer );

  procedure proc2( param21 : integer; param22 : integer ) is
  begin
    put_line( param21 );
    put_line( param22 );
  end proc2;

begin
  proc0;
  proc1( 1 );
  proc2( 21, 22 );
end t2;
