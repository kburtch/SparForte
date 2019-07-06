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

  --procedure proc2a( params21 : integer; param2a : integer );

  --procedure proc2a( params21 : integer; param2 : integer ) is
  --begin
  --  put_line( param21 );
  --  put_line( param22 );
  --end proc1a;

  --procedure proc2b( params21 : integer; param2b : integer );

  --procedure proc2b( params21 : integer; param2b : string ) is
  --begin
  --  put_line( param2b );
  --end proc2b;

  --procedure proc2c( params21 : integer; param2c : in out integer );

  --procedure proc2c( params21 : integer; param2c : in integer ) is
  --begin
  --  put_line( param2c );
  --end proc2c;

  --procedure proc2d( params21 : integer; param2 : integer );

  --procedure proc2d is
  --begin
  --  put_line( param2 );
  --end proc2d;

  --procedure proc2e( params21 : integer; param2 : integer );

  --procedure proc2e( params21 : integer; param2 : integer );

  function forward_func0 return string;

  function forward_func0 return string is
  begin
    return "ff0";
  end forward_func0;

  function forward_func1( param1 : integer ) return integer;

  function forward_func1( param1 : integer ) return integer is
  begin
    return param1;
  end forward_func1;

  --function forward_func1a( param1a : integer ) return integer;

  --function forward_func1a( param1 : integer ) return integer is
  --begin
  --  return param1;
  --end forward_func1;

  --function forward_func1b( param1b : integer ) return integer;

  --function forward_func1b( param1b : string ) return integer is
  --begin
  --  return numerics.value( param1b );
  --end forward_func1;

  --function forward_func1c( param1c : in out integer ) return integer;

  --function forward_func1c( param1c : in integer ) return integer is
  --begin
  --  return param1c;
  --end forward_func1c;

  --function forward_func1d( param1d : integer ) return integer;

  --function forward_func1d return integer is
  --begin
  --  return param1d;
  --end forward_func1d;

  -- function forward_func1e( param1d : integer ) return integer;

  -- function forward_func1e( param1d : integer ) return integer;

  function forward_func1f( param1f : integer ) return string;

  function forward_func1f( param1f : integer ) return integer is
  begin
    return param1f;
  end forward_func1f;


  -- TODO: validating return value
  -- TODO: in vs <none> not distiguished but probably should be

begin
  proc0;
  proc1( 1 );
  proc2( 21, 22 );
  --proc2a( 21, 22 );
  ? forward_func0;
  ? forward_func1( 31 );
  ? forward_func1f( 31 );
end t2;
