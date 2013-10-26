procedure t is
  e : exception with 3,
begin
  foo_does_not_exist;
exception when others =>
  put_line( "exception raised" );
  raise;
  raise; -- unreachable code
end t;

