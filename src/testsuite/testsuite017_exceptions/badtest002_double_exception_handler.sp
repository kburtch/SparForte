procedure t is
begin
  foo_does_not_exist;
exception when others =>
  put_line( "exception raised" );
exception when others => -- extra exception handler
  put_line( "exception raised" );
end t;

