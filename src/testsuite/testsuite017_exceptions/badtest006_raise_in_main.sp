procedure t is
begin
  raise;  -- raise nothing
exception when others =>
  put_line( "exception raised" );
end t;

