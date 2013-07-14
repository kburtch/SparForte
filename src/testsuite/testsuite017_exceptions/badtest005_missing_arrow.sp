procedure t is
begin
  null;
exception when others -- no arrows
  put_line( "exception raised" );
end t;

