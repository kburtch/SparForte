procedure t is
begin
  null;
exception when others others => -- double when
  put_line( "exception raised" );
end t;

