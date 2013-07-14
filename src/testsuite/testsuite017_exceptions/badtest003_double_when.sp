procedure t is
begin
  null;
exception when when others => -- double when
  put_line( "exception raised" );
end t;

