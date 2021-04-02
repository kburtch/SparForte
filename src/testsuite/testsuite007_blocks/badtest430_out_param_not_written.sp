procedure t is

  procedure say_write( s : out string ) is
  begin
    null;
  end say_write; -- s not written to

  msg : string;
begin
  say_write( msg );
  ? msg;
end t;

