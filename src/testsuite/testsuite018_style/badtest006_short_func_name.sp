procedure t is

  function fo return boolean is -- name too short
  begin
    return true;
  end fo;

begin
  ? fo;
end t;

