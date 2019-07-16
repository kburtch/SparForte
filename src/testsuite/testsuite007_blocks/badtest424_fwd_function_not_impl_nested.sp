procedure p is

  function forward_f return integer;

begin
  declare
    function forward_f return integer is
    begin
      return 1;
    end forward_f;
  begin
    ? forward_f;
  end;
  ? forward_f;
end p;
