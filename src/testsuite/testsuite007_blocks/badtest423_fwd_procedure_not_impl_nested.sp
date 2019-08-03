procedure p is

  procedure forward_p;

begin
  declare
    procedure forward_p is
    begin
      null;
    end forward_p;
  begin
    forward_p;
  end;
  forward_p;
end p;
