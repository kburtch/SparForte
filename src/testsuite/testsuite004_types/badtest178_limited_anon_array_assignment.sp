# This should fail

procedure p is

  sa : constant array( 1..3 ) of integer;

begin
  sa(1) := 5; -- should fail on assignment to constant array
end p;

