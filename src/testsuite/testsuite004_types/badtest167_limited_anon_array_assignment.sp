# This should fail

procedure p is

  sa : limited array( 1..3 ) of integer;

begin
  sa(1) := 5; -- should fail on assignment to limited array
end p;

