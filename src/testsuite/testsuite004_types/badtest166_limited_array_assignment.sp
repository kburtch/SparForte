# This should fail

procedure p is

  type some_array is limited array( 1..3 ) of integer;
  sa : some_array;

begin
  sa(1) := 5; -- should fail on assignment to limited array
end p;

