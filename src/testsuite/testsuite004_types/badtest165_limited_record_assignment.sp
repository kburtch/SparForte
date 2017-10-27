# This should fail

procedure p is

  type some_rec is limited record
     i : integer;
  end record;

  sr : some_rec;

begin
  sr.i := 5; -- should fail on assignment to limited record
end p;

