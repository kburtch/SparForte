# this should fail

procedure p is

type t1 is new integer;
type t2 is new t1;

  procedure proc1( i : t2 ) is
  begin
    ? i;
  end proc1;

begin
   null;
   -- t1 not used for a parameter
end p;



