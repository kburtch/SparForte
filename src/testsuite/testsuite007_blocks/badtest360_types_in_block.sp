procedure p is
begin
  type t is new integer; -- this should no longer be allowed
  s : t;
  ? s;
end p;
