procedure t is
  x : integer;
  type t is new integer
    affirm
      x := 2;
      ? t;
    end affirm;
  i : t := 1;

begin
  x := 3; -- x written by two "threads"
  ? i;
end t;

