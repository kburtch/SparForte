procedure main is

type abstract_type is new abstract integer;

function some_f return abstract_type is
begin
  return 0;
end some_f;

begin
  null;
end main;

