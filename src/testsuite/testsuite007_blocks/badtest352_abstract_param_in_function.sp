procedure main is

type abstract_type is new abstract integer;

function some_f( i : abstract_type ) return boolean is
begin
  return false;
end some_f;

begin
  null;
end main;

