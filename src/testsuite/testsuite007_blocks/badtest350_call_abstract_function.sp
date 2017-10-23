procedure main is

function some_f return boolean is null abstract;

begin
  ? some_f; -- cannot call abstract function
end main;

