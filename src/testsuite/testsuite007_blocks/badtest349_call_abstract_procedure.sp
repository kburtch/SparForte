procedure main is

procedure some_p( i : integer) is null abstract;

begin
  some_p; -- cannot call abstract procedure
end main;

