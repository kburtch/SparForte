procedure t is
  x : constant boolean := true;
begin
  if x then
     return;
  end if;

  -- should not be an infinite loop
  while x loop
     ? x;
     exit;
  end loop;
end t;

