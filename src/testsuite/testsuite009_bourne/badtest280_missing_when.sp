procedure t is
  cs : constant string := "there";
begin
  -- missing a when with string case looks like a shell command
  case cs is
  when "one" =>
     ? "one";
  "two" =>
     ? "two";
  when others =>
     ? "others";
  end case;
end t;

