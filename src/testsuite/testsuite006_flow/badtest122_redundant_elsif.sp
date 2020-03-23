# This should fail

if true = true then
  null;
elsif elsif false = false then -- redundant elsif
  null;
end if;
