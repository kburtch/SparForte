# This should fail

if true = true then
  null;
elsif false = false then then -- redundant elsif then
  null;
end if;
