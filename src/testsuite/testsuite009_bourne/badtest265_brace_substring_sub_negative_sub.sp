-- This should fail

s : string := "foobar";

echo ${s:1:-99}; -- not integer

