-- This should fail

type art is array(1..1) of integer;

a : art := (1);

echo ${a};

