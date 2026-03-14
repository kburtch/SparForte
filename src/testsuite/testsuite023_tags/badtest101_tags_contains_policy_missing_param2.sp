-- This should fail

meta policy newpolicy is new meta;

i : constant integer := 5;

? tags.contains_policy( i );

