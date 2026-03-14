-- This should fail

meta newunit is new meta;

i : constant integer := 5;

? tags.contains_policy( i, newunit );

