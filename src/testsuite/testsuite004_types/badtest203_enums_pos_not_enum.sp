# This should fail

b : boolean;
i : constant integer := 5;

b := enums.pos(i); -- not an enum

