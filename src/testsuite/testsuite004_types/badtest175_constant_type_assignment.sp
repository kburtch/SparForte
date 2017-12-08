# This should fail

type ct is new constant integer;

c : ct := 1;

c := 2; -- can't assign to a constant


