type rec is new record  -- new should not be used with record
   i : integer;
end record;

r : rec;
r.i := 1;
? r.i;

