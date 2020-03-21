procedure p2 is
   e : exception with "foo" use 1255;
begin
   raise e;
end p2;

