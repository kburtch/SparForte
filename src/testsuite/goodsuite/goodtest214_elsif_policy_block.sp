policy test is
  if System.Development_Phase then
     pragma ada_95;
  elsif System.Design_Phase then
     null;
  else
     null;
  end if;
end test;

procedure p is
begin
   null;
end p;

