policy test is
  case System.Development_Phase is
  when true =>
     pragma ada_95;
  when others =>
     null;
  end case;
end test;

procedure p is
begin
   null;
end p;

