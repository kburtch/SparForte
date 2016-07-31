procedure p is
  procedure in_out_proc ( i : out integer ) is
  begin
    i := 5;
  end in_out_proc;
  j : constant integer := 5;
begin
  in_out_proc( j ); -- constant now allowed
end p;
