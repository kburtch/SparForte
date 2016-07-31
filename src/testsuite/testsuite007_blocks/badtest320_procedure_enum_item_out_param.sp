procedure p is
  procedure out_proc ( i : out boolean ) is
  begin
    i := false;
  end out_proc;
begin
  out_proc( true ); -- enum item now allowed
end p;
