procedure p is
  procedure in_out_proc ( i : in out boolean ) is
  begin
    i := false;
  end in_out_proc;
begin
  in_out_proc( true ); -- enum item now allowed
end p;
