# This should fail

procedure param_test is

  j : integer;

  procedure test_proc( i : integer ) is
  begin
    j := i;
  end test_proc;

begin
  test_proc;  -- should error
end param_test;

