# This should fail

procedure param_test is

  function test_func( i : integer ) return integer is
  begin
    return i;
  end test_func;

  j : integer;
begin
  j := test_func; -- should error
end param_test;

