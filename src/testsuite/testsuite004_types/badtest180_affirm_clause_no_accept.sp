# This should fail

type t is new integer
  begin -- affirm, not begin
     null;
  end affirm;

