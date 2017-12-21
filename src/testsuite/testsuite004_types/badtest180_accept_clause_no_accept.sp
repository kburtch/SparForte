# This should fail

type t is new integer
  begin -- accept, not begin
     null;
  end accept;

