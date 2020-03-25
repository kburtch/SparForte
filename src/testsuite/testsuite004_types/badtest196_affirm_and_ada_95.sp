# This should fail

pragma ada_95;

type t is new integer
  affirm -- not allowed with ada_95
    t := t;
  end affirm;

a : t := 1;
? a;

