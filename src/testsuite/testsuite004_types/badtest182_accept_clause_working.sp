# This should fail

validate_error : exception;

type t is new integer
  affirm
    raise validate_error;
  end affirm;

a : t := 1; -- should always fail
? a;

