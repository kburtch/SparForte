# This should fail

validate_error : exception;

type t is new integer
  accept
    raise validate_error;
  end accept;

a : t := 1; -- should always fail
? a;

