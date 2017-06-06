# This should fail

pragma ada_95;
b : boolean;
b := true = true and true = true or true = true; -- (..) required

