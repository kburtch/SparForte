# This should fail

type user_type is (apple, berry, cherry );

b : user_type;

b := enums.val(user_type, 3); -- no such value
env b;
