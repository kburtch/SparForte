m : hashed_maps.map( string, string );
c : hashed_maps.cursor( string, string );
k : constant string := "foo";
b : boolean;

e : exception;

hashed_maps.insert( m, k, k, c, b );
hashed_maps.insert( m, k, k, c, b );
-- not an actual error but if b is false raise an exception
if b = false then
   raise e;
end if;
