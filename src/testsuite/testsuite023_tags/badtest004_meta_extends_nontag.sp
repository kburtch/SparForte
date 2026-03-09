-- This should fail

i : integer;
meta policy newmeta is new i; -- i is not a tag

i := 5 tagged newmeta;
pragma assert( tags.has_unit( i ) );

