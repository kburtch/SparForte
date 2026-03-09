-- This should fail

meta newmeta is new boolean; -- integer is not a tag

i : constant integer := 5 tagged newmeta;
pragma assert( tags.has_unit( i ) );

