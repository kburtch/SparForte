v : vectors.vector( natural, string );

-- if element is numeric, it may be impossible to detect certain
-- combos because the optional count will be taken as the element

vectors.insert( v, "foo", 1 ); -- index is missing

