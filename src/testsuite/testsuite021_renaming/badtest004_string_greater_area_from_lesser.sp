s : string := "hello";
u : universal_string renames s; -- universal string greater range than string
pragma assert( u = s );

