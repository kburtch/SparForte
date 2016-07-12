s : universal_string := "hello";
u : universal_typeless renames s; -- uni typeless greater range than uni string
pragma assert( u = s );

