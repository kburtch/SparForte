s : universal_string := "hello";
u : universal_typeless copies s; -- uni typeless greater range than uni string
pragma assert( u = s );

