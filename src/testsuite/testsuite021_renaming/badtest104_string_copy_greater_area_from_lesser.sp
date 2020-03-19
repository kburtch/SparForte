s : string := "hello";
u : universal_string copies s; -- universal string greater range than string
pragma assert( u = s );

