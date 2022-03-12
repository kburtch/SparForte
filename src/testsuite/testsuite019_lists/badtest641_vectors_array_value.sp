
type myarray is array(1..1) of integer;
type myvector is new vectors.vector( natural, myarray );

v : myvector;

vectors.append_elements( v, 0 );

