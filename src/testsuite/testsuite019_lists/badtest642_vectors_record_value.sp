
type myrec is record
   i : integer := 0;
end myrec;

type myvector is new vectors.vector( natural, myrec );

v : myvector;

vectors.append_elements( v, 0 );

