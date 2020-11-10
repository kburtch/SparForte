procedure t is
  pragma declare_affinity( file, foo1 );
  pragma declare_affinity( file, foo2 );

  procedure inner1 is
    pragma affinity( foo1 );
    pragma affinity( foo1 );
  begin
    null;
  end inner1;

  procedure inner2 is
    pragma affinity( foo2 );
    pragma affinity( foo2 );
  begin
    null;
  end inner2;

begin
  inner1;
  inner2;
end t;

