#!/usr/local/bin/bush

-- From Rosetta Code http://rosettacode.org/wiki/Dot_product

pragma annotate( "dotproduct" );
pragma annotate( "" );
pragma annotate( "Create a function/use an in-built function, to compute" );
pragma annotate( "the dot product, also known as the scalar product of two" );
pragma annotate( "vectors. If possible, make the vectors of arbitrary length." );
pragma annotate( "As an example, compute the dot product of the vectors [1," );
pragma annotate( " 3, -5] and [4, -2, -1]." );
pragma annotate( "If implementing the dot product of two vectors directly," );
pragma annotate( "each vector must be the same length; multiply" );
pragma annotate( "corresponding terms from each vector then sum the results" );
pragma annotate( "to produce the answer. " );
pragma annotate( "translated by Ken O. Burtch" );

pragma restriction( no_external_commands );

procedure dotproduct is
  type vect3 is array(1..3) of integer;
  v1 : vect3 := (1,3,-5);
  v2 : vect3 := (4,-2,-1);

  sum : integer := 0;
begin
  if arrays.length( v1 ) /= arrays.length( v2 ) then
     put_line( standard_error, "different lengths" );
     command_list.set_exit_status( 193 );
     return;
  end if;
  if arrays.first( v1 ) /= arrays.first( v2 ) then
     put_line( standard_error, "different starts" );
     command_list.set_exit_status( 194 );
     return;
  end if;
  for p in arrays.first( v1 )..arrays.last( v1 ) loop
      sum := @ + v1(p)*v2(p);
  end loop;
  ? sum;
end dotproduct;

