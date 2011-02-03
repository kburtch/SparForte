#!/usr/local/bin/bush

pragma annotate( "randdist" );
pragma annotate( "" );
pragma annotate( "Given a mapping between items and their required" );
pragma annotate( "probability of occurrence, generate a million items" );
pragma annotate( "randomly subject to the given probabilities and compare" );
pragma annotate( "the target probability of occurrence versus the" );
pragma annotate( "generated values." );
pragma annotate( "" );
pragma annotate( "The total of all the probabilities should equal one." );
pragma annotate( "(Because floating point arithmetic is involved this is" );
pragma annotate( "subject to rounding errors).  Use the following mapping" );
pragma annotate( "to test your programs: aleph 1/5.0, beth 1/6.0," );
pragma annotate( "gimel 1/7.0, daleth 1/8.0, he 1/9.0,  waw 1/10.0" );
pragma annotate( "zayin 1/11.0, heth 1759/27720 adjusted so that" );
pragma annotate( "probabilities add to 1" );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Probabilistic_choice" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure randdist is
  trials : constant positive := 1_000_000;
  type outcome is (aleph, beth, gimel, daleth, he, waw, zayin, heth);
  pr : constant array(aleph..heth) of float :=
     (1/5, 1/6, 1/7, 1/8, 1/9, 1/10, 1/11, 1 );
  samples : array(aleph..heth) of natural := (0, 0, 0, 0, 0, 0, 0, 0);
  value : float;
begin
  for try in 1..trials loop
    value := numerics.random;
    for i in arrays.first( pr )..arrays.last( pr ) loop
       if value <= pr(i) then
         samples(i) := samples(i) + 1;
         exit;
       else
         value := @ - pr(i);
       end if;
    end loop;
  end loop;
  -- Show results
  for i in arrays.first( pr )..arrays.last( pr ) loop
    put( i ) @ ( " " ) @ ( float( samples( i ) ) / float( trials ) );
    if i = heth then
       put_line( " rest" );
    else
       put_line( pr(i) );
    end if;
  end loop;
end randdist;

