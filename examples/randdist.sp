#!/usr/local/bin/spar

pragma annotate( summary, "randdist" );
pragma annotate( description, "Given a mapping between items and their required" );
pragma annotate( description, "probability of occurrence, generate a million items" );
pragma annotate( description, "randomly subject to the given probabilities and compare" );
pragma annotate( description, "the target probability of occurrence versus the" );
pragma annotate( description, "generated values." );
pragma annotate( description, "" );
pragma annotate( description, "The total of all the probabilities should equal one." );
pragma annotate( description, "(Because floating point arithmetic is involved this is" );
pragma annotate( description, "subject to rounding errors).  Use the following mapping" );
pragma annotate( description, "to test your programs: aleph 1/5.0, beth 1/6.0," );
pragma annotate( description, "gimel 1/7.0, daleth 1/8.0, he 1/9.0,  waw 1/10.0" );
pragma annotate( description, "zayin 1/11.0, heth 1759/27720 adjusted so that" );
pragma annotate( description, "probabilities add to 1" );
pragma annotate( see_also, "http://rosettacode.org/wiki/Probabilistic_choice" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

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

-- VIM editor formatting instructions
-- vim: ft=spar

