#!/usr/local/bin/spar

pragma annotate( summary, "calcrms" )
       @( description, "Compute the Root mean square of the numbers 1..10." )
       @( description, "The root mean square is also known by its initial RMS (or rms), and as the" )
       @( description, "quadratic mean.  The RMS is calculated as the mean of the squares of the" )
       @( description, "numbers, square-rooted" )
       @( category, "algorithms" )
       @( see_also, "http://rosettacode.org/wiki/Averages/Root_mean_square" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure calcrms is
  type float_arr is array(1..10) of float;
  list : constant float_arr := (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0);
  total: float := 0.0;
  rms  : float;
begin
  for p in arrays.first(list)..arrays.last(list) loop
      total := @ + list(p)**2;
  end loop;
  rms := numerics.sqrt( total / float(arrays.length(list)));
  ?  rms;
end calcrms;

-- VIM editor formatting instructions
-- vim: ft=spar

