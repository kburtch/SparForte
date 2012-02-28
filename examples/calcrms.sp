#!/usr/local/bin/spar

pragma annotate( summary, "calcrms" );
pragma annotate( description, "Compute the Root mean square of the numbers 1..10." );
pragma annotate( description, "The root mean square is also known by its initial RMS (or rms), and as the" );
pragma annotate( description, "quadratic mean.  The RMS is calculated as the mean of the squares of the" );
pragma annotate( description, "numbers, square-rooted" );
pragma annotate( see_also, "http://rosettacode.org/wiki/Averages/Root_mean_square" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure calcrms is
  type float_arr is array(1..10) of float;
  list : float_arr := (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0);
  sum  : float := 0.0;
  rms  : float;
begin
  for p in arrays.first(list)..arrays.last(list) loop
      sum := @ + list(p)**2;
  end loop;
  rms := numerics.sqrt( sum / float(arrays.length(list)));
  ?  rms;
end calcrms;

-- VIM editor formatting instructions
-- vim: ft=spar

