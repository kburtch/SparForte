#!/usr/local/bin/spar

pragma annotate( summary, "distance" )
       @( description, "Calculate the distance between two points on the Earth's surface" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma ada_95;
pragma restriction( no_external_commands );

procedure distance is

  here_lat   : float;
  here_long  : float;
  there_lat  : float;
  there_long : float;
  distance_raw : float;

  earth_radius : constant float := 3963.0;

begin

  put( " Starting Latitude: " );
  here_lat := numerics.value( get_line );
  put( "Starting Longitude: " );
  here_long := numerics.value( get_line );

  put( "   Ending Latitude: " );
  there_lat := numerics.value( get_line );
  put( "  Ending Longitude: " );
  there_long := numerics.value( get_line );

  new_line;

  distance_raw :=
    numerics.sin( here_lat, 360 ) * numerics.sin( there_lat, 360 ) +
    numerics.cos( here_lat, 360 ) * numerics.cos( there_lat, 360 ) *
    numerics.cos( there_long - here_long, 360 );

  declare
     distance : constant float := earth_radius * numerics.arccos( distance_raw );
  begin
    put_line( "Distance between the two places is" );
    put( distance, "ZZZZZ.99999" );
    put_line( " miles" );
  end;

end distance;

-- VIM editor formatting instructions
-- vim: ft=spar

