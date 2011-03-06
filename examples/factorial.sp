#!/usr/local/bin/bush

pragma annotate( "factorial" );
pragma annotate( "" );
pragma annotate( "Write a function to return the factorial of a number." );
pragma annotate( "by Ken O. Burtch" );

pragma restriction( no_external_commands );

procedure factorial is
  param  : integer := numerics.value( $1 );
  result : natural;
  count  : natural;
begin
  if param < 0 then
     put_line( standard_error, source_info.source_location & ": number must be >= 0" );
     command_line.set_exit_status( 192 );
     return;
  end if;
  if param = 0 then
     ? 0;
     return;
  end if;
  result := natural( param );
  count  := natural( param - 1 );
  for i in reverse 1..count loop
      result := @ * i;
  end loop;
  ? result;
end factorial;

