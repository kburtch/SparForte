#!/usr/local/bin/spar

pragma annotate( summary, "factorial n" );
pragma annotate( description, "Write a function to return the factorial of a number." );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

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

-- VIM editor formatting instructions
-- vim: ft=spar

