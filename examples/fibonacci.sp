#!/usr/local/bin/spar

pragma annotate( summary, "fibonacci n" );
pragma annotate( description, "Write a function to generate the nth Fibonacci number." );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure fibonacci is

  -- fib
  -- return the nth fibonacci number

  function fib( n : natural ) return natural is
    a : natural := 0;
    b : natural := 1;
    sum : natural;
  begin
    for i in 1..n loop
        sum := a+b;
        b := a;
        a := sum;
    end loop;
    return sum;
  end fib;

  param  : integer := numerics.value( $1 );

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
  ? fib( natural( param ) );
end fibonacci;

-- VIM editor formatting instructions
-- vim: ft=spar

