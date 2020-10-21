#!/usr/local/bin/spar

pragma annotate( summary, "fibonacci n" )
       @( description, "Write a function to generate the nth Fibonacci number." )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure fibonacci is

  -- fib
  -- return the nth fibonacci number

  function fib( n : natural ) return natural is
    a : natural := 0;
    b : natural := 1;
    sum_total : natural;
  begin
    for i in 1..n loop
        sum_total := a+b;
        b := a;
        a := sum_total;
    end loop;
    return sum_total;
  end fib;

  fib_pos : constant integer := numerics.value( $1 );

begin
  if fib_pos < 0 then
     put_line( standard_error, source_info.source_location & ": number must be >= 0" );
     command_line.set_exit_status( 192 );
     return;
  end if;
  if fib_pos = 0 then
     ? 0;
     return;
  end if;
  ? fib( natural( fib_pos ) );
end fibonacci;

-- VIM editor formatting instructions
-- vim: ft=spar

