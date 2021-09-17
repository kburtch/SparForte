#!/usr/local/bin/spar

pragma annotate( summary, "side_effects" );
pragma annotate( description, "A demonstration of expression side-effect detection" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure side_effects is
  global_var : natural := 0;

  -- oops adds inc to global_var and returns it

  function oops( inc : natural ) return natural is
  begin
    global_var := global_var + inc;
    return inc;
  end oops;

  temp : natural;
begin

  -- PROBLEM
  --
  -- Global var is changed by function oops.  These two expressions
  -- no longer produce the same result.
  --
  -- temp := oops( 5 ) + global_var;
  -- temp := global_var + oops( 5 );

  temp := oops( 5 ) + global_var;  -- ERROR here

end side_effects;

-- VIM editor formatting instructions
-- vim: ft=spar

