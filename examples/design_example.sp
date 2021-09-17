#!/usr/local/bin/spar

pragma annotate( summary, "design_example" );
pragma annotate( description, "A demonstration of design features." );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure design_example is

  team_awesome : teams.member;

  type customer is new natural;
  no_customer : constant customer := 0;

  type supplier is new natural;
  no_supplier : constant supplier := 0;

  total : integer;

  function get_customer return customer is null abstract;
  pragma constraint( optimization, scalability );
  pragma constraint( physical, network );
  pragma todo( team_awesome, "get_customer", work_measure.story_points, 2, work_priority.severity, 3 );
  -- a stub function

  function get_supplier return supplier is null abstract;
  pragma constraint( optimization, performance ); -- ERROR: design constraint conflict
  pragma constraint( physical, network );
  pragma todo( team_awesome, "get_supplier", work_measure.story_points, 2, work_priority.severity, 3 );
  -- a stub function

  procedure set_customer( c : customer; name : string ) is null abstract;
  pragma constraint( optimization, scalability );
  pragma constraint( physical, network );
  pragma todo( team_awesome, "set_customer", work_measure.story_points, 2, work_priority.severity, 3 );
  -- a stub function

  procedure set_supplier( s : supplier; name : string ) is null abstract;
  pragma constraint( optimization, scalability );
  pragma constraint( physical, network );
  pragma todo( team_awesome, "set_supplier", work_measure.story_points, 2, work_priority.severity, 3 );
  -- a stub function

begin
  null; -- no main program
end design_example;

-- VIM editor formatting instructions
-- vim: ft=spar

