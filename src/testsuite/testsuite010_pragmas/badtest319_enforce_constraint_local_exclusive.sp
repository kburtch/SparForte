# This should fail
procedure t is
pragma declare_constraint( subprogram, development_team, team_a );
pragma declare_constraint( subprogram, development_team, team_b );

procedure inner is
   pragma constraint( development_team, team_a );
   pragma constraint( development_team, team_b ); -- conflict
begin
  null;
end inner;

begin
  inner;
end t;

