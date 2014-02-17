------------------------------------------------------------------------------
-- Team Package Parser                                                      --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2014 Free Software Foundation              --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  This is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

with bush_os,
     string_util,
     world,
     scanner,
     parser,
     parser_aux,
     parser_params;
use  bush_os,
     string_util,
     world,
     scanner,
     parser,
     parser_aux,
     parser_params;

package body parser_team is


---------------------------------------------------------
-- PARSE THE TEAM PACKAGE
---------------------------------------------------------

-------------------------------------------------------------------------------
-- Housekeeping
-------------------------------------------------------------------------------

procedure StartupTeam is
begin

  declareIdent( team_programmer_t, "team.programmer", root_record_t, typeClass );
  identifiers( team_programmer_t ).value := to_unbounded_string( "17" );

  declareIdent( team_prog_desc_t, "team.programmer.description", string_t, subClass );
  identifiers(  team_prog_desc_t ).field_of := team_programmer_t;
  identifiers(  team_prog_desc_t ).value := to_unbounded_string( "1" );

  declareIdent( team_prog_skills_t, "team.programmer.skills", string_t, subClass );
  identifiers(  team_prog_skills_t ).field_of := team_programmer_t;
  identifiers(  team_prog_skills_t ).value := to_unbounded_string( "2" );

  declareIdent( team_prog_lang_t, "team.programmer.lang", string_t, subClass );
  identifiers(  team_prog_lang_t ).field_of := team_programmer_t;
  identifiers(  team_prog_lang_t ).value := to_unbounded_string( "3" );

  declareIdent( team_prog_id_t, "team.programmer.id", natural_t, subClass );
  identifiers(  team_prog_id_t ).field_of := team_programmer_t;
  identifiers(  team_prog_id_t ).value := to_unbounded_string( "4" );

  declareIdent( team_prog_prefcontact_t, "team.programmer.preferred_contact", string_t, subClass );
  identifiers(  team_prog_prefcontact_t ).field_of := team_programmer_t;
  identifiers(  team_prog_prefcontact_t ).value := to_unbounded_string( "5" );

  declareIdent( team_prog_email_t, "team.programmer.email", string_t, subClass );
  identifiers(  team_prog_email_t ).field_of := team_programmer_t;
  identifiers(  team_prog_email_t ).value := to_unbounded_string( "6" );

  declareIdent( team_prog_sec_email_t, "team.programmer.secondary_email", string_t, subClass );
  identifiers(  team_prog_sec_email_t ).field_of := team_programmer_t;
  identifiers(  team_prog_sec_email_t ).value := to_unbounded_string( "7" );

  declareIdent( team_prog_prefname_t, "team.programmer.preferred_name", string_t, subClass );
  identifiers(  team_prog_prefname_t ).field_of := team_programmer_t;
  identifiers(  team_prog_prefname_t ).value := to_unbounded_string( "8" );

  declareIdent( team_prog_full_t, "team.programmer.full_name", string_t, subClass );
  identifiers(  team_prog_full_t ).field_of := team_programmer_t;
  identifiers(  team_prog_full_t ).value := to_unbounded_string( "9" );

  declareIdent( team_prog_chair_t, "team.programmer.chair", string_t, subClass );
  identifiers(  team_prog_chair_t ).field_of := team_programmer_t;
  identifiers(  team_prog_chair_t ).value := to_unbounded_string( "10" );

  declareIdent( team_prog_nickname_t, "team.programmer.nickname", string_t, subClass );
  identifiers(  team_prog_nickname_t ).field_of := team_programmer_t;
  identifiers(  team_prog_nickname_t ).value := to_unbounded_string( "11" );

  declareIdent( team_prog_business_phone_t, "team.programmer.business_phone", string_t, subClass );
  identifiers(  team_prog_business_phone_t ).field_of := team_programmer_t;
  identifiers(  team_prog_business_phone_t ).value := to_unbounded_string( "12" );

  declareIdent( team_prog_messenging_t, "team.programmer.messenging", string_t, subClass );
  identifiers(  team_prog_messenging_t ).field_of := team_programmer_t;
  identifiers(  team_prog_messenging_t ).value := to_unbounded_string( "13" );

  declareIdent( team_prog_teams_t, "team.programmer.teams", string_t, subClass );
  identifiers(  team_prog_teams_t ).field_of := team_programmer_t;
  identifiers(  team_prog_teams_t ).value := to_unbounded_string( "14" );

  declareIdent( team_prog_manager_t, "team.programmer.manager", string_t, subClass );
  identifiers(  team_prog_manager_t ).field_of := team_programmer_t;
  identifiers(  team_prog_manager_t ).value := to_unbounded_string( "15" );

  declareIdent( team_prog_roles_t, "team.programmer.roles", string_t, subClass );
  identifiers(  team_prog_roles_t ).field_of := team_programmer_t;
  identifiers(  team_prog_roles_t ).value := to_unbounded_string( "16" );

  declareIdent( team_prog_active_t, "team.programmer.active", string_t, subClass );
  identifiers(  team_prog_active_t ).field_of := team_programmer_t;
  identifiers(  team_prog_active_t ).value := to_unbounded_string( "17" );

end StartupTeam;

procedure ShutdownTeam is
begin
  null;
end ShutdownTeam;

end parser_team;
