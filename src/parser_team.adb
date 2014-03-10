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

  declareIdent( team_member_t, "team.member", root_record_t, typeClass );
  identifiers( team_member_t ).value := to_unbounded_string( "18" );

  declareIdent( team_member_desc_t, "team.member.description", string_t, subClass );
  identifiers(  team_member_desc_t ).field_of := team_member_t;
  identifiers(  team_member_desc_t ).value := to_unbounded_string( "1" );

  declareIdent( team_member_skills_t, "team.member.skills", string_t, subClass );
  identifiers(  team_member_skills_t ).field_of := team_member_t;
  identifiers(  team_member_skills_t ).value := to_unbounded_string( "2" );

  declareIdent( team_member_lang_t, "team.member.lang", string_t, subClass );
  identifiers(  team_member_lang_t ).field_of := team_member_t;
  identifiers(  team_member_lang_t ).value := to_unbounded_string( "3" );

  declareIdent( team_member_id_t, "team.member.id", natural_t, subClass );
  identifiers(  team_member_id_t ).field_of := team_member_t;
  identifiers(  team_member_id_t ).value := to_unbounded_string( "4" );

  declareIdent( team_member_prefcontact_t, "team.member.preferred_contact", string_t, subClass );
  identifiers(  team_member_prefcontact_t ).field_of := team_member_t;
  identifiers(  team_member_prefcontact_t ).value := to_unbounded_string( "5" );

  declareIdent( team_member_email_t, "team.member.email", string_t, subClass );
  identifiers(  team_member_email_t ).field_of := team_member_t;
  identifiers(  team_member_email_t ).value := to_unbounded_string( "6" );

  declareIdent( team_member_sec_email_t, "team.member.secondary_email", string_t, subClass );
  identifiers(  team_member_sec_email_t ).field_of := team_member_t;
  identifiers(  team_member_sec_email_t ).value := to_unbounded_string( "7" );

  declareIdent( team_member_prefname_t, "team.member.preferred_name", string_t, subClass );
  identifiers(  team_member_prefname_t ).field_of := team_member_t;
  identifiers(  team_member_prefname_t ).value := to_unbounded_string( "8" );

  declareIdent( team_member_full_t, "team.member.full_name", string_t, subClass );
  identifiers(  team_member_full_t ).field_of := team_member_t;
  identifiers(  team_member_full_t ).value := to_unbounded_string( "9" );

  declareIdent( team_member_chair_t, "team.member.chair", string_t, subClass );
  identifiers(  team_member_chair_t ).field_of := team_member_t;
  identifiers(  team_member_chair_t ).value := to_unbounded_string( "10" );

  declareIdent( team_member_nickname_t, "team.member.nickname", string_t, subClass );
  identifiers(  team_member_nickname_t ).field_of := team_member_t;
  identifiers(  team_member_nickname_t ).value := to_unbounded_string( "11" );

  declareIdent( team_member_business_phone_t, "team.member.business_phone", string_t, subClass );
  identifiers(  team_member_business_phone_t ).field_of := team_member_t;
  identifiers(  team_member_business_phone_t ).value := to_unbounded_string( "12" );

  declareIdent( team_member_messenging_t, "team.member.messenging", string_t, subClass );
  identifiers(  team_member_messenging_t ).field_of := team_member_t;
  identifiers(  team_member_messenging_t ).value := to_unbounded_string( "13" );

  declareIdent( team_member_teams_t, "team.member.teams", string_t, subClass );
  identifiers(  team_member_teams_t ).field_of := team_member_t;
  identifiers(  team_member_teams_t ).value := to_unbounded_string( "14" );

  declareIdent( team_member_manager_t, "team.member.manager", string_t, subClass );
  identifiers(  team_member_manager_t ).field_of := team_member_t;
  identifiers(  team_member_manager_t ).value := to_unbounded_string( "15" );

  declareIdent( team_member_roles_t, "team.member.roles", string_t, subClass );
  identifiers(  team_member_roles_t ).field_of := team_member_t;
  identifiers(  team_member_roles_t ).value := to_unbounded_string( "16" );

  declareIdent( team_member_active_t, "team.member.active", boolean_t, subClass );
  identifiers(  team_member_active_t ).field_of := team_member_t;
  identifiers(  team_member_active_t ).value := to_unbounded_string( "17" );

  declareIdent( team_member_active_t, "team.member.is_team", boolean_t, subClass );
  identifiers(  team_member_active_t ).field_of := team_member_t;
  identifiers(  team_member_active_t ).value := to_unbounded_string( "18" );

  declareIdent( team_work_measure_t, "team.work_measure", root_enumerated_t, typeClass );
  declareStandardConstant( team_work_measure_unknown_t, "work_measure.unknown", team_work_measure_t, "0" );
  declareStandardConstant( team_work_measure_hours_t, "work_measure.hours", team_work_measure_t, "1" );
  declareStandardConstant( team_work_measure_fpoints_t, "work_measure.function_points", team_work_measure_t, "2" );
  declareStandardConstant( team_work_measure_spoints_t, "work_measure.story_points", team_work_measure_t, "3" );
  declareStandardConstant( team_work_measure_sloc_t, "work_measure.lines_of_code", team_work_measure_t, "4" );
  declareStandardConstant( team_work_measure_size_t, "work_measure.size", team_work_measure_t, "5" );

  declareIdent( team_work_priority_t, "team.work_priority", root_enumerated_t, typeClass );
  declareStandardConstant( team_work_priority_unknown_t, "work_priority.unknown", team_work_priority_t, "0" );
  declareStandardConstant( team_work_priority_level_t, "work_priority.level", team_work_priority_t, "1" );
  declareStandardConstant( team_work_priority_severity_t, "work_priority.severity", team_work_priority_t, "2" );
  declareStandardConstant( team_work_priority_risk_t, "work_priority.risk", team_work_priority_t, "3" );

end StartupTeam;

procedure ShutdownTeam is
begin
  null;
end ShutdownTeam;

end parser_team;
