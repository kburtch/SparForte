------------------------------------------------------------------------------
-- Teams Package Parser                                                     --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2020 Free Software Foundation              --
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

with ada.strings.unbounded,
     world,
     scanner,
     parser_params;
use  ada.strings.unbounded,
     world,
     scanner,
     parser_params;

package body parser_teams is

---------------------------------------------------------
-- PARSE THE TEAMS PACKAGE
---------------------------------------------------------

-------------------------------------------------------------------------------
-- Housekeeping
-------------------------------------------------------------------------------

procedure StartupTeams is
begin
  declareNamespace( "teams" );
  declareIdent( teams_member_t, "teams.member", root_record_t, typeClass );
  identifiers( teams_member_t ).value.all := to_unbounded_string( "18" );

  declareIdent( teams_member_desc_t, "teams.member.description", string_t, subClass );
  identifiers(  teams_member_desc_t ).field_of := teams_member_t;
  identifiers(  teams_member_desc_t ).value.all := to_unbounded_string( "1" );

  declareIdent( teams_member_skills_t, "teams.member.skills", string_t, subClass );
  identifiers(  teams_member_skills_t ).field_of := teams_member_t;
  identifiers(  teams_member_skills_t ).value.all := to_unbounded_string( "2" );

  declareIdent( teams_member_lang_t, "teams.member.lang", string_t, subClass );
  identifiers(  teams_member_lang_t ).field_of := teams_member_t;
  identifiers(  teams_member_lang_t ).value.all := to_unbounded_string( "3" );

  declareIdent( teams_member_id_t, "teams.member.id", natural_t, subClass );
  identifiers(  teams_member_id_t ).field_of := teams_member_t;
  identifiers(  teams_member_id_t ).value.all := to_unbounded_string( "4" );

  declareIdent( teams_member_prefcontact_t, "teams.member.preferred_contact", string_t, subClass );
  identifiers(  teams_member_prefcontact_t ).field_of := teams_member_t;
  identifiers(  teams_member_prefcontact_t ).value.all := to_unbounded_string( "5" );

  declareIdent( teams_member_email_t, "teams.member.email", string_t, subClass );
  identifiers(  teams_member_email_t ).field_of := teams_member_t;
  identifiers(  teams_member_email_t ).value.all := to_unbounded_string( "6" );

  declareIdent( teams_member_sec_email_t, "teams.member.secondary_email", string_t, subClass );
  identifiers(  teams_member_sec_email_t ).field_of := teams_member_t;
  identifiers(  teams_member_sec_email_t ).value.all := to_unbounded_string( "7" );

  declareIdent( teams_member_prefname_t, "teams.member.preferred_name", string_t, subClass );
  identifiers(  teams_member_prefname_t ).field_of := teams_member_t;
  identifiers(  teams_member_prefname_t ).value.all := to_unbounded_string( "8" );

  declareIdent( teams_member_full_t, "teams.member.full_name", string_t, subClass );
  identifiers(  teams_member_full_t ).field_of := teams_member_t;
  identifiers(  teams_member_full_t ).value.all := to_unbounded_string( "9" );

  declareIdent( teams_member_chair_t, "teams.member.chair", string_t, subClass );
  identifiers(  teams_member_chair_t ).field_of := teams_member_t;
  identifiers(  teams_member_chair_t ).value.all := to_unbounded_string( "10" );

  declareIdent( teams_member_nickname_t, "teams.member.nickname", string_t, subClass );
  identifiers(  teams_member_nickname_t ).field_of := teams_member_t;
  identifiers(  teams_member_nickname_t ).value.all := to_unbounded_string( "11" );

  declareIdent( teams_member_business_phone_t, "teams.member.business_phone", string_t, subClass );
  identifiers(  teams_member_business_phone_t ).field_of := teams_member_t;
  identifiers(  teams_member_business_phone_t ).value.all := to_unbounded_string( "12" );

  declareIdent( teams_member_messenging_t, "teams.member.messenging", string_t, subClass );
  identifiers(  teams_member_messenging_t ).field_of := teams_member_t;
  identifiers(  teams_member_messenging_t ).value.all := to_unbounded_string( "13" );

  declareIdent( teams_member_teams_t, "teams.member.teams", string_t, subClass );
  identifiers(  teams_member_teams_t ).field_of := teams_member_t;
  identifiers(  teams_member_teams_t ).value.all := to_unbounded_string( "14" );

  declareIdent( teams_member_manager_t, "teams.member.manager", string_t, subClass );
  identifiers(  teams_member_manager_t ).field_of := teams_member_t;
  identifiers(  teams_member_manager_t ).value.all := to_unbounded_string( "15" );

  declareIdent( teams_member_roles_t, "teams.member.roles", string_t, subClass );
  identifiers(  teams_member_roles_t ).field_of := teams_member_t;
  identifiers(  teams_member_roles_t ).value.all := to_unbounded_string( "16" );

  declareIdent( teams_member_active_t, "teams.member.active", boolean_t, subClass );
  identifiers(  teams_member_active_t ).field_of := teams_member_t;
  identifiers(  teams_member_active_t ).value.all := to_unbounded_string( "17" );

  declareIdent( teams_member_active_t, "teams.member.is_team", boolean_t, subClass );
  identifiers(  teams_member_active_t ).field_of := teams_member_t;
  identifiers(  teams_member_active_t ).value.all := to_unbounded_string( "18" );

  declareIdent( teams_work_measure_t, "teams.work_measure", root_enumerated_t, typeClass );
  declareIdent( teams_work_priority_t, "teams.work_priority", root_enumerated_t, typeClass );

  declareNamespaceClosed( "teams" );

  declareNamespace( "work_measure" );
  declareStandardConstant( teams_work_measure_unknown_t, "work_measure.unknown", teams_work_measure_t, "0" );
  declareStandardConstant( teams_work_measure_hours_t, "work_measure.hours", teams_work_measure_t, "1" );
  declareStandardConstant( teams_work_measure_fpoints_t, "work_measure.function_points", teams_work_measure_t, "2" );
  declareStandardConstant( teams_work_measure_spoints_t, "work_measure.story_points", teams_work_measure_t, "3" );
  declareStandardConstant( teams_work_measure_sloc_t, "work_measure.lines_of_code", teams_work_measure_t, "4" );
  declareStandardConstant( teams_work_measure_size_t, "work_measure.size", teams_work_measure_t, "5" );
  declareNamespaceClosed( "work_measure" );

  declareNamespace( "work_priority" );
  declareStandardConstant( teams_work_priority_unknown_t, "work_priority.unknown", teams_work_priority_t, "0" );
  declareStandardConstant( teams_work_priority_completed_t, "work_priority.completed", teams_work_priority_t, "1" );
  declareStandardConstant( teams_work_priority_level_t, "work_priority.level", teams_work_priority_t, "2" );
  declareStandardConstant( teams_work_priority_severity_t, "work_priority.severity", teams_work_priority_t, "3" );
  declareStandardConstant( teams_work_priority_risk_t, "work_priority.risk", teams_work_priority_t, "4" );
  declareStandardConstant( teams_work_priority_cvss_t, "work_priority.cvss", teams_work_priority_t, "5" );
  declareNamespaceClosed( "work_priority" );

end StartupTeams;

procedure ShutdownTeams is
begin
  null;
end ShutdownTeams;

end parser_teams;
