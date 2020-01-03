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

with world;
use  world;

package parser_teams is

------------------------------------------------------------------------------
-- Teams package identifiers
------------------------------------------------------------------------------

teams_member_t       : identifier;
teams_member_desc_t        : identifier;
teams_member_skills_t      : identifier;
teams_member_lang_t        : identifier;
teams_member_id_t          : identifier;
teams_member_prefcontact_t : identifier;
teams_member_email_t       : identifier;
teams_member_sec_email_t   : identifier;
teams_member_prefname_t    : identifier;
teams_member_full_t        : identifier;
teams_member_chair_t       : identifier;
teams_member_nickname_t    : identifier;
teams_member_business_phone_t : identifier;
teams_member_messenging_t  : identifier;
teams_member_teams_t       : identifier;
teams_member_manager_t     : identifier;
teams_member_roles_t       : identifier;
teams_member_active_t      : identifier;
teams_member_is_team_t     : identifier;

teams_work_measure_t         : identifier;
teams_work_measure_unknown_t : identifier;
teams_work_measure_hours_t   : identifier;
teams_work_measure_fpoints_t : identifier;
teams_work_measure_spoints_t : identifier;
teams_work_measure_sloc_t    : identifier;
teams_work_measure_size_t    : identifier;

teams_work_priority_t        : identifier;
teams_work_priority_unknown_t : identifier;
teams_work_priority_completed_t : identifier;
teams_work_priority_level_t   : identifier;
teams_work_priority_severity_t : identifier;
teams_work_priority_risk_t    : identifier;
teams_work_priority_cvss_t    : identifier;

-----------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupTeams;
procedure ShutdownTeams;

end parser_teams;
