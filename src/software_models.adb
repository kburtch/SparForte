------------------------------------------------------------------------------
-- Software Models                                                          --
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

package body software_models is

-- NONSTANDARD

function name( r : nonstandardRequirements ) return unbounded_string is
begin
  return shell_script_model_name;
end name;

function meetsRequirements( r : nonstandardRequirements ) return boolean is
begin
  return true;
end meetsRequirements;

function error( r : nonstandardRequirements ) return unbounded_string is
  msg : unbounded_string;
begin
  return null_unbounded_string;
end error;


-- SHELL SCRIPT

function name( r : shellScriptRequirements ) return unbounded_string is
begin
  return shell_script_model_name;
end name;

function meetsRequirements( r : shellScriptRequirements ) return boolean is
begin
  return r.hasStandardError and
         r.hasExitStatus;
end meetsRequirements;

function error( r : shellScriptRequirements ) return unbounded_string is
  msg : unbounded_string;
begin
  if r.hasStandardError then
     msg := "software model " & r.name & " requires standard_error";
  elsif r.hasExitStatus then
     msg := "software model " & r.name & " requires set_exit_status";
  end if;
  return msg;
end error;


-- SHELL BATCH

function name( r : shellBatchRequirements ) return unbounded_string is
begin
  return shell_batch_model_name;
end name;

function meetsRequirements( r : shellBatchRequirements ) return boolean is
begin
  return meetsRequirements( shellScriptRequirements( r ) );
end meetsRequirements;

function error( r : shellBatchRequirements ) return unbounded_string is
  msg : unbounded_string;
begin
  return error( shellScriptRequirements( r ) );
end error;


-- SHELL FILTER SCRIPT

function name( r : shellFilterScriptRequirements ) return unbounded_string is
begin
  return shell_filter_script_model_name;
end name;

function meetsRequirements( r : shellFilterScriptRequirements ) return boolean is
begin
  return meetsRequirements( shellScriptRequirements( r ) );
end meetsRequirements;

function error( r : shellFilterScriptRequirements ) return unbounded_string is
  msg : unbounded_string;
begin
  return error( shellScriptRequirements( r ) );
end error;


-- SHELL REPORT SCRIPT

function name( r : shellReportScriptRequirements ) return unbounded_string is
begin
  return shell_report_script_model_name;
end name;

function meetsRequirements( r : shellReportScriptRequirements ) return boolean is
begin
  return meetsRequirements( shellScriptRequirements( r ) );
end meetsRequirements;

function error( r : shellReportScriptRequirements ) return unbounded_string is
  msg : unbounded_string;
begin
  return error( shellScriptRequirements( r ) );
end error;

end software_models;

