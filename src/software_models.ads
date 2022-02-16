------------------------------------------------------------------------------
-- Software Models                                                          --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
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

with ada.strings.unbounded;
use  ada.strings.unbounded;

package software_models is


------------------------------------------------------------------------------
-- Unbounded String Constants
------------------------------------------------------------------------------

system_script_software_model_name : unbounded_string
   := to_unbounded_string( "System.Script_Software_Model" );
shell_script_model_name : unbounded_string
   := to_unbounded_string( "shell_script" );
shell_batch_model_name : unbounded_string
   := to_unbounded_string( "shell_batch" );
shell_filter_script_model_name : unbounded_string
   := to_unbounded_string( "shell_filter_script" );
shell_report_script_model_name : unbounded_string
   := to_unbounded_string( "shell_report_script" );

------------------------------------------------------------------------------
-- Software Model Requirements
------------------------------------------------------------------------------

-- Class Root

type softwareModelRequirements is abstract tagged null record;

type softwareModelPtr is access all softwareModelRequirements'class;

-- NONSTANDARD

type nonstandardRequirements is new softwareModelRequirements with record
  hasStandardError : boolean := false;
  hasExitStatus    : boolean := false;
end record;

function name( r : nonstandardRequirements ) return unbounded_string;

function meetsRequirements( r : nonstandardRequirements ) return boolean;

function error( r : nonstandardRequirements ) return unbounded_string;

-- SHELL SCRIPT

type shellScriptRequirements is new softwareModelRequirements with record
  hasStandardError : boolean := false;
  hasExitStatus    : boolean := false;
end record;

function name( r : shellScriptRequirements ) return unbounded_string;

function meetsRequirements( r : shellScriptRequirements ) return boolean;

function error( r : shellScriptRequirements ) return unbounded_string;


-- SHELL BATCH

type shellBatchRequirements is new shellScriptRequirements with record
  null;
end record;

function name( r : shellBatchRequirements ) return unbounded_string;

function meetsRequirements( r : shellBatchRequirements ) return boolean;

function error( r : shellBatchRequirements ) return unbounded_string;


-- SHELL FILTER SCRIPT

type shellFilterScriptRequirements is new shellScriptRequirements with record
  null;
end record;

function name( r : shellFilterScriptRequirements ) return unbounded_string;

function meetsRequirements( r : shellFilterScriptRequirements ) return boolean;

function error( r : shellFilterScriptRequirements ) return unbounded_string;


-- SHELL REPORT SCRIPT

type shellReportScriptRequirements is new shellScriptRequirements with record
  null;
end record;

function name( r : shellReportScriptRequirements ) return unbounded_string;

function meetsRequirements( r : shellReportScriptRequirements ) return boolean;

function error( r : shellReportScriptRequirements ) return unbounded_string;

end software_models;

