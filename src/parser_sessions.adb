------------------------------------------------------------------------------
-- Sessions Package Parser                                                  --
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

package body parser_sessions is


---------------------------------------------------------
-- PARSE THE SESSIONS PACKAGE
---------------------------------------------------------

-------------------------------------------------------------------------------
-- Housekeeping
-------------------------------------------------------------------------------

procedure StartupSessions is
begin

  declareIdent( session_variable_name_t,  sessions_session_variable_name_str, string_t, varClass );
  declareIdent( session_variable_value_t, sessions_session_variable_value_str, string_t, varClass );

end StartupSessions;

procedure ShutdownSessions is
begin
  null;
end ShutdownSessions;

end parser_sessions;
