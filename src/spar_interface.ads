------------------------------------------------------------------------------
-- SparForte Ada Language API                                               --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2023 Free Software Foundation              --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with ada.strings.unbounded,
     world,
     compiler,
     scanner,
     user_io,
     parser,
     signal_flags;
use  ada.strings.unbounded,
     world,
     compiler,
     scanner,
     user_io,
     parser,
     signal_flags;

package spar_interface is

type spar_session is tagged private;

------------------------------------------------------------------------------
-- Setup
------------------------------------------------------------------------------

procedure Set_Command_Line_Option( S : spar_session'class; option_string : string );

------------------------------------------------------------------------------
-- running things
------------------------------------------------------------------------------

procedure Interpret_Commands( S : spar_session'class; commandString : string; preserve : boolean := true );

procedure Interpret_Script( S : spar_session'class; scriptPath : string; preserve : boolean := true );

------------------------------------------------------------------------------
-- Finish Up
------------------------------------------------------------------------------

function Get_Status return natural;

function Get_Value( S : spar_session'class; name : string ) return string;

------------------------------------------------------------------------------
private
------------------------------------------------------------------------------

type spar_session is tagged record
   lastError         : unbounded_string;
   raisingErrors     : boolean := true;

   lastStatus        : aStatusCode;
   savedScriptState  : aScriptState;
   savedScannerState : aScannerState;
end record;

end spar_interface;

