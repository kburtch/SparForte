------------------------------------------------------------------------------
-- Enums Package Parser                                                     --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2011 Free Software Foundation              --
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

with ada.strings.unbounded, world, scanner;
use  ada.strings.unbounded, world, scanner;

package parser_enums is

------------------------------------------------------------------------------
-- Arrays package identifiers
------------------------------------------------------------------------------

enums_first_t        : identifier;
enums_last_t         : identifier;
enums_prev_t         : identifier;
enums_succ_t         : identifier;

-----------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupEnums;
procedure ShutdownEnums;

------------------------------------------------------------------------------
-- PARSE THE ENUMS PACKAGE
------------------------------------------------------------------------------

procedure ParseEnumsFirst( f : out unbounded_string; kind : out identifier );
procedure ParseEnumsLast( f : out unbounded_string; kind : out identifier );
procedure ParseEnumsPrev( f : out unbounded_string; kind : out identifier );
procedure ParseEnumsSucc( f : out unbounded_string; kind : out identifier );

end parser_enums;
