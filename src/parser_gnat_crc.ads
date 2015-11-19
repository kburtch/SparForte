------------------------------------------------------------------------------
-- BUSH GNAT CRC Package Parser                                             --
--                                                                          --
-- Part of BUSH                                                             --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2009 Ken O. Burtch & FSF                 --
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
-- CVS: $Id$

with ada.strings.unbounded, world;
use  ada.strings.unbounded, world;

package parser_gnat_crc is

------------------------------------------------------------------------------
-- CGI package identifiers
------------------------------------------------------------------------------

gnat_crc32_crc32_t : identifier;

gnat_crc32_initialize_t : identifier;
gnat_crc32_update_t : identifier;
gnat_crc32_get_value_t : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupGnatCRC;
procedure ShutdownGnatCRC;

------------------------------------------------------------------------------
-- PARSE THE CGI PACKAGE
------------------------------------------------------------------------------

-- procedure ParseGnatCRC32Initialize;
-- procedure ParseGnatCRC32Update;
-- procedure ParseGnatCRC32GetValue( result : out unbounded_string; kind : out identifier );

end parser_gnat_crc;
