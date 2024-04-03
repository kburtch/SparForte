------------------------------------------------------------------------------
-- AWK Package Parser                                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2024 Free Software Foundation              --
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

with ada.strings.unbounded, world;
use  ada.strings.unbounded, world;

package parser_awk is


------------------------------------------------------------------------------
-- AWK package identifiers
------------------------------------------------------------------------------

awk_count_t       : identifier;
awk_default_separators : identifier;
awk_use_current   : identifier;
awk_session_type_t : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupAWK;
procedure ShutdownAWK;

------------------------------------------------------------------------------
-- PARSE THE AWK PACKAGE
------------------------------------------------------------------------------

procedure ParseAwkSetCurrent;
procedure ParseAwkCurrentSession( result : out unbounded_string );
procedure ParseAwkDefaultSession( result : out unbounded_string );
procedure ParseAwkSetFieldSeparators;
procedure ParseAwkSetFS;
procedure ParseAwkSetFieldWidths;
procedure ParseAwkAddFile;
procedure ParseAwkAddFiles;
procedure ParseAwkNumberOfFields( result : out unbounded_string );
procedure ParseAwkNF( result : out unbounded_string );
procedure ParseAwkNunberOfFileLines( result : out unbounded_string );
procedure ParseAwkFNR( result : out unbounded_string );
procedure ParseAwkNumberOfLines( result : out unbounded_string );
procedure ParseAwkNR( result : out unbounded_string );
procedure ParseAwkNumberOfFiles( result : out unbounded_string );
procedure ParseAwkFile( result : out unbounded_string );
procedure ParseAwkField( result : out unbounded_string );
procedure ParseAwkDiscreteField( result : out unbounded_string );

end parser_awk;
