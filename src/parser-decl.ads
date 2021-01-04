------------------------------------------------------------------------------
-- AdaScript Language Parser                                                --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
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
     world;
use ada.strings.unbounded,
    world;

package parser.decl is

   ------------------------------------------------------------------------------
   -- ADASCRIPT PARSER - DECLARATIONS
   --
   -- Declarations are split off here to reduce the size of the main parser file.
   ------------------------------------------------------------------------------

   procedure ParseAffirmBlock;
   procedure ParseType;
   procedure ParseSubtype;
   procedure ParseDeclarationPart( id : in out identifier; anon_arrays : boolean; exceptions : boolean );
   procedure ParseAssignPart( expr_value : out unbounded_string; expr_type : out identifier );
   procedure ParseRecordDeclaration( id : identifier; recType : identifier; canAssign : boolean := true );

end parser.decl;
