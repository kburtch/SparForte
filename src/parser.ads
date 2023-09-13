------------------------------------------------------------------------------
-- AdaScript Language Parser                                                --
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
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

with ada.strings.unbounded,
     world;
use ada.strings.unbounded,
    world;

package parser is

------------------------------------------------------------------------------
-- ADASCRIPT CORE PARSER
--
-- Basic parsing for identifiers and expressions, split off to reduce the
-- size of the main parser file.
------------------------------------------------------------------------------

procedure ParseFieldIdentifier( record_id : identifier; id : out identifier );
procedure ParseProcedureIdentifier( id : out identifier );
procedure ParseVariableIdentifier( id : out identifier );
procedure ParseNewIdentifier( id : out identifier );
procedure ParseIdentifier( id : out identifier );
procedure ParseStaticIdentifier( id : out identifier );
procedure ParseProgramName( program_id : out identifier );
procedure ParseExpression( ex : out unbounded_string; expr_type : out identifier );
procedure ParseStaticExpression( ex : out unbounded_string; expr_type : out identifier );

procedure ParsePragmaIdentifier;
procedure ParsePragmaIdentifier( name : out unbounded_string );
procedure ParseDesignPragmaConstraintIdentifier( name : out unbounded_string );
procedure ParseDesignPragmaAffinityIdentifier( name : out unbounded_string );
procedure ParseDesignPragmaModeIdentifier( name : out unbounded_string );
procedure ParseDesignPragmaAffinityModeIdentifier( name : out unbounded_string );

procedure DoContracts( kind_id : identifier; expr_val : in out unbounded_string );


------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure startParser;
procedure shutdownParser;

end parser;
