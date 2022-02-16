------------------------------------------------------------------------------
-- AdaScript Language Parser                                                --
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

with ada.strings.unbounded,
     world, scanner;
use ada.strings.unbounded,
    world, scanner;

package parser.decl.as is

   ------------------------------------------------------------------------------
   -- ADASCRIPT PARSER - Declarations - AdaScript
   --
   -- Remaining parsing functions not covered in core file, such as executable
   -- features (loops, conditionals, blocks, Bourne shell, SQL, etc.)
   ------------------------------------------------------------------------------

   procedure CompileAndRun( commands : unbounded_string; firstLineNo : natural := 1; fragment : boolean := true );
   procedure CompileRunAndCaptureOutput( commands : unbounded_string; results : out
      unbounded_string; firstLineNo : natural := 1  );

   procedure ParseGeneralStatement;
   procedure ParseExecutableStatement;
   procedure ParseStaticIfBlock;
   procedure ParseStaticCaseBlock;

--    procedure SkipBlock( termid1, termid2 : identifier := keyword_t );
   procedure ParseBlock( termid1, termid2 : identifier := keyword_t );
   procedure ParseBlockExecutablePartStatement;
   procedure parseNewCommands( scriptState : out aScriptState; byteCode : unbounded_string; fragment : boolean := true );
   procedure DoUserDefinedFunction( s : unbounded_string; result : out unbounded_string );

   procedure parsePolicy;
   procedure parseConfig;
   procedure parse;

end parser.decl.as;
