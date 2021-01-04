------------------------------------------------------------------------------
-- AdaScript Language Parser (Bourne Shell)                                 --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with ada.strings.unbounded,
     gen_list,
     scanner,
     scanner.shell;

use  ada.strings.unbounded,
     scanner,
     scanner.shell;

package parser.decl.shell is

shellWord : aShellWord;

package bourneShellWordLists is new gen_list( anExpandedShellWord, ">","=" );


semicolon_string : constant unbounded_string := to_unbounded_string( ";" );
--   semi-colon, as an unbounded string

verticalbar_string : constant unbounded_string := to_unbounded_string( "|" );
--   vertical bar, as an unbounded string

ampersand_string : constant unbounded_string := to_unbounded_string( "&" );
--   ampersand, as an unbounded string

redirectIn_string : constant unbounded_string := to_unbounded_string( "<" );
--   less than, as an unbounded string

redirectOut_string : constant unbounded_string := to_unbounded_string( ">" );
--   greater than, as an unbounded string

redirectAppend_string : constant unbounded_string := to_unbounded_string( ">>" );
--   double greater than, as an unbounded string

redirectErrOut_string : constant unbounded_string := to_unbounded_string( "2>" );
--   '2' + greater than, as an unbounded string

redirectErrAppend_string : constant unbounded_string := to_unbounded_string( "2>>" );
--   '2' + double greater than, as an unbounded string

redirectErr2Out_string : constant unbounded_string := to_unbounded_string( "2>&1" );
--   '2' + greater than + ampersand and '1', as an unbounded string

itself_string : constant unbounded_string := to_unbounded_string( "@" );
--   itself, as an unbounded string


-----------------------------------------------------------------------------
--
--  Public Subprograms
--
-----------------------------------------------------------------------------


procedure addAdaScriptValue( wordList : in out bourneShellWordLists.List;
  unbounded_val : unbounded_string );
-- Add an AdaScript value to the shell word list as if it was a final shell
-- word.

procedure parseShellWord(
      --rawWordValue : aRawShellWord;
      bourneShellWordList : in out bourneShellWordLists.List );

procedure parseUniqueShellWord( shellWord : in out anExpandedShellWord );
-- A shell word that expands into exactly one word.

-----------------------------------------------------------------------------
--
--  Housekeeping
--
-----------------------------------------------------------------------------


--procedure startShellScanner;

--procedure stopShellScanner;

end parser.decl.shell;
