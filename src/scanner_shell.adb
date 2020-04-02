------------------------------------------------------------------------------
-- AdaScript Language Scanner (Shell)                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2020 Free Software Foundation              --
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

with gen_list,
     world,
     scanner;

use  world,
     scanner;

package body scanner_shell is

package shellWordLists is new gen_list( shellWord, ">","=" );

shellWordList : shellWordLists.List;


-----------------------------------------------------------------------------
--
--  Expansions and Globbing
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  SCAN TILDE
-----------------------------------------------------------------------------

procedure scanTilde( rawWordValue : shellWord ) is
begin
   null;
end scanTilde;

-----------------------------------------------------------------------------
--  SCAN DOLLAR EXPANSION
-----------------------------------------------------------------------------

procedure scanDollarExpansion( rawWordValue : shellWord ) is
begin
   null;
end scanDollarExpansion;

-----------------------------------------------------------------------------
--  SCAN GLOB PATTERN
-----------------------------------------------------------------------------

procedure scanGlobPattern( rawWordValue : shellWord ) is
begin
   null;
end scanGlobPattern;


-----------------------------------------------------------------------------
--
--  Quote handling
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  SCAN BARE QUOTED SHELL WORD
-----------------------------------------------------------------------------

procedure scanBareQuotedShellWord( rawWordValue : shellWord ) is
begin
   null;
end scanBareQuotedShellWord;

-----------------------------------------------------------------------------
--  SCAN SINGLE QUOTED SHELL WORD
-----------------------------------------------------------------------------

procedure scanSingleQuotedShellWord( rawWordValue : shellWord ) is
begin
   null;
end scanSingleQuotedShellWord;

-----------------------------------------------------------------------------
--  SCAN DOUBLE QUOTED SHELL WORD
-----------------------------------------------------------------------------

procedure scanSingleQuotedShellWord( rawWordValue : shellWord ) is
begin
   null;
end scanSingleQuotedShellWord;

-----------------------------------------------------------------------------
--  SCAN SHELL WORD
--
-- Treat the token as a shell word.
-- Perform any substitutions, expansions and word splitting.
-----------------------------------------------------------------------------

procedure scanShellWord( rawWordValue : shellWord ) is
   rawWordValue : constant shellWord := unbounded_string(
      identifiers( token ).value.all );
begin
   null;
end scanShellWord;


-----------------------------------------------------------------------------
--
--  Public Subprograms
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE NEXT SHELL WORD
--
-- return the next shell word
-----------------------------------------------------------------------------

procedure getNextShellWord is
begin
  if not shellWordLists.Empty( shellWordList ) then
     shellWordLists.Pull( shellWordList, shellWord );
  else
     getNextToken;
     scanShellWord;
  end if;
end getNextShellWord;


-----------------------------------------------------------------------------
--
--  Housekeeping
--
-----------------------------------------------------------------------------


procedure startShellScanner is
begin
  null;
end startShellScanner;

procedure stopShellScanner is
begin
  null;
end stopShellScanner;

end scanner_shell;
