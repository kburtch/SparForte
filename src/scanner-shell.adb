------------------------------------------------------------------------------
-- AdaScript Language Scanner (Bourne Shell)                                 --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2025 Free Software Foundation              --
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

with scanner.communications;
use  scanner.communications;

package body scanner.shell is

-----------------------------------------------------------------------------
--
--  Shell Mini-scanner
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  GET NEXT CHAR
-----------------------------------------------------------------------------

procedure getNextChar(
   wordLen : natural;
   wordPos : in out natural ) is
begin
   if endOfShellWord then
      err( +"end of characters in shell command" );
   elsif wordPos = wordLen then
      endOfShellWord := true;
   else
      wordPos := wordPos + 1;
   end if;
end getNextChar;


-----------------------------------------------------------------------------
--  EXPECT CHAR
-----------------------------------------------------------------------------

procedure expectChar(
   expectedChar : character;
   rawWordValue : aRawShellWord;
   wordLen : natural;
   wordPos : in out natural ) is
begin
   if endOfShellWord then
      err( pl( "'" & expectedChar & "' expected" ) );
   elsif expectedChar = element( rawWordValue, wordPos ) then
      getNextChar( wordLen, wordPos );
   elsif expectedChar = ''' then
      err( +"missing single quote" );
   elsif expectedChar = '"' then
      err( +"missing double quote" );
   elsif expectedChar = '`' then
      err( +"missing backquote" );
   else
      err( pl( "'" & expectedChar & "' expected" ) );
   end if;
end expectChar;


-----------------------------------------------------------------------------
--  RESET SHELL SCANNER
-----------------------------------------------------------------------------

procedure resetShellScanner is
begin
  -- TODO: this depends on wordPos and wordLen
  endOfShellWord := false;
end resetShellScanner;


end scanner.shell;
