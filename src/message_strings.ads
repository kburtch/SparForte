------------------------------------------------------------------------------
-- SparForte markup messages and escaping functions                         --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2026 Free Software Foundation              --
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
     world;
use  ada.strings.unbounded,
     world;

package message_strings is

------------------------------------------------------------------------------
-- Message "mark up" and escaping functions
------------------------------------------------------------------------------

function getNewLine return messageStrings;
-- get a new line appropriate for an error message

function "&"( left, right : messageStrings ) return messageStrings;
-- concatenation

function pl( s : string ) return messageStrings;
function pl( c : character ) return messageStrings;
function "+"( s : string ) return messageStrings renames pl;
-- plain text

function name_em( id : identifier ) return messageStrings;
-- identifier name field, emphasized

function unb_pl( us : unbounded_string ) return messageStrings;
-- unbounded plain text

function ok( s : string ) return messageStrings;
-- success text (usually green)

function inv( s : string ) return messageStrings;
-- inverse text

function em( s : string ) return messageStrings;
-- emphasized text

function unb_em( us : unbounded_string ) return messageStrings;
-- unbounded string emphasized text

function em_esc( s : unbounded_string ) return messageStrings;
-- escape plus emphasize

function em_esc( c : character ) return messageStrings;
-- escape plus emphasize

function em_value( s : unbounded_string ) return messageStrings;
-- combines bold, secured data and escaped.  A null value will
-- return double single quotes.

function qp( s : string ) return string;
-- Uppercase the first letter of the string if quiet option is in use.
-- The string is then passed to pl() or em() to turn into a message string.

end message_strings;
