------------------------------------------------------------------------------
-- UTF-8 Icons                                                              --
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

package body world.utf8 is

-----------------------------------------------------------------------------
--  UTF-8 icons
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  UTF BALLOT
--
-- Output the bytes for a UTF-8 "x" ballot symbol.
-----------------------------------------------------------------------------

function utf_ballot return string is
begin
  if colourOpt then
     -- if colourOpt then
     --    return character'val( 226 ) & character'val( 157 ) & character'val( 140 );
     -- else
     return character'val( 226 ) & character'val( 156 ) & character'val( 151 );
     -- end if;
  end if;
  return "";
end utf_ballot;


-----------------------------------------------------------------------------
--  UTF CHECKMARK
--
-- Output the bytes for a UTF-8 checkmark symbol.
-----------------------------------------------------------------------------
function utf_checkmark return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 156 ) & character'val( 147 );
  end if;
  return "";
end utf_checkmark;


-----------------------------------------------------------------------------
--  UTF WARNING SIGN
--
-- Output the bytes for a UTF-8 warning sign symbol.
-----------------------------------------------------------------------------

function utf_warning_sign return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 154 ) & character'val( 160 );
  end if;
  return "";
end utf_warning_sign;


-----------------------------------------------------------------------------
--  UTF WRISTWATCH
--
-- Output the bytes for a UTF-8 watch symbol.
-----------------------------------------------------------------------------

function utf_wristwatch return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 140 ) & character'val( 154 );
  end if;
  return "";
end utf_wristwatch;


-----------------------------------------------------------------------------
--  UTF LEFT
--
-- Output the bytes for a UTF-8 left upturn line.
-----------------------------------------------------------------------------

function utf_left return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 148 ) & character'val( 148 );
  end if;
  return "";
end utf_left;


-----------------------------------------------------------------------------
--  UTF RIGHT
--
-- Output the bytes for a UTF-8 right upturn line.
-----------------------------------------------------------------------------

function utf_right return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 148 ) & character'val( 152 );
  end if;
  return "";
end utf_right;


-----------------------------------------------------------------------------
--  UTF TRIANGLE
--
-- Output the bytes for a UTF-8 triangle.
-----------------------------------------------------------------------------

function utf_triangle return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 150 ) & character'val( 179 );
  end if;
  return "";
end utf_triangle;


-----------------------------------------------------------------------------
--  UTF HORIZONTAL LINE
--
-- Output the bytes for a horizontal line symbol or a minus.
-----------------------------------------------------------------------------

function utf_horizontalLine return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 148 ) & character'val( 128 );
  end if;
  return "-";
end utf_horizontalLine;

function utf_horizontalLineOnly return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 148 ) & character'val( 128 );
  end if;
  return "";
end utf_horizontalLineOnly;


-----------------------------------------------------------------------------
--  UTF CROSSED LINES
--
-- Return the bytes for a UTF-8 crossed lines, otherwise a plus sign.
-----------------------------------------------------------------------------

function utf_crossedLines return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 148 ) & character'val( 188 );
  end if;
  return "+";
end utf_crossedLines;


-----------------------------------------------------------------------------
--  UTF VERTICAL LINE
--
-- Return the bytes for a UTF-8 vertical line, otherwise a vertical bar.
-----------------------------------------------------------------------------

function utf_verticalLine return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 148 ) & character'val( 130 );
  end if;
  return "|";
end utf_verticalLine;


-----------------------------------------------------------------------------
--  UTF BULLET
--
-- Return the bytes for a UTF-8 bullet, otherwise asterisk
-----------------------------------------------------------------------------

function utf_bullet return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 128 ) & character'val( 162 );
  end if;
  return "*";
end utf_bullet;


-----------------------------------------------------------------------------
--  UTF DIAMOND
--
-- Return the bytes for a UTF-8 diamond, otherwise a minus sign
-----------------------------------------------------------------------------

function utf_diamond return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 151 ) & character'val( 134 );
  end if;
  return "-";
end utf_diamond;

end world.utf8;

