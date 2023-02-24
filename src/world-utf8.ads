------------------------------------------------------------------------------
-- UTF-8 Icons                                                              --
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

package world.utf8 is

-----------------------------------------------------------------------------
--  UTF-8 icons
--
-----------------------------------------------------------------------------

function utf_ballot return string;
-- Return the bytes for a UTF-8 "x" ballot symbol.

function utf_checkmark return string;
-- Return the bytes for a UTF-8 checkmark symbol.

function utf_warning_sign return string;
-- Return the bytes for a UTF-8 warning sign symbol.

function utf_wristwatch return string;
-- Return the bytes for a UTF-8 watch symbol.

function utf_left return string;
-- Return the bytes for a UTF-8 left-end of underline

function utf_right return string;
-- Return the bytes for a UTF-8 right-end of underline

function utf_triangle return string;
-- Return the bytes for a UTF-8 triangle

function utf_horizontalLine return string;
-- Return the bytes for a UTF-8 horizontal line, otherwise a minus sign.

function utf_horizontalLineOnly return string;
-- Return the bytes for a UTF-8 horizontal line

function utf_crossedLines return string;
-- Return the bytes for a UTF-8 crossed lines, otherwise a plus sign.

function utf_verticalLine return string;
-- Return the bytes for a UTF-8 vertical line, otherwise a vertical bar.

function utf_bullet return string;
-- Return the bytes for a UTF-8 bullet, otherwise asterisk

function utf_diamond return string;
-- Return the bytes for a UTF-8 diamond, otherwise a minus sign

end world.utf8;

