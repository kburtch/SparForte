------------------------------------------------------------------------------
-- Parser Side Effects                                                      --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2018 Free Software Foundation              --
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
use  ada.strings.unbounded,
     world;

package parser_sidefx is

procedure checkExpressionFactorVolatility( id: identifier );
-- Check to see if a factor in an expression is read after it was written
-- to after the expression was started.

procedure checkExpressionFactorVolatilityOnWrite( id: identifier; context : line_count );
-- Check to see if an identifier is written after it was read
-- by an expression, while the expression is incomplete.

procedure checkDoubleThreadWrite( id : identifier );
-- Check to see if, at run-time, two different "threads" write to the same
-- unprotected variable.  Also updates writtenByThread.

procedure checkDoubleGlobalWrite( id : identifier );
-- A strict check for anything being written twice after an expression
-- is started.

end parser_sidefx;
