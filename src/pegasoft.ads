------------------------------------------------------------------------------
-- PEGASOFT parent package                                                --
--                                                                          --
-- Part of SparForte                                                        --
-- Designed and Programmed by Ken O. Burtch                                 --
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

with System,
     ada.strings.unbounded;
use  ada.strings.unbounded;

package pegasoft is

type integerOutputType is delta 0.1 digits System.Max_Digits-2;
--   delta can't be 1, so we'll settle for 0.1.  This is the largest
--   long float number we can convert to an integer (and vice vera)
--   without rounding or resorting to scientific notation.  Why
--   -2? Anything higher resulted in rounding of Max_Int and Min_Int.

function to_numeric( s : unbounded_string ) return long_float;

function to_unbounded_string( f : long_float ) return unbounded_string;

end pegasoft;
