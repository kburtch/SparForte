------------------------------------------------------------------------------
-- Arrays Package Parser                                                    --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2011 Free Software Foundation              --
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

with ada.strings.unbounded, world, scanner;
use  ada.strings.unbounded, world, scanner;

package parser_arrays is

------------------------------------------------------------------------------
-- Arrays package identifiers
------------------------------------------------------------------------------

arrays_first_t       : identifier;
arrays_last_t        : identifier;
arrays_length_t      : identifier;
arrays_bubble_sort_t : identifier;
arrays_bubble_sort_descending_t : identifier;
arrays_heap_sort_t   : identifier;
arrays_heap_sort_descending_t : identifier;
arrays_shuffle_t     : identifier;
arrays_flip_t        : identifier;
arrays_rotate_left_t : identifier;
arrays_rotate_right_t: identifier;
arrays_shift_left_t  : identifier;
arrays_shift_right_t : identifier;
arrays_from_json_t   : identifier;
arrays_to_json_t     : identifier;

-----------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupArrays;
procedure ShutdownArrays;

------------------------------------------------------------------------------
-- PARSE THE ARRAYS PACKAGE
------------------------------------------------------------------------------

procedure ParseArraysFirst( f : out unbounded_string; kind : out identifier );
procedure ParseArraysLast( f : out unbounded_string; kind : out identifier );
procedure ParseArraysLength( f : out unbounded_string );
procedure ParseArraysBubbleSort;
procedure ParseArraysBubbleSortDescending;
procedure ParseArraysHeapSort;
procedure ParseArraysHeapSortDescending;
procedure ParseArraysShuffle;
procedure ParseArraysFlip;
procedure ParseArraysRotateLeft;
procedure ParseArraysRotateRight;
procedure ParseArraysShiftLeft;
procedure ParseArraysShiftRight;
procedure ParseArraysFromJSON;
procedure ParseArraysToJSON;

end parser_arrays;
