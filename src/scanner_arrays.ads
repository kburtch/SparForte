------------------------------------------------------------------------------
-- BUSH Scanner Arrays                                                      --
--                                                                          --
-- Part of BUSH                                                             --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2005 Ken O. Burtch & FSF                 --
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
-- CVS: $Id: scanner_arrays.ads,v 1.2 2005/02/11 02:59:31 ken Exp $

with gen_list,
     unchecked_deallocation,
     ada.strings.unbounded,
     ada.finalization,
     world,
     scanner;
use  ada.strings.unbounded,
     ada.finalization,
     world,
     scanner;

package scanner_arrays is

-- This package implements arrays for the BUSH shell.


------------------------------------------------------------------------------
-- Array Elements
--
-- Everything in BUSH is represented as string values.  Array elements
-- will be saved in a linked list of strings.
------------------------------------------------------------------------------

package stringList is new gen_list( unbounded_string, ">", "=" );

type arrayElementID is new stringList.AListIndex;
-- tighter type checking

------------------------------------------------------------------------------
-- Array Info
--
-- Arrays have upper and lower bounds, an index type, etc.  Instead of
-- storing this information in the global symbol table (identifiers),
-- we'll store it in this table.  Since arrays involve memory allocation,
-- this works out doubly good since items on the identifier table won't
-- have to be searched when a block ends: search only the array table
-- which we know will have dynamic array elements that to be deallocated.
--
-- bushArray should be private but is be public for the instantiation here
------------------------------------------------------------------------------

type bushArray is record
     name       : unbounded_string;           -- declared name (for debugging)
     firstIndex : long_integer := 0;          -- low array bound
     lastIndex  : long_integer := 0;          -- high array bound
     offset     : arrayElementID := 0;        -- first element (0 if none)
     indType    : identifier;                 -- index type
     blocklvl   : block;                      -- declaration block
     isType     : boolean := false;           -- true if is a array type
end record;


------------------------------------------------------------------------------
-- Array Info List
--
-- Array List is a linked list of bushArray descriptions
-- BUSH array types value field contains an index to this list.
-- BUSH array variables value field contains an index to this list, too.
------------------------------------------------------------------------------

function arraySort( left, right : bushArray ) return boolean;
package arrayList is new gen_list( bushArray, arraySort, "=" );

type arrayID is new arrayList.AListIndex;
-- tighter type checking

------------------------------------------------------------------------------
-- Array Subprograms
--
-- These subprograms operate on arrays or array elements.
------------------------------------------------------------------------------

procedure findArray( name : unbounded_string; id : out arrayID );
-- lookup the position of an array in the list.  Returns zero if the array
-- is not found

procedure declareArray( id : out arrayID;
  name  : unbounded_string;
  first : long_integer;
  last  : long_integer;
  ind   : identifier;
  blocklvl : block );
  -- create a new array, allocating space for the array

procedure declareArrayType( id : out arrayID;
  name  : unbounded_string;
  first : long_integer;
  last  : long_integer;
  ind   : identifier;
  blocklvl : block );
  -- create a new array type (don't allocate any space)

function indexTypeOK( array_id : arrayID; id : identifier )
  return boolean;
  -- true if index type is compatible with the array index type

function indexType( array_id : arrayID ) return identifier;
  -- return the index type for the array

function inBounds( id : arrayID; index : long_integer )
  return boolean;
  -- true if index is in bounds

function firstBound( id : arrayID ) return long_integer;
function lastBound( id : arrayID ) return long_integer;
-- return first or last array index bound

function arrayElement( id : arrayID; index : long_integer ) return unbounded_string;
-- fetch an item from an array.  Assumes the index is good

procedure assignElement( id : arrayID; index : long_integer; value : unbounded_string );
-- assign a new value to an array element

procedure clearArray( id : arrayID );
-- deallocate memory used by an array (but leaves the array id unchanged so
-- as not to affect other array ids since arrays are in a linked list)

procedure pullArrayBlock( blocklvl : block );
-- discard all arrays up to and including this block level, deallocating
-- all related memory

end scanner_arrays;

