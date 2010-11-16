------------------------------------------------------------------------------
-- BUSH Scanner Arrays                                                       --
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
-- CVS: $Id: scanner_arrays.adb,v 1.2 2005/02/11 02:59:30 ken Exp $

with ada.text_io;
use  ada.text_io;

package body scanner_arrays is

arrayElements : stringList.List;
bushArrays    : arrayList.List;

function arraySort( left, right : bushArray ) return boolean is
begin
  return true; -- we don't care
end arraySort;

procedure findArray( name : unbounded_string; id : out arrayID )is
  b : bushArray;
begin
  id := 0;
  for i in reverse 1..arrayList.length( bushArrays ) loop
      arrayList.Find( bushArrays, i, b );
      if b.name = name then
         id := arrayID( i );
         exit;
      end if;
  end loop;
end findArray;

procedure declareArray( id : out arrayID; name : unbounded_string;
  first, last : long_integer;
  ind : identifier; blocklvl : block ) is
  b   : bushArray;
  lastElement : arrayElementID;
  -- create a new array
begin
  b.name := name;
  b.blocklvl := blocklvl;
  b.firstIndex := first;
  b.lastIndex := last;
  b.indType := ind;
  if first <= last then
     b.offset := arrayElementID( stringList.length( arrayElements )+1 );
  else
     b.offset := 0;
  end if;
  begin
    arrayList.Queue( bushArrays, b );
  exception when storage_error =>
      err( "out of memory" );
      id := 0;
      return;
  end; 
  id := arrayID( arrayList.length( bushArrays ) );
  for i in 1..last-first+1 loop
      stringList.Queue( arrayElements, Null_Unbounded_String );
  end loop;
  if trace then
     put_trace( to_string( name ) & " declared as array #" & id'img );
     put_trace( "There are" & stringList.length( arrayElements )'img & " array elements declared by all arrays" );
  end if;
  exception when storage_error =>
      err( "out of memory" );
      lastElement := arrayElementID( stringList.length( arrayElements ) );
      for i in b.offset..lastElement loop
          stringList.Clear( arrayElements, stringList.AListIndex( i ) );
      end loop;
      b.offset := 0;
      id := 0;
      return;
end declareArray;

procedure declareArrayType( id : out arrayID; name : unbounded_string;
  first, last : long_integer;
  ind : identifier; blocklvl : block ) is
  b   : bushArray;
  -- create a new array
begin
  b.name := name;
  b.blocklvl := blocklvl;
  b.firstIndex := first;
  b.lastIndex := last;
  b.indType := ind;
  b.offset := 0;
  b.isType := true;
  begin
    arrayList.Queue( bushArrays, b );
  exception when storage_error =>
      err( "out of memory" );
      id := 0;
      return;
  end; 
  id := arrayID( arrayList.length( bushArrays ) );
  if trace then
     put_trace( to_string( name ) & " declared as array #" & id'img );
  end if;
end declareArrayType;

function indexTypeOK( array_id : arrayID; id : identifier )
  return boolean is
  b : bushArray;
  -- true if index type is compatible
begin
  arrayList.Find( bushArrays, arrayList.AListIndex( array_id ), b );
  return baseTypesOK( id, b.indType );
end indexTypeOK;

function indexType( array_id : arrayID ) return identifier is
  b : bushArray;
  -- return the index type
begin
  arrayList.Find( bushArrays, arrayList.AListIndex( array_id ), b );
  return b.indType;
end indexType;

function inBounds( id : arrayID; index : long_integer )
return boolean is
  b : bushArray;
  result : boolean;
  -- true if index is in bounds
begin
  arrayList.Find( bushArrays, arrayList.AListIndex( id ), b );
  result := index >= b.firstIndex and index <= b.lastIndex;
  if not result then
     err( "array index out of bounds" );
  end if;
  return result;
end inBounds;

function firstBound( id : arrayID ) return long_integer is
  -- return low array bound
  b : bushArray;
begin
  arrayList.Find( bushArrays, arrayList.AListIndex( id ), b );
  return b.firstIndex;
end firstBound;

function lastBound( id : arrayID ) return long_integer is
  -- return high array bound
  b : bushArray;
begin
  arrayList.Find( bushArrays, arrayList.AListIndex( id ), b );
  return b.lastIndex;
end lastBound;

function arrayElement( id : arrayID; index : long_integer )
return unbounded_string is
-- fetch an item from an array.  Assumes the index is good
  b : bushArray;
  s : unbounded_string;
begin
  arrayList.Find( bushArrays, arrayList.AListIndex( id ), b );
  if b.firstIndex = 1 and b.lastIndex = 0 then
     return null_unbounded_string;
  end if;
--put_line( "b.offset = " & b.offset'img );
--put_line( "index = " & index'img );
--put_line( "b.firstIndex = " & b.FirstIndex'img );
  stringList.Find( arrayElements, stringList.AListIndex( b.offset ) +
    (index-b.firstIndex), s );
  return s;
end arrayElement;

procedure assignElement( id : arrayID; index: long_integer;
value : unbounded_string ) is
-- fetch an item from an array.  Assumes the index is good
  b : bushArray;
begin
  arrayList.Find( bushArrays, arrayList.AListIndex( id ), b );
  if b.firstIndex = 1 and b.lastIndex = 0 then
     return;
  end if;
--put_line( "b.offset = " & b.offset'img );
--put_line( "index = " & index'img );
--put_line( "b.firstIndex = " & b.FirstIndex'img );
  stringList.Replace( arrayElements, stringList.AListIndex( b.offset ) +
    (index-b.firstIndex), value );
end assignElement;

procedure clearArray( id : arrayID ) is
-- delete an array and deallocate any memory
  b : bushArray;
begin
  b.offset := arrayElementID'last;
  arrayList.Find( bushArrays, long_integer( id ), b );
  if b.offset = arrayElementID'last then
     err( "internal error: destroyArray: already destroyed or bad id" );
     return;
  end if;
  if not b.isType then
     if b.lastIndex >= b.firstIndex then
        for i in reverse b.offset..b.offset+arrayElementID( b.lastIndex-b.firstIndex) loop
            stringList.Clear( arrayElements, stringList.AListIndex( i ) );
        end loop;
     end if;
  end if;
  -- Deleting the array record will change the array index numbers since
  -- we are using linked lists.
  -- arrayList.clear( bushArrays, long_integer( id ) );
  if trace then
     put_trace( "There are" & stringList.length( arrayElements )'img & " remaining array elements declared by all arrays" );
  end if;
end clearArray;

procedure pullArrayBlock( blocklvl : block ) is
-- called by scanner.pullBlcok, discard all arrays and memory declared
-- by them.
  b : bushArray;
  best : bushArray;
  lastElement : arrayElementID;
begin
  best.offset := 0;                                        -- assume none
  for i in reverse 1..arrayList.length( bushArrays ) loop  -- look thru arrays
      arrayList.Find( bushArrays, i, b );                  -- next array
      exit when b.blocklvl < blocklvl;                     -- completed block?
      if b.offset > 0 then                                 -- not a null array?
         best := b;                                        -- delete this too
      end if;
      arrayList.Clear( bushArrays, i );                    -- destroy array rec
      if trace then
         put_trace( to_string( b.name ) & " array deallocated" );
      end if;
  end loop;
  if best.offset > 0 then                                  -- dealloc elements
     --put_trace( "Starting with array " & to_string( best.name ) );
     --put_trace( "First element to be discarded is" & best.offset'img );
     --put_trace( "Last element to be discarded is" & stringList.length( arrayElements )'img );
     lastElement := arrayElementID( stringList.length( arrayElements ) );
     for e in reverse best.offset..lastElement loop
         stringList.Clear( arrayElements, stringList.AListIndex( e ) );
     end loop;
  end if;
  if trace then
     put_trace( "There are" & stringList.length( arrayElements )'img & " array elements declared by all arrays" );
  end if;
end pullArrayBlock;

end scanner_arrays;

