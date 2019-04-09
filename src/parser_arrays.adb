------------------------------------------------------------------------------
-- Arrays Package Parser                                                    --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2019 Free Software Foundation              --
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

with gnat.bubble_sort_a,
     gnat.heap_sort_a,
     ada.numerics.float_random,
     world,
     string_util,
     performance_monitoring,
     scanner,
     parser_aux,
     parser_sidefx,
     parser,
     parser_params;
use
     world,
     string_util,
     scanner,
     performance_monitoring,
     parser_aux,
     parser_sidefx,
     parser,
     parser_params;

package body parser_arrays is

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
arrays_to_array_t    : identifier;
arrays_to_json_t     : identifier;

---------------------------------------------------------
-- PARSE THE ARRAYS PACKAGE
---------------------------------------------------------

procedure ParseArraysFirst( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: arrays.first( arraytypeorvar );
  -- Source: arraytypeorvar'first
  var_id   : identifier;
  --array_id : arrayID;
begin
  expect( arrays_first_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or
     identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     if identifiers( var_id ).class = subClass or identifiers( var_id ).class = typeClass then
        f := to_unbounded_string( long_integer'image( identifiers( var_id ).firstBound ) );
     else
        begin
           f := to_unbounded_string( long_integer'image( identifiers( var_id ).aValue'first ) );
        exception when CONSTRAINT_ERROR =>
           err( "internal error: constraint_error : index out of range " & identifiers( var_id ).avalue'first'img & " .. " & identifiers( var_id ).avalue'last'img );
        when STORAGE_ERROR =>
           err( "internal error : storage error raised in ParseFactor" );
        end;
     end if;
     kind := identifiers( var_id ).genKind;
     --array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     --kind := indexType( array_id );
     --f := to_unbounded_string( long_integer'image( firstBound( array_id ) ) );
  elsif syntax_check then
     kind := universal_t; -- type is unknown during syntax check
  else                     -- exiting block, etc. still need type info...
     kind := identifiers( var_id ).genKind;
     --array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     --array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     --kind := indexType( array_id );
  end if;
end ParseArraysFirst;

procedure ParseArraysLast( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: arrays.last( arraytypeorvar );
  -- Source: arraytypeorvar'last
  var_id   : identifier;
  --array_id : arrayID;
begin
  expect( arrays_last_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or
     identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     if identifiers( var_id ).class = subClass or identifiers( var_id ).class = typeClass then
        f := to_unbounded_string( long_integer'image( identifiers( var_id ).lastBound ) );
     else
        begin
           f := to_unbounded_string( long_integer'image( identifiers( var_id ).aValue'last ) );
        exception when CONSTRAINT_ERROR =>
           err( "internal error: constraint_error : index out of range " & identifiers( var_id ).avalue'first'img & " .. " & identifiers( var_id ).avalue'last'img );
        when STORAGE_ERROR =>
           err( "internal error : storage error raised in ParseFactor" );
        end;
     end if;
     kind := identifiers( var_id ).genKind;
     --array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     --kind := indexType( array_id );
     --f := to_unbounded_string( long_integer'image( lastBound( array_id ) ) );
  elsif syntax_check then
     kind := universal_t; -- type is unknown during syntax check
  else                     -- exiting block, etc. still need type info...
     kind := identifiers( var_id ).genKind;
     --array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     --array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     --kind := indexType( array_id );
  end if;
end ParseArraysLast;

procedure ParseArraysLength( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: arrays.length( arraytypeorvar );
  -- Source: arraytypeorvar'length
  var_id   : identifier;
  -- array_id : arrayID;
begin
  kind := natural_t;
  expect( arrays_length_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
        f := to_unbounded_string( long_integer'image( identifiers( var_id ).lastBound - identifiers( var_id ).firstBound + 1 ) );
     else
        f := to_unbounded_string( long_integer'image( identifiers( var_id ).avalue'length ) );
     end if;
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     -- f := to_unbounded_string( long_integer'image( lastBound( array_id ) - firstBound( array_id ) + 1 ) );
  end if;
end ParseArraysLength;

-------------------------------------------------------------------------------
-- Array Sorts
-------------------------------------------------------------------------------

-- Stuff for Sorting with GNAT packages
-- I'm concerned that GNAT uses integers for indexes in sorts...may not
-- work with long_integer arrays on some platforms...assuming peole would
-- have such friggin' huge arrays in the first place.

--arrayIdBeingSorted     : arrayID;
offsetArrayBeingSorted : long_integer;
ZeroElement            : unbounded_string;
arrayBeingSortedId     : identifier;

procedure moveElement( From, To : natural ) is
  data : unbounded_string;
begin
  if From = 0 then
     data := ZeroElement;
  else
     -- data := arrayElement( arrayIdBeingSorted, long_integer(From)+offsetArrayBeingSorted);
     data := identifiers( arrayBeingSortedId ).avalue( long_integer(From)+offsetArrayBeingSorted );
  end if;
  if To = 0 then
     ZeroElement := data;
  else
     -- assignElement( arrayIdBeingSorted, long_integer(To)+offsetArrayBeingSorted, data );
     identifiers( arrayBeingSortedId ).avalue( long_integer(To)+offsetArrayBeingSorted ) := data;
   end if;
end moveElement;

function Lt_string( Op1, Op2 : natural ) return boolean is
  data1, data2 : unbounded_string;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     data1 := identifiers( arrayBeingSortedId ).avalue( long_integer( Op1 )+offsetArrayBeingSorted );
     --data1 := arrayElement( arrayIdBeingSorted, long_integer( Op1 )+offsetArrayBeingSorted);
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     --data2 := arrayElement( arrayIdBeingSorted, long_integer( Op2 )+offsetArrayBeingSorted);
     data2 := identifiers( arrayBeingSortedId ).avalue( long_integer( Op2 )+offsetArrayBeingSorted );
  end if;
  return data1 < data2;
end Lt_string;

function Lt_string_descending( Op1, Op2 : natural ) return boolean is
  data1, data2 : unbounded_string;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     -- data1 := arrayElement( arrayIdBeingSorted, long_integer( Op1 )+offsetArrayBeingSorted);
     data1 := identifiers( arrayBeingSortedId ).avalue( long_integer( Op1 )+offsetArrayBeingSorted );
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     -- data2 := arrayElement( arrayIdBeingSorted, long_integer( Op2 )+offsetArrayBeingSorted);
     data2 := identifiers( arrayBeingSortedId ).avalue( long_integer( Op2 )+offsetArrayBeingSorted );
  end if;
  return data1 > data2;
end Lt_string_descending;

function Lt_numeric( Op1, Op2 : natural ) return boolean is
  data1, data2 : unbounded_string;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     -- data1 := arrayElement( arrayIdBeingSorted, long_integer( Op1 )+offsetArrayBeingSorted);
     data1 := identifiers( arrayBeingSortedId ).avalue( long_integer( Op1 )+offsetArrayBeingSorted );
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     -- data2 := arrayElement( arrayIdBeingSorted, long_integer( Op2 )+offsetArrayBeingSorted);
     data2 := identifiers( arrayBeingSortedId ).avalue( long_integer( Op2 )+offsetArrayBeingSorted );
  end if;
  return to_numeric( data1 ) < to_numeric( data2 );
end Lt_numeric;

function Lt_numeric_descending( Op1, Op2 : natural ) return boolean is
  data1, data2 : unbounded_string;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     -- data1 := arrayElement( arrayIdBeingSorted, long_integer( Op1 )+offsetArrayBeingSorted);
     data1 := identifiers( arrayBeingSortedId ).avalue( long_integer( Op1 )+offsetArrayBeingSorted );
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     -- data2 := arrayElement( arrayIdBeingSorted, long_integer( Op2 )+offsetArrayBeingSorted);
     data2 := identifiers( arrayBeingSortedId ).avalue( long_integer( Op2 )+offsetArrayBeingSorted );
  end if;
  return to_numeric( data1 ) > to_numeric( data2 );
end Lt_numeric_descending;

procedure ParseArraysBubbleSort is
  var_id : identifier;
  first, last : long_integer;
  kind   : identifier;
begin
  expect( arrays_bubble_sort_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  --if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
  --   var_id := getBaseType( var_id );
  --   if not identifiers( var_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check and not error_found then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleThreadWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     arrayBeingSortedId := var_id;
     -- arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     -- first := firstBound( arrayIdBeingSorted );
     -- last  := lastBound( arrayIdBeingSorted );
     first := identifiers( var_id ).avalue'first;
     last  := identifiers( var_id ).avalue'last;
     offsetArrayBeingSorted := first-1;
     kind := getUniType( identifiers( var_id ).kind );
     if kind = uni_string_t or kind = universal_t then
        GNAT.Bubble_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_string'access );
     elsif kind = uni_numeric_t or kind = root_enumerated_t then
        GNAT.Bubble_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_numeric'access );
     else
        err( "unable to sort this element type" );
     end if;
  end if;
end ParseArraysBubbleSort;

procedure ParseArraysBubbleSortDescending is
  var_id : identifier;
  first, last : long_integer;
  kind   : identifier;
begin
  expect( arrays_bubble_sort_descending_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  --if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
  --   var_id := getBaseType( var_id );
  --   if not identifiers( var_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check and not error_found then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleThreadWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     arrayBeingSortedId := var_id;
     -- arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     -- first := firstBound( arrayIdBeingSorted );
     -- last  := lastBound( arrayIdBeingSorted );
     first := identifiers( var_id ).avalue'first;
     last  := identifiers( var_id ).avalue'last;
     offsetArrayBeingSorted := first-1;
     kind := getUniType( identifiers( var_id ).kind );
     if kind = uni_string_t or kind = universal_t then
        GNAT.Bubble_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_string_descending'access );
     elsif kind = uni_numeric_t or kind = root_enumerated_t then
        GNAT.Bubble_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_numeric_descending'access );
     else
        err( "unable to sort this element type" );
     end if;
  end if;
end ParseArraysBubbleSortDescending;

procedure ParseArraysHeapSort is
  var_id : identifier;
  first, last : long_integer;
  kind   : identifier;
begin
  expect( arrays_heap_sort_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  --if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
  --   var_id := getBaseType( var_id );
  --   if not identifiers( var_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleThreadWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     arrayBeingSortedId := var_id;
     -- arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     -- first := firstBound( arrayIdBeingSorted );
     -- last  := lastBound( arrayIdBeingSorted );
     first := identifiers( var_id ).avalue'first;
     last  := identifiers( var_id ).avalue'last;
     offsetArrayBeingSorted := first-1;
     kind := getUniType( identifiers( var_id ).kind );
     if kind = uni_string_t or kind = universal_t then
        GNAT.Heap_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_string'access );
     elsif kind = uni_numeric_t or kind = root_enumerated_t then
        GNAT.Heap_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_numeric'access );
     else
        err( "unable to sort this element type" );
     end if;
  end if;
end ParseArraysHeapSort;

procedure ParseArraysHeapSortDescending is
  var_id : identifier;
  first, last : long_integer;
  kind   : identifier;
begin
  expect( arrays_heap_sort_descending_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  --if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
  --   var_id := getBaseType( var_id );
  --   if not identifiers( var_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleThreadWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     arrayBeingSortedId := var_id;
     -- arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     -- first := firstBound( arrayIdBeingSorted );
     -- last  := lastBound( arrayIdBeingSorted );
     first := identifiers( var_id ).avalue'first;
     last  := identifiers( var_id ).avalue'last;
     offsetArrayBeingSorted := first-1;
     kind := getUniType( identifiers( var_id ).kind );
     if kind = uni_string_t or kind = universal_t then
        GNAT.Heap_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_string_descending'access );
     elsif kind = uni_numeric_t or kind = root_enumerated_t then
        GNAT.Heap_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_numeric_descending'access );
     else
        err( "unable to sort this element type" );
     end if;
  end if;
end ParseArraysHeapSortDescending;

procedure ParseArraysShuffle is
  var_id : identifier;
  -- first, last : long_integer;
  newpos : long_integer;
  len    : long_integer;
  -- array_id : arrayID;
  tmp : unbounded_string;
begin
  expect( arrays_shuffle_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleThreadWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     -- first := firstBound( array_id );
     -- last  := lastBound( array_id );
     len   := identifiers( var_id ).avalue'length;
     begin
        for i in identifiers( var_id ).avalue'range loop
            -- unfortunately, ada random numbers are 0..1, so they can be too
            -- large if 1...repeat if that happens
            loop
                newpos := long_integer( long_float'truncation( long_float( len ) *
                    long_float( Ada.Numerics.Float_Random.Random( random_generator
) ) ) );
            newpos := newpos + identifiers( var_id ).avalue'first;
            exit when newpos <= identifiers( var_id ).avalue'last;
            end loop;
            tmp := identifiers( var_id ).avalue( i );
            identifiers( var_id ).avalue( i ) := identifiers( var_id ).avalue( newpos );
            identifiers( var_id ).avalue( newpos ) := tmp;
            -- moveElement( integer(i), 0 );
            -- moveElement( integer(newpos), integer(i) );
            -- moveElement( 0, integer(newpos) );
        end loop;
     exception when CONSTRAINT_ERROR =>
        err( "internal error : index out of range when shuffling range" & identifiers( var_id ).avalue'first'img & " .." & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when shuffling array" );
     end;
  end if;
end ParseArraysShuffle;

procedure ParseArraysFlip is
  var_id : identifier;
  first, last, len : long_integer;
  oldpos : long_integer;
  newpos : long_integer;
  -- array_id : arrayID;
  tmp    : unbounded_string;
begin
  expect( arrays_flip_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array expected" );
  end if;
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleThreadWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     --array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     --first := firstBound( array_id );
     --last  := lastBound( array_id );
     --len   := last-first+1;
     first   := identifiers( var_id ).avalue'first;
     last    := identifiers( var_id ).avalue'last;
     len     := identifiers( var_id ).avalue'length;
     begin
        if last > first then
           for i in 0..len/2 loop
               oldpos := long_integer( first + i );
               newpos := long_integer( last - i );
               tmp := identifiers( var_id ).avalue( oldpos );
               identifiers( var_id ).avalue( oldpos ) := identifiers( var_id ).avalue( newpos );
               identifiers( var_id ).avalue( newpos ) := tmp;
               -- moveElement( integer(i), 0 );
               -- moveElement( integer(newpos), integer(i) );
               -- moveElement( 0, integer(newpos) );
           end loop;
        end if;
     exception when CONSTRAINT_ERROR =>
        err( "internal error : index out of range when copying" & oldpos'img & " and" & newpos'img & " in" & identifiers( var_id ).avalue'first'img & " .." & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when flipping arrays" );
     end;
  end if;
end ParseArraysFlip;

procedure ParseArraysShiftRight is
  var_id : identifier;
  --first, last : long_integer;
  --len    : long_integer;
begin
  expect( arrays_shift_right_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array expected" );
  end if;
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleThreadWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     --arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     --first := firstBound( arrayIdBeingSorted );
     --last  := lastBound( arrayIdBeingSorted );
     --len   := last-first+1;
     --offsetArrayBeingSorted := first-1;
     begin
        for i in reverse identifiers( var_id ).avalue'first..identifiers( var_id ).avalue'last-1 loop
            identifiers( var_id ).avalue( i+1 ) := identifiers( var_id ).avalue( i );
            --moveElement( integer(i), integer(i+1) );
        end loop;
     exception when CONSTRAINT_ERROR =>
        err( "internal error : index out of range when shifting range" & identifiers( var_id ).avalue'first'img & " .." & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when shifting array" );
     end;
  end if;
end ParseArraysShiftRight;

procedure ParseArraysShiftLeft is
  var_id : identifier;
  --first, last : long_integer;
  --len    : long_integer;
begin
  expect( arrays_shift_left_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleThreadWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     --arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     --first := firstBound( arrayIdBeingSorted );
     --last  := lastBound( arrayIdBeingSorted );
     --len   := last-first+1;
     --offsetArrayBeingSorted := first-1;
     begin
        for i in identifiers( var_id ).avalue'first..identifiers( var_id ).avalue'last-1 loop
            identifiers( var_id ).avalue( i ) := identifiers( var_id ).avalue( i+1 );
            --moveElement( integer(i+1), integer(i) );
        end loop;
     exception when CONSTRAINT_ERROR =>
        err( "internal error : index out of range when shifting range" & identifiers( var_id ).avalue'first'img & " .." & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when shifting array" );
     end;
  end if;
end ParseArraysShiftLeft;

procedure ParseArraysRotateRight is
  var_id : identifier;
  -- first, last : long_integer;
  -- len    : long_integer;
  tmp    : unbounded_string;
begin
  expect( arrays_rotate_right_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleThreadWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     -- arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     -- first := firstBound( arrayIdBeingSorted );
     -- last  := lastBound( arrayIdBeingSorted );
     -- len   := last-first+1;
     -- offsetArrayBeingSorted := first-1;
     -- moveElement( integer( len ), 0 );
     begin
        if identifiers( var_id ).avalue'length > 0 then
           tmp := identifiers( var_id ).avalue( identifiers( var_id ).avalue'last );
           for i in reverse identifiers( var_id ).avalue'first..identifiers( var_id ).avalue'last-1 loop
               identifiers( var_id ).avalue( i+1 ) := identifiers( var_id ).avalue( i );
               -- moveElement( integer(i), integer(i+1) );
           end loop;
           identifiers( var_id ).avalue( identifiers( var_id ).avalue'first ) := tmp;
        end if;
        --moveElement( 0, 1 );
     exception when CONSTRAINT_ERROR =>
        err( "internal error : index out of range when rotating range" & identifiers( var_id ).avalue'first'img & " .." & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when rotating array" );
     end;
  end if;
end ParseArraysRotateRight;

procedure ParseArraysRotateLeft is
  var_id : identifier;
  --first, last : long_integer;
  --len    : long_integer;
  tmp : unbounded_string;
begin
  expect( arrays_rotate_left_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleThreadWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     --arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     --first := firstBound( arrayIdBeingSorted );
     --last  := lastBound( arrayIdBeingSorted );
     --len   := last-first+1;
     --offsetArrayBeingSorted := first-1;
     --moveElement( 1, 0 );
     --for i in 1..len-1 loop
         --moveElement( integer(i+1), integer(i) );
     --end loop;
     --moveElement( 0, integer( len ) );
     begin
        if identifiers( var_id ).avalue'length > 0 then
           tmp := identifiers( var_id ).avalue( identifiers( var_id ).avalue'first );
           for i in identifiers( var_id ).avalue'first..identifiers( var_id ).avalue'last-1 loop
               identifiers( var_id ).avalue( i ) := identifiers( var_id ).avalue( i+1 );
               -- moveElement( integer(i), integer(i+1) );
           end loop;
           identifiers( var_id ).avalue( identifiers( var_id ).avalue'last ) := tmp;
        end if;
     exception when CONSTRAINT_ERROR =>
        err( "internal error : index out of range when rotating range" & identifiers( var_id ).avalue'first'img & " .." & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when rotating array" );
     end;
  end if;
end ParseArraysRotateLeft;

procedure ParseArraysToArray is
  -- Syntax: to_array( str_array | num_array, string )
  -- Example: arrays.to_array( a, "[1,2]" )
  --          arrays.to_array( d, "[" & ASCII.Quotation & "foo" &
  --          ASCII.Quotation & "," & ASCII.Quotation & "bar" &
  --          ASCII.Quotation & "]" );
  -- Source: N/A
  target_var_id : identifier;
  --target_base_id: identifier;
  source_val    : unbounded_string;
  source_type   : identifier;
begin
  expect( arrays_to_array_t );
  expect( symbol_t, "(" );
  ParseIdentifier( target_var_id );
  --if identifiers( target_var_id ).class = typeClass or identifiers( target_var_id ).class = subClass then
  --   target_base_id := getBaseType( target_var_id );
  --   if not identifiers( target_base_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( target_var_id, otherClass ) and identifiers( target_var_id ).list) then
  if not (class_ok( target_var_id, varClass ) and identifiers( target_var_id ).list) then
     err( "Array expected" );
  end if;
  expect( symbol_t, "," );
  ParseExpression( source_val, source_type );
  if baseTypesOK( source_type, json_string_t ) then
     expect( symbol_t, ")" );
  end if;
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( target_var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( target_var_id );
     checkDoubleThreadWrite( target_var_id );
     --checkDoubleGlobalWrite( target_var_id );
     identifiers( target_var_id ).writtenOn := perfStats.lineCnt;
     -- DoJsonToArray actually populates the array, so you don't use assign parameter
     -- assignParameter( target_var_id, jsonString );
     begin
       DoJsonToArray( target_var_id, source_val );
     exception when constraint_error =>
       err( "bad JSON string " & to_string( toEscaped( source_val ) ) );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseArraysToArray;

procedure ParseArraysToJSON is
  -- Syntax: to_json( string, array )
  -- Example: arrays.to_json( s, a )
  -- Source: N/A
  source_var_id : identifier;
  target_ref    : reference;
  jsonString    : unbounded_string;
begin
  expect( arrays_to_json_t );
  expect( symbol_t, "(" );
  ParseOutParameter( target_ref, json_string_t );
  expect( symbol_t, "," );
  ParseIdentifier( source_var_id );
  --if identifiers( source_var_id ).class = typeClass or identifiers( source_var_id ).class = subClass then
  --   source_base_id := getBaseType( source_var_id );
  --   if not identifiers( source_base_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( source_var_id, otherClass ) and identifiers( source_var_id ).list) then
  if not (class_ok( source_var_id, varClass ) and identifiers( source_var_id ).list) then
     err( "Array expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     DoArrayToJson( jsonString, source_var_id );
     assignParameter( target_ref, jsonString );
  end if;
end ParseArraysToJSON;

-------------------------------------------------------------------------------
-- Housekeeping
-------------------------------------------------------------------------------

procedure StartupArrays is
begin
  declareNamespace( "arrays" );
  declareFunction( arrays_first_t, "arrays.first", ParseArraysFirst'access );
  declareFunction( arrays_last_t, "arrays.last", ParseArraysLast'access );
  declareFunction( arrays_length_t, "arrays.length", ParseArraysLength'access );
  declareProcedure( arrays_bubble_sort_t, "arrays.bubble_sort", ParseArraysBubbleSort'access );
  declareProcedure( arrays_bubble_sort_descending_t, "arrays.bubble_sort_descending",ParseArraysBubbleSortDescending'access );
  declareProcedure( arrays_heap_sort_t, "arrays.heap_sort", ParseArraysHeapSort'access );
  declareProcedure( arrays_heap_sort_descending_t, "arrays.heap_sort_descending", ParseArraysHeapSortDescending'access );
  declareProcedure( arrays_shuffle_t, "arrays.shuffle", ParseArraysShuffle'access );
  declareProcedure( arrays_flip_t, "arrays.flip", ParseArraysFlip'access );
  declareProcedure( arrays_rotate_left_t, "arrays.rotate_left", ParseArraysRotateLeft'access );
  declareProcedure( arrays_rotate_right_t, "arrays.rotate_right", ParseArraysRotateRight'access );
  declareProcedure( arrays_shift_left_t, "arrays.shift_left", ParseArraysShiftLeft'access );
  declareProcedure( arrays_shift_right_t, "arrays.shift_right", ParseArraysShiftRight'access );
  declareProcedure( arrays_to_array_t, "arrays.to_array", ParseArraysToArray'access );
  declareProcedure( arrays_to_json_t, "arrays.to_json", ParseArraysToJSON'access );
  declareNamespaceClosed( "arrays" );
end StartupArrays;

procedure ShutdownArrays is
begin
  null;
end ShutdownArrays;

end parser_arrays;
