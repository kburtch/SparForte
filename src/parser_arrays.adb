------------------------------------------------------------------------------
-- Arrays Package Parser                                                    --
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
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------
--with ada.text_io; use ada.text_io;
with gnat.bubble_sort_a,
     gnat.heap_sort_a,
     gnat.source_info,
     ada.numerics.float_random,
     ada.strings.unbounded,
     world,
     pegasoft.strings,
     performance_monitoring,
     symbol_table,
     message_strings,
     scanner,
     scanner.communications,
     parser_sidefx,
     parser,
     parser_params;
use  ada.strings.unbounded,
     world,
     pegasoft,
     pegasoft.strings,
     symbol_table,
     message_strings,
     scanner,
     scanner.communications,
     performance_monitoring,
     parser_sidefx,
     parser,
     parser_params;

--with ada.text_io; use ada.text_io;

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
--
-- PARSE THE ARRAYS PACKAGE
--
---------------------------------------------------------

procedure expectArrayOrArrayType( sub_id, id : identifier ) is
begin
  if identifiers( sub_id ).class = funcClass then
     err( context => sub_id,
          subject => id,
          subjectType => identifiers( id ).kind,
          reason => +"was used but the function expects",
          obstructorNotes => pl( "an " ) & em( "array or array type" ),
          seeAlso => docPkgArrays
     );
  else
     err( context => sub_id,
          subject => id,
          subjectType => identifiers( id ).kind,
          reason => +"was used but the procedure expects",
          obstructorNotes => pl( "an " ) & em( "array or array type" ),
          seeAlso => docPkgArrays
     );
  end if;
end expectArrayOrArrayType;

procedure expectArray( sub_id, id : identifier ) is
begin
  if identifiers( sub_id ).class = funcClass then
     err( context => sub_id,
          subject => id,
          subjectType => identifiers( id ).kind,
          reason => +"was used but the function expects",
          obstructorNotes => pl( "an " ) & em( "array" ),
          seeAlso => docPkgArrays
     );
  else
     err( context => sub_id,
          subject => id,
          subjectType => identifiers( id ).kind,
          reason => +"was used but the procedure expects",
          obstructorNotes => pl( "an " ) & em( "array" ),
          seeAlso => docPkgArrays
     );
  end if;
end expectArray;

procedure expectWritableItems( subprogramId, var_id : identifier ) is
begin
  if identifiers( identifiers( var_id ).kind ).usage = constantUsage then
     err( context => subprogramId,
          subject => var_id,
          subjectType => identifiers( var_id ).kind,
          reason => +"cannot be rearranged or changed because its constant elements",
          obstructorNotes => em( "cannot be written" )
     );
  elsif identifiers( identifiers( var_id ).kind ).usage = limitedUsage then
     err( context => subprogramId,
          subject => var_id,
          subjectType => identifiers( var_id ).kind,
          reason => +"cannot be rearranged or changed because its limited elements",
          obstructorNotes => em( "cannot be read or written" )
     );
  end if;
end expectWritableItems;

procedure expectSortableArray( sub_id, id : identifier ) is
begin
   err( context => sub_id,
        subject => id,
        subjectType => identifiers( id ).kind,
        reason => +"cannot be sorted because it must have a",
        obstructorNotes => +"a string, numeric or enumerated element type",
        seeAlso => docPkgArrays
   );
end expectSortableArray;


------------------------------------------------------------------------------
--  ERR STORAGE
--
------------------------------------------------------------------------------

procedure err_storage is
begin
  err( +"storage_error raised" );
end err_storage;


-----------------------------------------------------------------------------
--  PARSE ARRAYS FIRST                                    (built-in function)
--
-- AdaScript Syntax: arrays.first( arraytypeorvar );
--       Ada Target: arraytypeorvar'first
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.first
-----------------------------------------------------------------------------

procedure ParseArraysFirst( result : out storage; kind : out identifier ) is
  var_id   : identifier;
  subprogramId : constant identifier := arrays_first_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or
     identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        expectArrayOrArrayType( subprogramId, var_id );
     end if;
  elsif not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArrayOrArrayType( subprogramId, var_id );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result.unitMetaLabel := noMetaLabel;
     result.policyMetaLabels := noMetaLabels;
     if identifiers( var_id ).class = subClass or identifiers( var_id ).class = typeClass then
        result.value := to_unbounded_string( long_integer'image( identifiers( var_id ).firstBound ) );
     else
        begin
           result.value := to_unbounded_string( long_integer'image( identifiers( var_id ).aStorage'first ) );
        exception when CONSTRAINT_ERROR =>
           err(
               contextNotes => pl( "At " & gnat.source_info.source_location &
                   " while in " ) & em( to_string( identifiers( subprogramId ).name ) ),
               subjectNotes => subjectInterpreter,
               reason => +"had an internal error because",
               obstructorNotes => pl( "the index is out of range " &
                   identifiers( var_id ).aStorage'first'img & " .. " &
                   identifiers( var_id ).aStorage'last'img )
           );
        when STORAGE_ERROR =>
           err_storage;
        end;
     end if;
     kind := identifiers( var_id ).genKind;
  elsif syntax_check then
     kind := universal_t; -- type is unknown during syntax check
  else                     -- exiting block, etc. still need type info...
     kind := identifiers( var_id ).genKind;
  end if;
end ParseArraysFirst;


-----------------------------------------------------------------------------
--  PARSE ARRAYS LAST                                     (built-in function)
--
-- AdaScript Syntax: arrays.last( arraytypeorvar );
--       Ada Target: arraytypeorvar'last
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.last
-----------------------------------------------------------------------------

procedure ParseArraysLast( result : out storage; kind : out identifier ) is
  var_id   : identifier;
  --array_id : arrayID;
  subprogramId : constant identifier := arrays_last_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or
     identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        expectArrayOrArrayType( subprogramId, var_id );
     end if;
  elsif not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArrayOrArrayType( subprogramId, var_id );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result.unitMetaLabel := noMetaLabel;
     result.policyMetaLabels := noMetaLabels;
     if identifiers( var_id ).class = subClass or identifiers( var_id ).class = typeClass then
        result.value := to_unbounded_string( long_integer'image( identifiers( var_id ).lastBound ) );
     else
        begin
           result.value := to_unbounded_string( long_integer'image( identifiers( var_id ).aStorage'last ) );
        exception when CONSTRAINT_ERROR =>
           err(
               contextNotes => pl( "At " & gnat.source_info.source_location &
                   " while in " ) & em( to_string( identifiers( subprogramId ).name ) ),
               subjectNotes => subjectInterpreter,
               reason => +"had an internal error because",
               obstructorNotes => pl( "the index is out of range " &
                   identifiers( var_id ).aStorage'first'img & " .. " &
                   identifiers( var_id ).aStorage'last'img )
           );
        when STORAGE_ERROR =>
           err_storage;
        end;
     end if;
     kind := identifiers( var_id ).genKind;
  elsif syntax_check then
     kind := universal_t; -- type is unknown during syntax check
  else                     -- exiting block, etc. still need type info...
     kind := identifiers( var_id ).genKind;
  end if;
end ParseArraysLast;


-----------------------------------------------------------------------------
--  PARSE ARRAYS LENGTH                                   (built-in function)
--
-- AdaScript Syntax: arrays.length( arraytypeorvar );
--       Ada Target: arraytypeorvar'length
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.length
-----------------------------------------------------------------------------

procedure ParseArraysLength( result : out storage; kind : out identifier ) is
  var_id   : identifier;
  subprogramId : constant identifier := arrays_length_t;
begin
  kind := natural_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        expectArrayOrArrayType( subprogramId, var_id );
     end if;
  elsif not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArrayOrArrayType( subprogramId, var_id );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result.unitMetaLabel := noMetaLabel;
     result.policyMetaLabels := noMetaLabels;
     if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
        result.value := to_unbounded_string( long_integer'image( identifiers( var_id ).lastBound - identifiers( var_id ).firstBound + 1 ) );
     else
        result.value := to_unbounded_string( long_integer'image( identifiers( var_id ).aStorage'length ) );
     end if;
  end if;
end ParseArraysLength;


-------------------------------------------------------------------------------
--
-- Array Sorts
--
-- Stuff for Sorting with GNAT packages
-- I'm concerned that GNAT uses integers for indexes in sorts...may not
-- work with long_integer arrays on some platforms...assuming peole would
-- have such friggin' huge arrays in the first place.
--
-------------------------------------------------------------------------------

offsetArrayBeingSorted : long_integer;
ZeroElement            : storage;
arrayBeingSortedId     : identifier;


-----------------------------------------------------------------------------
--  MOVE ELEMENT
--
-- Move an element in a SparForte array.
-----------------------------------------------------------------------------

procedure moveElement( From, To : natural ) is
  data : storage;
begin
  if From = 0 then
     data := ZeroElement;
  else
     data := identifiers( arrayBeingSortedId ).aStorage( long_integer(From)+offsetArrayBeingSorted );
  end if;
  if To = 0 then
     ZeroElement := data;
  else
     identifiers( arrayBeingSortedId ).aStorage( long_integer(To)+offsetArrayBeingSorted ) := data;
   end if;
end moveElement;


-----------------------------------------------------------------------------
--  LT STRING
--
-- String less than, ascending.
-----------------------------------------------------------------------------

function Lt_string( Op1, Op2 : natural ) return boolean is
  data1, data2 : storage;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     data1 := identifiers( arrayBeingSortedId ).aStorage( long_integer( Op1 )+offsetArrayBeingSorted );
     --data1 := arrayElement( arrayIdBeingSorted, long_integer( Op1 )+offsetArrayBeingSorted);
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     --data2 := arrayElement( arrayIdBeingSorted, long_integer( Op2 )+offsetArrayBeingSorted);
     data2 := identifiers( arrayBeingSortedId ).aStorage( long_integer( Op2 )+offsetArrayBeingSorted );
  end if;
  return data1.value < data2.value;
end Lt_string;


-----------------------------------------------------------------------------
--  LT STRING DESCENDING
--
-- String less than, descending.
-----------------------------------------------------------------------------

function Lt_string_descending( Op1, Op2 : natural ) return boolean is
  data1, data2 : storage;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     -- data1 := arrayElement( arrayIdBeingSorted, long_integer( Op1 )+offsetArrayBeingSorted);
     data1 := identifiers( arrayBeingSortedId ).aStorage( long_integer( Op1 )+offsetArrayBeingSorted );
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     -- data2 := arrayElement( arrayIdBeingSorted, long_integer( Op2 )+offsetArrayBeingSorted);
     data2 := identifiers( arrayBeingSortedId ).aStorage( long_integer( Op2 )+offsetArrayBeingSorted );
  end if;
  return data1.value > data2.value;
end Lt_string_descending;


-----------------------------------------------------------------------------
--  LT NUMERIC
--
-- Numeric less than, ascending.
-----------------------------------------------------------------------------

function Lt_numeric( Op1, Op2 : natural ) return boolean is
  data1, data2 : storage;
  result : boolean := false;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     data1 := identifiers( arrayBeingSortedId ).aStorage( long_integer( Op1 )+offsetArrayBeingSorted );
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     data2 := identifiers( arrayBeingSortedId ).aStorage( long_integer( Op2 )+offsetArrayBeingSorted );
  end if;
  begin
    result := to_numeric( data1.value ) < to_numeric( data2.value );
  exception when others =>
    err( contextNotes => +"comparing values during the sort",
         subject => arrayBeingSortedId,
         subjectType => identifiers( arrayBeingSortedId ).kind,
         reason => pl( "cannot be sorted because it has an " ) & em( "undefined value" ) &
                   pl(" at index" & Op1'img & " or" & Op2'img),
         obstructorNotes => nullMessageStrings
    );
  end;
  return result;
end Lt_numeric;


-----------------------------------------------------------------------------
--  LT NUMERIC DESCENDING
--
-- Numeric less than, descending.
-----------------------------------------------------------------------------

function Lt_numeric_descending( Op1, Op2 : natural ) return boolean is
  data1, data2 : storage;
  result : boolean := false;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     data1 := identifiers( arrayBeingSortedId ).aStorage( long_integer( Op1 )+offsetArrayBeingSorted );
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     data2 := identifiers( arrayBeingSortedId ).aStorage( long_integer( Op2 )+offsetArrayBeingSorted );
  end if;
  begin
    result := to_numeric( data1.value ) > to_numeric( data2.value );
  exception when others =>
    err( contextNotes => +"comparing values during the sort",
         subject => arrayBeingSortedId,
         subjectType => identifiers( arrayBeingSortedId ).kind,
         reason => pl( "cannot be sorted because it has an " ) & em( "undefined value" ) &
                   pl(" at index" & Op1'img & " or" & Op2'img),
         obstructorNotes => nullMessageStrings
    );
  end;
  return result;
end Lt_numeric_descending;


-----------------------------------------------------------------------------
--
-- The Sorts
--
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  PARSE ARRAYS BUBBLE SORT                             (built-in procedure)
--
-- AdaScript Syntax: arrays.bubble_sort( a )
--       Ada Target: GNAT.Bubble_Sort_A
--   GNAT Spec File: g-busora.ads
--   SparForte Docs: doc/pkg_arrays.html#arrays.bubble_sort
-----------------------------------------------------------------------------

procedure ParseArraysBubbleSort is
  var_id : identifier;
  first, last : long_integer;
  kind   : identifier;
  subprogramId : constant identifier := arrays_bubble_sort_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArray( subprogramId, var_id );
  end if;
  expectWritableItems( subprogramId, var_id );
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check and not error_found then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleDataFlowWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     arrayBeingSortedId := var_id;
     first := identifiers( var_id ).aStorage'first;
     last  := identifiers( var_id ).aStorage'last;
     -- do not sort an empty array
     if first /= 1 or last /= 0 then
        offsetArrayBeingSorted := first-1;
        kind := getUniType( identifiers( var_id ).kind );
        if kind = uni_string_t or kind = universal_t then
           GNAT.Bubble_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_string'access );
        elsif kind = uni_numeric_t or kind = root_enumerated_t then
           GNAT.Bubble_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_numeric'access );
        else
           expectSortableArray( subprogramId, var_id );
        end if;
     end if;
  end if;
end ParseArraysBubbleSort;


-----------------------------------------------------------------------------
--  PARSE ARRAYS BUBBLE SORT DESCENDING                  (built-in procedure)
--
-- AdaScript Syntax: arrays.bubble_sort_descending( a )
--       Ada Target: GNAT.Bubble_Sort_A
--   GNAT Spec File: g-busora.ads
--   SparForte Docs: doc/pkg_arrays.html#arrays.bubble_sort_descending
-----------------------------------------------------------------------------

procedure ParseArraysBubbleSortDescending is
  var_id : identifier;
  first, last : long_integer;
  kind   : identifier;
  subprogramId : constant identifier := arrays_bubble_sort_descending_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArray( subprogramId, var_id );
  end if;
  expectWritableItems( subprogramId, var_id );
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check and not error_found then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleDataFlowWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     arrayBeingSortedId := var_id;
     first := identifiers( var_id ).aStorage'first;
     last  := identifiers( var_id ).aStorage'last;
     -- do not sort an empty array
     if first /= 1 or last /= 0 then
        offsetArrayBeingSorted := first-1;
        kind := getUniType( identifiers( var_id ).kind );
        if kind = uni_string_t or kind = universal_t then
           GNAT.Bubble_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_string_descending'access );
        elsif kind = uni_numeric_t or kind = root_enumerated_t then
           GNAT.Bubble_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_numeric_descending'access );
        else
           expectSortableArray( subprogramId, var_id );
        end if;
     end if;
  end if;
end ParseArraysBubbleSortDescending;


-----------------------------------------------------------------------------
--  PARSE ARRAYS HEAP SORT                               (built-in procedure)
--
-- AdaScript Syntax: arrays.heap_sort( a )
--       Ada Target: GNAT.Heap_Sort_A
--   GNAT Spec File: g-hesora.ads
--   SparForte Docs: doc/pkg_arrays.html#arrays.heap_sort
-----------------------------------------------------------------------------

procedure ParseArraysHeapSort is
  var_id : identifier;
  first, last : long_integer;
  kind   : identifier;
  subprogramId : constant identifier := arrays_heap_sort_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArray( subprogramId, var_id );
  end if;
  expectWritableItems( subprogramId, var_id );
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleDataFlowWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
    arrayBeingSortedId := var_id;
     first := identifiers( var_id ).aStorage'first;
     last  := identifiers( var_id ).aStorage'last;
     -- do not sort an empty array
     if first /= 1 or last /= 0 then
        offsetArrayBeingSorted := first-1;
        kind := getUniType( identifiers( var_id ).kind );
        if kind = uni_string_t or kind = universal_t then
           GNAT.Heap_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_string'access );
        elsif kind = uni_numeric_t or kind = root_enumerated_t then
           GNAT.Heap_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_numeric'access );
        else
           expectSortableArray( subprogramId, var_id );
        end if;
     end if;
  end if;
end ParseArraysHeapSort;


-----------------------------------------------------------------------------
--  PARSE ARRAYS HEAP SORT DESCENDING                    (built-in procedure)
--
-- AdaScript Syntax: arrays.heap_sort_descending( a )
--       Ada Target: GNAT.Heap_Sort_A
--   GNAT Spec File: g-hesora.ads
--   SparForte Docs: doc/pkg_arrays.html#arrays.heap_sort_descending
-- Heap sort the array in descending order, treating the elements as strings or
-- numbers depending on the element type.
-----------------------------------------------------------------------------

procedure ParseArraysHeapSortDescending is
  var_id : identifier;
  first, last : long_integer;
  kind   : identifier;
  subprogramId : constant identifier := arrays_heap_sort_descending_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArray( subprogramId, var_id );
  end if;
  expectWritableItems( subprogramId, var_id );
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleDataFlowWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     arrayBeingSortedId := var_id;
     first := identifiers( var_id ).aStorage'first;
     last  := identifiers( var_id ).aStorage'last;
     -- do not sort an empty array
     if first /= 1 or last /= 0 then
        offsetArrayBeingSorted := first-1;
        kind := getUniType( identifiers( var_id ).kind );
        if kind = uni_string_t or kind = universal_t then
           GNAT.Heap_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_string_descending'access );
        elsif kind = uni_numeric_t or kind = root_enumerated_t then
           GNAT.Heap_Sort_A.Sort( natural( last - first ) + 1, moveElement'access, lt_numeric_descending'access );
        else
           expectSortableArray( subprogramId, var_id );
        end if;
     end if;
  end if;
end ParseArraysHeapSortDescending;


-----------------------------------------------------------------------------
--  PARSE ARRAYS SHUFFLE                                 (built-in procedure)
--
-- AdaScript Syntax: arrays.shuffle( a )
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.shuffle
-----------------------------------------------------------------------------

procedure ParseArraysShuffle is
  var_id : identifier;
  newpos : long_integer;
  len    : long_integer;
  tmp : storage;
  subprogramId : constant identifier := arrays_shuffle_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArray( subprogramId, var_id );
  end if;
  expectWritableItems( subprogramId, var_id );
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleDataFlowWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     len   := identifiers( var_id ).aStorage'length;
     begin
        for i in identifiers( var_id ).aStorage'range loop
            -- unfortunately, ada random numbers are 0..1, so they can be too
            -- large if 1...repeat if that happens
            loop
                newpos := long_integer( long_float'truncation( long_float( len ) *
                    long_float( Ada.Numerics.Float_Random.Random( random_generator
) ) ) );
            newpos := newpos + identifiers( var_id ).aStorage'first;
            exit when newpos <= identifiers( var_id ).aStorage'last;
            end loop;
            tmp := identifiers( var_id ).aStorage( i );
            -- Newer version of Ada will raise an exception on a no-op
            -- assignment from something to itself.
            if i /= newpos then
               identifiers( var_id ).aStorage( i ) := identifiers( var_id ).aStorage( newpos );
            end if;
            identifiers( var_id ).aStorage( newpos ) := tmp;
        end loop;
     exception when CONSTRAINT_ERROR =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
                " while shuffling" ),
             subject => var_id,
             subjectType => identifiers( var_id ).kind,
             reason => pl( "an index out of range " &
                 identifiers( var_id ).aStorage'first'img & " .." &
                 identifiers( var_id ).aStorage'last'img &
                 " because" ),
             obstructorNotes => +"a constraint error was raised"
        );
     when STORAGE_ERROR =>
        err_storage;
     end;
  end if;
end ParseArraysShuffle;


-----------------------------------------------------------------------------
--  PARSE ARRAYS FLIP                                    (built-in procedure)
--
-- AdaScript Syntax: arrays.flip( a )
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.flip
-----------------------------------------------------------------------------

procedure ParseArraysFlip is
  var_id : identifier;
  first, last, len : long_integer;
  oldpos : long_integer;
  newpos : long_integer;
  -- array_id : arrayID;
  tmp    : storage;
  subprogramId : constant identifier := arrays_flip_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArray( subprogramId, var_id );
  end if;
  expectWritableItems( subprogramId, var_id );
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleDataFlowWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     first   := identifiers( var_id ).aStorage'first;
     last    := identifiers( var_id ).aStorage'last;
     len     := identifiers( var_id ).aStorage'length;
     begin
        if last > first then
           for i in 0..len/2 loop
               oldpos := long_integer( first + i );
               newpos := long_integer( last - i );
               -- in newer versions of GCC Ada, assignment to oneself is an error
               if newpos /= oldpos then
                  tmp := identifiers( var_id ).aStorage( oldpos );
                  identifiers( var_id ).aStorage( oldpos ) := identifiers( var_id ).aStorage( newpos );
                  identifiers( var_id ).aStorage( newpos ) := tmp;
               end if;
           end loop;
        end if;
     exception when CONSTRAINT_ERROR =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
                " while flipping" ),
             subject => var_id,
             subjectType => identifiers( var_id ).kind,
             reason => pl( "an index out of range " &
                 identifiers( var_id ).aStorage'first'img & " .." &
                 identifiers( var_id ).aStorage'last'img &
                 " because" ),
             obstructorNotes => +"a constraint error was raised"
        );
     when STORAGE_ERROR =>
        err_storage;
     end;
  end if;
end ParseArraysFlip;


-----------------------------------------------------------------------------
--  PARSE ARRAYS SHIFT RIGHT                             (built-in procedure)
--
-- AdaScript Syntax: arrays.shift_right( a )
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.rotate_right
-----------------------------------------------------------------------------

procedure ParseArraysShiftRight is
  var_id : identifier;
  subprogramId : constant identifier := arrays_shift_right_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArray( subprogramId, var_id );
  end if;
  expectWritableItems( subprogramId, var_id );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleDataFlowWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     begin
        for i in reverse identifiers( var_id ).aStorage'first..identifiers( var_id ).aStorage'last-1 loop
            identifiers( var_id ).aStorage( i+1 ) := identifiers( var_id ).aStorage( i );
        end loop;
     exception when CONSTRAINT_ERROR =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
                " while shifting right" ),
             subject => var_id,
             subjectType => identifiers( var_id ).kind,
             reason => pl( "an index out of range " &
                 identifiers( var_id ).aStorage'first'img & " .." &
                 identifiers( var_id ).aStorage'last'img &
                 " because" ),
             obstructorNotes => +"a constraint error was raised"
        );
     when STORAGE_ERROR =>
        err_storage;
     end;
  end if;
end ParseArraysShiftRight;


-----------------------------------------------------------------------------
--  PARSE ARRAYS SHIFT LEFT                              (built-in procedure)
--
-- AdaScript Syntax: arrays.shift_left( a )
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.shift_left
-----------------------------------------------------------------------------

procedure ParseArraysShiftLeft is
  var_id : identifier;
  subprogramId : constant identifier := arrays_shift_left_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArrayOrArrayType( subprogramId, var_id );
  end if;
  expectWritableItems( subprogramId, var_id );
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleDataFlowWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     begin
        for i in identifiers( var_id ).aStorage'first..identifiers( var_id ).aStorage'last-1 loop
            identifiers( var_id ).aStorage( i ) := identifiers( var_id ).aStorage( i+1 );
        end loop;
     exception when CONSTRAINT_ERROR =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
                " while shifting left" ),
             subject => var_id,
             subjectType => identifiers( var_id ).kind,
             reason => pl( "an index out of range " &
                 identifiers( var_id ).aStorage'first'img & " .." &
                 identifiers( var_id ).aStorage'last'img &
                 " because" ),
             obstructorNotes => +"a constraint error was raised"
        );
     when STORAGE_ERROR =>
        err_storage;
     end;
  end if;
end ParseArraysShiftLeft;


-----------------------------------------------------------------------------
--  PARSE ARRAYS ROTATE RIGHT                            (built-in procedure)
--
-- AdaScript Syntax: arrays.rotate_right( a )
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.rotate_right
-----------------------------------------------------------------------------

procedure ParseArraysRotateRight is
  var_id : identifier;
  tmp    : storage;
  subprogramId : constant identifier := arrays_rotate_right_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArrayOrArrayType( subprogramId, var_id );
  end if;
  expectWritableItems( subprogramId, var_id );
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleDataFlowWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     begin
        if identifiers( var_id ).aStorage'length > 0 then
           tmp := identifiers( var_id ).aStorage( identifiers( var_id ).aStorage'last );
           for i in reverse identifiers( var_id ).aStorage'first..identifiers( var_id ).aStorage'last-1 loop
               identifiers( var_id ).aStorage( i+1 ) := identifiers( var_id ).aStorage( i );
           end loop;
           identifiers( var_id ).aStorage( identifiers( var_id ).aStorage'first ) := tmp;
        end if;
        --moveElement( 0, 1 );
     exception when CONSTRAINT_ERROR =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
                " while rotate right" ),
             subject => var_id,
             subjectType => identifiers( var_id ).kind,
             reason => pl( "an index out of range " &
                 identifiers( var_id ).aStorage'first'img & " .." &
                 identifiers( var_id ).aStorage'last'img &
                 " because" ),
             obstructorNotes => +"a constraint error was raised"
        );
     when STORAGE_ERROR =>
        err_storage;
     end;
  end if;
end ParseArraysRotateRight;


-----------------------------------------------------------------------------
--  PARSE ARRAYS ROTATE LEFT                             (built-in procedure)
--
-- AdaScript Syntax: arrays.rotate_left( a )
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.rotate_left
-----------------------------------------------------------------------------

procedure ParseArraysRotateLeft is
  var_id : identifier;
  tmp : storage;
  subprogramId : constant identifier := arrays_rotate_left_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     expectArrayOrArrayType( subprogramId, var_id );
  end if;
  expectWritableItems( subprogramId, var_id );
  expect( symbol_t, ")" );
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( var_id );
     checkDoubleDataFlowWrite( var_id );
     --checkDoubleGlobalWrite( var_id );
     identifiers( var_id ).writtenOn := perfStats.lineCnt;
     begin
        if identifiers( var_id ).aStorage'length > 0 then
           tmp := identifiers( var_id ).aStorage( identifiers( var_id ).aStorage'first );
           for i in identifiers( var_id ).aStorage'first..identifiers( var_id ).aStorage'last-1 loop
               identifiers( var_id ).aStorage( i ) := identifiers( var_id ).aStorage( i+1 );
           end loop;
           identifiers( var_id ).aStorage( identifiers( var_id ).aStorage'last ) := tmp;
        end if;
     exception when CONSTRAINT_ERROR =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
                " while rotate left" ),
             subject => var_id,
             subjectType => identifiers( var_id ).kind,
             reason => pl( "an index out of range " &
                 identifiers( var_id ).aStorage'first'img & " .." &
                 identifiers( var_id ).aStorage'last'img &
                 " because" ),
             obstructorNotes => +"a constraint error was raised"
        );
     when STORAGE_ERROR =>
        err_storage;
     end;
  end if;
end ParseArraysRotateLeft;


-----------------------------------------------------------------------------
--  PARSE ARRAYS TO ARRAY                                 (built-in function)
--
-- AdaScript Syntax: to_array( str_array | num_array, string )
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.to_array
-- Example: arrays.to_array( a, "[1,2]" )
--          arrays.to_array( d, "[" & ASCII.Quotation & "foo" &
--          ASCII.Quotation & "," & ASCII.Quotation & "bar" &
--          ASCII.Quotation & "]" );
-----------------------------------------------------------------------------

procedure ParseArraysToArray is
  target_var_id : identifier;
  source_st    : storage;
  source_type   : identifier;
  subprogramId : constant identifier := arrays_to_array_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( target_var_id );
  if not (class_ok( target_var_id, varClass ) and identifiers( target_var_id ).list) then
     expectArray( subprogramId, target_var_id );
  end if;
  expectWritableItems( subprogramId, target_var_id );
  expectParameterComma;
  ParseExpression( source_st, source_type );
  if baseTypesOK( source_type, json_string_t ) then
     expect( symbol_t, ")" );
  end if;
  -- mark as being altered for later tests
  if syntax_check then
     identifiers( target_var_id ).wasWritten := true;
  end if;
  if isExecutingCommand then
     checkExpressionFactorVolatilityOnWrite( target_var_id );
     checkDoubleDataFlowWrite( target_var_id );
     --checkDoubleGlobalWrite( target_var_id );
     identifiers( target_var_id ).writtenOn := perfStats.lineCnt;
     -- DoJsonToArray actually populates the array, so you don't use assign parameter
     if metaLabelOk( subprogramId, source_st ) then
       begin
         DoJsonToArray( target_var_id, source_st.value, noMetaLabels );
       exception when constraint_error =>
          err( contextNotes => pl( "At " & gnat.source_info.source_location ) &
                 contextAltText( source_st.value,"decoding the JSON string" ),
               subject => target_var_id,
               subjectType => identifiers( target_var_id ).kind,
               reason =>  +"the decoding failed because",
               obstructorNotes => +"a constraint error was raised"
          );
       when others =>
         err_exception_raised;
       end;
     end if;
  end if;
end ParseArraysToArray;


-----------------------------------------------------------------------------
--  PARSE ARRAYS TO JSON                                  (built-in function)
--
-- AdaScript Syntax: to_json( string, array )
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_arrays.html#arrays.to_json
-- Example: arrays.to_json( s, a )
-----------------------------------------------------------------------------

procedure ParseArraysToJSON is
  source_var_id : identifier;
  target_ref    : reference;
  jsonString    : unbounded_string;
  subprogramId : constant identifier := arrays_to_json_t;
begin
  expect( subprogramId );
  ParseFirstOutParameter( arrays_to_json_t, target_ref, json_string_t );
  expectParameterComma( arrays_to_json_t );
  ParseIdentifier( source_var_id );
  if not (class_ok( source_var_id, varClass ) and identifiers( source_var_id ).list) then
     expectArray( subprogramId, source_var_id );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     DoArrayToJson( jsonString, source_var_id );
     assignParameter( target_ref, storage'(jsonString , noMetaLabel, noMetaLabels ) );
  end if;
end ParseArraysToJSON;

-------------------------------------------------------------------------------
--
-- Housekeeping
--
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
