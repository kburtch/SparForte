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

with gnat.bubble_sort_a,
     gnat.heap_sort_a,
     ada.numerics.float_random,
     bush_os,
     string_util,
     user_io,
     world,
     scanner,
     scanner_arrays,
     parser,
     parser_aux;
use  bush_os,
     string_util,
     user_io,
     world,
     scanner,
     scanner_arrays,
     parser,
     parser_aux;

with ada.text_io;
use  ada.text_io;

package body parser_arrays is


---------------------------------------------------------
-- PARSE THE ARRAYS PACKAGE
---------------------------------------------------------

procedure ParseArraysFirst( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: arrays.first( arraytypeorvar );
  -- Source: arraytypeorvar'first
  var_id   : identifier;
  array_id : arrayID;
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
  elsif not (class_ok( var_id, otherClass, constClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     kind := indexType( array_id );
     f := to_unbounded_string( long_integer'image( firstBound( array_id ) ) );
  elsif syntax_check then
     kind := universal_t; -- type is unknown during syntax check
  else                     -- exiting block, etc. still need type info...
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     kind := indexType( array_id );
  end if;
end ParseArraysFirst;

procedure ParseArraysLast( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: arrays.last( arraytypeorvar );
  -- Source: arraytypeorvar'last
  var_id   : identifier;
  array_id : arrayID;
begin
  expect( arrays_last_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, otherClass, constClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     kind := indexType( array_id );
     f := to_unbounded_string( long_integer'image( lastBound( array_id ) ) );
  elsif syntax_check then
     kind := universal_t; -- type is unknown during syntax check
  else                     -- exiting block, etc. still need type info...
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     kind := indexType( array_id );
  end if;
end ParseArraysLast;

procedure ParseArraysLength( f : out unbounded_string ) is
  -- Syntax: arrays.length( arraytypeorvar );
  -- Source: arraytypeorvar'length
  var_id   : identifier;
  array_id : arrayID;
begin
  expect( arrays_length_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, otherClass, constClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     f := to_unbounded_string( long_integer'image( lastBound( array_id ) - firstBound( array_id ) + 1 ) );
  end if;
end ParseArraysLength;

-------------------------------------------------------------------------------
-- Array Sorts
-------------------------------------------------------------------------------

-- Stuff for Sorting with GNAT packages
-- I'm concerned that GNAT uses integers for indexes in sorts...may not
-- work with long_integer arrays on some platforms...assuming peole would
-- have such friggin' huge arrays in the first place.

arrayIdBeingSorted     : arrayID;
offsetArrayBeingSorted : long_integer;
ZeroElement            : unbounded_string;

procedure moveElement( From, To : natural ) is
  data : unbounded_string;
begin
  if From = 0 then
     data := ZeroElement;
  else
     data := arrayElement( arrayIdBeingSorted, long_integer(From)+offsetArrayBeingSorted);
  end if;
  if To = 0 then
     ZeroElement := data;
  else
     assignElement( arrayIdBeingSorted, long_integer(To)+offsetArrayBeingSorted, data );
   end if;
end moveElement;

function Lt_string( Op1, Op2 : natural ) return boolean is
  data1, data2 : unbounded_string;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     data1 := arrayElement( arrayIdBeingSorted, long_integer( Op1 )+offsetArrayBeingSorted);
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     data2 := arrayElement( arrayIdBeingSorted, long_integer( Op2 )+offsetArrayBeingSorted);
  end if;
  return data1 < data2;
end Lt_string;

function Lt_string_descending( Op1, Op2 : natural ) return boolean is
  data1, data2 : unbounded_string;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     data1 := arrayElement( arrayIdBeingSorted, long_integer( Op1 )+offsetArrayBeingSorted);
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     data2 := arrayElement( arrayIdBeingSorted, long_integer( Op2 )+offsetArrayBeingSorted);
  end if;
  return data1 > data2;
end Lt_string_descending;

function Lt_numeric( Op1, Op2 : natural ) return boolean is
  data1, data2 : unbounded_string;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     data1 := arrayElement( arrayIdBeingSorted, long_integer( Op1 )+offsetArrayBeingSorted);
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     data2 := arrayElement( arrayIdBeingSorted, long_integer( Op2 )+offsetArrayBeingSorted);
  end if;
  return to_numeric( data1 ) < to_numeric( data2 );
end Lt_numeric;

function Lt_numeric_descending( Op1, Op2 : natural ) return boolean is
  data1, data2 : unbounded_string;
begin
  if Op1 = 0 then
     data1 := ZeroElement;
  else
     data1 := arrayElement( arrayIdBeingSorted, long_integer( Op1 )+offsetArrayBeingSorted);
  end if;
  if Op2 = 0 then
     data2 := ZeroElement;
  else
     data2 := arrayElement( arrayIdBeingSorted, long_integer( Op2 )+offsetArrayBeingSorted);
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
  if not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( arrayIdBeingSorted );
     last  := lastBound( arrayIdBeingSorted );
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
  if not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( arrayIdBeingSorted );
     last  := lastBound( arrayIdBeingSorted );
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
  if not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( arrayIdBeingSorted );
     last  := lastBound( arrayIdBeingSorted );
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
  if not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( arrayIdBeingSorted );
     last  := lastBound( arrayIdBeingSorted );
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
  first, last : long_integer;
  newpos : natural;
  len    : long_integer;
  array_id : arrayID;
begin
  expect( arrays_shuffle_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  --if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
  --   var_id := getBaseType( var_id );
  --   if not identifiers( var_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
  if not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( array_id );
     last  := lastBound( array_id );
     len   := last-first+1;
     for i in 1..len loop
       newpos := natural( 1.0 + long_float'truncation( long_float( len ) *
             long_float( Ada.Numerics.Float_Random.Random( random_generator
) ) ) );

         moveElement( integer(i), 0 );
         moveElement( integer(newpos), integer(i) );
         moveElement( 0, integer(newpos) );
     end loop;
  end if;
end ParseArraysShuffle;

procedure ParseArraysFlip is
  var_id : identifier;
  first, last : long_integer;
  newpos : natural;
  len    : long_integer;
  array_id : arrayID;
begin
  expect( arrays_flip_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  --if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
  --   var_id := getBaseType( var_id );
  --   if not identifiers( var_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
  if not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( array_id );
     last  := lastBound( array_id );
     len   := last-first+1;
     for i in 1..len loop
         newpos := natural( len - i );
         moveElement( integer(i), 0 );
         moveElement( integer(newpos), integer(i) );
         moveElement( 0, integer(newpos) );
     end loop;
  end if;
end ParseArraysFlip;

procedure ParseArraysShiftRight is
  var_id : identifier;
  first, last : long_integer;
  len    : long_integer;
begin
  expect( arrays_shift_right_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  --if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
  --   var_id := getBaseType( var_id );
  --   if not identifiers( var_id ).list then
  --      err( "Array expected" );
  --   end if;
  --elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
  if not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( arrayIdBeingSorted );
     last  := lastBound( arrayIdBeingSorted );
     len   := last-first+1;
     offsetArrayBeingSorted := first-1;
     for i in reverse 1..len-1 loop
         moveElement( integer(i), integer(i+1) );
     end loop;
  end if;
end ParseArraysShiftRight;

procedure ParseArraysShiftLeft is
  var_id : identifier;
  first, last : long_integer;
  len    : long_integer;
begin
  expect( arrays_shift_left_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  --if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
  --   var_id := getBaseType( var_id );
  --   if not identifiers( var_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
  if not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( arrayIdBeingSorted );
     last  := lastBound( arrayIdBeingSorted );
     len   := last-first+1;
     offsetArrayBeingSorted := first-1;
     for i in 1..len-1 loop
         moveElement( integer(i+1), integer(i) );
     end loop;
  end if;
end ParseArraysShiftLeft;

procedure ParseArraysRotateRight is
  var_id : identifier;
  first, last : long_integer;
  len    : long_integer;
begin
  expect( arrays_rotate_right_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  --if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
  --   var_id := getBaseType( var_id );
  --   if not identifiers( var_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
  if not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( arrayIdBeingSorted );
     last  := lastBound( arrayIdBeingSorted );
     len   := last-first+1;
     offsetArrayBeingSorted := first-1;
     moveElement( integer( len ), 0 );
     for i in reverse 1..len-1 loop
         moveElement( integer(i), integer(i+1) );
     end loop;
     moveElement( 0, 1 );
  end if;
end ParseArraysRotateRight;

procedure ParseArraysRotateLeft is
  var_id : identifier;
  first, last : long_integer;
  len    : long_integer;
begin
  expect( arrays_rotate_left_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  --if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
  --   var_id := getBaseType( var_id );
  --   if not identifiers( var_id ).list then
  --      err( "Array or array type expected" );
  --   end if;
  --elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
  if not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     arrayIdBeingSorted := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( arrayIdBeingSorted );
     last  := lastBound( arrayIdBeingSorted );
     len   := last-first+1;
     offsetArrayBeingSorted := first-1;
     moveElement( 1, 0 );
     for i in 1..len-1 loop
         moveElement( integer(i+1), integer(i) );
     end loop;
     moveElement( 0, integer( len ) );
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
  if not (class_ok( target_var_id, otherClass ) and identifiers( target_var_id ).list) then
     err( "Array expected" );
  end if;
  expect( symbol_t, "," );
  ParseExpression( source_val, source_type );
  if baseTypesOK( source_type, json_string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- DoJsonToArray actually populates the array, so you don't use assign parameter
     -- assignParameter( target_var_id, jsonString );
     DoJsonToArray( target_var_id, source_val );
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
  if not (class_ok( source_var_id, otherClass ) and identifiers( source_var_id ).list) then
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
  declareFunction( arrays_first_t, "arrays.first" );
  declareFunction( arrays_last_t, "arrays.last" );
  declareFunction( arrays_length_t, "arrays.length" );
  declareProcedure( arrays_bubble_sort_t, "arrays.bubble_sort" );
  declareProcedure( arrays_bubble_sort_descending_t, "arrays.bubble_sort_descending" );
  declareProcedure( arrays_heap_sort_t, "arrays.heap_sort" );
  declareProcedure( arrays_heap_sort_descending_t, "arrays.heap_sort_descending" );
  declareProcedure( arrays_shuffle_t, "arrays.shuffle" );
  declareProcedure( arrays_flip_t, "arrays.flip" );
  declareProcedure( arrays_rotate_left_t, "arrays.rotate_left" );
  declareProcedure( arrays_rotate_right_t, "arrays.rotate_right" );
  declareProcedure( arrays_shift_left_t, "arrays.shift_left" );
  declareProcedure( arrays_shift_right_t, "arrays.shift_right" );
  declareProcedure( arrays_to_array_t, "arrays.to_array" );
  declareProcedure( arrays_to_json_t, "arrays.to_json" );
end StartupArrays;

procedure ShutdownArrays is
begin
  null;
end ShutdownArrays;

end parser_arrays;
