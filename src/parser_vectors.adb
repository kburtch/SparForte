------------------------------------------------------------------------------
-- Singly Linked Lists Package Parser                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with text_io;use text_io;
with pegasoft.strings;

with
    Ada.Containers,
    ada.strings.unbounded,
    pegasoft.strings,
    pegasoft.user_io,
    pegasoft.vectors,
    world,
    scanner.communications,
    scanner_res,
    scanner_restypes,
    parser,
    parser_params,
    parser_containers;
use
    ada.strings.unbounded,
    pegasoft.strings,
    pegasoft.user_io,
    pegasoft.vectors,
    world,
    scanner,
    scanner.communications,
    scanner_res,
    scanner_restypes,
    parser,
    parser_params,
    parser_containers;

package body parser_vectors is

------------------------------------------------------------------------------
--
-- Vectors package identifiers
--
------------------------------------------------------------------------------

vectors_clear_t         : identifier;
vectors_to_vector_t     : identifier;
vectors_capacity_t      : identifier;
vectors_reserve_capacity_t : identifier;
vectors_length_t        : identifier;
vectors_set_length_t    : identifier;
vectors_is_empty_t      : identifier;
vectors_append_vector_t : identifier;
vectors_append_elements_t : identifier;
vectors_prepend_elements_t : identifier;
vectors_prepend_t       : identifier;
vectors_first_index_t   : identifier;
vectors_last_index_t    : identifier;
vectors_element_t       : identifier;
vectors_first_element_t : identifier;
vectors_last_element_t  : identifier;
vectors_delete_first_t  : identifier;
vectors_delete_last_t   : identifier;
vectors_contains_t      : identifier;
vectors_move_t          : identifier;
vectors_assign_t          : identifier;
vectors_reverse_elements_t : identifier;
vectors_flip_t          : identifier;
vectors_first_t         : identifier;
vectors_last_t          : identifier;
vectors_next_t          : identifier;
vectors_previous_t      : identifier;
vectors_delete_t        : identifier;
vectors_has_element_t   : identifier;
vectors_equal_t         : identifier;
vectors_append_t        : identifier;
vectors_insert_t        : identifier;
vectors_insert_vector_t : identifier;
vectors_insert_before_t : identifier;
vectors_insert_before_and_mark_t : identifier;
vectors_insert_vector_and_mark_t : identifier;
vectors_insert_space_t  : identifier;
vectors_swap_t          : identifier;
vectors_find_t          : identifier;
vectors_reverse_find_t  : identifier;
vectors_find_index_t    : identifier;
vectors_reverse_find_index_t : identifier;
vectors_increment_t     : identifier;
vectors_decrement_t     : identifier;


------------------------------------------------------------------------------
--
-- Utilities
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  ERR STORAGE
--
------------------------------------------------------------------------------

procedure err_storage is
begin
  err( "storage_error raised" );
end err_storage;


------------------------------------------------------------------------------
--  ERR EMPTY
--
------------------------------------------------------------------------------

procedure err_empty( subprogram : identifier; vectorId : identifier) is
begin
  err( context => subprogram,
       subject => vectorId,
       reason =>"is",
       obstructorNotes => "empty" );
end err_empty;


------------------------------------------------------------------------------
--  ERR INDEX
--
------------------------------------------------------------------------------

procedure err_index( subprogram : identifier; idxExpr : unbounded_string ) is
begin
  err( context => subprogram,
       subjectNotes => "the index position" & toProtectedValue( idxExpr ),
       reason => "is",
       obstructorNotes => "not in the vector"
  );
end err_index;

procedure err_index( subprogram : identifier; idxExpr1, idxExpr2 : unbounded_string ) is
begin
  err( context => subprogram,
       subjectNotes => "the index position" & toProtectedValue( idxExpr1 ) &
                  " or " & toProtectedValue( idxExpr2 ),
       reason => "is",
       obstructorNotes => "not in the vector"
  );
end err_index;

--procedure err_index( idx : vector_index ) is
--begin
--  err( "index value" & toProtectedValue( to_unbounded_string( idx'img ) ) & " is out of range" );
--end err_index;


------------------------------------------------------------------------------
--  ERR COUNT
--
------------------------------------------------------------------------------

procedure err_count( subprogram : identifier; cntExpr : unbounded_string; cntType : identifier ) is
begin
   err( context => subprogram,
        subjectNotes => "the count value of " & toSecureData( to_string( cntExpr ) ),
        subjectType  => cntType,
        reason => "is not valid for",
        obstructor => containers_count_type_t,
        remedy => "the value should be >= 0" );
end err_count;


------------------------------------------------------------------------------
--  ERR CURSOR MISMATCH
--
------------------------------------------------------------------------------

procedure err_cursor_mismatch is
begin
  err( "the cursor does not match this vector" );
end err_cursor_mismatch;


------------------------------------------------------------------------------
--  VECTOR ITEM INDICES OK
--
-- Check vectors/cursors for identical index types.  If I used genTypesOK,
-- it reports the types but not that it's related to the index.  Also, the
-- types must be identical to avoid mapping one index to another.
------------------------------------------------------------------------------

procedure vectorItemIndicesOk( subprogram, leftVectorItem, rightVectorItem : identifier ) is
begin
-- TODO types ok flag
  if identifiers( leftVectorItem ).genKind /= identifiers( rightVectorItem ).genKind then
     err(
       context => subprogram,
       subjectNotes => "the index of " & optional_yellow( to_string( identifiers( leftVectorItem ).name ) ),
       subjectType => identifiers( leftVectorItem ).genKind,
       reason => "should have the identical type as",
       obstructorNotes => "the index of " & optional_yellow( to_string( identifiers( rightVectorItem ).name ) ),
       obstructorType => identifiers( rightVectorItem ).genKind
    );
  end if;
end vectorItemIndicesOk;


------------------------------------------------------------------------------
--  PARSE NEXT OUT VECTOR CURSOR
--
-- If necessary, declare a out cursor parameter.  Attach the resource.
------------------------------------------------------------------------------

procedure ParseNextOutVectorCursor( subprogram : identifier; vectorId : identifier; cursRef : in out reference ) is
  resId : resHandleId;
begin
  expectParameterComma( subprogram );
  if identifiers( token ).kind = new_t then
     ParseOutParameter( cursRef, vectors_cursor_t );

     if not error_found then
        identifiers( cursRef.id ).genKind := identifiers( vectorId ).genKind;
        identifiers( cursRef.id ).genKind2 := identifiers( vectorId ).genKind2;
     end if;

     if isExecutingCommand then

        -- Attach the resource

        declareResource( resId, vector_string_list_cursor, getIdentifierBlock( cursRef.id ) );
        identifiers( cursRef.id ).svalue := to_unbounded_string( resId );
        identifiers( cursRef.id ).value := identifiers( cursRef.id ).svalue'access;
        identifiers( cursRef.id ).resource := true;
     end if;
  else
     ParseInOutInstantiatedParameter( cursRef.id , vectors_cursor_t );
     -- Check the type against the vector
     if not error_found then
        genTypesOk( identifiers( vectorId ).genKind, identifiers( cursRef.id ).genKind );
        genTypesOk( identifiers( vectorId ).genKind2, identifiers( cursRef.id ).genKind2 );
     end if;
  end if;
end ParseNextOutVectorCursor;


------------------------------------------------------------------------------
--  PARSE LAsT OUT VECTOR CURSOR
--
-- If necessary, declare a out cursor parameter.  Attach the resource.
------------------------------------------------------------------------------

procedure ParseLastOutVectorCursor( subprogram : identifier; vectorId : identifier; cursRef : in out reference ) is
  resId : resHandleId;
begin
  expectParameterComma( subprogram );
  if identifiers( token ).kind = new_t then
     ParseOutParameter( cursRef, vectors_cursor_t );

     if not error_found then
        identifiers( cursRef.id ).genKind := identifiers( vectorId ).genKind;
        identifiers( cursRef.id ).genKind2 := identifiers( vectorId ).genKind2;
     end if;

     if isExecutingCommand then

        -- Attach the resource

        declareResource( resId, vector_string_list_cursor, getIdentifierBlock( cursRef.id ) );
        identifiers( cursRef.id ).svalue := to_unbounded_string( resId );
        identifiers( cursRef.id ).value := identifiers( cursRef.id ).svalue'access;
        identifiers( cursRef.id ).resource := true;
     end if;
  else
     ParseInOutInstantiatedParameter( cursRef.id , vectors_cursor_t );
     -- Check the type against the vector
     if not error_found then
        genTypesOk( identifiers( vectorId ).genKind, identifiers( cursRef.id ).genKind );
        genTypesOk( identifiers( vectorId ).genKind2, identifiers( cursRef.id ).genKind2 );
     end if;
  end if;
  expectParameterClose( subprogram );
end ParseLastOutVectorCursor;


------------------------------------------------------------------------------
--  TO REAL VECTOR INDEX
--
-- The vector index is really a positive.  We have to map the SparForte index
-- to a positive to access the Ada vector.
------------------------------------------------------------------------------

function toRealVectorIndex( vectorId : identifier; UserIdx : integer ) return vector_index is
   kind  : identifier;
   baseKind : identifier;
   uniKind : identifier;
   convertedIdx : vector_index;
begin
   kind := identifiers( vectorId ).genKind;
   uniKind := getUniType( kind );

   -- the index type is either a numeric or an enumerated
   --
   -- for a numeric we need to know the subrange.  For a type with no
   -- subrange, assume it starts at integer'first.  Remember that the
   -- base type of natural will be integer, not natural.

   begin
      if uniKind = uni_numeric_t then
         baseKind := getBaseType( kind );
         if kind = positive_t or baseKind = positive_t then
            convertedIdx := vector_index( UserIdx-1 );
         elsif kind = natural_t or baseKind = natural_t then
            convertedIdx := vector_index( UserIdx );
         elsif kind = integer_t or baseKind = integer_t then
            convertedIdx := vector_index( UserIdx-integer'first );
         else
            err( "internal error: unsupported index type" );
         end if;
      elsif uniKind = uni_string_t then
         err( "internal error: string for a vector index");
      else
         -- for an enumerated, asuume it starts at zero
         convertedIdx := vector_index( UserIdx );
      end if;
   exception when constraint_error =>
      err( "internal error: index out-of-range" );
   end;
   return convertedIdx;
end toRealVectorIndex;


------------------------------------------------------------------------------
--  TO USER VECTOR INDEX
--
------------------------------------------------------------------------------

function toUserVectorIndex( vectorId : identifier; realIdx : vector_index ) return integer is
   kind  : identifier;
   baseKind : identifier;
   uniKind : identifier;
   convertedIdx : integer;
begin
   kind  := identifiers( vectorId ).genKind;
   unikind := getUniType( kind );
   begin
      -- the index type is either a numeric or an enumerated
      if unikind = uni_numeric_t then
         -- for a numeric we need to know the subrange.  For a type with no
         -- subrange, assume it starts at integer'first
         baseKind := getBaseType( kind );
         if kind = positive_t or baseKind = positive_t then
            convertedIdx := integer( realIdx )+1;
         elsif kind = natural_t or baseKind = natural_t then
            convertedIdx := integer( realIdx );
         elsif kind = integer_t or baseKind = integer_t then
            convertedIdx := integer(realIdx ) + integer'first;
         else
            err( "internal error: unsupported integer type" );
         end if;
      elsif uniKind = uni_string_t then
         err( "internal error: string for a vector index");
      else
         -- for an enumerated, asuume it starts at zero
         convertedIdx := integer( realIdx );
      end if;
   exception when constraint_error =>
      err( "internal error: index out-of-range" );
   end;
   return convertedIdx;
end toUserVectorIndex;

-- A special message for the insert function

function insertTypesOk( leftType, rightType : identifier ) return boolean is
  effectiveLeftType : identifier;
  effectiveRightType : identifier;
begin

  -- Basic checks: if the root types don't match, then the base types
  -- won't.  If either type is universal typeless, they automatically
  -- match.

  if not uniTypesOk( leftType, rightType ) then
     return false;
  end if;
  if leftType = universal_t or rightType = universal_t then
     return true;
  end if;
  effectiveLeftType := getBaseType( leftType );
  effectiveRightType := getBaseType( rightType );

  -- Universal type cases: Universal numeric or universal string will
  -- match depending on the root type of the second type.

  if effectiveLeftType = uni_numeric_t and then getUniType( rightType ) = uni_numeric_t then
     return true;
  end if;
  if effectiveLeftType = uni_string_t and then getUniType( rightType ) = uni_string_t then
     return true;
  end if;
  if effectiveRightType = uni_numeric_t and then getUniType( leftType ) = uni_numeric_t then
     return true;
  end if;
  if effectiveRightType = uni_string_t and then getUniType( leftType ) = uni_string_t then
     return true;
  end if;

  -- Otherwise, the types must be identical.

  if effectiveLeftType /= effectiveRightType then
     err( "vectors.cursor or list item expected" );
     return false;
  end if;
  return true;
end insertTypesOk;

------------------------------------------------------------------------------
--
-- Parser subprograms
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  CLEAR
--
-- Syntax: vectors.clear( v );
-- Ada:    vectors.clear( v );
------------------------------------------------------------------------------

procedure ParseVectorsClear is
  vectorId   : identifier;
  theVector  : resPtr;
begin
  expect( vectors_clear_t );
  ParseSingleInOutInstantiatedParameter( vectors_clear_t, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_String_Lists.Clear( theVector.vslVector );
     end;
  end if;
end ParseVectorsClear;


------------------------------------------------------------------------------
--  TO VECTOR
--
-- Syntax: vectors.to_vector( v, e, n );
-- Ada:    v := vectors.to_vector( [e, ] n );
------------------------------------------------------------------------------

procedure ParseVectorsToVector is
  vectorId   : identifier;
  theVector  : resPtr;
  itemExpr   : unbounded_string;
  itemType   : identifier;
  cntExpr    : unbounded_string;
  cntType    : identifier;
begin
  expect( vectors_to_vector_t );
  ParseFirstInOutInstantiatedParameter( vectors_to_vector_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_to_vector_t, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  ParseLastNumericParameter( vectors_to_vector_t, cntExpr, cntType, containers_count_type_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       theVector.vslVector := Vector_String_Lists.To_Vector( itemExpr, ada.containers.count_type'value( to_string( cntExpr ) ) );
     end;
  end if;
end ParseVectorsToVector;


------------------------------------------------------------------------------
--  CAPACITY
--
-- Syntax: c := capacity( v );
-- Ada:    c := capacity( v );
------------------------------------------------------------------------------

procedure ParseVectorsCapacity( result : out unbounded_string; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
begin
  kind := containers_count_type_t;
  expect( vectors_capacity_t );
  ParseSingleInOutInstantiatedParameter( vectors_capacity_t, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := to_unbounded_string( ada.containers.count_type'image( Vector_String_Lists.Capacity( theVector.vslVector ) ) );
     end;
  end if;
end ParseVectorsCapacity;


------------------------------------------------------------------------------
--  RESERVE CAPACITY
--
-- Syntax: reserve_capacity( v, c );
-- Ada:    reserve_capacity( v, c );
------------------------------------------------------------------------------

procedure ParseVectorsReserveCapacity is
  vectorId   : identifier;
  theVector  : resPtr;
  cntExpr    : unbounded_string;
  cntType    : identifier;
begin
  expect( vectors_reserve_capacity_t );
  ParseFirstInOutInstantiatedParameter( vectors_reserve_capacity_t, vectorId, vectors_vector_t );
  ParseLastNumericParameter( vectors_reserve_capacity_t, cntExpr, cntType, containers_count_type_t );
  if isExecutingCommand then
     declare
       cnt : ada.containers.count_type;
     begin
       cnt := ada.containers.count_type( to_numeric( cntExpr ) );
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_String_Lists.Reserve_Capacity( theVector.vslVector, cnt );
     exception when constraint_error =>
       -- e.g. user gave "-1" for what is a natural type
       err( context => vectors_reserve_capacity_t,
            subjectNotes => "the capacity count value of " & toSecureData( to_string( cntExpr ) ),
            subjectType  => cntType,
            reason => "is not valid for",
            obstructor => containers_count_type_t,
            remedy => "the value should be >= 0" );
     when storage_error =>
       err_storage;
     end;
  end if;
end ParseVectorsReserveCapacity;


------------------------------------------------------------------------------
--  LENGTH
--
-- Syntax: c := length( v );
-- Ada:    c := length( v );
------------------------------------------------------------------------------

procedure ParseVectorsLength( result : out unbounded_string; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
begin
  kind := containers_count_type_t;
  expect( vectors_length_t );
  ParseSingleInOutInstantiatedParameter( vectors_length_t, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := to_unbounded_string( ada.containers.count_type'image( Vector_String_Lists.Length( theVector.vslVector ) ) );
     end;
  end if;
end ParseVectorsLength;


------------------------------------------------------------------------------
--  SET LENGTH
--
-- Syntax: set_length( v, c );
-- Ada:    set_length( v, c );
------------------------------------------------------------------------------

procedure ParseVectorsSetLength is
  vectorId   : identifier;
  theVector  : resPtr;
  cntExpr    : unbounded_string;
  cntType    : identifier;
begin
  expect( vectors_set_length_t );
  ParseFirstInOutInstantiatedParameter( vectors_set_length_t, vectorId, vectors_vector_t );
  ParseLastNumericParameter( vectors_reserve_capacity_t, cntExpr, cntType, containers_count_type_t );
  if isExecutingCommand then
     declare
       cnt : ada.containers.count_type;
     begin
       cnt := ada.containers.count_type( to_numeric( cntExpr ) );
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_String_Lists.Set_Length( theVector.vslVector, cnt );
     exception when constraint_error =>
       -- e.g. user gave "-1" for what is a natural type
       err( context => vectors_set_length_t,
            subjectNotes => "the capacity count value of " & toSecureData( to_string( cntExpr ) ),
            subjectType  => cntType,
            reason => "is not valid for",
            obstructor => containers_count_type_t,
            remedy => "the value should be >= 0" );
     when storage_error =>
       err_storage;
     end;
  end if;
end ParseVectorsSetLength;


------------------------------------------------------------------------------
--  IS EMPTY
--
-- Syntax: b := is_empty( v );
-- Ada:    b := is_empty( v );
------------------------------------------------------------------------------

procedure ParseVectorsIsEmpty( result : out unbounded_string; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
begin
  kind := boolean_t;
  expect( vectors_is_empty_t );
  ParseSingleInOutInstantiatedParameter( vectors_is_empty_t, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := to_bush_boolean( Vector_String_Lists.Is_Empty( theVector.vslVector ) );
     end;
  end if;
end ParseVectorsIsEmpty;


------------------------------------------------------------------------------
--  APPEND ELEMENTS
--
-- Syntax: vectors.append_elements( v, s, [c] );
-- Ada:    vectors.append_elements( v, s, [c] );
------------------------------------------------------------------------------

procedure ParseVectorsAppendElements is
  vectorId  : identifier;
  theVector : resPtr;
  itemExpr  : unbounded_string;
  itemType  : identifier;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( vectors_append_elements_t );
  ParseFirstInOutInstantiatedParameter( vectors_append_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_append_t, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( vectors_append_t, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expectParameterClose( vectors_append_t );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Vector_String_Lists.Append( theVector.vslVector, itemExpr, cnt );
       else
          Vector_String_Lists.Append( theVector.vslVector, itemExpr );
       end if;
     exception when constraint_error =>
       err_count( vectors_append_elements_t, cntExpr, cntType );
     when storage_error =>
       err_storage;
     end;
  end if;
end ParseVectorsAppendElements;


------------------------------------------------------------------------------
--  PREPEND ELEMENTS
--
-- Syntax: vectors.prepend_elements( v, e, [n] );
-- Ada:    vectors.prepend_elements( v, e, [n] );
------------------------------------------------------------------------------

procedure ParseVectorsPrependElements is
  vectorId  : identifier;
  theVector : resPtr;
  itemExpr  : unbounded_string;
  itemType  : identifier;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( vectors_prepend_elements_t );
  ParseFirstInOutInstantiatedParameter( vectors_prepend_elements_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_prepend_elements_t, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( vectors_prepend_elements_t, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expectParameterClose( vectors_prepend_elements_t );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Vector_String_Lists.Prepend( theVector.vslVector, itemExpr, cnt );
       else
          Vector_String_Lists.Prepend( theVector.vslVector, itemExpr );
       end if;
     exception when constraint_error =>
       err_count( vectors_prepend_elements_t, cntExpr, cntType );
     when storage_error =>
       err_storage;
     end;
  end if;
end ParseVectorsPrependElements;


------------------------------------------------------------------------------
--  APPEND
--
-- Syntax: vectors.append( v, i, s );
-- Ada:    vectors.append( v, i, s );
------------------------------------------------------------------------------
-- TODO: support cursors
-- TODO: subject defaults to current token?

procedure ParseVectorsAppend is
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : unbounded_string;
  idxType   : identifier;
  strExpr   : unbounded_string;
  strType   : identifier;
begin
  expectAdaScript( subject => vectors_append_t, remedy => "use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( vectors_append_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_append_t, idxExpr, idxType, identifiers( vectorId ).genKind );
  ParseLastStringParameter( vectors_append_t, strExpr, strType, identifiers( vectorId ).genKind2 );
  if isExecutingCommand then
     declare
       idx : vector_index;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       idx := toRealVectorIndex( vectorId, integer( to_numeric( idxExpr ) ) );
       Append( theVector.vslVector, idx, strExpr );
     exception when constraint_error =>
       err( "append index is wrong type" );
     when storage_error =>
       err_storage;
     end;
  end if;
end ParseVectorsAppend;


------------------------------------------------------------------------------
--  PREPEND
--
-- Syntax: vectors.prepend( v, e, s );
-- Ada:    vectors.prepend( v, e, s );
------------------------------------------------------------------------------

procedure ParseVectorsPrepend is
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : unbounded_string;
  idxType   : identifier;
  strExpr   : unbounded_string;
  strType   : identifier;
begin
  expectAdaScript( subject => vectors_prepend_t, remedy => "use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( vectors_prepend_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_prepend_t, idxExpr, idxType, identifiers( vectorId ).genKind );
  ParseLastStringParameter( vectors_prepend_t, strExpr, strType, identifiers( vectorId ).genKind2 );
  if isExecutingCommand then
     declare
       idx : vector_index;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       idx := toRealVectorIndex( vectorId, integer( to_numeric( idxExpr ) ) );
       Prepend( theVector.vslVector, idx, strExpr );
     exception when constraint_error =>
       err( "prepend count must be a natural integer" );
     when storage_error =>
       err_storage;
     end;
  end if;
end ParseVectorsPrepend;


------------------------------------------------------------------------------
--  FIRST INDEX
--
-- Syntax: n := vectors.first_index( v );
-- Ada:    n := vectors.first_index( v );
------------------------------------------------------------------------------

procedure ParseVectorsFirstIndex( result : out unbounded_string; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
  userIdx    : vector_index;
begin
  expect( vectors_first_index_t );
  ParseSingleInOutInstantiatedParameter( vectors_first_index_t, vectorId, vectors_vector_t );
  kind := identifiers( vectorId ).genKind;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       userIdx := Vector_String_Lists.First_Index( theVector.vslVector );
       result := to_unbounded_string( integer'image( toUserVectorIndex( vectorId, userIdx ) ) );
     end;
  end if;
end ParseVectorsFirstIndex;


------------------------------------------------------------------------------
--  LAST INDEX
--
-- Syntax: i := vectors.last_index( v );
-- Ada:    i := vectors.last_index( v );
------------------------------------------------------------------------------

procedure ParseVectorsLastIndex( result : out unbounded_string; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
  userIdx    : vector_index;
begin
  expect( vectors_last_index_t );
  ParseSingleInOutInstantiatedParameter( vectors_last_index_t, vectorId, vectors_vector_t );
  kind := identifiers( vectorId ).genKind;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
--put_line( "last_index: vector = " & to_string( identifiers( vectorId ).name ) );
       userIdx := Vector_String_Lists.Last_Index( theVector.vslVector );
-- put_line( "last_index: userIdx = " & userIdx'img );
       result := to_unbounded_string( integer'image( toUserVectorIndex( vectorId, userIdx ) ) );
-- put_line( "last_index: result = " & to_string( result ) ); -- DEBUG
     end;
  end if;
end ParseVectorsLastIndex;


------------------------------------------------------------------------------
--  ELEMENT
--
-- Syntax: e := vectors.element( c ) | ( v, i )
-- Ada:    e := vectors.element( c ) | ( v, i )
------------------------------------------------------------------------------

procedure ParseVectorsElement( result : out unbounded_string; kind : out identifier ) is
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : unbounded_string;
  idxType   : identifier;
  cursorId  : identifier := eof_t;
  theCursor : resPtr;
begin
  expect( vectors_element_t );
  -- A cursor is a single identifier.  An index is an expression.
  expectParameterOpen( vectors_element_t );
  if identifiers( token ).kind = vectors_cursor_t then
     ParseInOutInstantiatedParameter( cursorId, vectors_cursor_t );
  elsif identifiers( token ).kind = vectors_vector_t then
     ParseInOutInstantiatedParameter( vectorId, vectors_vector_t );
     ParseNextGenItemParameter( vectors_element_t, idxExpr, idxType, identifiers( vectorId ).genKind );
  else
     err( context => vectors_element_t,
          subject => token,
          subjectType => identifiers( token ).kind,
          obstructorNotes => optional_yellow( "vectors.vector" ) & " or " &
             optional_yellow( "vectors.cursor" ),
          reason =>  "is not compatible with the expected types"
     );
  end if;
  expectParameterClose( vectors_element_t );

  -- The function result type depends on whether or not we have a vector or cursor
  -- The result type must always be set because it's used during syntax checking.
  -- If there was an error, the id might be invalid and genKind2 undefined.

  if not error_found then
     if cursorId /= eof_t then
          kind := identifiers( cursorId ).genKind2;
     else
          kind := identifiers( vectorId ).genKind2;
     end if;
  else
     kind := eof_t;
  end if;

  if isExecutingCommand then
     declare
       idx : vector_index;
     begin
       if cursorId /= eof_t then
         findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
         result := Vector_String_Lists.Element( theCursor.vslCursor );
       else
         findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
         --idx := vector_index( to_numeric( idxExpr ) );
         idx := toRealVectorIndex( vectorId, integer( to_numeric( idxExpr ) ) );
         result := Vector_String_Lists.Element( theVector.vslVector, idx );
       end if;
-- NOTE: Vector Lists stores internally a natural
     exception when constraint_error =>
       err_index( vectors_element_t, idxExpr );
     when storage_error =>
       err_storage;
     end;
  end if;
end ParseVectorsElement;


------------------------------------------------------------------------------
--  FIRST ELEMENT
--
-- Syntax: e := vectors.first_element( v );
-- Ada:    e := vectors.first_element( v );
------------------------------------------------------------------------------

procedure ParseVectorsFirstElement( result : out unbounded_string; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
begin
  expect( vectors_first_element_t );
  ParseSingleInOutInstantiatedParameter( vectors_first_element_t, vectorId, vectors_vector_t );
  kind := identifiers( vectorId ).genKind2;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := Vector_String_Lists.First_Element( theVector.vslVector );
     exception when constraint_error =>
       err_empty( vectors_first_element_t, vectorId );
     end;
  end if;
end ParseVectorsFirstElement;


------------------------------------------------------------------------------
--  LAST ELEMENT
--
-- Syntax: e := vectors.last_element( v );
-- Ada:    e := vectors.last_element( v );
------------------------------------------------------------------------------

procedure ParseVectorsLastElement( result : out unbounded_string; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
begin
  expect( vectors_last_element_t );
  ParseSingleInOutInstantiatedParameter( vectors_last_element_t, vectorId, vectors_vector_t );
  kind := identifiers( vectorId ).genKind2;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := Vector_String_Lists.Last_Element( theVector.vslVector );
     exception when constraint_error =>
       err_empty( vectors_last_element_t, vectorId );
     end;
  end if;
end ParseVectorsLastElement;


------------------------------------------------------------------------------
--  DELETE FIRST
--
-- Syntax: vectors.delete_first( v [,n] )
-- Ada:    vectors.delete_first( v [,n] )
------------------------------------------------------------------------------

procedure ParseVectorsDeleteFirst is
  vectorId  : identifier;
  theVector : resPtr;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( vectors_delete_first_t );
  ParseFirstInOutInstantiatedParameter( vectors_delete_first_t, vectorId, vectors_vector_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( vectors_delete_first_t, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expectParameterClose( vectors_delete_first_t );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Vector_String_Lists.Delete_First( theVector.vslVector, cnt );
       else
          Vector_String_Lists.Delete_First( theVector.vslVector );
       end if;
     exception when constraint_error =>
       err_count( vectors_delete_first_t, cntExpr, cntType );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsDeleteFirst;


------------------------------------------------------------------------------
--  DELETE LAST
--
-- Syntax: vectors.delete_last( v [,c] )
-- Ada:    vectors.delete_last( v [,c] )
------------------------------------------------------------------------------

procedure ParseVectorsDeleteLast is
  vectorId  : identifier;
  theVector : resPtr;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( vectors_delete_last_t );
  ParseFirstInOutInstantiatedParameter( vectors_delete_last_t, vectorId, vectors_vector_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( vectors_delete_last_t, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expectParameterClose( vectors_delete_last_t );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Vector_String_Lists.Delete_Last( theVector.vslVector, cnt );
       else
          Vector_String_Lists.Delete_Last( theVector.vslVector );
       end if;
     exception when constraint_error =>
       err_count( vectors_delete_last_t, cntExpr, cntType );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsDeleteLast;


------------------------------------------------------------------------------
--  CONTAINS
--
-- Syntax: b := vectors.contains( v, e )
-- Ada:    b := vectors.contains( v, e )
------------------------------------------------------------------------------

procedure ParseVectorsContains( result : out unbounded_string; kind : out identifier ) is
  vectorId  : identifier;
  theVector : resPtr;
  itemExpr  : unbounded_string;
  itemType  : identifier;
begin
  kind := boolean_t;
  expect( vectors_contains_t );
  ParseFirstInOutInstantiatedParameter( vectors_contains_t, vectorId, vectors_vector_t );
  ParseLastGenItemParameter( vectors_contains_t, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := to_bush_boolean( Vector_String_Lists.Contains( theVector.vslVector, itemExpr ) );

     end;
  end if;
end ParseVectorsContains;


------------------------------------------------------------------------------
--  MOVE
--
-- Syntax: vectors.move( v1, v2 );
-- Ada:    vectors.move( v1, v2 );
------------------------------------------------------------------------------

procedure ParseVectorsMove is
  sourceVectorId   : identifier;
  theSourceVector  : resPtr;
  targetVectorId   : identifier;
  theTargetVector  : resPtr;
begin
  expect( vectors_move_t );
  ParseFirstInOutInstantiatedParameter( vectors_move_t, sourceVectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( vectors_move_t, targetVectorId, vectors_vector_t );
  vectorItemIndicesOk( vectors_move_t, sourceVectorId, targetVectorid );
  genTypesOk( identifiers( targetVectorId ).genKind2, identifiers( sourceVectorId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetVectorId ).value.all ), theTargetVector );
       findResource( to_resource_id( identifiers( sourceVectorId ).value.all ), theSourceVector );
       Vector_String_Lists.Move( theTargetVector.vslVector, theSourceVector.vslVector );
     end;
  end if;
end ParseVectorsMove;

--procedure ParseVectorsAssign is
--  -- Syntax: assign( v1, v2 );
--  -- Ada:    assign( v1, v2 );
--  sourceVectorId   : identifier;
--  theSourceVector  : resPtr;
--  targetVectorId   : identifier;
--  theTargetVector  : resPtr;
--begin
--  expect( vectors_copy_t );
--  ParseFirstVectorParameter( targetVectorId );
--  ParseLastVectorParameter( sourceVectorId );
--  genTypesOk( identifiers( targetVectorId ).genKind, identifiers( sourceVectorId ).genKind );
--  if isExecutingCommand then
--     begin
--       findResource( to_resource_id( identifiers( targetVectorId ).value.all ), theTargetVector );
--       findResource( to_resource_id( identifiers( sourceVectorId ).value.all ), theSourceVector );
--       Vector_String_Lists.Copy( theTargetVector.vslVector, theSourceVector.vslVector );
--     end;
--  end if;
--end ParseVectorsAssign;


------------------------------------------------------------------------------
--  REVERSE ELEMENTS
--
-- Syntax: vectors.reverse_elements( v );
-- Ada:    vectors.reverse_elements( v );
------------------------------------------------------------------------------

procedure ParseVectorsReverseElements is
  vectorId  : identifier;
  theVector : resPtr;
begin
  expect( vectors_reverse_elements_t );
  ParseSingleInOutInstantiatedParameter( vectors_reverse_elements_t, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_String_Lists.Reverse_Elements( theVector.vslVector );
     end;
  end if;
end ParseVectorsReverseElements;


------------------------------------------------------------------------------
--  FLIP
--
-- Syntax: vectors.flip( v );
-- Ada:    N/A
------------------------------------------------------------------------------

procedure ParseVectorsFlip is
  vectorId  : identifier;
  theVector : resPtr;
begin
  expectAdaScript( subject => vectors_flip_t, remedy => "use reverse_elements" );
  ParseSingleInOutInstantiatedParameter( vectors_flip_t, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_String_Lists.Reverse_Elements( theVector.vslVector );
     end;
  end if;
end ParseVectorsFlip;

--procedure ParseVectorsNewCursor is
--  -- Syntax: vectors.new_cursor( c, t );
--  -- Ada:    N/A
--  resId : resHandleId;
--  ref : reference;
--  genKindId : identifier;
--begin
--  expect( vectors_new_cursor_t );
--  ParseFirstOutParameter( ref, vectors_cursor_t );
--  baseTypesOK( ref.kind, vectors_cursor_t );
--  expect( symbol_t, "," );
--  ParseIdentifier( genKindId );
--  if class_ok( genKindId, typeClass, subClass ) then
--      null;
--  end if;
--  identifiers( ref.id ).genKind := genKindId;
--  expect( symbol_t, ")" );
--  if isExecutingCommand then
--     identifiers( ref.id ).resource := true;
--     declareResource( resId, vector_string_list_cursor, getIdentifierBlock( ref.id ) );
--     AssignParameter( ref, to_unbounded_string( resId ) );
--  end if;
--end ParseVectorsNewCursor;


------------------------------------------------------------------------------
--  FIRST
--
-- Syntax: vectors.first( v, c );
-- Ada:    c := vectors.first( v );
------------------------------------------------------------------------------

procedure ParseVectorsFirst is
  vectorId   : identifier;
  theVector  : resPtr;
  theCursor  : resPtr;
  --cursRef    : reference;
  cursorId   : identifier;
begin
  expect( vectors_first_t );
  ParseFirstInOutInstantiatedParameter( vectors_first_t, vectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( vectors_first_t, cursorId , vectors_cursor_t );

  -- Check the type against the vector

  if not error_found then
     vectorItemIndicesOk( vectors_first_t, vectorId, cursorId );
     genElementsOk( vectors_first_t, vectorId, cursorId, identifiers( vectorId ).genKind2, identifiers( cursorId ).genKind2 );
     -- genTypesOk( identifiers( vectorId ).genKind2, identifiers( cursorId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       theCursor.vslCursor := Vector_String_Lists.First( theVector.vslVector );
     end;
  end if;
end ParseVectorsFirst;


------------------------------------------------------------------------------
--  LAST
--
-- Syntax: vectors.last( v, c );
-- Ada:    c := vectors.last( v );
------------------------------------------------------------------------------

procedure ParseVectorsLast is
  vectorId   : identifier;
  theVector  : resPtr;
  cursorId   : identifier;
  theCursor  : resPtr;
begin
  expect( vectors_last_t );
  ParseFirstInOutInstantiatedParameter( vectors_last_t, vectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( vectors_last_t, cursorId, vectors_cursor_t );
  -- Check the type against the vector
  if not error_found then
     vectorItemIndicesOk( vectors_last_t, vectorid, cursorId );
     genTypesOk( identifiers( vectorId ).genKind2, identifiers( cursorId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       theCursor.vslCursor := Vector_String_Lists.Last( theVector.vslVector );
     end;
  end if;
end ParseVectorsLast;


------------------------------------------------------------------------------
--  NEXT
--
-- Syntax: vectors.next( c );
-- Ada:    vectors.next( c );
------------------------------------------------------------------------------

procedure ParseVectorsNext is
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( vectors_next_t );
  ParseSingleInOutInstantiatedParameter( vectors_next_t, cursId, vectors_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).value.all ), theCursor );
       Vector_String_Lists.Next( theCursor.vslCursor );
     end;
  end if;
end ParseVectorsNext;


------------------------------------------------------------------------------
--  PREVIOUS
--
-- Syntax: vectors.previous( c );
-- Ada:    vectors.previous( c );
------------------------------------------------------------------------------

procedure ParseVectorsPrevious is
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( vectors_previous_t );
  ParseSingleInOutInstantiatedParameter( vectors_previous_t, cursId, vectors_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).value.all ), theCursor );
       Vector_String_Lists.Previous( theCursor.vslCursor );
     end;
  end if;
end ParseVectorsPrevious;


------------------------------------------------------------------------------
--  DELETE
--
-- Syntax: vectors.delete( v, c | i [, n] )
-- Ada:    vectors.delete( v, c | i [, n] )
------------------------------------------------------------------------------

procedure ParseVectorsDelete is
  vectorId  : identifier;
  theVector : resPtr;
  cursorId  : identifier;
  theCursor : resPtr;
  idxExpr   : unbounded_string;
  idxType   : identifier;
  hasIdx    : boolean := false;
  idx       : Vector_String_Lists.Extended_Index;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
  cnt       : Ada.Containers.Count_Type;
begin
  expect( vectors_delete_t );
  ParseFirstInOutInstantiatedParameter( vectors_delete_t, vectorId, vectors_vector_t );
  expectParameterComma( vectors_delete_t );
  if identifiers( token ).kind = vectors_cursor_t then
     ParseInOutInstantiatedParameter( cursorId, vectors_cursor_t );
  else
     ParseExpression( idxExpr, idxType );
     baseTypesOK( idxType, identifiers( vectorId ).genKind );
     hasIdx := true;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseNextNumericParameter( vectors_delete_t, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  end if;
  expectParameterClose( vectors_delete_t );
  if isExecutingCommand then
     begin
        findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
        if hasIdx then
           if hasCnt then
              -- TODO: shouldn't the account be rounded on a universal numeric?  Check casting,
              -- here and elsewhere.
              cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
              idx := Vector_String_Lists.Extended_Index( to_numeric( idxExpr ) );
              Vector_String_Lists.Delete( theVector.vslVector, idx, cnt );
           else
              idx := Vector_String_Lists.Extended_Index( to_numeric( idxExpr ) );
              Vector_String_Lists.Delete( theVector.vslVector, idx );
           end if;
        else
           findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
           if hasCnt then
              cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
              Vector_String_Lists.Delete( theVector.vslVector, theCursor.vslCursor, cnt );
           else
              Vector_String_Lists.Delete( theVector.vslVector, theCursor.vslCursor );
           end if;
        end if;
     exception when constraint_error =>
       err_count( vectors_delete_t, cntExpr, cntType );
     when storage_error =>
       err_storage;
     end;
  end if;
end ParseVectorsDelete;


------------------------------------------------------------------------------
--  HAS ELEMENT
--
-- Syntax: b := has_element( c );
-- Ada:    b := has_element( c );
------------------------------------------------------------------------------

procedure ParseVectorsHasElement( result : out unbounded_string; kind : out identifier ) is
  cursorId   : identifier;
  theCursor  : resPtr;
begin
  kind := boolean_t;
  expect( vectors_has_element_t );
  ParseSingleInOutInstantiatedParameter( vectors_has_element_t, cursorId, vectors_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       result := to_bush_boolean( Vector_String_Lists.Has_Element( theCursor.vslCursor ) );
     end;
  end if;
end ParseVectorsHasElement;


------------------------------------------------------------------------------
--  EQUAL
--
-- Syntax: b := equal( v1, v2 );
-- Ada:    b := v1 = v2;
------------------------------------------------------------------------------

procedure ParseVectorsEqual( result : out unbounded_string; kind : out identifier ) is
  leftVectorId  : identifier;
  rightVectorId : identifier;
  leftVector    : resPtr;
  rightVector   : resPtr;
  use Vector_String_Lists;
begin
  kind := boolean_t;
  expect( vectors_equal_t );
  ParseFirstInOutInstantiatedParameter( vectors_equal_t, leftVectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( vectors_equal_t, rightVectorId, vectors_vector_t );
  if not error_found then
     genTypesOk( identifiers( leftVectorId ).genKind, identifiers( rightVectorId ).genKind );
     genTypesOk( identifiers( leftVectorId ).genKind2, identifiers( rightVectorId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( leftVectorId ).value.all ), leftVector );
       findResource( to_resource_id( identifiers( rightVectorId ).value.all ), rightVector );
       result := to_bush_boolean( leftVector.vslVector = rightVector.vslVector );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsEqual;


------------------------------------------------------------------------------
--
--  INSERT FAMILY
--
-- In Ada, Insert can include both before an index or a cursor, and inserting
-- elements, a second vector or blank space.  The count may be optional and a
-- second cursor may be returned.  There are more variations than in doubly
-- linked lists or hashed maps.
--
-- For SparForte, these will be separate functions to make them easier to
-- implement and manage, though it breaks Ada compatibility.
-- If the count type and element type are the same, SparForte cannot
-- distinguish between them.  So we don't permit the element to be
-- absent.
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  INSERT (Insert function 1)
--
-- Syntax: vectors.insert( v, i, e [, c] )
-- Ada:    vectors.insert
--
-- Note: "Insert" is a reserved word in SparForte.
------------------------------------------------------------------------------

procedure ParseVectorsInsert is
  vectorId   : identifier;
  theVector  : resPtr;
  beforeVal  : unbounded_string;
  beforeType : identifier;
  elemVal    : unbounded_string;
  elemType   : identifier;
  cntExpr    : unbounded_string;
  cntType    : identifier;
  hasCnt     : boolean := false;
begin
  expect( vectors_insert_t );
  ParseFirstInOutInstantiatedParameter( vectors_insert_t, vectorId, vectors_vector_t );
  ParseNextNumericParameter( vectors_insert_t, beforeVal, beforeType, identifiers( vectorId ).genKind );

  -- In Ada, the element can be missing, the count can be missing, or both.
  -- If the count type and element type are the same, SparForte cannot
  -- distinguish between them.  So we don't permit the element to be
  -- absent.

  if token = symbol_t and identifiers( token ).value.all = "," then
     expectParameterComma( vectors_insert_t );
     ParseExpression( elemVal, elemType );
     if baseTypesOk( elemType, identifiers( vectorId ).genKind2 ) then
        if token = symbol_t and identifiers( token ).value.all = "," then
           ParseNextNumericParameter( vectors_insert_t, cntExpr, cntType, containers_count_type_t );
           hasCnt := true;
        end if;
     end if;
  end if;
  expectParameterClose( vectors_insert_t );

  if isExecutingCommand then
     declare
       idx : vector_index;
       cnt : ada.containers.count_type := 1;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       idx := toRealVectorIndex( vectorId, integer( to_numeric( beforeVal ) ) );
       if hasCnt then
          begin
             cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          exception when others =>
             err_count( vectors_insert_t, cntExpr, cntType );
          end;
          Vector_String_Lists.Insert( theVector.vslVector, idx, elemVal, cnt );
       else
          Vector_String_Lists.Insert( theVector.vslVector, idx, elemVal );
       end if;
     exception when constraint_error =>
       err_index( vectors_insert_t, beforeVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsInsert;


------------------------------------------------------------------------------
-- INSERT VECTOR (Insert function 2)
--
-- Syntax: insert_vector( v, c, v2 )
-- Ada:    insert
------------------------------------------------------------------------------

procedure ParseVectorsInsertVector is
  vectorId   : identifier;
  cursorId   : identifier;
  vector2Id  : identifier;
begin
  expect( vectors_insert_vector_t );
  ParseFirstInOutInstantiatedParameter( vectors_insert_vector_t, vectorId, vectors_vector_t );
  ParseNextInOutInstantiatedParameter( vectors_insert_vector_t, cursorId, vectors_cursor_t );
  ParseLastInOutInstantiatedParameter( vectors_insert_vector_t, vector2Id, vectors_vector_t );

  if isExecutingCommand then
     declare
       theVector  : resPtr;
       theCursor  : resPtr;
       theVector2 : resPtr;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       findResource( to_resource_id( identifiers( vector2Id ).value.all ), theVector2 );
       Vector_String_Lists.Insert(
          theVector.vslVector,
          theCursor.vslCursor,
          theVector2.vslVector
       );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsInsertVector;


------------------------------------------------------------------------------
-- INSERT BEFORE (Insert function 3)
--
-- "before" functions use a cursor instead of an index
--
-- Syntax: insert_before( v, c, e [, n] )
-- Ada:    insert
------------------------------------------------------------------------------

procedure ParseVectorsInsertBefore is
  vectorId   : identifier;
  cursorId   : identifier;
  elemVal    : unbounded_string;
  elemType   : identifier;
  cntExpr    : unbounded_string;
  cntType    : identifier;
  hasCnt     : boolean := false;
  --cursorId2  : identifier;
begin
  expect( vectors_insert_before_t );
  ParseFirstInOutInstantiatedParameter( vectors_insert_before_t, vectorId, vectors_vector_t );
  ParseNextInOutInstantiatedParameter( vectors_insert_before_t, cursorId, vectors_cursor_t );
  ParseNextGenItemParameter( vectors_insert_before_t, elemVal, elemType, identifiers( vectorId ).genKind2 );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseNextNumericParameter( vectors_insert_before_t, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  end if;

  expectParameterClose( vectors_insert_before_t );

  if isExecutingCommand then
     declare
       cnt        : ada.containers.count_type := 1;
       theVector  : resPtr;
       theCursor  : resPtr;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       if hasCnt then
          begin
             cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          exception when others =>
             err_count( vectors_insert_before_t, cntExpr, cntType );
          end;
       end if;
       Vector_String_Lists.Insert( theVector.vslVector, theCursor.vslCursor, elemVal, cnt );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsInsertBefore;


------------------------------------------------------------------------------
-- INSERT VECTOR AND MARK (Insert function 4)
--
-- "mark" functions return a cursor
--
-- Syntax: insert_vector_and_mark( v, c, v2, c2 )
-- Ada:    insert
------------------------------------------------------------------------------

procedure ParseVectorsInsertVectorAndMark is
  vectorId   : identifier;
  cursorId   : identifier;
  vector2Id  : identifier;
  cursor2Ref : reference;
begin
  expect( vectors_insert_vector_and_mark_t );
  ParseFirstInOutInstantiatedParameter( vectors_insert_vector_and_mark_t, vectorId, vectors_vector_t );
  ParseNextInOutInstantiatedParameter( vectors_insert_vector_and_mark_t, cursorId, vectors_cursor_t );
  ParseNextInOutInstantiatedParameter( vectors_insert_vector_and_mark_t, vector2Id, vectors_vector_t );
  ParseLastOutVectorCursor( vectors_insert_vector_and_mark_t, vectorId, cursor2Ref );

  if isExecutingCommand then
     declare
       theVector  : resPtr;
       theCursor  : resPtr;
       theVector2 : resPtr;
       theCursor2 : resPtr;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       findResource( to_resource_id( identifiers( vector2Id ).value.all ), theVector2 );

       -- For a reference, the index is significant if it is an array.
       -- I cannot remember if value is set for me in the reference.  I will
       -- assume I handle it manually.
       -- TODO: even if someone were to try this, the cursor functions are
       -- not written to support an array element.

       if identifiers( cursor2ref.id ).list then
          findResource( to_resource_id(
             identifiers( cursor2ref.id ).avalue( cursor2ref.index ) ),
             theCursor2 );
       else
          findResource( to_resource_id( identifiers( cursor2ref.id ).value.all ), theCursor2 );
       end if;
       -- TODO: the cursor may not exist

       Vector_String_Lists.Insert(
          theVector.vslVector,
          theCursor.vslCursor,
          theVector2.vslVector,
          theCursor2.vslCursor
       );

       -- The resource does not need to be updated.

     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsInsertVectorAndMark;


------------------------------------------------------------------------------
-- INSERT BEFORE AND MARK (Insert function 5)
--
-- "before" functions use a cursor instead of an index
-- "mark" functions return a cursor
--
-- Syntax: insert_before_and_mark( v, c, e, c2 [, n])
-- Ada:    insert
------------------------------------------------------------------------------

procedure ParseVectorsInsertBeforeAndMark is
  vectorId   : identifier;
  cursorId   : identifier;
  elemVal    : unbounded_string;
  elemType   : identifier;
  cursor2Ref : reference;
  cntExpr    : unbounded_string;
  cntType    : identifier;
  hasCnt     : boolean := false;
begin
  expect( vectors_insert_before_and_mark_t );
  ParseFirstInOutInstantiatedParameter( vectors_insert_before_and_mark_t, vectorId, vectors_vector_t );
  ParseNextInOutInstantiatedParameter( vectors_insert_before_and_mark_t, cursorId, vectors_cursor_t );

  expectParameterComma( vectors_insert_before_and_mark_t );
  ParseExpression( elemVal, elemType );
  if baseTypesOk( elemType, identifiers( vectorId ).genKind2 ) then
     ParseNextOutVectorCursor( vectors_insert_before_and_mark_t, vectorId, cursor2Ref );
     if token = symbol_t and identifiers( token ).value.all = "," then
        ParseNextNumericParameter( vectors_insert_before_and_mark_t, cntExpr, cntType, containers_count_type_t );
        hasCnt := true;
     end if;
  end if;

  expectParameterClose( vectors_insert_before_and_mark_t );

  if isExecutingCommand then
     declare
       cnt        : ada.containers.count_type := 1;
       theVector  : resPtr;
       theCursor  : resPtr;
       theCursor2 : resPtr;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );

       -- For a reference, the index is significant if it is an array.
       -- I cannot remember if value is set for me in the reference.  I will
       -- assume I handle it manually.
       -- TODO: even if someone were to try this, the cursor functions are
       -- not written to support an array element.

       if identifiers( cursor2ref.id ).list then
          findResource( to_resource_id(
             identifiers( cursor2ref.id ).avalue( cursor2ref.index ) ),
             theCursor2 );
       else
          findResource( to_resource_id( identifiers( cursor2ref.id ).value.all ), theCursor2 );
       end if;
       -- TODO: the cursor may not exist

       if hasCnt then
          begin
             cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          exception when others =>
             err_count( vectors_insert_before_and_mark_t, cntExpr, cntType );
          end;
       end if;
       Vector_String_Lists.Insert(
          Container => theVector.vslVector,
          Before    => theCursor.vslCursor,
          New_Item  => elemVal,
          Position  => theCursor2.vslCursor,
          Count     => cnt
       );

       -- The resource does not need to be updated.

     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsInsertBeforeAndMark;


------------------------------------------------------------------------------
--  INSERT SPACE
--
-- Syntax: insert_space( v, i [, n] ) | ( v, c, c2 [, n] );
-- Ada:    insert_space( v, i [, n] ) | ( v, c, c2 [, n] );
------------------------------------------------------------------------------

procedure ParseVectorsInsertSpace is
  vectorId   : identifier;
  beforeCursorId : identifier;
  beforeIdxExpr : unbounded_string;
  beforeIdxType : identifier;
  positionCursorRef : reference;
  cntExpr    : unbounded_string;
  cntType    : identifier;
  theVector  : resPtr;
  cnt        : ada.containers.count_type := 1;
  hasIdx     : boolean := false;
  hasCnt     : boolean := false;
begin
  expect( vectors_insert_space_t );
  ParseFirstInOutInstantiatedParameter( vectors_insert_space_t, vectorId, vectors_vector_t );
  expectParameterComma( vectors_insert_space_t );
  if identifiers( token ).kind = vectors_cursor_t then
     ParseInOutInstantiatedParameter( beforeCursorId, vectors_cursor_t );
     ParseNextOutVectorCursor( vectors_insert_space_t, vectorId, positionCursorRef );
  else
     ParseExpression( beforeIdxExpr, beforeIdxType );
     baseTypesOK( beforeIdxType, identifiers( vectorId ).genKind );
     hasIdx := true;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseNextNumericParameter( vectors_insert_space_t, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  end if;
  expectParameterClose( vectors_insert_space_t );

  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );

       -- Either variation can have a count

       if hasCnt then
           begin
              cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
           exception when others =>
             err_count( vectors_insert_space_t, cntExpr, cntType );
           end;
       end if;

       -- variation 1: with index
       -- variation 2: with cursor and out position cursor

       if hasIdx then
          declare
             idx : vector_index;
          begin
             idx := toRealVectorIndex( vectorId, integer( to_numeric( beforeIdxExpr ) ) );
             Vector_String_Lists.Insert_Space( theVector.vslVector, idx, cnt );
          end;
       else
          declare
             positionCursorResourceId : resHandleId;
             theBeforeCursor  : resPtr;
             thePositionCursor : resPtr;
          begin
             findResource(
                 to_resource_id( identifiers( beforeCursorId ).value.all ),
                 theBeforeCursor
             );

             -- the second cursor is the out parameter.  Declare it.  Then fetch it.
             -- TODO: if out is overwriting a resource, delete old resource(s)
             identifiers( positionCursorRef.id ).resource := true;
             declareResource(
                 positionCursorResourceId,
                 vector_string_list_cursor,
                 getIdentifierBlock( positionCursorRef.id )
             );
             AssignParameter(
                 positionCursorRef,
                 to_unbounded_string( positionCursorResourceId )
             );
             findResource( positionCursorResourceId, thePositionCursor );
             Vector_String_Lists.Insert_Space(
                theVector.vslVector,
                theBeforeCursor.vslCursor,
                thePositionCursor.vslCursor,
                cnt
             );
          end;
       end if;
     exception when storage_error =>
       err_storage;
     when program_error =>
       err_cursor_mismatch;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsInsertSpace;


------------------------------------------------------------------------------
--  APPEND VECTOR
--
-- Syntax: append_vector( v1, v2 );
-- Ada:    v0 := v1 & v2;
------------------------------------------------------------------------------

procedure ParseVectorsAppendVector is
  leftVectorId  : identifier;
  rightVectorId : identifier;
  leftVector    : resPtr;
  rightVector   : resPtr;
  use Vector_String_Lists;
begin
  expect( vectors_append_vector_t );
  ParseFirstInOutInstantiatedParameter( vectors_append_vector_t, leftVectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( vectors_append_vector_t, rightVectorId, vectors_vector_t );
  if not error_found then
     genTypesOk( identifiers( leftVectorId ).genKind, identifiers( rightVectorId ).genKind );
     genTypesOk( identifiers( leftVectorId ).genKind2, identifiers( rightVectorId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( leftVectorId ).value.all ), leftVector );
       findResource( to_resource_id( identifiers( rightVectorId ).value.all ), rightVector );
       leftVector.vslVector := leftVector.vslVector & rightVector.vslVector;
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsAppendVector;


------------------------------------------------------------------------------
--  SWAP
--
-- Syntax: swap( v, i1, i2 | v, c1, c2 );
-- Ada:    swap( v, i1, i2 | v, c1, c2 );
------------------------------------------------------------------------------

procedure ParseVectorsSwap is
  vectorId   : identifier;
  cursorId   : identifier;
  cursorId2  : identifier;
  theVector  : resPtr;
  theCursor  : resPtr;
  theCursor2 : resPtr;
  idxExpr1   : unbounded_string;
  idxType1   : identifier;
  idxExpr2   : unbounded_string;
  idxType2   : identifier;
  hasIdx     : boolean := false;
begin
  expect( vectors_swap_t );
  ParseFirstInOutInstantiatedParameter( vectors_swap_t, vectorId, vectors_vector_t );
  expectParameterComma( vectors_swap_t );
  if identifiers( token ).kind = vectors_cursor_t then
     ParseInOutInstantiatedParameter( cursorId, vectors_cursor_t );
     ParseLastInOutInstantiatedParameter( vectors_swap_t, cursorId2, vectors_cursor_t );
  else
     ParseExpression( idxExpr1, idxType1 );
     baseTypesOK( idxType1, identifiers( vectorId ).genKind );
     expectParameterComma( vectors_swap_t );
     ParseExpression( idxExpr2, idxType2 );
     baseTypesOK( idxType2, identifiers( vectorId ).genKind );
     hasIdx := true;
     expectParameterClose( vectors_swap_t );
  end if;

  if isExecutingCommand then
     if hasIdx then
        declare
           idx1 : vector_index;
           idx2 : vector_index;
        begin
           idx1 := toRealVectorIndex( vectorId, integer( to_numeric( idxExpr1 ) ) );
           idx2 := toRealVectorIndex( vectorId, integer( to_numeric( idxExpr2 ) ) );
           findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
           Vector_String_Lists.Swap( theVector.vslVector, idx1, idx2 );
        exception when constraint_error =>
           err_index( vectors_swap_t, idxExpr1, idxExpr2 );
        when others =>
           err_exception_raised;
        end;
     else
        begin
           findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
           findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
           findResource( to_resource_id( identifiers( cursorId2 ).value.all ), theCursor2 );
           Vector_String_Lists.Swap( theVector.vslVector, theCursor.vslCursor, theCursor2.vslCursor );
        exception when program_error =>
           err_cursor_mismatch;
        when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParseVectorsSwap;


------------------------------------------------------------------------------
--  FIND
--
-- Syntax: find( v, e, ,c1 ,c2 );
-- Ada:    find( v, e, [,c1], c2 );
------------------------------------------------------------------------------

procedure ParseVectorsFind is
  vectorId      : identifier;
  itemExpr      : unbounded_string;
  itemType      : identifier;
  startCursorId : identifier;
  positionCursorRef : reference;
begin
  expect( vectors_find_t );
  ParseFirstInOutInstantiatedParameter( vectors_find_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_find_t, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  -- It is tricky to handle one optional cursor followed by a required one,
  -- one existing and one a reference.
  ParseNextInOutInstantiatedParameter( vectors_find_t, startCursorId, vectors_cursor_t );
  ParseLastOutVectorCursor( vectors_find_t, vectorId, positionCursorRef );

  if isExecutingCommand then
     declare
        theVector : resPtr;
        theCursor : resPtr;
        --
        positionCursorResourceId : resHandleId;
        thePositionCursor : resPtr;
     begin
        findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
        findResource( to_resource_id( identifiers( startCursorId ).value.all ), theCursor );

        -- the second cursor is the out parameter.  Declare it.  Then fetch it.
        -- TODO: if out is overwriting a resource, delete old resource(s)
        identifiers( positionCursorRef.id ).resource := true;
        declareResource(
            positionCursorResourceId,
            vector_string_list_cursor,
            getIdentifierBlock( positionCursorRef.id )
        );
        AssignParameter(
            positionCursorRef,
            to_unbounded_string( positionCursorResourceId )
        );
        findResource( positionCursorResourceId, thePositionCursor );

        thePositionCursor.vslCursor := Vector_String_Lists.Find( theVector.vslVector, itemExpr, theCursor.vslCursor );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsFind;


------------------------------------------------------------------------------
--  REVERSE FIND
--
-- Syntax: reverse_find( v, e, ,c1 ,c2 );
-- Ada:    reverse_find( v, e, [,c1], c2 );
------------------------------------------------------------------------------

procedure ParseVectorsReverseFind is
  vectorId      : identifier;
  itemExpr      : unbounded_string;
  itemType      : identifier;
  startCursorId : identifier;
  positionCursorRef : reference;
begin
  expect( vectors_reverse_find_t );
  ParseFirstInOutInstantiatedParameter( vectors_reverse_find_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_reverse_find_t, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  -- It is tricky to handle one optional cursor followed by a required one,
  -- one existing and one a reference.
  ParseNextInOutInstantiatedParameter( vectors_reverse_find_t, startCursorId, vectors_cursor_t );
  ParseLastOutVectorCursor( vectors_reverse_find_t, vectorId, positionCursorRef );

  if isExecutingCommand then
     declare
        theVector : resPtr;
        theCursor : resPtr;
        --
        positionCursorResourceId : resHandleId;
        thePositionCursor : resPtr;
     begin
        findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
        findResource( to_resource_id( identifiers( startCursorId ).value.all ), theCursor );

        -- the second cursor is the out parameter.  Declare it.  Then fetch it.
        -- TODO: if out is overwriting a resource, delete old resource(s)
        identifiers( positionCursorRef.id ).resource := true;
        declareResource(
            positionCursorResourceId,
            vector_string_list_cursor,
            getIdentifierBlock( positionCursorRef.id )
        );
        AssignParameter(
            positionCursorRef,
            to_unbounded_string( positionCursorResourceId )
        );
        findResource( positionCursorResourceId, thePositionCursor );

        thePositionCursor.vslCursor := Vector_String_Lists.Reverse_Find( theVector.vslVector, itemExpr, theCursor.vslCursor );
     end;
  end if;
end ParseVectorsReverseFind;


------------------------------------------------------------------------------
--  FIND INDEX
--
-- Syntax: find_index( v, e, ,i1, i2 );
-- Ada:    find_index( v, e, [,i1], i2 );
------------------------------------------------------------------------------

procedure ParseVectorsFindIndex is
  vectorId      : identifier;
  itemExpr      : unbounded_string;
  itemType      : identifier;
  startIdxExpr  : unbounded_string;
  startIdxType  : identifier;
  positionIdxRef : reference;
begin
  expect( vectors_find_index_t );
  ParseFirstInOutInstantiatedParameter( vectors_find_index_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_find_index_t, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  -- in Ada, the next parameter is optional but is easier to make optional
  -- than find with two cursors
  ParseNextGenItemParameter( vectors_find_index_t, startIdxExpr, startIdxType, identifiers( vectorId ).genKind );
  ParseLastOutParameter( vectors_find_index_t, positionIdxRef, identifiers( vectorId ).genKind );

  if isExecutingCommand then
     declare
        theVector   : resPtr;
        startIdx    : vector_index;
        positionIdx : vector_index;
        positionValue : unbounded_string;
     begin
        findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );

        startIdx := toRealVectorIndex( vectorId, integer( to_numeric( startIdxExpr ) ) );

        positionIdx := Vector_String_Lists.Find_Index( theVector.vslVector, itemExpr, startIdx );
        positionvalue := to_unbounded_string( long_float( toUserVectorIndex( vectorId, positionIdx ) ) );

        AssignParameter(
            positionIdxRef,
            positionValue
        );


     end;
  end if;
end ParseVectorsFindIndex;


------------------------------------------------------------------------------
--  REVERSE FIND INDEX
--
-- Syntax: reverse_find_index( v, e, ,i1, i2 );
-- Ada:    reverse_find_index( v, e, [,i1], i2 );
------------------------------------------------------------------------------

procedure ParseVectorsReverseFindIndex is
  vectorId      : identifier;
  itemExpr      : unbounded_string;
  itemType      : identifier;
  startIdxExpr  : unbounded_string;
  startIdxType  : identifier;
  positionIdxRef : reference;
begin
  expect( vectors_reverse_find_index_t );
  ParseFirstInOutInstantiatedParameter( vectors_reverse_find_index_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_reverse_find_index_t, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  -- in Ada, the next parameter is optional but is easier to make optional
  -- than find with two cursors
  ParseNextGenItemParameter( vectors_reverse_find_index_t, startIdxExpr, startIdxType, identifiers( vectorId ).genKind );
  ParseLastOutParameter( vectors_reverse_find_index_t, positionIdxRef, identifiers( vectorId ).genKind );

  if isExecutingCommand then
     declare
        theVector   : resPtr;
        startIdx    : vector_index;
        positionIdx : vector_index;
        positionValue : unbounded_string;
     begin
        findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );

        startIdx := toRealVectorIndex( vectorId, integer( to_numeric( startIdxExpr ) ) );

        positionIdx := Vector_String_Lists.Reverse_Find_Index( theVector.vslVector, itemExpr, startIdx );
        positionvalue := to_unbounded_string( long_float( toUserVectorIndex( vectorId, positionIdx ) ) );

        AssignParameter(
            positionIdxRef,
            positionValue
        );

     end;
  end if;
end ParseVectorsReverseFindIndex;


------------------------------------------------------------------------------
--  INCREMENT
--
-- Syntax: vectors.increment( v, i [, n] );
-- Ada:    vectors.increment( v, i [, n] );
------------------------------------------------------------------------------

procedure ParseVectorsIncrement is
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : unbounded_string;
  idxType   : identifier;
  numExpr   : unbounded_string;
  numType   : identifier;
  hasAmt    : boolean := false;
begin
  expectAdaScript( subject => vectors_increment_t, remedy => "use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( vectors_increment_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_increment_t, idxExpr, idxType, identifiers( vectorId ).genKind );
  if getUniType( identifiers( vectorId ).genKind2 ) /= uni_numeric_t then
     err( "increment requires a numeric element type" );
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     hasAmt := true;
     ParseLastStringParameter( vectors_increment_t, numExpr, numType, identifiers( vectorId ).genKind2 );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expect( symbol_t, ")" );
  else
     err( ", or ) expected" );
  end if;
  if isExecutingCommand then
     declare
       floatVal  : long_float;
     begin
       if hasAmt then
          floatVal := long_float( natural( to_numeric( numExpr ) ) );
       else
          floatVal := 1.0;
       end if;
       declare
         idx : vector_index;
       begin
         findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
         idx := toRealVectorIndex( vectorId, integer( to_numeric( idxExpr ) ) );
         Increment( theVector.vslVector, idx, floatVal );
       exception when constraint_error =>
         err( "prepend count must be a natural integer" );
       when storage_error =>
         err_storage;
       end;
     exception when constraint_error =>
       err( "increment value is not natural" );
     end;
  end if;
end ParseVectorsIncrement;


------------------------------------------------------------------------------
--  DECREMENT
--
-- Syntax: vectors.decrement( v, i [, n] );
-- Ada:    vectors.decrement( v, i [, n] );
------------------------------------------------------------------------------

procedure ParseVectorsDecrement is
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : unbounded_string;
  idxType   : identifier;
  numExpr   : unbounded_string;
  numType   : identifier;
  hasAmt    : boolean := false;
begin
  expectAdaScript( subject => vectors_decrement_t, remedy => "use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( vectors_decrement_t, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( vectors_decrement_t, idxExpr, idxType, identifiers( vectorId ).genKind );
  if getUniType( identifiers( vectorId ).genKind2 ) /= uni_numeric_t then
     err( "decrement requires a numeric element type" );
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     hasAmt := true;
     ParseLastStringParameter( vectors_decrement_t, numExpr, numType, identifiers( vectorId ).genKind2 );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expect( symbol_t, ")" );
  else
     err( ", or ) expected" );
  end if;
  if isExecutingCommand then
     declare
       floatVal  : long_float;
     begin
       if hasAmt then
          floatVal := long_float( natural( to_numeric( numExpr ) ) );
       else
          floatVal := 1.0;
       end if;
       declare
         idx : vector_index;
       begin
         findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
         idx := toRealVectorIndex( vectorId, integer( to_numeric( idxExpr ) ) );
         Decrement( theVector.vslVector, idx, floatVal );
       exception when constraint_error =>
         err( "prepend count must be a natural integer" );
       when storage_error =>
         err_storage;
       end;
     exception when constraint_error =>
       err( "increment value is not natural" );
     end;
  end if;
end ParseVectorsDecrement;


------------------------------------------------------------------------------
--  ASSIGN
--
-- Syntax: hashed_maps.assign( t, s );
-- Ada:    hashed_maps.assign( t, s );
------------------------------------------------------------------------------

procedure ParseVectorsAssign is
  targetVectorId   : identifier;
  sourceVectorId   : identifier;
  targetVector  : resPtr;
  sourceVector  : resPtr;
begin
  expect( vectors_assign_t );
  ParseFirstInOutInstantiatedParameter( vectors_assign_t, targetVectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( vectors_assign_t, sourceVectorId, vectors_vector_t );
  if not error_found then
     genTypesOk( identifiers( targetVectorId ).genKind, identifiers( sourceVectorId ).genKind );
     genTypesOk( identifiers( targetVectorId ).genKind2, identifiers( sourceVectorId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetVectorId ).value.all ), targetVector );
       findResource( to_resource_id( identifiers( sourceVectorId ).value.all ), sourceVector );
       Vector_String_Lists.Assign( targetVector.vslVector, sourceVector.vslVector );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsAssign;



-----------------------------------------------------------------------------
--
-- Housekeeping
--
-----------------------------------------------------------------------------


procedure StartupVectors is
begin
  declareNamespace( "vectors" );

  -- Data Types

  declareIdent( vectors_vector_t, "vectors.vector", variable_t, genericTypeClass );
  identifiers( vectors_vector_t).usage := limitedUsage;
  declareIdent( vectors_cursor_t, "vectors.cursor", variable_t, genericTypeClass );
  identifiers( vectors_cursor_t).usage := limitedUsage;
  -- no_index is not implemented as it has a number of challenges.  for example,
  -- it is outside of the range of the vector index.  It might be difficult
  -- to equate to an enumerated type.

  -- Subprograms

  declareProcedure( vectors_clear_t,     "vectors.clear",    ParseVectorsClear'access );
  declareProcedure( vectors_to_vector_t, "vectors.to_vector",   ParseVectorsToVector'access );
  declareFunction(  vectors_capacity_t,  "vectors.capacity",    ParseVectorsCapacity'access );
  declareProcedure( vectors_reserve_capacity_t,  "vectors.reserve_capacity",    ParseVectorsReserveCapacity'access );
  declareFunction(  vectors_length_t,    "vectors.length",    ParseVectorsLength'access );
  declareProcedure( vectors_set_length_t,  "vectors.set_length",    ParseVectorsSetLength'access );
  declareFunction(  vectors_is_empty_t,  "vectors.is_empty",  ParseVectorsIsEmpty'access );
  declareProcedure( vectors_append_vector_t,  "vectors.append_vector",    ParseVectorsAppendVector'access );
  declareProcedure( vectors_append_elements_t,  "vectors.append_elements",    ParseVectorsAppendElements'access );
  declareProcedure( vectors_prepend_elements_t,  "vectors.prepend_elements",    ParseVectorsPrependElements'access );
  declareProcedure( vectors_prepend_t,  "vectors.prepend",    ParseVectorsPrepend'access );
  declareFunction(  vectors_first_index_t,  "vectors.first_index",    ParseVectorsFirstIndex'access );
  declareFunction(  vectors_last_index_t,  "vectors.last_index",    ParseVectorsLastIndex'access );
  declareFunction(  vectors_element_t,  "vectors.element",    ParseVectorsElement'access );
  declareFunction(  vectors_first_element_t,  "vectors.first_element",    ParseVectorsFirstElement'access );
  declareFunction(  vectors_last_element_t,  "vectors.last_element",    ParseVectorsLastElement'access );
  declareProcedure( vectors_delete_first_t,  "vectors.delete_first",    ParseVectorsDeleteFirst'access );
  declareProcedure( vectors_delete_last_t,  "vectors.delete_last",    ParseVectorsDeleteLast'access );
  declareFunction(  vectors_contains_t,  "vectors.contains",    ParseVectorsContains'access );
  declareProcedure( vectors_move_t,  "vectors.move",    ParseVectorsMove'access );
  declareProcedure( vectors_reverse_elements_t,  "vectors.reverse_elements",    ParseVectorsReverseElements'access );
  declareProcedure( vectors_flip_t,  "vectors.flip",    ParseVectorsFlip'access );
  declareProcedure( vectors_first_t,  "vectors.first", ParseVectorsFirst'access );
  declareProcedure( vectors_last_t,  "vectors.last", ParseVectorsLast'access );
  declareProcedure( vectors_next_t,  "vectors.next", ParseVectorsNext'access );
  declareProcedure( vectors_previous_t,  "vectors.previous", ParseVectorsPrevious'access );
  declareProcedure( vectors_delete_t,  "vectors.delete", ParseVectorsDelete'access );
  declareFunction(  vectors_has_element_t,  "vectors.has_element", ParseVectorsHasElement'access );
  declareFunction(  vectors_equal_t,  "vectors.equal", ParseVectorsEqual'access );
  declareProcedure( vectors_append_t, "vectors.append", ParseVectorsAppend'access );
  declareProcedure( vectors_insert_t, "vectors.insert", ParseVectorsInsert'access );
  declareProcedure( vectors_insert_vector_t, "vectors.insert_vector", ParseVectorsInsertVector'access );
  declareProcedure( vectors_insert_before_t, "vectors.insert_before", ParseVectorsInsertBefore'access );
  declareProcedure( vectors_insert_before_and_mark_t, "vectors.insert_before_and_mark", ParseVectorsInsertBeforeAndMark'access );
  declareProcedure( vectors_insert_vector_and_mark_t, "vectors.insert_vector_and_mark", ParseVectorsInsertVectorAndMark'access );
  declareProcedure( vectors_insert_space_t, "vectors.insert_space", ParseVectorsInsertSpace'access );
  declareProcedure( vectors_swap_t, "vectors.swap", ParseVectorsSwap'access );
  declareProcedure( vectors_find_t, "vectors.find", ParseVectorsFind'access );
  declareProcedure( vectors_reverse_find_t, "vectors.reverse_find", ParseVectorsReverseFind'access );
  declareProcedure( vectors_find_index_t, "vectors.find_index", ParseVectorsFindIndex'access );
  declareProcedure( vectors_reverse_find_index_t, "vectors.reverse_find_index", ParseVectorsReverseFindIndex'access );
  declareProcedure( vectors_increment_t, "vectors.increment", ParseVectorsIncrement'access );
  declareProcedure( vectors_decrement_t, "vectors.decrement", ParseVectorsDecrement'access );
  declareProcedure( vectors_assign_t, "vectors.assign", ParseVectorsAssign'access );

  declareNamespaceClosed( "vectors" );
end StartupVectors;

procedure ShutdownVectors is
begin
  null;
end ShutdownVectors;

end parser_vectors;

