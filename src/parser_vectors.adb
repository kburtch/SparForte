------------------------------------------------------------------------------
-- Singly Linked Lists Package Parser                                       --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with
    Ada.Containers,
    ada.strings.unbounded,
    gnat.source_info,
    pegasoft.strings,
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
    pegasoft,
    pegasoft.strings,
    pegasoft.vectors,
    world,
    scanner,
    scanner.communications,
    scanner_res,
    scanner_restypes,
    parser,
    parser_params,
    parser_containers;

--with text_io;use text_io;

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
vectors_to_index_t      : identifier;
vectors_no_index_t      : identifier;

User_No_Index : constant long_integer := long_integer( integer'first - 1 );
-- long_integer'first will become a long float with an exponent.

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
  err( +"storage_error raised" );
end err_storage;


------------------------------------------------------------------------------
--  ERR EMPTY
--
------------------------------------------------------------------------------

procedure err_empty( subprogram : identifier; vectorId : identifier) is
begin
  err( context => subprogram,
       subject => vectorId,
       reason => +"is",
       obstructorNotes => +"empty" );
end err_empty;


------------------------------------------------------------------------------
--  ERR INDEX
--
------------------------------------------------------------------------------

procedure err_index( subprogram : identifier; idxExpr : storage ) is
begin
  err( context => subprogram,
       subjectNotes => pl( qp( "the index position" ) ) & em_value( idxExpr.value ),
       reason => +"is",
       obstructorNotes => +"not in the vector"
  );
end err_index;

procedure err_index( subprogram : identifier; idxExpr1, idxExpr2 : storage ) is
begin
  err( context => subprogram,
       subjectNotes => pl( qp( "the index position" ) ) & em_value( idxExpr1.value ) &
                  pl( " or" ) & em_value( idxExpr2.value ),
       reason => +"is",
       obstructorNotes => +"not in the vector"
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

procedure err_count( subprogram : identifier; cntExpr : storage; cntType : identifier ) is
begin
   if cntExpr.value = "" then
      err( context => subprogram,
           subjectNotes => pl( qp( "the count value" ) ),
           subjectType  => cntType,
           reason => +"has",
           obstructorNotes => +"no assigned value",
           remedy => +"the value should be >= 0"
      );
   else
      err( context => subprogram,
           subjectNotes => pl( qp( "the count value of " ) & toSecureData( to_string( cntExpr.value ) ) ),
           subjectType  => cntType,
           reason => +"is not valid for",
           obstructor => containers_count_type_t,
           remedy => +"the value should be >= 0"
     );
   end if;
end err_count;


------------------------------------------------------------------------------
--  ERR CURSOR MISMATCH
--
-- This probably never happens because we use a single Ada vector package for
-- everything.
------------------------------------------------------------------------------

procedure err_cursor_mismatch is
begin
  err( +"the cursor does not match this vector" );
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
       subjectNotes => pl( qp( "the index of " ) ) & name_em( leftVectorItem ),
       subjectType => identifiers( leftVectorItem ).genKind,
       reason => +"should have the identical type as",
       obstructorNotes => +"the index of " & name_em( rightVectorItem ),
       obstructorType => identifiers( rightVectorItem ).genKind
    );
  end if;
end vectorItemIndicesOk;

-- 3-way version

procedure vectorItemIndicesOk( subprogram, leftVectorItem, centerVectorItem, rightVectorItem : identifier ) is
begin
-- TODO types ok flag
  if identifiers( leftVectorItem ).genKind /= identifiers( rightVectorItem ).genKind then
     if identifiers( leftVectorItem ).genKind = identifiers( centerVectorItem ).genKind then
        -- left, center match but right
        err(
          context => subprogram,
          subjectNotes => pl( qp( "the index of " ) ) & name_em( rightVectorItem ),
          subjectType => identifiers( rightVectorItem ).genKind,
          reason => +"should have the identical type as",
          obstructorNotes => +"the index of " & name_em( leftVectorItem ) &
             pl( " and " ) & name_em( centerVectorItem ),
          obstructorType => identifiers( leftVectorItem ).genKind
        );
     elsif identifiers( rightVectorItem ).genKind = identifiers( centerVectorItem ).genKind then
        -- center, right match but left
        err(
          context => subprogram,
          subjectNotes => pl( qp( "the index of " ) ) & name_em( leftVectorItem ),
          subjectType => identifiers( leftVectorItem ).genKind,
          reason => +"should have the identical type as",
          obstructorNotes => +"the index of " & name_em( centerVectorItem ) &
             pl( " and " ) & name_em( rightVectorItem ),
          obstructorType => identifiers( rightVectorItem ).genKind
        );
     else
       -- all 3 different
        err(
          context => subprogram,
          subjectNotes => pl( qp( "all indices" ) ),
          reason => +"should have identical types but are different for",
          obstructorNotes => name_em( leftVectorItem ) &
             pl( ", " ) & name_em( centerVectorItem ) &
             pl( " and " ) & name_em( rightVectorItem )
        );
     end if;
  elsif identifiers( leftVectorItem ).genKind /= identifiers( centerVectorItem ).genKind then
     -- left, right match but not center
     err(
       context => subprogram,
       subjectNotes => pl( qp( "the index of " ) ) & name_em( centerVectorItem ),
       subjectType => identifiers( centerVectorItem ).genKind,
       reason => +"should have the identical type as",
       obstructorNotes => +"the index of " & name_em( leftVectorItem ) &
          pl( " and " ) & name_em( rightVectorItem ),
       obstructorType => identifiers( rightVectorItem ).genKind
    );
  end if;
end vectorItemIndicesOk;


------------------------------------------------------------------------------
--  VECTOR INDEX OK
--
-- Checks to see if an expression can be used for the vector index.
------------------------------------------------------------------------------
-- This should be merged into the Scanner.??

--procedure vectorIndexOk( subprogram, leftType, rightVectorItem : identifier ) is
--  effectiveLeftType : identifier;
--  effectiveRightType : identifier;
--begin
--  -- A universal typeless or universal numeric always matches a numeric index.
--  if leftType = uni_numeric_t and leftType = getUnitype( identifiers( rightVectorItem ).genKind ) then
--    return;
--  elsif leftType = universal_t and uni_numeric_t = getUnitype( identifiers( rightVectorItem ).genKind ) then
--    return;
--  end if;
--
--  effectiveLeftType := getBasetype( leftType );
--  effectiverightType := getBasetype( identifiers( rightVectorItem ).genKind );
--
--  -- Something derivedfrom universalnumeric also matches a numeric index
--  if effectiveLeftType = uni_numeric_t and uni_numeric_t = getUnitype( identifiers( rightVectorItem ).genKind )  then
--    return;
--  end if;
--
--  -- Otherwise, the effective types should match as normal.
---- TODO types ok flag to avoid excessive type checking
--  if effectiveLeftType /= effectiveRightType then
--     err(
--       context => subprogram,
--       subjectNotes => "the expression",
--       subjectType => leftType,
--       reason => "is not compatible with",
--       obstructorNotes => "the index of " & optional_yellow( to_string( identifiers( rightVectorItem ).name ) ),
--       obstructorType => identifiers( rightVectorItem ).genKind
--    );
--  end if;
--end vectorIndexOk;


------------------------------------------------------------------------------
-- VECTOR INDEX OR CURSOR
--
-- insert_space can be a vector or a cursor
------------------------------------------------------------------------------

procedure vectorIndexOrCursorOk( subprogram, leftType, rightVectorItem : identifier ) is
  effectiveLeftType : identifier;
  effectiveRightType : identifier;
begin
  -- A universal typeless or universal numeric always matches a numeric index.
  if leftType = uni_numeric_t and leftType = getUnitype( identifiers( rightVectorItem ).genKind ) then
    return;
  elsif leftType = universal_t and uni_numeric_t = getUnitype( identifiers( rightVectorItem ).genKind ) then
    return;
  end if;

  effectiveLeftType := getBasetype( leftType );
  effectiverightType := getBasetype( identifiers( rightVectorItem ).genKind );

  -- Something derivedfrom universalnumeric also matches a numeric index
  if effectiveLeftType = uni_numeric_t and uni_numeric_t = getUnitype( identifiers( rightVectorItem ).genKind )  then
    return;
  end if;

  -- Otherwise, the effective types should match as normal.
-- TODO types ok flag to avoid excessive type checking
  if effectiveLeftType /= effectiveRightType then
     err(
       context => subprogram,
       subjectNotes => subjectExpression,
       subjectType => leftType,
       reason => +"is not compatible with",
       obstructorNotes => +"a cursor or the index of " & name_em( rightVectorItem ),
       obstructorType => identifiers( rightVectorItem ).genKind
    );
  end if;
end vectorIndexOrCursorOk;


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

        declareResource( resId, vector_storage_list_cursor, getIdentifierBlock( cursRef.id ) );
        identifiers( cursRef.id ).sStorage.metaLabel := noMetaLabel;
        identifiers( cursRef.id ).sStorage.value := to_unbounded_string( resId );
        identifiers( cursRef.id ).value := identifiers( cursRef.id ).sStorage.value'access;
        identifiers( cursRef.id ).resource := true;
     end if;
  else
     ParseInOutInstantiatedParameter( cursRef.id , vectors_cursor_t );
     -- Check the type against the vector
     if not error_found then
        vectorItemIndicesOk( vectors_insert_vector_and_mark_t, cursRef.id, vectorId );
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

        declareResource( resId, vector_storage_list_cursor, getIdentifierBlock( cursRef.id ) );
        identifiers( cursRef.id ).sStorage.metaLabel := noMetaLabel;
        identifiers( cursRef.id ).sStorage.value := to_unbounded_string( resId );
        identifiers( cursRef.id ).value := identifiers( cursRef.id ).sStorage.value'access;
        identifiers( cursRef.id ).resource := true;
     end if;
  else
     ParseInOutInstantiatedParameter( cursRef.id , vectors_cursor_t );
     -- Check the type against the vector
     if not error_found then
        vectorItemIndicesOk( vectors_insert_vector_and_mark_t, cursRef.id, vectorId );
        genTypesOk( identifiers( vectorId ).genKind2, identifiers( cursRef.id ).genKind2 );
     end if;
  end if;
  expectParameterClose( subprogram );
end ParseLastOutVectorCursor;


------------------------------------------------------------------------------
--  TO REAL VECTOR INDEX
--
-- The vector index is really a long_integer to account for No_Index, otherwise
-- is an integer.  We have to map the SparForte index to an integer to access
-- the Ada vector.
------------------------------------------------------------------------------

function toRealVectorIndex( subId : identifier; vectorId : identifier; UserIdx : long_integer ) return vector_index is
   kind  : identifier;
   baseKind : identifier;
   uniKind : identifier;
   convertedIdx : Vector_Storage_Lists.Extended_Index; -- vector_index;
   theVector : resPtr;
begin
   kind := identifiers( vectorId ).genKind;
   uniKind := getUniType( kind );
   findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );

   -- the index type is either a numeric or an enumerated
   --
   -- for a numeric we need to know the subrange.  For a type with no
   -- subrange, assume it starts at integer'first.  Remember that the
   -- base type of natural will be integer, not natural.
   -- For No_Index, it is outside of a user's index.  Do not remap it.

   begin
      if uniKind = uni_numeric_t then
         baseKind := getBaseType( kind );
         if UserIdx = User_No_Index then
            err( context => subId,
                 subjectNotes => +"the index position No_Index",
                 reason   => +"is not in the vector",
                 obstructorNotes => nullMessageStrings
            );
            convertedIdx := Vector_Storage_Lists.No_Index;
         elsif kind = positive_t or baseKind = positive_t then
            convertedIdx := Vector_Storage_Lists.Extended_Index( UserIdx-1 );
         elsif kind = natural_t or baseKind = natural_t then
            convertedIdx := Vector_Storage_Lists.Extended_Index( UserIdx );
         elsif kind = integer_t or baseKind = integer_t then
            convertedIdx := Vector_Storage_Lists.Extended_Index(
               UserIdx-long_integer( integer'first ) );
         else
            err(
                contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while in " ) & em( to_string( identifiers( subId ).name ) ),
                subjectNotes => subjectInterpreter,
                reason => +"had an internal error because",
                obstructorNotes => pl( "there is an unsupported index type" )
            );
         end if;
      elsif uniKind = uni_string_t then
         err(
             contextNotes => pl( "At " & gnat.source_info.source_location &
              " while in " ) & em( to_string( identifiers( subId ).name ) ),
             subjectNotes => subjectInterpreter,
             reason => +"had an internal error because",
             obstructorNotes => pl( "the vector index should not be a string" )
         );
      else
         -- for an enumerated, asuume it starts at zero
         convertedIdx := vector_index( UserIdx );
      end if;
   exception when constraint_error =>
      err( context => subId,
           subjectNotes => +"the user index position" & pl( UserIdx'img ),
           reason   => +"is not in the vector",
           obstructorNotes => +"range for " &
              unb_em( identifiers( kind ).name )
      );
   when others =>
      err_exception_raised;
   end;
   return convertedIdx;
end toRealVectorIndex;


------------------------------------------------------------------------------
--  TO USER VECTOR INDEX
--
-- Inverse operation for To Real Vector Index.  User index must be a
-- long_integer to account for integer range plus the No_Index value.
------------------------------------------------------------------------------

function toUserVectorIndex( subId : identifier; vectorId : identifier;
     realIdx : Vector_Storage_Lists.Extended_Index ) return long_integer is
   kind  : identifier;
   baseKind : identifier;
   uniKind : identifier;
   convertedIdx : long_integer;
   theVector : resPtr;
begin
   kind  := identifiers( vectorId ).genKind;
   unikind := getUniType( kind );
   findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
   begin
      -- the index type is either a numeric or an enumerated
      if unikind = uni_numeric_t then
         -- For a numeric we need to know the subrange.  For a type with no
         -- subrange, assume it starts at integer'first
         -- If the index is No_Index, it will be out of the vector's index
         -- range.  Do not attempt to remap it to an integer because it will
         -- raise a constraint_error.
         baseKind := getBaseType( kind );
         if realIdx = Vector_Storage_Lists.No_Index then
            convertedIdx := User_No_Index;
         elsif kind = positive_t or baseKind = positive_t then
            convertedIdx := long_integer( realIdx )+1;
         elsif kind = natural_t or baseKind = natural_t then
            convertedIdx := long_integer( realIdx );
         elsif kind = integer_t or baseKind = integer_t then
            convertedIdx := long_integer( realIdx ) + long_integer( integer'first );
         else
            err(
                contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while in " ) & em( to_string( identifiers( subId ).name ) ),
                subjectNotes => subjectInterpreter,
                reason => +"had an internal error because",
                obstructorNotes => pl( "there is an unsupported index type" )
            );
         end if;
      elsif uniKind = uni_string_t then
         err(
             contextNotes => pl( "At " & gnat.source_info.source_location &
              " while in " ) & em( to_string( identifiers( subId ).name ) ),
             subjectNotes => subjectInterpreter,
             reason => +"had an internal error because",
             obstructorNotes => pl( "the vector index should not be a string" )
         );
      else
         -- for an enumerated, asuume it starts at zero
         convertedIdx := long_integer( realIdx );
      end if;
   exception when constraint_error =>
      err( context => subId,
           subjectNotes => +"the Ada index position " & pl( realIdx'img ),
           reason   => +"does not map to a vector index position",
           obstructorNotes => +"range" &
              pl( Vector_Storage_Lists.First_Index( theVector.vslVector )'img ) &
              pl( ".." ) &
              pl( Vector_Storage_Lists.Last_Index( theVector.vslVector )'img )
      );
   when others =>
      err_exception_raised;
   end;
   return convertedIdx;
end toUserVectorIndex;


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
-- Delete the contents of the vector.
------------------------------------------------------------------------------

procedure ParseVectorsClear is
  vectorId   : identifier;
  theVector  : resPtr;
  subprogramId : constant identifier := vectors_clear_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_Storage_Lists.Clear( theVector.vslVector );
     end;
  end if;
end ParseVectorsClear;


------------------------------------------------------------------------------
--  TO VECTOR
--
-- Syntax: vectors.to_vector( v, e, n );
-- Ada:    v := vectors.to_vector( [e, ] n );
-- Create a vector out of the element. A count of zero inserts nothing.
------------------------------------------------------------------------------

procedure ParseVectorsToVector is
  vectorId   : identifier;
  theVector  : resPtr;
  itemExpr   : storage;
  itemType   : identifier;
  cntExpr    : storage;
  cntType    : identifier;
  subprogramId : constant identifier := vectors_to_vector_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( subprogramId, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       if metaLabelOk( itemExpr ) then
          theVector.vslVector := Vector_Storage_Lists.To_Vector( itemExpr,
             ada.containers.count_type'value( to_string( cntExpr.value ) ) );
       end if;
     end;
  end if;
end ParseVectorsToVector;


------------------------------------------------------------------------------
--  CAPACITY
--
-- Syntax: c := capacity( v );
-- Ada:    c := capacity( v );
-- Return the spare element capacity of vector v. This is the number of
-- elements that can be inserted before the vector memory grows.
------------------------------------------------------------------------------

procedure ParseVectorsCapacity( result : out storage; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
  subprogramId : constant identifier := vectors_capacity_t;
begin
  kind := containers_count_type_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := storage'(
         to_unbounded_string(
            ada.containers.count_type'image( Vector_Storage_Lists.Capacity( theVector.vslVector ) )
         ),
         noMetaLabel
      );
     end;
  end if;
end ParseVectorsCapacity;


------------------------------------------------------------------------------
--  RESERVE CAPACITY
--
-- Syntax: reserve_capacity( v, n );
-- Ada:    reserve_capacity( v, n );
-- Add enough memory to the vector to handle at least n items.
------------------------------------------------------------------------------

procedure ParseVectorsReserveCapacity is
  vectorId   : identifier;
  theVector  : resPtr;
  cntExpr    : storage;
  cntType    : identifier;
  subprogramId : constant identifier := vectors_reserve_capacity_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
  if isExecutingCommand then
     declare
       cnt : ada.containers.count_type;
     begin
       cnt := ada.containers.count_type( to_numeric( cntExpr.value ) );
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_Storage_Lists.Reserve_Capacity( theVector.vslVector, cnt );
     exception when constraint_error =>
       -- e.g. user gave "-1" for what is a natural type
       err( context => subprogramId,
            subjectNotes => pl( qp( "the capacity count value of " ) & toSecureData( to_string( cntExpr.value ) ) ),
            subjectType  => cntType,
            reason => +"is not valid for",
            obstructor => containers_count_type_t,
            remedy => +"the value should be >= 0" );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsReserveCapacity;


------------------------------------------------------------------------------
--  LENGTH
--
-- Syntax: c := length( v );
-- Ada:    c := length( v );
-- Return the number of elements in the vector.
------------------------------------------------------------------------------

procedure ParseVectorsLength( result : out storage; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
  subprogramId : constant identifier := vectors_length_t;
begin
  kind := containers_count_type_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := storage'(
         to_unbounded_string( ada.containers.count_type'image( Vector_Storage_Lists.Length( theVector.vslVector ) ) ),
         noMetaLabel
      );
     end;
  end if;
end ParseVectorsLength;


------------------------------------------------------------------------------
--  SET LENGTH
--
-- Syntax: set_length( v, n );
-- Ada:    set_length( v, n );
-- Resize the vector to n items, removing elements or adding empty elements if
-- necessary.
------------------------------------------------------------------------------

procedure ParseVectorsSetLength is
  vectorId   : identifier;
  theVector  : resPtr;
  cntExpr    : storage;
  cntType    : identifier;
  subprogramId : constant identifier := vectors_set_length_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
  if isExecutingCommand then
     declare
       cnt : ada.containers.count_type;
     begin
       cnt := ada.containers.count_type( to_numeric( cntExpr.value ) );
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_Storage_Lists.Set_Length( theVector.vslVector, cnt );
     exception when constraint_error =>
       -- e.g. user gave "-1" for what is a natural type
       err( context => subprogramId,
            subjectNotes => pl( qp( "the capacity count value of " ) & toSecureData( to_string( cntExpr.value ) ) ),
            subjectType  => cntType,
            reason => +"is not valid for",
            obstructor => containers_count_type_t,
            remedy => +"the value should be >= 0" );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsSetLength;


------------------------------------------------------------------------------
--  IS EMPTY
--
-- Syntax: b := is_empty( v );
-- Ada:    b := is_empty( v );
-- True if the vector has no items.
------------------------------------------------------------------------------

procedure ParseVectorsIsEmpty( result : out storage; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
  subprogramId : constant identifier := vectors_is_empty_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := storage'( to_spar_boolean( Vector_Storage_Lists.Is_Empty( theVector.vslVector ) ), noMetaLabel );
     end;
  end if;
end ParseVectorsIsEmpty;


------------------------------------------------------------------------------
--  APPEND ELEMENTS
--
-- Syntax: vectors.append_elements( v, e, [n] );
-- Ada:    vectors.append_elements( v, e, [n] );
-- Queue list element e at the end of the vector v. If n is used, append n
-- copies of e.
------------------------------------------------------------------------------

procedure ParseVectorsAppendElements is
  vectorId  : identifier;
  theVector : resPtr;
  itemExpr  : storage;
  itemType  : identifier;
  cntExpr   : storage;
  cntType   : identifier;
  hasCnt    : boolean := false;
  subprogramId : constant identifier := vectors_append_elements_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( subprogramId, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expectParameterClose( subprogramId );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          if metaLabelOk( itemExpr ) then
             Vector_Storage_Lists.Append( theVector.vslVector, itemExpr, cnt );
          end if;
       else
          if metaLabelOk( itemExpr ) then
             Vector_Storage_Lists.Append( theVector.vslVector, itemExpr );
          end if;
       end if;
     exception when constraint_error =>
       err_count( subprogramId, cntExpr, cntType );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsAppendElements;


------------------------------------------------------------------------------
--  PREPEND ELEMENTS
--
-- Syntax: vectors.prepend_elements( v, e, [n] );
-- Ada:    vectors.prepend_elements( v, e, [n] );
-- Queue list element e at the start of the vector v. If n is used, append
-- c copies of e.
------------------------------------------------------------------------------

procedure ParseVectorsPrependElements is
  vectorId  : identifier;
  theVector : resPtr;
  itemExpr  : storage;
  itemType  : identifier;
  cntExpr   : storage;
  cntType   : identifier;
  hasCnt    : boolean := false;
  subprogramId : constant identifier := vectors_prepend_elements_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( subprogramId, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expectParameterClose( subprogramId );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          if metaLabelOk( itemExpr ) then
             Vector_Storage_Lists.Prepend( theVector.vslVector, itemExpr, cnt );
          end if;
       else
          if metaLabelOk( itemExpr ) then
             Vector_Storage_Lists.Prepend( theVector.vslVector, itemExpr );
          end if;
       end if;
     exception when constraint_error =>
       err_count( subprogramId, cntExpr, cntType );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsPrependElements;


------------------------------------------------------------------------------
--  APPEND
--
-- Syntax: vectors.append( v, i, e );
-- Ada:    N/A
-- Append string element e to the value under index i in the vector. If the key
-- does not exist, raise an exception.
------------------------------------------------------------------------------
-- TODO: support cursors
-- TODO: subject defaults to current token?

procedure ParseVectorsAppend is
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : storage;
  idxType   : identifier;
  strExpr   : storage;
  strType   : identifier;
  subprogramId : constant identifier := vectors_append_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( subprogramId, idxExpr, idxType, identifiers( vectorId ).genKind );
  ParseLastStringParameter( subprogramId, strExpr, strType, identifiers( vectorId ).genKind2 );
  if isExecutingCommand then
     declare
       idx : vector_index;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       idx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( idxExpr.value ) ) );
       -- Append( theVector.vslVector, idx, strExpr );
       declare
         the_string : storage;
       begin
         the_string := Vector_Storage_Lists.Element( theVector.vslVector, idx );
         if metaLabelOK( the_string, strExpr ) then
            the_string.value := the_string.value & strExpr.value;
            Vector_Storage_Lists.Replace_Element( theVector.vslVector, idx, the_string );
         end if;
       end;
     exception when constraint_error =>
       err( +"append index is wrong type" );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsAppend;


------------------------------------------------------------------------------
--  PREPEND
--
-- Syntax: vectors.prepend( v, i, e );
-- Ada:    N/A
-- Prepend string element e to the value under index i in the vector. If the key
-- does not exist, raise an exception.
------------------------------------------------------------------------------

procedure ParseVectorsPrepend is
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : storage;
  idxType   : identifier;
  strExpr   : storage;
  strType   : identifier;
  subprogramId : constant identifier := vectors_prepend_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( subprogramId, idxExpr, idxType, identifiers( vectorId ).genKind );
  ParseLastStringParameter( subprogramId, strExpr, strType, identifiers( vectorId ).genKind2 );
  if isExecutingCommand then
     declare
       idx : vector_index;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       idx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( idxExpr.value ) ) );
       -- Prepend( theVector.vslVector, idx, strExpr );
       declare
         the_string : storage;
       begin
         the_string := Vector_Storage_Lists.Element( theVector.vslVector, idx );
         if metaLabelOK( the_string, strExpr ) then
            the_string.value := strExpr.value & the_string.value;
            Vector_Storage_Lists.Replace_Element( theVector.vslVector, idx, the_string );
         end if;
       end;
     exception when constraint_error =>
       err( +"prepend count must be a natural integer" );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsPrepend;


------------------------------------------------------------------------------
--  FIRST INDEX
--
-- Syntax: n := vectors.first_index( v );
-- Ada:    n := vectors.first_index( v );
------------------------------------------------------------------------------

procedure ParseVectorsFirstIndex( result : out storage; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
  userIdx    : Vector_Storage_Lists.Extended_Index;
  subprogramId : constant identifier := vectors_first_index_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  --kind := identifiers( vectorId ).genKind;
  kind := long_integer_t;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       userIdx := Vector_Storage_Lists.First_Index( theVector.vslVector );
       result := storage'( to_unbounded_string( long_integer'image(
            toUserVectorIndex( subprogramId, vectorId, userIdx ) ) ),
         noMetaLabel );
     end;
  end if;
end ParseVectorsFirstIndex;


------------------------------------------------------------------------------
--  LAST INDEX
--
-- Syntax: i := vectors.last_index( v );
-- Ada:    i := vectors.last_index( v );
------------------------------------------------------------------------------

procedure ParseVectorsLastIndex( result : out storage; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
  userIdx    : Vector_Storage_Lists.Extended_Index;
  subprogramId : constant identifier := vectors_last_index_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  --kind := identifiers( vectorId ).genKind;
  kind := long_integer_t;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       userIdx := Vector_Storage_Lists.Last_Index( theVector.vslVector );
       result := storage'( to_unbounded_string( long_integer'image( toUserVectorIndex( subprogramId, vectorId, userIdx ) ) ), noMetaLabel );
     end;
  end if;
end ParseVectorsLastIndex;


------------------------------------------------------------------------------
--  ELEMENT
--
-- Syntax: e := vectors.element( c ) | ( v, i )
-- Ada:    e := vectors.element( c ) | ( v, i )
------------------------------------------------------------------------------

procedure ParseVectorsElement( result : out storage; kind : out identifier ) is
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : storage;
  idxType   : identifier;
  cursorId  : identifier := eof_t;
  theCursor : resPtr;
  oldElem   : storage;
  subprogramId : constant identifier := vectors_element_t;
begin
  expect( subprogramId );
  -- A cursor is a single identifier.  An index is an expression.
  expectParameterOpen( subprogramId );
  if identifiers( token ).class /= varClass then
     err( context => subprogramId,
          subject => token,
          subjectType => identifiers( token ).kind,
          obstructorNotes => em( "vectors.vector" ) & pl( " or " ) &
             em( "vectors.cursor" ),
          reason => +"is not compatible with the expected types"
     );
  elsif identifiers( token ).kind = new_t then
    -- since we are skipping the usual methods and looking at the token
    -- directly, force an error on an undeclared identifier
    declare
       discardId : identifier;
    begin
       ParseIdentifier( discardId );
    end;
  elsif getUniType( identifiers( token ).kind ) = vectors_cursor_t then
     ParseInOutInstantiatedParameter( cursorId, vectors_cursor_t );
  elsif getUniType( identifiers( token ).kind ) = vectors_vector_t then
     ParseInOutInstantiatedParameter( vectorId, vectors_vector_t );
     ParseNextGenItemParameter( subprogramId, idxExpr, idxType, identifiers( vectorId ).genKind );
  else
     err( context => subprogramId,
          subject => token,
          subjectType => identifiers( token ).kind,
          obstructorNotes => em( "vectors.vector" ) & pl( " or " ) &
             em( "vectors.cursor" ),
          reason => +"is not compatible with the expected types"
     );
  end if;
  expectParameterClose( subprogramId );

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
         oldElem := Vector_Storage_Lists.Element( theCursor.vslCursor );
         if metaLabelOk( oldElem ) then
            result := oldElem;
         end if;
       else
         findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
         --idx := vector_index( to_numeric( idxExpr ) );
         idx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( idxExpr.value ) ) );
         oldElem := Vector_Storage_Lists.Element( theVector.vslVector, idx );
         if metaLabelOk( oldElem ) then
            result := oldElem;
         end if;
       end if;
-- NOTE: Vector Lists stores internally a natural
     exception when constraint_error =>
       err_index( subprogramId, idxExpr );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsElement;


------------------------------------------------------------------------------
--  FIRST ELEMENT
--
-- Syntax: e := vectors.first_element( v );
-- Ada:    e := vectors.first_element( v );
------------------------------------------------------------------------------

procedure ParseVectorsFirstElement( result : out storage; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
  oldElem    : storage;
  subprogramId : constant identifier := vectors_first_element_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  kind := identifiers( vectorId ).genKind2;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       oldElem := Vector_Storage_Lists.First_Element( theVector.vslVector );
       if metaLabelOk( oldElem ) then
          result := oldElem;
       end if;
     exception when constraint_error =>
       err_empty( subprogramId, vectorId );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsFirstElement;


------------------------------------------------------------------------------
--  LAST ELEMENT
--
-- Syntax: e := vectors.last_element( v );
-- Ada:    e := vectors.last_element( v );
------------------------------------------------------------------------------

procedure ParseVectorsLastElement( result : out storage; kind : out identifier ) is
  vectorId   : identifier;
  theVector  : resPtr;
  oldElem    : storage;
  subprogramId : constant identifier := vectors_last_element_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  kind := identifiers( vectorId ).genKind2;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       oldElem := Vector_Storage_Lists.Last_Element( theVector.vslVector );
       if metaLabelOk( oldElem ) then
          result := oldElem;
       end if;
     exception when constraint_error =>
       err_empty( subprogramId, vectorId );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsLastElement;


------------------------------------------------------------------------------
--  DELETE FIRST
--
-- Syntax: vectors.delete_first( v [,n] )
-- Ada:    vectors.delete_first( v [,n] )
-- Remove an element from the start of the vector. If a count n is given,
-- delete n elements.
------------------------------------------------------------------------------

procedure ParseVectorsDeleteFirst is
  vectorId  : identifier;
  theVector : resPtr;
  cntExpr   : storage;
  cntType   : identifier;
  hasCnt    : boolean := false;
  subprogramId : constant identifier := vectors_delete_first_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expectParameterClose( subprogramId );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          -- Although delete_first has a count parameter, we cannot use it.
          -- Different elements may have different tags associated with the
          -- values and we have to test each element separately.
          for i in 1..cnt loop
             -- Note: check-before-use.  This will be an issue if concurrency is
             -- introduced.
             if metaLabelOK( Vector_Storage_Lists.First_Element( theVector.vslVector ) ) then
                Vector_Storage_Lists.Delete_First( theVector.vslVector, cnt );
             end if;
          end loop;
       else
          if metaLabelOK( Vector_Storage_Lists.First_Element( theVector.vslVector ) ) then
             Vector_Storage_Lists.Delete_First( theVector.vslVector );
          end if;
       end if;
     exception when constraint_error =>
       err_count( subprogramId, cntExpr, cntType );
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
-- Syntax: vectors.delete_last( v [,n] )
-- Ada:    vectors.delete_last( v [,n] )
-- Remove an element from the end of the vector. If a count n is given,
-- delete n elements.
------------------------------------------------------------------------------

procedure ParseVectorsDeleteLast is
  vectorId  : identifier;
  theVector : resPtr;
  cntExpr   : storage;
  cntType   : identifier;
  hasCnt    : boolean := false;
  subprogramId : constant identifier := vectors_delete_last_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expectParameterClose( subprogramId );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          -- Although delete_first has a count parameter, we cannot use it.
          -- Different elements may have different tags associated with the
          -- values and we have to test each element separately.
          for i in 1..cnt loop
             -- Note: check-before-use.  This will be an issue if concurrency is
             -- introduced.
             if metaLabelOK( Vector_Storage_Lists.Last_Element( theVector.vslVector ) ) then
                Vector_Storage_Lists.Delete_Last( theVector.vslVector, cnt );
             end if;
          end loop;
       else
          if metaLabelOK( Vector_Storage_Lists.Last_Element( theVector.vslVector ) ) then
             Vector_Storage_Lists.Delete_Last( theVector.vslVector );
          end if;
       end if;
     exception when constraint_error =>
       err_count( subprogramId, cntExpr, cntType );
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
-- Return true if the vector contains vector element e.
------------------------------------------------------------------------------

procedure ParseVectorsContains( result : out storage; kind : out identifier ) is
  vectorId  : identifier;
  theVector : resPtr;
  itemExpr  : storage;
  itemType  : identifier;
  subprogramId : constant identifier := vectors_contains_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseLastGenItemParameter( subprogramId, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := storage'( to_spar_boolean( Vector_Storage_Lists.Contains( theVector.vslVector, itemExpr ) ), noMetaLabel );
     end;
  end if;
end ParseVectorsContains;


------------------------------------------------------------------------------
--  MOVE
--
-- Syntax: vectors.move( v1, v2 );
-- Ada:    vectors.move( v1, v2 );
-- Overwrite the contents of vector v1 with a copy of v2 and erase v2.
------------------------------------------------------------------------------

procedure ParseVectorsMove is
  sourceVectorId   : identifier;
  theSourceVector  : resPtr;
  targetVectorId   : identifier;
  theTargetVector  : resPtr;
  subprogramId : constant identifier := vectors_move_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, sourceVectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( subprogramId, targetVectorId, vectors_vector_t );
  vectorItemIndicesOk( subprogramId, sourceVectorId, targetVectorid );
  genTypesOk( identifiers( targetVectorId ).genKind2, identifiers( sourceVectorId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetVectorId ).value.all ), theTargetVector );
       findResource( to_resource_id( identifiers( sourceVectorId ).value.all ), theSourceVector );
       Vector_Storage_Lists.Move( theTargetVector.vslVector, theSourceVector.vslVector );
     end;
  end if;
end ParseVectorsMove;


------------------------------------------------------------------------------
--  REVERSE ELEMENTS
--
-- Syntax: vectors.reverse_elements( v );
-- Ada:    vectors.reverse_elements( v );
-- Reverse the elements of the vector.
------------------------------------------------------------------------------

procedure ParseVectorsReverseElements is
  vectorId  : identifier;
  theVector : resPtr;
  subprogramId : constant identifier := vectors_reverse_elements_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_Storage_Lists.Reverse_Elements( theVector.vslVector );
     end;
  end if;
end ParseVectorsReverseElements;


------------------------------------------------------------------------------
--  FLIP
--
-- Syntax: vectors.flip( v );
-- Ada:    N/A
-- Reverse the elements of the vector. A renaming of vectors.reverse_elements.
------------------------------------------------------------------------------

procedure ParseVectorsFlip is
  vectorId  : identifier;
  theVector : resPtr;
  subprogramId : constant identifier := vectors_flip_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use reverse_elements" );
  ParseSingleInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_Storage_Lists.Reverse_Elements( theVector.vslVector );
     end;
  end if;
end ParseVectorsFlip;


------------------------------------------------------------------------------
--  FIRST
--
-- Syntax: vectors.first( v, c );
-- Ada:    c := vectors.first( v );
-- Move the cursor to the first element in the vectors.
------------------------------------------------------------------------------

procedure ParseVectorsFirst is
  vectorId   : identifier;
  theVector  : resPtr;
  theCursor  : resPtr;
  --cursRef    : reference;
  cursorId   : identifier;
  subprogramId : constant identifier := vectors_first_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( subprogramId, cursorId , vectors_cursor_t );

  -- Check the type against the vector

  if not error_found then
     vectorItemIndicesOk( subprogramId, vectorId, cursorId );
     genElementsOk( subprogramId, vectorId, cursorId, identifiers( vectorId ).genKind2, identifiers( cursorId ).genKind2 );
     -- genTypesOk( identifiers( vectorId ).genKind2, identifiers( cursorId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       theCursor.vslCursor := Vector_Storage_Lists.First( theVector.vslVector );
     end;
  end if;
end ParseVectorsFirst;


------------------------------------------------------------------------------
--  LAST
--
-- Syntax: vectors.last( v, c );
-- Ada:    c := vectors.last( v );
-- Move the cursor to the last element in the vector.
------------------------------------------------------------------------------

procedure ParseVectorsLast is
  vectorId   : identifier;
  theVector  : resPtr;
  cursorId   : identifier;
  theCursor  : resPtr;
  subprogramId : constant identifier := vectors_last_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( subprogramId, cursorId, vectors_cursor_t );
  -- Check the type against the vector
  if not error_found then
     vectorItemIndicesOk( subprogramId, vectorid, cursorId );
     genTypesOk( identifiers( vectorId ).genKind2, identifiers( cursorId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       theCursor.vslCursor := Vector_Storage_Lists.Last( theVector.vslVector );
     end;
  end if;
end ParseVectorsLast;


------------------------------------------------------------------------------
--  NEXT
--
-- Syntax: vectors.next( c );
-- Ada:    vectors.next( c );
-- Move the cursor to the next element in the vector. If there are no more
-- items, do nothing.
------------------------------------------------------------------------------

procedure ParseVectorsNext is
  cursId    : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := vectors_next_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, cursId, vectors_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).value.all ), theCursor );
       Vector_Storage_Lists.Next( theCursor.vslCursor );
     end;
  end if;
end ParseVectorsNext;


------------------------------------------------------------------------------
--  PREVIOUS
--
-- Syntax: vectors.previous( c );
-- Ada:    vectors.previous( c );
-- Move the cursor to the previous element in the vector. If there are no more
-- items, do nothing.
------------------------------------------------------------------------------

procedure ParseVectorsPrevious is
  cursId    : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := vectors_previous_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, cursId, vectors_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).value.all ), theCursor );
       Vector_Storage_Lists.Previous( theCursor.vslCursor );
     end;
  end if;
end ParseVectorsPrevious;


------------------------------------------------------------------------------
--  DELETE
--
-- Syntax: vectors.delete( v, c ) | ( v, i [, n] )
-- Ada:    vectors.delete( v, c | i [, n] )
-- Remove an element from the vector at the index position, effectively sliding
-- the elements toward the start of the vector by one position. If a cursor is
-- given instead of an index, delete the item at the cursor position. If a
-- count of n is given, delete n elements instead of one.
------------------------------------------------------------------------------

procedure ParseVectorsDelete is
  vectorId  : identifier;
  theVector : resPtr;
  cursorId  : identifier;
  theCursor : resPtr;
  idxExpr   : storage;
  idxType   : identifier;
  hasIdx    : boolean := false;
  idx       : Vector_Storage_Lists.Extended_Index;
  cntExpr   : storage;
  cntType   : identifier;
  hasCnt    : boolean := false;
  cnt       : Ada.Containers.Count_Type;
  oldElem   : storage;
  subprogramId : constant identifier := vectors_delete_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  expectParameterComma( subprogramId );
  if identifiers( token ).class = varClass and then
     getUniType( identifiers( token ).kind ) = vectors_cursor_t then
     ParseInOutInstantiatedParameter( cursorId, vectors_cursor_t );
  else
     ParseGenItemParameter( idxExpr, idxType, identifiers( vectorId ).genKind );
     -- This will not stronly match natural, positive and integer, as these
     -- subtypes.  But it will detect new data types derived from integer.
     --ParseExpression( idxExpr, idxType );
     --vectorIndexOk( subprogramId, idxType, vectorId );
     --baseTypesOK( idxType, identifiers( vectorId ).genKind );
     hasIdx := true;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseNextNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  end if;
  expectParameterClose( subprogramId );
  if isExecutingCommand then
     begin
        findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
        if hasIdx then
           if hasCnt then
              -- TODO: shouldn't the account be rounded on a universal numeric?  Check casting,
              -- here and elsewhere.
              cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
              idx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( idxExpr.value ) ) );
              for i in 1..cnt loop
                  oldElem := Vector_Storage_Lists.Element( theVector.vslVector, idx );
                  if metaLabelOk( oldElem ) then
                     vector_Storage_Lists.Delete( theVector.vslVector, idx );
                  end if;
              end loop;
              -- Vector_Storage_Lists.Delete( theVector.vslVector, idx, cnt );
           else
              idx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( idxExpr.value ) ) );
              oldElem := Vector_Storage_Lists.Element( theVector.vslVector, idx );
              if metaLabelOk( oldElem ) then
                 vector_Storage_Lists.Delete( theVector.vslVector, idx );
              end if;
           end if;
        else
           findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
           if hasCnt then
              -- the problem with a count and a cursor is that the cursor becomes
              -- invalid when the item beneath it is deleted
              err( context => subprogramId,
                   subjectNotes => pl( qp( "the count value" ) ),
                   subjectType  => cntType,
                   reason => +"cannot be used with with a cursor because",
                   obstructorNotes => +"of SparForte's meta data label implementation",
                   remedy => +"reposition the cursor and delete each item"
              );
              --begin
              --   cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
              --exception when constraint_error =>
              --   err_count( subprogramId, cntExpr, cntType );
              --   cnt := 0;
              --end;
              
              --declare
              --   nextCursor : Vector_Storage_Lists.cursor;
              --begin
              --for i in 1..cnt loop
              --    oldElem := Vector_Storage_Lists.Element( theCursor.vslCursor );
              --    if metaLabelOk( oldElem ) then
              --       nextCursor := Vector_Storage_Lists.Next( theCursor.vslCursor );
              --       Vector_Storage_Lists.Delete( theVector.vslVector, theCursor.vslCursor );
              --       theCursor.vslCursor := nextCursor;
              --    else
              --       exit;
              --    end if;
              --end loop;
              --end;
              -- Vector_Storage_Lists.Delete( theVector.vslVector, theCursor.vslCursor, cnt );
           else
              oldElem := Vector_Storage_Lists.Element( theVector.vslVector, idx );
              if metaLabelOk( oldElem ) then
                 Vector_Storage_Lists.Delete( theVector.vslVector, theCursor.vslCursor );
              end if;
           end if;
        end if;
     exception when storage_error =>
       err_storage;
     when program_error =>
       err_cursor_mismatch;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsDelete;


------------------------------------------------------------------------------
--  HAS ELEMENT
--
-- Syntax: b := has_element( c );
-- Ada:    b := has_element( c );
------------------------------------------------------------------------------

procedure ParseVectorsHasElement( result : out storage; kind : out identifier ) is
  cursorId   : identifier;
  theCursor  : resPtr;
  subprogramId : constant identifier := vectors_has_element_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, cursorId, vectors_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       result := storage'( to_spar_boolean( Vector_Storage_Lists.Has_Element( theCursor.vslCursor ) ), noMetaLabel );
     end;
  end if;
end ParseVectorsHasElement;


------------------------------------------------------------------------------
--  EQUAL
--
-- Syntax: b := equal( v1, v2 );
-- Ada:    b := v1 = v2;
------------------------------------------------------------------------------

procedure ParseVectorsEqual( result : out storage; kind : out identifier ) is
  leftVectorId  : identifier;
  rightVectorId : identifier;
  leftVector    : resPtr;
  rightVector   : resPtr;
  use Vector_Storage_Lists;
  subprogramId : constant identifier := vectors_equal_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, leftVectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( subprogramId, rightVectorId, vectors_vector_t );
  if not error_found then
     vectorItemIndicesOk( vectors_insert_before_t, leftVectorId, rightVectorId );
     genElementsOk( vectors_insert_before_t, leftVectorId, rightVectorId,
        identifiers( leftVectorId ).genKind2, identifiers( rightVectorId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( leftVectorId ).value.all ), leftVector );
       findResource( to_resource_id( identifiers( rightVectorId ).value.all ), rightVector );
       result := storage'( to_spar_boolean( leftVector.vslVector = rightVector.vslVector ), noMetaLabel );
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
-- Syntax: vectors.insert( v, i, e [, b] )
-- Ada:    vectors.insert
--
-- Insert element e into vector v before index i. If a count n is used, insert
-- n copies of the element. A count of zero inserts nothing.
-- Note: "Insert" is a reserved word in SparForte.
------------------------------------------------------------------------------

procedure ParseVectorsInsert is
  vectorId   : identifier;
  theVector  : resPtr;
  beforeExpr  : storage;
  beforeType : identifier;
  elemExpr    : storage;
  elemType   : identifier;
  cntExpr    : storage;
  cntType    : identifier;
  hasCnt     : boolean := false;
  subprogramId : constant identifier := vectors_insert_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextNumericParameter( subprogramId, beforeExpr, beforeType, identifiers( vectorId ).genKind );

  -- In Ada, the element can be missing, the count can be missing, or both.
  -- If the count type and element type are the same, SparForte cannot
  -- distinguish between them.  So we don't permit the element to be
  -- absent.

  --if token = symbol_t and identifiers( token ).value.all = "," then
     expectParameterComma( subprogramId );
     ParseExpression( elemExpr, elemType );
     if type_checks_done or else baseTypesOk( elemType, identifiers( vectorId ).genKind2 ) then
        if token = symbol_t and identifiers( token ).value.all = "," then
           ParseNextNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
           hasCnt := true;
        end if;
     end if;
  --end if;
  expectParameterClose( subprogramId );

  if isExecutingCommand then
     declare
       idx : vector_index;
       cnt : ada.containers.count_type := 1;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       idx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( beforeExpr.value ) ) );
       if hasCnt then
          begin
             cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          exception when others =>
             err_count( subprogramId, cntExpr, cntType );
          end;
          if metaLabelOk( elemExpr ) then
             Vector_Storage_Lists.Insert( theVector.vslVector, idx, elemExpr, cnt );
          end if;
       else
          if metaLabelOk( elemExpr ) then
             Vector_Storage_Lists.Insert( theVector.vslVector, idx, elemExpr );
          end if;
       end if;
     exception when constraint_error =>
       err_index( subprogramId, beforeExpr );
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
-- Copy the elements of v2 into vector v before cursor c. The second vector is
-- unchanged.
-- Meta data labels are not checked.
------------------------------------------------------------------------------

procedure ParseVectorsInsertVector is
  vectorId   : identifier;
  cursorId   : identifier;
  vector2Id  : identifier;
  subprogramId : constant identifier := vectors_insert_vector_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextInOutInstantiatedParameter( subprogramId, cursorId, vectors_cursor_t );
  ParseLastInOutInstantiatedParameter( subprogramId, vector2Id, vectors_vector_t );

  if not error_found then
     -- both vectors and the cursor must be type checked
     vectorItemIndicesOk( subprogramId, vectorId, cursorId, vector2Id );
     -- TODO: should be a 3-way version of genElementsOk but would require
     -- refactoring of uniTypesOk also.
     genElementsOk( subprogramId, vectorId, cursorId,
        identifiers( vectorId ).genKind2, identifiers( cursorId ).genKind2 );
     genElementsOk( subprogramId, cursorId, vectorId,
        identifiers( cursorId ).genKind2, identifiers( vectorId ).genKind2 );
     genElementsOk( subprogramId, vectorId, vector2Id,
        identifiers( vectorId ).genKind2, identifiers( vector2Id ).genKind2 );
  end if;

  if isExecutingCommand then
     declare
       theVector  : resPtr;
       theCursor  : resPtr;
       theVector2 : resPtr;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       findResource( to_resource_id( identifiers( vector2Id ).value.all ), theVector2 );
       Vector_Storage_Lists.Insert(
          theVector.vslVector,
          theCursor.vslCursor,
          theVector2.vslVector
       );
     exception when storage_error =>
       err_storage;
     when program_error =>
       err_cursor_mismatch;
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
-- Insert element e into vector v before cursor c. If a count n is used,
-- insert n copies of the element. A count of zero inserts nothing.
------------------------------------------------------------------------------

procedure ParseVectorsInsertBefore is
  vectorId   : identifier;
  cursorId   : identifier;
  elemExpr    : storage;
  elemType   : identifier;
  cntExpr    : storage;
  cntType    : identifier;
  hasCnt     : boolean := false;
  --cursorId2  : identifier;
  subprogramId : constant identifier := vectors_insert_before_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextInOutInstantiatedParameter( subprogramId, cursorId, vectors_cursor_t );
  ParseNextGenItemParameter( subprogramId, elemExpr, elemType, identifiers( vectorId ).genKind2 );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseNextNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  end if;
  if not error_found then
     vectorItemIndicesOk( subprogramId, cursorId, vectorId );
     genElementsOk( subprogramId, vectorId, cursorId,
        identifiers( vectorId ).genKind2, identifiers( cursorId ).genKind2 );
  end if;

  expectParameterClose( subprogramId );

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
             cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          exception when others =>
             err_count( subprogramId, cntExpr, cntType );
          end;
       end if;
       if metaLabelOk( elemExpr ) then
          Vector_Storage_Lists.Insert( theVector.vslVector, theCursor.vslCursor, elemExpr, cnt );
       end if;
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
-- Ada:    insert( v, c, v2, c2 )
-- Insert the elements of v2 into vector v before cursor c.  Return the new
-- position in cursor c2.  The second vector is unchanged.
------------------------------------------------------------------------------

procedure ParseVectorsInsertVectorAndMark is
  vectorId   : identifier;
  cursorId   : identifier;
  vector2Id  : identifier;
  cursor2Ref : reference;
  subprogramId : constant identifier := vectors_insert_vector_and_mark_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextInOutInstantiatedParameter( subprogramId, cursorId, vectors_cursor_t );
  ParseNextInOutInstantiatedParameter( subprogramId, vector2Id, vectors_vector_t );
  ParseLastOutVectorCursor( subprogramId, vectorId, cursor2Ref );

  -- I haven't written a 4-way type test.

  if not error_found then
     -- both vectors and the cursor must be type checked
     vectorItemIndicesOk( subprogramId, vectorId, cursorId, vector2Id );
     -- TODO: should be a 3-way version of genElementsOk but would require
     -- refactoring of uniTypesOk also.
     genElementsOk( subprogramId, vectorId, cursorId,
        identifiers( vectorId ).genKind2, identifiers( cursorId ).genKind2 );
     genElementsOk( subprogramId, cursorId, vectorId,
        identifiers( cursorId ).genKind2, identifiers( vectorId ).genKind2 );
     genElementsOk( subprogramId, vectorId, vector2Id,
        identifiers( vectorId ).genKind2, identifiers( vector2Id ).genKind2 );
     -- the second cursor generic types are checked by last out vector cursor
  end if;

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
             identifiers( cursor2ref.id ).aStorage( cursor2ref.index ).value ),
             theCursor2 );
       else
          findResource( to_resource_id( identifiers( cursor2ref.id ).value.all ), theCursor2 );
       end if;

       -- If the second cursor was auto-declared, the cursor would have been
       -- created by LastOutVectorCursor.

       Vector_Storage_Lists.Insert(
          theVector.vslVector,
          theCursor.vslCursor,
          theVector2.vslVector,
          theCursor2.vslCursor
       );

       -- The resource does not need to be updated.

     exception when storage_error =>
       err_storage;
     when program_error =>
       err_cursor_mismatch;
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
-- Insert element e into vector v before cursor c. Return the position in
-- cursor c2. If a count n is used, insert n copies of the element. A count of
-- zero inserts nothing.
------------------------------------------------------------------------------

procedure ParseVectorsInsertBeforeAndMark is
  vectorId   : identifier;
  cursorId   : identifier;
  elemExpr   : storage;
  elemType   : identifier;
  cursor2Ref : reference;
  cntExpr    : storage;
  cntType    : identifier;
  hasCnt     : boolean := false;
  subprogramId : constant identifier := vectors_insert_before_and_mark_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextInOutInstantiatedParameter( subprogramId, cursorId, vectors_cursor_t );

  expectParameterComma( subprogramId );
  ParseExpression( elemExpr, elemType );
  if type_checks_done or else baseTypesOk( elemType, identifiers( vectorId ).genKind2 ) then
     ParseNextOutVectorCursor( subprogramId, vectorId, cursor2Ref );
     if token = symbol_t and identifiers( token ).value.all = "," then
        ParseNextNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
        hasCnt := true;
     end if;
  end if;

  expectParameterClose( subprogramId );

  -- I haven't written a 4-way type test.

  if not error_found then
     -- the vector, element and cursor must be type checked
     vectorItemIndicesOk( subprogramId, vectorId, cursorId, cursor2Ref.id );
     genElementsOk( subprogramId, vectorId, cursorId,
        identifiers( vectorId ).genKind2, identifiers( cursorId ).genKind2 );
     genTypesOk( elemType, identifiers( vectorId ).genKind2 );
     -- the second cursor generic types are checked by last out vector cursor
  end if;

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
             identifiers( cursor2ref.id ).aStorage( cursor2ref.index ).value ),
             theCursor2 );
       else
          findResource( to_resource_id( identifiers( cursor2ref.id ).value.all ), theCursor2 );
       end if;
       -- TODO: the cursor may not exist

       if hasCnt then
          begin
             cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          exception when others =>
             err_count( subprogramId, cntExpr, cntType );
          end;
       end if;
       if metaLabelOK( elemExpr ) then
          Vector_Storage_Lists.Insert(
             Container => theVector.vslVector,
             Before    => theCursor.vslCursor,
             New_Item  => elemExpr,
             Position  => theCursor2.vslCursor,
             Count     => cnt
          );
       end if;

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
-- Insert empty elements into vector v before index i. If a cursor is used
-- instead, insert the elements before cursor c. Return the position new
-- cursor. If a count n is used, insert n copies of the element. The second
-- vector is unchanged.
-- Does not take into account meta data labels
------------------------------------------------------------------------------

procedure ParseVectorsInsertSpace is
  vectorId   : identifier;
  beforeCursorId : identifier;
  beforeIdxExpr  : storage;
  beforeIdxType  : identifier;
  positionCursorRef : reference;
  cntExpr    : storage;
  cntType    : identifier;
  theVector  : resPtr;
  cnt        : ada.containers.count_type := 1;
  hasIdx     : boolean := false;
  hasCnt     : boolean := false;
  subprogramId : constant identifier := vectors_insert_space_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  expectParameterComma( subprogramId );
  if identifiers( token ).class = varClass and then
     getUniType( identifiers( token ).kind ) = vectors_cursor_t then
     ParseInOutInstantiatedParameter( beforeCursorId, vectors_cursor_t );
     ParseNextOutVectorCursor( subprogramId, vectorId, positionCursorRef );
  else
     ParseExpression( beforeIdxExpr, beforeIdxType );
     vectorIndexOrCursorOk( subprogramId, beforeIdxType, vectorId );
     hasIdx := true;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseNextNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  end if;
  expectParameterClose( subprogramId );

  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );

       -- Either variation can have a count

       if hasCnt then
           begin
              cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
           exception when others =>
             err_count( subprogramId, cntExpr, cntType );
           end;
       end if;

       -- variation 1: with index
       -- variation 2: with cursor and out position cursor

       if hasIdx then
          declare
             idx : vector_index;
          begin
             idx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( beforeIdxExpr.value ) ) );
             Vector_Storage_Lists.Insert_Space( theVector.vslVector, idx, cnt );
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
                 vector_storage_list_cursor,
                 getIdentifierBlock( positionCursorRef.id )
             );
             AssignParameter(
                 positionCursorRef,
                 storage'( to_unbounded_string( positionCursorResourceId ), noMetaLabel )
             );
             findResource( positionCursorResourceId, thePositionCursor );
             Vector_Storage_Lists.Insert_Space(
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
-- Add the contents of vector v2 to the end of vector v1. Vector v2 is
-- unchanged.
-- Does not take into account meta data labels
------------------------------------------------------------------------------

procedure ParseVectorsAppendVector is
  leftVectorId  : identifier;
  rightVectorId : identifier;
  leftVector    : resPtr;
  rightVector   : resPtr;
  use Vector_Storage_Lists;
  subprogramId : constant identifier := vectors_append_vector_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, leftVectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( subprogramId, rightVectorId, vectors_vector_t );
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
-- Exchange two elements in the vector, either by a pair of indexes or a pair
-- of cursors.
------------------------------------------------------------------------------

procedure ParseVectorsSwap is
  vectorId   : identifier;
  cursorId   : identifier;
  cursorId2  : identifier;
  theVector  : resPtr;
  theCursor  : resPtr;
  theCursor2 : resPtr;
  idx1Expr   : storage;
  idxType1   : identifier;
  idx2Expr   : storage;
  idxType2   : identifier;
  hasIdx     : boolean := false;
  subprogramId : constant identifier := vectors_swap_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  expectParameterComma( subprogramId );
  if identifiers( token ).class = varClass and then
     getUniType( identifiers( token ).kind ) = vectors_cursor_t then
     ParseInOutInstantiatedParameter( cursorId, vectors_cursor_t );
     ParseLastInOutInstantiatedParameter( subprogramId, cursorId2, vectors_cursor_t );
  else
     ParseExpression( idx1Expr, idxType1 );
     vectorIndexOrCursorOk( subprogramId, idxType1, vectorId );
     expectParameterComma( subprogramId );
     -- special case error to improve readability and avoid an more general
     -- error on limited variables
     -- TODO: types done flag
     if identifiers( token ).class = varClass and then
        getUniType( identifiers( token ).kind ) = vectors_cursor_t then
        err(
          context => subprogramId,
          subjectNotes => pl( qp( "a second index" ) ),
          reason => +"is expected not",
          obstructor => token,
          obstructorType => identifiers( token ).kind
        );
     end if;
     ParseExpression( idx2Expr, idxType2 );
     if not type_checks_done then
        baseTypesOK( idxType2, identifiers( vectorId ).genKind );
     end if;
     hasIdx := true;
     expectParameterClose( subprogramId );
  end if;

  if isExecutingCommand then
     if hasIdx then
        declare
           idx1 : vector_index;
           idx2 : vector_index;
        begin
           idx1 := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( idx1Expr.value ) ) );
           idx2 := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( idx2Expr.value ) ) );
           findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
           if metaLabelOK( Vector_Storage_Lists.Element( theVector.vslVector, idx1 ),
                           Vector_Storage_Lists.Element( theVector.vslVector, idx2 ) ) then
              Vector_Storage_Lists.Swap( theVector.vslVector, idx1, idx2 );
           end if;
        exception when constraint_error =>
           err_index( subprogramId, idx1Expr, idx2Expr );
        when others =>
           err_exception_raised;
        end;
     else
        begin
           findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
           findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
           findResource( to_resource_id( identifiers( cursorId2 ).value.all ), theCursor2 );
           if metaLabelOK( Vector_Storage_Lists.Element( theCursor.vslCursor),
                           Vector_Storage_Lists.Element( theCursor2.vslCursor ) ) then
              Vector_Storage_Lists.Swap( theVector.vslVector, theCursor.vslCursor, theCursor2.vslCursor );
           end if;
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
-- Move the cursor c2 to the position in the vector for element e. Start at the
-- position of cursor c1. If the element does not exist, the cursor c2 will
-- have no element.
------------------------------------------------------------------------------

procedure ParseVectorsFind is
  vectorId      : identifier;
  itemExpr      : storage;
  itemType      : identifier;
  startCursorId : identifier;
  positionCursorRef : reference;
  subprogramId : constant identifier := vectors_find_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  -- special case error to improve readability and avoid an more general
  -- error on limited variables.  For this reason, "NextGenItem" is not used.
  expectParameterComma( subprogramId );
  -- TODO: types done flag
  if identifiers( token ).class = varClass and then
     getUniType( identifiers( token ).kind ) = vectors_cursor_t then
        err(
          context => subprogramId,
          subjectNotes => pl( qp( "a vector element" ) ),
          subjectType => identifiers( vectorId ).genKind2,
          reason => +"is expected not",
          obstructor => token,
          obstructorType => identifiers( token ).kind
        );
  end if;
  ParseGenItemParameter( itemExpr, itemType, identifiers( vectorId ).genKind2 );
  -- It is tricky to handle one optional cursor followed by a required one,
  -- one existing and one a reference.
  ParseNextInOutInstantiatedParameter( subprogramId, startCursorId, vectors_cursor_t );
  ParseLastOutVectorCursor( subprogramId, vectorId, positionCursorRef );

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
            vector_storage_list_cursor,
            getIdentifierBlock( positionCursorRef.id )
        );
        AssignParameter(
            positionCursorRef,
            storage'( to_unbounded_string( positionCursorResourceId ), noMetaLabel )
        );
        findResource( positionCursorResourceId, thePositionCursor );

        if metaLabelOk( itemExpr ) then
        thePositionCursor.vslCursor := Vector_Storage_Lists.Find( theVector.vslVector,
           itemExpr, theCursor.vslCursor );
        else
           thePositionCursor.vslCursor := Vector_Storage_Lists.No_Element;
        end if;
     exception when program_error =>
       err_cursor_mismatch;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsFind;


------------------------------------------------------------------------------
--  REVERSE FIND
--
-- Syntax: reverse_find( v, e, ,c1 ,c2 );
-- Ada:    reverse_find( v, e, [,c1], c2 );
-- Move the cursor c2 backwards to the position in the vector for element e.
-- Start at the position of cursor c1. If the element does not exist, the
-- cursor c2 will have no element.
------------------------------------------------------------------------------

procedure ParseVectorsReverseFind is
  vectorId      : identifier;
  itemExpr      : storage;
  itemType      : identifier;
  startCursorId : identifier;
  positionCursorRef : reference;
  subprogramId : constant identifier := vectors_reverse_find_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  -- special case error to improve readability and avoid an more general
  -- error on limited variables.  For this reason, "NextGenItem" is not used.
  expectParameterComma( subprogramId );
  -- TODO: types done flag
  if identifiers( token ).class = varClass and then
     getUniType( identifiers( token ).kind ) = vectors_cursor_t then
        err(
          context => subprogramId,
          subjectNotes => pl( qp( "a vector element" ) ),
          subjectType => identifiers( vectorId ).genKind2,
          reason => +"is expected not",
          obstructor => token,
          obstructorType => identifiers( token ).kind
        );
  end if;
  ParseGenItemParameter( itemExpr, itemType, identifiers( vectorId ).genKind2 );
  -- It is tricky to handle one optional cursor followed by a required one,
  -- one existing and one a reference.
  ParseNextInOutInstantiatedParameter( subprogramId, startCursorId, vectors_cursor_t );
  ParseLastOutVectorCursor( subprogramId, vectorId, positionCursorRef );

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
            vector_storage_list_cursor,
            getIdentifierBlock( positionCursorRef.id )
        );
        AssignParameter(
            positionCursorRef,
            storage'( to_unbounded_string( positionCursorResourceId ), noMetaLabel )
        );
        findResource( positionCursorResourceId, thePositionCursor );

        if metaLabelOk( itemExpr ) then
           thePositionCursor.vslCursor := Vector_Storage_Lists.Reverse_Find(
              theVector.vslVector, itemExpr, theCursor.vslCursor );
        else
           thePositionCursor.vslCursor := Vector_Storage_Lists.No_Element;
        end if;
     exception when program_error =>
       err_cursor_mismatch;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsReverseFind;


------------------------------------------------------------------------------
--  FIND INDEX
--
-- Syntax: find_index( v, e, ,i1, i2 );
-- Ada:    find_index( v, e, [,i1], i2 );
-- Return the index in the vector for element e, searching backwards. Start at
-- the position of index i1. If the element does not exist, the index i2 will
-- have no index.
------------------------------------------------------------------------------

procedure ParseVectorsFindIndex is
  vectorId      : identifier;
  itemExpr      : storage;
  itemType      : identifier;
  startIdxExpr  : storage;
  startIdxType  : identifier;
  positionIdxRef : reference;
  subprogramId : constant identifier := vectors_find_index_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( subprogramId, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  -- in Ada, the next parameter is optional but is easier to make optional
  -- than find with two cursors
  ParseNextGenItemParameter( subprogramId, startIdxExpr, startIdxType, identifiers( vectorId ).genKind );
  ParseLastOutParameter( subprogramId, positionIdxRef, long_integer_t );

  if isExecutingCommand then
     declare
        theVector   : resPtr;
        startIdx    : vector_index;
        positionIdx : Vector_Storage_Lists.Extended_Index;
        positionValue : unbounded_string;
     begin
        findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
        startIdx := toRealVectorIndex( subprogramId, vectorId, long_integer(
           to_numeric( startIdxExpr.value ) ) );
        if metaLabelOk( itemExpr ) then
           positionIdx := Vector_Storage_Lists.Find_Index( theVector.vslVector,
             itemExpr, startIdx );
        end if;
        positionvalue := to_unbounded_string( numericValue( toUserVectorIndex( subprogramId, vectorId, positionIdx ) ) );
        AssignParameter(
           positionIdxRef,
           storage'( positionValue, noMetaLabel )
        );
     exception when constraint_error =>
        err_index( subprogramId, startIdxExpr );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsFindIndex;


------------------------------------------------------------------------------
--  REVERSE FIND INDEX
--
-- Syntax: reverse_find_index( v, e, ,i1, i2 );
-- Ada:    reverse_find_index( v, e, [,i1], i2 );
-- Return the index in the vector for element e, searching backwards. Start at
-- the position of index i1. If the element does not exist, the index i2 will
-- have no index.
------------------------------------------------------------------------------

procedure ParseVectorsReverseFindIndex is
  vectorId      : identifier;
  itemExpr      : storage;
  itemType      : identifier;
  startIdxExpr  : storage;
  startIdxType  : identifier;
  positionIdxRef : reference;
  subprogramId : constant identifier := vectors_reverse_find_index_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( subprogramId, itemExpr, itemType, identifiers( vectorId ).genKind2 );
  -- in Ada, the next parameter is optional but is easier to make optional
  -- than find with two cursors
  ParseNextGenItemParameter( subprogramId, startIdxExpr, startIdxType, identifiers( vectorId ).genKind );
  ParseLastOutParameter( subprogramId, positionIdxRef, long_integer_t );

  if isExecutingCommand then
     declare
        theVector   : resPtr;
        startIdx    : vector_index;
        positionIdx : Vector_Storage_Lists.Extended_Index;
        positionValue : unbounded_string;
     begin
        findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );

        startIdx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( startIdxExpr.value ) ) );

        if metaLabelOk( itemExpr ) then
           positionIdx := Vector_Storage_Lists.Reverse_Find_Index( theVector.vslVector,
              itemExpr, startIdx );
        end if;
        positionvalue := to_unbounded_string( numericValue( toUserVectorIndex(
           subprogramId, vectorId, positionIdx ) ) );

        AssignParameter(
            positionIdxRef,
            storage'( positionValue, noMetaLabel )
        );
     exception when constraint_error =>
        err_index( subprogramId, startIdxExpr );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsReverseFindIndex;


------------------------------------------------------------------------------
--  INCREMENT
--
-- Syntax: vectors.increment( v, i [, n] );
-- Ada:    vectors.increment( v, i [, n] );
-- Increase the numeric value under index i in the vector by one (or n). If
-- the index does not exist, do nothing.
------------------------------------------------------------------------------

procedure ParseVectorsIncrement is
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : storage;
  idxType   : identifier;
  numExpr   : storage;
  numType   : identifier;
  hasAmt    : boolean := false;
  subprogramId : constant identifier := vectors_increment_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( subprogramId, idxExpr, idxType, identifiers( vectorId ).genKind );
  if getUniType( identifiers( vectorId ).genKind2 ) /= uni_numeric_t then
     err( context => subprogramId,
          subject => vectorId,
          subjectType => identifiers( vectorId ).kind,
          reason  => +"must have numeric elements but the elements are type",
          obstructor => identifiers( vectorId ).genKind2
     );
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     hasAmt := true;
     ParseLastStringParameter( subprogramId, numExpr, numType, identifiers( vectorId ).genKind2 );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expect( symbol_t, ")" );
  else
     err( context => subprogramId,
          subjectNotes => pl( qp( "the parameter list" ) ),
          reason  => +"expects a ')' for two parameters or ',' for three",
          obstructorNotes => +""
     );
  end if;
  if isExecutingCommand then
     declare
       floatVal  : numericValue;
     begin
       if hasAmt then
          floatVal := numericValue( natural( to_numeric( numExpr.value ) ) );
       else
          floatVal := 1.0;
       end if;
       declare
         idx : vector_index;
       begin
         findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
         begin
           idx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( idxExpr.value ) ) );
           --Increment( theVector.vslVector, idx, floatVal );
         exception when constraint_error =>
           err( context => subprogramId,
             subjectNotes => pl( qp( "the amount" ) ),
             reason  => +"should be a natural not",
             obstructorNotes => em_value( numExpr.value ),
             obstructorType => numType,
             remedy => +"the value should be >= 0"
           );
         end;

         declare
           the_string : storage;
         begin
           the_string := Vector_Storage_Lists.Element( theVector.vslVector, idx );
           if hasAmt then
              if metaLabelOk( the_string, numExpr ) then
                 the_string.value := to_unbounded_string( to_numeric( the_string.value ) + floatVal );
                 Vector_Storage_Lists.Replace_Element( theVector.vslVector, idx, the_string );
              end if;
           else
              if metaLabelOk( the_string ) then
                 the_string.value := to_unbounded_string( to_numeric( the_string.value ) + floatVal );
                 Vector_Storage_Lists.Replace_Element( theVector.vslVector, idx, the_string );
              end if;
           end if;
         end;
       end;
     exception when constraint_error =>
       err_index( subprogramId, idxExpr );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsIncrement;


------------------------------------------------------------------------------
--  DECREMENT
--
-- Syntax: vectors.decrement( v, i [, n] );
-- Ada:    vectors.decrement( v, i [, n] );
-- Decrease the numeric value under index i in the vector by one (or n). If
-- the index does not exist, do nothing.
------------------------------------------------------------------------------

procedure ParseVectorsDecrement is
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : storage;
  idxType   : identifier;
  numExpr   : storage;
  numType   : identifier;
  hasAmt    : boolean := false;
  subprogramId : constant identifier := vectors_decrement_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseNextGenItemParameter( subprogramId, idxExpr, idxType, identifiers( vectorId ).genKind );
  if getUniType( identifiers( vectorId ).genKind2 ) /= uni_numeric_t then
     err( context => subprogramId,
          subject => vectorId,
          subjectType => identifiers( vectorId ).kind,
          reason  => +"must have numeric elements but the elements are type",
          obstructor => identifiers( vectorId ).genKind2
     );
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     hasAmt := true;
     ParseLastStringParameter( subprogramId, numExpr, numType, identifiers( vectorId ).genKind2 );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expect( symbol_t, ")" );
  else
     err( context => subprogramId,
          subjectNotes => pl( qp( "the parameter list" ) ),
          reason  => +"expects a ')' for two parameters or ',' for three",
          obstructorNotes => +""
     );
  end if;
  if isExecutingCommand then
     declare
       floatVal  : numericValue;
     begin
       if hasAmt then
          floatVal := numericValue( natural( to_numeric( numExpr.value ) ) );
       else
          floatVal := 1.0;
       end if;
       declare
         idx : vector_index;
       begin
         begin
           idx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( idxExpr.value ) ) );
           --Increment( theVector.vslVector, idx, floatVal );
         exception when constraint_error =>
           err( context => subprogramId,
             subjectNotes => pl( qp( "the amount" ) ),
             reason  => +"should be a natural not",
             obstructorNotes => em_value( numExpr.value ),
             obstructorType => numType,
             remedy => +"the value should be >= 0"
           );
         end;
         findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
         idx := toRealVectorIndex( subprogramId, vectorId, long_integer( to_numeric( idxExpr.value ) ) );
         -- Decrement( theVector.vslVector, idx, floatVal );
         declare
           the_string : storage;
         begin
           the_string := Vector_Storage_Lists.Element( theVector.vslVector, idx );
           the_string.value := to_unbounded_string( to_numeric( the_string.value ) - floatVal );
           if hasAmt then
              if metaLabelOk( the_string, numExpr ) then
                 Vector_Storage_Lists.Replace_Element( theVector.vslVector, idx, the_string );
              end if;
           else
              if metaLabelOk( the_string ) then
                 Vector_Storage_Lists.Replace_Element( theVector.vslVector, idx, the_string );
              end if;
           end if;
         end;
       end;
     exception when constraint_error =>
       err_index( subprogramId, idxExpr );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsDecrement;


------------------------------------------------------------------------------
--  ASSIGN
--
-- Syntax: vector.assign( t, s );
-- Ada:    vector.assign( t, s );
-- Assign source vector s to target vector t, overwriting the contents of t. s
-- is unchanged.
-- Does not take into account data meta label
------------------------------------------------------------------------------

procedure ParseVectorsAssign is
  targetVectorId   : identifier;
  sourceVectorId   : identifier;
  targetVector  : resPtr;
  sourceVector  : resPtr;
  subprogramId : constant identifier := vectors_assign_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, targetVectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( subprogramId, sourceVectorId, vectors_vector_t );
  if not error_found then
     vectorItemIndicesOk( subprogramId, sourceVectorId, targetVectorid );
     genTypesOk( identifiers( targetVectorId ).genKind2, identifiers( sourceVectorId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetVectorId ).value.all ), targetVector );
       findResource( to_resource_id( identifiers( sourceVectorId ).value.all ), sourceVector );
       Vector_Storage_Lists.Assign( targetVector.vslVector, sourceVector.vslVector );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseVectorsAssign;


------------------------------------------------------------------------------
--  TO INDEX
--
-- Syntax: i := to_index( v, c );
-- Ada:    i := to_index( c );
-- Return the vector index at the cursor position
------------------------------------------------------------------------------

procedure ParseVectorsToIndex( result : out storage; kind : out identifier ) is
  cursorId   : identifier;
  vectorId   : identifier;
  theCursor  : resPtr;
  convertedIdx : long_integer;
  subprogramId : constant identifier := vectors_to_index_t;
begin
  -- kind := identifiers( vectorId ).genKind;
  kind := long_integer_t;
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, vectorId, vectors_vector_t );
  ParseLastInOutInstantiatedParameter( subprogramId, cursorId, vectors_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       convertedIdx := toUserVectorIndex( subprogramId, vectorId,
           Vector_Storage_Lists.To_Index( theCursor.vslCursor ) );
       result := storage'(
         to_unbounded_string(
            long_integer'image( convertedIdx )
         ),
         noMetaLabel );
     end;
  end if;
end ParseVectorsToIndex;


-----------------------------------------------------------------------------
--
-- Housekeeping
--
-----------------------------------------------------------------------------


procedure StartupVectors is
  userNoIndexStr : string := long_integer'image( User_No_Index );
begin
  declareNamespace( "vectors" );

  -- Data Types

  declareIdent( vectors_vector_t, "vectors.vector", variable_t, genericTypeClass );
  identifiers( vectors_vector_t).usage := limitedUsage;
  declareIdent( vectors_cursor_t, "vectors.cursor", variable_t, genericTypeClass );
  identifiers( vectors_cursor_t).usage := limitedUsage;
  declareStandardConstant( vectors_no_index_t, "vectors.no_index",
    long_integer_t, userNoIndexStr );

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
  declareFunction(  vectors_to_index_t, "vectors.to_index", ParseVectorsToIndex'access );

  declareNamespaceClosed( "vectors" );
end StartupVectors;

procedure ShutdownVectors is
begin
  null;
end ShutdownVectors;

end parser_vectors;

