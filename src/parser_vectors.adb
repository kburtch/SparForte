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

with
    Ada.Containers,
    ada.strings.unbounded,
    user_io,
    world,
    scanner,
    scanner_res,
    parser,
    parser_params,
    parser_containers;
use
    ada.strings.unbounded,
    user_io,
    world,
    scanner,
    scanner_res,
    parser,
    parser_params,
    parser_containers;

package body parser_vectors is

------------------------------------------------------------------------------
-- Vecotrs package identifiers
------------------------------------------------------------------------------

vectors_vector_t        : identifier;
vectors_cursor_t        : identifier;

vectors_new_vector_t    : identifier;
vectors_clear_t         : identifier;
vectors_to_vector_t     : identifier;
vectors_capacity_t      : identifier;
vectors_reserve_capacity_t : identifier;
vectors_length_t        : identifier;
vectors_set_length_t    : identifier;
vectors_is_empty_t      : identifier;
vectors_append_t        : identifier;
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
--vectors_copy_t          : identifier;
vectors_reverse_elements_t : identifier;
vectors_flip_t          : identifier;
vectors_new_cursor_t    : identifier;
vectors_first_t         : identifier;
vectors_last_t          : identifier;
vectors_next_t          : identifier;
vectors_previous_t      : identifier;
vectors_delete_t        : identifier;


------------------------------------------------------------------------------
-- Utility subprograms
------------------------------------------------------------------------------

procedure ParseSingleVectorParameter( vectorId : out identifier ) is
begin
  ParseSingleInOutInstantiatedParameter( vectorId, vectors_vector_t );
end ParseSingleVectorParameter;

procedure ParseFirstVectorParameter( vectorId : out identifier ) is
begin
  ParseFirstInOutInstantiatedParameter( vectorId, vectors_vector_t );
end ParseFirstVectorParameter;

--procedure ParseNextVectorParameter( vectorId : out identifier ) is
--begin
--  ParseNextInOutInstantiatedParameter( vectorId, vectors_vector_t );
--end ParseNextVectorParameter;

procedure ParseLastVectorParameter( vectorId : out identifier ) is
begin
  ParseLastInOutInstantiatedParameter( vectorId, vectors_vector_t );
end ParseLastVectorParameter;

------------------------------------------------------------------------------

function toRealVectorIndex( vectorId : identifier; UserIdx : integer ) return vector_index is
   kind  : identifier;
   bkind : identifier;
   ukind : identifier;
   convertedIdx : vector_index;
begin
   kind := identifiers( vectorId ).genKind2;
   ukind := getUniType( kind );
   -- the index type is either a numeric or an enumerated
   if ukind = uni_numeric_t then
      -- for a numeric we need to know the subrange.  For a type with no
      -- subrange, assume it starts at integer'first.  Remember that the
      -- base type of natural will be integer, not natural.
      bkind := getBaseType( kind );
-- put_line( "userIdx = " & userIdx'img );
-- put_line( "bkind = " & to_string( identifiers( bkind ).name ) );
      if kind = natural_t or bkind = natural_t then
-- put_line( "looks like a natural" );
         convertedIdx := vector_index( UserIdx );
      elsif kind = positive_t or bkind = positive_t then
-- put_line( "looks like a positive" );
         convertedIdx := vector_index( UserIdx-1 );
      else
-- put_line( "looks like a integer" );
-- TODO: incorrect warning on constraint
-- put_line( "long_integer => " & long_integer'image( long_integer( UserIdx-integer'first ) ) );
         convertedIdx := vector_index( UserIdx-integer'first );
-- put_line( "vector_index => " & convertedIdx'img );
      end if;
   else
      -- for an enumerated, asuume it starts at zero
      convertedIdx := vector_index( UserIdx );
   end if;
   return convertedIdx;
end toRealVectorIndex;

function toUserVectorIndex( vectorId : identifier; realIdx : vector_index ) return integer is
   kind  : identifier;
   bkind : identifier;
   ukind : identifier;
   convertedIdx : integer;
begin
   kind  := identifiers( vectorId ).genKind2;
   ukind := getUniType( kind );
   -- the index type is either a numeric or an enumerated
   if ukind = uni_numeric_t then
      -- for a numeric we need to know the subrange.  For a type with no
      -- subrange, assume it starts at integer'first
      bkind := getBaseType( kind );
      if bkind = natural_t then
         convertedIdx := integer( realIdx );
      elsif bkind = positive_t then
         convertedIdx := integer( realIdx )+1;
      else
         convertedIdx := integer(realIdx )+integer'first;
      end if;
   else
      -- for an enumerated, asuume it starts at zero
      convertedIdx := integer( realIdx );
   end if;
   return convertedIdx;
end toUserVectorIndex;

------------------------------------------------------------------------------

procedure ParseSingleCursorParameter( cursId : out identifier ) is
begin
  ParseSingleInOutInstantiatedParameter( cursId, vectors_cursor_t );
end ParseSingleCursorParameter;

--procedure ParseFirstCursorParameter( cursId : out identifier ) is
--begin
--  ParseFirstInOutInstantiatedParameter( cursId, vectors_cursor_t );
--end ParseFirstCursorParameter;

--procedure ParseNextCursorParameter( cursId : out identifier ) is
--begin
--  ParseNextInOutInstantiatedParameter( cursId, vectors_cursor_t );
--end ParseNextCursorParameter;

procedure ParseLastCursorParameter( cursId : out identifier ) is
begin
  ParseLastInOutInstantiatedParameter( cursId, vectors_cursor_t );
end ParseLastCursorParameter;

------------------------------------------------------------------------------

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
-- Parser subprograms
------------------------------------------------------------------------------


procedure ParseVectorsNewVector is
  -- Syntax: vectors.new_vector( l, idx_type, elem_type );
  -- Ada:    N/A
  -- With arrays, genKind is the index type and the element type is from
  -- the array type.  However, vectors do not have a base kind to refer
  -- to to get the element type.  We will use genKind2 to hold the index
  -- type.
  resId : resHandleId;
  ref : reference;
  genKindId : identifier;
  genKind2Id : identifier;
  baseIndexKind : identifier;

     function getIntegerBaseType( originalKind : identifier ) return identifier is
        id : identifier := originalKind;
     begin
        if getUniType( id ) = uni_numeric_t then
        -- dereference types to get the root type
        -- getBaseType only dereferences subtypes
           loop
              exit when id = positive_t or
                   id = natural_t or
                   identifiers( id ).kind = uni_numeric_t;
              id := identifiers( id ).kind;
           end loop;
        end if;
        return id;
     end getIntegerBaseType;

     -- true


     -- TODO: put in scanner
     -- TODO: this does not handle derived types, only subtypes
     -- All integer types will be treated as a standard integer
     -- Integer - starting at -2147483648
     -- Natural - 0
     -- Positive - 1
     -- Enumerated (including boolean)
     -- The index must be a discrete integer or an enumerated type
     function isDiscreteIntegerOrEnum( id : identifier ) return boolean is
        indexBaseKind : identifier;
        isDiscrete : boolean := false;
     begin
        indexBaseKind := getIntegerBaseType( genKind2Id );
        if identifiers( genKind2Id ).list then
           err( "index type should be a scalar type" );
        elsif identifiers( getBaseType( genKind2Id ) ).kind = root_record_t then
           err( "index type should be a scalar type" );
        -- descrete type or character
        elsif genKind2Id = natural_t or
           genKind2Id = positive_t or
           genKind2Id = natural_t or
           genKind2Id = integer_t or
           genKind2Id = short_short_integer_t or
           genKind2Id = short_integer_t or
           genKind2Id = long_integer_t or
           genKind2Id = long_long_integer_t then
           isDiscrete := true;
        -- derived type of descrete type or character
        elsif indexBaseKind = natural_t or
           indexBaseKind = positive_t or
           indexBaseKind = natural_t or
           indexBaseKind = integer_t or
           indexBaseKind = short_short_integer_t or
           indexBaseKind = short_integer_t or
           indexBaseKind = long_integer_t or
           indexBaseKind = long_long_integer_t then
           isDiscrete := true;
        elsif getUniType( identifiers( genKind2Id ).kind ) = root_enumerated_t then
           isDiscrete := true;
        elsif getUniType( identifiers( genKind2Id ).kind ) = uni_string_t then
           err( "index type should not be a string type (except character)" );
        elsif getUniType( identifiers( genKind2Id ).kind ) = uni_numeric_t then
           err( "index type should be a discrete numeric type" );
        end if;
        return isDiscrete;
     end isDiscreteIntegerOrEnum;

begin
  expect( vectors_new_vector_t );
  ParseFirstOutParameter( ref, vectors_vector_t );
  baseTypesOK( ref.kind, vectors_vector_t );
  expect( symbol_t, "," );
  ParseIdentifier( genKind2Id );
  if class_ok( genKind2Id, typeClass, subClass ) then
     -- Index must be natural / natural subtype or enumeraged
     baseIndexKind := getIntegerBaseType( genKind2Id );
     if genKind2Id = natural_t or baseIndexKind = natural_t then
        null;
     elsif getUniType( identifiers( genKind2Id ).kind ) = root_enumerated_t then
        null;
     elsif getUniType( genKind2Id ) = uni_string_t then
        err( "index type should not be a string type (except character)" );
     elsif getUniType( genKind2Id ) = uni_numeric_t then
put_line( to_string( identifiers( baseIndexKind ).name ) );
        err( "index type  should be a discrete numeric type" );
     end if;
  end if;
  identifiers( ref.id ).genKind2 := genKind2Id;
  expect( symbol_t, "," );
  ParseIdentifier( genKindId );
  if class_ok( genKindId, typeClass, subClass ) then
     if identifiers( genKindId ).list then
        err( "element type should be a scalar type" );
     elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
        err( "element type should be a scalar type" );
     end if;
  end if;
  identifiers( ref.id ).genKind := genKindId;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     -- TODO: don't reinitialize if already initialized
     identifiers( ref.id ).resource := true;
     declareResource( resId, vector_string_list, getIdentifierBlock( ref.id ) );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParseVectorsNewVector;

procedure ParseVectorsClear is
  -- Syntax: vectors.clear( v );
  -- Ada:    vectors.clear( v );
  vectorId   : identifier;
  theVector  : resPtr;
begin
  expect( vectors_clear_t );
  ParseSingleVectorParameter( vectorId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_String_Lists.Clear( theVector.vslVector );
     end;
  end if;
end ParseVectorsClear;

procedure ParseVectorsToVector is
  -- Syntax: vectors.to_vector( v, e, n );
  -- Ada:    v := vectors.to_vector( [e, ] n );
  vectorId   : identifier;
  theVector  : resPtr;
  itemExpr   : unbounded_string;
  itemType   : identifier;
  cntExpr    : unbounded_string;
  cntType    : identifier;
begin
  expect( vectors_to_vector_t );
  ParseFirstVectorParameter( vectorId );
  expect( symbol_t, "," );
  ParseExpression( itemExpr, itemType );
  if baseTypesOK( itemType, identifiers( vectorId ).genKind ) then
     null;
  end if;
  expect( symbol_t, "," );
  ParseExpression( cntExpr, cntType );
  if baseTypesOK( cntType, containers_count_type_t ) then
     null;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       theVector.vslVector := Vector_String_Lists.To_Vector( ada.containers.count_type'value( to_string( cntExpr ) ) );
     end;
  end if;
end ParseVectorsToVector;

procedure ParseVectorsCapacity( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: c := capacity( v );
  -- Ada:    c := capacity( v );
  vectorId   : identifier;
  theVector  : resPtr;
begin
  kind := containers_count_type_t;
  expect( vectors_capacity_t );
  ParseSingleVectorParameter( vectorId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
-- TODO: leading space
       result := to_unbounded_string( ada.containers.count_type'image( Vector_String_Lists.Capacity( theVector.vslVector ) ) );
     end;
  end if;
end ParseVectorsCapacity;

procedure ParseVectorsReserveCapacity is
  -- Syntax: reserve_capacity( v, c );
  -- Ada:    reserve_capacity( v, c );
  vectorId   : identifier;
  theVector  : resPtr;
  cntExpr    : unbounded_string;
  cntType    : identifier;
begin
  expect( vectors_reserve_capacity_t );
  ParseFirstVectorParameter( vectorId );
  expect( symbol_t, "," );
  ParseExpression( cntExpr, cntType );
  if baseTypesOK( cntType, containers_count_type_t ) then
     null;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     declare
       cnt : ada.containers.count_type;
     begin
       cnt := ada.containers.count_type'value( to_string( cntExpr ) );
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_String_Lists.Reserve_Capacity( theVector.vslVector, cnt );
     exception when constraint_error =>
       err( "capacity count is the wrong type" ); -- TODO: say the type
     when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseVectorsReserveCapacity;

procedure ParseVectorsLength( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: c := length( v );
  -- Ada:    c := length( v );
  vectorId   : identifier;
  theVector  : resPtr;
begin
  kind := containers_count_type_t;
  expect( vectors_length_t );
  ParseSingleVectorParameter( vectorId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
-- TODO: leading space
       result := to_unbounded_string( ada.containers.count_type'image( Vector_String_Lists.Length( theVector.vslVector ) ) );
     end;
  end if;
end ParseVectorsLength;

procedure ParseVectorsSetLength is
  -- Syntax: set_length( v, c );
  -- Ada:    set_length( v, c );
  vectorId   : identifier;
  theVector  : resPtr;
  cntExpr    : unbounded_string;
  cntType    : identifier;
begin
  expect( vectors_set_length_t );
  ParseFirstVectorParameter( vectorId );
  expect( symbol_t, "," );
  ParseExpression( cntExpr, cntType );
  if baseTypesOK( cntType, containers_count_type_t ) then
     null;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     declare
       cnt : ada.containers.count_type;
     begin
       cnt := ada.containers.count_type'value( to_string( cntExpr ) );
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_String_Lists.Set_Length( theVector.vslVector, cnt );
     exception when constraint_error =>
       err( "length count is the wrong type" ); -- TODO: say the type
     when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseVectorsSetLength;

procedure ParseVectorsIsEmpty( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := is_empty( v );
  -- Ada:    b := is_empty( v );
  vectorId   : identifier;
  theVector  : resPtr;
begin
  kind := boolean_t;
  expect( vectors_is_empty_t );
  ParseSingleVectorParameter( vectorId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
-- TODO: leading space
       result := to_bush_boolean( Vector_String_Lists.Is_Empty( theVector.vslVector ) );
     end;
  end if;
end ParseVectorsIsEmpty;

procedure ParseVectorsAppend is
  -- Syntax: doubly_linked_list.append( l, s, [c] );
  -- Ada:    doubly_linked_list.append( l, s, [c] );
  vectorId  : identifier;
  theVector : resPtr;
  itemExpr  : unbounded_string;
  itemType  : identifier;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( vectors_append_t );
  ParseFirstVectorParameter( vectorId );
  ParseNextGenItemParameter( itemExpr, itemType, identifiers( vectorId ).genKind );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
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
       err( "append count must be a natural integer" );
     when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseVectorsAppend;

procedure ParseVectorsPrepend is
  -- Syntax: doubly_linked_list.prepend( l, s, [c] );
  -- Ada:    doubly_linked_list.prepend( l, s, [c] );
  vectorId  : identifier;
  theVector : resPtr;
  itemExpr  : unbounded_string;
  itemType  : identifier;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( vectors_prepend_t );
  ParseFirstVectorParameter( vectorId );
  ParseNextGenItemParameter( itemExpr, itemType, identifiers( vectorId ).genKind );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
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
       err( "prepend count must be a natural integer" );
     when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseVectorsPrepend;

procedure ParseVectorsFirstIndex( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: n := first_index( v );
  -- Ada:    n := first_index( v );
  vectorId   : identifier;
  theVector  : resPtr;
begin
  kind := boolean_t;
  expect( vectors_first_index_t );
  ParseSingleVectorParameter( vectorId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
-- TODO: leading space
       result := to_unbounded_string( vector_index'image( Vector_String_Lists.First_Index( theVector.vslVector ) ) );
     end;
  end if;
end ParseVectorsFirstIndex;

procedure ParseVectorsLastIndex( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: n := first_index( v );
  -- Ada:    n := first_index( v );
  vectorId   : identifier;
  theVector  : resPtr;
begin
  kind := boolean_t;
  expect( vectors_last_index_t );
  ParseSingleVectorParameter( vectorId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
-- TODO: leading space
       result := to_unbounded_string( vector_index'image( Vector_String_Lists.Last_Index( theVector.vslVector ) ) );
     end;
  end if;
end ParseVectorsLastIndex;

procedure ParseVectorsElement( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: e := element( c ) | element( v, i )
  -- Ada:    e := element( c ) | element( v, i )
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : unbounded_string;
  idxType   : identifier;
  cursorId  : identifier;
  theCursor : resPtr;
  hasCursor : boolean := false;
  res : boolean;
begin
  expect( vectors_element_t );
--  ParseLastNumericParameter( idxExpr, idxType, identifiers( vectorId ).genKind2 );
  -- A cursor is a single identifier.  An index is an expression.
  expect( symbol_t, "(" );
  if identifiers( token ).kind = vectors_cursor_t then
     hasCursor := true;
     ParseIdentifier( cursorId );
     --CheckCursorIsInitialized( cursorId );
  elsif identifiers( token ).kind = vectors_vector_t then
     ParseIdentifier( vectorId );
     --CheckVectorIsInitialized( vectorId );
     expect( symbol_t, "," );
     ParseExpression( idxExpr, idxType );
     res := baseTypesOK( idxType, identifiers( vectorId ).genKind2 );
  else
     err( optional_bold( "vectors.vector" ) &
          " or " &
          optional_bold( "vectors.cursor" ) &
          " expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     declare
       idx : vector_index;
     begin
--put_line( to_string( idxExpr ) );
--put_line( idx'img );
       if hasCursor then
         findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
         kind := identifiers( cursorId ).genKind; -- TODO
         result := Vector_String_Lists.Element( theCursor.vslCursor );
       else
         findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
         kind := identifiers( vectorId ).genKind;
         idx := vector_index( to_numeric( idxExpr ) );
         idx := toRealVectorIndex( vectorId, integer( to_numeric( idxExpr ) ) );
         result := Vector_String_Lists.Element( theVector.vslVector, idx );
       end if;
--put_line( "HERE" );
-- NOTE: Vector Lists stores internally a natural
     exception when constraint_error =>
       err( "index value out of range" );
     when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseVectorsElement;

procedure ParseVectorsFirstElement( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: e := first_element( v );
  -- Ada:    e := first_element( v );
  vectorId   : identifier;
  theVector  : resPtr;
begin
  expect( vectors_first_element_t );
  ParseSingleVectorParameter( vectorId );
  kind := identifiers( vectorId ).genKind;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := Vector_String_Lists.First_Element( theVector.vslVector );
     exception when constraint_error =>
       err( "vector is empty" );
     end;
  end if;
end ParseVectorsFirstElement;

procedure ParseVectorsLastElement( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: e := last_element( v );
  -- Ada:    e := last_element( v );
  vectorId   : identifier;
  theVector  : resPtr;
begin
  expect( vectors_last_element_t );
  ParseSingleVectorParameter( vectorId );
  kind := identifiers( vectorId ).genKind;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := Vector_String_Lists.Last_Element( theVector.vslVector );
     exception when constraint_error =>
       err( "vector is empty" );
     end;
  end if;
end ParseVectorsLastElement;

procedure ParseVectorsDeleteFirst is
  -- Syntax: delete_first( v [,c] )
  -- Ada:    delete_first( v [,c] )
  vectorId  : identifier;
  theVector : resPtr;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( vectors_delete_first_t );
  ParseFirstVectorParameter( vectorId );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
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
       err( "count must be a natural integer" );
     when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseVectorsDeleteFirst;

procedure ParseVectorsDeleteLast is
  -- Syntax: delete_last( v [,c] )
  -- Ada:    delete_last( v [,c] )
  vectorId  : identifier;
  theVector : resPtr;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( vectors_delete_last_t );
  ParseFirstVectorParameter( vectorId );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
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
       err( "count must be a natural integer" );
     when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseVectorsDeleteLast;

procedure ParseVectorsContains( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := contains( v, e )
  -- Ada:    b := contains( v, e )
  vectorId  : identifier;
  theVector : resPtr;
  itemExpr  : unbounded_string;
  itemType  : identifier;
begin
  kind := boolean_t;
  expect( vectors_contains_t );
  ParseFirstVectorParameter( vectorId );
  ParseLastGenItemParameter( itemExpr, itemType, identifiers( vectorId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       result := to_bush_boolean( Vector_String_Lists.Contains( theVector.vslVector, itemExpr ) );

     end;
  end if;
end ParseVectorsContains;

procedure ParseVectorsMove is
  -- Syntax: move( v1, v2 );
  -- Ada:    move( v1, v2 );
  sourceVectorId   : identifier;
  theSourceVector  : resPtr;
  targetVectorId   : identifier;
  theTargetVector  : resPtr;
begin
  expect( vectors_move_t );
  ParseFirstVectorParameter( targetVectorId );
  ParseLastVectorParameter( sourceVectorId );
  genTypesOk( identifiers( targetVectorId ).genKind, identifiers( sourceVectorId ).genKind );
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

procedure ParseVectorsReverseElements is
  -- Syntax: reverse_elements( v );
  -- Ada:    reverse_elements( v );
  vectorId  : identifier;
  theVector : resPtr;
begin
  expect( vectors_reverse_elements_t );
  ParseSingleVectorParameter( vectorId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_String_Lists.Reverse_Elements( theVector.vslVector );
     end;
  end if;
end ParseVectorsReverseElements;

procedure ParseVectorsFlip is
  -- Syntax: flip( v );
  -- Ada:    flip( v );
  vectorId  : identifier;
  theVector : resPtr;
begin
  expect( vectors_flip_t );
  ParseSingleVectorParameter( vectorId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       Vector_String_Lists.Reverse_Elements( theVector.vslVector );
     end;
  end if;
end ParseVectorsFlip;

procedure ParseVectorsNewCursor is
  -- Syntax: vectors.new_cursor( c, t );
  -- Ada:    N/A
  resId : resHandleId;
  ref : reference;
  genKindId : identifier;
begin
  expect( vectors_new_cursor_t );
  ParseFirstOutParameter( ref, vectors_cursor_t );
  baseTypesOK( ref.kind, vectors_cursor_t );
  expect( symbol_t, "," );
  ParseIdentifier( genKindId );
  if class_ok( genKindId, typeClass, subClass ) then
      null;
  end if;
  identifiers( ref.id ).genKind := genKindId;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     identifiers( ref.id ).resource := true;
     declareResource( resId, vector_string_list_cursor, getIdentifierBlock( ref.id ) );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParseVectorsNewCursor;

procedure ParseVectorsFirst is
  -- Syntax: first( v, c );
  -- Ada:    c := first( v );
  vectorId   : identifier;
  theVector  : resPtr;
  cursId     : identifier;
  theCursor  : resPtr;
begin
  expect( vectors_first_t );
  ParseFirstVectorParameter( vectorId );
  ParseLastCursorParameter( cursId );
  genTypesOk( identifiers( vectorId ).genKind, identifiers( cursId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursId ).value.all ), theCursor );
       theCursor.vslCursor := Vector_String_Lists.First( theVector.vslVector );
     end;
  end if;
end ParseVectorsFirst;

procedure ParseVectorsLast is
  -- Syntax: last( v, c );
  -- Ada:    c := last( v );
  vectorId   : identifier;
  theVector  : resPtr;
  cursId     : identifier;
  theCursor  : resPtr;
begin
  expect( vectors_last_t );
  ParseFirstVectorParameter( vectorId );
  ParseLastCursorParameter( cursId );
  genTypesOk( identifiers( vectorId ).genKind, identifiers( cursId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
       findResource( to_resource_id( identifiers( cursId ).value.all ), theCursor );
       theCursor.vslCursor := Vector_String_Lists.Last( theVector.vslVector );
     end;
  end if;
end ParseVectorsLast;

procedure ParseVectorsNext is
  -- Syntax: next( c );
  -- Ada:    next( c );
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( vectors_next_t );
  ParseSingleCursorParameter( cursId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).value.all ), theCursor );
       Vector_String_Lists.Next( theCursor.vslCursor );
     end;
  end if;
end ParseVectorsNext;

procedure ParseVectorsPrevious is
  -- Syntax: previous( c );
  -- Ada:    previous( c );
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( vectors_previous_t );
  ParseSingleCursorParameter( cursId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).value.all ), theCursor );
       Vector_String_Lists.Previous( theCursor.vslCursor );
     end;
  end if;
end ParseVectorsPrevious;

procedure ParseVectorsDelete is
  -- Syntax: delete( v, c | i )
  -- Ada:    delete( v, c | i )
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
  ParseFirstVectorParameter( vectorId );
  expect( symbol_t, "," );
  if identifiers( token ).kind = vectors_cursor_t then
     ParseIdentifier( cursorId );
     --CheckCursorIsInitialized( cursorId );
  else
     ParseNumericParameter( idxExpr, idxType, identifiers( vectorId ).genKind2 );
     -- should be extended index
     hasIdx := true;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     expect( symbol_t, "," );
     ParseNumericParameter( cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
        findResource( to_resource_id( identifiers( vectorId ).value.all ), theVector );
        if hasIdx then
           if hasCnt then
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
       err( "count must be a natural integer" );
     when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseVectorsDelete;



-----------------------------------------------------------------------------

procedure StartupVectors is
begin
  declareNamespace( "vectors" );
  declareIdent( vectors_vector_t, "vectors.vector", variable_t, genericTypeClass );
  identifiers( vectors_vector_t).usage := limitedUsage;
  declareIdent( vectors_cursor_t, "vectors.cursor", variable_t, genericTypeClass );
  identifiers( vectors_cursor_t).usage := limitedUsage;

  declareProcedure( vectors_new_vector_t,  "vectors.new_vector", ParseVectorsNewVector'access );
  declareProcedure( vectors_clear_t,     "vectors.clear",    ParseVectorsClear'access );
  declareProcedure( vectors_to_vector_t, "vectors.to_vector",   ParseVectorsToVector'access );
  declareFunction(  vectors_capacity_t,  "vectors.capacity",    ParseVectorsCapacity'access );
  declareProcedure( vectors_reserve_capacity_t,  "vectors.reserve_capacity",    ParseVectorsReserveCapacity'access );
  declareFunction(  vectors_length_t,    "vectors.length",    ParseVectorsLength'access );
  declareProcedure( vectors_set_length_t,  "vectors.set_length",    ParseVectorsSetLength'access );
  declareFunction(  vectors_is_empty_t,  "vectors.is_empty",  ParseVectorsIsEmpty'access );
  declareProcedure( vectors_append_t,  "vectors.append",    ParseVectorsAppend'access );
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
--  declareProcedure( vectors_copy_t,  "vectors.copy",    ParseVectorsCopy'access );
  declareProcedure( vectors_new_cursor_t,  "vectors.new_cursor", ParseVectorsNewCursor'access );
  declareProcedure( vectors_first_t,  "vectors.first", ParseVectorsFirst'access );
  declareProcedure( vectors_last_t,  "vectors.last", ParseVectorsLast'access );
  declareProcedure( vectors_next_t,  "vectors.next", ParseVectorsNext'access );
  declareProcedure( vectors_previous_t,  "vectors.previous", ParseVectorsPrevious'access );
  declareProcedure( vectors_delete_t,  "vectors.delete", ParseVectorsDelete'access );
  declareNamespaceClosed( "vectors" );
end StartupVectors;

procedure ShutdownVectors is
begin
  null;
end ShutdownVectors;

end parser_vectors;

