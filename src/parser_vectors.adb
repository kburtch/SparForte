------------------------------------------------------------------------------
-- Singly Linked Lists Package Parser                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2014 Free Software Foundation              --
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
    world,
    scanner,
    scanner_res,
    string_util,
    parser,
    parser_aux,
    parser_params,
    parser_containers;
use
    world,
    scanner,
    scanner_res,
    string_util,
    parser,
    parser_aux,
    parser_params,
    parser_containers;

package body parser_vectors is

------------------------------------------------------------------------------
-- Utility subprograms
------------------------------------------------------------------------------

procedure CheckVectorIsInitialized( vectorId : identifier ) is
begin
  if identifiers( vectorId ).genKind = eof_t then
     err( "new_vector has not been called to initialize the vector" );
  end if;
end CheckVectorIsInitialized;

procedure ParseSingleVectorParameter( vectorId : out identifier ) is
begin
  ParseSingleInOutParameter( vectorId, vectors_vector_t );
  CheckVectorIsInitialized( vectorId );
end ParseSingleVectorParameter;

procedure ParseFirstVectorParameter( vectorId : out identifier ) is
begin
  ParseFirstInOutParameter( vectorId, vectors_vector_t );
  CheckVectorIsInitialized( vectorId );
end ParseFirstVectorParameter;

procedure ParseNextVectorParameter( vectorId : out identifier ) is
begin
  ParseNextInOutParameter( vectorId, vectors_vector_t );
  CheckVectorIsInitialized( vectorId );
end ParseNextVectorParameter;

procedure ParseLastVectorParameter( vectorId : out identifier ) is
begin
  ParseLastInOutParameter( vectorId, vectors_vector_t );
  CheckVectorIsInitialized( vectorId );
end ParseLastVectorParameter;

------------------------------------------------------------------------------

procedure CheckCursorIsInitialized( cursId : identifier ) is
begin
  if identifiers( cursId ).genKind = eof_t then
     err( "new_cursor has not been called to initialize the cursor" );
  end if;
end CheckCursorIsInitialized;

procedure ParseSingleCursorParameter( cursId : out identifier ) is
begin
  ParseSingleInOutParameter( cursId, vectors_cursor_t );
  CheckCursorIsInitialized( cursId );
end ParseSingleCursorParameter;

procedure ParseFirstCursorParameter( cursId : out identifier ) is
begin
  ParseFirstInOutParameter( cursId, vectors_cursor_t );
  CheckCursorIsInitialized( cursId );
end ParseFirstCursorParameter;

procedure ParseNextCursorParameter( cursId : out identifier ) is
begin
  ParseNextInOutParameter( cursId, vectors_cursor_t );
  CheckCursorIsInitialized( cursId );
end ParseNextCursorParameter;

procedure ParseLastCursorParameter( cursId : out identifier ) is
begin
  ParseLastInOutParameter( cursId, vectors_cursor_t );
  CheckCursorIsInitialized( cursId );
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
begin
  expect( vectors_new_vector_t );
  ParseFirstOutParameter( ref, vectors_vector_t );
  baseTypesOK( ref.kind, vectors_vector_t );
  expect( symbol_t, "," );
  ParseIdentifier( genKind2Id );
  if class_ok( genKind2Id, typeClass, subClass ) then
     if identifiers( genKind2Id ).list then
        err( "index type should be a scalar type" );
     elsif identifiers( getBaseType( genKind2Id ) ).kind = root_record_t then
        err( "index type should be a scalar type" );
     elsif getUniType( identifiers( genKind2Id ).kind ) = uni_string_t then
        err( "index type should not be a string type" );
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
     declareResource( resId, vector_string_list, blocks_top );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
  if token = symbol_t and identifiers( token ).value = "," then
     ParseLastNumericParameter( cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
  if token = symbol_t and identifiers( token ).value = "," then
     ParseLastNumericParameter( cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
-- TODO: leading space
       result := to_unbounded_string( vector_index'image( Vector_String_Lists.Last_Index( theVector.vslVector ) ) );
     end;
  end if;
end ParseVectorsLastIndex;

procedure ParseVectorsElement( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := is_empty( v );
  -- Ada:    e := element( c ) | element( v, i )
  vectorId  : identifier;
  theVector : resPtr;
  idxExpr   : unbounded_string;
  idxType   : identifier;
  hasIdx    : boolean := false;
begin
  expect( vectors_element_t );
  hasIdx := true;
  ParseFirstVectorParameter( vectorId );
  ParseLastNumericParameter( idxExpr, idxType, identifiers( vectorId ).genKind2 );
  kind := identifiers( vectorId ).genKind;
  if isExecutingCommand then
     declare
       idx : vector_index;
     begin
--put_line( to_string( idxExpr ) );
       idx := vector_index( to_numeric( idxExpr ) );
--put_line( idx'img );
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
--put_line( "HERE" );
-- NOTE: Vector Lists stores internally a natural
       result := Vector_String_Lists.Element( theVector.vslVector, idx );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
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
       findResource( to_resource_id( identifiers( vectorId ).value ), theVector );
       result := Vector_String_Lists.Last_Element( theVector.vslVector );
     exception when constraint_error =>
       err( "vector is empty" );
     end;
  end if;
end ParseVectorsLastElement;



-----------------------------------------------------------------------------

procedure StartupVectors is
begin
  declareIdent( vectors_vector_t, "vectors.vector", positive_t, typeClass );
  declareIdent( vectors_cursor_t, "vectors.cursor", positive_t, typeClass );

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
end StartupVectors;

procedure ShutdownVectors is
begin
  null;
end ShutdownVectors;

end parser_vectors;

