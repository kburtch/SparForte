------------------------------------------------------------------------------
-- Hashed Maps Package Parser                                               --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
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
    gnat.source_info,
    pegasoft.strings,
    pegasoft.user_io,
    pegasoft.hmaps,
    world,
    scanner.communications,
    scanner_res,
    scanner_restypes,
    parser,
    parser_params,
    parser_containers;
use
    ada.strings.unbounded,
    world,
    pegasoft,
    pegasoft.strings,
    pegasoft.user_io,
    pegasoft.hmaps,
    scanner,
    scanner.communications,
    scanner_res,
    scanner_restypes,
    parser,
    parser_params,
    parser_containers;

package body parser_hmaps is

hashed_maps_clear_t    : identifier;
hashed_maps_is_empty_t : identifier;
hashed_maps_capacity_t : identifier;
hashed_maps_reserve_capacity_t : identifier;
hashed_maps_length_t   : identifier;
hashed_maps_insert_t   : identifier;
hashed_maps_include_t  : identifier;
hashed_maps_replace_t  : identifier;
hashed_maps_exclude_t  : identifier;
hashed_maps_delete_t   : identifier;
hashed_maps_contains_t : identifier;
hashed_maps_element_t  : identifier;
hashed_maps_append_t   : identifier;
hashed_maps_prepend_t  : identifier;
hashed_maps_increment_t : identifier;
hashed_maps_decrement_t : identifier;
hashed_maps_extract_t   : identifier;
hashed_maps_assign_t    : identifier;
hashed_maps_move_t      : identifier;
hashed_maps_first_t     : identifier;
hashed_maps_next_t      : identifier;
hashed_maps_key_t       : identifier;
hashed_maps_find_t      : identifier;
hashed_maps_replace_element_t :identifier;
hashed_maps_has_element_t : identifier;
hashed_maps_equal_t     : identifier;


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
--  ERR NO KEY
--
------------------------------------------------------------------------------

procedure err_no_key( subprogram : identifier; keyName : unbounded_string ) is
begin
  err( context => subprogram,
       subjectNotes => pl( qp( "the key " ) ) & em( toSecureData( to_string( ToEscaped( keyName ) ) ) ),
       reason => +"is",
       obstructorNotes => +"not in the map" );
end err_no_key;


------------------------------------------------------------------------------
--  ERR KEY EXISTS
--
------------------------------------------------------------------------------

procedure err_key_exists( subprogram : identifier; keyName : unbounded_string ) is
begin
  err( context => subprogram,
       subjectNotes => pl( qp( "the key " ) ) & em( toSecureData( to_string( ToEscaped( keyName ) ) ) ),
       reason => +"is",
       obstructorNotes => +"already in the map" );
end err_key_exists;


------------------------------------------------------------------------------
--  PARSE OUT MAP CURSOR
--
-- If necessary, declare a out cursor parameter.  Attach the resource.
------------------------------------------------------------------------------

procedure ParseOutMapCursor( mapId : identifier; cursRef : in out reference ) is
  resId : resHandleId;
begin
  --expect( symbol_t, "," );
  if identifiers( token ).kind = new_t then
     ParseOutParameter( cursRef, hashed_maps_cursor_t );

     if not error_found then
        identifiers( cursRef.id ).genKind := identifiers( mapId ).genKind;
        identifiers( cursRef.id ).genKind2 := identifiers( mapId ).genKind2;
     end if;

     if isExecutingCommand then

        -- Attach the resource

        declareResource( resId, string_hashed_map_cursor, getIdentifierBlock( cursRef.id ) );
        identifiers( cursRef.id ).svalue := to_unbounded_string( resId );
        identifiers( cursRef.id ).value := identifiers( cursRef.id ).svalue'access;
        identifiers( cursRef.id ).resource := true;
     end if;
  else
     ParseInOutInstantiatedParameter( cursRef.id , hashed_maps_cursor_t );

     -- Check the type against the map

     if not error_found then
        genTypesOk( identifiers( mapId ).genKind, identifiers( cursRef.Id ).genKind );
        genTypesOk( identifiers( mapId ).genKind2, identifiers( cursRef.Id ).genKind2 );
     end if;
  end if;
  --expect( symbol_t, ")" );
end ParseOutMapCursor;

procedure ParseNextOutMapCursor( mapId : identifier; cursRef : in out reference ) is
begin
  expectParameterComma;
  ParseOutMapCursor( mapId, cursRef );
end ParseNextOutMapCursor;


------------------------------------------------------------------------------
--  CLEAR
--
-- Syntax: hashed_maps.clear( m );
-- Ada:    hashed_maps.clear( m );
------------------------------------------------------------------------------

procedure ParseHashedMapsClear is

  mapId   : identifier;
  theMap  : resPtr;
  subprogramId : constant identifier := hashed_maps_clear_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       String_Hashed_Maps.Clear( theMap.shmMap );
     end;
  end if;
end ParseHashedMapsClear;


------------------------------------------------------------------------------
--  IS EMPTY
--
-- Syntax: b := hashed_maps.is_empty( m );
-- Ada:    b := hashed_maps.is_empty( m );
------------------------------------------------------------------------------

procedure ParseHashedMapsIsEmpty( result : out unbounded_string; kind : out identifier ) is
  mapId   : identifier;
  theMap  : resPtr;
  subprogramId : constant identifier := hashed_maps_is_empty_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       result := to_bush_boolean( String_Hashed_Maps.Is_Empty( theMap.shmMap ) );
     end;
  end if;
end ParseHashedMapsIsEmpty;


------------------------------------------------------------------------------
--  CAPACITY
--
-- Syntax: c := hashed_maps.capacity( m );
-- Ada:    c := hashed_maps.capacity( m );
------------------------------------------------------------------------------

procedure ParseHashedMapsCapacity( result : out unbounded_string; kind : out identifier ) is
  mapId   : identifier;
  theMap  : resPtr;
  subprogramId : constant identifier := hashed_maps_capacity_t;
begin
  kind := containers_count_type_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       result := to_unbounded_string( String_Hashed_Maps.Capacity( theMap.shmMap )'img );
     end;
  end if;
end ParseHashedMapsCapacity;


------------------------------------------------------------------------------
--  RESERVE CAPACITY
--
-- Syntax: hashed_maps.reserve_capacity( m, c );
-- Ada:    hashed_maps.reserve_capacity( m, c );
------------------------------------------------------------------------------

procedure ParseHashedMapsReserveCapacity is
  mapId   : identifier;
  theMap  : resPtr;
  capVal : unbounded_string;
  capType : identifier;
  subprogramId : constant identifier := hashed_maps_reserve_capacity_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  ParseLastNumericParameter( subprogramId, capVal, capType, containers_count_type_t );
  if isExecutingCommand then
     declare
       cap : constant Ada.Containers.Count_Type := Ada.Containers.Count_Type( to_numeric( capVal ) );
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       String_Hashed_Maps.Reserve_Capacity( theMap.shmMap, cap );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsReserveCapacity;


------------------------------------------------------------------------------
--  INSERT
--
-- Syntax: hashed_maps.insert( m, k, e [, n, p, i ] ) | ( m, k, p, i )
-- Ada:    hashed_maps.insert( m, k, e [, n, p, i ] )
------------------------------------------------------------------------------

procedure ParseHashedMapsInsert is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  elemVal : unbounded_string;
  elemType : identifier;
  -- cursorId  : identifier := eof_t;
  cursorRef : reference;
  theCursor : resPtr;
  insertRef : reference;
  version   : natural := 1;
  result    : boolean;
  subprogramId : constant identifier := hashed_maps_insert_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  ParseNextGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  expectParameterComma;

  -- There are 3 variants to an insert

  -- If a token is undefind, we have to assume it is an out mode cursor.

  if identifiers( token ).kind = new_t or else getUniType( identifiers( token ).kind ) = hashed_maps_cursor_t then
     ParseOutMapCursor( mapId, cursorRef );
     --ParseInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
     ParseLastOutParameter( subprogramId, insertRef, boolean_t );
     version := 2;
  else
    ParseGenItemParameter( elemVal, elemType, identifiers( mapId ).genKind2 );
    if token = symbol_t and identifiers( token ).value.all = ")" then
       expect( symbol_t, ")" );
    else
       ParseNextOutMapCursor( mapId, cursorRef );
       --ParseNextInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
       ParseLastOutParameter( subprogramId, insertRef, boolean_t );
       version := 3;
    end if;
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );

       case version is
       -- ( m, k, e )
       when 1 =>
          String_Hashed_Maps.Insert( theMap.shmMap, keyVal, elemVal );
       -- (m, k, p, b )
       when 2 =>
          findResource( to_resource_id( identifiers( cursorRef.id ).value.all ), theCursor );
          String_Hashed_Maps.Insert( theMap.shmMap, keyVal, theCursor.shmCursor,
             result );
          AssignParameter( insertRef, to_bush_boolean( result ) );
       -- (m, k, e, p, b )
       when 3 =>
          findResource( to_resource_id( identifiers( cursorRef.Id ).value.all ), theCursor );
          String_Hashed_Maps.Insert( theMap.shmMap, keyVal, elemVal,
             theCursor.shmCursor, result );
          AssignParameter( insertRef, to_bush_boolean( result ) );
       when others =>
          put_line_retry( gnat.source_info.source_location &
             ": internal error: unknown insert version" );
       end case;

     exception when constraint_error =>
       err_key_exists( subprogramId, keyVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsInsert;


------------------------------------------------------------------------------
--  INCLUDE
--
-- Syntax: hashed_maps.include( m, k, e )
-- Ada:    hashed_maps.include( m, k, e )
------------------------------------------------------------------------------

procedure ParseHashedMapsInclude is
  mapId     : identifier;
  theMap    : resPtr;
  keyVal    : unbounded_string;
  keyType   : identifier;
  elemVal   : unbounded_string;
  elemType  : identifier;
  subprogramId : constant identifier := hashed_maps_include_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  ParseNextGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastGenItemParameter( subprogramId, elemVal, elemType, identifiers( mapId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       String_Hashed_Maps.Include( theMap.shmMap, keyVal, elemVal );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsInclude;


------------------------------------------------------------------------------
--  REPLACE
--
-- Syntax: hashed_maps.replace( m, k, e );
-- Ada:    hashed_maps.replace( m, k, e );
------------------------------------------------------------------------------

procedure ParseHashedMapsReplace is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  elemVal : unbounded_string;
  elemType : identifier;
  subprogramId : constant identifier := hashed_maps_replace_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  ParseNextGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastGenItemParameter( subprogramId, elemVal, elemType, identifiers( mapId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       String_Hashed_Maps.Replace( theMap.shmMap, keyVal, elemVal );
     exception when constraint_error =>
       err_no_key( subprogramId, keyVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsReplace;


------------------------------------------------------------------------------
--  EXCLUDE
--
-- Syntax: hashed_maps.exclude( m, k );
-- Ada:    hashed_maps.exclude( m, k );
------------------------------------------------------------------------------

procedure ParseHashedMapsExclude is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  subprogramId : constant identifier := hashed_maps_exclude_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  ParseLastGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       String_Hashed_Maps.Exclude( theMap.shmMap, keyVal );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsExclude;


------------------------------------------------------------------------------
--  DELETE
--
-- Syntax: hashed_maps.delete( m, k | c );
-- Ada:    hashed_maps.delete( m, k | c );
------------------------------------------------------------------------------

procedure ParseHashedMapsDelete is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  cursorId : identifier := eof_t;
  theCursor : resPtr;
  subprogramId : constant identifier := hashed_maps_delete_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  expectParameterComma;
  if identifiers( token ).class = varClass and then
     getUniType( identifiers( token ).kind ) = hashed_maps_cursor_t then
     ParseInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
  else
     ParseGenItemParameter( keyVal, keyType, identifiers( mapId ).genKind );
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       if cursorId = eof_t then
          String_Hashed_Maps.Delete( theMap.shmMap, keyVal );
       else
          findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
          String_Hashed_Maps.Delete( theMap.shmMap, theCursor.shmCursor );
       end if;
     exception when constraint_error =>
       err_no_key( subprogramId, keyVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsDelete;


------------------------------------------------------------------------------
--  CONTAINS
--
-- Syntax: b := hashed_maps.contains( m, k );
-- Ada:    b := hashed_maps.contains( m, k );
------------------------------------------------------------------------------

procedure ParseHashedMapsContains( result : out unbounded_string; kind : out identifier ) is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  subprogramId : constant identifier := hashed_maps_contains_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  ParseLastGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       result := to_bush_boolean( String_Hashed_Maps.Contains( theMap.shmMap, keyVal ) );
     end;
  end if;
end ParseHashedMapsContains;


------------------------------------------------------------------------------
--  ELEMENT
--
-- Syntax: e := hashed_maps.element( m, k ) | element(c)
-- Ada:    e := hashed_maps.element( m, k ); | element(c)
------------------------------------------------------------------------------

procedure ParseHashedMapsElement( result : out unbounded_string; kind : out identifier ) is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  cursorId  : identifier := eof_t;
  theCursor : resPtr;
  firstParamUniType : identifier;
  discard : identifier;
  subprogramId : constant identifier := hashed_maps_element_t;
begin
  -- in the event of an error, we don't know what the data type is
  kind := eof_t;

  expect( subprogramId );

  -- The first parameter may be a map or a cursor
  -- The universal type will be the generic type it descends from
  -- (if the user derived a new type).

  expect( symbol_t, "(" );
  if identifiers( token ).class /= varClass then
     err(
         context => subprogramId,
         subjectNotes => em( "hashed_maps.map" ) & pl( " or " ) &
             em( "hashed_maps.cursor" ),
         reason => +"or compatible type expected but found",
         obstructor => token,
         obstructorType => identifiers( token ).kind,
         remedy => +"declare a new limited type"
     );
  else
     firstParamUniType := getUniType( token );
     if firstParamUniType = hashed_maps_cursor_t then
        ParseInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
        expect( symbol_t, ")" );
     elsif firstParamUniType = hashed_maps_map_t then
        ParseInOutInstantiatedParameter( mapId, hashed_maps_map_t );
        ParseLastGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
     elsif firstParamUniType = new_t then
        ParseIdentifier( discard );
     else
        err(
            context => subprogramId,
            subjectNotes => em( "hashed_maps.map" ) & pl( " or " ) &
                em( "hashed_maps.cursor" ),
            reason => +"or compatible type expected but found",
            obstructor => token,
            obstructorType => identifiers( token ).kind,
            remedy => +"declare a new limited type"
        );
     end if;
  end if;

  -- The function result type depends on whether or not we have a map or cursor
  -- The result type must always be set because it's used during syntax checking.
  -- If there was an error, the id might be invalid and genKind2 undefined.

  if not error_found then
     if cursorId /= eof_t then
         kind := identifiers( cursorId ).genKind2;
     else
         kind := identifiers( mapId ).genKind2;
     end if;
  end if;

  if isExecutingCommand then
     begin
       if cursorId /= eof_t then
          findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
          result := String_Hashed_Maps.Element( theCursor.shmCursor );
       else
          findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
          result := String_Hashed_Maps.Element( theMap.shmMap, keyVal );
       end if;
     exception when constraint_error =>
       err_no_key( subprogramId, keyVal );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsElement;


------------------------------------------------------------------------------
--  LENGTH
--
-- Syntax: n := hashed_maps.length( m );
-- Ada:    n := hashed_maps.length( m );
------------------------------------------------------------------------------

procedure ParseHashedMapsLength( result : out unbounded_string; kind : out identifier ) is
  mapId   : identifier;
  theMap  : resPtr;
  subprogramId : constant identifier := hashed_maps_length_t;
begin
  kind := containers_count_type_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       result := to_unbounded_string( String_Hashed_Maps.Length( theMap.shmMap )'img );
     end;
  end if;
end ParseHashedMapsLength;


------------------------------------------------------------------------------
--  APPEND
--
-- Syntax: hashed_maps.append( m, k, e );
-- Ada:    N/A
------------------------------------------------------------------------------

procedure ParseHashedMapsAppend is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  elemVal : unbounded_string;
  elemType : identifier;
  subprogramId : constant identifier := hashed_maps_append_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  if getUniType( identifiers( mapId ).genKind2 ) /= uni_string_t then
     err( +"append requires a string element type" );
  end if;
  ParseNextGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastStringParameter( subprogramId, elemVal, elemType, identifiers( mapId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       Append( theMap.shmMap, keyVal, elemVal );
     exception when constraint_error =>
       err_no_key( subprogramId, keyVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsAppend;


------------------------------------------------------------------------------
--  PREPEND
--
-- Syntax: hashed_maps.prepend( m, k, e );
-- Ada:    N/A
------------------------------------------------------------------------------

procedure ParseHashedMapsPrepend is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  elemVal : unbounded_string;
  elemType : identifier;
  subprogramId : constant identifier := hashed_maps_prepend_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  if getUniType( identifiers( mapId ).genKind2 ) /= uni_string_t then
     err( +"prepend requires a string element type" );
  end if;
  ParseNextGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastStringParameter( subprogramId, elemVal, elemType, identifiers( mapId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       Prepend( theMap.shmMap, keyVal, elemVal );
     exception when constraint_error =>
       err_no_key( subprogramId, keyVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsPrepend;


------------------------------------------------------------------------------
--  INCREMENT
--
-- Syntax: hashed_maps.increment( m, k [, i] );
-- Ada:    N/A
------------------------------------------------------------------------------

procedure ParseHashedMapsIncrement is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal  : unbounded_string;
  keyType : identifier;
  incVal  : unbounded_string;
  incType : identifier;
  floatVal : numericValue;
  hasAmt  : boolean := false;
  subprogramId : constant identifier := hashed_maps_increment_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  if getUniType( identifiers( mapId ).genKind2 ) /= uni_numeric_t then
     err( +"increment requires a numeric element type" );
  end if;
  ParseNextGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  if token = symbol_t and identifiers( token ).value.all = "," then
     hasAmt := true;
     ParseLastNumericParameter( subprogramId, incVal, incType, identifiers( mapId ).genKind2 );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expect( symbol_t, ")" );
  else
     err( +", or ) expected" );
  end if;
  if isExecutingCommand then
     begin
       if hasAmt then
          floatVal := numericValue( natural( to_numeric( incVal ) ) );
       else
          floatVal := 1.0;
       end if;
     exception when constraint_error =>
       err( +"increment value is not natural" );
     end;
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       increment( theMap.shmMap, keyVal, floatVal );
     exception when constraint_error =>
       err_no_key( subprogramId, keyVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsIncrement;


------------------------------------------------------------------------------
--  DECREMENT
--
-- Syntax: hashed_maps.decrement( m, k [, d] );
-- Ada:    N/A
------------------------------------------------------------------------------

procedure ParseHashedMapsDecrement is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal  : unbounded_string;
  keyType : identifier;
  decVal  : unbounded_string;
  decType : identifier;
  floatVal : numericValue;
  hasAmt  : boolean := false;
  subprogramId : constant identifier := hashed_maps_decrement_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  if getUniType( identifiers( mapId ).genKind2 ) /= uni_numeric_t then
     err( +"decrement requires a numeric element type" );
  end if;
  ParseNextGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  if token = symbol_t and identifiers( token ).value.all = "," then
     hasAmt := true;
     ParseLastNumericParameter( subprogramId, decVal, decType, identifiers( mapId ).genKind2 );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expect( symbol_t, ")" );
  else
     err( +", or ) expected" );
  end if;
  if isExecutingCommand then
     begin
       if hasAmt then
          floatVal := numericValue( natural( to_numeric( decVal ) ) );
       else
          floatVal := 1.0;
       end if;
     exception when constraint_error =>
       err( +"decrement value is not natural" );
     end;
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       decrement( theMap.shmMap, keyVal, floatVal );
     exception when constraint_error =>
       err_no_key( subprogramId, keyVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsDecrement;


------------------------------------------------------------------------------
--  EXTRACT
--
-- Syntax: e := hashed_maps.extract( m, k );
-- Ada:    N/A (element + delete)
------------------------------------------------------------------------------

procedure ParseHashedMapsExtract( result : out unbounded_string; kind : out identifier ) is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  subprogramId : constant identifier := hashed_maps_extract_t;
begin
  -- in the event of an error, we don't know what the data type is
  kind := eof_t;
  expectAdaScript( subject => subprogramId, remedy => +"use element and delete" );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  ParseLastGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  if not error_found then
     kind := identifiers( mapId ).genKind2;
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       extract( theMap.shmMap, keyVal, result );
     exception when constraint_error =>
       err_no_key( subprogramId, keyVal );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsExtract;


------------------------------------------------------------------------------
--  ASSIGN
--
-- Syntax: hashed_maps.assign( t, s );
-- Ada:    hashed_maps.assign( t, s );
------------------------------------------------------------------------------

procedure ParseHashedMapsAssign is
  targetMapId   : identifier;
  sourceMapId   : identifier;
  targetMap  : resPtr;
  sourceMap  : resPtr;
  subprogramId : constant identifier := hashed_maps_assign_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, targetMapId, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( subprogramId, sourceMapId, hashed_maps_map_t );
  if not error_found then
     genTypesOk( identifiers( targetMapId ).genKind, identifiers( sourceMapId ).genKind );
     genTypesOk( identifiers( targetMapId ).genKind2, identifiers( sourceMapId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetMapId ).value.all ), targetMap );
       findResource( to_resource_id( identifiers( sourceMapId ).value.all ), sourceMap );
       String_Hashed_Maps.Assign( targetMap.shmMap, sourceMap.shmMap );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsAssign;


------------------------------------------------------------------------------
--  MOVE
--
-- Syntax: hashed_maps.move( t, s );
-- Ada:    hashed_maps.move( t, s );
------------------------------------------------------------------------------

procedure ParseHashedMapsMove is
  targetMapId   : identifier;
  sourceMapId   : identifier;
  targetMap  : resPtr;
  sourceMap  : resPtr;
  subprogramId : constant identifier := hashed_maps_move_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, targetMapId, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( subprogramId, sourceMapId, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetMapId ).value.all ), targetMap );
       findResource( to_resource_id( identifiers( sourceMapId ).value.all ), sourceMap );
       String_Hashed_Maps.Move( targetMap.shmMap, sourceMap.shmMap );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsMove;


------------------------------------------------------------------------------
--  FIRST
--
-- Syntax: hashed_maps.first( m, c );
-- Ada:    c := hashed_maps.first( m ); -- cannot return resource types
------------------------------------------------------------------------------

procedure ParseHashedMapsFirst is
  mapId : identifier;
  cursorId : identifier;
  --cursRef : reference;
  theMap : resPtr;
  theCursor : resPtr;
  subprogramId : constant identifier := hashed_maps_first_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( subprogramId, cursorId, hashed_maps_cursor_t );
  --ParseLastOutMapCursor( mapId, cursRef );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       theCursor.shmCursor := String_Hashed_Maps.First( theMap.shmMap );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsFirst;


------------------------------------------------------------------------------
--  NEXT
--
-- Syntax: hashed_maps.next( c );
-- Ada:    c := hashed_maps.next( c ); -- cannot return resource types
------------------------------------------------------------------------------

procedure ParseHashedMapsNext is
  cursorId : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := hashed_maps_next_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, cursorId, hashed_maps_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       theCursor.shmCursor := String_Hashed_Maps.Next( theCursor.shmCursor );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsNext;


------------------------------------------------------------------------------
--  KEY
--
-- Syntax: k := hashed_maps.key( c );
-- Ada:    k := hashed_maps.key( c );
------------------------------------------------------------------------------

procedure ParseHashedMapsKey( result : out unbounded_string; kind : out identifier ) is
  cursorId : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := hashed_maps_key_t;
begin
  -- in the event of an error, we don't know what the data type is
  kind := eof_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, cursorId, hashed_maps_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       result := String_Hashed_Maps.Key( theCursor.shmCursor );
     exception when constraint_error =>
       err( +"cursor position has no element" );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
  if not error_found then
     kind := identifiers( cursorId ).genKind;
  end if;
end ParseHashedMapsKey;


------------------------------------------------------------------------------
--  FIND
--
-- Syntax: hashed_maps.find( m, k, c );
-- Ada:    c := hashed_maps.find( m, k );
------------------------------------------------------------------------------

procedure ParseHashedMapsFind is
  cursorId : identifier;
  theCursor : resPtr;
  mapId : identifier;
  theMap : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  subprogramId : constant identifier := hashed_maps_find_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  ParseNextGenItemParameter( subprogramId, keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastInOutInstantiatedParameter( subprogramId, cursorId, hashed_maps_cursor_t );
 -- TODO: not really in out
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       theCursor.shmCursor := String_Hashed_Maps.Find( theMap.shmMap, keyVal );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsFind;


------------------------------------------------------------------------------
--  REPLACE ELEMENT
--
-- Syntax: hashed_maps.replace_element( m, c, e );
-- Ada:    hashed_maps.replace_element( m, c, e );
------------------------------------------------------------------------------

procedure ParseHashedMapsReplaceElement is
  cursorId : identifier;
  theCursor : resPtr;
  mapId : identifier;
  theMap : resPtr;
  newVal : unbounded_string;
  newType : identifier;
  subprogramId : constant identifier := hashed_maps_replace_element_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapId, hashed_maps_map_t );
  ParseNextInOutInstantiatedParameter( subprogramId, cursorId, hashed_maps_cursor_t );
  ParseLastGenItemParameter( subprogramId, newVal, newType, identifiers( mapId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       String_Hashed_Maps.Replace_Element( theMap.shmMap, theCursor.shmCursor , newVal );
     exception when constraint_error =>
       err( +"cursor position has no element" );
     when program_error =>
       err( +"cursor position not in map" );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsReplaceElement;


------------------------------------------------------------------------------
--  HAS ELEMENT
--
-- Syntax: b := hashed_maps.has_element( c );
-- Ada:    b := hashed_maps.has_element( c );
------------------------------------------------------------------------------

procedure ParseHashedMapsHasElement( result : out unbounded_string; kind : out identifier ) is
  cursorId  : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := hashed_maps_has_element_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, cursorId, hashed_maps_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       result := to_bush_boolean( String_Hashed_Maps.Has_Element( theCursor.shmCursor ) );
     end;
  end if;
end ParseHashedMapsHasElement;


------------------------------------------------------------------------------
--  EQUAL
--
-- Syntax: b := hashed_maps.equal( m1, m2 );
-- Ada:    b := m1 = m2;
------------------------------------------------------------------------------

procedure ParseHashedMapsEqual( result : out unbounded_string; kind : out identifier ) is
  leftMapId  : identifier;
  rightMapId : identifier;
  leftMap    : resPtr;
  rightMap   : resPtr;
  subprogramId : constant identifier := hashed_maps_equal_t;
  use String_Hashed_Maps;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, leftMapId, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( subprogramId, rightMapId, hashed_maps_map_t );
  if not error_found then
     genTypesOk( identifiers( leftMapId ).genKind, identifiers( rightMapId ).genKind );
     genTypesOk( identifiers( leftMapId ).genKind2, identifiers( rightMapId ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( leftMapId ).value.all ), leftMap );
       findResource( to_resource_id( identifiers( rightMapId ).value.all ), rightMap );
       result := to_bush_boolean( leftMap.shmMap = rightMap.shmMap );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsEqual;


------------------------------------------------------------------------------
--
-- HOUSEKEEPING
--
------------------------------------------------------------------------------


procedure StartupHMaps is
begin
  declareNamespace( "hashed_maps" );

  declareIdent( hashed_maps_map_t, "hashed_maps.map", variable_t, genericTypeClass );
  identifiers( hashed_maps_map_t ).usage := limitedUsage;
  identifiers( hashed_maps_map_t ).resource := true;

  declareIdent( hashed_maps_cursor_t, "hashed_maps.cursor", variable_t, genericTypeClass );
  identifiers( hashed_maps_cursor_t ).usage := limitedUsage;
  identifiers( hashed_maps_cursor_t ).resource := true;

  declareProcedure( hashed_maps_clear_t,     "hashed_maps.clear",    ParseHashedMapsClear'access );
  declareFunction(  hashed_maps_is_empty_t,  "hashed_maps.is_empty", ParseHashedMapsIsEmpty'access );
  declareFunction(  hashed_maps_capacity_t,  "hashed_maps.capacity", ParseHashedMapsCapacity'access );
  declareProcedure( hashed_maps_reserve_capacity_t, "hashed_maps.reserve_capacity",    ParseHashedMapsReserveCapacity'access );
  declareFunction(  hashed_maps_length_t,    "hashed_maps.length",   ParseHashedMapsLength'access );
  declareProcedure( hashed_maps_insert_t,    "hashed_maps.insert",   ParseHashedMapsInsert'access );
  declareProcedure( hashed_maps_include_t,   "hashed_maps.include",  ParseHashedMapsInclude'access );
  declareProcedure( hashed_maps_replace_t,   "hashed_maps.replace",  ParseHashedMapsReplace'access );
  declareProcedure( hashed_maps_exclude_t,   "hashed_maps.exclude",  ParseHashedMapsExclude'access );
  declareProcedure( hashed_maps_delete_t,    "hashed_maps.delete",   ParseHashedMapsDelete'access );
  declareFunction(  hashed_maps_contains_t,  "hashed_maps.contains", ParseHashedMapsContains'access );
  declareFunction(  hashed_maps_element_t,   "hashed_maps.element",  ParseHashedMapsElement'access );
  declareProcedure( hashed_maps_append_t,    "hashed_maps.append",   ParseHashedMapsAppend'access );
  declareProcedure( hashed_maps_prepend_t,   "hashed_maps.prepend",  ParseHashedMapsPrepend'access );
  declareProcedure( hashed_maps_increment_t, "hashed_maps.increment",  ParseHashedMapsIncrement'access );
  declareProcedure( hashed_maps_decrement_t, "hashed_maps.decrement",  ParseHashedMapsDecrement'access );
  declareFunction(  hashed_maps_extract_t,   "hashed_maps.extract",  ParseHashedMapsExtract'access );
  declareProcedure( hashed_maps_assign_t,    "hashed_maps.assign",   ParseHashedMapsAssign'access );
  declareProcedure( hashed_maps_move_t,      "hashed_maps.move",     ParseHashedMapsMove'access );
  declareProcedure( hashed_maps_first_t,     "hashed_maps.first",    ParseHashedMapsFirst'access );
  declareProcedure( hashed_maps_next_t,      "hashed_maps.next",     ParseHashedMapsNext'access );
  declareFunction(  hashed_maps_key_t,       "hashed_maps.key",      ParseHashedMapsKey'access );
  declareProcedure( hashed_maps_find_t,      "hashed_maps.find",     ParseHashedMapsFind'access );
  declareProcedure( hashed_maps_replace_element_t, "hashed_maps.replace_element",     ParseHashedMapsReplaceElement'access );
  declareFunction(  hashed_maps_has_element_t, "hashed_maps.has_element",     ParseHashedMapsHasElement'access );
  declareFunction(  hashed_maps_equal_t,     "hashed_maps.equal",    ParseHashedMapsEqual'access );

  declareNamespaceClosed( "hashed_maps" );
end StartupHMaps;

procedure ShutdownHMaps is
begin
  null;
end ShutdownHMaps;

end parser_hmaps;
