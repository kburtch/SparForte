------------------------------------------------------------------------------
-- Hashed Maps Package Parser                                               --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2026 Free Software Foundation              --
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

-- with text_io;use text_io;

with
    Ada.Containers,
    ada.strings.unbounded,
    gnat.source_info,
    pegasoft.strings,
    pegasoft.user_io,
    pegasoft.hmaps,
    world,
    symbol_table,
    message_strings,
    value_conversion,
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
    symbol_table,
    message_strings,
    value_conversion,
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

procedure ParseOutMapCursor( subprogramId : identifier; mapId : identifier; cursRef : in out reference ) is
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
        --
        -- If the identifier already has a resource, don't create a second one
        -- to avoid a memory leak in the resource table.

        if not identifiers( cursRef.id ).resource then
           declareResource( resId, storage_hashed_map_cursor, getIdentifierBlock( cursRef.id ) );
           identifiers( cursRef.id ).sStorage.value := to_unbounded_string( resId );
           identifiers( cursRef.id ).store := identifiers( cursRef.id ).sStorage'access;
           identifiers( cursRef.id ).resource := true;
        end if;
     end if;
  else
     ParseInOutInstantiatedParameter( subprogramId, cursRef, hashed_maps_cursor_t );

     -- Check the type against the map

     if not error_found then
        genTypesOk( identifiers( mapId ).genKind, identifiers( cursRef.Id ).genKind );
        genTypesOk( identifiers( mapId ).genKind2, identifiers( cursRef.Id ).genKind2 );
     end if;
  end if;
  --expect( symbol_t, ")" );
end ParseOutMapCursor;

procedure ParseNextOutMapCursor( subprogramId : identifier; mapId : identifier; cursRef : in out reference ) is
begin
  expectParameterComma;
  ParseOutMapCursor( subprogramId, mapId, cursRef );
end ParseNextOutMapCursor;


------------------------------------------------------------------------------
--  CLEAR
--
-- Syntax: hashed_maps.clear( m );
-- Ada:    hashed_maps.clear( m );
-- Delete the contents of map m.
------------------------------------------------------------------------------

procedure ParseHashedMapsClear is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  subprogramId : constant identifier := hashed_maps_clear_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       Storage_Hashed_Maps.Clear( theMap.shmMap );
     end;
  end if;
end ParseHashedMapsClear;


------------------------------------------------------------------------------
--  IS EMPTY
--
-- Syntax: b := hashed_maps.is_empty( m );
-- Ada:    b := hashed_maps.is_empty( m );
-- Return true if the map has no elements.
------------------------------------------------------------------------------

procedure ParseHashedMapsIsEmpty( result : out storage; kind : out identifier ) is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  subprogramId : constant identifier := hashed_maps_is_empty_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       result := storage'( to_spar_boolean( Storage_Hashed_Maps.Is_Empty( theMap.shmMap ) ), noMetaLabel, noMetaLabels );
     end;
  end if;
end ParseHashedMapsIsEmpty;


------------------------------------------------------------------------------
--  CAPACITY
--
-- Syntax: c := hashed_maps.capacity( m );
-- Ada:    c := hashed_maps.capacity( m );
-- How much spare item space exists in the map.
------------------------------------------------------------------------------

procedure ParseHashedMapsCapacity( result : out storage; kind : out identifier ) is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  subprogramId : constant identifier := hashed_maps_capacity_t;
begin
  kind := containers_count_type_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       result := storage'( to_unbounded_string( Storage_Hashed_Maps.Capacity( theMap.shmMap )'img ), noMetaLabel, noMetaLabels );
     end;
  end if;
end ParseHashedMapsCapacity;


------------------------------------------------------------------------------
--  RESERVE CAPACITY
--
-- Syntax: hashed_maps.reserve_capacity( m, c );
-- Ada:    hashed_maps.reserve_capacity( m, c );
-- Add enough spare item space in the map to store at least c items.
------------------------------------------------------------------------------

procedure ParseHashedMapsReserveCapacity is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  capExpr : storage;
  capType : identifier;
  subprogramId : constant identifier := hashed_maps_reserve_capacity_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  ParseLastNumericParameter( subprogramId, capExpr, capType, containers_count_type_t );
  if isExecutingCommand then
     declare
       cap : constant Ada.Containers.Count_Type := Ada.Containers.Count_Type( to_numeric( capExpr.value ) );
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       Storage_Hashed_Maps.Reserve_Capacity( theMap.shmMap, cap );
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
-- Insert the value v under key k in the hash map. If b exists, b is true if
-- the insert was successful. If value e exists, set the value to e. If c
-- exists, move the cursor to the insert position. If the key exists, raise an
-- exception.
------------------------------------------------------------------------------

procedure ParseHashedMapsInsert is
  mapRef    : reference;
  theMap    : resPtr;
  mapResId  : storage;
  keyExpr   : storage;
  keyType   : identifier;
  elemExpr  : storage;
  elemType  : identifier;
  cursorRef : reference;
  theCursor : resPtr;
  insertRef : reference;
  version   : natural := 1;
  result    : boolean;
  subprogramId : constant identifier := hashed_maps_insert_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  ParseNextGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  expectParameterComma;

  -- There are 3 variants to an insert

  -- If a token is undefind, we have to assume it is an out mode cursor.

  if identifiers( token ).kind = new_t or else getUniType( identifiers( token ).kind ) = hashed_maps_cursor_t then
     ParseOutMapCursor( subprogramId, mapRef.Id, cursorRef );
     --ParseInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
     ParseLastOutParameter( subprogramId, insertRef, boolean_t );
     version := 2;
  else
    ParseGenItemParameter( elemExpr, elemType, identifiers( mapRef.Id ).genKind2 );
    if token = symbol_t and identifiers( token ).store.value = ")" then
       expect( symbol_t, ")" );
    else
       ParseNextOutMapCursor( subprogramId, mapRef.Id, cursorRef );
       --ParseNextInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
       ParseLastOutParameter( subprogramId, insertRef, boolean_t );
       version := 3;
    end if;
  end if;
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );

       case version is
       -- ( m, k, e )
       when 1 =>
          -- assuming the key and value can fall under different policies
          if metaLabelOK( subprogramId, keyExpr ) and metaLabelOK( subprogramId, elemExpr ) then
             Storage_Hashed_Maps.Insert( theMap.shmMap, keyExpr, elemExpr );
          end if;
       -- (m, k, p, b )
       -- meta data labels require inserted values to have defined values so
       -- null storage is used.
       when 2 =>
          findResource( to_resource_id( identifiers( cursorRef.id ).store.value ), theCursor );
          if metaLabelOK( subprogramId, keyExpr ) then
             Storage_Hashed_Maps.Insert( theMap.shmMap, keyExpr, nullStorage, theCursor.shmCursor,
                result );
             AssignParameter( insertRef, storage'( to_spar_boolean( result ), noMetaLabel, noMetaLabels ) );
          end if;
       -- (m, k, e, p, b )
       when 3 =>
          findResource( to_resource_id( identifiers( cursorRef.Id ).store.value ), theCursor );
          -- assuming the key and value can fall under different policies
          if metaLabelOK( subprogramId, keyExpr ) and metaLabelOK( subprogramId, elemExpr ) then
             Storage_Hashed_Maps.Insert( theMap.shmMap, keyExpr, elemExpr,
                theCursor.shmCursor, result );
          end if;
          AssignParameter( insertRef, storage'( to_spar_boolean( result ), noMetaLabel, noMetaLabels ) );
       when others =>
          put_line_retry( gnat.source_info.source_location &
             ": internal error: unknown insert version" );
       end case;

     exception when constraint_error =>
       err_key_exists( subprogramId, keyExpr.value );
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
-- Insert the value v under key k in the hash map. If the key exists, the value
-- is overwritten.
------------------------------------------------------------------------------

procedure ParseHashedMapsInclude is
  mapRef    : reference;
  theMap    : resPtr;
  mapResId  : storage;
  keyExpr   : storage;
  keyType   : identifier;
  elemExpr  : storage;
  elemType  : identifier;
  oldElem   : storage;
  subprogramId : constant identifier := hashed_maps_include_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  ParseNextGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  ParseLastGenItemParameter( subprogramId, elemExpr, elemType, identifiers( mapRef.Id ).genKind2 );
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       -- the key, value and existing value must all be checked. The map
       -- may also be empty but existence is optional.
       if Storage_Hashed_Maps.Contains( theMap.shmMap, keyExpr ) then
          oldElem := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
          if metaLabelOK( subprogramId, oldElem ) and
             metaLabelOK( subprogramId, keyExpr ) and metaLabelOK( subprogramId, elemExpr ) then
             Storage_Hashed_Maps.Include( theMap.shmMap, keyExpr, elemExpr );
          end if;
       elsif metaLabelOK( subprogramId, keyExpr ) and metaLabelOK( subprogramId, elemExpr ) then
          Storage_Hashed_Maps.Include( theMap.shmMap, keyExpr, elemExpr );
       end if;
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
-- Replace the value v under key k in the hash map. If the key does not exist,
-- raise an exception.
------------------------------------------------------------------------------

procedure ParseHashedMapsReplace is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId : storage;
  keyExpr : storage;
  keyType : identifier;
  elemExpr : storage;
  elemType : identifier;
  oldElem  : storage;
  subprogramId : constant identifier := hashed_maps_replace_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  ParseNextGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  ParseLastGenItemParameter( subprogramId, elemExpr, elemType, identifiers( mapRef.Id ).genKind2 );
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       -- the key, value and existing value must all be checked.
       oldElem := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
       if metaLabelOK( subprogramId, oldElem ) and
          metaLabelOK( subprogramId, keyExpr ) and metaLabelOK( subprogramId, elemExpr ) then
          Storage_Hashed_Maps.Replace( theMap.shmMap, keyExpr, elemExpr );
       end if;
     exception when constraint_error =>
       err_no_key( subprogramId, keyExpr.value );
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
-- Delete the value v under key k in the hash map. If the key does not exist,
-- do nothing.
------------------------------------------------------------------------------

procedure ParseHashedMapsExclude is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  keyExpr : storage;
  keyType : identifier;
  oldElem : storage;
  subprogramId : constant identifier := hashed_maps_exclude_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  ParseLastGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       -- the key, value and existing value must all be checked.
       oldElem := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
       if metaLabelOK( subprogramId, oldElem ) and metaLabelOK( subprogramId, keyExpr ) then
          Storage_Hashed_Maps.Exclude( theMap.shmMap, keyExpr );
       end if;
     exception when storage_error =>
       err_storage;
     when constraint_error =>
       null;  -- no key? do nothing
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
-- Remove a key and the associated element from the hash map. If a cursor is
-- given instead of a key, delete the item at the cursor position.
------------------------------------------------------------------------------

procedure ParseHashedMapsDelete is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  keyExpr : storage;
  keyType : identifier;
  cursorRef : reference;
  theCursor : resPtr;
  cursResId : storage;
  oldElem : storage;
  subprogramId : constant identifier := hashed_maps_delete_t;
begin
  cursorRef.id := eof_t;
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  expectParameterComma;
  if identifiers( token ).class = varClass and then
     getUniType( identifiers( token ).kind ) = hashed_maps_cursor_t then
     ParseInOutInstantiatedParameter( subprogramId, cursorRef, hashed_maps_cursor_t );
  else
     ParseGenItemParameter( keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       if cursorRef.Id = eof_t then
          -- the key and existing value must be checked. The map
          -- may also be empty or the key may not exist.
          oldElem := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
          if metaLabelOK( subprogramId, oldElem ) and metaLabelOK( subprogramId, keyExpr ) then
             Storage_Hashed_Maps.Delete( theMap.shmMap, keyExpr );
          end if;
       else
          getParameterValue( cursorRef, cursResId );
          findResource( to_resource_id( cursResId.value ), theCursor );
          -- the existing value must be checked
          oldElem := Storage_Hashed_Maps.Element( theCursor.shmCursor );
          if metaLabelOK( subprogramId, oldElem ) then
             Storage_Hashed_Maps.Delete( theMap.shmMap, theCursor.shmCursor );
          end if;
       end if;
     exception when constraint_error =>
       err_no_key( subprogramId, keyExpr.value );
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
-- Return true if key k is in the map m.
-- Does not take into account the data meta label.  Result meta labels not
-- affected by the data found.
------------------------------------------------------------------------------

procedure ParseHashedMapsContains( result : out storage; kind : out identifier ) is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  keyExpr : storage;
  keyType : identifier;
  subprogramId : constant identifier := hashed_maps_contains_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  ParseLastGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       result := storage'(
          to_spar_boolean(
             Storage_Hashed_Maps.Contains( theMap.shmMap, keyExpr )
          ),
          noMetaLabel, noMetaLabels );
     end;
  end if;
end ParseHashedMapsContains;


------------------------------------------------------------------------------
--  ELEMENT
--
-- Syntax: e := hashed_maps.element( m, k ) | element(c)
-- Ada:    e := hashed_maps.element( m, k ); | element(c)
-- Return the value for key k in map m, or the value at cursor c.
------------------------------------------------------------------------------

procedure ParseHashedMapsElement( result : out storage; kind : out identifier ) is
  mapRef    : reference;
  theMap    : resPtr;
  mapResId  : storage;
  keyExpr   : storage;
  keyType   : identifier;
  cursorRef : reference;
  theCursor : resPtr;
  cursResId : storage;
  firstParamUniType : identifier;
  oldElem   : storage;
  discard   : identifier;
  subprogramId : constant identifier := hashed_maps_element_t;
begin
  -- in the event of an error, we don't know what the data type is
  kind := eof_t;
  cursorRef.id := eof_t;

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
        ParseInOutInstantiatedParameter( subprogramId, cursorRef, hashed_maps_cursor_t );
        expect( symbol_t, ")" );
     elsif firstParamUniType = hashed_maps_map_t then
        ParseInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
        ParseLastGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
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
     if cursorRef.Id /= eof_t then
         kind := identifiers( cursorRef.Id ).genKind2;
     else
         kind := identifiers( mapRef.Id ).genKind2;
     end if;
  end if;

  if isExecutingCommand then
     begin
       if cursorRef.Id /= eof_t then
          getParameterValue( cursorRef, cursResId );
          findResource( to_resource_id( cursResId.value ), theCursor );
          oldElem := Storage_Hashed_Maps.Element( theCursor.shmCursor );
          if metaLabelOK( subprogramId, oldElem ) then
             result := oldElem;
          end if;
       else
          getParameterValue( mapRef, mapResId );
          findResource( to_resource_id( mapResId.value ), theMap );
          oldElem := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
          if metaLabelOK( subprogramId, keyExpr ) and metaLabelOK( subprogramId, oldElem ) then
             result := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
          end if;
       end if;
     exception when constraint_error =>
       err_no_key( subprogramId, keyExpr.value );
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
-- Return the number of elements in the map.
------------------------------------------------------------------------------

procedure ParseHashedMapsLength( result : out storage; kind : out identifier ) is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  subprogramId : constant identifier := hashed_maps_length_t;
begin
  kind := containers_count_type_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       result := storage'( to_unbounded_string( Storage_Hashed_Maps.Length( theMap.shmMap )'img ), noMetaLabel, noMetaLabels );
     end;
  end if;
end ParseHashedMapsLength;


------------------------------------------------------------------------------
--  APPEND
--
-- Syntax: hashed_maps.append( m, k, e );
-- Ada:    N/A
-- Append string element e to the value under key k in the hash map. If the key
-- does not exist, raise an exception.
------------------------------------------------------------------------------

procedure ParseHashedMapsAppend is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  keyExpr : storage;
  keyType : identifier;
  elemExpr : storage;
  elemType : identifier;
  subprogramId : constant identifier := hashed_maps_append_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  if getUniType( identifiers( mapRef.Id ).genKind2 ) /= uni_string_t then
     err( +"append requires a string element type" );
  end if;
  ParseNextGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  ParseLastStringParameter( subprogramId, elemExpr, elemType, identifiers( mapRef.Id ).genKind2 );
  if isExecutingCommand then
     declare
       original : storage;
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       -- Append( theMap.shmMap, keyExpr, elemExpr );
       original := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
       -- labels must be the same for the original value and the appending value
       if metaLabelOk( subprogramId, elemExpr, original ) then
          original.value := original.value & elemExpr.value;
          Storage_Hashed_Maps.Include( theMap.shmMap, keyExpr, original );
       end if;
     exception when constraint_error =>
       err_no_key( subprogramId, keyExpr.value );
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
-- Prepend string element e to the value under key k in the hash table. If the
-- key does not exist, raise an exception.
------------------------------------------------------------------------------

procedure ParseHashedMapsPrepend is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  keyExpr : storage;
  keyType : identifier;
  elemExpr : storage;
  elemType : identifier;
  subprogramId : constant identifier := hashed_maps_prepend_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  if getUniType( identifiers( mapRef.Id ).genKind2 ) /= uni_string_t then
     err( +"prepend requires a string element type" );
  end if;
  ParseNextGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  ParseLastStringParameter( subprogramId, elemExpr, elemType, identifiers( mapRef.Id ).genKind2 );
  if isExecutingCommand then
     declare
       original : storage;
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       -- Prepend( theMap.shmMap, keyExpr, elemExpr );
       original := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
       if metaLabelOk( subprogramId, elemExpr, original ) then
          original.value := elemExpr.value & original.value;
          Storage_Hashed_Maps.Include( theMap.shmMap, keyExpr, original );
       end if;
     exception when constraint_error =>
       err_no_key( subprogramId, keyExpr.value );
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
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  keyExpr  : storage;
  keyType : identifier;
  incExpr  : storage;
  incType : identifier;
  floatVal : numericValue;
  hasAmt  : boolean := false;
  subprogramId : constant identifier := hashed_maps_increment_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  if getUniType( identifiers( mapRef.Id ).genKind2 ) /= uni_numeric_t then
     err( +"increment requires a numeric element type" );
  end if;
  ParseNextGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  if token = symbol_t and identifiers( token ).store.value = "," then
     hasAmt := true;
     ParseLastNumericParameter( subprogramId, incExpr, incType, identifiers( mapRef.Id ).genKind2 );
  elsif token = symbol_t and identifiers( token ).store.value = ")" then
     expect( symbol_t, ")" );
  else
     err( +", or ) expected" );
  end if;
  if isExecutingCommand then
     begin
       if hasAmt then
          floatVal := numericValue( natural( to_numeric( incExpr.value ) ) );
       else
          floatVal := 1.0;
       end if;
     exception when constraint_error =>
       err( +"increment value is not natural" );
     end;
     declare
       original : storage;
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       -- increment( theMap.shmMap, keyExpr, floatVal );
       original := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
       floatVal := numericValue( to_numeric( original.value ) ) + floatVal;
       if hasAmt then
          if metaLabelOk( subprogramId, keyExpr ) and metaLabelOk( subprogramId, incExpr, original ) then
             original.value := to_unbounded_string( floatVal'img ) ;
             Storage_Hashed_Maps.Include( theMap.shmMap, keyExpr, original );
          end if;
       else
          if metaLabelOk( subprogramId, keyExpr ) and metaLabelOk( subprogramId, original ) then
             original.value := to_unbounded_string( floatVal'img ) ;
             Storage_Hashed_Maps.Include( theMap.shmMap, keyExpr, original );
          end if;
       end if;
     exception when constraint_error =>
       err_no_key( subprogramId, keyExpr.value );
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
  mapRef  : reference;
  theMap  : resPtr;
  mapResId : storage;
  keyExpr  : storage;
  keyType : identifier;
  decExpr  : storage;
  decType : identifier;
  floatVal : numericValue;
  hasAmt  : boolean := false;
  subprogramId : constant identifier := hashed_maps_decrement_t;
begin
  expectAdaScript( subject => subprogramId, remedy => +"use element and replace_element" );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  if getUniType( identifiers( mapRef.Id ).genKind2 ) /= uni_numeric_t then
     err( +"decrement requires a numeric element type" );
  end if;
  ParseNextGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  if token = symbol_t and identifiers( token ).store.value = "," then
     hasAmt := true;
     ParseLastNumericParameter( subprogramId, decExpr, decType, identifiers( mapRef.Id ).genKind2 );
  elsif token = symbol_t and identifiers( token ).store.value = ")" then
     expect( symbol_t, ")" );
  else
     err( +", or ) expected" );
  end if;
  if isExecutingCommand then
     begin
       if hasAmt then
          floatVal := numericValue( natural( to_numeric( decExpr.value ) ) );
       else
          floatVal := 1.0;
       end if;
     exception when constraint_error =>
       err( +"decrement value is not natural" );
     end;
     declare
       original : storage;
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       -- decrement( theMap.shmMap, keyExpr, floatVal );
       original := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
       floatVal := numericValue( to_numeric( original.value ) ) - floatVal;
       if hasAmt then
          if metaLabelOk( subprogramId, decExpr, original ) then
             original.value := to_unbounded_string( floatVal'img ) ;
             Storage_Hashed_Maps.Include( theMap.shmMap, keyExpr, original );
          end if;
       else
          if metaLabelOk( subprogramId, original ) then
             original.value := to_unbounded_string( floatVal'img ) ;
             Storage_Hashed_Maps.Include( theMap.shmMap, keyExpr, original );
          end if;
       end if;
     exception when constraint_error =>
       err_no_key( subprogramId, keyExpr.value );
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
-- Like remove, delete the value v under key k in the hash map but return it.
-- If the key does not exist, raise an exception.
------------------------------------------------------------------------------

procedure ParseHashedMapsExtract( result : out storage; kind : out identifier ) is
  mapRef  : reference;
  theMap  : resPtr;
  mapResId: storage;
  keyExpr : storage;
  keyType : identifier;
  oldElem : storage;
  subprogramId : constant identifier := hashed_maps_extract_t;
begin
  -- in the event of an error, we don't know what the data type is
  kind := eof_t;
  expectAdaScript( subject => subprogramId, remedy => +"use element and delete" );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  ParseLastGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  if not error_found then
     kind := identifiers( mapRef.Id ).genKind2;
  end if;
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       oldElem := Storage_Hashed_Maps.Element( theMap.shmMap, keyExpr );
       if metaLabelOK( subprogramId, oldElem ) and metaLabelOK( subprogramId, keyExpr ) then
          result := oldElem;
          Storage_Hashed_Maps.Delete( theMap.shmMap, keyExpr );
       end if;
     exception when constraint_error =>
       err_no_key( subprogramId, keyExpr.value );
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
-- Assign source map s to target map t, overwriting the contents of t. s is
-- unchanged.
-- Does not take into account data meta label
------------------------------------------------------------------------------

procedure ParseHashedMapsAssign is
  targetmapRef  : reference;
  sourcemapRef  : reference;
  targetMap  : resPtr;
  sourceMap  : resPtr;
  targetMapResId : storage;
  sourceMapResId : storage;
  subprogramId : constant identifier := hashed_maps_assign_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, targetMapRef, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( subprogramId, sourceMapRef, hashed_maps_map_t );
  if not error_found then
     genTypesOk( identifiers( targetMapRef.Id ).genKind, identifiers( sourceMapRef.Id ).genKind );
     genTypesOk( identifiers( targetMapRef.Id ).genKind2, identifiers( sourceMapRef.Id ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       getParameterValue( targetMapRef, targetMapResId );
       findResource( to_resource_id( targetMapResId.value ), targetMap );
       getParameterValue( sourceMapRef, sourceMapResId );
       findResource( to_resource_id( sourceMapResId.value ), sourceMap );
       Storage_Hashed_Maps.Assign( targetMap.shmMap, sourceMap.shmMap );
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
  targetMapRef  : reference;
  sourceMapRef  : reference;
  targetMap  : resPtr;
  sourceMap  : resPtr;
  targetMapResId : storage;
  sourceMapResId : storage;
  subprogramId : constant identifier := hashed_maps_move_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, targetMapRef, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( subprogramId, sourceMapRef, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       getParameterValue( targetMapRef, targetMapResId );
       findResource( to_resource_id( targetMapResId.value ), targetMap );
       getParameterValue( sourceMapRef, sourceMapResId );
       findResource( to_resource_id( sourceMapResId.value ), sourceMap );
       Storage_Hashed_Maps.Move( targetMap.shmMap, sourceMap.shmMap );
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
  mapRef: reference;
  cursorRef : reference;
  theMap : resPtr;
  mapResId : storage;
  theCursor : resPtr;
  cursResId : storage;
  subprogramId : constant identifier := hashed_maps_first_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( subprogramId, cursorRef, hashed_maps_cursor_t );
  --ParseLastOutMapCursor( mapId, cursRef );
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       getParameterValue( cursorRef, cursResId );
       findResource( to_resource_id( cursResId.value ), theCursor );
       theCursor.shmCursor := Storage_Hashed_Maps.First( theMap.shmMap );
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
  cursorRef : reference;
  theCursor : resPtr;
  cursResId : storage;
  subprogramId : constant identifier := hashed_maps_next_t;
begin
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, cursorRef, hashed_maps_cursor_t );
  if isExecutingCommand then
     begin
       getParameterValue( cursorRef, cursResId );
       findResource( to_resource_id( cursResId.value ), theCursor );
       theCursor.shmCursor := Storage_Hashed_Maps.Next( theCursor.shmCursor );
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
-- Return the key at the cursor c. If there is no key, raise an exception.
------------------------------------------------------------------------------

procedure ParseHashedMapsKey( result : out storage; kind : out identifier ) is
  cursorRef : reference;
  theCursor : resPtr;
  cursResId : storage;
  oldElem   : storage;
  subprogramId : constant identifier := hashed_maps_key_t;
begin
  -- in the event of an error, we don't know what the data type is
  kind := eof_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, cursorRef, hashed_maps_cursor_t );
  if isExecutingCommand then
     begin
       getParameterValue( cursorRef, cursResId );
       findResource( to_resource_id( cursResId.value ), theCursor );
       oldElem := Storage_Hashed_Maps.Key( theCursor.shmCursor );
       if metaLabelOk( subprogramId, oldElem ) then
          result := oldElem;
       end if;
     exception when constraint_error =>
       err( +"cursor position has no element" );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
  if not error_found then
     kind := identifiers( cursorRef.Id ).genKind;
  end if;
end ParseHashedMapsKey;


------------------------------------------------------------------------------
--  FIND
--
-- Syntax: hashed_maps.find( m, k, c );
-- Ada:    c := hashed_maps.find( m, k );
-- Move the cursor to the position in the map for key k. If the key does not
-- exist, the cursor will have no element.
------------------------------------------------------------------------------

procedure ParseHashedMapsFind is
  cursorRef : reference;
  theCursor : resPtr;
  cursResId : storage;
  mapRef: reference;
  theMap : resPtr;
  mapResId : storage;
  keyExpr : storage;
  keyType : identifier;
  subprogramId : constant identifier := hashed_maps_find_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  ParseNextGenItemParameter( subprogramId, keyExpr, keyType, identifiers( mapRef.Id ).genKind );
  ParseLastInOutInstantiatedParameter( subprogramId, cursorRef, hashed_maps_cursor_t );
 -- TODO: not really in out
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       getParameterValue( cursorRef, cursResId );
       findResource( to_resource_id( cursResId.value ), theCursor );
       theCursor.shmCursor := Storage_Hashed_Maps.Find( theMap.shmMap, keyExpr );
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
-- Replace the element at cursor position c in the hash map. If the key does
-- not exist, raise an exception.
------------------------------------------------------------------------------

procedure ParseHashedMapsReplaceElement is
  cursorRef : reference;
  theCursor : resPtr;
  cursResId : storage;
  mapRef: reference;
  mapResId : storage;
  theMap : resPtr;
  newExpr : storage;
  newType : identifier;
  subprogramId : constant identifier := hashed_maps_replace_element_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, mapRef, hashed_maps_map_t );
  ParseNextInOutInstantiatedParameter( subprogramId, cursorRef, hashed_maps_cursor_t );
  ParseLastGenItemParameter( subprogramId, newExpr, newType, identifiers( mapRef.Id ).genKind );
  if isExecutingCommand then
     begin
       getParameterValue( mapRef, mapResId );
       findResource( to_resource_id( mapResId.value ), theMap );
       getParameterValue( cursorRef, cursResId );
       findResource( to_resource_id( cursResId.value ), theCursor );
       if metaLabelOK( subprogramId, Storage_Hashed_Maps.Element( theCursor.shmCursor ), newExpr ) then
          Storage_Hashed_Maps.Replace_Element( theMap.shmMap, theCursor.shmCursor , newExpr );
       end if;
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
-- True if item under cursor has a value.
-- Does not take into account the data meta label.  Result meta labels not
-- affected by the data found.
------------------------------------------------------------------------------

procedure ParseHashedMapsHasElement( result : out storage; kind : out identifier ) is
  cursorRef : reference;
  theCursor : resPtr;
  cursResId : storage;
  subprogramId : constant identifier := hashed_maps_has_element_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleInOutInstantiatedParameter( subprogramId, cursorRef, hashed_maps_cursor_t );
  if isExecutingCommand then
     begin
       getParameterValue( cursorRef, cursResId );
       findResource( to_resource_id( cursResId.value ), theCursor );
       result := storage'( to_spar_boolean( Storage_Hashed_Maps.Has_Element( theCursor.shmCursor ) ), noMetaLabel, noMetaLabels );
     end;
  end if;
end ParseHashedMapsHasElement;


------------------------------------------------------------------------------
--  EQUAL
--
-- Syntax: b := hashed_maps.equal( m1, m2 );
-- Ada:    b := m1 = m2;
-- True if the two maps have identical content.  Also checks meta data labels.
------------------------------------------------------------------------------

procedure ParseHashedMapsEqual( result : out storage; kind : out identifier ) is
  leftMapRef : reference;
  rightMapRef: reference;
  leftMap    : resPtr;
  rightMap   : resPtr;
  leftMapResId : storage;
  rightMapResId : storage;
  subprogramId : constant identifier := hashed_maps_equal_t;
  use Storage_Hashed_Maps;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, leftMapRef, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( subprogramId, rightMapRef, hashed_maps_map_t );
  if not error_found then
     genTypesOk( identifiers( leftMapRef.Id ).genKind, identifiers( rightMapRef.Id ).genKind );
     genTypesOk( identifiers( leftMapRef.Id ).genKind2, identifiers( rightMapRef.Id ).genKind2 );
  end if;
  if isExecutingCommand then
     begin
       getParameterValue( leftMapRef, leftMapResId );
       findResource( to_resource_id( leftMapResId.value ), leftMap );
       getParameterValue( rightMapRef, rightMapResId );
       findResource( to_resource_id( rightMapResId.value ), rightMap );
       result := storage'( to_spar_boolean( leftMap.shmMap = rightMap.shmMap ), noMetaLabel, noMetaLabels );
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
