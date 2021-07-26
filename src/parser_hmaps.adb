------------------------------------------------------------------------------
-- Hashed Maps Package Parser                                               --
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
    gnat.source_info,
    pegasoft.strings,
    pegasoft.user_io,
    pegasoft.hmaps,
    world,
    scanner,
    scanner_res,
    scanner_restypes,
    parser,
    parser_params,
    parser_containers;
use
    ada.strings.unbounded,
    world,
    pegasoft.strings,
    pegasoft.user_io,
    pegasoft.hmaps,
    scanner,
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
hashed_maps_set_t      : identifier;
hashed_maps_include_t  : identifier;
hashed_maps_replace_t  : identifier;
hashed_maps_exclude_t  : identifier;
hashed_maps_remove_t   : identifier;
hashed_maps_contains_t : identifier;
hashed_maps_element_t  : identifier;
hashed_maps_get_t      : identifier;
hashed_maps_add_t      : identifier;
hashed_maps_append_t   : identifier;
hashed_maps_prepend_t  : identifier;
hashed_maps_increment_t : identifier;
hashed_maps_decrement_t : identifier;
hashed_maps_extract_t   : identifier;
hashed_maps_assign_t    : identifier;
hashed_maps_move_t      : identifier;
hashed_maps_copy_t      : identifier;
hashed_maps_first_t     : identifier;
hashed_maps_next_t      : identifier;
hashed_maps_key_t       : identifier;
hashed_maps_find_t      : identifier;
hashed_maps_replace_element_t :identifier;
hashed_maps_has_element_t : identifier;


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
--  ERR NO KEY
--
------------------------------------------------------------------------------

procedure err_no_key( keyName : unbounded_string ) is
begin
  err( "map does not have the key " &
      optional_yellow( toSecureData( to_string( ToEscaped( keyName ) ) ) ) );
end err_no_key;


------------------------------------------------------------------------------
--  ERR KEY EXISTS
--
------------------------------------------------------------------------------

procedure err_key_exists( keyName : unbounded_string ) is
begin
  err( "map already has the key " &
      optional_yellow( toSecureData( to_string( ToEscaped( keyName ) ) ) ) );
end err_key_exists;


------------------------------------------------------------------------------
--  ERR KEY EXISTS
--
------------------------------------------------------------------------------

procedure err_ada95( subName : string ) is
begin
  if onlyAda95 then
     err( optional_bold( "pragma ada_95" ) & " doesn't allow " & subName );
  end if;
end err_ada95;


------------------------------------------------------------------------------
--  CLEAR
--
-- Syntax: hashed_maps.clear( m );
-- Ada:    hashed_maps.clear( m );
------------------------------------------------------------------------------

procedure ParseHashedMapsClear is

  mapId   : identifier;
  theMap  : resPtr;
begin
  expect( hashed_maps_clear_t );
  ParseSingleInOutInstantiatedParameter( mapId, hashed_maps_map_t );
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
begin
  kind := boolean_t;
  expect( hashed_maps_is_empty_t );
  ParseSingleInOutInstantiatedParameter( mapId, hashed_maps_map_t );
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
begin
  kind := containers_count_type_t;
  expect( hashed_maps_capacity_t );
  ParseSingleInOutInstantiatedParameter( mapId, hashed_maps_map_t );
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
begin
  expect( hashed_maps_reserve_capacity_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseLastNumericParameter( capVal, capType, containers_count_type_t );
  if isExecutingCommand then
     declare
       cap : Ada.Containers.Count_Type := Ada.Containers.Count_Type( to_numeric( capVal ) );
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       String_Hashed_Maps.Reserve_Capacity( theMap.shmMap, cap );
     exception when others =>
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
  cursorId  : identifier := eof_t;
  theCursor : resPtr;
  insertRef : reference;
  version   : natural := 1;
  result    : boolean;
begin
  expect( hashed_maps_insert_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseNextStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  expect( symbol_t,"," );

  -- There are 3 variants to an insert

  if getBaseType( identifiers( token ).kind ) = hashed_maps_cursor_t then
     ParseInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
     ParseLastOutParameter( insertRef, boolean_t );
     version := 2;
  else
    ParseStringParameter( elemVal, elemType, identifiers( mapId ).genKind2 );
    if token = symbol_t and identifiers( token ).value.all = ")" then
       expect( symbol_t, ")" );
    else
       ParseNextInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
       ParseLastOutParameter( insertRef, boolean_t );
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
          findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
          String_Hashed_Maps.Insert( theMap.shmMap, keyVal, theCursor.shmCursor,
             result );
          AssignParameter( insertRef, to_bush_boolean( result ) );
       -- (m, k, e, p, b )
       when 3 =>
          findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
          String_Hashed_Maps.Insert( theMap.shmMap, keyVal, elemVal,
             theCursor.shmCursor, result );
          AssignParameter( insertRef, to_bush_boolean( result ) );
       when others =>
          put_line( gnat.source_info.source_location &
             ": internal error: unknown insert version" );
       end case;

     exception when constraint_error =>
       err_key_exists( keyVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsInsert;


------------------------------------------------------------------------------
--  SET
--
-- Syntax: hashed_maps.set( m, k, e );
-- Ada:    hashed_maps.include( m, k, e );
------------------------------------------------------------------------------

procedure ParseHashedMapsSet is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  elemVal : unbounded_string;
  elemType : identifier;
begin
  err_ada95( "set" );
  expect( hashed_maps_set_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseNextStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastNumericParameter( elemVal, elemType, identifiers( mapId ).genKind2 );
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
end ParseHashedMapsSet;


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
begin
  expect( hashed_maps_include_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseNextStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastStringParameter( elemVal, elemType, identifiers( mapId ).genKind2 );
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
begin
  expect( hashed_maps_replace_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseNextStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastNumericParameter( elemVal, elemType, identifiers( mapId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       String_Hashed_Maps.Replace( theMap.shmMap, keyVal, elemVal );
     exception when constraint_error =>
       err_no_key( keyVal );
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
begin
  expect( hashed_maps_exclude_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseLastStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
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
--  REMOVE
--
-- Syntax: hashed_maps.remove( m, k | c );
-- Ada:    hashed_maps.delete( m, k | c );
------------------------------------------------------------------------------

procedure ParseHashedMapsRemove is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  cursorId : identifier := eof_t;
  theCursor : resPtr;
begin
  expect( hashed_maps_remove_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  expect( symbol_t, "," );
  if getBaseType( identifiers( token ).kind ) = hashed_maps_cursor_t then
     ParseInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
  else
     ParseStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
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
       err_no_key( keyVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsRemove;


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
begin
  kind := boolean_t;
  expect( hashed_maps_contains_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseLastStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
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
begin
  expect( hashed_maps_element_t );

  -- The first parameter may be a map or a cursor

  expect( symbol_t, "(" );
  if getBaseType( identifiers( token ).kind ) = hashed_maps_cursor_t then
     ParseInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
     expect( symbol_t, ")" );
  elsif getBaseType( identifiers( token ).kind ) = hashed_maps_map_t then
     ParseInOutInstantiatedParameter( mapId, hashed_maps_map_t );
     ParseLastStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  else
     err( "map or cursor expected" );
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
  else
     kind := eof_t;
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
       err_no_key( keyVal );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsElement;


------------------------------------------------------------------------------
--  GET
--
-- Syntax: e := hashed_maps.get( m, k );
-- Ada:    e := hashed_maps.element( m, k );
------------------------------------------------------------------------------
-- TODO: consolidate code

procedure ParseHashedMapsGet( result : out unbounded_string; kind : out identifier ) is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
begin
  -- TODO: probably not universal
  kind := universal_t; -- type in case of error
  err_ada95( "get" );
  expect( hashed_maps_get_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseLastStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       result := String_Hashed_Maps.Element( theMap.shmMap, keyVal );
       kind := identifiers( mapId ).genKind2;
     exception when constraint_error =>
       err_no_key( keyVal );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsGet;


------------------------------------------------------------------------------
--  LENGTH
--
-- Syntax: n := hashed_maps.length( m );
-- Ada:    n := hashed_maps.length( m );
------------------------------------------------------------------------------

procedure ParseHashedMapsLength( result : out unbounded_string; kind : out identifier ) is
  mapId   : identifier;
  theMap  : resPtr;
begin
  kind := containers_count_type_t;
  expect( hashed_maps_length_t );
  ParseSingleInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       result := to_unbounded_string( String_Hashed_Maps.Length( theMap.shmMap )'img );
     end;
  end if;
end ParseHashedMapsLength;


------------------------------------------------------------------------------
--  ADD
--
-- Syntax: hashed_maps.add( m, k, e );
-- Ada:    N/A
------------------------------------------------------------------------------

procedure ParseHashedMapsAdd is
  mapId   : identifier;
  theMap  : resPtr;
  keyVal : unbounded_string;
  keyType : identifier;
  elemVal : unbounded_string;
  elemType : identifier;
begin
  err_ada95( "add" );
  expect( hashed_maps_add_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseNextStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastNumericParameter( elemVal, elemType, identifiers( mapId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       if not String_Hashed_Maps.Contains( theMap.shmMap, keyVal ) then
          String_Hashed_Maps.Include( theMap.shmMap, keyVal, elemVal );
       end if;
     exception when constraint_error =>
       err_no_key( keyVal );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsAdd;


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
begin
  err_ada95( "append" );
  expect( hashed_maps_append_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  if getUniType( identifiers( mapId ).genKind2 ) /= uni_string_t then
     err( "append requires a string element type" );
  end if;
  ParseNextStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastNumericParameter( elemVal, elemType, identifiers( mapId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       elemVal := String_Hashed_Maps.Element( theMap.shmMap, keyVal ) & elemVal;
       String_Hashed_Maps.Include( theMap.shmMap, keyVal, elemVal );
     exception when constraint_error =>
       err_no_key( keyVal );
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
begin
  err_ada95( "prepend" );
  expect( hashed_maps_prepend_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  if getUniType( identifiers( mapId ).genKind2 ) /= uni_string_t then
     err( "append requires a string element type" );
  end if;
  ParseNextStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  ParseLastNumericParameter( elemVal, elemType, identifiers( mapId ).genKind2 );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       elemVal := elemVal & String_Hashed_Maps.Element( theMap.shmMap, keyVal );
       String_Hashed_Maps.Include( theMap.shmMap, keyVal, elemVal );
     exception when constraint_error =>
       err_no_key( keyVal );
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
  floatVal : long_float;
  hasAmt  : boolean := false;
begin
  err_ada95( "increment" );
  expect( hashed_maps_increment_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  if getUniType( identifiers( mapId ).genKind2 ) /= uni_numeric_t then
     err( "append requires a numeric element type" );
  end if;
  ParseNextStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  if token = symbol_t and identifiers( token ).value.all = "," then
     hasAmt := true;
     ParseLastNumericParameter( incVal, incType, identifiers( mapId ).genKind2 );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expect( symbol_t, ")" );
  else
     err( ", or ) expected" );
  end if;
  if isExecutingCommand then
     begin
       if hasAmt then
          floatVal := long_float( natural( to_numeric( incVal ) ) );
       else
          floatVal := 1.0;
       end if;
     exception when constraint_error =>
       err( "increment value is not natural" );
     end;
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       floatVal := long_float( to_numeric(
          String_Hashed_Maps.Element( theMap.shmMap, keyVal ) ) ) + floatVal;
       String_Hashed_Maps.Include( theMap.shmMap, keyVal,
          to_unbounded_string( floatVal'img ) );
     exception when constraint_error =>
       err_no_key( keyVal );
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
  floatVal : long_float;
  hasAmt  : boolean := false;
begin
  err_ada95( "decrement" );
  expect( hashed_maps_decrement_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  if getUniType( identifiers( mapId ).genKind2 ) /= uni_numeric_t then
     err( "append requires a numeric element type" );
  end if;
  ParseNextStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  if token = symbol_t and identifiers( token ).value.all = "," then
     hasAmt := true;
     ParseLastNumericParameter( decVal, decType, identifiers( mapId ).genKind2 );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expect( symbol_t, ")" );
  else
     err( ", or ) expected" );
  end if;
  if isExecutingCommand then
     begin
       if hasAmt then
          floatVal := long_float( natural( to_numeric( decVal ) ) );
       else
          floatVal := 1.0;
       end if;
     exception when constraint_error =>
       err( "decrement value is not natural" );
     end;
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       floatVal := long_float( to_numeric(
          String_Hashed_Maps.Element( theMap.shmMap, keyVal ) ) ) - floatVal;
       String_Hashed_Maps.Include( theMap.shmMap, keyVal,
          to_unbounded_string( floatVal'img ) );
     exception when constraint_error =>
       err_no_key( keyVal );
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
begin
  kind := universal_t; -- type in case of error  TODO
  err_ada95( "extract" );
  expect( hashed_maps_extract_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseLastStringParameter( keyVal, keyType, identifiers( mapId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       result := String_Hashed_Maps.Element( theMap.shmMap, keyVal );
       kind := identifiers( mapId ).genKind2;
       String_Hashed_Maps.Delete( theMap.shmMap, keyVal );
     exception when constraint_error =>
       err_no_key( keyVal );
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
begin
  expect( hashed_maps_assign_t );
  ParseFirstInOutInstantiatedParameter( targetMapId, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( sourceMapId, hashed_maps_map_t );
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
begin
  expect( hashed_maps_move_t );
  ParseFirstInOutInstantiatedParameter( targetMapId, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( sourceMapId, hashed_maps_map_t );
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
--  COPY
--
-- Syntax: hashed_maps.copy( t, s );
-- Ada:    t := hashed_maps.copy( s );
------------------------------------------------------------------------------

procedure ParseHashedMapsCopy is
  targetMapId   : identifier;
  sourceMapId   : identifier;
  targetMap  : resPtr;
  sourceMap  : resPtr;
begin
  expect( hashed_maps_copy_t );
  ParseFirstInOutInstantiatedParameter( targetMapId, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( sourceMapId, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetMapId ).value.all ), targetMap );
       findResource( to_resource_id( identifiers( sourceMapId ).value.all ), sourceMap );
       targetMap.shmMap := String_Hashed_Maps.Copy( sourceMap.shmMap );
     exception when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsCopy;


------------------------------------------------------------------------------
--  FIRST
--
-- Syntax: hashed_maps.first( m, c );
-- Ada:    c := hashed_maps.first( m ); -- cannot return resource types
------------------------------------------------------------------------------

procedure ParseHashedMapsFirst is
  mapId : identifier;
  cursorId : identifier;
  theMap : resPtr;
  theCursor : resPtr;
begin
  expect( hashed_maps_first_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseLastInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
  if not error_found then
     genTypesOk( identifiers( mapId ).genKind, identifiers( cursorId ).genKind );
     genTypesOk( identifiers( mapId ).genKind2, identifiers( cursorId ).genKind2 );
  end if;
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
begin
  expect( hashed_maps_next_t );
  ParseSingleInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
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
begin
  expect( hashed_maps_key_t );
  ParseSingleInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );
  kind := identifiers( cursorId ).genKind;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       result := String_Hashed_Maps.Key( theCursor.shmCursor );
     exception when constraint_error =>
       err( "cursor position has no element" );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
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
begin
  expect( hashed_maps_find_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseNextStringParameter( keyVal, keyType, identifiers( mapId ).genKind ); -- TODO
  ParseLastInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );  -- TODO: not really in out
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       theCursor.shmCursor := String_Hashed_Maps.Find( theMap.shmMap, keyVal );
     exception when constraint_error =>
       err( "cursor position has no element" );
     when storage_error =>
       err_storage;
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseHashedMapsFind;


------------------------------------------------------------------------------
--  REPLACE ELEMENT
--
-- Syntax: hashed_maps.replace_element( m, k, c );
-- Ada:    c := hashed_maps.replace_element( m, k );
------------------------------------------------------------------------------

procedure ParseHashedMapsReplaceElement is
  cursorId : identifier;
  theCursor : resPtr;
  mapId : identifier;
  theMap : resPtr;
  newVal : unbounded_string;
  newType : identifier;
begin
  expect( hashed_maps_replace_element_t );
  ParseFirstInOutInstantiatedParameter( mapId, hashed_maps_map_t );
  ParseNextInOutInstantiatedParameter( cursorId, hashed_maps_cursor_t );  -- TODO: not really in out
  ParseLastStringParameter( newVal, newType, identifiers( mapId ).genKind ); -- TODO
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( mapId ).value.all ), theMap );
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       String_Hashed_Maps.Replace_Element( theMap.shmMap, theCursor.shmCursor , newVal );
     exception when constraint_error =>
       err( "cursor position has no element" );
     when program_error =>
       err( "cursor position not in map" );
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
begin
  kind := boolean_t;
  expect( hashed_maps_has_element_t );
  ParseSingleInOutInstantiatedParameter( cursorId, hashed_maps_map_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursorId ).value.all ), theCursor );
       result := to_bush_boolean( String_Hashed_Maps.Has_Element( theCursor.shmCursor ) );
     end;
  end if;
end ParseHashedMapsHasElement;


------------------------------------------------------------------------------
-- HOUSEKEEPING
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
  declareProcedure( hashed_maps_set_t,       "hashed_maps.set",      ParseHashedMapsSet'access );
  declareProcedure( hashed_maps_include_t,   "hashed_maps.include",  ParseHashedMapsInclude'access );
  declareProcedure( hashed_maps_replace_t,   "hashed_maps.replace",  ParseHashedMapsReplace'access );
  declareProcedure( hashed_maps_exclude_t,   "hashed_maps.exclude",  ParseHashedMapsExclude'access );
  declareProcedure( hashed_maps_remove_t,    "hashed_maps.remove",   ParseHashedMapsRemove'access );
  declareFunction(  hashed_maps_contains_t,  "hashed_maps.contains", ParseHashedMapsContains'access );
  declareFunction(  hashed_maps_element_t,   "hashed_maps.element",  ParseHashedMapsElement'access );
  declareFunction(  hashed_maps_get_t,       "hashed_maps.get",      ParseHashedMapsGet'access );
  declareProcedure( hashed_maps_add_t,       "hashed_maps.add",      ParseHashedMapsAdd'access );
  declareProcedure( hashed_maps_append_t,    "hashed_maps.append",   ParseHashedMapsAppend'access );
  declareProcedure( hashed_maps_prepend_t,   "hashed_maps.prepend",  ParseHashedMapsPrepend'access );
  declareProcedure( hashed_maps_increment_t, "hashed_maps.increment",  ParseHashedMapsIncrement'access );
  declareProcedure( hashed_maps_decrement_t, "hashed_maps.decrement",  ParseHashedMapsDecrement'access );
  declareFunction(  hashed_maps_extract_t,   "hashed_maps.extract",  ParseHashedMapsExtract'access );
  declareProcedure( hashed_maps_assign_t,    "hashed_maps.assign",   ParseHashedMapsAssign'access );
  declareProcedure( hashed_maps_move_t,      "hashed_maps.move",     ParseHashedMapsMove'access );
  declareProcedure( hashed_maps_copy_t,      "hashed_maps.copy",     ParseHashedMapsCopy'access );
  declareProcedure( hashed_maps_first_t,     "hashed_maps.first",    ParseHashedMapsFirst'access );
  declareProcedure( hashed_maps_next_t,      "hashed_maps.next",     ParseHashedMapsNext'access );
  declareFunction(  hashed_maps_key_t,       "hashed_maps.key",      ParseHashedMapsKey'access );
  declareProcedure( hashed_maps_find_t,      "hashed_maps.find",     ParseHashedMapsFind'access );
  declareProcedure( hashed_maps_replace_element_t, "hashed_maps.replace_element",     ParseHashedMapsReplaceElement'access );
  declareFunction(  hashed_maps_has_element_t, "hashed_maps.has_element",     ParseHashedMapsHasElement'access );

  declareNamespaceClosed( "hashed_maps" );
end StartupHMaps;

procedure ShutdownHMaps is
begin
  null;
end ShutdownHMaps;

end parser_hmaps;
