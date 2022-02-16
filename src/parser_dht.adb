------------------------------------------------------------------------------
-- Dynamic Hash Tables Package Parser                                       --
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

--with text_io;use text_io;

with
    ada.strings.unbounded,
    pegasoft.user_io,
    world,
    scanner,
    scanner.communications,
    scanner_res,
    scanner_restypes,
    parser_params;
use
    ada.strings.unbounded,
    world,
    pegasoft.user_io,
    scanner,
    scanner.communications,
    scanner_res,
    scanner_restypes,
    parser_params;

package body parser_dht is

------------------------------------------------------------------------------
-- Dynmaic Hash Tables package identifiers
------------------------------------------------------------------------------

--dht_table_t         : identifier;

--dht_new_table_t     : identifier;
dht_set_t           : identifier;
dht_reset_t         : identifier;
dht_get_t           : identifier;
dht_has_element_t   : identifier;
dht_remove_t        : identifier;
dht_get_first_t     : identifier;
dht_get_next_t      : identifier;

dht_add_t           : identifier;
dht_append_t        : identifier;
dht_prepend_t       : identifier;
dht_replace_t       : identifier;
dht_increment_t     : identifier;
dht_decrement_t     : identifier;

--dht_assemble_t      : identifier;
--dht_disassemble_t   : identifier;


------------------------------------------------------------------------------
-- Parser subprograms
------------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  RESET
--
-- Syntax: dynamic_hash_tables.reset( t );
-- Source: dynamic_hash_tables.reset( t );
-----------------------------------------------------------------------------

procedure ParseDHTReset is
  tableId  : identifier;
  theTable : resPtr;
begin
  expect( dht_reset_t );
  ParseSingleInOutInstantiatedParameter( dht_reset_t, tableId, dht_table_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       Dynamic_String_Hash_Tables.Reset( theTable.dsht );
     exception when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseDHTReset;

-----------------------------------------------------------------------------
--  SET
--
-- Syntax: dynamic_hash_tables.set( t, s, e );
-- Source: dynamic_hash_tables.set( t, s, e );
-----------------------------------------------------------------------------

procedure ParseDHTSet is
  tableId  : identifier;
  theTable : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  itemExpr : unbounded_string;
  itemType : identifier;
begin
  expect( dht_set_t );
  ParseFirstInOutInstantiatedParameter( dht_set_t, tableId, dht_table_t ); -- TODO double gen
  ParseNextStringParameter( dht_set_t, keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( dht_set_t, itemExpr, itemType, identifiers( tableId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       Dynamic_String_Hash_Tables.Set(
          theTable.dsht,
          keyExpr,
          itemExpr );
     exception when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseDHTSet;

-----------------------------------------------------------------------------
--  GET
--
-- Syntax: e := doubly_linked_list.get( t, s );
-- Source: e := doubly_linked_list.get( t, s );
-----------------------------------------------------------------------------

procedure ParseDHTGet( result : out unbounded_string; kind : out identifier ) is
  tableId  : identifier;
  theTable : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
begin
  expect( dht_get_t );
  ParseFirstInOutInstantiatedParameter( dht_get_t, tableId, dht_table_t );
  kind := identifiers( tableId ).genKind;
  ParseLastStringParameter( dht_get_t, keyExpr, keyType, uni_string_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       result := Dynamic_String_Hash_Tables.Get( theTable.dsht, keyExpr );
     end;
  end if;
end ParseDHTGet;

-----------------------------------------------------------------------------
--  HAS ELEMENT
--
-- Syntax: e := doubly_linked_list.has_element( t, s );
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseDHTHasElement( result : out unbounded_string; kind : out identifier ) is
  tableId  : identifier;
  theTable : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
begin
  kind := boolean_t;
  expect( dht_has_element_t );
  ParseFirstInOutInstantiatedParameter( dht_has_element_t, tableId, dht_table_t );
  ParseLastStringParameter( dht_has_element_t, keyExpr, keyType, uni_string_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       result := to_bush_boolean( Dynamic_String_Hash_Tables.Get( theTable.dsht, keyExpr ) /= null_unbounded_string );
     end;
  end if;
end ParseDHTHasElement;

-----------------------------------------------------------------------------
--  REMOVE
--
-- Syntax: dynamic_hash_tables.remove( t, s );
-- Source: dynamic_hash_tables.remove( t, s );
-----------------------------------------------------------------------------

procedure ParseDHTRemove is
  tableId  : identifier;
  theTable : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
begin
  expect( dht_remove_t );
  ParseFirstInOutInstantiatedParameter( dht_remove_t, tableId, dht_table_t );
  ParseLastStringParameter( dht_remove_t, keyExpr, keyType, uni_string_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       Dynamic_String_Hash_Tables.Remove( theTable.dsht, keyExpr );
     exception when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseDHTRemove;

-----------------------------------------------------------------------------
--  GET FIRST
--
-- Syntax: dynamic_hash_tables.get_first( t, e, eof );
-- Source: e := dynamic_hash_tables.get_first( t );
-----------------------------------------------------------------------------

procedure ParseDHTGetFirst is
  tableId  : identifier;
  theTable : resPtr;
  itemRef  : reference;
  eofRef   : reference;
begin
  expectAdaScriptDifferences( subject => dht_get_first_t );
  ParseFirstInOutInstantiatedParameter( dht_get_first_t, tableId, dht_table_t );
  ParseNextOutParameter( dht_get_first_t, itemRef, identifiers( tableId ).genKind );
  baseTypesOK( itemRef.kind, identifiers( tableId ).genKind );
  ParseLastOutParameter( dht_get_first_t, eofRef, boolean_t );
  baseTypesOK( eofRef.kind, boolean_t );
  if isExecutingCommand then
     declare
       s : unbounded_string;
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       s := Dynamic_String_Hash_Tables.Get_First( theTable.dsht );
       AssignParameter( itemRef, s );
       AssignParameter( eofRef, to_bush_boolean( s = null_unbounded_string ) );
     end;
  end if;
end ParseDHTGetFirst;

-----------------------------------------------------------------------------
--  GET NEXT
--
-- Syntax: dynamic_hash_tables.get_next( t, e, eof );
-- Source: e := dynamic_hash_tables.get_next( t );
-----------------------------------------------------------------------------

procedure ParseDHTGetNext is
  tableId  : identifier;
  theTable : resPtr;
  itemRef  : reference;
  eofRef   : reference;
begin
  expectAdaScriptDifferences( subject => dht_get_next_t );
  ParseFirstInOutInstantiatedParameter( dht_get_next_t, tableId, dht_table_t );
  ParseNextOutParameter( dht_get_next_t, itemRef, identifiers( tableId ).genKind );
  baseTypesOK( itemRef.kind, identifiers( tableId ).genKind );
  ParseLastOutParameter( dht_get_next_t, eofRef, boolean_t );
  baseTypesOK( eofRef.kind, boolean_t );
  if isExecutingCommand then
     declare
       s : unbounded_string;
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       s := Dynamic_String_Hash_Tables.Get_Next( theTable.dsht );
       AssignParameter( itemRef, s );
       AssignParameter( eofRef, to_bush_boolean( s = null_unbounded_string ) );
     end;
  end if;
end ParseDHTGetNext;

-----------------------------------------------------------------------------
--  ADD
--
-- Syntax: dynamic_hash_tables.add( t, s, e );
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseDHTAdd is
  tableId  : identifier;
  theTable : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  itemExpr : unbounded_string;
  itemType : identifier;
  oldItem  : unbounded_string;
begin
  expectAdaScript( subject => dht_add_t, remedy => "use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_add_t, tableId, dht_table_t );
  ParseNextStringParameter( dht_add_t, keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( dht_add_t, itemExpr, itemType, identifiers( tableId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       oldItem := Dynamic_String_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem = null_unbounded_string then
          Dynamic_String_Hash_Tables.Set( theTable.dsht, keyExpr, itemExpr );
       end if;
     exception when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseDHTAdd;

-----------------------------------------------------------------------------
--  REPLACE
--
-- Syntax: dynamic_hash_tables.replace( t, s, e );
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseDHTReplace is
  tableId  : identifier;
  theTable : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  itemExpr : unbounded_string;
  itemType : identifier;
  oldItem  : unbounded_string;
begin
  expectAdaScript( subject => dht_replace_t, remedy => "use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_replace_t, tableId, dht_table_t );
  ParseNextStringParameter( dht_replace_t, keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( dht_replace_t, itemExpr, itemType, identifiers( tableId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       oldItem := Dynamic_String_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem /= null_unbounded_string then
          Dynamic_String_Hash_Tables.Set( theTable.dsht, keyExpr, itemExpr );
       end if;
     exception when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseDHTReplace;

-----------------------------------------------------------------------------
--  APPEND
--
-- Syntax: dynamic_hash_tables.append( t, s, e );
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseDHTAppend is
  tableId  : identifier;
  theTable : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  itemExpr : unbounded_string;
  itemType : identifier;
  oldItem  : unbounded_string;
begin
  expectAdaScript( subject => dht_append_t, remedy => "use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_append_t, tableId, dht_table_t );
  if getUniType( identifiers( tableId ).genKind ) /= uni_string_t then
     err( "append requires a string item type" );
  end if;
  ParseNextStringParameter( dht_append_t, keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( dht_append_t, itemExpr, itemType, identifiers( tableId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       oldItem := Dynamic_String_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem /= null_unbounded_string then
          Dynamic_String_Hash_Tables.Set( theTable.dsht, keyExpr, oldItem & itemExpr );
       end if;
     exception when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseDHTAppend;

-----------------------------------------------------------------------------
--  PREPEND
--
-- Syntax: dynamic_hash_tables.prepend( t, s, e );
-- Source:    N/A
-----------------------------------------------------------------------------

procedure ParseDHTPrepend is
  tableId  : identifier;
  theTable : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  itemExpr : unbounded_string;
  itemType : identifier;
  oldItem  : unbounded_string;
begin
  expectAdaScript( subject => dht_prepend_t, remedy => "use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_prepend_t, tableId, dht_table_t );
  if getUniType( identifiers( tableId ).genKind ) /= uni_string_t then
     err( "prepend requires a string item type" );
  end if;
  ParseNextStringParameter( dht_prepend_t, keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( dht_prepend_t, itemExpr, itemType, identifiers( tableId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       oldItem := Dynamic_String_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem /= null_unbounded_string then
          Dynamic_String_Hash_Tables.Set( theTable.dsht, keyExpr, itemExpr & oldItem );
       end if;
     exception when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseDHTPrepend;

-----------------------------------------------------------------------------
--  INCREMENT
--
-- Syntax: dynamic_hash_tables.increment( t, s [,n] );
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseDHTIncrement is
  tableId  : identifier;
  theTable : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  amtExpr  : unbounded_string;
  amtType  : identifier;
  hasAmt   : boolean := false;
  oldItem  : unbounded_string;
  oldItemValue : long_float;
begin
  expectAdaScript( subject => dht_increment_t, remedy => "use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_increment_t, tableId, dht_table_t );
  if getUniType( identifiers( tableId ).genKind ) /= uni_numeric_t then
     err( "increment requires a numeric item type" );
  end if;
  ParseNextStringParameter( dht_increment_t, keyExpr, keyType, uni_string_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     hasAmt := true;
     ParseLastNumericParameter( dht_increment_t, amtExpr, amtType, natural_t );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expect( symbol_t, ")" );
  else
     err( ", or ) expected" );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       oldItem := Dynamic_String_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem /= null_unbounded_string then
          oldItemValue := to_numeric( oldItem );
          if hasAmt then
             Dynamic_String_Hash_Tables.Set( theTable.dsht, keyExpr, to_unbounded_string( oldItemValue + long_float( natural( to_numeric( amtExpr ) ) ) ) );
          else
             Dynamic_String_Hash_Tables.Set( theTable.dsht, keyExpr, to_unbounded_string( oldItemValue + 1.0 ) );
          end if;
       end if;
     exception when storage_error =>
       err( "storage error raised" );
     when constraint_error =>
       err( "constraint error raised" );
     end;
  end if;
end ParseDHTIncrement;

-----------------------------------------------------------------------------
--  DECREMENT
--
-- Syntax: dynamic_hash_tables.decrement( t, s [,n] );
-- Ada:    N/A
-----------------------------------------------------------------------------

procedure ParseDHTDecrement is
  tableId  : identifier;
  theTable : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  amtExpr  : unbounded_string;
  amtType  : identifier;
  hasAmt   : boolean := false;
  oldItem  : unbounded_string;
  oldItemValue : long_float;
begin
  expectAdaScript( subject => dht_decrement_t, remedy => "use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_decrement_t, tableId, dht_table_t );
  if getUniType( identifiers( tableId ).genKind ) /= uni_numeric_t then
     err( "decrement requires a numeric item type" );
  end if;
  ParseNextStringParameter( dht_decrement_t, keyExpr, keyType, uni_string_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     hasAmt := true;
     ParseLastNumericParameter( dht_decrement_t, amtExpr, amtType, natural_t );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expect( symbol_t, ")" );
  else
     err( ", or ) expected" );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( tableId ).value.all ), theTable );
       oldItem := Dynamic_String_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem /= null_unbounded_string then
          oldItemValue := to_numeric( oldItem );
          if hasAmt then
             Dynamic_String_Hash_Tables.Set( theTable.dsht, keyExpr, to_unbounded_string( oldItemValue - long_float( natural( to_numeric( amtExpr ) ) ) ) );
          else
             Dynamic_String_Hash_Tables.Set( theTable.dsht, keyExpr, to_unbounded_string( oldItemValue - 1.0 ) );
          end if;
       end if;
     exception when storage_error =>
       err( "storage error raised" );
     when constraint_error =>
       err( "constraint error raised" );
     end;
  end if;
end ParseDHTDecrement;


-----------------------------------------------------------------------------

procedure StartupDHT is
begin
  declareNamespace( "dynamic_hash_tables" );
  declareIdent( dht_table_t,   "dynamic_hash_tables.table", variable_t, genericTypeClass );
  identifiers( dht_table_t ).usage := limitedUsage;
  identifiers( dht_table_t ).resource := true;

  --declareProcedure( dht_new_table_t, "dynamic_hash_tables.new_table", ParseDHTNewTable'access );
  declareProcedure( dht_set_t, "dynamic_hash_tables.set", ParseDHTSet'access );
  declareProcedure( dht_reset_t, "dynamic_hash_tables.reset", ParseDHTReset'access );
  declareFunction(  dht_get_t, "dynamic_hash_tables.get", ParseDHTGet'access );
  declareFunction(  dht_has_element_t, "dynamic_hash_tables.has_element", ParseDHTHasElement'access );
  declareProcedure( dht_remove_t, "dynamic_hash_tables.remove", ParseDHTRemove'access );
  declareProcedure(  dht_get_first_t, "dynamic_hash_tables.get_first", ParseDHTGetFirst'access );
  declareProcedure(  dht_get_next_t, "dynamic_hash_tables.get_next", ParseDHTGetNext'access );

  declareProcedure( dht_add_t, "dynamic_hash_tables.add", ParseDHTAdd'access );
  declareProcedure( dht_replace_t, "dynamic_hash_tables.replace", ParseDHTReplace'access );
  declareProcedure( dht_append_t, "dynamic_hash_tables.append", ParseDHTAppend'access );
  declareProcedure( dht_prepend_t, "dynamic_hash_tables.prepend", ParseDHTPrepend'access );
  declareProcedure( dht_increment_t, "dynamic_hash_tables.increment", ParseDHTIncrement'access );
  declareProcedure( dht_decrement_t, "dynamic_hash_tables.decrement", ParseDHTDecrement'access );

  --declareProcedure( dht_assemble_t, "dynamic_hash_tables.", 'access );
  --declareProcedure( dht_disassemble_t, "dynamic_hash_tables.", 'access );

  declareNamespaceClosed( "dynamic_hash_tables" );

  -- TODO: Add memcached functions
  -- TODO: enumerated items
  -- TODO: increment / decrement - amount, initial value

end StartupDHT;

procedure ShutdownDHT is
begin
  null;
end ShutdownDHT;

end parser_dht;

