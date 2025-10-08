------------------------------------------------------------------------------
-- Dynamic Hash Tables Package Parser                                       --
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

--with text_io;use text_io;

with
    ada.strings.unbounded,
    pegasoft,
    world,
    symbol_table,
    message_strings,
    value_conversion,
    scanner,
    scanner.communications,
    scanner_res,
    scanner_restypes,
    parser_params;
use
    ada.strings.unbounded,
    pegasoft,
    world,
    symbol_table,
    message_strings,
    value_conversion,
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
-- Delete all keys and associated values in the hash table..
-----------------------------------------------------------------------------

procedure ParseDHTReset is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
begin
  expect( dht_reset_t );
  ParseSingleInOutInstantiatedParameter( dht_reset_t, tableRef, dht_table_t );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       Dynamic_Storage_Hash_Tables.Reset( theTable.dsht );
     exception when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDHTReset;

-----------------------------------------------------------------------------
--  SET
--
-- Syntax: dynamic_hash_tables.set( t, s, e );
-- Source: dynamic_hash_tables.set( t, s, e );
-- Assign the value v to the key k in the hash table. If the key does not
-- exist, create it.
-----------------------------------------------------------------------------

procedure ParseDHTSet is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  keyExpr  : storage;
  keyType  : identifier;
  itemExpr : storage;
  itemType : identifier;
  oldElem  : storage;
begin
  expect( dht_set_t );
  ParseFirstInOutInstantiatedParameter( dht_set_t, tableRef, dht_table_t ); -- TODO double gen
  ParseNextStringParameter( dht_set_t, keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( dht_set_t, itemExpr, itemType, identifiers( tableRef.Id ).genKind );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       -- the key, value and existing value must all be checked.
       oldElem := Dynamic_Storage_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldElem /= nullStorage then
          if metaLabelOK( oldElem ) and
             metaLabelOK( keyExpr ) and metaLabelOK( itemExpr ) then
             Dynamic_Storage_Hash_Tables.Set(
                theTable.dsht,
                keyExpr,
                itemExpr );
          end if;
       elsif metaLabelOK( keyExpr ) and metaLabelOK( itemExpr ) then
          Dynamic_Storage_Hash_Tables.Set(
             theTable.dsht,
             keyExpr,
             itemExpr );
       end if;
     exception when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDHTSet;

-----------------------------------------------------------------------------
--  GET
--
-- Syntax: e := doubly_linked_list.get( t, s );
-- Source: e := doubly_linked_list.get( t, s );
-- Get the value under key k in the hash table.
-----------------------------------------------------------------------------

procedure ParseDHTGet( result : out storage; kind : out identifier ) is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  keyExpr  : storage;
  keyType  : identifier;
  oldElem  : storage;
begin
  expect( dht_get_t );
  ParseFirstInOutInstantiatedParameter( dht_get_t, tableRef, dht_table_t );
  kind := identifiers( tableRef.Id ).genKind;
  ParseLastStringParameter( dht_get_t, keyExpr, keyType, uni_string_t );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       oldElem := Dynamic_Storage_Hash_Tables.Get( theTable.dsht, keyExpr );
       if metaLabelOk( oldElem ) then
          result := oldElem;
       end if;
     end;
  end if;
end ParseDHTGet;

-----------------------------------------------------------------------------
--  HAS ELEMENT
--
-- Syntax: e := dynamic_hash_tables.has_element( t, s );
-- Source: N/A
-- Return true if key k is in the hash table.
-----------------------------------------------------------------------------

procedure ParseDHTHasElement( result : out storage; kind : out identifier ) is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  keyExpr  : storage;
  keyType  : identifier;
begin
  kind := boolean_t;
  expect( dht_has_element_t );
  ParseFirstInOutInstantiatedParameter( dht_has_element_t, tableRef, dht_table_t );
  ParseLastStringParameter( dht_has_element_t, keyExpr, keyType, uni_string_t );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       result := storage'( to_spar_boolean( Dynamic_Storage_Hash_Tables.Get( theTable.dsht, keyExpr ) /= nullStorage ), noMetaLabel );
     end;
  end if;
end ParseDHTHasElement;

-----------------------------------------------------------------------------
--  REMOVE
--
-- Syntax: dynamic_hash_tables.remove( t, s );
-- Source: dynamic_hash_tables.remove( t, s );
-- Delete a key and the associated value from the hash table.
-----------------------------------------------------------------------------

procedure ParseDHTRemove is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  keyExpr  : storage;
  keyType  : identifier;
  oldElem  : storage;
begin
  expect( dht_remove_t );
  ParseFirstInOutInstantiatedParameter( dht_remove_t, tableRef, dht_table_t );
  ParseLastStringParameter( dht_remove_t, keyExpr, keyType, uni_string_t );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       oldElem := Dynamic_Storage_Hash_Tables.Get( theTable.dsht, keyExpr );
       if metaLabelOk( oldElem ) and metaLabelOk( keyExpr ) then
          Dynamic_Storage_Hash_Tables.Remove( theTable.dsht, keyExpr );
       end if;
     exception when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDHTRemove;

-----------------------------------------------------------------------------
--  GET FIRST
--
-- Syntax: dynamic_hash_tables.get_first( t, e, eof );
-- Source: e := dynamic_hash_tables.get_first( t );
-- Iterate through the hash table. Return the first value in the table. Order
-- of the values is not predictable.
-----------------------------------------------------------------------------

procedure ParseDHTGetFirst is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  itemRef  : reference;
  eofRef   : reference;
begin
  expectAdaScriptDifferences( subject => dht_get_first_t );
  ParseFirstInOutInstantiatedParameter( dht_get_first_t, tableRef, dht_table_t );
  ParseNextOutParameter( dht_get_first_t, itemRef, identifiers( tableRef.Id ).genKind );
  baseTypesOK( itemRef.kind, identifiers( tableRef.Id ).genKind );
  ParseLastOutParameter( dht_get_first_t, eofRef, boolean_t );
  baseTypesOK( eofRef.kind, boolean_t );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     declare
       s : storage;
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       s := Dynamic_Storage_Hash_Tables.Get_First( theTable.dsht );
       if metaLabelOk( s ) then
          AssignParameter( itemRef, s );
       end if;
       AssignParameter( eofRef, storage'( to_spar_boolean( s = nullStorage ), noMetaLabel ) );
     end;
  end if;
end ParseDHTGetFirst;

-----------------------------------------------------------------------------
--  GET NEXT
--
-- Syntax: dynamic_hash_tables.get_next( t, e, eof );
-- Source: e := dynamic_hash_tables.get_next( t );
-- Iterate through the hash table. Return the next value in the table. Order
-- of the values is not predictable. Use get_first to start iterating. If
-- iteration breaks if the table is altered.
-----------------------------------------------------------------------------

procedure ParseDHTGetNext is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  itemRef  : reference;
  eofRef   : reference;
begin
  expectAdaScriptDifferences( subject => dht_get_next_t );
  ParseFirstInOutInstantiatedParameter( dht_get_next_t, tableRef, dht_table_t );
  ParseNextOutParameter( dht_get_next_t, itemRef, identifiers( tableRef.Id ).genKind );
  baseTypesOK( itemRef.kind, identifiers( tableRef.Id ).genKind );
  ParseLastOutParameter( dht_get_next_t, eofRef, boolean_t );
  baseTypesOK( eofRef.kind, boolean_t );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     declare
       s : storage;
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       s := Dynamic_Storage_Hash_Tables.Get_Next( theTable.dsht );
       if metaLabelOk( s ) then
          AssignParameter( itemRef, s );
       end if;
       AssignParameter( eofRef, storage'( to_spar_boolean( s = nullStorage ), noMetaLabel ) );
     end;
  end if;
end ParseDHTGetNext;

-----------------------------------------------------------------------------
--  ADD
--
-- Syntax: dynamic_hash_tables.add( t, s, e );
-- Source: N/A
-- Put value v under key k in the hash table. If the key already exists, do
-- nothing.
-----------------------------------------------------------------------------

procedure ParseDHTAdd is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  keyExpr  : storage;
  keyType  : identifier;
  itemExpr : storage;
  itemType : identifier;
  oldItem  : storage;
begin
  expectAdaScript( subject => dht_add_t, remedy => +"use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_add_t, tableRef, dht_table_t );
  ParseNextStringParameter( dht_add_t, keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( dht_add_t, itemExpr, itemType, identifiers( tableRef.Id ).genKind );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       oldItem := Dynamic_Storage_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem = nullStorage then
          if metaLabelOk( keyExpr ) and metaLabelOk( itemExpr ) then
             Dynamic_Storage_Hash_Tables.Set( theTable.dsht, keyExpr, itemExpr );
          end if;
       end if;
     exception when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDHTAdd;

-----------------------------------------------------------------------------
--  REPLACE
--
-- Syntax: dynamic_hash_tables.replace( t, s, e );
-- Source: N/A
-- Replace the value v under key k in the hash table. If the key does not
-- exist, do nothing.
-----------------------------------------------------------------------------

procedure ParseDHTReplace is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  keyExpr  : storage;
  keyType  : identifier;
  itemExpr : storage;
  itemType : identifier;
  oldItem  : storage;
begin
  expectAdaScript( subject => dht_replace_t, remedy => +"use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_replace_t, tableRef, dht_table_t );
  ParseNextStringParameter( dht_replace_t, keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( dht_replace_t, itemExpr, itemType, identifiers( tableRef.Id ).genKind );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       oldItem := Dynamic_Storage_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem /= nullStorage then
          if metaLabelOk( keyExpr ) and metaLabelOk( itemExpr ) and
             metaLabelOk( oldItem ) then
                Dynamic_Storage_Hash_Tables.Set( theTable.dsht, keyExpr, itemExpr );
          end if;
       end if;
     exception when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDHTReplace;

-----------------------------------------------------------------------------
--  APPEND
--
-- Syntax: dynamic_hash_tables.append( t, s, e );
-- Source: N/A
-- Append string value v to the value under key k in the hash table. If the key
-- does not exist, do nothing.
-----------------------------------------------------------------------------

procedure ParseDHTAppend is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  keyExpr  : storage;
  keyType  : identifier;
  itemExpr : storage;
  itemType : identifier;
  oldItem  : storage;
begin
  expectAdaScript( subject => dht_append_t, remedy => +"use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_append_t, tableRef, dht_table_t );
  if getUniType( identifiers( tableRef.Id ).genKind ) /= uni_string_t then
     err( +"append requires a string item type" );
  end if;
  ParseNextStringParameter( dht_append_t, keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( dht_append_t, itemExpr, itemType, identifiers( tableRef.Id ).genKind );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       oldItem := Dynamic_Storage_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem /= nullStorage then
          if metaLabelOk( keyExpr ) and metaLabelOk( itemExpr, oldItem ) then
             -- labels must be the same for the original value and the appending value
             oldItem.value := oldItem.value & itemExpr.value;
                Dynamic_Storage_Hash_Tables.Set( theTable.dsht, keyExpr, oldItem );
          end if;
       end if;
     exception when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDHTAppend;

-----------------------------------------------------------------------------
--  PREPEND
--
-- Syntax: dynamic_hash_tables.prepend( t, s, e );
-- Source:    N/A
-- Prepend string value v to the value under key k in the hash table. If the
-- key does not exist, do nothing.
-----------------------------------------------------------------------------

procedure ParseDHTPrepend is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  keyExpr  : storage;
  keyType  : identifier;
  itemExpr : storage;
  itemType : identifier;
  oldItem  : storage;
begin
  expectAdaScript( subject => dht_prepend_t, remedy => +"use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_prepend_t, tableRef, dht_table_t );
  if getUniType( identifiers( tableRef.Id ).genKind ) /= uni_string_t then
     err( +"prepend requires a string item type" );
  end if;
  ParseNextStringParameter( dht_prepend_t, keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( dht_prepend_t, itemExpr, itemType, identifiers( tableRef.Id ).genKind );
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       oldItem := Dynamic_Storage_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem /= nullStorage then
          -- labels must be the same for the original value and the appending value
          if metaLabelOk( keyExpr ) and metaLabelOK( olditem, itemExpr ) then
             oldItem.value := itemExpr.value & oldItem.value;
             Dynamic_Storage_Hash_Tables.Set( theTable.dsht, keyExpr, oldItem );
          end if;
       end if;
     exception when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDHTPrepend;

-----------------------------------------------------------------------------
--  INCREMENT
--
-- Syntax: dynamic_hash_tables.increment( t, s [,n] );
-- Source: N/A
-- Increase the numeric value under key k in the hash table by one (or n). If
-- the key does not exist, do nothing.
-----------------------------------------------------------------------------

procedure ParseDHTIncrement is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  keyExpr  : storage;
  keyType  : identifier;
  amtExpr  : storage;
  amtType  : identifier;
  hasAmt   : boolean := false;
  oldItem  : storage;
  oldItemValue : numericValue;
begin
  expectAdaScript( subject => dht_increment_t, remedy => +"use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_increment_t, tableRef, dht_table_t );
  if getUniType( identifiers( tableRef.Id ).genKind ) /= uni_numeric_t then
     err( +"increment requires a numeric item type" );
  end if;
  ParseNextStringParameter( dht_increment_t, keyExpr, keyType, uni_string_t );
  if token = symbol_t and identifiers( token ).store.value = "," then
     hasAmt := true;
     ParseLastNumericParameter( dht_increment_t, amtExpr, amtType, natural_t );
  elsif token = symbol_t and identifiers( token ).store.value = ")" then
     expect( symbol_t, ")" );
  else
     err( +", or ) expected" );
  end if;
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       oldItem := Dynamic_Storage_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem /= nullStorage then
          oldItemValue := to_numeric( oldItem.value );
          if hasAmt then
             oldItem.value := to_unbounded_string(
                 oldItemValue + numericValue( natural( to_numeric( amtExpr.value ) ) )
             );
             if metaLabelOk( keyExpr ) and metaLabelOk( amtExpr, oldItem ) then
                Dynamic_Storage_Hash_Tables.Set( theTable.dsht, keyExpr, oldItem );
             end if;
          else
             oldItem.value := to_unbounded_string( oldItemValue + 1.0 );
             if metaLabelOk( keyExpr ) and metaLabelOk( oldItem ) then
                Dynamic_Storage_Hash_Tables.Set( theTable.dsht, keyExpr, oldItem );
             end if;
          end if;
       end if;
     exception when storage_error =>
       err( +"storage error raised" );
     when constraint_error =>
       err( +"constraint error raised" );
     end;
  end if;
end ParseDHTIncrement;

-----------------------------------------------------------------------------
--  DECREMENT
--
-- Syntax: dynamic_hash_tables.decrement( t, s [,n] );
-- Ada:    N/A
-- Reduce the numeric value under key k in the hash table by one (or n). If the
-- key does not exist, do nothing.
-----------------------------------------------------------------------------

procedure ParseDHTDecrement is
  tableRef : reference;
  theTable : resPtr;
  tableResId: storage;
  keyExpr  : storage;
  keyType  : identifier;
  amtExpr  : storage;
  amtType  : identifier;
  hasAmt   : boolean := false;
  oldItem  : storage;
  oldItemValue : numericValue;
begin
  expectAdaScript( subject => dht_decrement_t, remedy => +"use get and set" );
  ParseFirstInOutInstantiatedParameter( dht_decrement_t, tableRef, dht_table_t );
  if getUniType( identifiers( tableRef.Id ).genKind ) /= uni_numeric_t then
     err( +"decrement requires a numeric item type" );
  end if;
  ParseNextStringParameter( dht_decrement_t, keyExpr, keyType, uni_string_t );
  if token = symbol_t and identifiers( token ).store.value = "," then
     hasAmt := true;
     ParseLastNumericParameter( dht_decrement_t, amtExpr, amtType, natural_t );
  elsif token = symbol_t and identifiers( token ).store.value = ")" then
     expect( symbol_t, ")" );
  else
     err( +", or ) expected" );
  end if;
  if isExecutingCommand then
     getParameterValue( tableRef, tableResId );
     begin
       findResource( to_resource_id( tableResId.value ), theTable );
       oldItem := Dynamic_Storage_Hash_Tables.Get( theTable.dsht, keyExpr );
       if oldItem /= nullStorage then
          oldItemValue := to_numeric( oldItem.value );
          if hasAmt then
             oldItem.value := to_unbounded_string( oldItemValue - numericValue( natural( to_numeric( amtExpr.value ) ) ) );
             if metaLabelOk( keyExpr ) and metaLabelOk( amtExpr, oldItem ) then
                Dynamic_Storage_Hash_Tables.Set( theTable.dsht, keyExpr, oldItem);
             end if;
          else
             oldItem.value := to_unbounded_string( oldItemValue - 1.0 );
             if metaLabelOk( keyExpr ) and metaLabelOk( oldItem ) then
                Dynamic_Storage_Hash_Tables.Set( theTable.dsht, keyExpr, oldItem);
             end if;
          end if;
       end if;
     exception when storage_error =>
       err( +"storage error raised" );
     when constraint_error =>
       err( +"constraint error raised" );
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

