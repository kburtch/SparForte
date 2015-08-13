------------------------------------------------------------------------------
-- Berekeley BTree Files Package Parser                                     --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2015 Free Software Foundation              --
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
    Interfaces.C,
    world,
    scanner,
    scanner_res,
    string_util,
    parser,
    parser_aux,
    parser_params;

with
    bdb,
    bdb_constants;

use
    world,
    scanner,
    scanner_res,
    string_util,
    parser,
    parser_aux,
    parser_params;

use
    bdb,
    bdb_constants;

package body parser_btree is

------------------------------------------------------------------------------
-- Utility subprograms
------------------------------------------------------------------------------

procedure CheckFileIsInitialized( fileId : identifier ) is
begin
  if identifiers( fileId ).genKind = eof_t then
     err( "new_file has not been called to initialize the file" );
  end if;
end CheckFileIsInitialized;

procedure ParseSingleFileParameter( fileId : out identifier ) is
begin
  ParseSingleInOutParameter( fileId, btree_file_t );
  CheckFileIsInitialized( fileId );
end ParseSingleFileParameter;

procedure ParseFirstFileParameter( fileId : out identifier ) is
begin
  ParseFirstInOutParameter( fileId, btree_file_t );
  CheckFileIsInitialized( fileId );
end ParseFirstFileParameter;

procedure ParseNextFileParameter( fileId : out identifier ) is
begin
  ParseNextInOutParameter( fileId, btree_file_t );
  CheckFileIsInitialized( fileId );
end ParseNextFileParameter;

procedure ParseLastFileParameter( fileId : out identifier ) is
begin
  ParseLastInOutParameter( fileId, btree_file_t );
  CheckFileIsInitialized( fileId );
end ParseLastFileParameter;

------------------------------------------------------------------------------

--procedure CheckCursorIsInitialized( cursId : identifier ) is
--begin
--  if identifiers( cursId ).genKind = eof_t then
--     err( "new_cursor has not been called to initialize the cursor" );
--  end if;
--end CheckCursorIsInitialized;
--
--procedure ParseSingleCursorParameter( cursId : out identifier ) is
--begin
--  ParseSingleInOutParameter( cursId, doubly_cursor_t );
--  CheckCursorIsInitialized( cursId );
--end ParseSingleCursorParameter;
--
--procedure ParseFirstCursorParameter( cursId : out identifier ) is
--begin
--  ParseFirstInOutParameter( cursId, doubly_cursor_t );
--  CheckCursorIsInitialized( cursId );
--end ParseFirstCursorParameter;
--
--procedure ParseNextCursorParameter( cursId : out identifier ) is
--begin
--  ParseNextInOutParameter( cursId, doubly_cursor_t );
--  CheckCursorIsInitialized( cursId );
--end ParseNextCursorParameter;
--
--procedure ParseLastCursorParameter( cursId : out identifier ) is
--begin
--  ParseLastInOutParameter( cursId, doubly_cursor_t );
--  CheckCursorIsInitialized( cursId );
--end ParseLastCursorParameter;

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
     err( "doubly_linked_lists.cursor or list item expected" );
     return false;
  end if;
  return true;
end insertTypesOk;

------------------------------------------------------------------------------
-- Parser subprograms
------------------------------------------------------------------------------


procedure ParseBTreeNewFile is
  -- Syntax: btree.new_file( f, t );
  -- Ada:    N/A
  resId : resHandleId;
  ref : reference;
  genKindId : identifier;
begin
  expect( btree_new_file_t );
  ParseFirstOutParameter( ref, btree_file_t );
  baseTypesOK( ref.kind, btree_file_t );
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
     identifiers( ref.id ).resource := true;
     declareResource( resId, btree_file, blocks_top );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParseBtreeNewFile;

procedure ParseBTreeClear is
  -- Syntax: btree.clear( l );
  -- Ada:    btree.clear( l );
  --listExpr : unbounded_string;
  --listType : identifier;
  fileId   : identifier;
  --theFile  : resPtr;
begin
  expect( btree_clear_t );
  ParseSingleFileParameter( fileId );
  if isExecutingCommand then
     null;
     -- TODO: Check and close open resources?
     --begin
     --  findResource( to_resource_id( identifiers( fileId ).value ), theFile );
     --  Doubly_Linked_String_Lists.Clear( theFile.btree.session );
     --  Doubly_Linked_String_Lists.Clear( theFile.btree.env );
     --end;
  end if;
end ParseBTreeClear;

procedure ParseBTreeCreate is
  -- Syntax: btree.create( f, path, keyLen, valLen );
  -- Ada:    btree.create( f, path, keyLen, valLen );
  fileId     : identifier;
  theFile    : resPtr;
  fname_val  : unbounded_string;
  fname_type : identifier;
  keyLen     : interfaces.C.size_t;
  keyLenExpr : unbounded_string;
  keyLenType : identifier;
  valLen     : interfaces.C.size_t;
  valLenExpr : unbounded_string;
  valLenType : identifier;
begin
  expect( btree_create_t );
  -- NOTE: normally this is an out parameter but it is a resource so it
  -- must be initialized separately.
  ParseFirstFileParameter( fileId );
  ParseNextStringParameter( fname_val, fname_type, string_t );
  ParseNextNumericParameter( keyLenExpr, keyLenType, positive_t );
  ParseLastNumericParameter( valLenExpr, valLenType, positive_t );
  if isExecutingCommand then
     begin
        keyLen := Interfaces.C.size_t'value( to_string( keyLenExpr ) );
     exception when others =>
        err( "key length must be" & Interfaces.C.size_t'first'img & ".." & Interfaces.C.size_t'last'img );
     end;
     begin
        valLen := Interfaces.C.size_t'value( to_string( valLenExpr ) );
     exception when others =>
        err( "value length must be" & Interfaces.C.size_t'first'img & ".." & Interfaces.C.size_t'last'img );
     end;
     begin
        -- TODO: pathname handling
        findResource( to_resource_id( identifiers( fileId ).value ), theFile );
        -- Create an environment
        init( theFile.btree.env );
        create( theFile.btree.env,
                "",
                -- I assume we don't need logging or transactions but
                -- they could be implemented.
                DB_E_OPEN_INIT_LOCK OR
                DB_E_OPEN_INIT_MPOOL,
                0 ); -- TODO: dirname
        -- Create the file
        new_berkeley_session(
           theFile.btree.session,
           theFile.btree.env,
           keyLen,
           valLen );
        create( theFile.btree.session, to_string( fname_val ), "", DB_BTREE, 0, 0 );
        theFile.btree.isOpen := true;
        theFile.btree.name := fname_val;
     --exception when berkeley_error:s =>
     --   err( to_string( last_error( theFile.btree.session ) );
     end;
  end if;
end ParseBTreeCreate;

procedure ParseBTreeClose is
  -- Syntax: btree.create( f );
  -- Ada:    btree.create( f );
  fileId     : identifier;
  theFile    : resPtr;
begin
  expect( btree_close_t );
  ParseSingleFileParameter( fileId );
  if isExecutingCommand then
     begin
        findResource( to_resource_id( identifiers( fileId ).value ), theFile );
        close( theFile.btree.session );
        close( theFile.btree.env );
        theFile.btree.isOpen := false;
     --exception when berkeley_error =>
     --   err( to_string( last_error( theFile.btree.session ) );
     end;
  end if;
end ParseBTreeClose;

procedure ParseBTreeOpen is
  -- Syntax: btree.open( f, path, keyLen, valLen );
  -- Ada:    btree.open( f, path, keyLen, valLen );
  fileId     : identifier;
  theFile    : resPtr;
  fname_val  : unbounded_string;
  fname_type : identifier;
  keyLen     : interfaces.C.size_t;
  keyLenExpr : unbounded_string;
  keyLenType : identifier;
  valLen     : interfaces.C.size_t;
  valLenExpr : unbounded_string;
  valLenType : identifier;
begin
  expect( btree_open_t );
  -- NOTE: normally this is an out parameter but it is a resource so it
  -- must be initialized separately.
  ParseFirstFileParameter( fileId );
  ParseNextStringParameter( fname_val, fname_type, string_t );
  -- TODO: can this be dynamic?
  ParseNextNumericParameter( keyLenExpr, keyLenType, positive_t );
  ParseLastNumericParameter( valLenExpr, valLenType, positive_t );
  if isExecutingCommand then
     begin
        keyLen := Interfaces.C.size_t'value( to_string( keyLenExpr ) );
     exception when others =>
        err( "key length must be" & Interfaces.C.size_t'first'img & ".." & Interfaces.C.size_t'last'img );
     end;
     begin
        valLen := Interfaces.C.size_t'value( to_string( valLenExpr ) );
     exception when others =>
        err( "value length must be" & Interfaces.C.size_t'first'img & ".." & Interfaces.C.size_t'last'img );
     end;
     begin
        -- TODO: pathname handling
        findResource( to_resource_id( identifiers( fileId ).value ), theFile );
        -- Create an environment
        init( theFile.btree.env );
        open( theFile.btree.env,
                "",
                -- I assume we don't need logging or transactions but
                -- they could be implemented.
                DB_E_OPEN_INIT_LOCK OR
                DB_E_OPEN_INIT_MPOOL,
                0 ); -- TODO: dirname
        -- Create the file
        new_berkeley_session(
           theFile.btree.session,
           theFile.btree.env,
           keyLen,
           valLen );
        open( theFile.btree.session, to_string( fname_val ), "", DB_BTREE, 0, 0 );
        theFile.btree.isOpen := true;
        theFile.btree.name := fname_val;
     --exception when berkeley_error =>
     --   err( to_string( last_error( theFile.btree.session ) );
     end;
  end if;
end ParseBTreeOpen;

procedure ParseBTreeIsOpen( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := btree.is_open( f );
  -- Ada:    N/A
  fileId     : identifier;
  theFile    : resPtr;
begin
  kind := boolean_t;
  expect( btree_is_open_t );
  ParseSingleFileParameter( fileId );
  if isExecutingCommand then
     begin
        findResource( to_resource_id( identifiers( fileId ).value ), theFile );
        result := to_bush_boolean( theFile.btree.isOpen );
     exception when berkeley_error =>
        result := to_bush_boolean( false );
     end;
  end if;
end ParseBTreeIsOpen;

procedure ParseBTreeName( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := btree.name( f );
  -- Ada:    N/A
  fileId     : identifier;
  theFile    : resPtr;
begin
  kind := string_t;
  expect( btree_name_t );
  ParseSingleFileParameter( fileId );
  if isExecutingCommand then
     begin
        findResource( to_resource_id( identifiers( fileId ).value ), theFile );
        result := theFile.btree.name;
     --exception when berkeley_error =>
     --   result := null_unbounded_string;
     end;
  end if;
end ParseBTreeName;

procedure ParseBTreeDelete is
  -- Syntax: btree.delete( f );
  -- Ada:    bdb.remove( f );
  -- TODO:   keep environment
  fileId     : identifier;
  theFile    : resPtr;
begin
  expect( btree_delete_t );
  ParseSingleFileParameter( fileId );
  if isExecutingCommand then
     begin
        findResource( to_resource_id( identifiers( fileId ).value ), theFile );
        if theFile.btree.isOpen then
           close( theFile.btree.session );
        end if;
  -- TODO:   hangs on a lock
        dbremove( theFile.btree.env,
          to_string( theFile.btree.name ),
          "",
          0 );
        -- keep the environment
        if theFile.btree.isOpen then
          close( theFile.btree.env );
          -- remove( theFile.btree.env, dbhome )
        end if;
        theFile.btree.isOpen := false;
     --exception when berkeley_error =>
     --   err( to_string( last_error( theFile.btree.session ) );
     end;
  end if;
end ParseBTreeDelete;

procedure ParseBTreeSet is
  -- Syntax: btree.set( f, key, value );
  -- Ada:    bdb.put( f, key, value );
  fileId     : identifier;
  theFile    : resPtr;
  keyExpr    : unbounded_string;
  keyType    : identifier;
  valExpr    : unbounded_string;
  valType    : identifier;
begin
  expect( btree_set_t );
  ParseFirstFileParameter( fileId );
  ParseNextStringParameter( keyExpr, keyType, string_t );
  ParseLastGenItemParameter( valExpr, valType, identifiers( fileId ).genKind );
  if isExecutingCommand then
     begin
        findResource( to_resource_id( identifiers( fileId ).value ), theFile );
        put( theFile.btree.session, to_string( keyExpr ), to_string( valExpr ) );
     end;
  end if;
end ParseBTreeSet;

procedure ParseBTreeGet( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: v := btree.get( f, k );
  -- Ada:    bdb.get( f, k );
  fileId     : identifier;
  theFile    : resPtr;
  keyExpr    : unbounded_string;
  keyType    : identifier;
begin
  -- TODO: null string instead of exception to parallel dht
  expect( btree_get_t );
  ParseFirstFileParameter( fileId );
  ParseLastStringParameter( keyExpr, keyType, string_t );
  if isExecutingCommand then
     begin
        kind := identifiers( fileId ).genKind;
        findResource( to_resource_id( identifiers( fileId ).value ), theFile );
        get( theFile.btree.session, to_string( keyExpr ), result );
     exception when berkeley_error =>
        err( "key not found" );
     --   err( to_string( last_error( theFile.btree.session ) );
     end;
  end if;
end ParseBTreeGet;

procedure ParseBTreeHasElement( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: v := btree.has_element( f, k );
  -- Ada:    bdb.exists( f, k );
  fileId     : identifier;
  theFile    : resPtr;
  keyExpr    : unbounded_string;
  keyType    : identifier;
begin
  kind := boolean_t;
  expect( btree_has_element_t );
  ParseFirstFileParameter( fileId );
  ParseLastStringParameter( keyExpr, keyType, string_t );
  if isExecutingCommand then
     begin
        findResource( to_resource_id( identifiers( fileId ).value ), theFile );
        exists( theFile.btree.session, to_string( keyExpr ) );
        result := to_bush_boolean( true );
     exception when berkeley_error =>
        result := to_bush_boolean( false );
     end;
  end if;
end ParseBTreeHasElement;

procedure ParseBTreeRemove is
  -- Syntax: v := btree.remove( f, k );
  -- Ada:    bdb.delete( f, k );
  fileId     : identifier;
  theFile    : resPtr;
  keyExpr    : unbounded_string;
  keyType    : identifier;
begin
  expect( btree_remove_t );
  ParseFirstFileParameter( fileId );
  ParseLastStringParameter( keyExpr, keyType, string_t );
  if isExecutingCommand then
     begin
        findResource( to_resource_id( identifiers( fileId ).value ), theFile );
        delete( theFile.btree.session, to_string( keyExpr ) );
     -- exception when berkeley_error =>
      --   result := to_bush_boolean( false );
     end;
  end if;
end ParseBTreeRemove;

procedure ParseBtreeIncrement is
  -- Syntax: btree.increment( f, s [,n] );
  -- Ada:    N/A
  fileId     : identifier;
  theFile    : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  amtExpr  : unbounded_string;
  amtType  : identifier;
  hasAmt   : boolean := false;
  oldItem  : unbounded_string;
  oldItemValue : long_float;
begin
  expect( btree_increment_t );
  ParseFirstFileParameter( fileId );
  if getUniType( identifiers( fileId ).genKind ) /= uni_numeric_t then
     err( "increment requires a numeric item type" );
  end if;
  ParseNextStringParameter( keyExpr, keyType, uni_string_t );
  if token = symbol_t and identifiers( token ).value = "," then
     hasAmt := true;
     ParseLastNumericParameter( amtExpr, amtType, natural_t );
  elsif token = symbol_t and identifiers( token ).value = ")" then
     expect( symbol_t, ")" );
  else
     err( ", or ) expected" );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( fileId ).value ), theFile );

       get( theFile.btree.session, to_string( keyExpr ), oldItem );
       -- berkeley throws exception on not found
       --if oldItem /= null_unbounded_string then
          oldItemValue := to_numeric( oldItem );
          if hasAmt then
             put( theFile.btree.session, to_string( keyExpr ),  to_string( to_unbounded_string( oldItemValue + long_float( natural( to_numeric( amtExpr ) ) ) ) ) );
          else
             put( theFile.btree.session, to_string( keyExpr ), to_string( to_unbounded_string( oldItemValue + 1.0 ) ) );
          end if;
       --end if;
     exception when storage_error =>
       err( "storage error raised" );
     when constraint_error =>
       err( "constraint error raised" );
     -- TODO: berkeley exception handing
     end;
  end if;
end ParseBTreeIncrement;

procedure ParseBtreeDecrement is
  -- Syntax: btree.decrement( f, s [,n] );
  -- Ada:    N/A
  fileId     : identifier;
  theFile    : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  amtExpr  : unbounded_string;
  amtType  : identifier;
  hasAmt   : boolean := false;
  oldItem  : unbounded_string;
  oldItemValue : long_float;
begin
  expect( btree_decrement_t );
  ParseFirstFileParameter( fileId );
  if getUniType( identifiers( fileId ).genKind ) /= uni_numeric_t then
     err( "increment requires a numeric item type" );
  end if;
  ParseNextStringParameter( keyExpr, keyType, uni_string_t );
  if token = symbol_t and identifiers( token ).value = "," then
     hasAmt := true;
     ParseLastNumericParameter( amtExpr, amtType, natural_t );
  elsif token = symbol_t and identifiers( token ).value = ")" then
     expect( symbol_t, ")" );
  else
     err( ", or ) expected" );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( fileId ).value ), theFile );

       get( theFile.btree.session, to_string( keyExpr ), oldItem );
       -- berkeley throws exception on not found
       --if oldItem /= null_unbounded_string then
          oldItemValue := to_numeric( oldItem );
          if hasAmt then
             put( theFile.btree.session, to_string( keyExpr ),  to_string( to_unbounded_string( oldItemValue - long_float( natural( to_numeric( amtExpr ) ) ) ) ) );
          else
             put( theFile.btree.session, to_string( keyExpr ), to_string( to_unbounded_string( oldItemValue - 1.0 ) ) );
          end if;
       --end if;
     exception when storage_error =>
       err( "storage error raised" );
     when constraint_error =>
       err( "constraint error raised" );
     -- TODO: berkeley exception handing
     end;
  end if;
end ParseBTreeDecrement;

procedure ParseBtreeAdd is
  -- Syntax: btree.add( f, s, e );
  -- Ada:    N/A
  fileId     : identifier;
  theFile  : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  itemExpr : unbounded_string;
  itemType : identifier;
begin
  expect( btree_add_t );
  ParseFirstFileParameter( fileId );
  ParseNextStringParameter( keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( itemExpr, itemType, identifiers( fileId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( fileId ).value ), theFile );
       exists( theFile.btree.session, to_string( keyExpr ) );
       -- berkeley throws exception on not found.  no exception? do nothing
       --if oldItem = null_unbounded_string then
       ----end if;
     exception when storage_error =>
       err( "storage error raised" );
     when berkeley_error =>
       put( theFile.btree.session, to_string( keyExpr ), to_string( itemExpr ) );
     -- other berkeley exceptions
     end;
  end if;
end ParseBTreeAdd;

procedure ParseBtreeReplace is
  -- Syntax: btree.replace( f, s, e );
  -- Ada:    N/A
  fileId     : identifier;
  theFile  : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  itemExpr : unbounded_string;
  itemType : identifier;
begin
  expect( btree_replace_t );
  ParseFirstFileParameter( fileId );
  ParseNextStringParameter( keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( itemExpr, itemType, identifiers( fileId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( fileId ).value ), theFile );
       exists( theFile.btree.session, to_string( keyExpr ) );
       put( theFile.btree.session, to_string( keyExpr ), to_string( itemExpr ) );
     exception when storage_error =>
       err( "storage error raised" );
     when berkeley_error =>
       null;
     -- other berkeley exceptions
     end;
  end if;
end ParseBTreeReplace;

procedure ParseBtreeAppend is
  -- Syntax: btree.append( f, s, e );
  -- Ada:    N/A
  fileId     : identifier;
  theFile  : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  itemExpr : unbounded_string;
  itemType : identifier;
  oldItem  : unbounded_string;
begin
  expect( btree_append_t );
  ParseFirstFileParameter( fileId );
  if getUniType( identifiers( fileId ).genKind ) /= uni_string_t then
     err( "append requires a string item type" );
  end if;
  ParseNextStringParameter( keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( itemExpr, itemType, identifiers( fileId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( fileId ).value ), theFile );
       get( theFile.btree.session, to_string( keyExpr ), oldItem );
       put( theFile.btree.session, to_string( keyExpr ), to_string( oldItem & itemExpr ) );
     exception when storage_error =>
       err( "storage error raised" );
     when berkeley_error =>
       null; -- do not store
     -- other berkeley exceptions
     end;
  end if;
end ParseBTreeAppend;

procedure ParseBtreePrepend is
  -- Syntax: btree.prepend( f, s, e );
  -- Ada:    N/A
  fileId     : identifier;
  theFile  : resPtr;
  keyExpr  : unbounded_string;
  keyType  : identifier;
  itemExpr : unbounded_string;
  itemType : identifier;
  oldItem  : unbounded_string;
begin
  expect( btree_prepend_t );
  ParseFirstFileParameter( fileId );
  if getUniType( identifiers( fileId ).genKind ) /= uni_string_t then
     err( "append requires a string item type" );
  end if;
  ParseNextStringParameter( keyExpr, keyType, uni_string_t );
  ParseLastGenItemParameter( itemExpr, itemType, identifiers( fileId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( fileId ).value ), theFile );
       get( theFile.btree.session, to_string( keyExpr ), oldItem );
       put( theFile.btree.session, to_string( keyExpr ), to_string( itemExpr & oldItem ) );
     exception when storage_error =>
       err( "storage error raised" );
     when berkeley_error =>
       null; -- do not store
     -- other berkeley exceptions
     end;
  end if;
end ParseBTreePrepend;

procedure ParseBTreeNewCursor is
  -- Syntax: btree.new_cursor( f, t );
  -- Ada:    N/A
  resId : resHandleId;
  ref : reference;
  genKindId : identifier;
begin
  expect( btree_new_cursor_t );
  ParseFirstOutParameter( ref, btree_cursor_t );
  baseTypesOK( ref.kind, btree_cursor_t );
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
     identifiers( ref.id ).resource := true;
     declareResource( resId, btree_file, blocks_top );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParseBtreeNewCursor;


-----------------------------------------------------------------------------

procedure StartupBTree is
begin
  -- TODO: rename btree_io
  declareNamespace( "btree" );

  declareIdent( btree_file_t,   "btree.file", positive_t, typeClass );
  declareIdent( btree_cursor_t, "btree.cursor", positive_t, typeClass );

  declareProcedure( btree_new_file_t,  "btree.new_file", ParseBTreeNewFile'access );
  declareProcedure( btree_clear_t,     "btree.clear",    ParseBTreeClear'access );

  declareProcedure( btree_create_t,    "btree.create",   ParseBTreeCreate'access );
  declareProcedure( btree_close_t,     "btree.close",    ParseBTreeClose'access );
  declareProcedure( btree_open_t,      "btree.open",     ParseBTreeOpen'access );
  declareFunction(  btree_is_open_t,   "btree.is_open",  ParseBTreeIsOpen'access );
  declareFunction(  btree_name_t,      "btree.name",     ParseBTreeName'access );
  declareProcedure( btree_delete_t,    "btree.delete",   ParseBTreeDelete'access );

  declareProcedure( btree_set_t,       "btree.set",      ParseBTreeSet'access );
  declareFunction(  btree_get_t,       "btree.get",      ParseBTreeGet'access );
  declareFunction(  btree_has_element_t, "btree.has_element",  ParseBTreeHasElement'access );
  declareProcedure( btree_remove_t, "btree.remove",      ParseBTreeRemove'access );
  declareProcedure( btree_increment_t, "btree.increment",ParseBTreeIncrement'access );
  declareProcedure( btree_decrement_t, "btree.decrement",ParseBTreeDecrement'access );
  declareProcedure( btree_add_t, "btree.add", ParseBTreeAdd'access );
  declareProcedure( btree_replace_t, "btree.replace", ParseBTreeReplace'access );
  declareProcedure( btree_append_t, "btree.append", ParseBTreeAppend'access );
  declareProcedure( btree_prepend_t, "btree.prepend", ParseBTreePrepend'access );

  declareProcedure( btree_new_cursor_t,  "btree.new_cursor", ParseBTreeNewCursor'access );

  declareNamespaceClosed( "btree" );

end StartupBTree;

procedure ShutdownBTree is
begin
  null;
end ShutdownBTree;

end parser_btree;

