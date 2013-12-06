------------------------------------------------------------------------------
-- Doubly Linked Lists Package Parser                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2013 Free Software Foundation              --
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

with bush_os.exec;
use  bush_os.exec;

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
    bush_os;
use
    world,
    scanner,
    scanner_res,
    string_util,
    parser,
    parser_aux,
    parser_params,
    bush_os;

package body parser_doubly is

------------------------------------------------------------------------------
-- Utility subprograms
------------------------------------------------------------------------------

procedure CheckListIsInitialized( listId : identifier ) is
begin
  if identifiers( listId ).genKind = eof_t then
     err( "new_list has not been called to initialize the list" );
  end if;
end CheckListIsInitialized;

procedure ParseSingleListParameter( listId : out identifier ) is
begin
  ParseSingleInOutParameter( listId, doubly_list_t );
  CheckListIsInitialized( listId );
end ParseSingleListParameter;

procedure ParseFirstListParameter( listId : out identifier ) is
begin
  ParseFirstInOutParameter( listId, doubly_list_t );
  CheckListIsInitialized( listId );
end ParseFirstListParameter;

procedure ParseNextListParameter( listId : out identifier ) is
begin
  ParseNextInOutParameter( listId, doubly_list_t );
  CheckListIsInitialized( listId );
end ParseNextListParameter;

procedure ParseLastListParameter( listId : out identifier ) is
begin
  ParseLastInOutParameter( listId, doubly_list_t );
  CheckListIsInitialized( listId );
end ParseLastListParameter;

------------------------------------------------------------------------------

procedure CheckCursorIsInitialized( cursId : identifier ) is
begin
  if identifiers( cursId ).genKind = eof_t then
     err( "new_cursor has not been called to initialize the cursor" );
  end if;
end CheckCursorIsInitialized;

procedure ParseSingleCursorParameter( cursId : out identifier ) is
begin
  ParseSingleInOutParameter( cursId, doubly_cursor_t );
  CheckCursorIsInitialized( cursId );
end ParseSingleCursorParameter;

procedure ParseFirstCursorParameter( cursId : out identifier ) is
begin
  ParseFirstInOutParameter( cursId, doubly_cursor_t );
  CheckCursorIsInitialized( cursId );
end ParseFirstCursorParameter;

procedure ParseNextCursorParameter( cursId : out identifier ) is
begin
  ParseNextInOutParameter( cursId, doubly_cursor_t );
  CheckCursorIsInitialized( cursId );
end ParseNextCursorParameter;

procedure ParseLastCursorParameter( cursId : out identifier ) is
begin
  ParseLastInOutParameter( cursId, doubly_cursor_t );
  CheckCursorIsInitialized( cursId );
end ParseLastCursorParameter;


------------------------------------------------------------------------------
-- Parser subprograms
------------------------------------------------------------------------------


procedure ParseDoublyNewList is
  -- Syntax: doubly_linked_list.new_list( l );
  -- Ada:    N/A
  resId : resHandleId;
  ref : reference;
  genKindId : identifier;
begin
  expect( doubly_new_list_t );
  ParseFirstOutParameter( ref, doubly_list_t );
  baseTypesOK( ref.kind, doubly_list_t );
  expect( symbol_t, "," );
  ParseIdentifier( genKindId );
  if class_ok( genKindId, typeClass, subClass ) then
      null;
  end if;
  identifiers( ref.id ).genKind := genKindId;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     identifiers( ref.id ).resource := true;
     declareResource( resId, doubly_linked_string_list, blocks_top );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParseDoublyNewList;

procedure ParseDoublyClear is
  -- Syntax: doubly_linked_list.clear( l );
  -- Ada:    doubly_linked_list.clear( l );
  --listExpr : unbounded_string;
  --listType : identifier;
  listId   : identifier;
  theList  : resPtr;
begin
  expect( doubly_clear_t );
  ParseSingleListParameter( listId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       Doubly_Linked_String_Lists.Clear( theList.dlslList );
     end;
  end if;
end ParseDoublyClear;

procedure ParseDoublyIsEmpty( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := doubly_linked_list.is_empty( l );
  -- Ada:    b := doubly_linked_list.is_empty( l );
  listId   : identifier;
  theList  : resPtr;
begin
  kind := boolean_t;
  expect( doubly_is_empty_t );
  ParseSingleListParameter( listId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       result := to_bush_boolean( Doubly_Linked_String_Lists.Is_Empty( theList.dlslList ) );
     end;
  end if;
end ParseDoublyIsEmpty;

procedure ParseDoublyLength( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: n := doubly_linked_list.length( l );
  -- Ada:    n := doubly_linked_list.length( l );
  listId   : identifier;
  theList  : resPtr;
begin
  kind := containers_count_type_t;
  expect( doubly_length_t );
  ParseSingleListParameter( listId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       result := to_unbounded_string( long_float ( Doubly_Linked_String_Lists.Length( theList.dlslList ) ) );
     end;
  end if;
end ParseDoublyLength;

procedure ParseDoublyAppend is
  -- Syntax: doubly_linked_list.append( l, s );
  -- Ada:    doubly_linked_list.append( l, s );
  --listExpr : unbounded_string;
  --listType : identifier;
  listId : identifier;
  theList  : resPtr;
  itemExpr : unbounded_string;
  itemType : identifier;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( doubly_append_t );
  ParseFirstListParameter( listId );
  ParseNextGenItemParameter( itemExpr, itemType, identifiers( listId ).genKind );
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
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Doubly_Linked_String_Lists.Append( theList.dlslList, itemExpr, cnt );
       else
          Doubly_Linked_String_Lists.Append( theList.dlslList, itemExpr );
       end if;
     exception when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseDoublyAppend;

procedure ParseDoublyPrepend is
  -- Syntax: doubly_linked_list.prepend( l, s );
  -- Ada:    doubly_linked_list.prepend( l, s );
  --listExpr : unbounded_string;
  --listType : identifier;
  listId : identifier;
  itemExpr  : unbounded_string;
  itemType  : identifier;
  theList  : resPtr;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( doubly_prepend_t );
  ParseFirstListParameter( listId );
  ParseNextGenItemParameter( itemExpr, itemType, identifiers( listId ).genKind );
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
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Doubly_Linked_String_Lists.Prepend( theList.dlslList, itemExpr, cnt );
       else
          Doubly_Linked_String_Lists.Prepend( theList.dlslList, itemExpr );
       end if;
     exception when storage_error =>
       err( "storage error raised" );
     end;
  end if;
end ParseDoublyPrepend;

procedure ParseDoublyFirstElement( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: s := doubly_linked_list.first_element( l );
  -- Ada:    s := doubly_linked_list.first_element( l );
  listId : identifier;
  theList  : resPtr;
begin
  expect( doubly_first_element_t );
  ParseSingleListParameter( listId );
  kind := identifiers( listId ).genKind;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       result := Doubly_Linked_String_Lists.First_Element( theList.dlslList );
     end;
  end if;
end ParseDoublyFirstElement;

procedure ParseDoublyLastElement( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: s := doubly_linked_list.last_element( l );
  -- Ada:    s := doubly_linked_list.last_element( l );
  listId   : identifier;
  theList  : resPtr;
begin
  expect( doubly_last_element_t );
  ParseSingleListParameter( listId );
  kind := identifiers( listId ).genKind;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       result := Doubly_Linked_String_Lists.Last_Element( theList.dlslList );
     end;
  end if;
end ParseDoublyLastElement;

procedure ParseDoublyDeleteFirst is
  -- Syntax: doubly_linked_list.delete_first( l, c );
  -- Ada:    doubly_linked_list.delete_first( l, c );
  -- listExpr : unbounded_string;
  -- listType : identifier;
  listId   : identifier;
  theList  : resPtr;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( doubly_delete_first_t );
  ParseFirstListParameter( listId );
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
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Doubly_Linked_String_Lists.Delete_First( theList.dlslList, cnt );
       else
          Doubly_Linked_String_Lists.Delete_First( theList.dlslList );
       end if;
     end;
  end if;
end ParseDoublyDeleteFirst;

procedure ParseDoublyDeleteLast is
  -- Syntax: doubly_linked_list.delete_last( l, s );
  -- Ada:    doubly_linked_list.delete_last( l, s );
  --listExpr : unbounded_string;
  --listType : identifier;
  listId   : identifier;
  theList  : resPtr;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( doubly_delete_last_t );
  ParseFirstListParameter( listId );
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
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Doubly_Linked_String_Lists.Delete_Last( theList.dlslList, cnt );
       else
          Doubly_Linked_String_Lists.Delete_Last( theList.dlslList );
       end if;
     end;
  end if;
end ParseDoublyDeleteLast;

procedure ParseDoublyNewCursor is
  -- Syntax: doubly_linked_list.new_cursor( c );
  -- Ada:    N/A
  resId : resHandleId;
  ref : reference;
  genKindId : identifier;
begin
  expect( doubly_new_cursor_t );
  ParseFirstOutParameter( ref, doubly_cursor_t );
  baseTypesOK( ref.kind, doubly_cursor_t );
  expect( symbol_t, "," );
  ParseIdentifier( genKindId );
  if class_ok( genKindId, typeClass, subClass ) then
      null;
  end if;
  identifiers( ref.id ).genKind := genKindId;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     identifiers( ref.id ).resource := true;
     declareResource( resId, doubly_linked_string_list_cursor, blocks_top );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParseDoublyNewCursor;

procedure ParseDoublyFirst is
  -- Syntax: doubly_linked_list.first( l, c );
  -- Ada:    c := doubly_linked_list.first( l );
  listId    : identifier;
  theList   : resPtr;
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( doubly_first_t );
  ParseFirstListParameter( listId );
  ParseLastCursorParameter( cursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       theCursor.dlslCursor := Doubly_Linked_String_Lists.First( theList.dlslList );
     end;
  end if;
end ParseDoublyFirst;

procedure ParseDoublyLast is
  -- Syntax: doubly_linked_list.delete_last( l, s, c );
  -- Ada:    c := doubly_linked_list.last( l );
  listId    : identifier;
  theList   : resPtr;
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( doubly_last_t );
  ParseFirstListParameter( listId );
  ParseLastCursorParameter( cursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       theCursor.dlslCursor := Doubly_Linked_String_Lists.Last( theList.dlslList );
     end;
  end if;
end ParseDoublyLast;

procedure ParseDoublyNext is
  -- Syntax: doubly_linked_list.next( c );
  -- Ada:    doubly_linked_list.next( c );
  --cursExpr  : unbounded_string;
  --cursType  : identifier;
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( doubly_next_t );
  ParseSingleCursorParameter( cursId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       Doubly_Linked_String_Lists.Next( theCursor.dlslCursor );
     end;
  end if;
end ParseDoublyNext;

procedure ParseDoublyPrevious is
  -- Syntax: doubly_linked_list.previous( c );
  -- Ada:    doubly_linked_list.previous( c );
  --cursExpr  : unbounded_string;
  --cursType  : identifier;
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( doubly_previous_t );
  ParseSingleCursorParameter( cursId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       Doubly_Linked_String_Lists.Previous( theCursor.dlslCursor );
     end;
  end if;
end ParseDoublyPrevious;

procedure ParseDoublyElement( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: s := doubly_linked_list.element( c );
  -- Ada:    s := doubly_linked_list.element( c );
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( doubly_element_t );
  ParseSingleCursorParameter( cursId );
  kind := identifiers( cursId ).genKind;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       result := Doubly_Linked_String_Lists.Element( theCursor.dlslCursor );
     exception when constraint_error =>
       err( "position cursor has no element" );
     end;
  end if;
end ParseDoublyElement;

procedure ParseDoublyReplaceElement is
  -- Syntax: doubly_linked_list.replace_element( l, c, s );
  -- Ada:    doubly_linked_list.replace_element( l ,c, s);
  --listExpr  : unbounded_string;
  --listType  : identifier;
  listId    : identifier;
  theList   : resPtr;
  cursId    : identifier;
  theCursor : resPtr;
  itemExpr  : unbounded_string;
  itemType  : identifier;
begin
  expect( doubly_replace_element_t );
  ParseFirstListParameter( listId );
  ParseNextCursorParameter( cursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  ParseLastGenItemParameter( itemExpr, itemType, identifiers( listId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       Doubly_Linked_String_Lists.Replace_Element( theList.dlslList, theCursor.dlslCursor, itemExpr );
     end;
  end if;
end ParseDoublyReplaceElement;

-- Insert Family
--
-- The Original Ada is:
--
--procedure Insert (Container : in out List;
--                  Before    : in     Cursor;
--                  New_Item  : in     Element_Type;
--                  Count     : in     Count_Type := 1);
--If Before is not No_Element, and does not designate an element in Container, then Program_Error is propagated. Otherwise, Insert inserts Count copies of New_Item prior to the element designated by Before. If Before equals No_Element, the new elements are inserted after the last node (if any). Any exception raised during allocation of internal storage is propagated, and Container is not modified.
--procedure Insert (Container : in out List;
--                  Before    : in     Cursor;
--                  New_Item  : in     Element_Type;
--                  Position  :    out Cursor;
--                  Count     : in     Count_Type := 1);
--If Before is not No_Element, and does not designate an element in Container, then Program_Error is propagated. Otherwise, Insert allocates Count copies of New_Item, and inserts them prior to the element designated by Before. If Before equals No_Element, the new elements are inserted after the last element (if any). Position designates the first newly-inserted element. Any exception raised during allocation of internal storage is propagated, and Container is not modified.
--procedure Insert (Container : in out List;
--                  Before    : in     Cursor;
--                  Position  :    out Cursor;
--                  Count     : in     Count_Type := 1);
--If Before is not No_Element, and does not designate an element in Container, then Program_Error is propagated. Otherwise, Insert inserts Count new elements prior to the element designated by Before. If Before equals No_Element, the new elements are inserted after the last node (if any). The new elements are initialized by default (see 3.3.1). Any exception raised during allocation of internal storage is propagated, and Container is not modified.
--
-- The third parameter can be a cursor, a new item, or an expression.  The
-- problem is that we may not be able to distinguish between the new item
-- and the count because both could be numeric.  So we have to have two
-- inserts to differentiate the cases.

procedure ParseDoublyInsertBefore is
  -- Syntax: doubly_linked_list.insert_before( l, c, s [, n] );
  -- Ada:    doubly_linked_list.insert( l ,c, s [, n]);
  -- This is the basic form of insert that doesn't return another cursor.
  -- Note: "insert" is a reserved word in SparForte
  listId     : identifier;
  theList    : resPtr;
  cursId     : identifier;
  theCursor  : resPtr;
  itemExpr   : unbounded_string;
  itemType   : identifier;
  cntExpr    : unbounded_string;
  cntType    : identifier;
  hasItem    : boolean := false;
  hasCnt     : boolean := false;
begin
  expect( doubly_insert_before_t );
  ParseFirstListParameter( listId );
  ParseNextCursorParameter( cursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  ParseNextGenItemParameter( itemExpr, itemType, identifiers( listId ).genKind );
  genTypesOk( identifiers( listId ).genKind, itemType );
  if token = symbol_t and identifiers( token ).value = "," then
     ParseLastNumericParameter( cntExpr, cntType );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Doubly_Linked_String_Lists.Insert( theList.dlslList, theCursor.dlslCursor, itemExpr, cnt );
       else
          Doubly_Linked_String_Lists.Insert( theList.dlslList, theCursor.dlslCursor, itemExpr );
       end if;
     exception when constraint_error =>
       err( "constraint error raised" );
     end;
  end if;
end ParseDoublyInsertBefore;

procedure ParseDoublyInsertBeforeAndMark is
  -- Syntax: doubly_linked_list.insert_before_with_mark( l, c, c2 [, n] );
  --         doubly_linked_list.insert_before_with_mark( l, c, s, c2 [, n] );
  -- Ada:    doubly_linked_list.insert( l ,c, s [, n]);
  --         doubly_linked_list.insert( l ,c, c2 [, n]);
  --         doubly_linked_list.insert( l ,c, s, c2 [, n]);
  -- Note: inserts before the cursor position
  --       "insert" is a reserved word in SparForte

  listId     : identifier;
  theList    : resPtr;
  cursId     : identifier;
  theCursor  : resPtr;
  theSecondCursor : resPtr;
  itemExpr   : unbounded_string;
  itemType   : identifier;
  cntExpr    : unbounded_string;
  cntType    : identifier;
  hasItem    : boolean := false;
  hasCnt     : boolean := false;
  resId : resHandleId;
  ref        : reference;
begin
  expect( doubly_insert_before_and_mark_t );
  ParseFirstListParameter( listId );
  ParseNextCursorParameter( cursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  expect( symbol_t, "," );
  -- The third parameter can be a cursor or a new item.
  if getBaseType( identifiers( token ).kind ) = doubly_cursor_t or
     getBaseType( identifiers( token ).kind ) = new_t then
     ParseOutParameter( ref, doubly_cursor_t );
     baseTypesOK( ref.kind, doubly_cursor_t );
     if token = symbol_t and identifiers( token ).value = "," then
        getNextToken;
        ParseNumericParameter( cntExpr, cntType );
        if getBaseType( cntType ) /= containers_count_type_t and
           getBaseType( cntType ) /= uni_numeric_t then
           err( "containers.count_type or doubly_linked_lists.cursor expected" );
        end if;
        hasCnt := true;
     end if;
     expect( symbol_t, ")" );
  else
     ParseGenItemParameter( itemExpr, itemType, identifiers( listId ).genKind );
     genTypesOk( identifiers( listId ).genKind, itemType );
     ParseNextOutParameter( ref, doubly_cursor_t );
     baseTypesOK( ref.kind, doubly_cursor_t );
     if token = symbol_t and identifiers( token ).value = "," then
        getNextToken;
        ParseNumericParameter( cntExpr, cntType );
        if getBaseType( cntType ) /= containers_count_type_t and
           getBaseType( cntType ) /= uni_numeric_t then
           err( "containers.count_type or doubly_linked_lists.cursor expected" );
        end if;
        hasCnt := true;
     end if;
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       -- the second cursor is the out parameter.  Declare it.  Then fetch it.
       identifiers( ref.id ).resource := true;
       identifiers( ref.id ).genKind := identifiers( listId ).genKind;
       declareResource( resId, doubly_linked_string_list_cursor, blocks_top );
       AssignParameter( ref, to_unbounded_string( resId ) );
       findResource( resId, theSecondCursor );
       -- there are four variations
       if hasItem and hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Doubly_Linked_String_Lists.Insert( theList.dlslList, theCursor.dlslCursor, itemExpr, theSecondCursor.dlslCursor, cnt );
       elsif hasItem then
          Doubly_Linked_String_Lists.Insert( theList.dlslList, theCursor.dlslCursor, itemExpr, theSecondCursor.dlslCursor );
       elsif hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          Doubly_Linked_String_Lists.Insert( theList.dlslList, theCursor.dlslCursor, theSecondCursor.dlslCursor, cnt );
       else
          Doubly_Linked_String_Lists.Insert( theList.dlslList, theCursor.dlslCursor, theSecondCursor.dlslCursor );
       end if;
     exception when constraint_error =>
       err( "constraint error raised" );
     end;
  end if;
end ParseDoublyInsertBeforeAndMark;

procedure ParseDoublyDelete is
  -- Syntax: doubly_linked_list.delete( l, c,[, n] );
  -- Ada:    doubly_linked_list.delete( l ,c [, n]);
  --listExpr  : unbounded_string;
  --listType  : identifier;
  listId   : identifier;
  theList   : resPtr;
  --cursExpr  : unbounded_string;
  --cursType  : identifier;
  cursId    : identifier;
  theCursor : resPtr;
  cntExpr   : unbounded_string;
  cntType   : identifier;
  hasCnt    : boolean := false;
begin
  expect( doubly_delete_t );
  ParseFirstListParameter( listId );
  ParseNextCursorParameter( cursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
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
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       if hasCnt then
          begin
            cnt := Ada.Containers.Count_Type( to_numeric( cntExpr ) );
          exception when constraint_error =>
            err( "constraint error raised" );
          end;
          Doubly_Linked_String_Lists.Delete( theList.dlslList, theCursor.dlslCursor, cnt );
       else
          Doubly_Linked_String_Lists.Delete( theList.dlslList, theCursor.dlslCursor );
       end if;
     exception when constraint_error =>
       err( "position cursor has no element" );
     end;
  end if;
end ParseDoublyDelete;

procedure ParseDoublyContains( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := doubly_linked_list.contains( l, s );
  -- Ada:    b := doubly_linked_list.contains( l, s );
  listId   : identifier;
  theList  : resPtr;
  itemExpr : unbounded_string;
  itemType : identifier;
begin
  kind := boolean_t;
  expect( doubly_contains_t );
  ParseFirstListParameter( listId );
  ParseLastGenItemParameter( itemExpr, itemType, identifiers( listId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       result := to_bush_boolean( Doubly_Linked_String_Lists.Contains( theList.dlslList, itemExpr ) );
     end;
  end if;
end ParseDoublyContains;

procedure ParseDoublyFind is
  -- Syntax: doubly_linked_list.find( l, s, c );
  -- Ada:    c := doubly_linked_list.find( l, s );
  listId    : identifier;
  theList   : resPtr;
  itemExpr  : unbounded_string;
  itemType  : identifier;
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( doubly_find_t );
  ParseFirstListParameter( listId );
  ParseNextGenItemParameter( itemExpr, itemType, identifiers( listId ).genKind );
  ParseLastCursorParameter( cursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       theCursor.dlslCursor := Doubly_Linked_String_Lists.Find( theList.dlslList, itemExpr );
     end;
  end if;
end ParseDoublyFind;

procedure ParseDoublyReverseFind is
  -- Syntax: doubly_linked_list.reverse_find( l, s, c );
  -- Ada:    c := doubly_linked_list.reverse_find( l, s );
  listId    : identifier;
  theList   : resPtr;
  itemExpr  : unbounded_string;
  itemType  : identifier;
  cursId    : identifier;
  theCursor : resPtr;
begin
  expect( doubly_reverse_find_t );
  ParseFirstListParameter( listId );
  ParseNextGenItemParameter( itemExpr, itemType, identifiers( listId ).genKind );
  ParseLastCursorParameter( cursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       theCursor.dlslCursor := Doubly_Linked_String_Lists.Reverse_Find( theList.dlslList, itemExpr );
     end;
  end if;
end ParseDoublyReverseFind;

procedure ParseDoublyReverseElements is
  -- Syntax: doubly_linked_list.reverse_elements( l );
  -- Ada:    doubly_linked_list.reverse_elements( l );
  --listExpr : unbounded_string;
  --listType : identifier;
  listId   : identifier;
  theList  : resPtr;
begin
  expect( doubly_reverse_elements_t );
  ParseSingleListParameter( listId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       Doubly_Linked_String_Lists.Reverse_Elements( theList.dlslList );
     end;
  end if;
end ParseDoublyReverseElements;

procedure ParseDoublyFlip is
  -- Syntax: doubly_linked_list.flip( l );
  -- Ada:    N/A (alias for reverse_elements)
  listId   : identifier;
  theList  : resPtr;
begin
  expect( doubly_flip_t );
  ParseSingleListParameter( listId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       Doubly_Linked_String_Lists.Reverse_Elements( theList.dlslList );
     end;
  end if;
end ParseDoublyFlip;

procedure ParseDoublyAssign is
  -- Syntax: doubly_linked_list.assign( l1, l2 );
  -- Ada:    doubly_linked_list.assign( l1, l2 );
  sourceListId  : identifier;
  theSourceList : resPtr;
  targetListId  : identifier;
  theTargetList : resPtr;
begin
  expect( doubly_assign_t );
  ParseFirstListParameter( targetListId );
  ParseLastListParameter( sourceListId );
  genTypesOk( identifiers( targetListId ).genKind, identifiers( sourceListId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetListId ).value ), theTargetList );
       findResource( to_resource_id( identifiers( sourceListId ).value ), theSourceList );
       Doubly_Linked_String_Lists.Assign( theTargetList.dlslList, theSourceList.dlslList );
     end;
  end if;
end ParseDoublyAssign;

procedure ParseDoublyMove is
  -- Syntax: doubly_linked_list.move( l1, l2 );
  -- Ada:    doubly_linked_list.move( l1, l2 );
  --sourceListExpr : unbounded_string;
  --sourceListType : identifier;
  sourceListId   : identifier;
  theSourceList  : resPtr;
  --targetListExpr : unbounded_string;
  --targetListType : identifier;
  targetListId   : identifier;
  theTargetList  : resPtr;
begin
  expect( doubly_move_t );
  ParseFirstListParameter( targetListId );
  ParseLastListParameter( sourceListId );
  genTypesOk( identifiers( targetListId ).genKind, identifiers( sourceListId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetListId ).value ), theTargetList );
       findResource( to_resource_id( identifiers( sourceListId ).value ), theSourceList );
       Doubly_Linked_String_Lists.Move( theTargetList.dlslList, theSourceList.dlslList );
     end;
  end if;
end ParseDoublyMove;

procedure ParseDoublySwap is
  -- Syntax: doubly_linked_list.swap( l, c1, c2 );
  -- Ada:    doubly_linked_list.swap( l, c1, c2 );
  -- Swaps values stored at c1, c2.
  listId         : identifier;
  theList        : resPtr;
  firstCursId    : identifier;
  theFirstCursor : resPtr;
  secondCursId   : identifier;
  theSecondCursor: resPtr;
begin
  expect( doubly_swap_t );
  ParseFirstListParameter( listId );
  ParseNextCursorParameter( firstCursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( firstCursId ).genKind );
  ParseLastCursorParameter( secondCursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( secondCursId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       findResource( to_resource_id( identifiers( firstCursId ).value ), theFirstCursor );
       findResource( to_resource_id( identifiers( secondCursId ).value ), theSecondCursor );
       Doubly_Linked_String_Lists.Swap( theList.dlslList, theFirstCursor.dlslCursor, theSecondCursor.dlslCursor );
     exception when constraint_error =>
       err( "cursor has no element" );
     end;
  end if;
end ParseDoublySwap;

procedure ParseDoublySwapLinks is
  -- Syntax: doubly_linked_list.swap( l, c1, c2 );
  -- Ada:    doubly_linked_list.swap( l, c1, c2 );
  -- Swaps the list nodes stored at c1, c2.  Cursors will follow the node as it moves as it's just a pointer to the node.
  listId         : identifier;
  theList        : resPtr;
  firstCursId    : identifier;
  theFirstCursor : resPtr;
  secondCursId   : identifier;
  theSecondCursor: resPtr;
begin
  expect( doubly_swap_links_t );
  ParseFirstListParameter( listId );
  ParseNextCursorParameter( firstCursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( firstCursId ).genKind );
  ParseLastCursorParameter( secondCursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( secondCursId ).genKind );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).value ), theList );
       findResource( to_resource_id( identifiers( firstCursId ).value ), theFirstCursor );
       findResource( to_resource_id( identifiers( secondCursId ).value ), theSecondCursor );
       Doubly_Linked_String_Lists.Swap_Links( theList.dlslList, theFirstCursor.dlslCursor, theSecondCursor.dlslCursor );
     exception when constraint_error =>
       err( "cursor has no element" );
     end;
  end if;
end ParseDoublySwapLinks;

procedure ParseDoublySplice is
  -- Syntax: doubly_linked_list.splice( l1, c, l2 [,c2] );
  -- Ada:    doubly_linked_list.splice( l1, c, l2 );
  -- Append one list to another, or a node from one list to another, or
  -- a node within one list.
  sourceListId    : identifier;
  theSourceList   : resPtr;
  targetListId    : identifier;
  theTargetList   : resPtr;
  cursId          : identifier;
  theCursor       : resPtr;
  curs2Id         : identifier;
  theSecondCursor : resPtr;
  tempId          : identifier;
  hasSourceId     : boolean := false;
  hasCurs2        : boolean := false;
begin
  expect( doubly_splice_t );
  ParseFirstListParameter( targetListId );
  ParseNextCursorParameter( cursId );
  genTypesOk( identifiers( targetListId ).genKind, identifiers( cursId ).genKind );
  -- There's no easy way to handle this.  Two optional parameters, one is
  -- in out and one isn't.
  expect( symbol_t, "," );
  ParseIdentifier( tempId );
  if getBaseType( identifiers( tempId ).kind ) = doubly_list_t then
     sourceListId := tempId;
     hasSourceId := true;
     genTypesOk( identifiers( targetListId ).genKind, identifiers( sourceListId ).genKind );
     if token = symbol_t and identifiers( token ).value = "," then
        ParseLastCursorParameter( curs2Id );
        hasCurs2 := true;
        genTypesOk( identifiers( targetListId ).genKind, identifiers( curs2Id ).genKind );
     else
        expect( symbol_t, ")" );
     end if;
  elsif getBaseType( identifiers( tempId ).kind ) = doubly_cursor_t then
     -- technically this is an in parameter so could be an expression.  But
     -- it never will be.
     curs2Id := tempId;
     hasCurs2 := true;
     genTypesOk( identifiers( targetListId ).genKind, identifiers( curs2Id ).genKind );
     expect( symbol_t, ")" );
  else
     err( "list or cursor expected" );
  end if;

  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetListId ).value ), theTargetList );
       findResource( to_resource_id( identifiers( cursId ).value ), theCursor );
       if hasSourceId then
          findResource( to_resource_id( identifiers( sourceListId ).value ), theSourceList );
       end if;
       if hasCurs2 then
          findResource( to_resource_id( identifiers( curs2Id ).value ), theSecondCursor );
       end if;

       -- There are 3 variations to this function
       -- doubly_linked_list.splice( l1, c, l2 );

       if not hasCurs2 and hasSourceId then
          Doubly_Linked_String_Lists.Splice(
              theTargetList.dlslList,
              theCursor.dlslCursor,
              theSourceList.dlslList );

       -- doubly_linked_list.splice( l1, c, l2, c2 );
       elsif hasCurs2 and hasSourceId then
          Doubly_Linked_String_Lists.Splice(
              theTargetList.dlslList,
              theCursor.dlslCursor,
              theSourceList.dlslList,
              theSecondCursor.dlslCursor );

       -- doubly_linked_list.splice( l1, c, c2 );
       elsif hasCurs2 and not hasSourceId then
          Doubly_Linked_String_Lists.Splice(
              theTargetList.dlslList,
              theCursor.dlslCursor,
              theSecondCursor.dlslCursor );

       else
          err( "internal error: unexpected splice variation" );
       end if;

     end;
  end if;
end ParseDoublySplice;

procedure ParseDoublyHasElement( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := doubly_linked_list.has_element( l );
  -- Ada:    b := doubly_linked_list.has_element( l );
  cursExpr  : unbounded_string;
  cursType  : identifier;
  theCursor : resPtr;
  use Doubly_Linked_String_Lists; -- needed for =
begin
  kind := boolean_t;
  expect( doubly_has_element_t );
  ParseSingleNumericParameter( cursExpr, cursType, doubly_cursor_t );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( cursExpr ), theCursor );
       result := to_bush_boolean( theCursor.dlslCursor /= Doubly_Linked_String_Lists.No_Element );
     end;
  end if;
end ParseDoublyHasElement;

procedure StartupDoubly is
begin
  -- These two are actually mod types but we don't support that yet
  declareIdent( containers_count_type_t, "containers.count_type", natural_t, typeClass );
  declareIdent( containers_hash_type_t, "containers.hash_type", natural_t, typeClass );

  declareIdent( doubly_list_t,   "doubly_linked_lists.list", positive_t, typeClass );
  declareIdent( doubly_cursor_t, "doubly_linked_lists.cursor", positive_t, typeClass );

  declareProcedure( doubly_new_list_t,  "doubly_linked_lists.new_list", ParseDoublyNewList'access );
  declareProcedure( doubly_clear_t,     "doubly_linked_lists.clear",    ParseDoublyClear'access );
  declareFunction(  doubly_is_empty_t,  "doubly_linked_lists.is_empty", ParseDoublyIsEmpty'access );
  declareFunction(  doubly_length_t,    "doubly_linked_lists.length",   ParseDoublyLength'access );
  declareProcedure( doubly_append_t,    "doubly_linked_lists.append",   ParseDoublyAppend'access );
  declareProcedure( doubly_prepend_t,   "doubly_linked_lists.prepend",  ParseDoublyPrepend'access );
  declareFunction(  doubly_first_element_t, "doubly_linked_lists.first_element", ParseDoublyFirstElement'access );
  declareFunction(  doubly_last_element_t,  "doubly_linked_lists.last_element", ParseDoublyLastElement'access );
  declareProcedure( doubly_delete_first_t,  "doubly_linked_lists.delete_first",  ParseDoublyDeleteFirst'access );
  declareProcedure( doubly_delete_last_t,   "doubly_linked_lists.delete_last",  ParseDoublyDeleteLast'access );

  declareProcedure( doubly_new_cursor_t, "doubly_linked_lists.new_cursor", ParseDoublyNewCursor'access );
  declareProcedure( doubly_first_t,     "doubly_linked_lists.first",    ParseDoublyFirst'access );
  declareProcedure( doubly_last_t,      "doubly_linked_lists.last",     ParseDoublyLast'access );
  declareProcedure( doubly_next_t,      "doubly_linked_lists.next",     ParseDoublyNext'access );
  declareProcedure( doubly_previous_t,  "doubly_linked_lists.previous", ParseDoublyPrevious'access );
  declareFunction(  doubly_element_t,   "doubly_linked_lists.element",  ParseDoublyElement'access );
  declareProcedure( doubly_replace_element_t, "doubly_linked_lists.replace_element",  ParseDoublyReplaceElement'access );
  declareProcedure( doubly_insert_before_t,   "doubly_linked_lists.insert_before",   ParseDoublyInsertBefore'access );
  declareProcedure( doubly_insert_before_and_mark_t,   "doubly_linked_lists.insert_before_and_mark",   ParseDoublyInsertBeforeAndMark'access );
  declareProcedure( doubly_delete_t,    "doubly_linked_lists.delete",   ParseDoublyDelete'access );
  declareFunction(  doubly_contains_t,  "doubly_linked_lists.contains", ParseDoublyContains'access );
  declareProcedure( doubly_find_t,      "doubly_linked_lists.find",     ParseDoublyFind'access );
  declareProcedure( doubly_reverse_find_t, "doubly_linked_lists.reverse_find", ParseDoublyReverseFind'access );

  declareProcedure( doubly_reverse_elements_t, "doubly_linked_lists.reverse_elements",   ParseDoublyReverseElements'access );
  declareProcedure( doubly_flip_t,      "doubly_linked_lists.flip",   ParseDoublyFlip'access );
  declareFunction(  doubly_has_element_t, "doubly_linked_lists.has_element", ParseDoublyHasElement'access );

  declareProcedure( doubly_assign_t,    "doubly_linked_lists.assign",   ParseDoublyAssign'access );
  declareProcedure( doubly_move_t,      "doubly_linked_lists.move",     ParseDoublyMove'access );
  declareProcedure( doubly_swap_t,      "doubly_linked_lists.swap",     ParseDoublySwap'access );
  declareProcedure( doubly_swap_links_t,"doubly_linked_lists.swap_links", ParseDoublySwapLinks'access );
  declareProcedure( doubly_splice_t,    "doubly_linked_lists.splice",   ParseDoublySplice'access );


  -- copy not implemented
  -- TODO: to array, to json?, to string/image
  -- TODO: iterate, reverse_iterate? possible?
  -- TODO: write/echo, read as a command - not easy because commands only take strings
  -- TODO: shuffle, bubble_sort, heap_sort
  -- TODO: genTypesOk refactor
  -- TODO: error messages do not appear on the token
  -- TODO: complete insert variations

end StartupDoubly;

procedure ShutdownDoubly is
begin
  null;
end ShutdownDoubly;

end parser_doubly;

