------------------------------------------------------------------------------
-- Doubly Linked Lists Package Parser                                       --
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
    Ada.Containers,
    ada.strings.unbounded,
    pegasoft.user_io,
    world,
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
use
    ada.strings.unbounded,
    pegasoft,
    world,
    pegasoft.user_io,
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

package body parser_doubly is

------------------------------------------------------------------------------
-- Doubly Linked Lists package identifiers
------------------------------------------------------------------------------

-- doubly_list_t          : identifier;
-- doubly_cursor_t        : identifier;

doubly_clear_t         : identifier;
doubly_is_empty_t      : identifier;
doubly_length_t        : identifier;
doubly_append_t        : identifier;
doubly_prepend_t       : identifier;
doubly_first_element_t : identifier;
doubly_last_element_t  : identifier;
doubly_delete_first_t  : identifier;
doubly_delete_last_t   : identifier;

doubly_first_t         : identifier;
doubly_last_t          : identifier;
doubly_next_t          : identifier;
doubly_previous_t      : identifier;
doubly_element_t       : identifier;
doubly_replace_element_t : identifier;
doubly_insert_before_t : identifier;
doubly_insert_before_and_mark_t : identifier;
doubly_delete_t        : identifier;
doubly_contains_t      : identifier;
doubly_find_t          : identifier;
doubly_reverse_find_t  : identifier;

doubly_reverse_elements_t : identifier;
doubly_flip_t          : identifier;
doubly_assign_t        : identifier;
doubly_move_t          : identifier;
doubly_swap_t          : identifier;
doubly_swap_links_t    : identifier;
doubly_splice_t        : identifier;
doubly_has_element_t   : identifier;

doubly_assemble_t      : identifier;
doubly_disassemble_t   : identifier;
doubly_parcel_t        : identifier;

------------------------------------------------------------------------------
-- Utility subprograms
------------------------------------------------------------------------------
-- TODO: these should be removed

procedure ParseSingleListParameter( subprogram : identifier; listId : out identifier ) is
begin
  ParseSingleInOutInstantiatedParameter( subprogram, listId, doubly_list_t );
end ParseSingleListParameter;

procedure ParseNextListParameter( subprogram : identifier; listId : out identifier ) is
begin
  ParseNextInOutInstantiatedParameter( subprogram, listId, doubly_list_t );
end ParseNextListParameter;

procedure ParseLastListParameter( subprogram : identifier; listId : out identifier ) is
begin
  ParseLastInOutInstantiatedParameter( subprogram, listId, doubly_list_t );
end ParseLastListParameter;

------------------------------------------------------------------------------

procedure ParseSingleCursorParameter( subprogram : identifier; cursId : out identifier ) is
begin
  ParseSingleInOutInstantiatedParameter( subprogram, cursId, doubly_cursor_t );
end ParseSingleCursorParameter;

procedure ParseNextCursorParameter( subprogram : identifier; cursId : out identifier ) is
begin
  ParseNextInOutInstantiatedParameter( subprogram, cursId, doubly_cursor_t );
end ParseNextCursorParameter;

procedure ParseLastCursorParameter( subprogram : identifier; cursId : out identifier ) is
begin
  ParseLastInOutInstantiatedParameter( subprogram, cursId, doubly_cursor_t );
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
     err( +"doubly_linked_lists.cursor or list item expected" );
     return false;
  end if;
  return true;
end insertTypesOk;

------------------------------------------------------------------------------
-- Parser subprograms
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  CLEAR
--
-- Syntax: doubly_linked_list.clear( l );
-- Ada:    doubly_linked_list.clear( l );
-- Delete the contents of list l.
------------------------------------------------------------------------------

procedure ParseDoublyClear is
  listId   : identifier;
  theList  : resPtr;
  subprogramId : constant identifier := doubly_clear_t;
begin
  expect( subprogramId );
  ParseSingleListParameter( doubly_clear_t, listId );
  if isExecutingCommand then
     -- Technically, I should check each item for the right to delete but
     -- I have not done so here.
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       Doubly_Linked_Storage_Lists.Clear( theList.dlslList );
     end;
  end if;
end ParseDoublyClear;


------------------------------------------------------------------------------
--  IS EMPTY
--
-- Syntax: b := doubly_linked_list.is_empty( l );
-- Ada:    b := doubly_linked_list.is_empty( l );
-- Return true if the list has no elements.
------------------------------------------------------------------------------

procedure ParseDoublyIsEmpty( result : out storage; kind : out identifier ) is
  listId   : identifier;
  theList  : resPtr;
  subprogramId : constant identifier := doubly_is_empty_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleListParameter( subprogramId, listId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       result := storage'( to_spar_boolean( Doubly_Linked_Storage_Lists.Is_Empty( theList.dlslList ) ),
          noMetaLabel );
     end;
  end if;
end ParseDoublyIsEmpty;


------------------------------------------------------------------------------
--  LENGTH
--
-- Syntax: n := doubly_linked_list.length( l );
-- Ada:    n := doubly_linked_list.length( l );
-- Return the number of elements in the list.
------------------------------------------------------------------------------

procedure ParseDoublyLength( result : out storage; kind : out identifier ) is
  listId   : identifier;
  theList  : resPtr;
  subprogramId : constant identifier := doubly_length_t;
begin
  kind := containers_count_type_t;
  expect( subprogramId );
  ParseSingleListParameter( subprogramId, listId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       result := storage'( to_unbounded_string( numericValue ( Doubly_Linked_Storage_Lists.Length( theList.dlslList ) ) ), noMetaLabel );
     end;
  end if;
end ParseDoublyLength;


------------------------------------------------------------------------------
--  APPEND
--
-- Syntax: doubly_linked_list.append( l, s [,c] );
-- Ada:    doubly_linked_list.append( l, s [,c] );
-- Queue list element e at the end of the list (not the end of an element).
------------------------------------------------------------------------------

procedure ParseDoublyAppend is
  listId    : identifier;
  theList   : resPtr;
  itemExpr  : storage;
  itemType  : identifier;
  cntExpr   : storage;
  cntType   : identifier;
  hasCnt    : boolean := false;
  subprogramId : constant identifier := doubly_append_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     ParseNextGenItemParameter( subprogramId, itemExpr, itemType, identifiers( listId ).genKind );
  end if;
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          Doubly_Linked_Storage_Lists.Append( theList.dlslList, itemExpr, cnt );
       else
          Doubly_Linked_Storage_Lists.Append( theList.dlslList, itemExpr );
       end if;
     exception when constraint_error =>
       err( +"append count must be a natural integer" );
     when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDoublyAppend;


------------------------------------------------------------------------------
--  PREPEND
--
-- Syntax: doubly_linked_list.prepend( l, s );
-- Ada:    doubly_linked_list.prepend( l, s );
-- Push list element e on the front of the list. (not the front of an element).
------------------------------------------------------------------------------

procedure ParseDoublyPrepend is
  listId : identifier;
  itemExpr  : storage;
  itemType  : identifier;
  theList  : resPtr;
  cntExpr   : storage;
  cntType   : identifier;
  hasCnt    : boolean := false;
  subprogramId : constant identifier := doubly_prepend_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     ParseNextGenItemParameter( subprogramId, itemExpr, itemType, identifiers( listId ).genKind );
  end if;
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          Doubly_Linked_Storage_Lists.Prepend( theList.dlslList, itemExpr, cnt );
       else
          Doubly_Linked_Storage_Lists.Prepend( theList.dlslList, itemExpr );
       end if;
     exception when constraint_error =>
       err( +"append count must be a natural integer" );
     when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDoublyPrepend;


------------------------------------------------------------------------------
--  FIRST ELEMENT
--
-- Syntax: s := doubly_linked_list.first_element( l );
-- Ada:    s := doubly_linked_list.first_element( l );
-- Return the first list element.
------------------------------------------------------------------------------

procedure ParseDoublyFirstElement( result : out storage; kind : out identifier ) is
  listId   : identifier;
  theList  : resPtr;
  oldElem  : storage;
  subprogramId : constant identifier := doubly_first_element_t;
begin
  expect( subprogramId );
  ParseSingleListParameter( subprogramId, listId );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     kind := identifiers( listId ).genKind;
  else
     kind := eof_t;
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       oldElem := Doubly_Linked_Storage_Lists.First_Element( theList.dlslList );
       if metaLabelOk( oldElem ) then
          result := oldElem;
       end if;
     end;
  end if;
end ParseDoublyFirstElement;


------------------------------------------------------------------------------
--  LAST ELEMENT
--
-- Syntax: s := doubly_linked_list.last_element( l );
-- Ada:    s := doubly_linked_list.last_element( l );
-- Return the last list element.
------------------------------------------------------------------------------

procedure ParseDoublyLastElement( result : out storage; kind : out identifier ) is
  listId   : identifier;
  theList  : resPtr;
  oldElem  : storage;
  subprogramId : constant identifier := doubly_last_element_t;
begin
  expect( subprogramId );
  ParseSingleListParameter( subprogramId, listId );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     kind := identifiers( listId ).genKind;
  else
     kind := eof_t;
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       oldElem := Doubly_Linked_Storage_Lists.Last_Element( theList.dlslList );
       if metaLabelOk( oldElem ) then
          result := oldElem;
       end if;
     end;
  end if;
end ParseDoublyLastElement;


------------------------------------------------------------------------------
--  DELETE FIRST
--
-- Syntax: doubly_linked_list.delete_first( l, [, n] );
-- Ada:    doubly_linked_list.delete_first( l, [, n] );
-- Remove the list element from the start of the list. If n exists, remove n
-- items instead of one.
------------------------------------------------------------------------------

procedure ParseDoublyDeleteFirst is
  listId   : identifier;
  theList  : resPtr;
  cntExpr   : storage;
  cntType   : identifier;
  hasCnt    : boolean := false;
  subprogramId : constant identifier := doubly_delete_first_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          -- Although delete_first has a count parameter, we cannot use it.
          -- Different elements may have different tags associated with the
          -- values and we have to test each element separately.
          for i in 1..cnt loop
             -- Note: check-before-use.  This will be an issue if concurrency is
             -- introduced.
              if metaLabelOK( Doubly_Linked_Storage_Lists.First_Element( theList.dlslList ) ) then
                 Doubly_Linked_Storage_Lists.Delete_First( theList.dlslList );
              end if;
          end loop;
       else
          -- Note: check-before-use.  This will be an issue if concurrency is
          -- introduced.
          if metaLabelOK( Doubly_Linked_Storage_Lists.First_Element( theList.dlslList ) ) then
             Doubly_Linked_Storage_Lists.Delete_First( theList.dlslList );
          end if;
       end if;
     exception when constraint_error =>
       err( +"no more elements" );
     end;
  end if;
end ParseDoublyDeleteFirst;


------------------------------------------------------------------------------
--  DELETE LAST
--
-- Syntax: doubly_linked_list.delete_last( l, [, n] );
-- Ada:    doubly_linked_list.delete_last( l, [, n] );
-- Remove the list element from the end of the list. If n exists, remove n 
-- tems instead of one.
------------------------------------------------------------------------------

procedure ParseDoublyDeleteLast is
  listId   : identifier;
  theList  : resPtr;
  cntExpr   : storage;
  cntType   : identifier;
  hasCnt    : boolean := false;
  subprogramId : constant identifier := doubly_delete_last_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       if hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          -- Although delete_last has a count parameter, we cannot use it.
          -- Different elements may have different tags associated with the
          -- values and we have to test each element separately.
          for i in 1..cnt loop
              -- Note: check-before-use.  This will be an issue if concurrency is
              -- introduced.
              if metaLabelOK( Doubly_Linked_Storage_Lists.Last_Element( theList.dlslList ) ) then
                 Doubly_Linked_Storage_Lists.Delete_Last( theList.dlslList );
              end if;
          end loop;
       else
          -- Note: check-before-use.  This will be an issue if concurrency is
          -- introduced.
          if metaLabelOK( Doubly_Linked_Storage_Lists.Last_Element( theList.dlslList ) ) then
             Doubly_Linked_Storage_Lists.Delete_Last( theList.dlslList );
          end if;
       end if;
     exception when constraint_error =>
       err( +"no more elements" );
     end;
  end if;
end ParseDoublyDeleteLast;


procedure ParseDoublyFirst is
  -- Syntax: doubly_linked_list.first( l, c );
  -- Ada:    c := doubly_linked_list.first( l );
  -- Move the cursor to the first element in the list.
  -- doubly_linked_lists.has_element will be false if the list is empty.
  listId    : identifier;
  theList   : resPtr;
  cursId    : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := doubly_first_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  ParseLastCursorParameter( subprogramId, cursId );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       theCursor.dlslCursor := Doubly_Linked_Storage_Lists.First( theList.dlslList );
     end;
  end if;
end ParseDoublyFirst;

procedure ParseDoublyLast is
  -- Syntax: doubly_linked_list.delete_last( l, c );
  -- Ada:    c := doubly_linked_list.last( l );
  -- Move the cursor to the last element in the list.
  -- doubly_linked_lists.has_element will be false if the list is empty.
  listId    : identifier;
  theList   : resPtr;
  cursId    : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := doubly_last_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  ParseLastCursorParameter( subprogramId, cursId );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       theCursor.dlslCursor := Doubly_Linked_Storage_Lists.Last( theList.dlslList );
     end;
  end if;
end ParseDoublyLast;

procedure ParseDoublyNext is
  -- Syntax: doubly_linked_list.next( c );
  -- Ada:    doubly_linked_list.next( c );
  -- Move the cursor to the next element in the list.
  -- doubly_linked_lists.has_element will be false if the end of the list is
  -- reached. Moving when there is no element will not move the cursor.
  --cursExpr  : unbounded_string;
  --cursType  : identifier;
  cursId    : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := doubly_next_t;
begin
  expect( subprogramId );
  ParseSingleCursorParameter( subprogramId, cursId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       Doubly_Linked_Storage_Lists.Next( theCursor.dlslCursor );
     end;
  end if;
end ParseDoublyNext;

procedure ParseDoublyPrevious is
  -- Syntax: doubly_linked_list.previous( c );
  -- Ada:    doubly_linked_list.previous( c );
  -- Move the cursor to the previous element in the list.
  -- doubly_linked_lists.has_element will be false if the start of the list is
  -- reached. Moving when there is no element will not move the cursor.
  --cursExpr  : unbounded_string;
  --cursType  : identifier;
  cursId    : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := doubly_previous_t;
begin
  expect( subprogramId );
  ParseSingleCursorParameter( subprogramId, cursId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       Doubly_Linked_Storage_Lists.Previous( theCursor.dlslCursor );
     end;
  end if;
end ParseDoublyPrevious;

procedure ParseDoublyElement( result : out storage; kind : out identifier ) is
  -- Syntax: s := doubly_linked_list.element( c );
  -- Ada:    s := doubly_linked_list.element( c );
  -- Return the list element at the cursor.
  cursId    : identifier;
  theCursor : resPtr;
  oldElem   : storage;
  subprogramId : constant identifier := doubly_element_t;
begin
  expect( subprogramId );
  ParseSingleCursorParameter( subprogramId, cursId );
  -- if an error occurred, the cursId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     kind := identifiers( cursId ).genKind;
  else
     kind := eof_t;
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       oldElem := Doubly_Linked_Storage_Lists.Element( theCursor.dlslCursor );
       if metaLabelOk( oldElem ) then
          result := oldElem;
       end if;
     exception when constraint_error =>
       err( +"position cursor has no element" );
     end;
  end if;
end ParseDoublyElement;

procedure ParseDoublyReplaceElement is
  -- Syntax: doubly_linked_list.replace_element( l, c, e );
  -- Ada:    doubly_linked_list.replace_element( l ,c, e );
  -- Replace the list element at the cursor position with c.
  --listExpr  : unbounded_string;
  --listType  : identifier;
  listId    : identifier;
  theList   : resPtr;
  cursId    : identifier;
  theCursor : resPtr;
  itemExpr  : storage;
  itemType  : identifier;
  subprogramId : constant identifier := doubly_replace_element_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  ParseNextCursorParameter( subprogramId, cursId );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  end if;
  if not error_found then
     ParseLastGenItemParameter( subprogramId, itemExpr, itemType, identifiers( listId ).genKind );
  end if;
  if isExecutingCommand then
     -- Replace is delete then insert.  The meta labels of the existing element
     -- must be checked before replacing it with the new element.
     -- Note: check-before-use.  This will be an issue if concurrency is
     -- introduced.
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       if metaLabelOK( Doubly_Linked_Storage_Lists.Element( theCursor.dlslCursor ), itemExpr ) then
          Doubly_Linked_Storage_Lists.Replace_Element( theList.dlslList, theCursor.dlslCursor, itemExpr );
       end if;
     exception when program_error =>
       err( +"the cursor refers to a different list" );
     when storage_error =>
       err( +"storage_error raised" );
     end;
  end if;
end ParseDoublyReplaceElement;


------------------------------------------------------------------------------
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
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  INSERT BEFORE
--
-- Syntax: doubly_linked_list.insert_before( l, c, e [, n] )
-- Ada:    doubly_linked_list.insert( l ,c, e [, n]);
-- Insert element e before the cursor c. If no element is given, a node is
-- inserted with an unknown element. If the cursor does not point at an
-- element, the element is appended. If n is given, insert n copies instead
-- of one.
-- In Ada, parameters e, n or both can be missing but, without named
-- parameters, SparForte cannot distinguish whether a third parameter belongs
-- to e or n.
-- This is the basic form of insert that doesn't return another cursor.
-- Note: "insert" is a reserved SQL word in SparForte
------------------------------------------------------------------------------

procedure ParseDoublyInsertBefore is
  listId     : identifier;
  theList    : resPtr;
  cursId     : identifier;
  theCursor  : resPtr;
  itemExpr   : storage;
  itemType   : identifier;
  cntExpr    : storage;
  cntType    : identifier;
  hasCnt     : boolean := false;
  subprogramId : constant identifier := doubly_insert_before_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  ParseNextCursorParameter( subprogramId, cursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  ParseNextGenItemParameter( subprogramId, itemExpr, itemType, identifiers( listId ).genKind );
  genTypesOk( identifiers( listId ).genKind, itemType );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cntExpr, cntType );
     hasCnt := true;
  elsif token = symbol_t and identifiers( token ).store.value = ")" then
     expect( symbol_t, ")" );
  else
     err( +", or ) expected" );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       if metaLabelOk( itemExpr ) then
          if hasCnt then
             cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
             Doubly_Linked_Storage_Lists.Insert( theList.dlslList, theCursor.dlslCursor, itemExpr, cnt );
          else
             Doubly_Linked_Storage_Lists.Insert( theList.dlslList, theCursor.dlslCursor, itemExpr );
          end if;
       end if;
     exception when program_error =>
       err( +"the cursor refers to a different list" );
     when storage_error =>
       err( +"storage_error raised" );
     end;
  end if;
end ParseDoublyInsertBefore;


------------------------------------------------------------------------------
--  INSERT BEFORE AND MARK
--
-- Syntax: doubly_linked_list.insert_before_and_mark( l, c, c2 [, n] );
--         doubly_linked_list.insert_before_and_mark( l, c, s, c2 [, n] );
-- Ada:    doubly_linked_list.insert( l ,c, s [, n]);
--         doubly_linked_list.insert( l ,c, c2 [, n]);
--         doubly_linked_list.insert( l ,c, s, c2 [, n]);
-- Note: inserts before the cursor position
--       "insert" is a reserved word in SparForte
------------------------------------------------------------------------------

procedure ParseDoublyInsertBeforeAndMark is
  listId     : identifier;
  theList    : resPtr;
  cursId     : identifier;
  theCursor  : resPtr;
  theSecondCursor : resPtr;
  itemExpr   : storage;
  itemType   : identifier;
  cntExpr    : storage;
  cntType    : identifier;
  hasItem    : boolean := false;
  hasCnt     : boolean := false;
  resId      : resHandleId;
  ref        : reference;
  b          : boolean;
  hasOutCursor : boolean := false;
  subprogramId : constant identifier := doubly_insert_before_and_mark_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  ParseNextCursorParameter( subprogramId, cursId );
  genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  expectParameterComma;
  -- The third parameter can be a cursor or a new item.  Since a cursor
  -- is a single variable, it will either be the token or else new_t
  -- for an undeclared variable.  This assumes the undeclared variable
  -- was intended as a cursor, not intended as a data item.
  --
  -- We need to confirm that it is a variable before checking if it is a
  -- a cursor type.
  if identifiers( token ).kind = new_t then
     hasOutCursor := true;
  elsif identifiers( token ).class = varClass then
     if getUniType( identifiers( token ).kind ) = doubly_cursor_t then
        hasOutCursor := true;
     end if;
  end if;

  if hasOutCursor then
     ParseOutParameter( ref, doubly_cursor_t );
     baseTypesOK( ref.kind, doubly_cursor_t );
     identifiers( ref.id ).genKind := identifiers( listId ).genKind;
     -- A cursor may be followed by an optional count
     if token = symbol_t and identifiers( token ).store.value = "," then
        ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
        hasCnt := true;
     elsif token = symbol_t and identifiers( token ).store.value = ")" then
        expect( symbol_t, ")" );
     else
        err( +", or ) expected" );
     end if;
  else
     -- If it's a new item value, check the generic item type and
     -- then get the cursor
     ParseGenItemParameter( itemExpr, itemType, identifiers( listId ).genKind );
     -- Instead of genTypeOK, we'll show a custom message explaining
     -- the two different variations for the third parameter.
     b := insertTypesOk( itemType, identifiers( listId ).genKind );
     hasItem := true;
     ParseNextOutParameter( subprogramId, ref, doubly_cursor_t );
     baseTypesOK( ref.kind, doubly_cursor_t );
     identifiers( ref.id ).genKind := identifiers( listId ).genKind;
     if token = symbol_t and identifiers( token ).store.value = "," then
        ParseLastNumericParameter( subprogramId, cntExpr, cntType );
        hasCnt := true;
     elsif token = symbol_t and identifiers( token ).store.value = ")" then
        expect( symbol_t, ")" );
     else
        err( +", or ) expected" );
     end if;
  end if;

  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       -- the second cursor is the out parameter.  Declare it.  Then fetch it.
       identifiers( ref.id ).resource := true;
       declareResource( resId, doubly_linked_storage_list_cursor, getIdentifierBlock( ref.id ) );
       AssignParameter( ref, storage'( to_unbounded_string( resId ), noMetaLabel ) );
       findResource( resId, theSecondCursor );

       -- there are four variations
       if hasItem and hasCnt then
          if metaLabelOk( itemExpr ) then
             cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
             Doubly_Linked_Storage_Lists.Insert(
                theList.dlslList,
                theCursor.dlslCursor,
                itemExpr,
                theSecondCursor.dlslCursor,
                cnt
             );
          end if;
       elsif hasItem then
          if metaLabelOk( itemExpr ) then
             Doubly_Linked_Storage_Lists.Insert(
                theList.dlslList,
                theCursor.dlslCursor,
                itemExpr,
                theSecondCursor.dlslCursor
             );
          end if;
       elsif hasCnt then
          cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
          -- the meta data labels require nullStorage to be insert, not just
          -- random data
          Doubly_Linked_Storage_Lists.Insert(
             theList.dlslList,
             theCursor.dlslCursor,
             nullStorage,
             theSecondCursor.dlslCursor,
             cnt
          );
       else
          -- the meta data labels require nullStorage to be insert, not just
          -- random data
          Doubly_Linked_Storage_Lists.Insert(
             theList.dlslList,
             theCursor.dlslCursor,
             nullStorage,
             theSecondCursor.dlslCursor
          );
       end if;

     exception when program_error =>
       err( +"the cursor refers to a different list" );
     when storage_error =>
       err( +"storage_error raised" );
     end;
  end if;
end ParseDoublyInsertBeforeAndMark;


------------------------------------------------------------------------------
--  DELETE
--
-- Syntax: doubly_linked_list.delete( l, c,[, n] );
-- Ada:    doubly_linked_list.delete( l ,c [, n] );
-- Note: delete is an SQL keyword
------------------------------------------------------------------------------

procedure ParseDoublyDelete is
  listId   : identifier;
  theList   : resPtr;
  cursId    : identifier;
  theCursor : resPtr;
  cntExpr   : storage;
  cntType   : identifier;
  hasCnt    : boolean := false;
  subprogramId : constant identifier := doubly_delete_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  ParseNextCursorParameter( subprogramId, cursId );
  -- if an error occurred, the cursId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  end if;
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cntExpr, cntType, containers_count_type_t );
     hasCnt := true;
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       cnt : Ada.Containers.Count_Type;
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       if hasCnt then
         begin
           cnt := Ada.Containers.Count_Type( to_numeric( cntExpr.value ) );
         exception when constraint_error =>
           err( +"constraint error raised" );
         end;
         -- Note: check-before-use.  This will be an issue if concurrency is
         -- introduced.
         if metaLabelOK( Doubly_Linked_Storage_Lists.Element( theCursor.dlslCursor ) ) then
            Doubly_Linked_Storage_Lists.Delete( theList.dlslList, theCursor.dlslCursor, cnt );
         end if;
       else
         -- Note: check-before-use.  This will be an issue if concurrency is
         -- introduced.
         if metaLabelOK( Doubly_Linked_Storage_Lists.Element( theCursor.dlslCursor ) ) then
            Doubly_Linked_Storage_Lists.Delete( theList.dlslList, theCursor.dlslCursor );
         end if;
       end if;
     exception when constraint_error =>
       err( +"position cursor has no element" );
     end;
  end if;
end ParseDoublyDelete;


------------------------------------------------------------------------------
--  CONTAINS
--
-- Syntax: b := doubly_linked_list.contains( l, s );
-- Ada:    b := doubly_linked_list.contains( l, s );
-- Return true if the list contains list element e.
-- Does not take into account the data meta label.  Result meta labels not
-- affected by the data found.
------------------------------------------------------------------------------

procedure ParseDoublyContains( result : out storage; kind : out identifier ) is
  listId   : identifier;
  theList  : resPtr;
  itemExpr : storage;
  itemType : identifier;
  subprogramId : constant identifier := doubly_contains_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     ParseLastGenItemParameter( subprogramId, itemExpr, itemType, identifiers( listId ).genKind );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       result := storage'( to_spar_boolean( Doubly_Linked_Storage_Lists.Contains( theList.dlslList, itemExpr ) ), noMetaLabel);
     end;
  end if;
end ParseDoublyContains;


------------------------------------------------------------------------------
--  FIND
--
-- Syntax: doubly_linked_list.find( l, s, c );
-- Ada:    c := doubly_linked_list.find( l, s );
-- Move the cursor to the location of e in the list Start from the beginning.
-- doubly_linked_lists.has_element will be false if the element is not found.
-- Does not take into account the data meta label.
------------------------------------------------------------------------------

procedure ParseDoublyFind is
  listId    : identifier;
  theList   : resPtr;
  itemExpr  : storage;
  itemType  : identifier;
  cursId    : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := doubly_find_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     ParseNextGenItemParameter( subprogramId, itemExpr, itemType, identifiers( listId ).genKind );
  end if;
  ParseLastCursorParameter( subprogramId, cursId );
  -- if an error occurred, the cursId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       theCursor.dlslCursor := Doubly_Linked_Storage_Lists.Find( theList.dlslList, itemExpr );
     end;
  end if;
end ParseDoublyFind;


------------------------------------------------------------------------------
--  REVERSE FIND
--
-- Syntax: doubly_linked_list.reverse_find( l, s, c );
-- Ada:    c := doubly_linked_list.reverse_find( l, s );
-- Move the cursor to the location of e in the list. Start from the end of
-- the list. doubly_linked_lists.has_element will be false if the element is
-- not found.
-- Does not take into account the data meta label
------------------------------------------------------------------------------

procedure ParseDoublyReverseFind is
  listId    : identifier;
  theList   : resPtr;
  itemExpr  : storage;
  itemType  : identifier;
  cursId    : identifier;
  theCursor : resPtr;
  subprogramId : constant identifier := doubly_reverse_find_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     ParseNextGenItemParameter( subprogramId, itemExpr, itemType, identifiers( listId ).genKind );
  end if;
  ParseLastCursorParameter( subprogramId, cursId );
  -- if an error occurred, the cursId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( listId ).genKind, identifiers( cursId ).genKind );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       theCursor.dlslCursor := Doubly_Linked_Storage_Lists.Reverse_Find( theList.dlslList, itemExpr );
     end;
  end if;
end ParseDoublyReverseFind;


------------------------------------------------------------------------------
--  REVERSE ELEMENTS
--
-- Syntax: doubly_linked_list.reverse_elements( l );
-- Ada:    doubly_linked_list.reverse_elements( l );
-- Reverse the order of the list elements.
-- Does not take into account data meta label
------------------------------------------------------------------------------

procedure ParseDoublyReverseElements is
  listId   : identifier;
  theList  : resPtr;
  subprogramId : constant identifier := doubly_reverse_elements_t;
begin
  expect( subprogramId );
  ParseSingleListParameter( subprogramId, listId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       Doubly_Linked_Storage_Lists.Reverse_Elements( theList.dlslList );
     end;
  end if;
end ParseDoublyReverseElements;


------------------------------------------------------------------------------
--  FLIP
--
-- Syntax: doubly_linked_list.flip( l );
-- Ada:    N/A (alias for reverse_elements)
-- Reverse the order of the list elements.
-- Does not take into account data meta label
------------------------------------------------------------------------------

procedure ParseDoublyFlip is
  listId   : identifier;
  theList  : resPtr;
  subprogramId : constant identifier := doubly_flip_t;
begin
  expect( subprogramId );
  ParseSingleListParameter( subprogramId, listId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       Doubly_Linked_Storage_Lists.Reverse_Elements( theList.dlslList );
     end;
  end if;
end ParseDoublyFlip;


------------------------------------------------------------------------------
--  ASSIGN
--
-- Syntax: doubly_linked_list.assign( l1, l2 );
-- Ada:    doubly_linked_list.assign( l1, l2 );
-- Overwrite the contents of list l1 with a copy of l2.
-- Does not take into account data meta label
------------------------------------------------------------------------------

procedure ParseDoublyAssign is
  sourceListId  : identifier;
  theSourceList : resPtr;
  targetListId  : identifier;
  theTargetList : resPtr;
  subprogramId : constant identifier := doubly_assign_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, targetListId, doubly_list_t );
  ParseLastListParameter( subprogramId, sourceListId );
  if not error_found then
     genTypesOk( identifiers( targetListId ).genKind, identifiers( sourceListId ).genKind );
  end if;
  if isExecutingCommand then
     declare
       sourceCursor : Doubly_Linked_Storage_Lists.Cursor;
     begin
       findResource( to_resource_id( identifiers( targetListId ).store.value ), theTargetList );
       findResource( to_resource_id( identifiers( sourceListId ).store.value ), theSourceList );
       -- this is only available starting in GCC Ada 4.7 or 4.8 or newer
       --Doubly_Linked_Storage_Lists.Assign( theTargetList.dlslList, theSourceList.dlslList );
       -- we'll write our own
       sourceCursor := Doubly_Linked_Storage_Lists.First( theSourceList.dlslList );
       Doubly_Linked_Storage_Lists.Clear( theTargetList.dlslList );
       for i in 1..Doubly_Linked_Storage_Lists.Length( theSourceList.dlslList ) loop
          Doubly_Linked_Storage_Lists.Append( theTargetList.dlslList,
             Doubly_Linked_Storage_Lists.Element( sourceCursor ) );
          Doubly_Linked_Storage_Lists.Next( sourceCursor );
       end loop;
     end;
  end if;
end ParseDoublyAssign;


------------------------------------------------------------------------------
--  MOVE
--
-- Syntax: doubly_linked_list.move( l1, l2 );
-- Ada:    doubly_linked_list.move( l1, l2 );
-- Overwrite the contents of list l1 with a copy of l2 and erase l2.
-- Does not take into account data meta label
------------------------------------------------------------------------------

procedure ParseDoublyMove is
  sourceListId   : identifier;
  theSourceList  : resPtr;
  targetListId   : identifier;
  theTargetList  : resPtr;
  subprogramId : constant identifier := doubly_move_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, targetListId, doubly_list_t );
  ParseLastListParameter( subprogramId, sourceListId );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( targetListId ).genKind, identifiers( sourceListId ).genKind );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetListId ).store.value ), theTargetList );
       findResource( to_resource_id( identifiers( sourceListId ).store.value ), theSourceList );
       Doubly_Linked_Storage_Lists.Move( theTargetList.dlslList, theSourceList.dlslList );
     end;
  end if;
end ParseDoublyMove;


------------------------------------------------------------------------------
--  SWAP
--
-- Syntax: doubly_linked_list.swap( l, c1, c2 );
-- Ada:    doubly_linked_list.swap( l, c1, c2 );
-- Swaps values stored at c1, c2.
------------------------------------------------------------------------------

procedure ParseDoublySwap is
  listId         : identifier;
  theList        : resPtr;
  firstCursId    : identifier;
  theFirstCursor : resPtr;
  secondCursId   : identifier;
  theSecondCursor: resPtr;
  subprogramId : constant identifier := doubly_swap_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  ParseNextCursorParameter( subprogramId, firstCursId );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( listId ).genKind, identifiers( firstCursId ).genKind );
  end if;
  ParseLastCursorParameter( subprogramId, secondCursId );
  -- if an error occurred, the listId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( listId ).genKind, identifiers( secondCursId ).genKind );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       findResource( to_resource_id( identifiers( firstCursId ).store.value ), theFirstCursor );
       findResource( to_resource_id( identifiers( secondCursId ).store.value ), theSecondCursor );
       if metaLabelOK( Doubly_Linked_Storage_Lists.Element( theFirstCursor.dlslCursor ),
                       Doubly_Linked_Storage_Lists.Element( theSecondCursor.dlslCursor ) ) then
          Doubly_Linked_Storage_Lists.Swap( theList.dlslList,
             theFirstCursor.dlslCursor, theSecondCursor.dlslCursor );
       end if;
     exception when constraint_error =>
       err( +"a cursor has no element" );
     when program_error =>
       err( +"a cursor refers to a different list" );
     when storage_error =>
       err( +"storage_error: an invalid access occurred" );
     end;
  end if;
end ParseDoublySwap;


------------------------------------------------------------------------------
--  SWAP LINKS
--
-- Syntax: doubly_linked_list.swap_links( l, c1, c2 );
-- Ada:    doubly_linked_list.swap_links( l, c1, c2 );
-- Swaps the list nodes stored at c1, c2.  Cursors will follow the node as it
-- moves as it's just a pointer to the node.
------------------------------------------------------------------------------

procedure ParseDoublySwapLinks is
  listId         : identifier;
  theList        : resPtr;
  firstCursId    : identifier;
  theFirstCursor : resPtr;
  secondCursId   : identifier;
  theSecondCursor: resPtr;
  subprogramId : constant identifier := doubly_swap_links_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  ParseNextCursorParameter( subprogramId, firstCursId );
  -- if an error occurred, the cursId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( listId ).genKind, identifiers( firstCursId ).genKind );
  end if;
  ParseLastCursorParameter( subprogramId, secondCursId );
  -- if an error occurred, the cursId may be invalid and won't have a genKind
  -- defined
  if not error_found then
     genTypesOk( identifiers( listId ).genKind, identifiers( secondCursId ).genKind );
  end if;
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       findResource( to_resource_id( identifiers( firstCursId ).store.value ), theFirstCursor );
       findResource( to_resource_id( identifiers( secondCursId ).store.value ), theSecondCursor );
       if metaLabelOK( Doubly_Linked_Storage_Lists.Element( theFirstCursor.dlslCursor ),
                       Doubly_Linked_Storage_Lists.Element( theSecondCursor.dlslCursor ) ) then
          Doubly_Linked_Storage_Lists.Swap_Links( theList.dlslList,
             theFirstCursor.dlslCursor, theSecondCursor.dlslCursor );
       end if;
     exception when constraint_error =>
       err( +"a cursor has no element" );
     when program_error =>
       err( +"a cursor refers to a different list" );
     when storage_error =>
       err( +"storage_error: an invalid access occurred" );
     end;
  end if;
end ParseDoublySwapLinks;


------------------------------------------------------------------------------
--  SPLICE
--
-- Syntax: doubly_linked_list.splice( l1, c, l2 [,c2] ) | ( l1, c, c2 );
-- Ada:    doubly_linked_list.splice( l1, c, l2 [,c2] ) | ( l1, c, c2 );
-- Append one list to another, or a node from one list to another, or
-- a node within one list.
------------------------------------------------------------------------------

procedure ParseDoublySplice is
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
  subprogramId : constant identifier := doubly_splice_t;
begin
  expect( subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, targetListId, doubly_list_t );
  ParseNextCursorParameter( subprogramId, cursId );
  genTypesOk( identifiers( targetListId ).genKind, identifiers( cursId ).genKind );
  -- There's no easy way to handle this.  Two optional parameters, one is
  -- in out and one isn't.
  expectParameterComma( subprogramId );
  ParseIdentifier( tempId );
  if getUniType( identifiers( tempId ).kind ) = doubly_list_t then
     sourceListId := tempId;
     hasSourceId := true;
     genTypesOk( identifiers( targetListId ).genKind, identifiers( sourceListId ).genKind );
     if token = symbol_t and identifiers( token ).store.value = "," then
        ParseLastCursorParameter( subprogramId, curs2Id );
        hasCurs2 := true;
        genTypesOk( identifiers( targetListId ).genKind, identifiers( curs2Id ).genKind );
     else
        expect( symbol_t, ")" );
     end if;
  elsif getUniType( identifiers( tempId ).kind ) = doubly_cursor_t then
     -- technically this is an in parameter so could be an expression.  But
     -- it never will be.
     curs2Id := tempId;
     hasCurs2 := true;
     genTypesOk( identifiers( targetListId ).genKind, identifiers( curs2Id ).genKind );
     expect( symbol_t, ")" );
  else
     err( +"list or cursor expected" );
  end if;

  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( targetListId ).store.value ), theTargetList );
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       if hasSourceId then
          findResource( to_resource_id( identifiers( sourceListId ).store.value ), theSourceList );
       end if;
       if hasCurs2 then
          findResource( to_resource_id( identifiers( curs2Id ).store.value ), theSecondCursor );
       end if;

       -- There are 3 variations to this function
       --
       -- In all cases, check the element at the source cursor for the data
       -- meta label.

       -- doubly_linked_list.splice( l1, c, l2 );

       if not hasCurs2 and hasSourceId then
          begin
             if metaLabelOK( Doubly_Linked_Storage_Lists.Element( theCursor.dlslCursor ) ) then
                Doubly_Linked_Storage_Lists.Splice(
                   theTargetList.dlslList,
                   theCursor.dlslCursor,
                   theSourceList.dlslList );
             end if;
          exception when constraint_error =>
             err( +"a cursor has no element" );
          when program_error =>
             err( +"a cursor refers to a different list" );
          end;

       -- doubly_linked_list.splice( l1, c, l2, c2 );
       elsif hasCurs2 and hasSourceId then
          begin
             if metaLabelOK( Doubly_Linked_Storage_Lists.Element( theCursor.dlslCursor ) ) then
                Doubly_Linked_Storage_Lists.Splice(
                    theTargetList.dlslList,
                    theCursor.dlslCursor,
                    theSourceList.dlslList,
                    theSecondCursor.dlslCursor );
             end if;
          exception when constraint_error =>
             err( +"a cursor has no element" );
          when program_error =>
             err( +"a cursor refers to a different list" );
          end;

       -- doubly_linked_list.splice( l1, c, c2 );
       elsif hasCurs2 and not hasSourceId then
          begin
             if metaLabelOK( Doubly_Linked_Storage_Lists.Element( theCursor.dlslCursor ) ) then
                Doubly_Linked_Storage_Lists.Splice(
                    theTargetList.dlslList,
                    theCursor.dlslCursor,
                    theSecondCursor.dlslCursor );
             end if;
          exception when constraint_error =>
             err( +"a cursor has no element" );
          when program_error =>
             err( +"a cursor refers to a different list" );
          end;

       else
          err( +"internal error: unexpected splice variation" );
       end if;

     end;
  end if;
end ParseDoublySplice;


------------------------------------------------------------------------------
--  HAS ELEMENT
--
-- Syntax: b := doubly_linked_list.has_element( l );
-- Ada:    b := doubly_linked_list.has_element( l );
-- True if the cursor is positioned at a list element.
-- Data meta labels are ignored
------------------------------------------------------------------------------

procedure ParseDoublyHasElement( result : out storage; kind : out identifier ) is
  cursId : identifier;
  theCursor : resPtr;
  use Doubly_Linked_Storage_Lists; -- needed for =
  subprogramId : constant identifier := doubly_has_element_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleCursorParameter( subprogramId, cursId );
  if isExecutingCommand then
     begin
       findResource( to_resource_id( identifiers( cursId ).store.value ), theCursor );
       result := storage'( to_spar_boolean( theCursor.dlslCursor /= Doubly_Linked_Storage_Lists.No_Element ), noMetaLabel );
     end;
  end if;
end ParseDoublyHasElement;


-----------------------------------------------------------------------------
--  ASSEMBLE
--
-- Syntax: s := doubly_linked_lists.assemble( l [,d [,f]] );
-- Ada:    N/A
-- Apply strings.image to each element in the list. Concatenate the strings
-- together to form a new string, separated by delimiter string d and ending
-- with optional string f. An list will result in an empty string (but f will
-- be appended even if the string is empty).
-- This should run faster than if the user did it themselves in a script.
-- As a shell language, this is useful for passing data to a pipeline
-- echo( doubly_linked_lists.assemble( l ) ) | wc
-- TODO: shouldn't this stream directly to a pipeline so that it can
-- be avoid storing the data twice in memory (using an intermediate
-- variable) ?
------------------------------------------------------------------------------

procedure ParseDoublyAssemble( result : out storage; kind : out identifier ) is
  listId    : identifier;
  theList   : resPtr;
  delimExpr : storage;
  delimType : identifier;
  hasDelim  : boolean := false;
  finalExpr : storage;
  finalType : identifier;
  hasFinal  : boolean := false;
  curs      : Doubly_Linked_Storage_Lists.Cursor;
  subprogramId : constant identifier := doubly_assemble_t;
begin
  kind := uni_string_t;
  expectAdaScript( subject => subprogramId );
  ParseFirstInOutInstantiatedParameter( subprogramId, listId, doubly_list_t );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseNextStringParameter( subprogramId, delimExpr, delimType, uni_string_t );
     hasDelim := true;
     if token = symbol_t and identifiers( token ).store.value = "," then
        ParseLastStringParameter( subprogramId, finalExpr, finalType, uni_string_t );
        hasFinal := true;
     else
        expect( symbol_t, ")" );
     end if;
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
        elementStore : storage;
        firstElement : boolean := true;
     begin
       if not hasDelim then
          -- TODO: the eol delimiter should be based on the operating system
          delimExpr := storage'( to_unbounded_string( "" & ASCII.LF ), noMetaLabel );
       end if;
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       curs := Doubly_Linked_Storage_Lists.First( theList.dlslList );
       result := nullStorage;
       if Doubly_Linked_Storage_Lists.Has_Element( curs ) then
          loop
             elementStore := Doubly_Linked_Storage_Lists.Element( curs );
             -- the data meta labels of the result is the first element's
             -- data meta labels.
             if firstElement then
                result.metaLabel := elementStore.metaLabel;
                firstElement := false;
             end if;
             if metaLabelOk( result, elementStore ) then
                -- TODO: DATA META LABEL: when combining list items into a string,
                -- the items could have different security levels but the
                -- string has only one.  Should resolve differences, if any
                result.value := result.value & elementStore.value;
             end if;
             Doubly_Linked_Storage_Lists.Next( curs );
             exit when not Doubly_Linked_Storage_Lists.Has_Element( curs );
             result.value := result.value & delimExpr.value;
          end loop;
       end if;
       if hasFinal then
          result.value := result.value & finalExpr.value;
       end if;
     exception when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDoublyAssemble;


-----------------------------------------------------------------------------
--  DISASSEMBLE
--
-- Syntax: doubly_linked_lists.dissemble( s, l [,d [,f]] );
-- Ada:    N/A
-- The inverse of assemble.  Take a string and convert it to a list of
-- strings.  Strip off the final suffix, if it exists.  Use d as the
-- element delimiter (will not be included in the list element.  The
-- default of d is ASCII.LF.
-- This should run faster than if the user did it themselves in a script.
-- As a shell language, this is useful for processing data received from
-- a shell command.
-- All list items receive the meta data labels of the string.  The script
-- must be able to work with the meta data labels of the string.
-----------------------------------------------------------------------------

procedure ParseDoublyDisassemble is
  strExpr   : storage;
  strType   : identifier;
  listId    : identifier;
  delimExpr : storage;
  delimType : identifier;
  hasDelim  : boolean := false;
  finalExpr : storage;
  finalType : identifier;
  hasFinal  : boolean := false;
  subprogramId : constant identifier := doubly_disassemble_t;
begin
  expectAdaScript( subject => subprogramId );
  ParseFirstStringParameter( subprogramId, strExpr, strType, uni_string_t );
  ParseNextListParameter( subprogramId, listId );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseNextStringParameter( subprogramId, delimExpr, delimType, uni_string_t );
     hasDelim := true;
     if token = symbol_t and identifiers( token ).store.value = "," then
        ParseLastStringParameter( subprogramId, finalExpr, finalType, uni_string_t );
        hasFinal := true;
     else
        expect( symbol_t, ")" );
     end if;
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       ch : character;
       i  : natural := 1;
       l  : natural := length( strExpr.value );
       tempStore : storage;
       theList : resPtr;
       delimPos : natural;
       delimLen : positive;
     begin
       -- default delimiter if none supplied
       if not hasDelim then
          -- TODO: the eol delimiter should be based on the operating system
          delimExpr := storage'( null_unbounded_string & ASCII.LF, noMetaLabel );
       end if;
       -- remove final part, if it exists and is defined
       if hasFinal then
          if l > length( finalExpr.value ) then
             if unbounded_slice( strExpr.value, l - length( finalExpr.value ) + 1, l ) = finalExpr.value then
                delete( strExpr.value, l - length( finalExpr.value ) + 1, l );
             end if;
             l := length( strExpr.value ); -- new length
          end if;
       end if;
       delimLen := length( delimExpr.value );
       delimPos := 0;
       -- generate the list items
       findResource( to_resource_id( identifiers( listId ).store.value ), theList );
       tempStore.metaLabel := strExpr.metaLabel;
       if metaLabelOk( strExpr ) then
          while l >= i loop
             ch := element( strExpr.value, i );
             if ch = element( delimExpr.value, delimPos+1 ) then                           -- looking at delim?
                tempStore.value := tempStore.value & ch;                             -- add chr to running string
                delimPos := delimPos + 1;                                            -- advance counter
                if delimPos = delimLen then                                                -- found delimiter?
                   Delete( tempStore.value, length( tempStore.value ) - delimLen + 1, length( tempStore.value ) ); -- remove delimiter
                   Doubly_Linked_Storage_Lists.Append( theList.dlslList, tempStore );   -- put string in list
                   tempStore := nullStorage;                             -- reset the running string
                   tempStore.metaLabel := strExpr.metaLabel;
                   delimPos := 0;                                                    -- and the delim posn
                end if;
             else                                                                       -- not the delim?
                tempStore.value := tempStore.value & ch;                        -- add chr to running string
                delimPos := 0;                                                  -- reset delim posn
             end if;
             i := i + 1;
          end loop;
          -- anything left, just append.
          if length( tempStore.value ) > 0 then
             Doubly_Linked_Storage_Lists.Append( theList.dlslList, tempStore );
          end if;
       end if;  -- data meta label OK
     exception when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDoublyDisassemble;


-----------------------------------------------------------------------------
--  PARCEL
--
-- Syntax: doubly_linked_lists.parcel( s, w, l );
-- Ada:    N/A
-- Convert ("parcel out") the string to a list of strings. Each string is
-- length w except for the final string that has the remaining characters. An
-- empty string will result in an unchanged list. If the list is not empty,
-- the new strings will be appended. The list can be converted to a string
-- with doubly_linked_lists.assemble.
-----------------------------------------------------------------------------

procedure ParseDoublyParcel is
  strExpr   : storage;
  strType   : identifier;
  widthExpr : storage;
  widthType : identifier;
  listId    : identifier;

  strLen   : natural;
  firstPos : positive;
  lastPos  : natural;
  width    : natural;
  tempStore : storage;
  theList  : resPtr;
  elemType : identifier;
  subprogramId : constant identifier := doubly_parcel_t;
begin
  expectAdaScript( subject => subprogramId );
  ParseFirstStringParameter( subprogramId, strExpr, strType, uni_string_t );
  ParseNextNumericParameter( subprogramId, widthExpr, widthType, positive_t );
  ParseLastListParameter( subprogramId, listId );
  elemType := getUniType( identifiers( listId ).genKind );
  if elemType /= uni_string_t and elemType /= universal_t then
     err( +"list elements should be strings or typeless" );
  end if;
  if isExecutingCommand then
     begin
        -- generate the list items
        findResource( to_resource_id( identifiers( listId ).store.value ), theList );
        width := positive'value( " " & to_string( widthExpr.value ) );
        -- width should be positive by this point
        firstPos := 1;
        strLen := length( strExpr.value );
        tempStore.metaLabel := strExpr.metaLabel;
        if metaLabelOK( strExpr ) then
           while firstPos < strLen loop
              lastPos := firstPos + width - 1;
              if lastPos > strLen then
                 lastPos := strLen;
              end if;
              tempStore.value := unbounded_slice( strExpr.value, firstPos, lastPos );
              if length( tempStore.value ) > 0 then
                 Doubly_Linked_Storage_Lists.Append( theList.dlslList, tempStore );
              end if;
              firstPos := lastPos + 1;
           end loop;
        end if;
     exception when storage_error =>
       err( +"storage error raised" );
     end;
  end if;
end ParseDoublyParcel;


-----------------------------------------------------------------------------
--
-- HOUSEKEEPING
--
-----------------------------------------------------------------------------


procedure StartupDoubly is
begin
  declareNamespace( "doubly_linked_lists" );

  declareIdent( doubly_list_t,   "doubly_linked_lists.list", variable_t, genericTypeClass );
  identifiers( doubly_list_t ).usage := limitedUsage;
  identifiers( doubly_list_t ).resource := true;
  declareIdent( doubly_cursor_t, "doubly_linked_lists.cursor", variable_t, genericTypeClass );
  identifiers( doubly_cursor_t ).usage := limitedUsage;
  identifiers( doubly_cursor_t ).resource := true;

  --declareProcedure( doubly_new_list_t,  "doubly_linked_lists.new_list", ParseDoublyNewList'access );
  declareProcedure( doubly_clear_t,     "doubly_linked_lists.clear",    ParseDoublyClear'access );
  declareFunction(  doubly_is_empty_t,  "doubly_linked_lists.is_empty", ParseDoublyIsEmpty'access );
  declareFunction(  doubly_length_t,    "doubly_linked_lists.length",   ParseDoublyLength'access );
  declareProcedure( doubly_append_t,    "doubly_linked_lists.append",   ParseDoublyAppend'access );
  declareProcedure( doubly_prepend_t,   "doubly_linked_lists.prepend",  ParseDoublyPrepend'access );
  declareFunction(  doubly_first_element_t, "doubly_linked_lists.first_element", ParseDoublyFirstElement'access );
  declareFunction(  doubly_last_element_t,  "doubly_linked_lists.last_element", ParseDoublyLastElement'access );
  declareProcedure( doubly_delete_first_t,  "doubly_linked_lists.delete_first",  ParseDoublyDeleteFirst'access );
  declareProcedure( doubly_delete_last_t,   "doubly_linked_lists.delete_last",  ParseDoublyDeleteLast'access );

  --declareProcedure( doubly_new_cursor_t, "doubly_linked_lists.new_cursor", ParseDoublyNewCursor'access );
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

  declareFunction(  doubly_assemble_t, "doubly_linked_lists.assemble", ParseDoublyAssemble'access );
  declareProcedure( doubly_disassemble_t, "doubly_linked_lists.disassemble", ParseDoublyDisassemble'access );
  declareProcedure( doubly_parcel_t, "doubly_linked_lists.parcel", ParseDoublyParcel'access );

  declareNamespaceClosed( "doubly_linked_lists" );

  -- copy not implemented
  -- TODO: to array - we need to refactor arrays first, to json?
  -- TODO: iterate, reverse_iterate? possible...we don't have callbacks but use `...`?
  -- doubly_linked_lists.echo - read as a command - not easy because commands only take strings
  -- shuffle, bubble_sort, heap_sort -- skipped because doubly linked lists are not indexed
  -- TODO: genTypesOk refactor
  -- TODO: error messages do not appear on the token
  -- TODO: find/reverse find: should take the cursor position

end StartupDoubly;

procedure ShutdownDoubly is
begin
  null;
end ShutdownDoubly;

end parser_doubly;

