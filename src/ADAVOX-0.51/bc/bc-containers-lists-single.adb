-- Copyright (C) 1994-2001 Grady Booch, David Weller and Simon Wright.
-- All Rights Reserved.
--
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

-- $Id: bc-containers-lists-single.adb,v 1.2 2005/02/11 02:59:33 ken Exp $

with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Containers.Lists.Single is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Lists.Single");

  -- We can't take 'Access of non-aliased components. But if we alias
  -- discriminated objects they become constrained - even if the
  -- discriminant has a default.
  package Allow_Element_Access
  is new System.Address_To_Access_Conversions (Item);

  use type Nodes.Single_Node_Ref;

  function "=" (L, R : List) return Boolean is
  begin
    return L.Rep = R.Rep;
  end "=";

  procedure Clear (L : in out List) is
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Ptr : Nodes.Single_Node_Ref;
  begin
    while Curr /= null loop
      Ptr := Curr;
      Curr := Curr.Next;
      if Ptr.Count > 1 then
        Ptr.Count := Ptr.Count - 1;
        exit;
      else
        Nodes.Delete (Ptr);
      end if;
    end loop;
    L.Rep := null;
  end Clear;

  procedure Insert (L : in out List; Elem : Item) is
  begin
    L.Rep := Nodes.Create (Elem, Next => L.Rep);
  end Insert;

  procedure Insert (L : in out List; From_List : in out List) is
    Ptr : Nodes.Single_Node_Ref := From_List.Rep;
  begin
    if Ptr /= null then
      while Ptr.Next /= null loop
        Ptr := Ptr.Next;
      end loop;
    end if;
    Ptr.Next := L.Rep;
    L.Rep := From_List.Rep;
    L.Rep.Count := L.Rep.Count + 1;
  end Insert;

  procedure Insert (L : in out List; Elem : Item; Before : Positive) is
    Prev : Nodes.Single_Node_Ref;
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Index: Positive := 1;
  begin
    if Curr = null or else Before = 1 then
      Insert(L, Elem);
    else
      while Curr /= null and then Index < Before loop
        Prev := Curr;
        Curr := Curr.Next;
        Index := Index + 1;
      end loop;
      Assert (Curr /= null,
              BC.Range_Error'Identity,
              "Insert",
              BSE.Invalid_Index);
      Prev.Next := Nodes.Create (Elem, Next => Curr);
    end if;
  end Insert;

  procedure Insert (L : in out List;
                    From_List: in out List;
                    Before : Positive) is
    Prev : Nodes.Single_Node_Ref;
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Ptr  : Nodes.Single_Node_Ref := From_List.Rep;
    Index: Positive := 1;
  begin
    if Ptr /= null then
      if Curr = null or else Before = 1 then
        Insert(L, From_List);
      else
        while Curr /= null and then Index < Before loop
          Prev := Curr;
          Curr := Curr.Next;
          Index := Index + 1;
        end loop;
        Assert (Curr /= null,
                BC.Range_Error'Identity,
                "Insert",
                BSE.Invalid_Index);
        while Ptr.Next /= null loop
          Ptr := Ptr.Next;
        end loop;
        Ptr.Next := Curr;
        Prev.Next := From_List.Rep;
        From_List.Rep.Count := From_List.Rep.Count + 1;
      end if;
    end if;
  end Insert;

  procedure Append (L : in out List; Elem : Item) is
    Curr : Nodes.Single_Node_Ref := L.Rep;
  begin
    if Curr /= null then
      while Curr.Next /= null loop
        Curr := Curr.Next;
      end loop;
      Curr.Next := Nodes.Create (Elem, Next => null);
    else
      L.Rep := Nodes.Create (Elem, Next => null);
    end if;
  end Append;

  procedure Append (L : in out List; From_List : in out List) is
    Curr : Nodes.Single_Node_Ref := L.Rep;
  begin
    if From_List.Rep /= null then
      if Curr /= null then
        while Curr.Next /= null loop
          Curr := Curr.Next;
        end loop;
      end if;
      if Curr /= null then
        Curr.Next := From_List.Rep;
      else
        L.Rep := From_List.Rep;
      end if;
      From_List.Rep.Count := From_List.Rep.Count + 1;
    end if;
  end Append;

  procedure Append (L : in out List; Elem : Item; After : Positive) is
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Index: Positive := 1;
  begin
    if Curr = null then
      Append(L, Elem);
    else
      while Curr /= null and then Index < After loop
        Curr := Curr.Next;
        Index := Index + 1;
      end loop;
      Assert (Curr /= null,
              BC.Range_Error'Identity,
              "Append",
              BSE.Invalid_Index);
      Curr.Next := Nodes.Create (Elem, Next => Curr.Next);
    end if;
  end Append;

  procedure Append (L : in out List;
                    From_List : in out List;
                    After : Positive) is
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Ptr  : Nodes.Single_Node_Ref := From_List.Rep;
    Index: Positive := 1;
  begin
    if Ptr /= null then
      if Curr = null then
        Append(L, From_List);
      else
        while Curr /= null and then Index < After loop
          Curr := Curr.Next;
          Index := Index + 1;
        end loop;
        Assert (Curr /= null,
                BC.Range_Error'Identity,
                "Append",
                BSE.Invalid_Index);
        while Ptr.Next /= null loop
          Ptr := Ptr.Next;
        end loop;
        Ptr.Next := Curr.Next;
        Curr.Next := From_List.Rep;
        From_List.Rep.Count := From_List.Rep.Count + 1;
      end if;
    end if;
  end Append;

  procedure Remove (L : in out List; From : Positive) is
    Prev : Nodes.Single_Node_Ref;
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Index: Positive := 1;
  begin
    while Curr /= null and then Index < From loop
      Prev := Curr;
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Remove",
            BSE.Invalid_Index);
    -- Ensure we're not removing an aliased element.
    Assert (Curr.Count = 1,
            BC.Referenced'Identity,
            "Remove",
            BSE.Referenced);
    if Prev /= null then
      Prev.Next := Curr.Next;
    else
      L.Rep := Curr.Next;
    end if;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
    else
      Nodes.Delete (Curr);
    end if;
  end Remove;

  procedure Purge (L : in out List; From : Positive) is
    Prev : Nodes.Single_Node_Ref;
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Ptr : Nodes.Single_Node_Ref;
    Index : Positive := 1;
  begin
    while Curr /= null and then Index < From loop
      Prev := Curr;
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Purge",
            BSE.Invalid_Index);
    if Prev /= null then
      Prev.Next := null;
    else
      L.Rep := null;
    end if;
    while Curr /= null loop
      Ptr := Curr;
      Curr := Curr.Next;
      if Ptr.Count > 1 then
        Ptr.Count := Ptr.Count - 1;
        exit;
      else
        Nodes.Delete (Ptr);
      end if;
    end loop;
  end Purge;

  procedure Purge (L : in out List;
                   From : Positive;
                   Count : Positive) is
    Prev, Ptr : Nodes.Single_Node_Ref;
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Index : Positive := 1;
    Shared_Node_Found : Boolean := False;
  begin
    while Curr /= null and then Index < From loop
      Prev := Curr;
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Purge",
            BSE.Invalid_Index);
    if Prev /= null then
      Prev.Next := null;
    else
      L.Rep := null;
    end if;
    Index := 1;
    while Curr /= null and then Index <= Count loop
      Ptr := Curr;
      Curr := Curr.Next;
      if not Shared_Node_Found then
        if Ptr.Count > 1 then
          Ptr.Count := Ptr.Count - 1;
          Shared_Node_Found := True;
        else
          Nodes.Delete (Ptr);
        end if;
      end if;
      Index := Index + 1;
    end loop;
    if Shared_Node_Found then
      Ptr.Next := null;
    end if;
    if Curr /= null then
      if Prev /= null then
        Prev.Next := Curr;
      else
        L.Rep := Curr;
      end if;
    end if;
  end Purge;

  procedure Preserve (L : in out List; From : Positive) is
    Temp : List;
  begin
    Share(Temp, L, From);
    Share_Head(L, Temp);
  end Preserve;

  procedure Preserve (L: in out List; From : Positive; Count : Positive) is
  begin
    Preserve (L, From);
    if Length (L) > Count then
      Purge (L, Count + 1); -- we start at 1, remember!
    end if;
  end Preserve;

  procedure Share (L : in out List; With_List: List; Starting_At : Positive) is
    Ptr : Nodes.Single_Node_Ref := With_List.Rep;
    Index : Positive := 1;
  begin
    Assert (Ptr /= null,
            BC.Is_Null'Identity,
            "Share",
            BSE.Is_Null);
    while Ptr /= null and then Index < Starting_At loop
      Ptr := Ptr.Next;
      Index := Index + 1;
    end loop;
    Assert (Ptr /= null,
            BC.Range_Error'Identity,
            "Share",
            BSE.Invalid_Index);
    Clear (L);
    L.Rep := Ptr;
    L.Rep.Count := L.Rep.Count + 1;
  end Share;

  procedure Share_Head (L : in out List; With_List : in List) is
  begin
    Assert (With_List.Rep /= null,
            BC.Is_Null'Identity,
            "Share_Head",
            BSE.Is_Null);
    Clear (L);
    L.Rep := With_List.Rep;
    L.Rep.Count := L.Rep.Count + 1;
  end Share_Head;

  procedure Share_Foot (L : in out List; With_List : in List) is
    Ptr : Nodes.Single_Node_Ref := With_List.Rep;
  begin
    Assert (Ptr /= null,
            BC.Is_Null'Identity,
            "Share_Foot",
            BSE.Is_Null);
    Clear (L);
    while Ptr.Next /= null loop
      Ptr := Ptr.Next;
    end loop;
    L.Rep := Ptr;
    L.Rep.Count := L.Rep.Count + 1;
  end Share_Foot;

  procedure Swap_Tail (L : in out List; With_List: in out List) is
    Curr : Nodes.Single_Node_Ref;
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Swap_Tail",
            BSE.Is_Null);
    Curr := L.Rep.Next;
    L.Rep.Next := With_List.Rep;
    With_List.Rep := Curr;
  end Swap_Tail;

  procedure Tail (L : in out List) is
    Curr : Nodes.Single_Node_Ref := L.Rep;
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Tail",
            BSE.Is_Null);
    L.Rep := L.Rep.Next;
    if L.Rep /= null then
      L.Rep.Count := L.Rep.Count + 1;
    end if;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
    else
      Nodes.Delete (Curr);
    end if;
  end Tail;

  procedure Set_Head (L : in out List; Elem : Item) is
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Set_Head",
            BSE.Is_Null);
    L.Rep.Element := Elem;
  end Set_Head;

  procedure Set_Item (L : in out List; Elem : Item; At_Loc : Positive) is
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Index: Positive := 1;
  begin
    while Curr /= null and then Index < At_Loc loop
      Curr := Curr.Next;
      Index := Index + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Set_Item",
            BSE.Invalid_Index);
    Curr.Element := Elem;
  end Set_Item;

  function Length (L : List) return Natural is
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Count : Natural := 0;
  begin
    while Curr /= null loop
      Curr := Curr.Next;
      Count := Count + 1;
    end loop;
    return Count;
  end Length;

  function Is_Null (L : List) return Boolean is
  begin
    return L.Rep = null;
  end Is_Null;

  function Is_Shared (L : List) return Boolean is
  begin
    return L.Rep /= null and then L.Rep.Count > 1;
  end Is_Shared;

  function Head (L : List) return Item is
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Head",
            BSE.Is_Null);
    return L.Rep.Element;
  end Head;

  procedure Process_Head (L : in out List) is
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Process_Head",
            BSE.Is_Null);
    Process (L.Rep.Element);
  end Process_Head;

  function Foot (L : List) return Item is
    Curr : Nodes.Single_Node_Ref := L.Rep;
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Foot",
            BSE.Is_Null);
    while Curr.Next /= null loop
      Curr := Curr.Next;
    end loop;
    return Curr.Element;
  end Foot;

  procedure Process_Foot (L : in out List) is
    Curr : Nodes.Single_Node_Ref := L.Rep;
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Process_Foot",
            BSE.Is_Null);
    while Curr.Next /= null loop
      Curr := Curr.Next;
    end loop;
    Process (Curr.Element);
  end Process_Foot;

  function Item_At (L : List; Index : Positive) return Item is
  begin
    return Item_At (L, Index).all;
  end Item_At;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (List);

  function New_Iterator (For_The_List : List) return Iterator'Class is
    Result : List_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_List'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  function Item_At (L : List; Index : Positive) return Item_Ptr is
    Curr : Nodes.Single_Node_Ref := L.Rep;
    Loc : Positive := 1;
  begin
    Assert (L.Rep /= null,
            BC.Is_Null'Identity,
            "Item_At",
            BSE.Is_Null);
    while Curr /= null and then Loc < Index loop
      Curr := Curr.Next;
      Loc := Loc + 1;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Item_At",
            BSE.Invalid_Index);
    return Item_Ptr
       (Allow_Element_Access.To_Pointer (Curr.Element'Address));
  end Item_At;

  procedure Initialize (L : in out List) is
  begin
    null;
  end Initialize;

  procedure Adjust (L : in out List) is
  begin
    if L.Rep /= null then
      L.Rep.Count := L.Rep.Count + 1;
    end if;
  end Adjust;

  procedure Finalize (L : in out List) is
  begin
    Clear (L);
  end Finalize;

  procedure Reset (It : in out List_Iterator) is
    L : List'Class renames List'Class (It.For_The_Container.all);
  begin
    It.Index := L.Rep;
  end Reset;

  procedure Next (It : in out List_Iterator) is
  begin
    if It.Index /= null then
      It.Index := It.Index.Next;
    end if;
  end Next;

  function Is_Done (It : List_Iterator) return Boolean is
  begin
    return It.Index = null;
  end Is_Done;

  function Current_Item (It : List_Iterator) return Item is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return It.Index.Element;
  end Current_Item;

  function Current_Item_Ptr (It : List_Iterator) return Item_Ptr is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_Ptr
       (Allow_Element_Access.To_Pointer (It.Index.Element'Address));
  end Current_Item_Ptr;

  procedure Delete_Item_At (It : in out List_Iterator) is
    L : List'Class renames List'Class (It.For_The_Container.all);
    Prev : Nodes.Single_Node_Ref;
    Curr : Nodes.Single_Node_Ref := L.Rep;
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    while Curr /= null and then Curr /= It.Index loop
      Prev := Curr;
      Curr := Curr.Next;
    end loop;
    Assert (Curr /= null,
            BC.Range_Error'Identity,
            "Delete_Item_At",
            BSE.Invalid_Index);
    -- we need a writable version of the Iterator
    declare
      package Conversions is new System.Address_To_Access_Conversions
         (List_Iterator'Class);
      P : Conversions.Object_Pointer := Conversions.To_Pointer (It'Address);
    begin
      P.Index := Curr.Next;
    end;
    if Prev /= null then
      Prev.Next := Curr.Next;
    else
      L.Rep := Curr.Next;
    end if;
    if Curr.Count > 1 then
      Curr.Count := Curr.Count - 1;
    else
      Nodes.Delete (Curr);
    end if;
  end Delete_Item_At;

  Empty_Container : List;
  pragma Warnings (Off, Empty_Container);

  function Null_Container return List is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Lists.Single;
