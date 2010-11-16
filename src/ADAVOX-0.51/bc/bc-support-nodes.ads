-- Copyright (C) 1994-2000 Grady Booch, David Weller, Pat Rogers and
-- Simon Wright.
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

-- $Id: bc-support-nodes.ads,v 1.2 2005/02/11 02:59:34 ken Exp $

with Ada.Unchecked_Deallocation;
with System.Storage_Pools;

generic
  type Item is private;
  type Storage_Manager(<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Support.Nodes is

  pragma Elaborate_Body;


  -- Type denoting a simple node consisting of an item and pointers to the
  -- previous and next Items

  type Node;
  type Node_Ref is access Node;
  for Node_Ref'Storage_Pool use Storage;
  type Node is record
    Element : Item;
    Next : Node_Ref;
    Previous : Node_Ref;
  end record;

  function Create (I : Item; Previous, Next : Node_Ref) return Node_Ref;
  pragma Inline (Create);

  procedure Delete is new Ada.Unchecked_Deallocation (Node, Node_Ref);


  -- Type denoting a simple node consisting of an item, a pointer to the
  -- next item, and a reference count

  type Single_Node;
  type Single_Node_Ref is access Single_Node;
  for Single_Node_Ref'Storage_Pool use Storage;
  type Single_Node is record
    Element : Item;
    Next : Single_Node_Ref;
    Count : Natural := 1;
  end record;

  function Create (I : Item; Next : Single_Node_Ref) return Single_Node_Ref;
  pragma Inline (Create);

  procedure Delete is
     new Ada.Unchecked_Deallocation (Single_Node, Single_Node_Ref);


  -- Type denoting a simple node consisting of an item, pointers to the
  -- previous and next items, and a reference count

  type Double_Node;
  type Double_Node_Ref is access Double_Node;
  for Double_Node_Ref'Storage_Pool use Storage;
  type Double_Node is record
    Element : Item;
    Previous : Double_Node_Ref;
    Next : Double_Node_Ref;
    Count : Natural := 1;
  end record;

  function Create
     (I : Item; Previous, Next : Double_Node_Ref) return Double_Node_Ref;
  pragma Inline (Create);

  procedure Delete is
     new Ada.Unchecked_Deallocation (Double_Node, Double_Node_Ref);


  -- Type denoting a simple node consisting of an item, a pointer to the
  -- parent, pointers to the left and right items, and a reference count

  type Binary_Node;
  type Binary_Node_Ref is access Binary_Node;
  for Binary_Node_Ref'Storage_Pool use Storage;

  type Binary_Node is record
    Element : Item;
    Parent, Left, Right : Binary_Node_Ref;
    Count : Natural := 1;
  end record;

  function Create
     (I : Item; Parent, Left, Right : Binary_Node_Ref) return Binary_Node_Ref;
  pragma Inline (Create);

  procedure Delete is
     new Ada.Unchecked_Deallocation (Binary_Node, Binary_Node_Ref);


  -- Type denoting a simple node consisting of an item, a pointer to the
  -- parent, pointers to the child and sibling items, and a reference count

  type Multiway_Node;
  type Multiway_Node_Ref is access Multiway_Node;
  for Multiway_Node_Ref'Storage_Pool use Storage;

  type Multiway_Node is record
    Element : Item;
    Parent, Child, Sibling : Multiway_Node_Ref;
    Count : Natural := 1;
  end record;

  function Create
     (I : Item; Parent, Child, Sibling : Multiway_Node_Ref)
      return Multiway_Node_Ref;
  pragma Inline (Create);

  procedure Delete is
     new Ada.Unchecked_Deallocation (Multiway_Node, Multiway_Node_Ref);


  -- Type denoting a simple node consisting of an item, pointers to the
  -- left and right items, and a balance

  type AVL_Node;
  type AVL_Node_Ref is access AVL_Node;
  for AVL_Node_Ref'Storage_Pool use Storage;

  type Node_Balance is (Left, Middle, Right);

  type AVL_Node is record
    Element : Item;
    Left, Right : AVL_Node_Ref;
    Balance : Node_Balance := Middle;
  end record;

  function Create (I : Item) return AVL_Node_Ref;
  pragma Inline (Create);

  procedure Delete is
     new Ada.Unchecked_Deallocation (AVL_Node, AVL_Node_Ref);


end BC.Support.Nodes;
