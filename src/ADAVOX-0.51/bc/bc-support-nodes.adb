-- Copyright (C) 1994-1999 Grady Booch, David Weller and Simon Wright.
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

-- $Id: bc-support-nodes.adb,v 1.2 2005/02/11 02:59:34 ken Exp $

package body BC.Support.Nodes is


  function Create (I : Item; Previous, Next : Node_Ref) return Node_Ref is
    Result : Node_Ref;
  begin
    Result := new Node'(Element => I,
                        Previous => Previous,
                        Next => Next);
    if Previous /= null then
      Previous.Next := Result;
    end if;
    if Next /= null then
      Next.Previous := Result;
    end if;
    return Result;
  end Create;


  function Create (I : Item; Next : Single_Node_Ref) return Single_Node_Ref is
  begin
    return new Single_Node'(Element => I,
                            Next => Next,
                            Count => 1);
  end Create;


  function Create
     (I : Item; Previous, Next : Double_Node_Ref) return Double_Node_Ref is
    Result : Double_Node_Ref;
  begin
    Result := new Double_Node'(Element => I,
                               Previous => Previous,
                               Next => Next,
                               Count => 1);
    if Previous /= null then
      Previous.Next := Result;
    end if;
    if Next /= null then
      Next.Previous := Result;
    end if;
    return Result;
  end Create;


  function Create
     (I : Item; Parent, Left, Right : Binary_Node_Ref)
      return Binary_Node_Ref is
    Result : Binary_Node_Ref;
  begin
    Result := new Binary_Node'(Element => I,
                               Parent => Parent,
                               Left => Left,
                               Right => Right,
                               Count => 1);
    if Left /= null then
      Left.Parent := Result;
    end if;
    if Right /= null then
      Right.Parent := Result;
    end if;
    return Result;
  end Create;


  function Create
     (I : Item; Parent, Child, Sibling : Multiway_Node_Ref)
      return Multiway_Node_Ref is
    Result : Multiway_Node_Ref;
  begin
    Result := new Multiway_Node'(Element => I,
                                 Parent => Parent,
                                 Child => Child,
                                 Sibling => Sibling,
                                 Count => 1);
    if Child /= null then
      Child.Parent := Result;
    end if;
    return Result;
  end Create;


  function Create (I : Item) return AVL_Node_Ref is
  begin
    return new AVL_Node'(Element => I,
                         Left => null,
                         Right => null,
                         Balance => Middle);
  end Create;


end BC.Support.Nodes;
