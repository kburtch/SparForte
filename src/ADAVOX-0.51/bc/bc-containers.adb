-- Copyright (C) 1994-2001 Grady Booch, David Weller, Steve Doiel
-- and Simon Wright.
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

-- $Id: bc-containers.adb,v 1.2 2005/02/11 02:59:33 ken Exp $

package body BC.Containers is

  -- Iteration support

  procedure Access_Current_Item (In_The_Iterator : Iterator'Class) is
  begin
    Apply (Current_Item_Ptr (In_The_Iterator).all);
  end Access_Current_Item;

  procedure Visit (Using : in out Iterator'Class) is
    Success : Boolean;
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Apply (Current_Item (Using), Success);
      exit when not Success;
      Next (Using);
    end loop;
  end Visit;

  procedure Visit_With_In_Param (Using : in out Iterator'Class;
                                 Param : Param_Type) is
    Success : Boolean;
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Apply (Current_Item (Using), Param, Success);
      exit when not Success;
      Next (Using);
    end loop;
  end Visit_With_In_Param;

  procedure Visit_With_In_Out_Param (Using : in out Iterator'Class;
                                     Param : in out Param_Type) is
    Success : Boolean;
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Apply (Current_Item (Using), Param, Success);
      exit when not Success;
      Next (Using);
    end loop;
  end Visit_With_In_Out_Param;

  procedure Modify (Using : in out Iterator'Class) is
    Success : Boolean;
    procedure Caller (I : in out Item) is
    begin
      Apply (I, Success);
    end Caller;
    procedure Call_Apply is new Access_Current_Item (Caller);
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Call_Apply (Using);
      exit when not Success;
      Next (Using);
    end loop;
  end Modify;

  procedure Modify_With_In_Param (Using : in out Iterator'Class;
                                  Param : in Param_Type ) is
    Success : Boolean;
    procedure Caller (I : in out Item) is
    begin
      Apply (I, Param, Success);
    end Caller;
    procedure Call_Apply is new Access_Current_Item (Caller);
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Call_Apply (Using);
      exit when not Success;
      Next (Using);
    end loop;
  end Modify_With_In_Param;

  procedure Modify_With_In_Out_Param (Using : in out Iterator'Class;
                                      Param : in out Param_Type) is
    Success : Boolean;
    procedure Caller (I : in out Item) is
    begin
      Apply (I, Param, Success);
    end Caller;
    procedure Call_Apply is new Access_Current_Item (Caller);
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Call_Apply (Using);
      exit when not Success;
      Next (Using);
    end loop;
  end Modify_With_In_Out_Param;

  -- Primitive implementations

  procedure Lock (C : in out Container) is
  begin
    null;
  end Lock;

  procedure Unlock (C : in out Container) is
  begin
    null;
  end Unlock;

  function Item_At (C : Container; Index : Positive) return Item_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Item_At;

  function Current_Item_Ptr (It : Iterator) return Item_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Current_Item_Ptr;

end BC.Containers;
