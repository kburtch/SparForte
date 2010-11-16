-- Copyright (C) 1998 Grady Booch and Simon Wright.
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

-- $Id: bc-support-exceptions.adb,v 1.2 2005/02/11 02:59:34 ken Exp $

package body BC.Support.Exceptions is


  function Reason_Message (For_The_Reason : Reason) return String is
  begin
    case For_The_Reason is
      when No_Reason_Given => return "";
      when Disjoint => return "objects are members of different structures";
      when Duplicate => return "object already exists";
      when Empty => return "object is empty";
      when Full => return "object is full";
      when Illegal => return "illegal pattern";
      when Invalid_Index => return "index is out of range";
      when Invalid_Number => return "string does not denote a valid number";
      when Missing => return "object does not exist";
      when Not_Empty => return "object is not empty";
      when Not_Root => return "object is not at root of structure";
      when Is_Null => return "object is null";
      when Out_Of_Memory => return "storage requested not available";
      when Referenced => return "object is referenced and cannot be destroyed";
      when Timing => return "possible race condition";
      when Too_Large => return "object is too large";
      when Too_Small => return "object is too small";
    end case;
  end Reason_Message;


  procedure Assert (Condition : Boolean;
                    Raising_If_False : Ada.Exceptions.Exception_Id;
                    From_Subprogram : String;
                    With_Reason : Reason := No_Reason_Given) is
  begin
    if not Condition then
      if With_Reason = No_Reason_Given then
        Ada.Exceptions.Raise_Exception
           (Raising_If_False,
            Module & "." & From_Subprogram);
      else
        Ada.Exceptions.Raise_Exception
           (Raising_If_False,
            Module & "." & From_Subprogram & ": "
            & Reason_Message (With_Reason));
      end if;
    end if;
  end Assert;


  procedure Report (The_Exception : Ada.Exceptions.Exception_Occurrence;
                    To : Ada.Text_Io.File_Type := Ada.Text_Io.Standard_Output) is
  begin
    if Ada.Exceptions.Exception_Message (The_Exception)'Length = 0 then
      Ada.Text_Io.Put_Line (File => To,
                            Item => "Exception "
                            & Ada.Exceptions.Exception_Name (The_Exception)
                            & " occurred.");
    else
      Ada.Text_Io.Put_Line (File => To,
                            Item => "Exception "
                            & Ada.Exceptions.Exception_Name (The_Exception)
                            & " ("
                            & Ada.Exceptions.Exception_Message (The_Exception)
                            & ") occurred.");
    end if;
  end Report;


end BC.Support.Exceptions;
