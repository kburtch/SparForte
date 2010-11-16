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

-- $Id: bc-support-exceptions.ads,v 1.2 2005/02/11 02:59:34 ken Exp $

with Ada.Exceptions;
with Ada.Text_Io;

package BC.Support.Exceptions is

  pragma Elaborate_Body;

  -- These codes are shorthand for standard messages
  type Reason is (No_Reason_Given,
                  Disjoint,
                  Duplicate,
                  Empty,
                  Full,
                  Illegal,
                  Invalid_Index,
                  Invalid_Number,
                  Missing,
                  Not_Empty,
                  Not_Root,
                  Is_Null,
                  Out_Of_Memory,
                  Referenced,
                  Timing,
                  Too_Large,
                  Too_Small);

  generic
    Module : String;
  procedure Assert (Condition : Boolean;
                    Raising_If_False : Ada.Exceptions.Exception_Id;
                    From_Subprogram : String;
                    With_Reason : Reason := No_Reason_Given);
  pragma Inline (Assert);

  procedure Report (The_Exception : Ada.Exceptions.Exception_Occurrence;
                    To : Ada.Text_Io.File_Type := Ada.Text_Io.Standard_Output);

end BC.Support.Exceptions;
