-- $Id: apq-postgresql-client.ads,v 1.2 2005/02/11 02:59:43 ken Exp $
-- Copyright (c) 2002, Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)
-- or
-- GNU Public License 2 (GPL2)
-- 
--     This program is free software; you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation; either version 2 of the License, or
--     (at your option) any later version.
-- 
--     This program is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.
-- 
--     You should have received a copy of the GNU General Public License
--     along with this program; if not, write to the Free Software
--     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

with System;
with Interfaces;
with Ada.Text_IO.C_Streams;
with Ada.Finalization;
with Ada.Streams.Stream_IO;
with Ada.Calendar;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Interfaces.C_Streams;

package APQ.PostgreSQL.Client is

   package Str renames Ada.Streams;
   package CStr renames Interfaces.C_Streams;

   ------------------------------
   -- CLIENT DATA TYPES
   ------------------------------
   type Connection_Type is new APQ.Root_Connection_Type with private;
   type Notify_Proc_Type is access procedure(C : in out Connection_Type; Message : String);
   type Query_Type is new Root_Query_Type with private;
   type Blob_Type is private;

   type Root_Stream_Access is access all Str.Root_Stream_Type'Class;

   ------------------------------
   -- DATABASE CONNECTION :
   ------------------------------
   function Engine_Of(C : Connection_Type) return Database_Type;
   function New_Query(C : Connection_Type) return Root_Query_Type'Class;

   procedure Notify_on_Standard_Error(C : in out Connection_Type; Message : String);

   function Host_Name(C : Connection_Type) return String;
   function Port(C : Connection_Type) return Integer;
   function Port(C : Connection_Type) return String;
   function DB_Name(C : Connection_Type) return String;
   function User(C : Connection_Type) return String;
   function Password(C : Connection_Type) return String;

   procedure Set_Options(C : in out Connection_Type; Options : String);
   function Options(C : Connection_Type) return String;

   procedure Set_Notify_Proc(C : in out Connection_Type; Notify_Proc : Notify_Proc_Type);
   function Notify_Proc(C : Connection_Type) return Notify_Proc_Type;

   procedure Connect(C : in out Connection_Type);
   procedure Connect(C : in out Connection_Type; Same_As : Root_Connection_Type'Class);
   procedure Disconnect(C : in out Connection_Type);

   function Is_Connected(C : Connection_Type) return Boolean;
   procedure Reset(C : in out Connection_Type);
   function Error_Message(C : Connection_Type) return String;
   function Notice_Message(C : Connection_Type) return String;

   -- Open trace output file
   procedure Open_DB_Trace(C : in out Connection_Type; Filename : String; Mode : Trace_Mode_Type := Trace_APQ);
   procedure Close_DB_Trace(C : in out Connection_Type);                         -- Close trace output file
   procedure Set_Trace(C : in out Connection_Type; Trace_On : Boolean := True);  -- Enable/Disable tracing
   function Is_Trace(C : Connection_Type) return Boolean;                        -- Test trace enabled/disabled

   function In_Abort_State(C : Connection_Type) return Boolean;

   No_Notify :             constant Notify_Proc_Type := null;    -- Null disables notification
   Standard_Error_Notify : constant Notify_Proc_Type;

   ------------------------------
   -- SQL QUERY API :
   ------------------------------

   procedure Clear(Q : in out Query_Type);
   procedure Append_Quoted(Q : in out Query_Type; Connection : Root_Connection_Type'Class; SQL : String; After : String := "");

   procedure Execute(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class);
   procedure Execute_Checked(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class; Msg : String := "");
   
   procedure Begin_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class);
   procedure Commit_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class);
   procedure Rollback_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class);

   procedure Rewind(Q : in out Query_Type);
   procedure Fetch(Q : in out Query_Type);
   procedure Fetch(Q : in out Query_Type; TX : Tuple_Index_Type);

   function End_of_Query(Q : Query_Type) return Boolean;       -- Avoid use (catch exception instead)

   function Tuple(Q : Query_Type) return Tuple_Index_Type;
   function Tuples(Q : Query_Type) return Tuple_Count_Type;

   function Columns(Q : Query_Type) return Natural;
   function Column_Name(Q : Query_Type; CX : Column_Index_Type) return String;
   function Column_Index(Q : Query_Type; Name : String) return Column_Index_Type;
   function Column_Type(Q : Query_Type; CX : Column_Index_Type) return Row_ID_Type;

   function Is_Null(Q : Query_Type; CX : Column_Index_Type) return Boolean;
   function Value(Query : Query_Type; CX : Column_Index_Type) return String;

   function Result(Query : Query_Type) return Natural;            -- Returns Result_Type'Pos()
   function Result(Query : Query_Type) return Result_Type;
   function Command_Oid(Query : Query_Type) return Row_ID_Type;
   function Null_Oid(Query : Query_Type) return Row_ID_Type;

   function Command_Status(Query : Query_Type) return String;     -- PostgreSQL only

   function Error_Message(Query : Query_Type) return String;
   function Is_Duplicate_Key(Query : Query_Type) return Boolean;
   function Engine_Of(Q : Query_Type) return Database_Type;

   ------------------------------
   -- BLOB API :
   ------------------------------

   Buf_Size_Default : constant Natural;

   type Blob_Count is new Ada.Streams.Stream_Element_Offset range 0..Ada.Streams.Stream_Element_Offset'Last;
   subtype Blob_Offset is Blob_Count range 1..Blob_Count'Last;

   function Blob_Create(DB : access Connection_Type; Buf_Size : Natural := Buf_Size_Default) return Blob_Type;
   function Blob_Open(DB : access Connection_Type; Oid : Row_ID_Type; Mode : Mode_Type; Buf_Size : Natural := Buf_Size_Default) return Blob_Type;
   procedure Blob_Flush(Blob : Blob_Type);
   procedure Blob_Close(Blob : in out Blob_Type);

   procedure Blob_Set_Index (Blob : Blob_Type; To : Blob_Offset);
   function Blob_Index(Blob : Blob_Type) return Blob_Offset;
   function End_of_Blob(Blob : Blob_Type) return Boolean;

   function Blob_Oid(Blob : Blob_Type) return Row_ID_Type;
   function Blob_Size(Blob : Blob_Type) return Blob_Count;

   function Blob_Stream(Blob : Blob_Type) return Root_Stream_Access;

   procedure Blob_Unlink(DB : Connection_Type; Oid : Row_ID_Type);

   procedure Blob_Import(DB : Connection_Type; Pathname : String; Oid : out Row_ID_Type);
   procedure Blob_Export(DB : Connection_Type; Oid : Row_ID_Type; Pathname : String);

   generic
      type Oid_Type is new Row_ID_Type;
   function Generic_Blob_Open(DB : access Connection_Type; Oid : Oid_Type; Mode : Mode_Type; Buf_Size : Natural := Buf_Size_Default) return Blob_Type;

   generic
      type Oid_Type is new Row_ID_Type;
   function Generic_Blob_Oid(Blob : Blob_Type) return Oid_Type;

   generic
      type Oid_Type is new Row_ID_Type;
   procedure Generic_Blob_Unlink(DB : Connection_Type; Oid : Oid_Type);

   generic
      type Oid_Type is new Row_ID_Type;
   procedure Generic_Blob_Import(DB : Connection_Type; Pathname : String; Oid : out Oid_Type);

   generic
      type Oid_Type is new Row_ID_Type;
   procedure Generic_Blob_Export(DB : Connection_Type; Oid : Oid_Type; Pathname : String);

private

   type PG_Conn is new System.Address;
   Null_Connection : PG_Conn := PG_Conn(System.Null_Address);

   type Connection_Type is new APQ.Root_Connection_Type with
      record
         Options :         String_Ptr;                   -- Debug and trace options, if any
         Connection :      PG_Conn := Null_Connection;
         Error_Message :   String_Ptr;                   -- Error message after failed to connect (only)
         Notice :          String_Ptr;                   -- Last notice message if any
         Notify_Proc :     Notify_Proc_Type;             -- Notify procedure or NULL
      end record;

   procedure Finalize(C : in out Connection_Type);
   function Internal_Connection(C : Connection_Type) return PG_Conn;

   type PQ_Result is new System.Address;
   Null_Result : PQ_Result := PQ_Result(System.Null_Address);
 
   type Query_Type is new Root_Query_Type with
      record
         Result :          PQ_Result := Null_Result;  -- Result from a command
      end record;

   procedure Adjust(Q : in out Query_Type);
   procedure Finalize(Q : in out Query_Type);

   type Blob_Fd is range -2 ** 31 .. 2 ** 31 - 1;

   type Blob_Object(Conn : access Connection_Type) is new Ada.Streams.Root_Stream_Type with
      record
         Oid :             Row_ID_Type := Row_ID_Type'First;   -- Oid of this blob
         Mode :            Mode_Type   := Read;                -- I/O mode of blob
         Fd :              Blob_Fd     := -1;                  -- Blob file descriptor
         Buffer :          Stream_Element_Array_Ptr;           -- The stream buffer, if any
         Buf_Empty :       Boolean     := True;                -- True when buffer is empty
         Buf_Dirty :       Boolean     := False;               -- True when the buffer needs writing out
         Buf_Size :        Str.Stream_Element_Offset := 0;     -- The logical size of the buffer
         Buf_Offset :      Str.Stream_Element_Offset := 0;     -- The physical offset of the buffer
         Log_Offset :      Str.Stream_Element_Offset := 0;     -- The current logical offset within the blob
         Phy_Offset :      Str.Stream_Element_Offset := 0;     -- Physical blob offset
         The_Size :        Str.Stream_Element_Offset := 0;     -- The blob's size in bytes
      end record;

   type Blob_Type is access all Blob_Object;

   procedure Finalize(Blob : in out Blob_Object);

   procedure Read(
      Stream:  in out   Blob_Object;
      Item:       out   Ada.Streams.Stream_Element_Array;
      Last:       out   Ada.Streams.Stream_Element_Offset
   );

   procedure Write(
      Stream:  in out   Blob_Object;
      Item:    in       Ada.Streams.Stream_Element_Array
   );
                            
   Buf_Size_Default : constant Natural := 5 * 1024;

   Standard_Error_Notify : constant Notify_Proc_Type := Notify_on_Standard_Error'Access;

   pragma Inline(Is_Connected);
   pragma Inline(In_Abort_State);
   pragma Inline(Clear_Abort_State);
   pragma Inline(Raise_Exceptions);
   pragma Inline(Report_Errors);
   pragma Inline(Rewind);
   pragma Inline(End_Of_Query);

   pragma Inline(Blob_Oid);
   pragma Inline(End_Of_Blob);

end APQ.PostgreSQL.Client;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/apq-postgresql-client.ads,v $
