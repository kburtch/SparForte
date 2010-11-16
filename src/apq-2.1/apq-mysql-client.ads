-- $Id: apq-mysql-client.ads,v 1.2 2005/02/11 02:59:42 ken Exp $
-- Copyright (c) 2003, Warren W. Gay VE3WWG
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

package APQ.MySQL.Client is

   package Str renames Ada.Streams;
   package CStr renames Interfaces.C_Streams;

   ------------------------------
   -- CLIENT DATA TYPES
   ------------------------------
   type Connection_Type is new APQ.Root_Connection_Type with private;

   type Query_Type is new APQ.Root_Query_Type with private;

   ------------------------------
   -- DATABASE CONNECTION :
   ------------------------------

   function Engine_Of(C : Connection_Type) return Database_Type;
   function New_Query(C : Connection_Type) return Root_Query_Type'Class;

   procedure Set_DB_Name(C : in out Connection_Type; DB_Name : String);

   function Port(C : Connection_Type) return Integer;
   function Port(C : Connection_Type) return String;

   procedure Set_Options(C : in out Connection_Type; Options : String);
   function Options(C : Connection_Type) return String;

   procedure Connect(C : in out Connection_Type);
   procedure Connect(C : in out Connection_Type; Same_As : Root_Connection_Type'Class);
   procedure Disconnect(C : in out Connection_Type);

   function Is_Connected(C : Connection_Type) return Boolean;
   procedure Reset(C : in out Connection_Type);
   function Error_Message(C : Connection_Type) return String;

   -- Open trace output file
   procedure Open_DB_Trace(C : in out Connection_Type; Filename : String; Mode : Trace_Mode_Type := Trace_APQ);
   procedure Close_DB_Trace(C : in out Connection_Type);                         -- Close trace output file
   procedure Set_Trace(C : in out Connection_Type; Trace_On : Boolean := True);  -- Enable/Disable tracing
   function Is_Trace(C : Connection_Type) return Boolean;                        -- Test trace enabled/disabled

   function In_Abort_State(C : Connection_Type) return Boolean;

   ------------------------------
   -- SQL QUERY API :
   ------------------------------

   procedure Clear(Q : in out Query_Type);
   procedure Append_Quoted(Q : in out Query_Type; Connection : Root_Connection_Type'Class; SQL : String; After : String := "");
   procedure Set_Fetch_Mode(Q : in out Query_Type; Mode : Fetch_Mode_Type);

   procedure Execute(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class);
   procedure Execute_Checked(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class; Msg : String := "");

   procedure Begin_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class);
   procedure Commit_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class);
   procedure Rollback_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class);

   procedure Rewind(Q : in out Query_Type);
   procedure Fetch(Q : in out Query_Type);
   procedure Fetch(Q : in out Query_Type; TX : Tuple_Index_Type);

   function End_of_Query(Q : Query_Type) return Boolean;    -- Do not use with Sequential_Fetch mode!

   function Tuple(Q : Query_Type) return Tuple_Index_Type;
   function Tuples(Q : Query_Type) return Tuple_Count_Type;

   function Columns(Q : Query_Type) return Natural;
   function Column_Name(Q : Query_Type; CX : Column_Index_Type) return String;
   function Column_Index(Q : Query_Type; Name : String) return Column_Index_Type;
   function Column_Type(Q : Query_Type; CX : Column_Index_Type) return Field_Type;

   function Is_Null(Q : Query_Type; CX : Column_Index_Type) return Boolean;
   function Value(Query : Query_Type; CX : Column_Index_Type) return String;

   function Result(Query : Query_Type) return Natural;      -- Returns Result_Type'Pos()  (for generics)
   function Result(Query : Query_Type) return Result_Type;
   function Command_Oid(Query : Query_Type) return Row_ID_Type;
   function Null_Oid(Query : Query_Type) return Row_ID_Type;

   function Error_Message(Query : Query_Type) return String;
   function Is_Duplicate_Key(Query : Query_Type) return Boolean;
   function Engine_Of(Q : Query_Type) return Database_Type;

private

   type Connection_Type is new APQ.Root_Connection_Type with
      record
         Options :         String_Ptr;                   -- MySQL database engine options
         Connection :      MYSQL := Null_Connection;     -- MySQL connection object
         Connected :       Boolean := False;             -- True when connected
         Error_Code :      Result_Type;                  -- Error code (should agree with message)
         Error_Message :   String_Ptr;                   -- Error message after failed to connect (only)
      end record;

   procedure Finalize(C : in out Connection_Type);
   procedure Initialize(C : in out Connection_Type);

   type Query_Type is new APQ.Root_Query_Type with
      record
         Results :         APQ.MySQL.MYSQL_RES := Null_Result; -- MySQL Query Results (if any)
         Error_Code :      Result_Type;                  -- Error code (should agree with message)
         Error_Message :   String_Ptr;                   -- Error message after failed to connect (only)
         Row :             APQ.MySQL.MYSQL_ROW := Null_Row; -- The current/last row fetched
         Row_ID :          Row_ID_Type;                  -- Row ID from last INSERT/UPDATE
      end record;

   procedure Adjust(Q : in out Query_Type);
   procedure Finalize(Q : in out Query_Type);

end APQ.MySQL.Client;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/apq-mysql-client.ads,v $
