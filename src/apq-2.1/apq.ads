-- $Id: apq.ads,v 1.2 2005/02/11 02:59:43 ken Exp $
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

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Streams;
with Ada.Characters.Latin_1;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with Interfaces.C_Streams;
with System;

package APQ is

   pragma Linker_Options("-lapq");

   Is_Abstract :        exception;     -- Required functionality was not implemented
   SQL_Error :          exception;     -- SQL Error Occurred
   Not_Connected :      exception;     -- Connect failed, or no connection
   Already_Connected :  exception;     -- A connection has already been established
   No_Result :          exception;     -- No result available
   No_Column :          exception;     -- Column does not exist (at index)
   No_Tuple :           exception;     -- No such tuple
   Null_Value :         exception;     -- Attempt to access a null value
   Invalid_Format :     exception;     -- Invalid format or bad data
   Small_Buffer :       exception;     -- Truncation into a small buffer
   Blob_Error :         exception;     -- Operation on blob failed
   Abort_State :        exception;     -- A ROLLBACK operation is required
   Tracing_State :      exception;     -- Already tracing to a file
   Failed :             exception;     -- General operation failed

   type Database_Type is (
      Engine_PostgreSQL,               -- PostgreSQL database engine is being used
      Engine_MySQL,                    -- MySQL database engine is being used
      Engine_ORACLE,                   -- Not supported yet
      Engine_Sybase,                   -- Not supported yet
      Engine_DB2                       -- Not supported yet
   );

   Line_Feed : constant String(1..1) := Ada.Characters.Latin_1.LF & "";

   type APQ_Smallint is range -32768..32767;
   type APQ_Integer is range -2 ** 31 .. 2 ** 31 - 1;
   type APQ_Bigint is range -2 ** 63 .. 2 ** 63 - 1;
   type APQ_Real is digits 6;
   type APQ_Double is digits 15;
   type APQ_Serial is range 1..2147483647;
   type APQ_Bigserial is range 1..9223372036854775807;

   subtype APQ_Boolean is Boolean;                 -- Boolean type
   subtype APQ_Date is Ada.Calendar.Time;          -- Date (time ignored)
   subtype APQ_Time is Ada.Calendar.Day_Duration;  -- Time only (date ignored)
   type APQ_Timestamp is new Ada.Calendar.Time;    -- Date and time
   type APQ_Timezone is new Integer range -23..23; -- Timezone in +/- hours from UTC
   type APQ_Bitstring is array(Positive range <>) of APQ_Boolean;
   pragma pack(APQ_Bitstring);


   type Row_ID_Type is mod 2 ** 64;                -- Identifies a specific row

   type Hour_Number is range 0..23;
   type Minute_Number is range 0..59;
   type Second_Number is range 0..59;

   type Tuple_Index_Type is mod 2 ** 64;     -- Related concept to Row_ID_Type
   First_Tuple_Index : constant Tuple_Index_Type := 1;

   subtype Tuple_Count_Type is Tuple_Index_Type;

   type Column_Index_Type is new Positive;

   type Root_Connection_Type is new Ada.Finalization.Limited_Controlled with private;

   type Trace_Mode_Type is (
      Trace_None,                      -- No tracing
      Trace_DB,                        -- Enable database library tracing
      Trace_APQ,                       -- APQ Trace
      Trace_Full                       -- Full trace information (Trace_DB and Trace_APQ)
   );

   type Fetch_Mode_Type is ( Sequential_Fetch, Random_Fetch );

   type Root_Query_Type is new Ada.Finalization.Controlled with private;


   function Engine_Of(C : Root_Connection_Type) return Database_Type;
   function New_Query(C : Root_Connection_Type) return Root_Query_Type'Class;

   procedure Set_Host_Name(C : in out Root_Connection_Type; Host_Name : String);
   procedure Set_Host_Address(C : in out Root_Connection_Type; Host_Address : String);
   function Host_Name(C : Root_Connection_Type) return String;

   procedure Set_Port(C : in out Root_Connection_Type; Port_Number : Integer);
   procedure Set_Port(C : in out Root_Connection_Type; Port_Name : String);
   function Port(C : Root_Connection_Type) return Integer;
   function Port(C : Root_Connection_Type) return String;

   procedure Set_DB_Name(C : in out Root_Connection_Type; DB_Name : String);
   function DB_Name(C : Root_Connection_Type) return String;

   procedure Set_User_Password(C : in out Root_Connection_Type; User_Name, User_Password : String);
   function User(C : Root_Connection_Type) return String;
   function Password(C : Root_Connection_Type) return String;

   procedure Connect(C : in out Root_Connection_Type);   -- IS ABSTRACT
   procedure Connect(C : in out Root_Connection_Type; Same_As : Root_Connection_Type'Class); -- IS ABSTRACT
   procedure Disconnect(C : in out Root_Connection_Type); -- IS ABSTRACT

   function Is_Connected(C : Root_Connection_Type) return Boolean; -- IS ABSTRACT
   procedure Reset(C : in out Root_Connection_Type); -- IS ABSTRACT
   function Error_Message(C : Root_Connection_Type) return String; -- IS ABSTRACT

   function In_Abort_State(C : Root_Connection_Type) return Boolean;

   procedure Set_Rollback_On_Finalize(C : in out Root_Connection_Type; Rollback : Boolean);
   function Will_Rollback_On_Finalize(C : Root_Connection_Type) return Boolean;


   function Engine_Of(Q : Root_Query_Type) return Database_Type;

   procedure Clear(Q : in out Root_Query_Type);
   function Fetch_Mode(Q : Root_Query_Type) return Fetch_Mode_Type;
   procedure Set_Fetch_Mode(Q : in out Root_Query_Type; Mode : Fetch_Mode_Type);
   procedure Prepare(Q : in out Root_Query_Type; SQL : String; After : String := Line_Feed);
   procedure Append(Q : in out Root_Query_Type; SQL : String; After : String := "");
   procedure Append(Q : in out Root_Query_Type; SQL : Ada.Strings.Unbounded.Unbounded_String; After : String := "");
   procedure Append_Line(Q : in out Root_Query_Type; SQL : String := "");

   procedure Append(Q : in out Root_Query_Type; V : APQ_Boolean; After : String := "");
   procedure Append(Q : in out Root_Query_Type; V : APQ_Date; After : String := "");

   procedure Append(Q : in out Root_Query_Type; V : APQ_Time; After : String := "");
   procedure Append(Q : in out Root_Query_Type; V : APQ_Timestamp; After : String := "");
   procedure Append(Q : in out Root_Query_Type; TS : APQ_Timestamp; TZ : APQ_Timezone; After : String := "");

   procedure Append(Q : in out Root_Query_Type; V : APQ_Bitstring; After : String := "");
   procedure Append(Q : in out Root_Query_Type; V : Row_ID_Type; After : String := "");

   procedure Append_Quoted(Q : in out Root_Query_Type; Connection : Root_Connection_Type'Class; SQL : String; After : String := "");
   procedure Append_Quoted(Q : in out Root_Query_Type; Connection : Root_Connection_Type'Class; SQL : Ada.Strings.Unbounded.Unbounded_String; After : String := "");

   procedure Execute(Query : in out Root_Query_Type; Connection : in out Root_Connection_Type'Class);
   procedure Execute_Checked(Query : in out Root_Query_Type; Connection : in out Root_Connection_Type'Class; Msg : String := "");

   procedure Begin_Work(Query : in out Root_Query_Type; Connection : in out Root_Connection_Type'Class);
   procedure Commit_Work(Query : in out Root_Query_Type; Connection : in out Root_Connection_Type'Class);
   procedure Rollback_Work(Query : in out Root_Query_Type; Connection : in out Root_Connection_Type'Class);

   procedure Raise_Exceptions(Query : in out Root_Query_Type; Raise_On : Boolean := True);
   procedure Report_Errors(Query : in out Root_Query_Type; Report_On : Boolean := True);
   
   procedure Rewind(Q : in out Root_Query_Type);
   procedure Fetch(Q : in out Root_Query_Type);
   procedure Fetch(Q : in out Root_Query_Type; TX : Tuple_Index_Type);

   function End_of_Query(Q : Root_Query_Type) return Boolean;
   function Tuple(Q : Root_Query_Type) return Tuple_Index_Type;
   function Tuples(Q : Root_Query_Type) return Tuple_Count_Type;

   function Value(Query : Root_Query_Type; CX : Column_Index_Type) return String;     -- Abstract

   procedure Value(Query: Root_Query_Type; CX : Column_Index_Type; V : out String);
   function Value(Query : Root_Query_Type; CX : Column_Index_Type) return Ada.Strings.Unbounded.Unbounded_String;
   function Value(Query : Root_Query_Type; CX : Column_Index_Type) return Row_ID_Type;
   function Value(Query : Root_Query_Type; CX : Column_Index_Type) return APQ_Bitstring;

   function Result(Query : Root_Query_Type) return Natural;    -- Returns Result_Type'Pos()

   function Is_Null(Q : Root_Query_Type; CX : Column_Index_Type) return Boolean;

   function Command_Oid(Query : Root_Query_Type) return Row_ID_Type;
   function Null_Oid(Query : Root_Query_Type) return Row_ID_Type;

   function Error_Message(Query : Root_Query_Type) return String;
   function Is_Duplicate_Key(Query : Root_Query_Type) return Boolean;
   function To_String(Query : Root_Query_Type) return String;


   generic
      type Ind_Type is new Boolean;
   function Column_Is_Null(Q : Root_Query_Type'Class; CX : Column_Index_Type) return Ind_Type;


   generic
      type Val_Type is new Boolean;
   procedure Append_Boolean(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "");

   generic
      type Val_Type is range <>;
   procedure Append_Integer(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "");

   generic
      type Val_Type is mod <>;
   procedure Append_Modular(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "");

   generic
      type Val_Type is digits <>;
   procedure Append_Float(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "");

   generic
      type Val_Type is delta <>;
   procedure Append_Fixed(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "");

   generic
      type Val_Type is delta <> digits <>;
   procedure Append_Decimal(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "");

   generic
      type Val_Type is new Ada.Calendar.Time;
   procedure Append_Date(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "");

   generic
      type Val_Type is new Ada.Calendar.Day_Duration;
   procedure Append_Time(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "");

   generic
      type Val_Type is new APQ_Timestamp;
   procedure Append_Timestamp(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "");

   generic
      type Val_Type is new APQ_Bitstring;
   procedure Append_Bitstring(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "");

   generic
      type Date_Type is new Ada.Calendar.Time;
      type Zone_Type is new APQ_Timezone;
   procedure Append_Timezone(Q : in out Root_Query_Type'Class; V : Date_Type; Z : Zone_Type; After : String := "");

   generic
      with package P is new Ada.Strings.Bounded.Generic_Bounded_Length(<>);
   procedure Append_Bounded(Q : in out Root_Query_Type'Class; SQL : P.Bounded_String; After : String := "");

   generic
      with package P is new Ada.Strings.Bounded.Generic_Bounded_Length(<>);
   procedure Append_Bounded_Quoted(Q : in out Root_Query_Type'Class; Connection : Root_Connection_Type'Class; SQL : P.Bounded_String; After : String := "");

   generic
      type Ind_Type is new Boolean;
   procedure Encode_String_Quoted(Q : in out Root_Query_Type'Class; Connection : Root_Connection_Type'Class; SQL : String; Indicator : Ind_Type; After : String := "");

   generic
      type Ind_Type is new Boolean;
   procedure Encode_Unbounded_Quoted(Q : in out Root_Query_Type'Class; Connection : Root_Connection_Type'Class; SQL : Ada.Strings.Unbounded.Unbounded_String; Indicator : Ind_Type; After : String := "");

   generic
      type Ind_Type is new Boolean;
      with package P is new Ada.Strings.Bounded.Generic_Bounded_Length(<>);
   procedure Encode_Bounded_Quoted(Q : in out Root_Query_Type'Class; Connection : Root_Connection_Type'Class; SQL : P.Bounded_String; Indicator : Ind_Type; After : String := "");

   generic
      type Val_Type is new Boolean;
      type Ind_Type is new Boolean;
   procedure Encode_Boolean(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "");

   generic
      type Val_Type is range <>;
      type Ind_Type is new Boolean;
   procedure Encode_Integer(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "");

   generic
      type Val_Type is mod <>;
      type Ind_Type is new Boolean;
   procedure Encode_Modular(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "");

   generic
      type Val_Type is digits <>;
      type Ind_Type is new Boolean;
   procedure Encode_Float(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "");

   generic
      type Val_Type is delta <>;
      type Ind_Type is new Boolean;
   procedure Encode_Fixed(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "");

   generic
      type Val_Type is delta <> digits <>;
      type Ind_Type is new Boolean;
   procedure Encode_Decimal(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "");

   generic
      type Val_Type is new APQ_Date;
      type Ind_Type is new Boolean;
   procedure Encode_Date(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "");

   generic
      type Val_Type is new APQ_Time;
      type Ind_Type is new Boolean;
   procedure Encode_Time(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "");

   generic
      type Val_Type is new APQ_Timestamp;
      type Ind_Type is new Boolean;
   procedure Encode_Timestamp(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "");

   generic
      type Val_Type is new APQ_Bitstring;
      type Ind_Type is new Boolean;
   procedure Encode_Bitstring(Q : in out Root_Query_Type'Class; V: Val_Type; Indicator : Ind_Type; After : String := "");

   generic
      type Date_Type is new APQ_Timestamp;
      type Zone_Type is new APQ_Timezone;
      type Ind_Type is new Boolean;
   procedure Encode_Timezone(Q : in out Root_Query_Type'Class; D : Date_Type; Z : Zone_Type; Indicator : Ind_Type; After : String := "");


   generic
      type Ind_Type is new Boolean;
   procedure Char_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out String; Indicator : out Ind_Type);

   generic
      type Ind_Type is new Boolean;
   procedure Unbounded_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Ada.Strings.Unbounded.Unbounded_String; Indicator : out Ind_Type);


   generic
      type Ind_Type is new Boolean;
   procedure Varchar_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out String; Last : out Natural; Indicator : out Ind_Type);

   generic
      type Ind_Type is new Boolean;
   procedure Bitstring_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out APQ_Bitstring; Last : out Natural; Indicator : out Ind_Type);

   generic
      type Ind is new Boolean;
      with package P is new Ada.Strings.Bounded.Generic_Bounded_Length(<>);
   procedure Bounded_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out P.Bounded_String; Indicator : out Ind);


   generic
      type Val_Type is new Boolean;
      type Ind_Type is new Boolean;
   procedure Boolean_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type);

   generic
      type Val_Type is range <>;
      type Ind_Type is new Boolean;
   procedure Integer_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type);

   generic
      type Val_Type is mod <>;
      type Ind_Type is new Boolean;
   procedure Modular_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type);

   generic
      type Val_Type is digits <>;
      type Ind_Type is new Boolean;
   procedure Float_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type);

   generic
      type Val_Type is delta <>;
      type Ind_Type is new Boolean;
   procedure Fixed_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type);

   generic
      type Val_Type is delta <> digits <>;
      type Ind_Type is new Boolean;
   procedure Decimal_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type);

   generic
      type Val_Type is new Ada.Calendar.Time;
      type Ind_Type is new Boolean;
   procedure Date_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type);

   generic
      type Val_Type is new Ada.Calendar.Day_Duration;
      type Ind_Type is new Boolean;
   procedure Time_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type);

   generic
      type Val_Type is new Ada.Calendar.Time;
      type Ind_Type is new Boolean;
   procedure Timestamp_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type);

   generic
      type Date_Type is new Ada.Calendar.Time;
      type Zone_Type is new APQ_Timezone;
      type Ind_Type is new Boolean;
   procedure Timezone_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Date_Type; Z : out Zone_Type; Indicator : out Ind_Type);


   generic
      type Val_Type is mod <>;
   function Modular_String(V : Val_Type) return String;

   generic
      type Val_Type is new Boolean;
   function Boolean_String(V : Val_Type) return String;

   generic
      type Val_Type is range <>;
   function Integer_String(V : Val_Type) return String;

   generic
      type Val_Type is digits <>;
   function Float_String(V : Val_Type) return String;

   generic
      type Val_Type is delta <>;
   function Fixed_String(V : Val_Type) return String;

   generic
      type Val_Type is delta <> digits <>;
   function Decimal_String(V : Val_Type) return String;

   ------------------------------
   -- Values
   ------------------------------

   generic
      type Val_Type is new Boolean;
   function Boolean_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type;

   generic
      type Val_Type is range <>;
   function Integer_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type;

   generic
      type Val_Type is mod <>;
   function Modular_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type;

   generic
      type Val_Type is digits <>;
   function Float_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type;

   generic
      type Val_Type is delta <>;
   function Fixed_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type;

   generic
      type Val_Type is delta <> digits <>;
   function Decimal_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type;

   generic
      type Val_Type is new APQ_Date;
   function Date_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type;

   generic
      type Val_Type is new APQ_Time;
   function Time_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type;

   generic
      type Val_Type is new Ada.Calendar.Time;
   function Timestamp_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type;

   generic
      type Date_Type is new Ada.Calendar.Time;
      type Zone_Type is new APQ_Timezone;
   procedure Timezone_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type; TS : out Date_Type; TZ : out Zone_Type);

   generic
      with package P is new Ada.Strings.Bounded.Generic_Bounded_Length(<>);
   function Bounded_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return P.Bounded_String;


   ------------------------------
   -- Conversion :
   ------------------------------

   generic
      type Val_Type is new Ada.Calendar.Time;
   function Date_String(V : Val_Type) return String;

   generic
      type Val_Type is new Ada.Calendar.Day_Duration;
   function Time_String(V : Val_Type) return String;

   generic
      type Val_Type is new Ada.Calendar.Time;
   function Timestamp_String(V : Val_Type) return String;

   generic
      type Val_Type is new APQ_Timezone;
   function Timezone_String(V : Val_Type) return String;

   generic
      type Val_Type is new Boolean;
   function Convert_To_Boolean(S : String) return Val_Type;

   generic
      type Val_Type is new Ada.Calendar.Time;
   function Convert_To_Date(S : String) return Val_Type; -- YYYY-MM-DD format

   generic
      type Val_Type is new Ada.Calendar.Day_Duration;
   function Convert_To_Time(S : String) return Val_Type; -- HH:MM:SS format

   generic
      type Val_Type is new Ada.Calendar.Time;
   function Convert_To_Timestamp(S : String) return Val_Type;  -- YYYY-MM-DD HH:MM:SS format

   generic
      type Date_Type is new Ada.Calendar.Time;
      type Time_Type is new Ada.Calendar.Day_Duration;
      type Result_Type is new Ada.Calendar.Time;
   function Convert_Date_and_Time(DT : Date_Type; TM : Time_Type) return Result_Type;

   ------------------------------
   -- MISC.
   ------------------------------

-- The Generic_Command_Oid causes GNAT 3.14p to fall over and die.
--
-- It isn't really required, since Command_Oid(Query) can be used instead,
-- and the return value converted to whatever Oid_Type is.
--
--   generic
--      type Oid_Type is new Row_ID_Type;
--   function Generic_Command_Oid(Query : Root_Query_Type'Class) return Oid_Type;

   generic
      type Date_Type is new Ada.Calendar.Time;
      type Zone_Type is new APQ_Timezone;
   procedure Extract_Timezone(S : String; DT : out Date_Type; TZ : out Zone_Type);

   ------------------------------
   -- EXTENDED CALENDAR FUNCTIONS :
   ------------------------------

   generic
      type Date_Type is new Ada.Calendar.Time;
      type Time_Type is new Ada.Calendar.Day_Duration;
   function Generic_Time_of_Day(V : Date_Type) return Time_Type;

   generic
      type Time_Type is new Ada.Calendar.Day_Duration;
   function Generic_Hour(TM : Time_Type) return Hour_Number;

   generic
      type Time_Type is new Ada.Calendar.Day_Duration;
   function Generic_Minute(TM : Time_Type) return Minute_Number;

   generic
      type Time_Type is new Ada.Calendar.Day_Duration;
   function Generic_Second(TM : Time_Type) return Second_Number;

   ------------------------------
   -- To_String Functions
   ------------------------------

   function To_String(V : APQ_Boolean) return String;
   function To_String(V : APQ_Date) return String;
   function To_String(V : APQ_Time) return String;

   function To_String(V : APQ_Timestamp) return String;
   function To_String(V : APQ_Timestamp; TZ : APQ_Timezone) return String;
   function To_String(V : APQ_Timezone) return String;

   function To_String(V : APQ_Bitstring) return String;

private

   package CStr renames Interfaces.C_Streams;

   type String_Ptr is access all String;
   type String_Ptr_Array is array(Natural range <>) of String_Ptr;
   type String_Ptr_Array_Access is access all String_Ptr_Array;
   type Stream_Element_Array_Ptr is access all Ada.Streams.Stream_Element_Array;

   subtype Port_Integer is Integer range 0..32768;
   type Port_Format_Type is ( IP_Port, UNIX_Port );

   type Root_Connection_Type is new Ada.Finalization.Limited_Controlled with
      record
         Host_Name :       String_Ptr;                      -- Host name string or..
         Host_Address :    String_Ptr;                      -- Host IP address
         Port_Format :     Port_Format_Type := UNIX_Port;   -- I/O type
         Port_Number :     Port_Integer := 0;               -- Port number of the database server
         Port_Name :       String_Ptr;                      -- UNIX pathname for UNIX socket
         DB_Name :         String_Ptr;                      -- Database name
         User_Name :       String_Ptr;                      -- The user name
         User_Password :   String_Ptr;                      -- User password (if required)
         Abort_State :     Boolean := False;                -- Transaction abort state
         Rollback_Finalize : Boolean := True;               -- Rollback transaction on Finalization
         Trace_Filename :  String_Ptr;                      -- Filename for tracing
         Trace_On :        Boolean := False;                -- True if tracing is enabled
         Trace_Mode :      Trace_Mode_Type := Trace_None;   -- Current Trace mode
         Trace_File :      CStr.FILEs := CStr.Null_Stream;  -- C Stream (FILE *)
         Trace_Ada :       Ada.Text_IO.File_Type;           -- Ada version of Trace_File
      end record;

   procedure Clear_Abort_State(C : in out Root_Connection_Type);

   type Root_Query_Type is new Ada.Finalization.Controlled with
      record
         Count :           Natural := 0;              -- # of elements in the Collection
         Alloc :           Natural := 0;              -- # of allocated elements in the Collection
         Collection :      String_Ptr_Array_Access;   -- Array of strings
         Raise_Exceptions: Boolean := True;           -- Raise exception in Execute_Checked()
         Report_Errors:    Boolean := True;           -- Report SQL error in Execute_Checked()
         Mode :            Fetch_Mode_Type := Random_Fetch;             -- Random Fetches
         Rewound :         Boolean := True;                             -- At first tuple
         Tuple_Index :     Tuple_Index_Type := Tuple_Index_Type'First;  -- Current tuple index
      end record;

   procedure Adjust(Q : in out Root_Query_Type);
   procedure Finalize(Q : in out Root_Query_Type);

   procedure Free is new Ada.Unchecked_Deallocation(String,String_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(Interfaces.C.char_array,Interfaces.C.Strings.char_array_access);
   procedure Free is new Ada.Unchecked_Deallocation(String_Ptr_Array,String_Ptr_Array_Access);
   procedure Free is new Ada.Unchecked_Deallocation(Ada.Streams.Stream_Element_Array,Stream_Element_Array_Ptr);

   procedure Free_Ptr(SP : in out String_Ptr);

   function To_String(S : String_Ptr) return String;
   function To_Ada_String(P : Interfaces.C.Strings.chars_ptr) return String;
   function Blanks_To_Zero(S : String) return String;
   procedure C_String(S : String_Ptr; CP : out Interfaces.C.Strings.char_array_access; Addr : out System.Address);
   function Strip_NL(S : String) return String;
   procedure Replace_String(SP : in out String_Ptr; S : String);

   procedure Grow(Q : in out Root_Query_Type);

   function Value_Of(C_String : Interfaces.C.Strings.chars_ptr) return String;
   function Is_Null(C_String : Interfaces.C.Strings.chars_ptr) return Boolean;

end APQ;
