-- $Id: apq.adb,v 1.2 2005/02/11 02:59:43 ken Exp $
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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

package body APQ is

   type Time_Unit is ( Hour, Minute, Second );


   function Value_Of(C_String : Interfaces.C.Strings.chars_ptr) return String is
      use Interfaces.C.Strings, Interfaces.C;
   begin
      return To_Ada(Value(C_String));
   end Value_Of;

   function Is_Null(C_String : Interfaces.C.Strings.chars_ptr) return Boolean is
      use Interfaces.C.Strings;
   begin
      return C_String = Null_Ptr;
   end Is_Null;

   function To_String(S : String_Ptr) return String is
   begin
      if S /= null then
         return S.all;
      else
         return "";
      end if;
   end To_String;

   function To_Ada_String(P : Interfaces.C.Strings.chars_ptr) return String is
      use Interfaces.C, Interfaces.C.Strings;
   begin
      if P = Null_Ptr then
         return "";
      end if;
      return To_Ada(Value(P));
   end To_Ada_String;

   procedure Free_Ptr(SP : in out String_Ptr) is
   begin
      if SP /= null then
         Free(SP);
      end if;
   end Free_Ptr;

   function Blanks_To_Zero(S : String) return String is
      R : String(S'Range) := S;
   begin
      for X in S'Range loop
         if R(X) = ' ' then
            R(X) := '0';
         end if;
      end loop;
      return R;
   end Blanks_To_Zero;

   procedure C_String(S : String_Ptr; CP : out Interfaces.C.Strings.char_array_access; Addr : out System.Address) is
      use Interfaces.C;
   begin
      if S /= null then
         declare
            CS : char_array := To_C(S.all);
         begin
            CP := new char_array(0..S.all'Length);
            CP.all := CS;
            Addr := CP.all'Address;
         end;
      else
         CP := null;
         Addr := System.Null_Address;
      end if;
   end C_String;

   function Strip_NL(S : String) return String is
      use Ada.Characters.Latin_1;
      NX : Natural := S'Last;
   begin
      for X in S'Range loop
         if S(X) = LF or S(X) = CR then
            return S(S'First..X-1);
         end if;
      end loop;
      return S;
   end Strip_NL;

   procedure Replace_String(SP : in out String_Ptr; S : String) is
   begin
      if SP /= null then
         Free(SP);
      end if;
      if S'Length > 1 then
         SP := new String(1..S'Length);
         SP.all := S;
      end if;
   end Replace_String;

   procedure Set_Host_Name(C : in out Root_Connection_Type; Host_Name : String) is
   begin
      Replace_String(C.Host_Address,"");
      Replace_String(C.Host_Name,Set_Host_Name.Host_Name);
   end Set_Host_Name;
   
   function Host_Name(C : Root_Connection_Type) return String is
   begin
      if C.Host_Name /= null then
         return C.Host_Name.all;
      else
         return To_String(C.Host_Address);
      end if;
   end Host_Name;

   procedure Set_Host_Address(C : in out Root_Connection_Type; Host_Address : String) is
   begin
      Replace_String(C.Host_Name,"");
      Replace_String(C.Host_Address,Set_Host_Address.Host_Address);
   end Set_Host_Address;
   
   procedure Set_Port(C : in out Root_Connection_Type; Port_Number : Integer) is
   begin
      C.Port_Format := IP_Port;
      C.Port_Number := Set_Port.Port_Number;
   end Set_Port;
   
   function Port(C : Root_Connection_Type) return Integer is
   begin
      case C.Port_Format is
         when IP_Port =>
            return C.Port_Number;
         when UNIX_Port =>
            raise Invalid_Format;      -- We have a UNIX socket type instead
      end case;
   end Port;

   procedure Set_Port(C : in out Root_Connection_Type; Port_Name : String) is
   begin
      C.Port_Format     := UNIX_Port;
      C.Port_Name       := new String(1..Port_Name'Length);
      C.Port_Name.all   := Set_Port.Port_Name;
   end Set_Port;

   function Port(C : Root_Connection_Type) return String is
   begin
      case C.Port_Format is
         when IP_Port =>
            raise Invalid_Format;
         when UNIX_Port =>
            return To_String(C.Port_Name);
      end case;
   end Port;

   procedure Set_DB_Name(C : in out Root_Connection_Type; DB_Name : String) is
   begin
      Replace_String(C.DB_Name,Set_DB_Name.DB_Name);
   end Set_DB_Name;

   function DB_Name(C : Root_Connection_Type) return String is
   begin
      return To_String(C.DB_Name);
   end DB_Name;
   
   procedure Set_User_Password(C : in out Root_Connection_Type; User_Name, User_Password : String) is
   begin
      Replace_String(C.User_Name,Set_User_Password.User_Name);
      Replace_String(C.User_Password,Set_User_Password.User_Password);
   end Set_User_Password;
   
   function User(C : Root_Connection_Type) return String is
   begin
      return To_String(C.User_Name);
   end User;
   
   function Password(C : Root_Connection_Type) return String is
   begin
      return To_String(C.User_Password);
   end Password;

   -- ABSTRACT PRIMITIVES

   procedure Connect(C : in out Root_Connection_Type) is
   begin
      raise Is_Abstract;
   end Connect;

   procedure Connect(C : in out Root_Connection_Type; Same_As : Root_Connection_Type'Class) is
   begin
      raise Is_Abstract;
   end Connect;
   
   procedure Disconnect(C : in out Root_Connection_Type) is
   begin
      raise Is_Abstract;
   end Disconnect;

   function Is_Connected(C : Root_Connection_Type) return Boolean is
   begin
      raise Is_Abstract;
      return False;
   end Is_Connected;
   
   procedure Reset(C : in out Root_Connection_Type) is
   begin
      raise Is_Abstract;
   end Reset;
   
   function Error_Message(C : Root_Connection_Type) return String is
   begin
      raise Is_Abstract;
      return "";
   end Error_Message;

   procedure Clear(Q : in out Root_Query_Type) is
   begin
      for X in 1..Q.Count loop
         Free_Ptr(Q.Collection(X));
      end loop;
      Free(Q.Collection);
      Q.Count := 0;
      Q.Tuple_Index := Tuple_Index_Type'First;
      Q.Rewound := True;
   end Clear;

   function Fetch_Mode(Q : Root_Query_Type) return Fetch_Mode_Type is
   begin
      return Q.Mode;
   end Fetch_Mode;

   procedure Set_Fetch_Mode(Q : in out Root_Query_Type; Mode : Fetch_Mode_Type) is
   begin
      Q.Mode := Mode;
   end Set_Fetch_Mode;

   procedure Grow(Q : in out Root_Query_Type) is
   begin
      if Q.Count <= 0 then
         Q.Alloc := 64;
         Q.Collection := new String_Ptr_Array(1..Q.Alloc);
      elsif Q.Count >= Q.Alloc then
         declare
            New_Alloc : Natural := Q.Alloc + 128;
            New_Array : String_Ptr_Array_Access := new String_Ptr_Array(1..New_Alloc);
         begin
            New_Array(1..Q.Alloc) := Q.Collection.all;
            Free(Q.Collection);
            Q.Alloc := New_Alloc;
            Q.Collection := New_Array;
         end;
      end if;
   end Grow;

   procedure Prepare(Q : in out Root_Query_Type; SQL : String; After : String := Line_Feed) is
   begin
      Clear(Root_Query_Type'Class(Q));
      Append(Root_Query_Type'Class(Q),SQL,After);
   end Prepare;

   procedure Append(Q : in out Root_Query_Type; SQL : String; After : String := "") is
      use Ada.Characters.Latin_1;
      NSL : Natural := SQL'Length + After'Length;
   begin
      Grow(Q);
      Q.Count := Q.Count + 1;
      Q.Collection(Q.Count) := new String(1..NSL);
      Q.Collection(Q.Count).all(1..SQL'Length) := SQL;
      Q.Collection(Q.Count).all(SQL'Length+1..NSL) := After;
   end Append;

   procedure Append(Q : in out Root_Query_Type; SQL : Ada.Strings.Unbounded.Unbounded_String; After : String := "") is
      use Ada.Characters.Latin_1, Ada.Strings.Unbounded;
      Len : Natural := Length(SQL);
      NSL : Natural := Len + After'Length;
   begin
      Grow(Q);
      Q.Count := Q.Count + 1;
      Q.Collection(Q.Count) := new String(1..NSL);
      Q.Collection(Q.Count).all(1..Len) := To_String(SQL);
      Q.Collection(Q.Count).all(Len+1..NSL) := After;
   end Append;

   procedure Append_Line(Q : in out Root_Query_Type; SQL : String := "") is
      New_Line : String(1..1);
   begin
      New_Line(1) := Ada.Characters.Latin_1.LF;

      Append(Root_Query_Type'Class(Q),SQL,New_Line);
   end Append_Line;

   -- This primitive should normally be overriden for a specific database.
   -- PostgreSQL and MySQL will potentially have different quoting requirements.

   procedure Append_Quoted(Q : in out Root_Query_Type; Connection : Root_Connection_Type'Class; SQL : String; After : String := "") is
   begin
      Append(Root_Query_Type'Class(Q),"'" & SQL & "'",After);    
   end Append_Quoted;

   procedure Append_Quoted(Q : in out Root_Query_Type; Connection : Root_Connection_Type'Class; SQL : Ada.Strings.Unbounded.Unbounded_String; After : String := "") is
   begin
      Append_Quoted(Root_Query_Type'Class(Q),Connection,Ada.Strings.Unbounded.To_String(SQL),After);
   end Append_Quoted;

   procedure Finalize(Q : in out Root_Query_Type) is
   begin
      raise Is_Abstract;
   end Finalize;

   procedure Adjust(Q : in out Root_Query_Type) is
   begin
      Q.Count := 0;
      Q.Alloc := 0;
      Q.Collection := null;
      Q.Tuple_Index := Tuple_Index_Type'First;
   end Adjust;

   function To_String(Query : Root_Query_Type) return String is
      use Ada.Characters.Latin_1;
      Total_Length : Natural := 0;
      Append_NL    : Boolean := False;
   begin

      for X in 1..Query.Count loop
         Total_Length := Total_Length + Query.Collection(X).all'Length;
      end loop;

      if Total_Length <= 0 then
         return "";        -- No query started
      end if;

      Append_NL := Query.Collection(Query.Count).all(Query.Collection(Query.Count).all'Last) /= LF;
      if Append_NL then
         Total_Length := Total_Length + 1;
      end if;

      declare
         Return_String :   String(1..Total_Length);
         RX :              Positive := Return_String'First;
         EX :              Positive;
      begin
         for X in 1..Query.Count loop
            EX := RX + Query.Collection(X).all'Length - 1;
            Return_String(RX..EX) := Query.Collection(X).all;
            RX := EX + 1;
         end loop;
         if Append_NL then
            Return_String(Return_String'Last) := LF;
         end if;
         return Return_String;
      end;
   end To_String;

   procedure Rewind(Q : in out Root_Query_Type) is
   begin
      raise Is_Abstract;
   end Rewind;

   procedure Fetch(Q : in out Root_Query_Type) is
   begin
      raise Is_Abstract;
   end Fetch;

   procedure Fetch(Q : in out Root_Query_Type; TX : Tuple_Index_Type) is
   begin
      raise Is_Abstract;
   end Fetch;

   function End_of_Query(Q : Root_Query_Type) return Boolean is
   begin
      raise Is_Abstract;
      return False;
   end End_of_Query;

   function Tuple(Q : Root_Query_Type) return Tuple_Index_Type is
   begin
      raise Is_Abstract;
      return Tuple_Index_Type'First;
   end Tuple;

   function Tuples(Q : Root_Query_Type) return Tuple_Count_Type is
   begin
      raise Is_Abstract;
      return Tuple_Count_Type'First;
   end Tuples;

   function Value(Query : Root_Query_Type; CX : Column_Index_Type) return String is
   begin
      raise Is_Abstract;
      return ":-)";
   end Value;

   function Value(Query : Root_Query_Type; CX : Column_Index_Type) return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;
   begin
      return To_Unbounded_String(Value(Root_Query_Type'Class(Query),CX));
   end Value;

   procedure Raise_Exceptions(Query : in out Root_Query_Type; Raise_On : Boolean := True) is
   begin
      Query.Raise_Exceptions := Raise_On;
   end Raise_Exceptions;

   procedure Report_Errors(Query : in out Root_Query_Type; Report_On : Boolean := True) is
   begin
      Query.Report_Errors := Report_On;
   end Report_Errors;

   function In_Abort_State(C : Root_Connection_Type) return Boolean is
   begin
      return C.Abort_State;
   end In_Abort_State;

   procedure Clear_Abort_State(C : in out Root_Connection_Type) is
   begin
      C.Abort_State := False;
   end Clear_Abort_State;

   procedure Set_Rollback_On_Finalize(C : in out Root_Connection_Type; Rollback : Boolean) is
   begin
      C.Rollback_Finalize := Rollback;
   end Set_Rollback_On_Finalize;

   function Will_Rollback_On_Finalize(C : Root_Connection_Type) return Boolean is
   begin
      return C.Rollback_Finalize;
   end Will_Rollback_On_Finalize;

   function Result(Query : Root_Query_Type) return Natural is
   begin
      raise Is_Abstract;         -- This primitive must be overridden by the implementation
      return 0;                  -- This is just to satisfy the compiler (not executed)
   end Result;

   function Engine_Of(C : Root_Connection_Type) return Database_Type is
   begin
      raise Is_Abstract;            -- Must be overridden
      return Database_Type'First;   -- To quiet the compiler
   end Engine_Of;

   function Engine_Of(Q : Root_Query_Type) return Database_Type is
   begin
      raise Is_Abstract;            -- Must be overridden
      return Database_Type'First;   -- To quiet the compiler
   end Engine_Of;

   function Command_Oid(Query : Root_Query_Type) return Row_ID_Type is
   begin
      raise Is_Abstract;
      return Row_ID_Type'First;
   end Command_Oid;

   function Null_Oid(Query : Root_Query_Type) return Row_ID_Type is
   begin
      raise Is_Abstract;
      return Row_ID_Type'First;
   end Null_Oid;

   function Error_Message(Query : Root_Query_Type) return String is
   begin
      raise Is_Abstract;
      return "";
   end Error_Message;

   function Is_Duplicate_Key(Query : Root_Query_Type) return Boolean is
   begin
      raise Is_Abstract;
      return False;
   end Is_Duplicate_Key;

   procedure Execute(Query : in out Root_Query_Type; Connection : in out Root_Connection_Type'Class) is
   begin
      raise Is_Abstract;
   end Execute;

   procedure Execute_Checked(Query : in out Root_Query_Type; Connection : in out Root_Connection_Type'Class; Msg : String := "") is
   begin
      raise Is_Abstract;
   end Execute_Checked;

   procedure Begin_Work(Query : in out Root_Query_Type; Connection : in out Root_Connection_Type'Class) is
   begin
      raise Is_Abstract;
   end Begin_Work;

   procedure Commit_Work(Query : in out Root_Query_Type; Connection : in out Root_Connection_Type'Class) is
   begin
      raise Is_Abstract;
   end Commit_Work;

   procedure Rollback_Work(Query : in out Root_Query_Type; Connection : in out Root_Connection_Type'Class) is
   begin
      raise Is_Abstract;
   end Rollback_Work;

   function Time_Component(TM : Ada.Calendar.Day_Duration; Unit : Time_Unit) return Natural is
   begin
      case Unit is
         when Hour =>
            return Natural(TM) / 3600;
         when Minute =>
            declare
               M3600 : Natural := Natural(TM) mod 3600;
            begin
               return M3600 / 60;
            end;
         when Second =>
            return Natural(TM) mod 60;
      end case;
   end Time_Component;

   function Time_Component(TM : Ada.Calendar.Time; Unit : Time_Unit) return Natural is
      use Ada.Calendar;
      Year :      Year_Number;
      Month :     Month_Number;
      Day :       Day_Number;
      Seconds :   Day_Duration;
   begin
      Split(TM,Year,Month,Day,Seconds);
      return Time_Component(Seconds,Unit);
   end Time_Component;
      
   -- This function split out to avoid GNAT 3.13p compiler bug
   function Internal_Time_of_Day(DT : Ada.Calendar.Time) return Ada.Calendar.Day_Duration is
      use Ada.Calendar;
      Year :      Year_Number;
      Month :     Month_Number;
      Day :       Day_Number;
      Seconds :   Day_Duration;
   begin
      Split(DT,Year,Month,Day,Seconds);
      return Seconds;
   end Internal_Time_of_Day;

   function Generic_Time_of_Day(V : Date_Type) return Time_Type is
   begin
      return Time_Type(Internal_Time_of_Day(Ada.Calendar.Time(V)));
   end Generic_Time_of_Day;

   function Generic_Hour(TM : Time_Type) return Hour_Number is
   begin
      return Hour_Number(Time_Component(Ada.Calendar.Day_Duration(TM),Hour));
   end Generic_Hour;

   function Generic_Minute(TM : Time_Type) return Minute_Number is
   begin
      return Minute_Number(Time_Component(Ada.Calendar.Day_Duration(TM),Minute));
   end Generic_Minute;

   function Generic_Second(TM : Time_Type) return Second_Number is
   begin
      return Second_Number(Time_Component(Ada.Calendar.Day_Duration(TM),Second));
   end Generic_Second;

   function To_String(V : APQ_Boolean) return String is
      TF :  String(1..5) := "FALSE";
      L  :  Positive := 5;
   begin
      if V then
         TF := "TRUE ";
         L := 4;
      end if;
      return TF(1..L);
   end To_String;
   
   function To_String(V : APQ_Date) return String is
      use Ada.Calendar;
      package INTIO2 is new Ada.Text_IO.Integer_IO(Integer);
      YY :           Integer        := Integer(Year(V));
      MM :           Integer        := Integer(Month(V));
      DD :           Integer        := Integer(Day(V));
      YYYY_MM_DD :   String(1..10)  := "YYYY-MM-DD";
   begin
      INTIO2.Put(To => YYYY_MM_DD(1..4), Item => YY, Base => 10);
      INTIO2.Put(To => YYYY_MM_DD(6..7), Item => MM, Base => 10);
      INTIO2.Put(To => YYYY_MM_DD(9..10), Item => DD, Base => 10);
      return Blanks_To_Zero(YYYY_MM_DD);
   end To_String;

   function To_String(V : APQ_Time) return String is
      use Ada.Calendar;
      package INTIO3 is new Ada.Text_IO.Integer_IO(Integer);
      function Hour is new Generic_Hour(APQ_Time);
      function Minute is new Generic_Minute(APQ_Time);
      function Second is new Generic_Second(APQ_Time);
      HH :        Integer        := Integer(Hour(V));
      MM :        Integer        := Integer(Minute(V));
      SS :        Integer        := Integer(Second(V));
      HH_MM_SS :  String(1..8)   := "HH:MM:SS";
   begin
      INTIO3.Put(To => HH_MM_SS(1..2), Item => HH, Base => 10);
      INTIO3.Put(To => HH_MM_SS(4..5), Item => MM, Base => 10);
      INTIO3.Put(To => HH_MM_SS(7..8), Item => SS, Base => 10);
      return Blanks_To_Zero(HH_MM_SS);
   end To_String;

   function To_String(V : APQ_Timestamp) return String is
      function Time_of_Day is new Generic_Time_of_Day(APQ_Timestamp,APQ_Time);
      DS : String := To_String(APQ_Date(V));
      ST : String := To_String(Time_of_Day(V));
   begin
      return DS & " " & ST;
   end To_String;

   function To_String(V : APQ_Timezone) return String is
      package ZONEIO is new Ada.Text_IO.Integer_IO(APQ_Timezone);
      ZS : String(1..3);
   begin
      ZONEIO.Put(To => ZS, Item => V, Base => 10);
      if ZS(1) = ' ' then
         ZS(1) := ZS(2);
         ZS(2) := '0';
      end if;
      return ZS;
   end To_String;

   function To_String(V : APQ_Timestamp; TZ : APQ_Timezone) return String is
      ST : String := To_String(V);
      ZS : String := To_String(TZ);
   begin
      return ST & ZS;
   end To_String;

   function To_String(V : APQ_Bitstring) return String is
      S : String(V'Range);
   begin
      for X in V'Range loop
         if V(X) then
            S(X) := '1';
         else
            S(X) := '0';
         end if;
      end loop;
      return S;
   end To_String;

   function Boolean_String(V : Val_Type) return String is
   begin
      return To_String(APQ_Boolean(V));
   end Boolean_String;

   function Modular_String(V : Val_Type) return String is
      use Ada.Strings.Fixed, Ada.Strings;
      package MODIO is new Ada.Text_IO.Modular_IO(Val_Type);
      S : String(1..40);
   begin
      MODIO.Put(To => S, Item => V, Base => 10);
      return Trim(S,Both);
   end Modular_String;

   function Integer_String(V : Val_Type) return String is
      use Ada.Strings.Fixed, Ada.Strings;
      package INTIO1 is new Ada.Text_IO.Integer_IO(Val_Type);
      S : String(1..40);
   begin
      INTIO1.Put(To => S, Item => V, Base => 10);
      return Trim(S,Both);
   end Integer_String;

   function Float_String(V : Val_Type) return String is
      use Ada.Strings.Fixed, Ada.Strings;
      package FLTIO is new Ada.Text_IO.Float_IO(Val_Type);
      S : String(1..50);
   begin
      FLTIO.Put(To => S, Item => V, Exp => 3);
      return Trim(S,Both);
   end Float_String;

   function Fixed_String(V : Val_Type) return String is
      use Ada.Strings.Fixed, Ada.Strings;
      package FXTIO is new Ada.Text_IO.Fixed_IO(Val_Type);
      S : String(1..50);
   begin
      FXTIO.Put(To => S, Item => V, Exp => 3);
      return Trim(S,Both);
   end Fixed_String;
   
   function Date_String(V : Val_Type) return String is
   begin
      return To_String(APQ_Date(V));
   end Date_String;

   function Time_String(V : Val_Type) return String is
   begin
      return To_String(APQ_Time(V));
   end Time_String;

   function Timestamp_String(V : Val_Type) return String is
   begin
      return To_String(APQ_Timestamp(V));
   end Timestamp_String;

   function Timezone_String(V : Val_Type) return String is
   begin
      return To_String(APQ_Timezone(V));
   end Timezone_String;

   -- S must be YYYY-MM-DD format
   function Convert_To_Date(S : String) return Val_Type is
      use Ada.Strings, Ada.Strings.Fixed, Ada.Calendar;
      T : String := Trim(S,Both);
      Hyphen_X1 :    Positive := T'Last + 1;
      Hyphen_X2 :    Positive := T'Last + 1;
      Both_Found :   Boolean := False;
   begin

      for X in T'Range loop
         if T(X) = '-' or T(X) = '/' then
            Hyphen_X1 := X;
            if X < T'Last then
               for Y in X+1..T'Last loop
                  if T(Y) = '-' or T(Y) = '/' then
                     Hyphen_X2 := Y;
                     Both_Found := True;
                  end if;
               end loop;
            end if;
            exit;
         end if;
      end loop;

      if not Both_Found then
         raise Invalid_Format;
      end if;

      begin
         declare
            Year :   Year_Number    := Year_Number'Value(T(1..Hyphen_X1-1));
            Month :  Month_Number   := Month_Number'Value(T(Hyphen_X1+1..Hyphen_X2-1));
            Day :    Day_Number     := Day_Number'Value(T(Hyphen_X2+1..T'Last));
            R :      Ada.Calendar.Time := Ada.Calendar.Time_Of(Year,Month,Day);
         begin
            return Val_Type(R);
         end;
      exception
         when others =>
            raise Invalid_Format;
      end;
   end Convert_To_Date;

   function Internal_Date_and_Time(DT : Ada.Calendar.Time; TM : Ada.Calendar.Day_Duration) return Ada.Calendar.Time is
      use Ada.Calendar;
      Year :      Year_Number;
      Month :     Month_Number;
      Day :       Day_Number;
      Second :    Day_Duration;
   begin
      Split(DT,Year,Month,Day,Second);
      Second := Day_Duration(TM);
      return Time_Of(Year,Month,Day,Second);
   end Internal_Date_and_Time;

   function Convert_Date_and_Time(DT : Date_Type; TM : Time_Type) return Result_Type is
      use Ada.Calendar;
   begin
      -- Internal_Date_and_Time() function necessary to avoid 3.13p compiler bug
      return Result_Type( Internal_Date_and_Time(Time(DT),Day_Duration(TM)) );
   end Convert_Date_and_Time;

   -- S must be HH:MM:SS format
   function Convert_To_Time(S : String) return Val_Type is
      use Ada.Strings, Ada.Strings.Fixed, Ada.Calendar;
      T : String := Trim(S,Both);
      Colon_X1 :     Positive;
      Colon_X2 :     Positive;
      Colon_1F :     Boolean := False;
      Colon_2F :     Boolean := False;
   begin

      for X in T'Range loop
         if T(X) = ':' then
            Colon_X1 := X;
            Colon_1F := True;
            if X < T'Last then
               for Y in X+1..T'Last loop
                  if T(Y) = ':' then
                     Colon_X2 := Y;
                     Colon_2F := True;
                  end if;
               end loop;
            end if;
            exit;
         end if;
      end loop;

      if not Colon_1F then
         raise Invalid_Format;
      end if;

      if not Colon_2F then
         Colon_X2 := T'Last + 1;
      end if;

      begin
         declare
            Hour :   Natural        := Natural'Value(T(1..Colon_X1-1));
            Minute : Natural        := Natural'Value(T(Colon_X1+1..Colon_X2-1));
            Second : Natural        := 0;
         begin
            if Colon_2F then
               Second := Natural'Value(T(Colon_X2+1..T'Last));
            end if;

            return Val_Type( Hour * 60 * 60 + Minute * 60 + Second );
         end;
      exception
         when others =>
            raise Invalid_Format;
      end;
   end Convert_To_Time;

   -- S must be YYYY-MM-DD HH:MM:SS format
   function Convert_To_Timestamp(S : String) return Val_Type is
      use Ada.Strings, Ada.Strings.Fixed;
      function To_Date is new Convert_To_Date(Val_Type);
      T :   String := Trim(S,Both);
      BX :  Positive := T'Last + 1;
      BF :  Boolean := False;
   begin

      for X in T'Range loop
         if T(X) = ' ' then
            BF := True;
            BX := X;
            exit;
         end if;
      end loop;

      if not BF then
         return To_Date(T);
      else
         declare
            TZX :    Positive := T'Last + 1;       -- Location of the Time zone sign character
         begin
            for X in reverse T'Range loop
               if T(X) = '-' or T(X) = '+' then
                  if T(X..T'Last)'Length <= 3 then
                     TZX := X;
                  end if;
               end if;
            end loop;

            declare
               function To_Date is new Convert_To_Date(APQ_Date);
               function To_Time is new Convert_To_Time(APQ_Time);
               function To_Val_Type is new Convert_Date_and_Time(APQ_Date,APQ_Time,Val_Type);
               DT :  APQ_Date;
               TM :  APQ_Time;
            begin
               DT := To_Date(T(1..BX-1));
               TM := To_Time(T(BX+1..TZX-1));
               return To_Val_Type(DT,TM);
            end;
         end;
      end if;

   end Convert_To_Timestamp;

   procedure Append(Q : in out Root_Query_Type; V : APQ_Boolean; After : String := "") is
   begin
      Append(Root_Query_Type'Class(Q),To_String(V),After);
   end Append;

   procedure Append(Q : in out Root_Query_Type; V : Row_ID_Type; After : String := "") is
      function To_String is new Modular_String(Row_ID_Type);
   begin
      Append(Root_Query_Type'Class(Q),To_String(V),After);
   end Append;
                     
   procedure Append(Q : in out Root_Query_Type; V : APQ_Date; After : String := "") is
      use Ada.Calendar;
      S : String := To_String(V);
   begin
      Append(Root_Query_Type'Class(Q),"'",S);
      Append(Root_Query_Type'Class(Q),"'",After);
   end Append;

   procedure Append(Q : in out Root_Query_Type; V : APQ_Time; After : String := "") is
      use Ada.Calendar;
      S : String := To_String(V);
   begin
      Append(Root_Query_Type'Class(Q),"'",S);
      Append(Root_Query_Type'Class(Q),"'",After);
   end Append;

   procedure Append(Q : in out Root_Query_Type; V : APQ_Timestamp; After : String := "") is
      use Ada.Calendar;
      D : String := To_String(V);
   begin
      Append(Root_Query_Type'Class(Q),"'",D);
      Append(Root_Query_Type'Class(Q),"'",After);
   end Append;

   procedure Append(Q : in out Root_Query_Type; TS : APQ_Timestamp; TZ : APQ_Timezone; After : String := "") is
      use Ada.Calendar, Ada.Strings, Ada.Strings.Fixed;
      D : String := To_String(TS);
      Z : String := APQ_Timezone'Image(TZ);
   begin
      Append(Root_Query_Type'Class(Q),"'",D);
      Append(Root_Query_Type'Class(Q),Trim(Z,Left),"'");
      if After'Length > 0 then
         Append(Root_Query_Type'Class(Q),After);
      end if;
   end Append;

   procedure Append_Timestamp(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "") is
      function To_String is new Timestamp_String(Val_Type);
   begin
      Append(Root_Query_Type'Class(Q),"'",To_String(V));
      Append(Root_Query_Type'Class(Q),"'",After);
   end Append_Timestamp;

   procedure Append_Timezone(Q : in out Root_Query_Type'Class; V : Date_Type; Z : Zone_Type; After : String := "") is
      function To_String is new Timestamp_String(Date_Type);
      function To_String is new Timezone_String(Zone_Type);
   begin
      Append(Root_Query_Type'Class(Q),"'",To_String(V));
      Append(Root_Query_Type'Class(Q),To_String(Z),"'");
      if After'Length > 0 then
         Append(Root_Query_Type'Class(Q),After);
      end if;
   end Append_Timezone;

   procedure Append(Q : in out Root_Query_Type; V : APQ_Bitstring; After : String := "") is
      S : String := To_String(V);
   begin
      Append(Root_Query_Type'Class(Q),"B'",S);
      Append(Root_Query_Type'Class(Q),"'",After);
   end Append;

   procedure Append_Bitstring(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "") is
   begin
      Append(Root_Query_Type'Class(Q),To_String(APQ_Bitstring(V)),After);
   end Append_Bitstring;

   procedure Append_Integer(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "") is
      function To_String is new Integer_String(Val_Type);
      S : String := To_String(V);
   begin
      Append(Root_Query_Type'Class(Q),S,After);
   end Append_Integer;

   procedure Append_Modular(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "") is
      function To_String is new Modular_String(Val_Type);
      S : String := To_String(V);
   begin
      Append(Root_Query_Type'Class(Q),S,After);
   end Append_Modular;

   procedure Append_Float(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "") is
      function To_String is new Float_String(Val_Type);
   begin
      Append(Root_Query_Type'Class(Q),To_String(V),After);
   end Append_Float;

   procedure Append_Date(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "") is
      function To_String is new Date_String(Val_Type);
   begin
      Append(Root_Query_Type'Class(Q),"'",To_String(V));
      Append(Root_Query_Type'Class(Q),"'",After);
   end Append_Date;

   procedure Append_Time(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "") is
      function To_String is new Time_String(Val_Type);
   begin
      Append(Root_Query_Type'Class(Q),"'",To_String(V));
      Append(Root_Query_Type'Class(Q),"'",After);
   end Append_Time;

   procedure Append_Fixed(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "") is
      function To_String is new APQ.Fixed_String(Val_Type);
   begin
      Append(Root_Query_Type'Class(Q),To_String(V),After);
   end Append_Fixed;

   function Decimal_String(V : Val_Type) return String is
      use Ada.Strings.Fixed, Ada.Strings;
      package DECIO is new Ada.Text_IO.Decimal_IO(Val_Type);
      S : String(1..50);
   begin
      DECIO.Put(To => S, Item => V);
      return Trim(S,Both);
   end Decimal_String;   

   procedure Append_Decimal(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "") is
      function To_String is new Decimal_String(Val_Type);
   begin
      Append(Root_Query_Type'Class(Q),To_String(V),After);
   end Append_Decimal;

   procedure Append_Boolean(Q : in out Root_Query_Type'Class; V : Val_Type; After : String := "") is
      function To_String is new Boolean_String(Val_Type);
   begin
      Append(Root_Query_Type'Class(Q),To_String(V),After);
   end Append_Boolean;

   procedure Append_Bounded_Quoted(Q : in out Root_Query_Type'Class; Connection : Root_Connection_Type'Class; SQL : P.Bounded_String; After : String := "") is
   begin
      Append_Quoted(Root_Query_Type'Class(Q),Connection,P.To_String(SQL),After);
   end Append_Bounded_Quoted;

   procedure Append_Bounded(Q : in out Root_Query_Type'Class; SQL : P.Bounded_String; After : String := "") is
   begin
      Append(Root_Query_Type'Class(Q),P.To_String(SQL),After);
   end Append_Bounded;

   function Convert_To_Boolean(S : String) return Val_Type is
      use Ada.Characters.Handling, Ada.Strings, Ada.Strings.Fixed;
      UC : String := To_Upper(Trim(S,Both));
   begin
      if UC = "FALSE" then
         return False;
      elsif UC = "TRUE" then
         return True;
      end if;
      if UC'Length = 1 then
         if UC = "T" then
            return True;
         elsif UC = "F" then
            return False;
         end if;
      end if;
      raise Invalid_Format;
   end Convert_To_Boolean;

   procedure Encode_String_Quoted(Q : in out Root_Query_Type'Class; Connection : Root_Connection_Type'Class; SQL : String; Indicator : Ind_Type; After : String := "") is
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append_Quoted(Root_Query_Type'Class(Q),Connection,SQL,After);
      end if;
   end Encode_String_Quoted;

   procedure Encode_Unbounded_Quoted(Q : in out Root_Query_Type'Class; Connection : Root_Connection_Type'Class; SQL : Ada.Strings.Unbounded.Unbounded_String; Indicator : Ind_Type; After : String := "") is
      use Ada.Strings.Unbounded;
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append_Quoted(Root_Query_Type'Class(Q),Connection,To_String(SQL),After);
      end if;
   end Encode_Unbounded_Quoted;

   procedure Encode_Bounded_Quoted(Q : in out Root_Query_Type'Class; Connection : Root_Connection_Type'Class; SQL : P.Bounded_String; Indicator : Ind_Type; After : String := "") is
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append_Quoted(Root_Query_Type'Class(Q),Connection,P.To_String(SQL),After);
      end if;
   end Encode_Bounded_Quoted;

   procedure Encode_Integer(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "") is
      procedure Append is new Append_Integer(Val_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append(Root_Query_Type'Class(Q),V,After);
      end if;
   end Encode_Integer;

   procedure Encode_Modular(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "") is
      procedure Append is new Append_Modular(Val_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append(Root_Query_Type'Class(Q),V,After);
      end if;
   end Encode_Modular;

   procedure Encode_Float(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "") is
      procedure Append is new Append_Float(Val_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append(Root_Query_Type'Class(Q),V,After);
      end if;
   end Encode_Float;

   procedure Encode_Fixed(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "") is
      procedure Append is new Append_Fixed(Val_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append(Root_Query_Type'Class(Q),V,After);
      end if;
   end Encode_Fixed;

   procedure Encode_Decimal(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "") is
      procedure Append is new Append_Decimal(Val_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append(Root_Query_Type'Class(Q),V,After);
      end if;
   end Encode_Decimal;

   procedure Encode_Boolean(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "") is
      procedure Append is new Append_Boolean(Val_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append(Root_Query_Type'Class(Q),V,After);
      end if;
   end Encode_Boolean;

   procedure Encode_Date(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "") is
      procedure Append is new Append_Date(Val_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append(Root_Query_Type'Class(Q),V,After);
      end if;
   end Encode_Date;

   procedure Encode_Time(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "") is
      procedure Append is new Append_Time(Val_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append(Root_Query_Type'Class(Q),V,After);
      end if;
   end Encode_Time;
      
   procedure Encode_Timestamp(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "") is
      procedure App is new Append_Timestamp(Val_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         App(Root_Query_Type'Class(Q),V,After);
      end if;
   end Encode_Timestamp;

   procedure Encode_Timezone(Q : in out Root_Query_Type'Class; D : Date_Type; Z : Zone_Type; Indicator : Ind_Type; After : String := "") is
      procedure Append is new Append_Timezone(Date_Type,Zone_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         Append(Root_Query_Type'Class(Q),D,Z,After);
      end if;
   end Encode_Timezone;

   procedure Encode_Bitstring(Q : in out Root_Query_Type'Class; V : Val_Type; Indicator : Ind_Type; After : String := "") is
      procedure App is new Append_Bitstring(Val_Type);
   begin
      if Indicator then
         Append(Root_Query_Type'Class(Q),"NULL",After);
      else
         App(Root_Query_Type'Class(Q),V,After);
      end if;
   end Encode_Bitstring;

   function Is_Null(Q : Root_Query_Type; CX : Column_Index_Type) return Boolean is
   begin
      raise Is_Abstract;      -- Must be overriden
      return False;
   end Is_Null;

   function Column_Is_Null(Q : Root_Query_Type'Class; CX : Column_Index_Type) return Ind_Type is
   begin
      return Ind_Type(Is_Null(Root_Query_Type'Class(Q),CX));
   end Column_Is_Null;

   function Value(Query : Root_Query_Type; CX : Column_Index_Type) return Row_ID_Type is
      S : String := Value(Root_Query_Type'Class(Query),CX);
   begin
      return Row_ID_Type'Value(S);
   end Value;

   function Value(Query : Root_Query_Type; CX : Column_Index_Type) return APQ_Bitstring is
      use Ada.Strings, Ada.Strings.Fixed;
      S : String := Trim(Value(Root_Query_Type'Class(Query),CX),Both);
      R : APQ_Bitstring(1..S'Length);
   begin
      for X in S'Range loop
         R(X) := S(X) /= '0';
      end loop;
      return R;
   end Value;

   -- Fixed length String Fetch
   procedure Value(Query: Root_Query_Type; CX : Column_Index_Type; V : out String) is
      S : String := Value(Root_Query_Type'Class(Query),CX);
   begin
      if S'Length = V'Length then
         V := S;
      elsif S'Length > V'Length then
         raise Small_Buffer;
      else
         V(V'First..S'Length) := S;
         V(S'Length+1..V'Last) := ( others => ' ' );
      end if;
   end Value;

   function Boolean_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type is
      function To_Boolean is new Convert_To_Boolean(Val_Type);
   begin
      return To_Boolean(Value(Root_Query_Type'Class(Query),CX));
   end Boolean_Value;

   function Integer_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type is
      S : String := Value(Root_Query_Type'Class(Query),CX);
   begin
      return Val_Type'Value(S);
   end Integer_Value;

   function Modular_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type is
      S : String := Value(Root_Query_Type'Class(Query),CX);
   begin
      return Val_Type'Value(S);
   end Modular_Value;

   function Float_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type is
      S : String := Value(Root_Query_Type'Class(Query),CX);
   begin
      return Val_Type'Value(S);
   end Float_Value;

   function Fixed_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type is
      S : String := Value(Root_Query_Type'Class(Query),CX);
   begin
      return Val_Type'Value(S);
   end Fixed_Value;

   function Decimal_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type is
      S : String := Value(Root_Query_Type'Class(Query),CX);
   begin
      return Val_Type'Value(S);
   end Decimal_Value;

   function Date_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type is
      function To_Date is new Convert_To_Date(Val_Type);
   begin
      return To_Date(Value(Root_Query_Type'Class(Query),CX));
   end Date_Value;

   function Time_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type is
      function To_Time is new Convert_To_Time(Val_Type);
   begin
      return To_Time(Value(Root_Query_Type'Class(Query),CX));
   end Time_Value;

   function Timestamp_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return Val_Type is
      function To_Timestamp is new Convert_To_Timestamp(Val_Type);
   begin
      return To_Timestamp(Value(Root_Query_Type'Class(Query),CX));
   end Timestamp_Value;

   procedure Timezone_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type; TS : out Date_Type; TZ : out Zone_Type) is
      use Ada.Strings, Ada.Strings.Fixed;
      function To_Timestamp is new Convert_To_Timestamp(Date_Type);
      S : String := Trim(Value(Root_Query_Type'Class(Query),CX),Both);
   begin
      TS := To_Timestamp(S);
      for X in reverse S'Range loop
         if S(X) = '-' or else S(X) = '+' then
            if S(X..S'Last)'Length <= 3 then
               begin
                  TZ := Zone_Type'Value(S(X..S'Last));
                  return;
               exception
                  when others =>
                     raise Invalid_Format;
               end;
            else
               raise Invalid_Format;
            end if;
         end if;
      end loop;
      raise Invalid_Format;
   end Timezone_Value;

   function Bounded_Value(Query : Root_Query_Type'Class; CX : Column_Index_Type) return P.Bounded_String is
      use Ada.Strings.Bounded;
   begin
      return P.To_Bounded_String(Value(Root_Query_Type'Class(Query),CX));
   end Bounded_Value;


   procedure Integer_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type) is
      function Value is new Integer_Value(Val_Type);
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         V := Value(Root_Query_Type'Class(Query),CX);
      else
         V := Val_Type'First;
      end if;
   end Integer_Fetch;

   procedure Modular_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type) is
      function Value is new Modular_Value(Val_Type);
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         V := Value(Root_Query_Type'Class(Query),CX);
      else
         V := Val_Type'First;
      end if;
   end Modular_Fetch;

   procedure Float_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type) is
      function Value is new Float_Value(Val_Type);
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         V := Value(Root_Query_Type'Class(Query),CX);
      else
         V := Val_Type'First;
      end if;
   end Float_Fetch;

   procedure Fixed_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type) is
      function Value is new Fixed_Value(Val_Type);
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         V := Value(Root_Query_Type'Class(Query),CX);
      else
         V := Val_Type'First;
      end if;
   end Fixed_Fetch;

   procedure Decimal_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type) is
      function Value is new Decimal_Value(Val_Type);
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         V := Value(Root_Query_Type'Class(Query),CX);
      else
         V := Val_Type'First;
      end if;
   end Decimal_Fetch;

   procedure Boolean_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type) is
      function Value is new Boolean_Value(Val_Type);
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         V := Value(Root_Query_Type'Class(Query),CX);
      else
         V := Val_Type'First;
      end if;
   end Boolean_Fetch;

   procedure Date_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type) is
      function Value is new Date_Value(Val_Type);
      --D : APQ_Date;
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         V := Value(Root_Query_Type'Class(Query),CX);
      end if;
   end Date_Fetch;

   procedure Time_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type) is
      function Value is new Time_Value(Val_Type);
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         V := Value(Root_Query_Type'Class(Query),CX);
      else
         V := Val_Type'First;
      end if;
   end Time_Fetch;

   procedure Timestamp_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Val_Type; Indicator : out Ind_Type) is
      function Value is new Timestamp_Value(Val_Type);
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         V := Value(Root_Query_Type'Class(Query),CX);
      end if;
   end Timestamp_Fetch;

   procedure Timezone_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Date_Type; Z : out Zone_Type; Indicator : out Ind_Type) is
      procedure Value is new Timezone_Value(Date_Type,Zone_Type);
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         Value(Root_Query_Type'Class(Query),CX,V,Z);   -- Get Timestamp and Timezone
      else
         Z := Zone_Type'First;
      end if;
   end Timezone_Fetch;

   procedure Varchar_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out String; Last : out Natural; Indicator : out Ind_Type) is
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         declare
            S : String := Value(Root_Query_Type'Class(Query),CX);
         begin
            if S'Length > V'Length then
               raise Small_Buffer;
            end if;
            Last := V'First + S'Length - 1;
            V(V'First..Last) := S;
         end;
      end if;
   end Varchar_Fetch;

   procedure Char_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out String; Indicator : out Ind_Type) is
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         declare
            S :      String := Value(Root_Query_Type'Class(Query),CX);
            Last :   Natural := V'First + S'Length - 1;
         begin
            if S'Length > V'Length then
               raise Small_Buffer;
            end if;
            if S'Length > 0 then
               V(V'First..Last) := S;
               if Last < V'Last then
                  V(Last+1..V'Last) := ( others => ' ' );
               end if;
            else
               V := ( others => ' ' );
            end if;
         end;
      end if;
   end Char_Fetch;

   procedure Unbounded_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out Ada.Strings.Unbounded.Unbounded_String; Indicator : out Ind_Type) is
      use Ada.Strings.Unbounded;
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         V := To_Unbounded_String(Value(Root_Query_Type'Class(Query),CX));
      else
         V := Null_Unbounded_String;
      end if;
   end Unbounded_Fetch;

   procedure Bounded_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out P.Bounded_String; Indicator : out Ind) is
      use Ada.Strings, P;
   begin
      Indicator := Ind( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         declare
            S : String := Value(Root_Query_Type'Class(Query),CX);
         begin
            if S'Length > Max_Length then
               raise Small_Buffer;
            else
               V := To_Bounded_String(S,Error);
            end if;
         end;
      else
         V := Null_Bounded_String;
      end if;
   end Bounded_Fetch;

   procedure Bitstring_Fetch(Query : Root_Query_Type'Class; CX : Column_Index_Type; V : out APQ_Bitstring; Last : out Natural; Indicator : out Ind_Type) is
   begin
      Indicator := Ind_Type( Is_Null(Root_Query_Type'Class(Query),CX) );
      if not Indicator then
         declare
            B : APQ_Bitstring := Value(Root_Query_Type'Class(Query),CX);
         begin
            if B'Length > V'Length then
               raise Small_Buffer;
            end if;
            Last := V'First + B'Length - 1;
            V(V'First..Last) := B;
         end;
      end if;
   end Bitstring_Fetch;

-- GNAT 3.14p falls over and dies compiling this one. I have yet
-- to find a work-around for it.
--
--   function Generic_Command_Oid(Query : Root_Query_Type'Class) return Oid_Type is
--   begin
--      return Oid_Type(Row_ID_Type(Command_Oid(Query)));
--   end Generic_Command_Oid;

   procedure Extract_Timezone(S : String; DT : out Date_Type; TZ : out Zone_Type) is
      use Ada.Strings, Ada.Strings.Fixed;
      function To_Timestamp is new Convert_To_Timestamp(Date_Type);
      T :            String := Trim(S,Both);
      Have_TZ :      Boolean := False;
      End_X :        Positive := T'Last + 1;
   begin

      for X in reverse T'Range loop
         if T(X) = '-' or T(X) = '+' then
            Have_TZ := True;
            End_X := X;
            exit;
         elsif T(X) = ':' or T(X) = ' ' then
            exit;
         end if;
      end loop;

      DT := To_Timestamp(T(1..End_X-1));
      if Have_TZ then
         TZ := Zone_Type'Value(T(End_X+1..T'Last));
      else
         TZ := 0;
      end if;

   end Extract_Timezone;

   -- This function is provided to avoid "possible recursion" error message

   function Factory(C : Root_Connection_Type) return Root_Query_Type'Class is
   begin
      return New_Query(Root_Connection_Type'Class(C));
   end Factory;

   function New_Query(C : Root_Connection_Type) return Root_Query_Type'Class is
   begin
      return Factory(C);
   end New_Query;

end APQ;
