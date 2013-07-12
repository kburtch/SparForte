-- $Id: apq-postgresql-client.adb,v 1.2 2005/02/11 02:59:42 ken Exp $
-- Copyright(c) 2002, Warren W. Gay VE3WWG
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
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.IO_Exceptions;
with System;
with System.Address_To_Access_Conversions;
with Interfaces.C.Strings;
with GNAT.OS_Lib;

use Interfaces.C;

package body APQ.PostgreSQL.Client is

   Seek_Set : constant Interfaces.C.int := 0;
   Seek_Cur : constant Interfaces.C.int := 1;
   Seek_End : constant Interfaces.C.int := 2;

   No_Date : Ada.Calendar.Time;

   type PQ_Status_Type is (
      Connection_OK,
      Connection_Bad,
      Connection_Started,              -- Waiting for connection to be made.
      Connection_Made,                 -- Connection OK; waiting to send.
      Connection_Awaiting_Response,    -- Waiting for a response
      Connection_Auth_OK,              -- Received authentication
      Connection_Setenv                -- Negotiating environment.
   );
   for PQ_Status_Type use (
      0,    -- CONNECTION_OK
      1,    -- CONNECTION_BAD
      2,    -- CONNECTION_STARTED
      3,    -- CONNECTION_MADE
      4,    -- CONNECTION_AWAITING_RESPONSE
      5,    -- CONNECTION_AUTH_OK
      6     -- CONNECTION_SETENV
   );

   function Host_Name(C : Connection_Type) return String is
   begin
      if not Is_Connected(C) then
         return Host_Name(Root_Connection_Type(C));
      else
         declare
            use Interfaces.C.Strings;
            function PQhost(PGconn : PG_Conn) return chars_ptr;
            pragma Import(C,PQhost,"PQhost");

            The_Host : chars_ptr := PQhost(C.Connection);
         begin
            if The_Host = Null_Ptr then
               return "localhost";
            end if;
            return Value_Of(The_Host);
         end;
      end if;

      -- return "";
   end Host_Name;

   function Port(C : Connection_Type) return Integer is
   begin
      if not Is_Connected(C) then
         return Port(Root_Connection_Type(C));
      else
         declare
            use Interfaces.C.Strings;
            function PQport(PGconn : PG_Conn) return chars_ptr;
            pragma Import(C,PQport,"PQport");

            The_Port : String := Value_Of(PQport(C.Connection));
         begin
            return Integer'Value(The_Port);
         exception
            when others =>
               raise Invalid_Format;   -- Probably a UNIX socket type, not IP_Port
         end;
      end if;

      --return 0;
   end Port;

   function Port(C : Connection_Type) return String is
   begin
      if not Is_Connected(C) then
         return Port(Root_Connection_Type(C));
      else
         declare
            use Interfaces.C.Strings;
            function PQport(PGconn : PG_Conn) return chars_ptr;
            pragma Import(C,PQport,"PQport");
         begin
            return Value_Of(PQport(C.Connection));
         end;
      end if;

      --return "";     -- No port name
   end Port;

   function DB_Name(C : Connection_Type) return String is
   begin
      if not Is_Connected(C) then
         return DB_Name(Root_Connection_Type(C));
      else
         declare
            use Interfaces.C.Strings;
            function PQdb(PGconn : PG_Conn) return chars_ptr;
            pragma Import(C,PQdb,"PQdb");
         begin
            return Value_Of(PQdb(C.Connection));
         end;
      end if;

      --return "";
   end DB_Name;

   function User(C : Connection_Type) return String is
   begin
      if not Is_Connected(C) then
         return User(Root_Connection_Type(C));
      else
         declare
            use Interfaces.C.Strings;
            function PQuser(PGconn : PG_Conn) return chars_ptr;
            pragma Import(C,PQuser,"PQuser");
         begin
            return Value_Of(PQuser(C.Connection));
         end;
      end if;
      --return "";
   end User;

   function Password(C : Connection_Type) return String is
   begin
      if not Is_Connected(C) then
         return Password(Root_Connection_Type(C));
      else
         declare
            use Interfaces.C.Strings;
            function PQpass(PGconn : PG_Conn) return chars_ptr;
            pragma Import(C,PQpass,"PQpass");
         begin
            return Value_Of(PQpass(C.Connection));
         end;
      end if;
      --return "";
   end Password;

   procedure Open_DB_Trace(C : in out Connection_Type; Filename : String; Mode : Trace_Mode_Type := Trace_APQ) is
   begin
      if C.Trace_Filename /= null then
         raise Tracing_State;
      end if;

      if not Is_Connected(C) then
         raise Not_Connected;
      end if;

      if Mode = Trace_None then
         pragma assert(C.Trace_Mode = Trace_None);
         return;     -- No trace required
      end if;

      declare
         use CStr, System, Ada.Text_IO, Ada.Text_IO.C_Streams;
         procedure PQtrace(PGconn : PG_Conn; debug_port : CStr.FILEs);
         pragma Import(C,PQtrace,"PQtrace");

         C_Filename :   char_array := To_C(Filename);
         File_Mode :    char_array := To_C("a");
      begin
         C.Trace_File := fopen(C_Filename'Address,File_Mode'Address);
         if C.Trace_File = Null_Stream then
            raise Ada.IO_Exceptions.Name_Error;
         end if;

         Open(C.Trace_Ada,Append_File,C.Trace_File,Form => "shared=yes");
         Ada.Text_IO.Put_Line(C.Trace_Ada,"-- Start of Trace, Mode = " & Trace_Mode_Type'Image(Mode));

         if Mode = Trace_DB or Mode = Trace_Full then
            PQtrace(C.Connection,C.Trace_File);
         end if;

      end;

      C.Trace_Filename     := new String(1..Filename'Length);
      C.Trace_Filename.all := Filename;
      C.Trace_Mode         := Mode;
      C.Trace_On           := True;          -- Enabled by default until Set_Trace disables this

   end Open_DB_Trace;

   procedure Close_DB_Trace(C : in out Connection_Type) is
   begin

      if C.Trace_Mode = Trace_None then
         return;           -- No tracing in progress
      end if;

      pragma assert(C.Trace_Filename /= null);

      declare
         use CStr;
         procedure PQuntrace(PGconn : PG_Conn);
         pragma Import(C,PQuntrace,"PQuntrace");
      begin
         if C.Trace_Mode = Trace_DB or C.Trace_Mode = Trace_Full then
            PQuntrace(C.Connection);
         end if;

         Free(C.Trace_Filename);

         Ada.Text_IO.Put_Line(C.Trace_Ada,"-- End of Trace.");
         Ada.Text_IO.Close(C.Trace_Ada);  -- This closes C.Trace_File too

         C.Trace_Mode := Trace_None;
         C.Trace_On   := True;            -- Restore default
      end;

   end Close_DB_Trace;

   procedure Set_Trace(C : in out Connection_Type; Trace_On : Boolean := True) is
      procedure PQtrace(PGconn : PG_Conn; debug_port : CStr.FILEs);
      procedure PQuntrace(PGconn : PG_Conn);
      pragma Import(C,PQtrace,"PQtrace");
      pragma Import(C,PQuntrace,"PQuntrace");

      Orig_Trace : Boolean := C.Trace_On;
   begin
      C.Trace_On := Set_Trace.Trace_On;

      if Orig_Trace = C.Trace_On then
         return;        -- No change
      end if;

      if C.Trace_On then
         if C.Trace_Mode = Trace_DB or C.Trace_Mode = Trace_Full then
            PQtrace(C.Connection,C.Trace_File);             -- Enable libpq tracing
         end if;
      else
         if C.Trace_Mode = Trace_DB or C.Trace_Mode = Trace_Full then
            PQuntrace(C.Connection);                        -- Disable libpq tracing
         end if;
      end if;
   end Set_Trace;

   function Is_Trace(C : Connection_Type) return Boolean is
   begin
      return C.Trace_On;
   end Is_Trace;

   procedure Set_Options(C : in out Connection_Type; Options : String) is
   begin
      Replace_String(C.Options,Set_Options.Options);
   end Set_Options;

   function PQ_Status(C : Connection_Type) return PQ_Status_Type is
      function PQstatus(C : PG_Conn) return PQ_Status_Type;
      pragma Import(C,PQstatus,"PQstatus");
   begin
      if C.Connection = Null_Connection then
         return Connection_Bad;
      else
         return PQstatus(C.Connection);
      end if;
   end PQ_Status;

   function Options(C : Connection_Type) return String is
   begin
      if not Is_Connected(C) then
         if C.Options /= null then
            return C.Options.all;
         end if;
      else
         declare
            use Interfaces.C.Strings;
            function PQoptions(PGconn : PG_Conn) return chars_ptr;
            pragma Import(C,PQoptions,"PQoptions");
         begin
            return Value_Of(PQoptions(C.Connection));
         end;
      end if;
      return "";
   end Options;

   procedure Set_Notify_Proc(C : in out Connection_Type; Notify_Proc : Notify_Proc_Type) is
   begin
      C.Notify_Proc := Set_Notify_Proc.Notify_Proc;
   end Set_Notify_Proc;

   function Notify_Proc(C : Connection_Type) return Notify_Proc_Type is
   begin
      return C.Notify_Proc;
   end Notify_Proc;

   --------------------------------------------------
   -- Connection_Notify is called by notices.c as
   -- a callback from the libpq interface.
   --------------------------------------------------
   procedure Connection_Notify(C_Addr : System.Address; Msg_Ptr : Interfaces.C.Strings.chars_ptr);
   pragma Export(C,Connection_Notify,"Connection_Notify");

   procedure Connection_Notify(C_Addr : System.Address; Msg_Ptr : Interfaces.C.Strings.chars_ptr) is
      use Interfaces.C.Strings;
      package Addr is new System.Address_To_Access_Conversions(Connection_Type);

      function Strip_Prefix(S : String) return String is
         use Ada.Strings.Fixed, Ada.Strings;
      begin
         if S(S'First..S'First+6) = "NOTICE:" then
            return Trim(S(S'First+7..S'Last),Left);
         end if;
         return S;
      end Strip_Prefix;

      Abrt_Notice :  constant String := "current transaction is aborted, queries ignored until end of transaction block";
      Conn :         Addr.Object_Pointer := Addr.To_Pointer(C_Addr);
      Msg :          String := Strip_Prefix(Strip_NL(To_Ada_String(Msg_Ptr)));
   begin
      if Conn.Notice /= null then
         Free(Conn.Notice);                  -- Free last notice
      end if;
      -- Store new notice
      Conn.Notice := new String(1..Msg'Length);
      Conn.Notice.all := Msg;

      if Conn.Notice.all = Abrt_Notice then
         Conn.Abort_State := True;
      end if;

      if Conn.Notify_Proc /= Null then
         Conn.Notify_Proc(Conn.all,Conn.Notice.all);
      end if;

   end Connection_Notify;

   procedure Connect(C : in out Connection_Type) is
      procedure Notice_Install(Conn : PG_Conn; ada_obj_ptr : System.Address);
      pragma import(C,Notice_Install,"notice_install");
      function PQsetdbLogin(pghost, pgport, pgoptions, pgtty, dbname, login, pwd : System.Address) return PG_Conn;
      pragma import(C,PQsetdbLogin,"PQsetdbLogin");

      use Interfaces.C.Strings;

      C_Host :       char_array_access;
      A_Host :       System.Address := System.Null_Address;
      C_Options :    char_array_access;
      A_Options :    System.Address := System.Null_Address;
      C_Tty :        char_array_access;
      A_Tty :        System.Address := System.Null_Address;
      C_Dbname :     char_array_access;
      A_Dbname :     System.Address := System.Null_Address;
      C_Login :      char_array_access;
      A_Login :      System.Address := System.Null_Address;
      C_Pwd :        char_array_access;
      A_Pwd :        System.Address := System.Null_Address;

   begin

      if Is_Connected(C) then
         raise Already_Connected;
      end if;

      C_String(C.Host_Name,C_Host,A_Host);
      C_String(C.Options,C_Options,A_Options);
      C_String(null,C_Tty,A_Tty);
      C_String(C.DB_Name,C_Dbname,A_Dbname);
      C_String(C.User_Name,C_Login,A_Login);
      C_String(C.User_Password,C_Pwd,A_Pwd);
      
      if C.Port_Format = IP_Port then
         declare
            C_Port :       char_array := To_C(Port_Integer'Image(C.Port_Number));
            A_Port :       System.Address := C_Port'Address;
         begin
            -- Use application specified port #
            C.Connection := PQsetdbLogin(A_Host,A_Port,A_Options,A_Tty,A_Dbname,A_Login,A_Pwd);
         end;
      elsif C.Port_Format = UNIX_Port then
         declare
            C_Port :       char_array_access;
            A_Port :       System.Address := System.Null_Address;
         begin
            C_String(C.Port_Name,C_Port,A_Port);
            -- Use application specified UNIX pathname for UNIX socket ELSE rely on environment var
            C.Connection := PQsetdbLogin(A_Host,A_Port,A_Options,A_Tty,A_Dbname,A_Login,A_Pwd);
         end;
      else
         raise Program_Error;
      end if;

      if C_Host /= null then
         Free(C_Host);
      end if;
      if C_Options /= null then
         Free(C_Options);
      end if;
      if C_Tty /= null then
         Free(C_Tty);
      end if;
      if C_Dbname /= null then
         Free(C_Dbname);
      end if;
      if C_Login /= null then
         Free(C_Login);
      end if;
      if C_Pwd /= null then
         Free(C_Pwd);
      end if;

      Free_Ptr(C.Error_Message);

      if PQ_Status(C) /= Connection_OK then
         declare
            procedure PQfinish(C : PG_Conn);
            pragma Import(C,PQfinish,"PQfinish");
            Msg : String := Strip_NL(Error_Message(C));
         begin
            PQfinish(C.Connection);
            C.Connection := Null_Connection;
            C.Error_Message := new String(1..Msg'Length);
            C.Error_Message.all := Msg;
            raise Not_Connected;
         end;
      end if;

      Notice_Install(C.Connection,C'Address);   -- Install Connection_Notify handler

      ------------------------------
      -- SET PGDATESTYLE TO ISO;
      --
      -- This is necessary for all of the
      -- APQ date handling routines to
      -- function correctly. This implies
      -- that all APQ applications programs
      -- should use the ISO date format.
      ------------------------------
      declare
         SQL : Query_Type;
      begin
         Prepare(SQL,"SET DATESTYLE TO ISO");
         Execute(SQL,C);
      exception
         when others =>
            Disconnect(C);
            raise;
      end;

   end Connect;

   procedure Connect(C : in out Connection_Type; Same_As : Root_Connection_Type'Class) is
      type Info_Func is access function(C : Connection_Type) return String;

      procedure Clone(S : in out String_Ptr; Get_Info : Info_Func) is
         Info : String := Get_Info(Connection_Type(Same_As));
      begin
         if Info'Length > 0 then
            S     := new String(1..Info'Length);
            S.all := Info;
         else
            null;
            pragma assert(S = null);
         end if;
      end Clone;
   begin
      Reset(C);

      Clone(C.Host_Name,Host_Name'Access);

      C.Port_Format := Same_As.Port_Format;
      if C.Port_Format = IP_Port then
         C.Port_Number := Port(Same_As);     -- IP_Port
      else
         Clone(C.Port_Name,Port'Access);     -- UNIX_Port
      end if;

      Clone(C.DB_Name,DB_Name'Access);
      Clone(C.User_Name,User'Access);
      Clone(C.User_Password,Password'Access);
      Clone(C.Options,Options'Access);

      C.Rollback_Finalize  := Same_As.Rollback_Finalize;
      C.Notify_Proc        := Connection_Type(Same_As).Notify_Proc;

      Connect(C);          -- Connect to database before worrying about trace facilities

      -- TRACE FILE & TRACE SETTINGS ARE NOT CLONED

   end Connect;

   function Is_Connected(C : Connection_Type) return Boolean is
   begin
      return PQ_Status(C) = Connection_OK;
   end Is_Connected;

   procedure Disconnect(C : in out Connection_Type) is
      procedure Notice_Uninstall(C : PG_Conn);
      pragma Import(C,notice_uninstall,"notice_uninstall");
      procedure PQfinish(C : PG_Conn);
      pragma Import(C,PQfinish,"PQfinish");
   begin

      if not Is_Connected(C) then
         raise Not_Connected;
      end if;

      Notice_Uninstall(C.Connection);     -- Disconnect callback notices
      PQfinish(C.Connection);             -- Now release the connection
      C.Connection  := Null_Connection;
      C.Abort_State := False;             -- Clear abort state
      C.Notify_Proc := null;              -- De-register the notify procedure

      if C.Trace_Mode = Trace_APQ or else C.Trace_Mode = Trace_Full then
         Ada.Text_IO.Put_Line(C.Trace_Ada,"-- DISCONNECT");
      end if;

      Reset(C);

   end Disconnect;

   procedure Internal_Reset(C : in out Connection_Type; In_Finalize : Boolean := False) is
   begin
      Free_Ptr(C.Error_Message);

      if C.Connection /= Null_Connection then
         declare
            Q : Query_Type;
         begin
            Clear_Abort_State(C);
            if C.Rollback_Finalize or In_Abort_State(C) then
               if C.Trace_On and then C.Trace_Filename /= null and then In_Finalize = True then
                  -- KB: trace can be on even though file is not open
                  if is_open( C.Trace_Ada ) then

                     Ada.Text_IO.Put_Line(C.Trace_Ada,"-- ROLLBACK ON FINALIZE");
                  end if;
               end if;
               Rollback_Work(Q,C);
            else
               if C.Trace_On and then C.Trace_Filename /= null and then In_Finalize = True then
                  -- KB: trace can be on even though file is not open
                  if is_open( C.Trace_Ada ) then
                     Ada.Text_IO.Put_Line(C.Trace_Ada,"-- COMMIT ON FINALIZE");
                  end if;
               end if;
               Commit_Work(Q,C);
            end if;
         exception
            when others =>
               null;       -- Ignore if the Rollback/commit fails
         end;

         Clear_Abort_State(C);

         Disconnect(C);

         if C.Trace_Filename /= null then
            Close_DB_Trace(C);
         end if;

      end if;

      if C.Connection = Null_Connection then
         Free_Ptr(C.Host_Name);
         Free_Ptr(C.Host_Address);
         Free_Ptr(C.DB_Name);
         Free_Ptr(C.User_Name);
         Free_Ptr(C.User_Password);
         Free_Ptr(C.Options);
         Free_Ptr(C.Error_Message);
         Free_Ptr(C.Notice);
      end if;
   end Internal_Reset;

   procedure Reset(C : in out Connection_Type) is
   begin
      Internal_Reset(C,In_Finalize => False);
   end Reset;

   procedure Notify_on_Standard_Error(C : in out Connection_Type; Message : String) is
      use Ada.Text_IO;
   begin
      Put(Standard_Error,"*** NOTICE : ");
      Put_Line(Standard_Error,Message);
   end Notify_on_Standard_Error;

   procedure Finalize(C : in out Connection_Type) is
   begin
      Internal_Reset(C,In_Finalize => True);
   end Finalize;

   function Error_Message(C : Connection_Type) return String is
      function PQerrorMessage(C : PG_Conn) return Interfaces.C.Strings.chars_ptr;
      pragma Import(C,PQerrorMessage,"PQerrorMessage");
   begin
      if C.Connection = Null_Connection then
         if C.Error_Message /= null then
            return C.Error_Message.all;
         else
            return "";
         end if;
      else
         return To_Ada_String(PQerrorMessage(C.Connection));
      end if;
   end Error_Message;

   function Notice_Message(C : Connection_Type) return String is
   begin
      if C.Notice /= null then
         return C.Notice.all;
      end if;
      return "";
   end Notice_Message;

   function Internal_Connection(C : Connection_Type) return PG_Conn is
   begin
      return C.Connection;
   end Internal_Connection;

   procedure Free(R : in out PQ_Result) is
      procedure PQclear(R : PQ_Result);
      pragma Import(C,PQclear,"PQclear");
   begin
      if R /= Null_Result then
         PQclear(R);
         R := Null_Result;
      end if;
   end Free;

   procedure Append_Quoted(Q : in out Query_Type; Connection : Root_Connection_Type'Class; SQL : String; After : String := "") is
      function PQescapeString(to, from : System.Address; length : size_t) return size_t;
      pragma Import(C,PQescapeString,"PQescapeString");
      C_Length :  size_t := SQL'Length * 2 + 1;
      C_From :    char_array := To_C(SQL);
      C_To :      char_array(0..C_Length-1);
      R_Length :  size_t := PQescapeString(C_To'Address,C_From'Address,C_Length);
   begin
      Append(Q,"'",To_Ada(C_To));
      Append(Q,"'",After);
   end Append_Quoted;

   procedure Adjust(Q : in out Query_Type) is
   begin
      Q.Result := Null_Result;
      Adjust(Root_Query_Type(Q));
   end Adjust;

   procedure Finalize(Q : in out Query_Type) is
   begin
      Clear(Q);
   end Finalize;

   function Error_Message(Query : Query_Type) return String is
      use Interfaces.C.Strings;
      function PQresultErrorMessage(R : PQ_Result) return chars_ptr;
      pragma Import(C,PQresultErrorMessage,"PQresultErrorMessage");
   begin
      if Query.Result = Null_Result then
         raise No_Result;
      end if;

      declare
         use Interfaces.C.Strings;
         Msg_Ptr : chars_ptr := PQresultErrorMessage(Query.Result);
      begin
         if Msg_Ptr = Null_Ptr then
            return "";
         else
            return Strip_NL(Value_Of(Msg_Ptr));
         end if;
      end;
   end Error_Message;

   function Command_Status(Query : Query_Type) return String is
      use Interfaces.C.Strings;
      function PQcmdStatus(R : PQ_Result) return chars_ptr;
      pragma Import(C,PQcmdStatus,"PQcmdStatus");
   begin

      if Query.Result = Null_Result then
         raise No_Result;
      end if;

      declare
         use Interfaces.C.Strings;
         Msg_Ptr : chars_ptr := PQcmdStatus(Query.Result);
      begin
         if Msg_Ptr = Null_Ptr then
            return "";
         else
            return Strip_NL(Value_Of(Msg_Ptr));
         end if;
      end;
   end Command_Status;

   function Command_Oid(Query : Query_Type) return Row_ID_Type is
      function PQoidValue(R : PQ_Result) return PQOid_Type;
      pragma Import(C,PQoidValue,"PQoidValue");
   begin

      if Query.Result = Null_Result then
         raise No_Result;
      end if;

      return Row_ID_Type(PQoidValue(Query.Result));
   end Command_Oid;

   function Is_Duplicate_Key(Query : Query_Type) return Boolean is
      Msg : String := Error_Message(Query);
      Dup : constant String := "ERROR:  Cannot insert a duplicate key";
   begin
      if Msg'Length < Dup'Length then
         return False;
      end if;
      return Msg(Msg'First..Msg'First+Dup'Length-1) = Dup;
   end Is_Duplicate_Key;

   function Result(Query : Query_Type) return Result_Type is
      function PQresultStatus(R : PQ_Result) return Result_Type;
      pragma Import(C,PQresultStatus,"PQresultStatus");
   begin
      if Query.Result = Null_Result then
         raise No_Result;
      end if;
      return PQresultStatus(Query.Result);
   end Result;

   function Result(Query : Query_Type) return Natural is
   begin
      return Result_Type'Pos(Result(Query));
   end Result;

   procedure Execute(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
      function PQexec(C : PG_Conn; Q : System.Address) return PQ_Result;
      pragma Import(C,PQexec,"PQexec");
      R : Result_Type;
   begin

      if not Is_Connected(Connection) then
         raise Not_Connected;
      end if;

      if In_Abort_State(Connection) then
         raise Abort_State;
      end if;

      if Query.Result /= Null_Result then
         Free(Query.Result);
      end if;

      declare
         A_Query :   String := To_String(Query);
         C_Query :   char_array := To_C(A_Query);
      begin
         if Connection.Trace_On then
            if Connection.Trace_Mode = Trace_APQ or Connection.Trace_Mode = Trace_Full then
               -- KB: trace can be on even though file is not open
               if is_open( Connection.Trace_Ada ) then
                  Ada.Text_IO.Put_Line(Connection.Trace_Ada,"-- SQL QUERY:");
                  Ada.Text_IO.Put_Line(Connection.Trace_Ada,A_Query);
                  Ada.Text_IO.Put_Line(Connection.Trace_Ada,";");
               end if;
            end if;
         end if;

         Query.Result := PQexec(Internal_Connection(Connection_Type(Connection)),C_Query'Address);

         if Connection.Trace_On then
            if Connection.Trace_Mode = Trace_APQ or Connection.Trace_Mode = Trace_Full then
               -- KB: trace can be on even though file is not open
               if is_open( Connection.Trace_Ada ) then

                  Ada.Text_IO.Put_Line(Connection.Trace_Ada,"-- Result: '" & Command_Status(Query) & "'");
                  Ada.Text_IO.New_Line(Connection.Trace_Ada);
               end if;
            end if;
         end if;
      end;

      if Query.Result /= Null_Result then
         Query.Tuple_Index := First_Tuple_Index;
         R := Result(Query);
         if R /= Command_OK and R /= Tuples_OK then
--            if Connection.Trace_On then
--               Ada.Text_IO.Put_Line(Connection.Trace_Ada,"-- Error " & 
--                  Result_Type'Image(Query.Error_Code) & " : " & Error_Message(Query));
--            end if;
            raise SQL_Error;
         end if;
      else
--         if Connection.Trace_On then
--            Ada.Text_IO.Put_Line(Connection.Trace_Ada,"-- Error " & 
--               Result_Type'Image(Query.Error_Code) & " : " & Error_Message(Query));
--         end if;
         raise SQL_Error;
      end if;

   end Execute;

   procedure Execute_Checked(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class; Msg : String := "") is
      use Ada.Text_IO;
   begin
      begin
         Execute(Query,Connection);
      exception
         when SQL_Error =>
            if Msg'Length > 0 then
               Put(Standard_Error,"*** SQL ERROR: ");
               Put_Line(Standard_Error,Msg);
            else
               Put(Standard_Error,"*** SQL ERROR IN QUERY:");
               New_Line(Standard_Error);
               Put(Standard_Error,To_String(Query));
               if Col(Standard_Error) > 1 then
                  New_Line(Standard_Error);
               end if;
            end if;
            Put(Standard_Error,"[");
            Put(Standard_Error,Result_Type'Image(Result(Query)));
            Put(Standard_Error,": ");
            Put(Standard_Error,Error_Message(Query));
            Put_Line(Standard_Error,"]");
            raise;
         when others =>
            raise;
      end;
   end Execute_Checked;

   function Tuples(Q : Query_Type) return Tuple_Count_Type is
      use Interfaces.C;
      function PQntuples(R : PQ_Result) return int;
      pragma Import(C,PQntuples,"PQntuples");
   begin
      if Q.Result = Null_Result then
         raise No_Result;
      end if;
      return Tuple_Count_Type(PQntuples(Q.Result));
   end Tuples;

   function Columns(Q : Query_Type) return Natural is
      use Interfaces.C;
      function PQnfields(R : PQ_Result) return int;
      pragma Import(C,PQnfields,"PQnfields");
   begin
      if Q.Result = Null_Result then
         raise No_Result;
      end if;
      return Natural(PQnfields(Q.Result));
   end Columns;

   function Column_Name(Q : Query_Type; CX : Column_Index_Type) return String is
      use Interfaces.C.Strings;
      function PQfname(R : PQ_Result; CBX : int) return chars_ptr;
      pragma Import(C,PQfname,"PQfname");

      CBX : int := int(CX) - 1;     -- Make zero based
   begin
      if Q.Result = Null_Result then
         raise No_Result;
      end if;
      declare
         use Interfaces.C.Strings;
         CP : chars_ptr := PQfname(Q.Result,CBX);
      begin
         if CP = Null_Ptr then
            raise No_Column;
         end if;
         return Value_Of(CP);
      end;
   end Column_Name;

   function Column_Index(Q : Query_Type; Name : String) return Column_Index_Type is
      use Interfaces.C.Strings;
      function PQfnumber(R : PQ_Result; CBX : System.Address) return int;
      pragma Import(C,PQfnumber,"PQfnumber");

      C_Name :    char_array := To_C(Name);
      CBX :       int := -1;
   begin
      if Q.Result = Null_Result then
         raise No_Result;
      end if;
      CBX := PQfnumber(Q.Result,C_Name'Address);
      if CBX < 0 then
         raise No_Column;
      end if;
      return Column_Index_Type(CBX+1);
   end Column_Index;

   function Is_Column(Q : Query_Type; CX : Column_Index_Type) return Boolean is
   begin
      if Q.Result = Null_Result then
         return False;
      end if;
      return Natural(CX) <= Columns(Q);
   end Is_Column;

   function Column_Type(Q : Query_Type; CX : Column_Index_Type) return Row_ID_Type is
      function PQftype(R : PQ_Result; Field_Index : int) return PQOid_Type;
      pragma Import(C,PQftype,"PQftype");
      CBX : int := int(CX) - 1;
   begin
      if Q.Result = Null_Result then
         raise No_Result;
      end if;
      if not Is_Column(Q,CX) then
         raise No_Column;
      end if;
      return Row_ID_Type(PQftype(Q.Result,CBX));
   end Column_Type;

   function Value(Query : Query_Type; CX : Column_Index_Type) return String is
      use Interfaces.C.Strings;
      function PQgetvalue(R : PQ_Result; tup_num, field_num : int) return chars_ptr;
      pragma Import(C,PQgetvalue,"PQgetvalue");
      function PQgetisnull(R : PQ_Result; tup_num, field_num : int) return int;
      pragma Import(C,PQgetisnull,"PQgetisnull");
      C_TX :   int := int(Query.Tuple_Index) - 1;   -- Make zero based tuple #
      C_CX :   int := int(CX) - 1;              -- Field index
   begin
      if Query.Result = Null_Result then
         raise No_Result;
      end if;
      if not Is_Column(Query,CX) then
         raise No_Column;
      end if;
      declare
         use Ada.Strings.Fixed;
         C_Val : chars_ptr := PQgetvalue(Query.Result,C_TX,C_CX);
      begin
         if C_Val = Null_Ptr then
            raise No_Tuple;
         elsif PQgetisnull(Query.Result,C_TX,C_CX) /= 0 then
            raise Null_Value;
         else
            --return Value_Of(C_Val);
            return Trim(Value_Of(C_Val),Right); -- APQ 3.0
         end if;
      end;

   end Value;

   function Is_Null(Q : Query_Type; CX : Column_Index_Type) return Boolean is
      use Interfaces.C.Strings;
      function PQgetisnull(R : PQ_Result; tup_num, field_num : int) return int;
      pragma Import(C,PQgetisnull,"PQgetisnull");
      C_TX :   int := int(Q.Tuple_Index) - 1;      -- Make zero based tuple #
      C_CX :   int := int(CX) - 1;                 -- Field index
   begin
      if Q.Result = Null_Result then
         raise No_Result;
      end if;
      if not Is_Column(Q,CX) then
         raise No_Column;
      end if;
      return PQgetisnull(Q.Result,C_TX,C_CX) /= 0;
   end Is_Null;

   procedure Rewind(Q : in out Query_Type) is
   begin
      Q.Rewound := True;
      Q.Tuple_Index := First_Tuple_Index;
   end Rewind;

   procedure Fetch(Q : in out Query_Type) is
   begin
      if not Q.Rewound then
         Q.Tuple_Index := Q.Tuple_Index + 1;
      else
         Q.Rewound := False;
      end if;
      Fetch(Q,Q.Tuple_Index);
   end Fetch;

   procedure Fetch(Q : in out Query_Type; TX : Tuple_Index_Type) is
      NT : Tuple_Count_Type := Tuples(Q); -- May raise No_Result
   begin
      if NT < 1 then
         raise No_Tuple;
      end if;
      Q.Tuple_Index := TX;
      Q.Rewound := False;
      if TX > NT then
         raise No_Tuple;
      end if;
   end Fetch;

   function Tuple(Q : Query_Type) return Tuple_Index_Type is
      NT : Tuple_Count_Type := Tuples(Q); -- May raise No_Result
   begin
      if NT < 1 or else Q.Rewound then
         raise No_Tuple;
      end if;
      return Q.Tuple_Index;
   end Tuple;

   function End_of_Query(Q : Query_Type) return Boolean is
      NT : Tuple_Count_Type := Tuples(Q); -- May raise No_Result
   begin
      if NT < 1 then
         return True;      -- There are no tuples to return
      end if;

      if Q.Rewound then
         return False;     -- There is at least 1 tuple to return yet
      end if;

      return Tuple_Count_Type(Q.Tuple_Index) >= NT;   -- We've fetched them all
   end End_of_Query;

   procedure Begin_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
   begin
      if In_Abort_State(Connection) then
         raise Abort_State;
      end if;
      Clear(Query); -- APQ 3.0 patch
      Prepare(Query,"BEGIN WORK");
      Execute(Query,Connection);
      Clear(Query); -- APQ 3.0 patch
   end Begin_Work;

   procedure Commit_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
   begin
      if In_Abort_State(Connection) then
         raise Abort_State;
      end if;
      Clear(Query); -- APQ 3.0 patch
      Prepare(Query,"COMMIT WORK");
      Execute(Query,Connection);
      Clear(Query); -- APQ 3.0 patch
   end Commit_Work;

   procedure Rollback_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
   begin
      Clear(Query); -- APQ 3.0 patch
      Prepare(Query,"ROLLBACK WORK");
      Execute(Query,Connection);
      Clear(Query); -- APQ 3.0 patch
      Clear_Abort_State(Connection);
   end Rollback_Work;

   --------------------------------------------------
   -- BLOB SUPPORT :
   --------------------------------------------------

   function lo_creat(conn : PG_Conn; Mode : Mode_Type) return PQOid_Type;
   pragma Import(C,lo_creat,"lo_creat");

   function lo_open(conn : PG_Conn; Oid : PQOid_Type; Mode : Mode_Type) return Blob_Fd;
   pragma Import(C,lo_open,"lo_open");

   function lo_close(conn : PG_Conn; fd : Blob_Fd) return int;
   pragma Import(C,lo_close,"lo_close");

   function lo_read(conn : PG_Conn; fd : Blob_Fd; buf : System.Address; len : size_t) return int;
   pragma Import(C,lo_read,"lo_read");

   function lo_write(conn : PG_Conn; fd : Blob_Fd; buf : System.Address; len : size_t) return int;
   pragma Import(C,lo_write,"lo_write");

   function lo_unlink(conn : PG_Conn; Oid : PQOid_Type) return int;
   pragma Import(C,lo_unlink,"lo_unlink");

   function lo_lseek(conn : PG_Conn; fd : Blob_Fd; offset, whence : int) return int;
   pragma Import(C,lo_lseek,"lo_lseek");

   procedure Free is new Ada.Unchecked_Deallocation(Blob_Object,Blob_Type);

   function Raw_Index(Blob : Blob_Type) return Str.Stream_Element_Offset is
      use Ada.Streams;
      Offset : int;
   begin
      loop  -- In loop form in case EINTR processing should be required someday
         Offset := lo_lseek(Blob.Conn.Connection,Blob.Fd,0,Seek_Cur);
         exit when Offset >= 0;
         raise Blob_Error;    -- If not, raise exception
      end loop;

      return Stream_Element_Offset(Offset + 1);
   end Raw_Index;

   procedure Raw_Set_Index(Blob : Blob_Object; To : Str.Stream_Element_Offset) is
      Offset : int := int(To) - 1;
      Z :      int;
   begin
      loop  -- In loop form in case EINTR processing should be required someday
         Z := lo_lseek(Blob.Conn.Connection,Blob.Fd,Offset,Seek_Set);
         exit when Z >= 0;
         raise Blob_Error;    -- If not, then raise error
      end loop;
   end Raw_Set_Index;

   function Internal_Index(Blob : Blob_Type) return Str.Stream_Element_Offset is
   begin
      return Blob.Phy_Offset;
   end Internal_Index;

   procedure Internal_Set_Index(Blob : in out Blob_Object; To : Str.Stream_Element_Offset) is
      use Ada.Streams;
   begin
      if Blob.Phy_Offset /= Stream_Element_Offset(To) then
         Raw_Set_Index(Blob,To);
         Blob.Phy_Offset := Stream_Element_Offset(To);
      end if;
   end Internal_Set_Index;

   function Internal_Size(Blob : Blob_Type) return Str.Stream_Element_Offset is
      use Ada.Streams;
      Saved_Pos :    Stream_Element_Offset := Raw_Index(Blob);
      End_Offset :   int := lo_lseek(Blob.Conn.Connection,Blob.Fd,0,Seek_End);
   begin
      if End_Offset < 0 then
         raise Blob_Error;
      end if;
      Raw_Set_Index(Blob.all,Saved_Pos);
      return Stream_Element_Offset(End_Offset);
   end Internal_Size;

   procedure Internal_Read(
      Stream:  in out   Blob_Object;
      Item:       out   Ada.Streams.Stream_Element_Array;
      Last:       out   Ada.Streams.Stream_Element_Offset
   ) is
      use Ada.Streams;

      Len :    size_t := size_t(Item'Length);
      N :      int;
   begin

      loop  -- In loop form in case EINTR processing should be required someday
         N   := lo_read(Stream.Conn.Connection,Stream.Fd,Item(Item'First)'Address,Len);
         exit when N >= 0;
         raise Blob_Error;    -- No, then raise exception
      end loop;

      if N = 0 then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      Last := Item'First + Stream_Element_Offset(N) - 1;
      Stream.Phy_Offset := Stream.Phy_Offset + Stream_Element_Offset(N);

   end Internal_Read;

   procedure Internal_Write(
      Stream:  in out   Blob_Object;
      Item:    in       Ada.Streams.Stream_Element_Array
   ) is
      use Ada.Streams;
      Total :  size_t := 0;
      Len :    size_t;
      IX :     Stream_Element_Offset := Item'First;
      N :      int;
   begin
      while IX < Item'Last loop
         Len := size_t(Item'Last - IX + 1);
         N   := lo_write(Stream.Conn.Connection,Stream.Fd,Item(IX)'Address,Len);
         if N < 0 then
            raise Blob_Error;       -- If not, raise exception
         elsif N > 0 then
            IX := IX + Stream_Element_Offset(N);

            Stream.Phy_Offset := Stream.Phy_Offset + Stream_Element_Offset(N);
            if Stream.Phy_Offset - 1 > Stream.The_Size then
               Stream.The_Size := Stream.Phy_Offset - 1;
            end if;
         end if;

         if N = 0 then
            raise Ada.IO_Exceptions.End_Error;
         end if;
      end loop;

   end Internal_Write;

   procedure Internal_Blob_Open(Blob : in out Blob_Type; Mode : Mode_Type; Buf_Size : Natural := Buf_Size_Default) is
      use Ada.Streams;
   begin
      Blob.Mode   := Internal_Blob_Open.Mode;
      Blob.Fd     := lo_open(Blob.Conn.Connection,PQOid_Type(Blob.Oid),Blob.Mode);
      if Blob.Fd = -1 then
         Free(Blob);
         raise Blob_Error;
      end if;
      if Buf_Size > 0 then
         Blob.Buffer       := new Stream_Element_Array(1..Stream_Element_Offset(Buf_Size));
         Blob.Buf_Empty    := True;
         Blob.Buf_Dirty    := False;
         Blob.Buf_Offset   := 0;
         Blob.Log_Offset   := 1;
         Blob.Phy_Offset   := 1;
         Blob.The_Size     := Stream_Element_Offset(Internal_Size(Blob));
      else
         null;             -- unbuffered blob operations will be used
      end if; 
   end Internal_Blob_Open;

   procedure Blob_Flush(Blob : in out Blob_Object) is
   begin
      if Blob.Buffer /= null then
         if ( not Blob.Buf_Empty ) and Blob.Buf_Dirty then
            Internal_Set_Index(Blob,Blob.Buf_Offset);
            Internal_Write(Blob,Blob.Buffer(1..Blob.Buf_Size));
         end if;
         Blob.Buf_Dirty := False;
      else
         null;             -- Ignore flush calls in the unbuffered case
      end if;
   end Blob_Flush;

   procedure Blob_Flush(Blob : Blob_Type) is
   begin
      Blob_Flush(Blob.all);
   end Blob_Flush;

   function Blob_Create(DB : access Connection_Type; Buf_Size : Natural := Buf_Size_Default) return Blob_Type is
      Blob : Blob_Type;
   begin
      Blob := new Blob_Object(DB);
      Blob.Oid := Row_ID_Type(lo_creat(Blob.Conn.Connection,Read_Write));
      if Blob.Oid = -1 then
         free(Blob);
         raise Blob_Error;
      end if;

      begin
         Internal_Blob_Open(Blob,Write,Buf_Size);
      exception
         when others =>
            Blob_Unlink(DB.all,Blob.Oid); -- Release what will result in an unused blob!
            raise;                        -- HINT: Internal_Blob_Open() FAILS IF IT IS NOT IN A TRANSACTION!
      end;

      return Blob;
   end Blob_Create;

   function Blob_Open(DB : access Connection_Type; Oid : Row_ID_Type; Mode : Mode_Type; Buf_Size : Natural := Buf_Size_Default) return Blob_Type is
      Blob : Blob_Type;
   begin
      Blob     := new Blob_Object(DB);
      Blob.Oid := Blob_Open.Oid;
      Internal_Blob_Open(Blob,Mode,Buf_Size);
      return Blob;
   end Blob_Open;

   function Blob_Oid(Blob : Blob_Type) return Row_ID_Type is
   begin
      return Blob.Oid;
   end Blob_Oid;

   procedure Internal_Blob_Close(Blob : in out Blob_Object) is
      Z : int;
   begin
      if Blob.Buffer /= null then
         if Blob.Buf_Dirty then
            Blob_Flush(Blob);
         end if;
         Free(Blob.Buffer);
      end if;

      Z := lo_close(Blob.Conn.Connection,Blob.Fd);
      if Z /= 0 then
         raise Blob_Error;
      end if;
      Blob.Fd := -1;
   end Internal_Blob_Close;

   procedure Blob_Close(Blob : in out Blob_Type) is
   begin
      Internal_Blob_Close(Blob.all);
      Free(Blob);
   end Blob_Close;

   procedure Blob_Set_Index(Blob : Blob_Type; To : Blob_Offset) is
      use Ada.Streams;
   begin
      if Blob.Buffer /= null then
         Blob.Log_Offset := Stream_Element_Offset(To);
      else
         Internal_Set_Index(Blob.all,Stream_Element_Offset(To));
      end if;
   end Blob_Set_Index;
   
   function Blob_Index(Blob : Blob_Type) return Blob_Offset is
   begin
      if Blob.Buffer /= null then
         return Blob_Offset(Blob.Log_Offset);
      else
         return Blob_Offset(Internal_Index(Blob));
      end if;
   end Blob_Index;
   
   function Blob_Size(Blob : Blob_Type) return Blob_Count is
   begin
      if Blob.Buffer /= null then
         return Blob_Count(Blob.The_Size);
      else
         return Blob_Count(Internal_Size(Blob));
      end if;
   end Blob_Size;

   procedure Read(
      Stream:  in out   Blob_Object;
      Item:       out   Ada.Streams.Stream_Element_Array;
      Last:       out   Ada.Streams.Stream_Element_Offset
   ) is
      use Ada.Streams;

      IX : Stream_Element_Offset := Item'First;
      BX : Stream_Element_Offset;
   begin

      if Stream.Buffer /= null then
         while IX <= Item'Last and Stream.Log_Offset <= Stream.The_Size loop

            if ( not Stream.Buf_Empty ) and then Stream.Buf_Dirty then                 -- if not empty and is dirty
               if      Stream.Log_Offset < Stream.Buf_Offset                           -- if offset too low
               or else Stream.Log_Offset >= Stream.Buf_Offset + Stream.Buf_Size then   -- or offset too high
                  Blob_Flush(Stream);
                  Stream.Buf_Empty := True;
               end if;
            end if;

            if Stream.Buf_Empty then                                 -- If we have an empty buffer then..
               if Stream.Log_Offset > Stream.The_Size + 1 then
                  raise Ada.IO_Exceptions.End_Error;                 -- There is nothing to read here
               end if;

               Stream.Buf_Offset   := Stream.Log_Offset;             -- Start with our convenient offset
               Stream.Buf_Size     := Stream.Buffer.all'Length;      -- Try to read entire buffer in
               if Stream.Buf_Offset + Stream.Buf_Size - 1 > Stream.The_Size then
                  Stream.Buf_Size  := Stream.The_Size + 1 - Stream.Buf_Offset;  -- read somewhat less in
               end if;
               Internal_Set_Index(Stream,Stream.Buf_Offset);
               Internal_Read(Stream,Stream.Buffer(1..Stream.Buf_Size),Last);
               if Last /= Stream.Buf_Size then                       -- Check that all was read
                  raise Blob_Error;
               end if;
               Stream.Buf_Empty := False;                            -- Buffer is not empty
               pragma assert(Stream.Buf_Dirty = False);              -- Should not be dirty at this point
               BX := Stream.Buffer.all'First;                        -- Start reading from buffer here
            else
               BX := Stream.Log_Offset - Stream.Buf_Offset + Stream.Buffer.all'First;
            end if;

            Item(IX)          := Stream.Buffer.all(BX);              -- Read item byte
            IX                := IX + 1;                             -- Advance item index
            Stream.Log_Offset := Stream.Log_Offset + 1;              -- Advance logical offset
         end loop;
         Last := IX - 1;
      else
         Internal_Read(Stream,Item,Last);
      end if;
   end Read;

   procedure Write(
      Stream:  in out   Blob_Object;
      Item:    in       Ada.Streams.Stream_Element_Array
   ) is
      use Ada.Streams;

      IX : Stream_Element_Offset := Item'First;
      BX : Stream_Element_Offset := -1;
   begin

      if Stream.Buffer /= null then
         while IX <= Item'Last loop
            if ( not Stream.Buf_Empty ) and then Stream.Buf_Dirty then           -- Buffer is not empty and is dirty
               if      Stream.Log_Offset <  Stream.Buf_Offset                    -- if offset too low
               or else Stream.Log_Offset >  Stream.Buf_Offset + Stream.Buf_Size  -- or offset too high
               or else Stream.Buf_Size   >= Stream.Buffer.all'Length then        -- or buffer is full then..
                  Blob_Flush(Stream);                                            -- Flush out dirty data
                  Stream.Buf_Empty := True;                                      -- Now mark buffer as empty
               else
                  BX := Stream.Log_Offset - Stream.Buf_Offset + Stream.Buffer.all'First;
               end if;
            else
               BX := Stream.Log_Offset - Stream.Buf_Offset + Stream.Buffer.all'First;
            end if;            

            if Stream.Buf_Empty then                           -- if buf was empty or was just made empty then..
               Stream.Buf_Offset := Stream.Log_Offset;         -- Set to our convenient offset
               Stream.Buf_Size   := 0;                         -- No data in this buffer yet
               Stream.Buf_Dirty  := False;                     -- Make sure it's not marked dirty yet
               BX                := Stream.Buffer.all'First;   -- Point to start of buffer
            end if;

            Stream.Buffer.all(BX) := Item(IX);                 -- Write the byte
            IX                    := IX + 1;                   -- Advance Item Index
            Stream.Log_Offset     := Stream.Log_Offset + 1;    -- Advance the logical blob offset
            Stream.Buf_Empty      := False;                    -- Buffer is no longer empty
            Stream.Buf_Dirty      := True;                     -- Buffer has been modified

            if BX > Stream.Buf_Size then                       -- Did the buffer contents grow?
               Stream.Buf_Size    := Stream.Buf_Size + 1;      -- Buffer size has grown
            end if;
         end loop;
      else
         Internal_Write(Stream,Item);
      end if;
   end Write;

   procedure Finalize(Blob : in out Blob_Object) is
   begin
      if Blob.Fd /= -1 then
         Internal_Blob_Close(Blob);
      end if;
   end Finalize;

   function Blob_Stream(Blob : Blob_Type) return Root_Stream_Access is
   begin
      if Blob = Null then
         raise Blob_Error;
      end if;
      return Root_Stream_Access(Blob);
   end Blob_Stream;

   procedure Blob_Unlink(DB : Connection_Type; Oid : Row_ID_Type) is
      Z : int;
   begin
      Z := lo_unlink(DB.Connection,PQOid_Type(Oid));
      if Z = -1 then
         raise Blob_Error;
      end if;
   end Blob_Unlink;

   function lo_import(conn : PG_Conn; filename : System.Address) return int;
   pragma Import(C,lo_import,"lo_import");

   function lo_export(conn : PG_Conn; Oid : PQOid_Type; filename : System.Address) return int;
   pragma Import(C,lo_export,"lo_export");

   procedure Blob_Import(DB : Connection_Type; Pathname : String; Oid : out Row_ID_Type) is
      use Interfaces.C;
      P : char_array := To_C(Pathname);
      Z : int;
   begin
      Oid := Row_ID_Type'Last;
      Z := lo_import(DB.Connection,P'Address);
      if Z <= -1 then
         raise Blob_Error;
      end if;
      Oid := Row_ID_Type(Z);
   end Blob_Import;
   
   procedure Blob_Export(DB : Connection_Type; Oid : Row_ID_Type; Pathname : String) is
      P : char_array := To_C(Pathname);
      Z : int;
   begin
      Z := lo_export(DB.Connection,PQOid_Type(Oid),P'Address);
      if Z <= -1 then
         raise Blob_Error;
      end if;
   end Blob_Export;

   function End_of_Blob(Blob : Blob_Type) return Boolean is
      use Ada.Streams;
   begin
      if Blob.Buffer /= null then
         return Blob.Log_Offset > Blob.The_Size;
      else
         return Blob_Index(Blob) > Blob_Size(Blob);
      end if;
   end End_of_Blob;

   function Generic_Blob_Open(DB : access Connection_Type; Oid : Oid_Type; Mode : Mode_Type; Buf_Size : Natural := Buf_Size_Default) return Blob_Type is
   begin
      return Blob_Open(DB,Row_ID_Type(Oid),Mode,Buf_Size);
   end Generic_Blob_Open;

   function Generic_Blob_Oid(Blob : Blob_Type) return Oid_Type is
   begin
      return Oid_Type(Blob_Oid(Blob));
   end Generic_Blob_Oid;

   procedure Generic_Blob_Unlink(DB : Connection_Type; Oid : Oid_Type) is
   begin
      Blob_Unlink(DB,Row_ID_Type(Oid));
   end Generic_Blob_Unlink;

   procedure Generic_Blob_Import(DB : Connection_Type; Pathname : String; Oid : out Oid_Type) is
      Local_Oid : Row_ID_Type;
   begin
      Blob_Import(DB,Pathname,Local_Oid);
      Oid := Oid_Type(Local_Oid);
   end Generic_Blob_Import;

   procedure Generic_Blob_Export(DB : Connection_Type; Oid : Oid_Type; Pathname : String) is
   begin
      Blob_Export(DB,Row_ID_Type(Oid),Pathname);
   end Generic_Blob_Export;

   procedure Clear(Q : in out Query_Type) is
   begin
      Free(Q.Result);
      Clear(Root_Query_Type(Q));
   end Clear;

   function In_Abort_State(C : Connection_Type) return Boolean is
   begin
      if C.Connection = Null_Connection then
         return False;
      end if;
      return C.Abort_State;
   end In_Abort_State;

   function Engine_Of(C : Connection_Type) return Database_Type is
   begin
      return Engine_PostgreSQL;
   end Engine_Of;

   function Engine_Of(Q : Query_Type) return Database_Type is
   begin
      return Engine_PostgreSQL;
   end Engine_Of;

   function Null_Oid(Query : Query_Type) return Row_ID_Type is
   begin
      return APQ.PostgreSQL.Null_Row_ID;
   end Null_Oid;

   function New_Query(C : Connection_Type) return Root_Query_Type'Class is
      Q : Query_Type;
   begin
      return Q;
   end New_Query;

begin
   declare
      use Ada.Calendar;
      pragma suppress (range_check); -- GCC 3.3.3 (Fedora Core 2)
      -- unfortunately, the error is thrown in calendar, not here...
   begin
     --null; -- No_Date := Time_Of(Year_Number'First,Month_Number'First,Day_Number'First);
     No_Date := Time_Of(Year_Number'First,Month_Number'First,Day_Number'First);
   end;
end APQ.PostgreSQL.Client;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/apq-postgresql-client.adb,v $
