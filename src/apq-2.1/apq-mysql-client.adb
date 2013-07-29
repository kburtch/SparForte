-- $Id: apq-mysql-client.adb,v 1.2 2005/02/11 02:59:42 ken Exp $
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

WITH ADA.TEXT_IO; USE ADA.TEXT_IO;

with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.IO_Exceptions;
with Interfaces.C.Strings;

package body APQ.MySQL.Client is

   type Field_Count_Type is mod 2 ** 32;

   procedure my_init;
   pragma import(C,my_init,"c_my_init");

   function mysql_init return MYSQL;
   pragma import(C,mysql_init,"c_mysql_init");

   function mysql_connect(conn : MYSQL; host, user, passwd, db : system.address; port : Port_Integer;
      local_socket : system.address) return Return_Status;
   pragma import(C,mysql_connect,"c_mysql_connect");

   function mysql_close(C : MYSQL) return MYSQL;
   pragma import(C,mysql_close,"c_mysql_close");

   function mysql_errno(C : MYSQL) return Result_Type;
   pragma import(C,mysql_errno,"c_mysql_errno");

   function mysql_error(C : MYSQL) return Interfaces.C.Strings.chars_ptr;
   pragma import(C,mysql_error,"c_mysql_error");

   function cheat_mysql_error(results : MYSQL_RES) return Interfaces.C.Strings.chars_ptr;
   pragma import(C,cheat_mysql_error,"c_cheat_mysql_error");

   function cheat_mysql_errno(results : MYSQL_RES) return Result_Type;
   pragma import(C,cheat_mysql_errno,"c_cheat_mysql_errno");

   function mysql_port(C : MYSQL) return Integer;
   pragma import(C,mysql_port,"c_mysql_port");

   function mysql_unix_socket(C : MYSQL) return Interfaces.C.Strings.chars_ptr;
   pragma import(C,mysql_unix_socket,"c_mysql_unix_socket");

   function mysql_select_db(C : MYSQL; Database : System.Address) return Return_Status;
   pragma import(C,mysql_select_db,"c_mysql_select_db");

   function mysql_get_host_name(C : MYSQL) return Interfaces.C.Strings.chars_ptr;
   pragma import(C,mysql_get_host_name,"c_mysql_get_host_name");

   function mysql_query(conn : MYSQL; query : System.Address) return Return_Status;
   pragma import(C,mysql_query,"c_mysql_query");

   function mysql_use_result(conn : MYSQL) return MYSQL_RES;
   pragma import(C,mysql_use_result,"c_mysql_use_result");

   function mysql_store_result(conn : MYSQL) return MYSQL_RES;
   pragma import(C,mysql_store_result,"c_mysql_store_result");

   procedure mysql_free_result(result : MYSQL_RES);
   pragma import(C,mysql_free_result,"c_mysql_free_result");

   function mysql_fetch_field(result : MYSQL_RES; fieldno : Interfaces.C.int) return MYSQL_FIELD;
   pragma import(C,mysql_fetch_field,"c_mysql_fetch_field");

   function mysql_field_count(conn : MYSQL) return Field_Count_Type;
   pragma import(C,mysql_field_count,"c_mysql_field_count");

   function mysql_field_name(field : MYSQL_FIELD) return Interfaces.C.Strings.chars_ptr;
   pragma import(C,mysql_field_name,"c_mysql_field_name");

   function mysql_field_type(field : MYSQL_FIELD) return Field_Type;
   pragma import(C,mysql_field_type,"c_mysql_field_type");

   function mysql_field_value(result : MYSQL_RES; row : MYSQL_ROW; fieldno : Interfaces.C.int)
      return Interfaces.C.Strings.chars_ptr;
   pragma import(C,mysql_field_value,"c_mysql_field_value");

   function mysql_fetch_row(result : MYSQL_RES) return MYSQL_ROW;
   pragma import(C,mysql_fetch_row,"c_mysql_fetch_row");

   function mysql_fetch_row_direct(result : MYSQL_RES; row_no : MYSQL_ROW_NO) return MYSQL_ROW;
   pragma import(C,mysql_fetch_row_direct,"c_mysql_fetch_row_direct");

   function mysql_num_rows(result : MYSQL_RES) return MYSQL_ROW_NO;
   pragma import(C,mysql_num_rows,"c_mysql_num_rows");

   function mysql_sqlstate(conn : MYSQL) return Interfaces.C.Strings.chars_ptr;
   pragma import(C,mysql_sqlstate,"c_mysql_sqlstate");

   function mysql_eof(results : MYSQL_RES) return Interfaces.C.int;
   pragma import(C,mysql_eof,"c_mysql_eof");

   function mysql_num_fields(results : MYSQL_RES) return Interfaces.C.int;
   pragma import(C,mysql_num_fields,"c_mysql_num_fields");

   function mysql_fetch_field_direct(results : MYSQL_RES; x : Interfaces.C.int) return MYSQL_FIELD;
   pragma import(C,mysql_fetch_field_direct,"c_mysql_fetch_field_direct");

   function mysql_name_index(results : MYSQL_RES; name : System.Address) return Interfaces.C.int;
   pragma import(C,mysql_name_index,"c_mysql_name_index");

   function mysql_get_field_type(results : MYSQL_RES; x : Interfaces.C.int) return Field_Type;
   pragma import(C,mysql_get_field_type,"c_mysql_get_field_type");

   function mysql_insert_id(connection : MYSQL) return MySQL_Oid_Type;
   pragma import(C,mysql_insert_id,"c_mysql_insert_id");

   function mysql_real_escape_string(connection : MYSQL; to, from : System.Address; length : u_long)
      return u_long;
   pragma import(C,mysql_real_escape_string,"c_mysql_real_escape_string");

   function mysql_options_notused(connection : MYSQL; opt : MySQL_Enum_Option)
      return Interfaces.C.int;
   pragma import(C,mysql_options_notused,"c_mysql_options_notused");

   function mysql_options_uint(connection : MYSQL; opt : MySQL_Enum_Option; arg : Interfaces.C.unsigned)
      return Interfaces.C.int;
   pragma import(C,mysql_options_uint,"c_mysql_options_uint");

   function mysql_options_puint(connection : MYSQL; opt : MySQL_Enum_Option; arg : Interfaces.C.unsigned)
      return Interfaces.C.int;
   pragma import(C,mysql_options_puint,"c_mysql_options_puint");

   function mysql_options_char(connection : MYSQL; opt : MySQL_Enum_Option; arg : System.Address)
      return Interfaces.C.int;
   pragma import(C,mysql_options_char,"c_mysql_options_char");


   procedure Free(Results : in out MYSQL_RES) is
   begin
      mysql_free_result(Results);
      Results := Null_Result;
   end Free;

   function Name_Of(Field : MYSQL_FIELD) return String is
      use Interfaces.C.Strings, Interfaces.C;
   begin
      return To_Ada(Value(mysql_field_name(Field)));
   end Name_Of;

   function Type_Of(Field : MYSQL_FIELD) return Field_Type is
   begin
      return mysql_field_type(Field);
   end Type_Of;

   procedure Clear_Error(C : in out Connection_Type) is
   begin
      C.Error_Code := CR_NO_ERROR;
      Free(C.Error_Message);
   end Clear_Error;

   procedure Clear_Error(Q : in out Query_Type; C : in out Connection_Type) is
   begin
      C.Error_Code := CR_NO_ERROR;
      Free(C.Error_Message);

      Q.Error_Code := C.Error_Code;
      Free(Q.Error_Message);
   end Clear_Error;

   procedure Post_Error(C : in out Connection_Type) is
      use Interfaces.C, Interfaces.C.Strings;
      Error_Msg : String := To_Ada(Value(mysql_error(C.Connection)));
   begin
      C.Error_Code := mysql_errno(C.Connection);
      Replace_String(C.Error_Message,Error_Msg);
   end Post_Error;

   ------------------------------
   -- INTERNAL :
   ------------------------------
   -- This one cheats because here
   -- we have no access to the
   -- Connection_Type object.
   ------------------------------
   procedure Post_Error(Q : in out Query_Type) is
      use Interfaces.C, Interfaces.C.Strings;
      Error_Msg : String := To_Ada(Value(cheat_mysql_error(Q.Results)));
   begin
      if Q.Results = Null_Result then
         Q.Error_Code := CR_NO_ERROR;
         Free(Q.Error_Message);
      else
         Q.Error_Code := cheat_mysql_errno(Q.Results);
         Replace_String(Q.Error_Message,Error_Msg);
      end if;
   end Post_Error;

   procedure Post_Error(Q : in out Query_Type; C : in out Connection_Type) is
      use Interfaces.C, Interfaces.C.Strings;
      Error_Msg : String := To_Ada(Value(mysql_error(C.Connection)));
   begin
      C.Error_Code := mysql_errno(C.Connection);
      Replace_String(C.Error_Message,Error_Msg);

      Q.Error_Code := C.Error_Code;
      if C.Error_Message /= null then
         Replace_String(Q.Error_Message,Error_Msg);
      else
         Free(Q.Error_Message);
      end if;
   end Post_Error;

   procedure Clear_Results(Q : in out Query_Type) is
   begin
      if Q.Results /= Null_Result then
         Free(Q.Results);
      end if;
      Q.Row := Null_Row;
   end Clear_Results;

   function Is_Column(Q : Query_Type; CX : Column_Index_Type) return Boolean is
      Cols : Natural := Columns(Q);
   begin
      return Natural(CX) >= 1 and then Natural(CX) <= Cols;
   end Is_Column;

   function Value_Of(Results : MYSQL_RES; Row : MYSQL_ROW; Column_Index : Column_Index_Type)
      return Interfaces.C.Strings.chars_ptr
   is
      use Interfaces.C;
   begin
      return mysql_field_value(Results,Row,Interfaces.C.Int(Column_Index)-1);
   end Value_Of;

   function Port(C : Connection_Type) return Integer is
   begin
      if not Is_Connected(C) then
         return Port(Root_Connection_Type(C));
      else
         return mysql_port(C.Connection);
      end if;
   end Port;
   
   function Port(C : Connection_Type) return String is
      use Interfaces.C, Interfaces.C.Strings;
   begin
      if not Is_Connected(C) then
         return Port(Root_Connection_Type(C));
      else
         return To_Ada(Value(mysql_unix_socket(C.Connection)));
      end if;
   end Port;

   procedure Set_DB_Name(C : in out Connection_Type; DB_Name : String) is
   begin
      if not Is_Connected(C) then
         Set_DB_Name(Root_Connection_Type(C),DB_Name);
      else
         declare
            use Interfaces.C;
            Use_Database : char_array := To_C(DB_Name);
         begin
            if mysql_select_db(C.Connection,Use_Database'Address) /= 0 then
               Set_DB_Name(Root_Connection_Type(C),DB_Name);
            else
               raise Failed;
            end if;
         end;
      end if;
   end Set_DB_Name;

   procedure Parse_Option(
      Options :   in out Ada.Strings.Unbounded.Unbounded_String;
      Keyword :   in out Ada.Strings.Unbounded.Unbounded_String;
      Argument :  in out Ada.Strings.Unbounded.Unbounded_String
   ) is
      use Ada.Strings.Unbounded;
      Option :  Unbounded_String;
      The_End : Natural;
      Value_X : Natural;
   begin
      Keyword := Null_Unbounded_String;
      Argument := Null_Unbounded_String;

      while Length(Options) > 0 loop
         exit when Slice(Options,1,1) /= ",";
         Delete(Options,1,1);
      end loop;

      if Length(Options) < 1 then
         return;
      end if;

      The_End := Index(Options,",");
      if The_End < 1 then
         The_End := Length(Options)+1;
      end if;

      Option := Options;
      if The_End <= Length(Options) then
         Delete(Option,The_End,Length(Options));
      end if;

      if The_End <= Length(Options) then
         Delete(Options,1,The_End);
      else
         Delete(Options,1,The_End-1);
      end if;

      Value_X := Index(Option,"=");
      if Value_X < 1 then
         Keyword := Option;
      else
         Keyword := To_Unbounded_String(Slice(Option,1,Value_X-1));
         Argument := To_Unbounded_String(Slice(Option,Value_X+1,Length(Option)));
      end if;

   end Parse_Option;

   procedure Process_Option(C : in out Connection_Type; Keyword, Argument : String) is
      use Interfaces.C;
      L : Natural;
      X : Natural := 0;
      E : MySQL_Enum_Option;
      T : Option_Argument_Type;
      z : Interfaces.C.int;
   begin
      for Y in APQ.MySQL.Options'Range loop
         L := APQ.MySQL.Options(Y).Length;
         if APQ.MySQL.Options(Y).Name(1..L) = Keyword then
            X := Y;
            exit;
         end if;
      end loop;

      if X = 0 then
         raise Failed;                       -- Unknown option
      end if;

      E := APQ.MySQL.Options(X).MySQL_Enum;  -- Database option value
      T := APQ.MySQL.Options(X).Argument;    -- Argument type

      case T is
         when ARG_NOT_USED =>
            z := mysql_options_notused(C.Connection,E);
            if z /= 0 then
               raise Failed;
            end if;
         when ARG_UINT =>
            declare
               U : unsigned;
            begin
               U := unsigned'Value(Argument);
               z := mysql_options_uint(C.Connection,E,U);
            end;
            if z /= 0 then
               raise Failed;
            end if;
         when ARG_PTR_UINT =>
            declare
               U : unsigned;
            begin
               U := unsigned'Value(Argument);
               z := mysql_options_puint(C.Connection,E,U);
            end;
            if z /= 0 then
               raise Failed;
            end if;
         when ARG_CHAR_PTR =>
            declare
               S : char_array := To_C(Argument);
            begin
               z := mysql_options_char(C.Connection,E,S'Address);
            end;
            if z /= 0 then
               raise Failed;
            end if;
      end case;

   end Process_Option;

   procedure Process_Connection_Options(C : in out Connection_Type) is
      use Ada.Strings.Unbounded, Ada.Characters.Handling;
      Opts :     Unbounded_String;
      Keyword :  Unbounded_String;
      Argument : Unbounded_String;
   begin
      Opts := To_Unbounded_String(C.Options.all);

      while Length(Opts) > 0 loop
         Parse_Option(Opts,Keyword,Argument);
         Process_Option(C,To_Upper(To_String(Keyword)),To_String(Argument));
      end loop;
   end Process_Connection_Options;

   procedure Connect(C : in out Connection_Type) is
      use Interfaces.C.Strings;

      C_Host :       char_array_access;
      A_Host :       System.Address := System.Null_Address;
      C_Dbname :     char_array_access;
      A_Dbname :     System.Address := System.Null_Address;
      C_Login :      char_array_access;
      A_Login :      System.Address := System.Null_Address;
      C_Pwd :        char_array_access;
      A_Pwd :        System.Address := System.Null_Address;

      C_Port :       Port_Integer := C.Port_Number;

      C_Unix :       char_array_access;
      A_Unix :       System.Address := System.Null_Address;

   begin

      Clear_Error(C);

      if Is_Connected(C) then
         raise Already_Connected;
      end if;

      if C.Port_Format = IP_Port and then C.Port_Number <= 0 then
         raise Not_Connected;    -- Bad port number
      end if;

      C_String(C.Host_Name,C_Host,A_Host);
      C_String(C.DB_Name,C_Dbname,A_Dbname);
      C_String(C.User_Name,C_Login,A_Login);
      C_String(C.User_Password,C_Pwd,A_Pwd);
      
      case C.Port_Format is
         when IP_Port =>
            null;
         when UNIX_Port =>
            C_Port := 0;         -- Zero indicates to mysql_connect() that we are using unix socket
            C_String(C.Port_Name,C_Unix,A_Unix);
      end case;

      -- Patch from APQ 3.0
      --
      -- Must re-establish a C.Connection after a Disconnect/Reset (object reuse)
      --
      if C.Connection = Null_Connection then
         C.Connection := mysql_init;      -- Needed after disconnects
      end if;
      C.Connected := mysql_connect(
         conn           => C.Connection,  -- Connection object
         host           => A_Host,        -- host or IP #
         user           => A_Login,       -- user name
         passwd         => A_Pwd,         -- password
         db             => A_Dbname,      -- database
         port           => C_Port,        -- IP Port # or zero
         local_socket   => A_Unix         -- UNIX socket name or null
      ) /= 0;

      if C_Host /= null then
         Free(C_Host);
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
      if C_Unix /= null then
         Free(C_Unix);
      end if;

      if not C.Connected then
         Post_Error(C);
         raise Not_Connected;
      else
         declare
            use Interfaces.C, Interfaces.C.Strings;
            Host_Name : String := To_Ada(Value(mysql_get_host_name(C.Connection)));
         begin
            Replace_String(C.Host_Name,Host_Name);
         end;

         declare
            use Interfaces.C, Interfaces.C.Strings;
            UNIX_Socket : String := To_Ada(Value(mysql_unix_socket(C.Connection)));
         begin
            if UNIX_Socket /= "" then
               C.Port_Format := UNIX_Port;
               Replace_String(C.Port_Name,UNIX_Socket);     -- Update socket pathname
            else
               C.Port_Format := IP_Port;
               C.Port_Number := mysql_port(C.Connection);   -- Update port number used
               if C.Port_Name /= null then
                  Free(C.Port_Name);
               end if;
            end if;
         end;

         if C.Options /= null then
            Process_Connection_Options(C);
         end if;
      end if;

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

      Connect(C);          -- Connect to database before worrying about trace facilities

      -- TRACE FILE & TRACE SETTINGS ARE NOT CLONED

   end Connect;

   procedure Internal_Reset(C : in out Connection_Type; In_Finalize : Boolean := False) is
   begin
      Free_Ptr(C.Error_Message);

      if C.Connection /= Null_Connection then
         -- Abort query, and rollback..
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

      -- KB: added In_Finalize: NQS if this is right
      if C.Connection = Null_Connection or In_Finalize then
         Free_Ptr(C.Host_Name);
         Free_Ptr(C.Host_Address);
         Free_Ptr(C.DB_Name);
         Free_Ptr(C.User_Name);
         Free_Ptr(C.User_Password);
         Free_Ptr(C.Error_Message);
      end if;

   end Internal_Reset;

   procedure Reset(C : in out Connection_Type) is
   begin
      Internal_Reset(C,False);
   end Reset;

   function Error_Message(C : Connection_Type) return String is
   begin
      if C.Error_Message /= null then
         return C.Error_Message.all;
      else
         return "";
      end if;
   end Error_Message;

   procedure Disconnect(C : in out Connection_Type) is
   begin
      -- KB: from apq-postgres.  this causes weird PROGRAM_ERROR exception.
      -- why?
      --if not Is_Connected(C) then
      --   raise Not_Connected;
      --end if;

      if C.Connection /= Null_Connection then
         C.Connection := mysql_close(C.Connection);
         if C.Trace_Mode = Trace_APQ or else C.Trace_Mode = Trace_Full then
            -- KB: trace can be on even though file is not open
            if is_open( C.Trace_Ada ) then
               Ada.Text_IO.Put_Line(C.Trace_Ada,"-- DISCONNECT");
            end if;
         end if;
         C.Connection := Null_Connection; -- From APQ 3.0

         Reset(C); -- KB: close open trace files, etc.
      end if;
   end Disconnect;

   function Is_Connected(C : Connection_Type) return Boolean is
   begin
      if C.Connection /= Null_Connection then
         return C.Connected;
      else
         return False;
      end if;
   end Is_Connected;

   function Options(C : Connection_Type) return String is
   begin
      return To_String(C.Options);
   end Options;

   procedure Initialize(C : in out Connection_Type) is
   begin
      C.Connection := mysql_init;
      C.Connected  := False;
   end Initialize;

   procedure Finalize(C : in out Connection_Type) is
   begin
      Internal_Reset(C,True);
   end Finalize;

   procedure Adjust(Q : in out Query_Type) is
   begin
      Q.Results := Null_Result;
      Q.Row     := Null_Row;
      Q.Error_Message := null;
      Q.Row_ID  := Row_ID_Type'First;
      Adjust(Root_Query_Type(Q));
   end Adjust;

   procedure Finalize(Q : in out Query_Type) is
   begin
      Clear(Q);
   end Finalize;

   procedure Execute(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
      R : Return_Status;
   begin

      if not Is_Connected(Connection_Type(Connection)) then
         raise Not_Connected;
      end if;

      Clear_Results(Query);
      Query.Rewound := True;

      declare
         use Interfaces.C;
         A_Query :   String := To_String(Query);
         C_Query :   char_array := To_C(A_Query);
      begin
         if Connection_Type(Connection).Trace_On then
            if Connection_Type(Connection).Trace_Mode = Trace_APQ
            or Connection_Type(Connection).Trace_Mode = Trace_Full then
               -- KB: trace can be on even though file is not open
               if is_open( Connection.Trace_Ada ) then
                  Ada.Text_IO.Put_Line(Connection.Trace_Ada,"-- SQL QUERY:");
                  Ada.Text_IO.Put_Line(Connection.Trace_Ada,A_Query);
                  Ada.Text_IO.Put_Line(Connection.Trace_Ada,";");
               end if;
            end if;
         end if;

         R := mysql_query(Connection_Type(Connection).Connection,C_Query'Address);
      end;

      Post_Error(Query,Connection_Type(Connection));

      if R /= 0 then
         -- Successful
         Query.Tuple_Index := Tuple_Index_Type'First;
         Query.Row_ID := Row_ID_Type(mysql_insert_id(Connection_Type(Connection).Connection));

         case Query.Mode is
            when Sequential_Fetch =>
               Query.Results := mysql_use_result(Connection_Type(Connection).Connection);
            when Random_Fetch =>
               Query.Results := mysql_store_result(Connection_Type(Connection).Connection);
         end case;

         if Query.Results = Null_Result then
            if mysql_field_count(Connection_Type(Connection).Connection) /= 0 then
               -- The query should return data, hence an error :
               Post_Error(Query,Connection_Type(Connection));
               if Connection.Trace_On then
                  -- KB: trace can be on even though file is not open
                  if is_open( Connection_Type(Connection).Trace_Ada ) then
                     Ada.Text_IO.Put_Line(Connection_Type(Connection).Trace_Ada,"-- Error " & 
                        Result_Type'Image(Query.Error_Code) & " : " & Error_Message(Query));
                     Ada.Text_IO.New_Line(Connection_Type(Connection).Trace_Ada);
                  end if;
               end if;
               raise SQL_Error;
            else
               if Connection_Type(Connection).Trace_On then
                  -- KB: trace can be on even though file is not open
                  if is_open( Connection_Type(Connection).Trace_Ada ) then
                     Ada.Text_IO.New_Line(Connection_Type(Connection).Trace_Ada);
                  end if;
               end if;
            end if;
         else
            -- We received a result :
            if Connection.Trace_On then
               if Connection_Type(Connection).Trace_Mode = Trace_APQ
               or Connection_Type(Connection).Trace_Mode = Trace_Full then
                  -- KB: trace can be on even though file is not open
                  if is_open( Connection_Type(Connection).Trace_Ada ) then
                     Ada.Text_IO.New_Line(Connection_Type(Connection).Trace_Ada);
                  end if;
               end if;
            end if;
         end if;
      else
         -- Query failed :
         if Connection_Type(Connection).Trace_On then
            -- KB: trace can be on even though file is not open
            if is_open( Connection_Type(Connection).Trace_Ada ) then
               Ada.Text_IO.Put_Line(Connection_Type(Connection).Trace_Ada,"-- Error " & 
                  Result_Type'Image(Query.Error_Code) & " : " & Error_Message(Query));
               Ada.Text_IO.New_Line(Connection_Type(Connection).Trace_Ada);
            end if;
         end if;
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

   function Error_Message(Query : Query_Type) return String is
   begin
      return To_String(Query.Error_Message);
   end Error_Message;

   function Value(Query : Query_Type; CX : Column_Index_Type) return String is
   begin
      if not Is_Column(Query,CX) then
         raise No_Column;
      end if;

      if Query.Row = Null_Row then
         raise No_Result;
      end if;

      declare
         use Interfaces.C.Strings;
         Col_Val : chars_ptr := Value_Of(Query.Results,Query.Row,CX);
      begin
         if Is_Null(Col_Val) then
            raise Null_Value;
         else
            return Value_Of(Col_Val);
         end if;
      end;
   end Value;

   procedure Rewind(Q : in out Query_Type) is
   begin
      Q.Row := Null_Row;
      case Q.Mode is
         when Random_Fetch =>
            Q.Rewound := True;
            Q.Tuple_Index := First_Tuple_Index;
         when Sequential_Fetch =>
            raise SQL_Error;
      end case;
   end Rewind;

   procedure Fetch(Q : in out Query_Type) is
   begin
      if not Q.Rewound then
         Q.Tuple_Index := Q.Tuple_Index + 1;
      else
         Q.Rewound := False;
         Q.Tuple_Index := First_Tuple_Index;
      end if;

      case Q.Mode is
         when Random_Fetch =>
            Fetch(Q,Q.Tuple_Index);
         when Sequential_Fetch =>
            Q.Row := mysql_fetch_row(Q.Results);
            Post_Error(Q);
            if Q.Row = Null_Row then
               raise No_Tuple;
            end if;
      end case;
   end Fetch;

   procedure Fetch(Q : in out Query_Type; TX : Tuple_Index_Type) is
      Row_No : MYSQL_ROW_NO := MYSQL_ROW_NO(TX);
   begin
      Q.Row := Null_Row;

      if TX < 1 then
         raise No_Tuple;
      end if;

      if Q.Mode = Random_Fetch then
         declare
	    X : MYSQL_ROW_NO := Row_No - 1;
            R : MYSQL_ROW;
         begin	
            R := mysql_fetch_row_direct(Q.Results,X);
            Q.Row := R;
         end;
         Post_Error(Q);
         if Q.Row = Null_Row then
            raise No_Tuple;
         end if;
      else
         Q.Row := Null_Row;   -- Don't try to reference this row later, if any
         raise Failed;        -- This Query_Type is not able to do Random fetches!
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
      use Interfaces.C;
      NT : Tuple_Count_Type := Tuples(Q); -- May raise No_Result
   begin
      case Q.Mode is
         when Random_Fetch =>
            if NT < 1 then
               return True;      -- There are no tuples to return
            end if;

            if Q.Rewound then
               return False;     -- There is at least 1 tuple to return yet
            end if;
            return Tuple_Count_Type(Q.Tuple_Index) >= NT;   -- We've fetched them all
         when Sequential_Fetch =>

            -- *** MySQL Limitation ***
            --
            --    For sequential fetches, you _MUST_ catch the
            -- exception No_Tuple while doing a FETCH, to determine
            -- where the last row is. MySQL C function mysql_eof()
            -- returns True, until an attempt to fetch the last row
            -- occurs (thus is of no value in preventing the bad
            -- fetch!)
            --
            -- End_Of_Query is a function, so it cannot prefetch either.
            -- Sooooo.. we just raise a Program_Error.

            raise Program_Error;    -- THIS REALLY BITES!
--          return mysql_eof(Q.Results) /= 0;
      end case;
   end End_of_Query;

   function Tuples(Q : Query_Type) return Tuple_Count_Type is
   begin
      if Q.Results = Null_Result then
         raise No_Result;
      end if;
      return Tuple_Count_Type(mysql_num_rows(Q.Results));
   end Tuples;

   function Result(Query : Query_Type) return Natural is
   begin
      return Result_Type'Pos(Result(Query));
   end Result;

   function Result(Query : Query_Type) return Result_Type is
   begin
      return Query.Error_Code;
   end Result;

   procedure Begin_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
   begin
      Clear(Query); -- APQ 3.0 patch
      Prepare(Query,"BEGIN WORK");
      Execute(Query,Connection_Type(Connection));
      Clear(Query); -- APQ 3.0 patch
   end Begin_Work;

   procedure Commit_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
   begin
      Clear(Query); -- APQ 3.0 patch
      Prepare(Query,"COMMIT");
      Execute(Query,Connection_Type(Connection));
      Clear(Query); -- APQ 3.0 patch
   end Commit_Work;
   
   procedure Rollback_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
   begin
      Clear(Query); -- APQ 3.0 patch
      Prepare(Query,"ROLLBACK");
      Execute(Query,Connection_Type(Connection));
      Clear(Query); -- APQ 3.0 patch
   end Rollback_Work;

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

      Ada.Text_IO.Create(C.Trace_Ada,Append_File,Filename,Form => "shared=yes");
      C.Trace_File := CStr.Null_Stream;      -- Not used for MySQL

      Ada.Text_IO.Put_Line(C.Trace_Ada,"-- Start of Trace, Mode = " & Trace_Mode_Type'Image(Mode));

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

      Free(C.Trace_Filename);

      Ada.Text_IO.Put_Line(C.Trace_Ada,"-- End of Trace.");
      Ada.Text_IO.Close(C.Trace_Ada);  -- C.Trace_File is not used for MySQL

      C.Trace_Mode := Trace_None;
      C.Trace_On   := True;            -- Restore default

   end Close_DB_Trace;

   procedure Set_Trace(C : in out Connection_Type; Trace_On : Boolean := True) is
      Orig_Trace : Boolean := C.Trace_On;
   begin
      C.Trace_On := Set_Trace.Trace_On;

      if Orig_Trace = C.Trace_On then
         return;        -- No change
      end if;

      if C.Trace_On then
         if C.Trace_Mode = Trace_DB or C.Trace_Mode = Trace_Full then
            null;
         end if;
      else
         if C.Trace_Mode = Trace_DB or C.Trace_Mode = Trace_Full then
            null;
         end if;
      end if;
   end Set_Trace;

   function Is_Trace(C : Connection_Type) return Boolean is
   begin
      return C.Trace_On;
   end Is_Trace;

   function In_Abort_State(C : Connection_Type) return Boolean is
   begin
      if C.Connection = Null_Connection then
         return False;
      end if;
      return C.Abort_State;
   end In_Abort_State;

   function Columns(Q : Query_Type) return Natural is
   begin
      if Q.Results = Null_Result then
         raise No_Result;
      end if;
      return Natural(mysql_num_fields(Q.Results));
   end Columns;

   function Column_Name(Q : Query_Type; CX : Column_Index_Type) return String is
      use Interfaces.C;
      CBX :    int := int(CX) - 1;     -- Make zero based
      Cols :   Natural := 0;           -- # of columns in result
   begin
      if Q.Results = Null_Result then
         raise No_Result;
      end if;

      -- We must check index, since mysql_fetch_field_direct() won't
      if not Is_Column(Q,CX) then
         raise No_Column;
      end if;

      declare
         use Interfaces.C.Strings;
         cp : chars_ptr := mysql_field_name(mysql_fetch_field_direct(Q.Results,CBX));
      begin
         if CP = Null_Ptr then
            raise No_Column;
         end if;
         return Value_Of(CP);
      end;
   end Column_Name;

   function Column_Index(Q : Query_Type; Name : String) return Column_Index_Type is
      use Interfaces.C;
      C_Name :    char_array := To_C(Name);
      CBX :       int := -1;
   begin
      if Q.Results = Null_Result then
         raise No_Result;
      end if;
      CBX := mysql_name_index(Q.Results,C_Name'Address);
      if CBX < 0 then
         raise No_Column;
      end if;
      return Column_Index_Type(CBX+1);
   end Column_Index;

   function Is_Null(Q : Query_Type; CX : Column_Index_Type) return Boolean is
   begin

      if Q.Results = Null_Result then
         raise No_Result;
      end if;

      if not Is_Column(Q,CX) then
         raise No_Column;
      end if;

      declare
         use Interfaces.C.Strings;
         Col_Val : chars_ptr := Value_Of(Q.Results,Q.Row,CX);
      begin
         return Is_Null(Col_Val);
      end;

   end Is_Null;

   function Column_Type(Q : Query_Type; CX : Column_Index_Type) return Field_Type is
      use Interfaces.C;
      CBX : int := int(CX) - 1;
   begin
      if Q.Results = Null_Result then
         raise No_Result;
      end if;
      if not Is_Column(Q,CX) then
         raise No_Column;
      end if;
      return mysql_get_field_type(Q.Results,CBX);
   end Column_Type;

   function Engine_Of(C : Connection_Type) return Database_Type is
   begin
      return Engine_MySQL;
   end Engine_Of;

   function Engine_Of(Q : Query_Type) return Database_Type is
   begin
      return Engine_MySQL;
   end Engine_Of;

   function Command_Oid(Query : Query_Type) return Row_ID_Type is
   begin
      return Query.Row_ID;
   end Command_Oid;

   function Null_Oid return Row_ID_Type is
   begin
      return Null_Row_ID;
   end Null_Oid;

   function Is_Duplicate_Key(Query : Query_Type) return Boolean is
      R : Result_Type := Result(Query);
   begin
      return R = ER_DUP_ENTRY or else R = ER_DUP_KEY;
   end Is_Duplicate_Key;

   procedure Append_Quoted(Q : in out Query_Type; Connection : Root_Connection_Type'Class; SQL : String; After : String := "") is
      use Interfaces.C;
      C_Length :  size_t := size_t(SQL'Length * 2 + 1);
      C_From :    char_array := To_C(SQL);
      C_To :      char_array(0..C_Length-1);
      R_Length :  u_long := mysql_real_escape_string(
         Connection_Type(Connection).Connection,C_To'Address,C_From'Address,u_long(SQL'Length));
   begin
      Append(Q,"'",To_Ada(C_To));
      Append(Q,"'",After);
   end Append_Quoted;

   procedure Clear(Q : in out Query_Type) is
   begin
      Clear_Results(Q);
      Clear(Root_Query_Type(Q));
   end Clear;

   function Null_Oid(Query : Query_Type) return Row_ID_Type is
   begin
      return Null_Row_ID;
   end Null_Oid;

   procedure Set_Fetch_Mode(Q : in out Query_Type; Mode : Fetch_Mode_Type) is
   begin
      if Q.Results /= Null_Result then
         raise Failed;                 -- Cannot change fetch mode when results exist already
      end if;
      Set_Fetch_Mode(Root_Query_Type(Q),Mode);
   end Set_Fetch_Mode;

   function New_Query(C : Connection_Type) return Root_Query_Type'Class is
      Q : Query_Type;
   begin
      return Q;
   end New_Query;

   procedure Set_Options(C : in out Connection_Type; Options : String) is
      use Ada.Strings.Unbounded;
   begin
      Replace_String(C.Options,Set_Options.Options);

      if C.Options = null then
         return;
      end if;

      if Is_Connected(C) then
         Process_Connection_Options(C);
      end if;

   end Set_Options;

begin
   my_init;
end APQ.MySQL.Client;
