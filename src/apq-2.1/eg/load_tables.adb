-- $Id: load_tables.adb,v 1.2 2005/02/11 02:59:46 ken Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ;
use APQ;

with Ada.Text_IO, Ada.Strings.Fixed, Ada.Strings.Maps;
use Ada.Text_IO;

package body Load_Tables is

   function Nth_Index(S : String; Delimiter : String; N : Positive) return Natural is
      use Ada.Strings.Fixed;
      X  : Natural := S'First - 1;
   begin

      for I in 1..N loop
         X := Index(S(X+1..S'Last),Delimiter);
         if X < 1 then
            return S'Last + 1;
         end if;
      end loop;

      return X;

   end Nth_Index;

   function Parse_Token(S, Delimiter : String; N : Positive) return String is
   begin
      if N = 1 then
         return S(S'First..Nth_Index(S,Delimiter,N)-1);
      end if;

      declare
         SX, EX : Natural;
      begin
         SX := Nth_Index(S,Delimiter,N-1) + 1;
         if SX > S'Last then
            return "";
         end if;
         EX := Nth_Index(S,Delimiter,N);
         return S(SX..EX-1);
      end;

   end Parse_Token;

   function More_Tokens(S, Delimiter : String; N : Positive) return Boolean is
   begin
      if S'Length < 1 then
         return False;
      end if;
      if N = 1 then
         return Nth_Index(S,Delimiter,N)-1 /= S'First;
      end if;

      return Nth_Index(S,Delimiter,N-1) + 1 <= S'Last;

   end More_Tokens;

   procedure Load_Passwd_Table(C : in out Connection_Type) is
      Q : Query_Type;
   begin

      Prepare(Q,     "CREATE TABLE PASSWD (");
      Append_Line(Q, "  NAME        VARCHAR(32) NOT NULL,");
      Append_Line(Q, "  PASSWD      VARCHAR(32),");
      Append_Line(Q, "  UID         INTEGER NOT NULL,");
      Append_Line(Q, "  GID         INTEGER NOT NULL,");
      Append_Line(Q, "  GECOS_NAME  VARCHAR(80),");
      Append_Line(Q, "  GECOS_LOC   VARCHAR(80),");
      Append_Line(Q, "  GECOS_EXT   VARCHAR(8),");
      Append_Line(Q, "  GECOS_PHONE VARCHAR(20),");
      Append_Line(Q, "  DIR         VARCHAR(256) NOT NULL,");
      Append_Line(Q, "  SHELL       VARCHAR(256) NOT NULL,");
      Append_Line(Q, "    CONSTRAINT PASSWD_PKEY PRIMARY KEY (UID,NAME)");
      Append_Line(Q, ")");
      Execute_Checked(Q,C);

      Prepare(Q,     "CREATE INDEX PASSWD_X1 ON PASSWD (NAME,UID)");
      Execute_Checked(Q,C);

      Put_Line("Loading PASSWD table..");

      declare
         PW_File : File_Type;
         PW_Line : String(1..4096);
         PW_LLen : Natural;
      begin
         Open(PW_File,In_File,"./passwd");
         while not End_of_File(PW_File) loop
            Get_Line(PW_File,PW_Line,PW_LLen);
            declare
               Line :   String := PW_Line(1..PW_LLen);
               Name :   String := Parse_Token(Line,":",1);
               Passwd : String := Parse_Token(Line,":",2);
               UID :    String := Parse_Token(Line,":",3);
               GID :    String := Parse_Token(Line,":",4);
               GECOS :  String := Parse_Token(Line,":",5);
               Dir :    String := Parse_Token(Line,":",6);
               Shell :  String := Parse_Token(Line,":",7);
               GECOS_Name :   String := Parse_Token(GECOS,",",1);
               GECOS_Loc :    String := Parse_Token(GECOS,",",2);
               GECOS_Ext :    String := Parse_Token(GECOS,",",3);
               GECOS_Phone :  String := Parse_Token(GECOS,",",4);
            begin
               Prepare(Q,     "INSERT INTO PASSWD (NAME,PASSWD,UID,GID,GECOS_NAME,");
               Append_Line(Q, "            GECOS_LOC,GECOS_EXT,GECOS_PHONE,DIR,SHELL)");
               Append(Q, "VALUES (");
               Append_Quoted(Q,C,Name,",");
               Append_Quoted(Q,C,Passwd,",");
               Append_Quoted(Q,C,UID,",");
               Append_Quoted(Q,C,GID,",");
               Append_Quoted(Q,C,GECOS_Name,",");
               Append_Quoted(Q,C,GECOS_Loc,",");
               Append_Quoted(Q,C,GECOS_Ext,",");
               Append_Quoted(Q,C,GECOS_Phone,",");
               Append_Quoted(Q,C,Dir,",");
               Append_Quoted(Q,C,Shell,")" & Line_Feed);
               begin
                  Execute_Checked(Q,C);
               exception 
                  when SQL_Error =>
                     Put(Standard_Error,"Password entry ignored: ");
                     Put_Line(Standard_Error,Line);
                  when others =>
                     raise;
               end;
            end;
         end loop;
         Close(PW_File);

         Put_Line("The PASSWD table is loaded.");
      end;

   end Load_Passwd_Table;

   procedure Load_Group_Table(C : in out Connection_Type) is
      Q :   Query_Type;
      Tr :  Query_Type;
   begin

      Prepare(Q,     "CREATE TABLE GRP (");
      Append_Line(Q, "  NAME        VARCHAR(32) NOT NULL,");
      Append_Line(Q, "  PASSWD      VARCHAR(32),");
      Append_Line(Q, "  GID         INTEGER NOT NULL,");
      Append_Line(Q, "    CONSTRAINT GROUP_PKEY PRIMARY KEY (GID)");
      Append_Line(Q, ")");
      Execute_Checked(Q,C);

      Prepare(Q,     "CREATE INDEX GRP_X1 ON GRP (NAME)");
      Execute_Checked(Q,C);

      Prepare(Q,     "CREATE TABLE GRP_MEMBER (");
      Append_Line(Q, "  NAME        VARCHAR(32) NOT NULL,");
      Append_Line(Q, "  UID         INTEGER,");
      Append_Line(Q, "  GID         INTEGER NOT NULL,");
      Append_Line(Q, "    CONSTRAINT GRP_MEM_PKEY PRIMARY KEY (GID)");
      Append_Line(Q, ")");
      Execute_Checked(Q,C);

      Prepare(Q,     "CREATE INDEX GRP_MEM_X1 ON GRP_MEMBER (NAME)");
      Execute_Checked(Q,C);

      Put_Line("Loading the GRP and GRP_MEMBER tables..");

      declare
         Gr_File : File_Type;
         Gr_Line : String(1..4096);
         Gr_LLen : Natural;
      begin
         Open(Gr_File,In_File,"./group");
         while not End_of_File(Gr_File) loop
            Get_Line(Gr_File,Gr_Line,Gr_LLen);
            declare
               Line :      String := Gr_Line(1..Gr_LLen);
               Name :      String := Parse_Token(Line,":",1);
               Passwd :    String := Parse_Token(Line,":",2);
               GID :       String := Parse_Token(Line,":",3);
               Members :   String := Parse_Token(Line,":",4);
               OK :        Boolean := False;
            begin
               Prepare(Q,        "INSERT INTO GRP (NAME,PASSWD,GID)");
               Append(Q,         "VALUES (");
               Append_Quoted(Q,C,Name,",");
               Append_Quoted(Q,C,Passwd,",");
               Append_Quoted(Q,C,GID,")" & Line_Feed);
               begin
                  Execute_Checked(Q,C);
               exception
                  when SQL_Error =>
                     Put(Standard_Error,"Skipping group entry: ");
                     Put_Line(Standard_Error,Line);
                  when others =>
                     raise;
               end;
               if OK then
                  declare
                     TX : Positive := 1;
                  begin
                     while More_Tokens(Members,",",TX) loop
                        declare
                           Member : String := Parse_Token(Members,",",TX);
                        begin
                           if Member'Length > 0 then
                              Prepare(Q,        "INSERT INTO GRP_MEMBER (NAME,GID)");
                              Append(Q,         "VALUES (");
                              Append_Quoted(Q,C,Member,",");
                              Append_Quoted(Q,C,GID,")" & Line_Feed);
                           end if;
                           begin
                              Execute(Q,C);
                           exception
                              when SQL_Error =>
                                 if not Is_Duplicate_Key(Q) then
                                    Put_Line(Standard_Error,Error_Message(Q));
                                    OK := False;
                                 end if;
                              when others =>
                                 raise;
                           end;
                        end;
                        TX := TX + 1;
                     end loop;
                  end;
               end if;
            end;
         end loop;

         Close(Gr_File);
         Clear(Q);
         Clear(Tr);

         Put_Line("The GRP and GRP_MEMBER tables have been loaded from the group file.");
      end;
      
      Put_Line("Now augmenting implied group membership from PASSWD table..");

      -- Membership is also implied by the primary group of the password file :

      Prepare(Q,        "SELECT NAME,GID");
      Append_Line(Q,    "FROM PASSWD");
      Execute_Checked(Q,C);

      declare
         Q2 : Query_Type;
      begin
         while not End_of_Query(Q) loop
            Fetch(Q);
            declare
               User :   String := Value(Q,1);
               GID :    String := Value(Q,2);
            begin
               Prepare(Q2,       "INSERT INTO GRP_MEMBER (NAME,GID)");
               Append(Q2,        "VALUES (");
               Append_Quoted(Q2,C,User,",");
               Append_Quoted(Q2,C,GID,")" & Line_Feed);
               begin
                  Execute(Q2,C);
               exception
                  when SQL_Error =>
                     if not Is_Duplicate_Key(Q2) then
                        Put_Line(Standard_Error,Error_Message(Q2));
                     end if;
                  when others =>
                     raise;
               end;
            end;
         end loop;
      end;

      -- Now update the GRP_MEMBER table with UID values that correspond
      -- to the Userid names

      declare
         Q2 : Query_Type;
      begin
         Prepare(Q,        "SELECT G.NAME,G.GID,P.UID");
         Append_Line(Q,    "FROM GRP_MEMBER G LEFT OUTER JOIN PASSWD P ON ( G.NAME = P.NAME )");
         Execute_Checked(Q,C);
         
         while not End_of_Query(Q) loop
            Fetch(Q);
            declare
               function Value is new Integer_Value(PG_Integer);
               procedure Append is new Append_Integer(PG_Integer);
               Name :   String := Value(Q,1);
               GID :    PG_Integer := Value(Q,2);
               UID :    PG_Integer;
            begin
               if not Is_Null(Q,3) then
                  UID := Value(Q,3);
                  Prepare(Q2,       "UPDATE GRP_MEMBER");
                  Append_Line(Q2,   "SET");
                  Append(Q2,        "   UID = ");
                  Append(Q2,UID,Line_Feed);
                  Append(Q2,        "WHERE NAME = ");
                  Append_Quoted(Q2,C,Name);
                  Append(Q2,        " AND GID = ");
                  Append(Q2,GID,Line_Feed);
                  Execute_Checked(Q2,C);
               else
                  Put_Line("*** Userid (from group) '" & Name & "' is not known in passwd file.");
               end if;
            end;
         end loop;

      end;

      Put_Line("The PASSWD, GRP and GRP_MEMBER tables are now ready.");

   end Load_Group_Table;

end Load_Tables;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/eg/load_tables.adb,v $
