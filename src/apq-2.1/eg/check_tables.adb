-- $Id: check_tables.adb,v 1.2 2005/02/11 02:59:46 ken Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with Ada.Text_IO;
use Ada.Text_IO;

package body Check_Tables is

   procedure Check_Userids(C : in out Connection_Type) is
      Q : APQ.PostgreSQL.Client.Query_Type;
   begin

      Prepare(Q,     "SELECT NAME,GECOS_NAME,SHELL");
      Append_Line(Q, "FROM PASSWD");
      Append_Line(Q, "WHERE UID = 0");
      Append_Line(Q, "ORDER BY 1");
      Execute_Checked(Q,C);
      
      Put_Line("ROOT USER ACCOUNTS ARE:");
      New_Line;
      Put_Line("Account      User Name                                Shell");
      Put_Line("------------ ---------------------------------------- --------------------");

      while not End_of_Query(Q) loop
         Fetch(Q);
         declare
            Name :         String(1..12);
            GECOS_Name :   String(1..40);
            Shell :        String := Value(Q,3);
         begin
            Value(Q,1,Name);
            Value(Q,2,GECOS_Name);
            Put(Name);
            Put(" ");
            Put(GECOS_Name);
            Put(" ");
            Put_Line(Shell);
         end;
      end loop;
      Clear(Q);
      New_Line;

   end Check_Userids;

   -- In this procedure we produce a complete list of GID
   -- values referenced in the PASSWD table. We check the
   -- GRP table to make certain they are all registered.

   procedure Check_Gid_Refs(C : in out Connection_Type) is
      Q : Query_Type;
   begin

      Prepare(Q,        "SELECT DISTINCT P.GID, G.NAME");
      Append_Line(Q,    "FROM PASSWD P LEFT OUTER JOIN GRP G ON ( P.GID = G.GID )");
      Append_Line(Q,    "ORDER BY 1");

      Execute_Checked(Q,C);   

      declare
         function Value is new Integer_Value(PG_Integer);
         package GIDIO is new Ada.Text_IO.Integer_IO(PG_Integer);
         GID :          PG_Integer;
         Undeclared :   Natural := 0;
         Need_Hdr :     Boolean := True;
      begin
         while not End_of_Query(Q) loop
            Fetch(Q);
            GID := Value(Q,1);
            if Is_Null(Q,2) then
               Undeclared := Undeclared + 1;
               if Need_Hdr then
                  Put_Line("THE FOLLOWING GID VALUES LACK A DEFINED GROUP:");
                  Need_Hdr := False;
               end if;

               New_Line;
               Put("  ");
               GIDIO.Put(Item=>GID,Width=>0,Base=>10);
               Put_Line(" IS REFERENCED BY:");
               New_Line;
               Put_Line("  USERID       GECOS NAME");
               Put_Line("  ------------ ------------------------------");

               declare
                  procedure Append is new Append_Integer(PG_Integer);
                  Q2 : Query_Type;
               begin
                  Prepare(Q2,    "SELECT NAME,GECOS_NAME");
                  Append_Line(Q2,"FROM PASSWD");
                  Append(Q2,     "WHERE GID=");
                  Append(Q2,GID,Line_Feed);
                  Append_Line(Q2,"ORDER BY NAME");
                  Execute_Checked(Q2,C);

                  while not End_of_Query(Q2) loop
                     Fetch(Q2);
                     declare
                        Userid :    String(1..12);
                        Name :      String := Value(Q2,2);
                     begin            
                        Value(Q2,1,Userid);
                        Put("  ");
                        Put(Userid);
                        Put(" ");
                        Put_Line(Name);
                     end;
                  end loop;
               end;
            end if;
         end loop;
         if Undeclared = 0 then
            Put_Line("OK: All GID values referenced in passwd are defined in group.");
         else
            Put_Line("*** The group file is missing group entries (see above).");
         end if;
         New_Line;
      end;

   end Check_Gid_Refs;

end Check_Tables;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/eg/check_tables.adb,v $
