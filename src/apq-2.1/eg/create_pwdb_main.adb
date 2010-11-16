-- $Id: create_pwdb_main.adb,v 1.2 2005/02/11 02:59:46 ken Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with Ada.Text_IO;
with APQ.PostgreSQL, APQ.PostgreSQL.Client;
with Load_Tables;

procedure Create_PwDb_Main is
   use APQ.PostgreSQL, APQ.PostgreSQL.Client, Load_Tables;

   C : Connection_Type;
begin

   Set_DB_Name(C,"apq_eg");
   Set_Notify_Proc(C,Standard_Error_Notify);
   Connect(C);

   Load_Passwd_Table(C);
   Load_Group_Table(C);

   Disconnect(C);

end Create_PwDb_Main;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/eg/create_pwdb_main.adb,v $
