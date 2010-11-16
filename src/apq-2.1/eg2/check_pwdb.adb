-- $Id: check_pwdb.adb,v 1.2 2005/02/11 02:59:46 ken Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ.PostgreSQL.Client;
use APQ.PostgreSQL.Client;

with Check_Tables;
use Check_Tables;

procedure Check_PwDB is
   C : Connection_Type;
begin

   Set_DB_Name(C,"apq_eg");
   Set_Notify_Proc(C,Standard_Error_Notify);
   Connect(C);

   Check_Userids(C);
   Check_Gid_Refs(C);

   Disconnect(C);

end Check_PwDB;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/eg2/check_pwdb.adb,v $
