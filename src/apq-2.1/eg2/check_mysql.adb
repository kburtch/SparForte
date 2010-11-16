-- $Id: check_mysql.adb,v 1.2 2005/02/11 02:59:46 ken Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ.MySQL.Client;
use APQ.MySQL.Client;

with Check_Tables;
use Check_Tables;

procedure Check_MySQL is
   C : Connection_Type;
begin

   Set_DB_Name(C,"apq_eg");
   Connect(C);

   Check_Userids(C);
   Check_Gid_Refs(C);

   Disconnect(C);

end Check_MySQL;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/eg2/check_mysql.adb,v $
