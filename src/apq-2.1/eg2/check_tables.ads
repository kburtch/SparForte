-- $Id: check_tables.ads,v 1.2 2005/02/11 02:59:46 ken Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ;
use APQ;

package Check_Tables is

   procedure Check_Userids(C : in out Root_Connection_Type'Class);
   procedure Check_Gid_Refs(C : in out Root_Connection_Type'Class);

end Check_Tables;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/eg2/check_tables.ads,v $
