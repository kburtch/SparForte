-- $Id: load_tables.ads,v 1.2 2005/02/11 02:59:46 ken Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ;
use APQ;

package Load_Tables is

   procedure Load_Passwd_Table(C : in out Root_Connection_Type'Class);
   procedure Load_Group_Table(C : in out Root_Connection_Type'Class);

end Load_Tables;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/eg2/load_tables.ads,v $
