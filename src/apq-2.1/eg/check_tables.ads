-- $Id: check_tables.ads,v 1.2 2005/02/11 02:59:46 ken Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ.PostgreSQL, APQ.PostgreSQL.Client;
use APQ, APQ.PostgreSQL, APQ.PostgreSQL.Client;

package Check_Tables is

   procedure Check_Userids(C : in out Connection_Type);
   procedure Check_Gid_Refs(C : in out Connection_Type);

end Check_Tables;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/eg/check_tables.ads,v $
