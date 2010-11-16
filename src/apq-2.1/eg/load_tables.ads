-- $Id: load_tables.ads,v 1.2 2005/02/11 02:59:46 ken Exp $
-- Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)

with APQ.PostgreSQL, APQ.PostgreSQL.Client;
use APQ.PostgreSQL, APQ.PostgreSQL.Client;

package Load_Tables is

   procedure Load_Passwd_Table(C : in out Connection_Type);
   procedure Load_Group_Table(C : in out Connection_Type);

end Load_Tables;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/eg/load_tables.ads,v $
