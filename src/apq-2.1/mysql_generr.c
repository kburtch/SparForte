/* $Id: mysql_generr.c,v 1.2 2005/02/11 02:59:45 ken Exp $
 * Copyright (c) 2003, Warren W. Gay VE3WWG
 *
 * Licensed under the ACL (Ada Community License)
 * or
 * GNU Public License 2 (GPL2)
 * 
 *     This program is free software; you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation; either version 2 of the License, or
 *     (at your option) any later version.
 * 
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 * 
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <stdio.h>
#include <errmsg.h>
#include <mysqld_error.h>

typedef struct {
	char     *name;      /* Eg. CR_UNKNOWN_ERROR */
	int      code;       /* Error code */
} ErrCode;

int
main(int argc,char **argv) {
	static ErrCode codes[] = {
#include "mysql_errmsg.h"
	};
	int x;

	for ( x=0; codes[x].name != 0; ++x )
		printf("%d %s\n",codes[x].code,codes[x].name);
	return 0;
}

/* End $Source: /home/cvsroot/bush/src/apq-2.1/mysql_generr.c,v $ */
