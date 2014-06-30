/* $Id: mysqlopts.c,v 1.2 2005/02/11 02:59:45 ken Exp $
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
#include <string.h>
#include <mysql.h>

typedef struct {
	char	*name;		/* Option string name */
	int	value;		/* Option enum value */
	char	*atype;		/* Option argument type */
} option_type;

int
main(int argc,char **argv) {
	static option_type options[] = {
		{ "CONNECT_TIMEOUT",	MYSQL_OPT_CONNECT_TIMEOUT,	"ARG_UINT" },
		{ "COMPRESS",		MYSQL_OPT_COMPRESS,		"ARG_NOT_USED" },
		{ "NAMED_PIPE",		MYSQL_OPT_NAMED_PIPE,		"ARG_NOT_USED" },
		{ "INIT_COMMAND",	MYSQL_INIT_COMMAND,		"ARG_CHAR_PTR" },
		{ "READ_DEFAULT_FILE",	MYSQL_READ_DEFAULT_FILE,	"ARG_CHAR_PTR" },
		{ "READ_DEFAULT_GROUP",	MYSQL_READ_DEFAULT_GROUP,	"ARG_CHAR_PTR" },
		{ "SET_CHARSET_DIR",	MYSQL_SET_CHARSET_DIR,		"ARG_CHAR_PTR" },
		{ "SET_CHARSET_NAME",	MYSQL_SET_CHARSET_NAME,		"ARG_CHAR_PTR" },
		{ "LOCAL_INFIL",	MYSQL_OPT_LOCAL_INFILE,		"ARG_PTR_UINT" }
	};
	int x;

	for ( x=0; x<sizeof options/sizeof options[0]; ++x ) {
		printf("      (\n");
		printf("        Name       => \"%-18.18s\",\n",options[x].name);
		printf("        Length     => %d,\n",(int)strlen(options[x].name));
		printf("        MySQL_Enum => %u,\n",options[x].value);
		printf("        Argument   => %s\n",options[x].atype);
		printf("      )%s\n",x+1==(sizeof options/sizeof options[0])?"":",");
	}

	return 0;
}
