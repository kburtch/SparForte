/* $Id: notices.c,v 1.2 2005/02/11 02:59:45 ken Exp $
 * Copyright (C) 2002, Warren W. Gay VE3WWG
 *
 * Licensed under the ACL (Ada Community License)
 * or
 * GNU Public License 2 (GPL2)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
#include <stdio.h>
#include <libpq-fe.h>

/*
 * Connection_Notify is an Ada procedure using C calling convention :
 */
extern void Connection_Notify(void *arg,const char *message);

/*
 * A do-nothing notices callback :
 */
static void
notices_dud(void *arg,const char *message) {
	return;
}

/*
 * Install a new notices callback :
 */
void
notice_install(PGconn *conn,void *ada_obj_ptr) {
	PQsetNoticeProcessor(conn,Connection_Notify,ada_obj_ptr);
}

/*
 * Disable callbacks to the Connection_Notify Ada procedure :
 */
void
notice_uninstall(PGconn *conn) {
	PQsetNoticeProcessor(conn,notices_dud,NULL);
}

/* End $Source: /home/cvsroot/bush/src/apq-2.1/notices.c,v $ */
