/* $Id: c_mysql.c,v 1.2 2005/02/11 02:59:44 ken Exp $
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
#include <my_global.h>
#include <my_sys.h>
#ifdef _WINDOWS
#include <my_global.h>
#endif
#include <mysql.h>
#include <errmsg.h>
#include <string.h>

#define IS_OK(z) (!(z) ? 1 : 0)

#ifdef _WINDOWS

/*
 * Blinken Windows does not have a strcasecmp() :
 */
#include <ctype.h>

#define EXPORT __declspec(dllexport)

static int
strcasecmp(const char *s1, const char *s2) {
	char c1, c2;

	while ( *s1 ) {
		c1 = *s1++;
		c2 = *s2++;
		if ( !c1 || !c2 )
			break;
		if ( islower(c1) )
			c1 = toupper(c1);
		if ( islower(c2) )
			c2 = toupper(c2);
		if ( c1 != c2 )
			return c1 < c2 ? -1 : 1;
	}

	if ( !c1 && !c2 )
		return 0;
	return !c1 ? -1 : 1;
}

int __stdcall
DllMain(HANDLE inst,DWORD reason_called,LPVOID reserved) {
	static inited = 0;
	HINSTANCE NEAR module;

	switch( reason_called ) {
	case DLL_PROCESS_ATTACH :
		if ( !inited++ )
			module = inst;
		break;
	case DLL_THREAD_ATTACH :
		break;
	case DLL_THREAD_DETACH :
		break;
	case DLL_PROCESS_DETACH :
		break;
	default :
		break;
	}

	return TRUE;
}

#else
#define EXPORT
#endif

EXPORT MYSQL *
c_mysql_init() {
	MYSQL *conn = mysql_init(NULL);

	/* Set any default options on conn here */
	return conn;
}

EXPORT int
c_mysql_connect(MYSQL *conn,char *host,char *user,char *passwd,char *db,unsigned port,char *local_socket) {
	int z;

	z = mysql_real_connect(conn,host,user,passwd,db,port,local_socket,0) == NULL;
	return IS_OK(z);
}

EXPORT MYSQL *
c_mysql_close(MYSQL *conn) {

	mysql_close(conn);
	return NULL;
}

EXPORT unsigned int
c_mysql_errno(MYSQL *conn) {
	return mysql_errno(conn);
}

EXPORT char *
c_mysql_error(MYSQL *conn) {
	return (char *) mysql_error(conn);
}

EXPORT char *
c_mysql_unix_socket(MYSQL *conn) {
	return conn->unix_socket ? conn->unix_socket : "";
}

EXPORT unsigned int
c_mysql_port(MYSQL *conn) {
	return conn->port;
}

EXPORT int
c_mysql_select_db(MYSQL *conn,const char *database) {
	int z;

	z = mysql_select_db(conn,database);
	return IS_OK(z);
}

EXPORT char *
c_mysql_get_host_name(MYSQL *conn) {
	return conn->host;
}

EXPORT int
c_mysql_query(MYSQL *conn,char *query) {
	int z;

	z = mysql_query(conn,query);
	return IS_OK(z);
}

EXPORT MYSQL_FIELD *
c_mysql_fetch_field(MYSQL_RES *result,int fieldno) {
	int count = mysql_num_fields(result);

	if ( fieldno >= count )
		return NULL;
	return mysql_fetch_field_direct(result,(unsigned)fieldno);
}


EXPORT char *
c_mysql_field_name(MYSQL_FIELD *field) {
	return field->name;
}

EXPORT int
c_mysql_field_type(MYSQL_FIELD *field) {
	return (int)field->type;
}

EXPORT MYSQL_ROW
c_mysql_fetch_row_direct(MYSQL_RES *result,my_ulonglong row_no) {
	my_ulonglong count = mysql_num_rows(result);

	if ( row_no >= count )
		return NULL;

	mysql_data_seek(result,row_no);

	return mysql_fetch_row(result);
}

EXPORT const char *
c_mysql_field_value(MYSQL_RES *result,MYSQL_ROW row,int fieldno) {
	int count = mysql_num_fields(result);

	if ( !row )
		return NULL;
	if ( fieldno >= count )
		return NULL;
	return (const char *)row[fieldno];
}

EXPORT const char *
c_mysql_sqlstate(MYSQL *conn) {
#ifdef mysql_sqlstate
	return mysql_sqlstate(conn);			/* Version 4.1.1 and later */
#else
	return "?????";
#endif
}

EXPORT int
c_mysql_eof(MYSQL_RES *result) {
	return mysql_eof(result) != 0 ? 1 : 0;
}

EXPORT int
c_mysql_name_index(MYSQL_RES *results,const char *name) {
	unsigned n = mysql_num_fields(results);			/* # of fields in result */
	MYSQL_FIELD *fields = mysql_fetch_fields(results);	/* List of fields */
	unsigned x;

	if ( !fields || n < 1 )
		return -1;

	for ( x=0; x<n; ++x )
		if ( !strcasecmp(fields[x].name,name) )		/* Caseless compare */
			return x;				/* Found match at index x */
	return -1;
}

EXPORT int
c_mysql_get_field_type(MYSQL_RES *results,int colx) {
	MYSQL_FIELD *fields = mysql_fetch_fields(results);	/* List of fields */

	return fields[colx].type;				/* Return field type */
}

EXPORT int
c_cheat_mysql_errno(MYSQL_RES *results) {
	MYSQL *conn = results->handle;                         /* CHEAT */

	if ( !conn )
		return 0;                                      /* CR_NO_RESULT */
	return mysql_errno(conn);
}

EXPORT const char *
c_cheat_mysql_error(MYSQL_RES *results) {
	MYSQL *conn = results->handle;                         /* CHEAT */

	if ( !conn )
		return "";
	else	return mysql_error(conn);
}

EXPORT int
c_mysql_options_notused(MYSQL *conn,unsigned option) {
	enum mysql_option opt = option;

	return mysql_options(conn,opt,0);
}

EXPORT int
c_mysql_options_uint(MYSQL *conn,unsigned option,unsigned arg) {
	enum mysql_option opt = option;

	return mysql_options(conn,opt,(char *)&arg);
}

EXPORT int
c_mysql_options_puint(MYSQL *conn,unsigned option,unsigned arg) {
	enum mysql_option opt = option;
	static const unsigned my_true = 1;
	static const unsigned my_false = 0;

	return mysql_options(conn,opt,(char *)(arg ? &my_true : &my_false));
}

EXPORT int
c_mysql_options_char(MYSQL *conn,unsigned option,char *arg) {
	enum mysql_option opt = option;

	return mysql_options(conn,opt,arg);
}

/************************************************/
/* These are only wrappers, to allow for the    */
/* fact that the DLL entry points may someday   */
/* use incompatible calling conventions. It     */
/* naturally works OK from C, because it has    */
/* the advantage of using the header files.     */
/************************************************/

EXPORT unsigned
c_mysql_field_count(MYSQL *mysql) {
	return mysql_field_count(mysql);
}

EXPORT my_ulonglong
c_mysql_num_rows(MYSQL_RES *result) {
	return mysql_num_rows(result);
}

EXPORT unsigned
c_mysql_num_fields(MYSQL_RES *result) {
	return mysql_num_fields(result);
}

EXPORT MYSQL_RES *
c_mysql_store_result(MYSQL *mysql) {
	return mysql_store_result(mysql);
}

EXPORT MYSQL_RES *
c_mysql_use_result(MYSQL *mysql) {
	return mysql_use_result(mysql);
}

EXPORT MYSQL_ROW
c_mysql_fetch_row(MYSQL_RES *result) {
	return mysql_fetch_row(result);
}

EXPORT void
c_mysql_free_result(MYSQL_RES *result) {
	mysql_free_result(result);
}

EXPORT void
c_my_init(void) {
	my_init();
}

EXPORT my_ulonglong
c_mysql_insert_id(MYSQL *mysql) {
	return mysql_insert_id(mysql);
}

EXPORT MYSQL_FIELD *
c_mysql_fetch_field_direct(MYSQL_RES *result,unsigned fieldno) {
	return mysql_fetch_field_direct(result,fieldno);
}

EXPORT unsigned long
c_mysql_real_escape_string(MYSQL *mysql,char *to,const char *from,unsigned long length) {
	return mysql_real_escape_string(mysql,to,from,length);
}

/* End $Source: /home/cvsroot/bush/src/apq-2.1/c_mysql.c,v $ */
