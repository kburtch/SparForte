------------------------------------------------------------------------------
-- MySQL utilities                                                          --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2015 Free Software Foundation              --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  This is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

package pegasoft.mysql is

function mysql_kind_to_string( kind : string ) return string;
-- convert the pg_class table's pg_relkind code to a readable string

function mysql_engine_to_string( kind : string ) return string;
  -- common types

function mysql_column_type_to_string( kind, len : string ) return string;
-- convert the pg_class table's pg_relkind code to a readable string
-- adapted from PostgreSQL support

function mysql_not_null_to_string( val : string ) return string;
-- convert a t/f value to "not null" like psql client
-- adapted from PostgreSQL support

function mysql_default_to_string( val : string ) return string;
-- convert a t/f value to "not null" like psql client
-- adapted from PostgreSQL support

function mysql_userattributes_to_string( super, create : string ) return string;
-- convert t/f values to "superuser, create database" like psql client
-- adapted from PostgreSQL support

end pegasoft.mysql;
