------------------------------------------------------------------------------
-- PostgreSQL utilities  r                                                  --
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

--with text_io;use text_io;
--with ada.io_exceptions,
--     ada.strings.unbounded,
--     APQ.PostgreSQL.Client,
--     bush_os.tty,
--     world,
--     signal_flags,
--     string_util,
--     user_io,
--     scanner,
--     parser,
--     parser_aux,
--     parser_params;
--use  ada.io_exceptions,
--     ada.strings.unbounded,
--     APQ,
--     APQ.PostgreSQL,
--     APQ.PostgreSQL.Client,
--     bush_os.tty,
--     world,
--     signal_flags,
--     string_util,
--     user_io,
--     scanner,
--     parser,
--     parser_aux,
--     parser_params;

package body utils_pg is

function pg_kind_to_string( kind : string ) return string is
-- convert the pg_class table's pg_relkind code to a readable string
begin
  if kind = "r" then
     return "table";
  elsif kind = "i" then
     return "index";
  elsif kind = "S" then
     return "sequence";
  elsif kind = "v" then
     return "view";
  elsif kind = "c" then
     return "composite type";
  elsif kind = "s" then
     return "special";
  elsif kind = "t" then
     return "TOAST table";
  end if;
  return "kind code " & kind;
end pg_kind_to_string;

function pg_column_type_to_string( kind, len : string ) return string is
-- convert the pg_class table's pg_relkind code to a readable string
begin
  if kind = "bpchar" then              -- blank-padded character array
     return "character(" & len & ")";  -- is char(n)
  elsif kind = "int4" then             -- 4-byte integer
     return "integer";                 -- is integer
  elsif kind = "varchar" then          -- varchar has a
     return "character varying(" & len & ")";   -- length
  elsif kind = "interval" then
     return kind;
  elsif kind = "timestamp" then
     return "timestamp without time zone";
  elsif kind = "int8" then
     return "bigint";
  elsif kind = "serial8" then
     return "bigserial";
  elsif kind = "bit" then
     return kind;
  elsif kind = "varbit" then
     return "bit varying(" & len & ")";   -- length
  elsif kind = "bool" then
     return "boolean";
  elsif kind = "box" then
     return kind;
  elsif kind = "bytea" then
     return kind;
  elsif kind = "cidr" then
     return kind;
  elsif kind = "circle" then
     return kind;
  elsif kind = "date" then
     return kind;
  elsif kind = "float8" then
     return "double precision";
  elsif kind = "inet" then
     return kind;
  elsif kind = "line" then
     return kind;
  elsif kind = "lseg" then
     return kind;
  elsif kind = "macaddr" then
     return kind;
  elsif kind = "money" then
     return kind;
  elsif kind = "decimal" then
     return kind;
  elsif kind = "path" then
     return kind;
  elsif kind = "point" then
     return kind;
  elsif kind = "polygon" then
     return kind;
  elsif kind = "float4" then
     return "real";
  elsif kind = "int2" then
     return "smallint";
  elsif kind = "serial4" then
     return "serial";
  elsif kind = "text" then
     return kind;
  elsif kind = "timetz" then
     return "time with time zone";
  elsif kind = "timestamptz" then
     return "timestamp with time zone";
  end if;
  return "type code " & kind;
end pg_column_type_to_string;

function pg_not_null_to_string( val : string ) return string is
-- convert a t/f value to "not null" like psql client
begin
  if val = "t" then
     return "not null";
  end if;
  return "";
end pg_not_null_to_string;

function pg_default_to_string( val : string ) return string is
-- convert a t/f value to "not null" like psql client
begin
  if val = "t" then
     return "default";
  end if;
  return "";
end pg_default_to_string;

function pg_userattributes_to_string( super, create : string ) return string is
-- convert t/f values to "superuser, create database" like psql client
begin
  if super = "t" and create = "t" then
     return "superuser, create database";
  elsif super = "t" then
     return "superuser";
  elsif create = "t" then
     return "create database";
  end if;
  return "";
end pg_userattributes_to_string;

end utils_pg;

