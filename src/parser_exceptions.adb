------------------------------------------------------------------------------
-- Exceptions Package Parser                                                --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2016 Free Software Foundation              --
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

with text_io;use text_io;
with
    world,
    scanner,
    string_util,
    parser_aux,
    parser,
    bush_os;
use
    world,
    scanner,
    string_util,
    parser_aux,
    parser,
    bush_os;

package body parser_exceptions is

------------------------------------------------------------------------------
-- Exceptions package identifiers
------------------------------------------------------------------------------

exceptions_exception_name_t : identifier;
exceptions_exception_info_t : identifier;
exceptions_exception_status_code_t : identifier;

procedure ParseExceptionsExceptionName( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: s := exceptions.exception_name
  -- Ada:    s := exceptions.exception_name( id/occur )
begin
  kind := string_t;
  expect( exceptions_exception_name_t );
  if isExecutingCommand then
     if err_exception.deleted then
        result := null_unbounded_string;
     else
        result := err_exception.name;
     end if;
  end if;
end ParseExceptionsExceptionName;

procedure ParseExceptionsExceptionInfo( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: s := Exception_info();
  -- Ada:    s := exceptions.exception_info( occur )
begin
  kind := string_t;
  expect( exceptions_exception_info_t );
  if isExecutingCommand then
     if err_exception.deleted then
        result := null_unbounded_string;
     else
        result := fullErrorMessage;
     end if;
  end if;
end ParseExceptionsExceptionInfo;

procedure ParseExceptionsExceptionStatusCode( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: s := exceptions.exception_status_code
  -- Ada:    N/A
begin
  kind := natural_t;
  expect( exceptions_exception_status_code_t );
  if isExecutingCommand then
     if err_exception.deleted then
        result := to_unbounded_string( "0" );
     else
        result := to_unbounded_string( long_float( character'pos( element( err_exception.value.all, 1 ) ) ) );
     end if;
  end if;
end ParseExceptionsExceptionStatusCode;

procedure StartupExceptions is
begin
  declareNamespace( "exceptions" );
  declareFunction( exceptions_exception_name_t, "exceptions.exception_name", ParseExceptionsExceptionName'access );
  declareFunction( exceptions_exception_info_t, "exceptions.exception_info", ParseExceptionsExceptionInfo'access );
  declareFunction( exceptions_exception_status_code_t, "exceptions.exception_status_code", ParseExceptionsExceptionStatusCode'access );
  declareNamespaceClosed( "exceptions" );
end StartupExceptions;

procedure ShutdownExceptions is
begin
  null;
end ShutdownExceptions;

end parser_exceptions;
