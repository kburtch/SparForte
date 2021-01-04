------------------------------------------------------------------------------
-- OS Package Parser                                                        --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
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
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

--with text_io;use text_io;

with ada.strings.unbounded,
    world,
    scanner,
    parser_params,
    parser_aux,
    spar_os;
use ada.strings.unbounded,
    world,
    scanner,
    parser_params,
    parser_aux,
    spar_os;

package body parser_os is

procedure ParseOSSystem is
  -- Syntax: os.system( string );
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( os_system_t );
  ParseSingleStringParameter( expr_val, expr_type, string_t );
  if isExecutingCommand then
     begin
        last_status:= aStatusCode( linux_system( to_string( expr_val ) & ascii.nul ) );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseOSSystem;

procedure ParseOSStatus( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: os.status
begin
  kind := integer_t;
  expect( os_status_t );
  if isExecutingCommand then
     result := to_unbounded_string( aStatusCode'image( last_status ) );
  end if;
end ParseOSStatus;

procedure ParseOSPid( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: os.pid
begin
  kind := natural_t;
  expect( os_pid_t );
  if isExecutingCommand then
     result := to_unbounded_string( aPID'image( getpid ) );
  end if;
end ParseOSPid;

procedure ParseOSErrorString( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: os.error_string
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := string_t;
  expect( os_error_string_t );
  ParseSingleNumericParameter( expr_val, expr_type, integer_t );
  if isExecutingCommand then
     begin
        result := to_unbounded_string( OSerror( integer( to_numeric( expr_val ) ) ) );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseOSErrorString;

procedure ParseOSLastChild( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: os.last_child
begin
  kind := natural_t;
  expect( os_last_child_t );
  if isExecutingCommand then
     result := to_unbounded_string( aPID'image( lastChild ) );
  end if;
end ParseOSLastChild;

procedure StartupSparOS is
begin
  declareNamespace( "os" );
  declareFunction( os_error_string_t, "os.error_string", ParseOSErrorString'access );
  declareFunction( os_pid_t, "os.pid", ParseOSPid'access );
  declareFunction( os_status_t, "os.status", ParseOSStatus'access );
  declareProcedure( os_system_t, "os.system", ParseOSSystem'access );
  declareFunction( os_last_child_t, "os.last_child", ParseOSLastChild'access );
  declareNamespaceClosed( "os" );
end StartupSparOS;

procedure ShutdownSparOS is
begin
  null;
end ShutdownSparOS;

end parser_os;
