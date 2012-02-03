------------------------------------------------------------------------------
-- BUSH_OS Package Parser                                                   --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2011 Free Software Foundation              --
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

with text_io;use text_io;
with gnat.lock_files,
    world,
    scanner,
    string_util,
    parser_aux,
    parser,
    bush_os;
use gnat.lock_files,
    world,
    scanner,
    string_util,
    parser_aux,
    parser,
    bush_os;

package body parser_os is

procedure ParseOSSystem is
  -- Syntax: os.system( string );
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( os_system_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
        last_status:= aStatusCode( linux_system( to_string( expr_val ) & ascii.nul ) );
     exception when others =>
       err( "exception raised" );
     end;
  end if;
end ParseOSSystem;

procedure ParseOSStatus( result : out unbounded_string ) is
  -- Syntax: os.status
begin
  expect( os_status_t );
  result := to_unbounded_string( aStatusCode'image( last_status ) );
end ParseOSStatus;

procedure StartupBushOS is
begin
  declareProcedure( os_system_t, "os.system" );
  declareFunction( os_status_t, "os.status" );
end StartupBushOS;

procedure ShutdownBushOS is
begin
  null;
end ShutdownBushOS;

end parser_os;
