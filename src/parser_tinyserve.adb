------------------------------------------------------------------------------
-- Tinyserve Package Parser                                                 --
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

--with text_io;use text_io;
with
    world,
    scanner,
    --string_util,
    --parser_aux,
    --parser,
    bush_os;
use
    world,
    scanner,
    --string_util,
    --parser_aux,
    --parser,
    bush_os;

package body parser_tinyserve is

procedure ParseTSStartUp is
begin
  null;
end ParseTSStartUp;

procedure ParseTSShutdown is
begin
  null;
end ParseTSShutdown;

procedure ParseTSManageConnections is
begin
  null;
end ParseTSManageConnections;

procedure ParseTSGetNextClient is
begin
  null;
end ParseTSGetNextClient;

procedure ParseTSGetListenerSocket is
begin
  null;
end ParseTSGetListenerSocket;

procedure ParseTSCountClients( result : out unbounded_string; kind : out identifier ) is
begin
  kind := integer_t;
  result := null_unbounded_string;
end ParseTSCountClients;

procedure ParseTSGetFDSetSize( result : out unbounded_string; kind : out identifier ) is
begin
  kind := integer_t;
  result := null_unbounded_string;
end ParseTSGetFDSetSize;

procedure ParseTSClientMightNotBlockOnWrite( result : out unbounded_string; kind : out identifier ) is
begin
  kind := boolean_t;
  result := null_unbounded_string;
end ParseTSClientMightNotBlockOnWrite;

procedure ParseTSEstablish is
begin
  null;
end ParseTSEstablish;

procedure ParseTSIsTimeout( result : out unbounded_string; kind : out identifier ) is
begin
  kind := boolean_t;
  result := null_unbounded_string;
end ParseTSIsTimeout;

procedure ParseTSClose is
begin
  null;
end ParseTSClose;

procedure ParseTSDrop is
begin
  null;
end ParseTSDrop;

procedure StartupTinyserve is
begin
  declareNamespace( "tinyserve" );
  declareProcedure( tinyserve_startup_t, "tinyserve.startup", ParseTSStartUp'access );
  declareProcedure( tinyserve_shutdown_t, "tinyserve.shutdown", ParseTSShutdown'access );
  declareProcedure( tinyserve_manage_connections_t, "tinyserve.manage_connections", ParseTSManageConnections'access );
  declareProcedure( tinyserve_get_next_client_t, "tinyserve.get_next_client", ParseTSGetNextClient'access );
  declareProcedure( tinyserve_get_listener_socket_t, "tinyserve.get_listener_socket", ParseTSGetListenerSocket'access );
  declareFunction(  tinyserve_count_clients_t, "tinyserve.count_clients", ParseTSCountClients'access );
  declareFunction(  tinyserve_get_fdset_size_t, "tinyserve.get_fdset_size", ParseTSGetFDSetSize'access );
  declareFunction(  tinyserve_client_might_not_block_on_write_t, "tinyserve.client_might_not_block_on_write", ParseTSClientMightNotBlockOnWrite'access );
  declareProcedure( tinyserve_establish_t, "tinyserve.establish", ParseTSEstablish'access );
  declareFunction(  tinyserve_is_timeout_t, "tinyserve.is_timeout", ParseTSIsTimeout'access );
  declareProcedure( tinyserve_close_t, "tinyserve.close", ParseTSClose'access );
  declareProcedure( tinyserve_drop_t, "tinyserve.drop", ParseTSDrop'access );

  declareNamespaceClosed( "tinyserve" );
end StartupTinyserve;

procedure ShutdownTinyserve is
begin
  null;
end ShutdownTinyserve;

end parser_tinyserve;
