------------------------------------------------------------------------------
-- Tinyserve Package Parser                                                 --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2025 Free Software Foundation              --
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
    interfaces.c,
    pegasock.tinyserve,
    pegasoft,
    symbol_table,
    message_strings,
    scanner,
    scanner.communications,
    scanner_res,
    parser,
    parser_params;
use
    pegasock,
    pegasoft,
    pegasock.tinyserve,
    pegasoft,
    symbol_table,
    message_strings,
    scanner,
    scanner.communications,
    scanner_res,
    parser,
    parser_params;

package body parser_tinyserve is

------------------------------------------------------------------------------
-- Utility subprograms
------------------------------------------------------------------------------

procedure CheckServerIsInitialized( serverId : identifier ) is
begin
  if identifiers( serverId ).genKind = eof_t then
     err( +"new_server has not been called to initialize the file" );
  end if;
end CheckServerIsInitialized;

procedure ParseSingleServerParameter( subprogram : identifier; serverId : out identifier ) is
begin
  ParseSingleInOutParameter( subprogram, serverId, tinyserve_socket_server_t );
  CheckServerIsInitialized( serverId );
end ParseSingleServerParameter;

procedure ParseFirstServerParameter( subprogram : identifier; serverId : out identifier ) is
begin
  ParseFirstInOutParameter( subprogram, serverId, tinyserve_socket_server_t );
  CheckServerIsInitialized( serverId );
end ParseFirstServerParameter;

procedure findServer( serverExpr : unbounded_string; server : in out resPtr ) is
begin
  if length( serverExpr ) = 0 then
     err( em( "new_socket_server" ) & pl( " has not been called to initialize the connection" ) );
  else
     findResource( to_resource_id( serverExpr ), server );
  end if;
end findServer;

procedure ParseTSNewSocketServer is
  -- Syntax: tinyserve.new_socket_server( ss, t );
  -- Ada:    N/A
  resId : resHandleId;
  ref : reference;
  genKindId : identifier;
begin
  expect( tinyserve_new_socket_server_t );
  ParseFirstOutParameter( tinyserve_new_socket_server_t, ref, tinyserve_socket_server_t );
  baseTypesOK( ref.kind, tinyserve_socket_server_t );
  expectParameterComma;
  ParseIdentifier( genKindId );
  if class_ok( genKindId, typeClass, subClass ) then
     null;
  end if;
  identifiers( ref.id ).genKind := genKindId;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     identifiers( ref.id ).resource := true;
     declareResource( resId, tinyserve_socket_server, getIdentifierBlock( ref.id ) );
     AssignParameter( ref, storage'( to_unbounded_string( resId ), noMetaLabel ) );
  end if;
end ParseTSNewSocketServer;

procedure ParseTSStartUp is
  -- Syntax: tinyserve.startup( server, host, port, recv, send, queue, linger
  -- secs, usecs );
  serverId   : identifier;
  hostExpr   : storage;
  hostKind   : identifier;
  portExpr   : storage;
  portKind   : identifier;
  recvExpr   : storage;
  recvKind   : identifier;
  sendExpr   : storage;
  sendKind   : identifier;
  queueExpr  : storage;
  queueKind  : identifier;
  lingerExpr : storage;
  lingerKind : identifier;
  secsExpr   : storage;
  secsKind   : identifier;
  usecsExpr  : storage;
  usecsKind  : identifier;
  server     : resPtr := null;
begin
  if rshOpt then
     err( em( "tinyserve.startup" ) & pl( " is not permitted in a " ) &
        em( "restricted shell" ) );
  else
     expect( tinyserve_startup_t );
     ParseFirstServerParameter( tinyserve_startup_t, serverId );
     ParseNextStringParameter( tinyserve_startup_t, hostExpr, hostKind, string_t );
     ParseNextNumericParameter( tinyserve_startup_t, portExpr, portKind, integer_t );
     ParseNextNumericParameter( tinyserve_startup_t, recvExpr, recvKind, integer_t );
     ParseNextNumericParameter( tinyserve_startup_t, sendExpr, sendKind, integer_t );
     ParseNextNumericParameter( tinyserve_startup_t, queueExpr, queueKind, integer_t );
     ParseNextNumericParameter( tinyserve_startup_t, lingerExpr, lingerKind, integer_t );
     ParseNextNumericParameter( tinyserve_startup_t, secsExpr, secsKind, integer_t );
     ParseLastNumericParameter( tinyserve_startup_t, usecsExpr, usecsKind, integer_t );
-- TODO: handle defaults
-- TODO: out parameter works here?
     if isExecutingCommand then
        findServer( identifiers( serverId ).store.value, server );
        if server /= null then
           -- TODO: number conversion could throw exception
           pegasock.tinyserve.startupTinyServe(
             socket_data => server.tinyserve_server,
             host => to_string( hostExpr.value ),
             port => integer( to_numeric( portExpr.value ) ),
             min_recv_buffer_size => integer( to_numeric( recvExpr.value ) ),
             min_send_buffer_size => integer( to_numeric( sendExpr.value ) ),
             socket_queue_length => integer( to_numeric( queueExpr.value ) ),
             socket_linger_seconds => integer( to_numeric( lingerExpr.value ) ),
             timeout_secs => integer( to_numeric( secsExpr.value ) ),
             timeout_usecs => integer( to_numeric( usecsExpr.value ) )
        );
        end if;
     end if;
   end if;
end ParseTSStartUp;

procedure ParseTSShutdown is
  -- Syntax: tinyserve.shutdown( server )
  serverId   : identifier;
  server     : resPtr := null;
begin
  expect( tinyserve_shutdown_t );
  ParseSingleServerParameter( tinyserve_shutdown_t, serverId );
  if isExecutingCommand then
     findServer( identifiers( serverId ).store.value, server );
     if server /= null then
        pegasock.tinyserve.shutdownTinyServe( server.tinyserve_server );
     end if;
  end if;
end ParseTSShutdown;

procedure ParseTSManageConnections is
  -- Syntax: tinyserve.manage_connections( server, client )
  serverId   : identifier;
  server     : resPtr := null;
  clientRef  : reference;
  client     : aClientID;
begin
  expect( tinyserve_manage_connections_t );
  ParseFirstServerParameter( tinyserve_manage_connections_t, serverId );
  ParseLastOutParameter( tinyserve_manage_connections_t, clientRef, tinyserve_client_id_t );
  if isExecutingCommand then
     findServer( identifiers( serverId ).store.value, server );
     manageConnections( server.tinyserve_server, client );
     assignParameter( clientRef, storage'( to_unbounded_string( client'img ), noMetaLabel ) );
  end if;
end ParseTSManageConnections;

procedure ParseTSGetNextClient is
  -- Syntax: tinyserve.get_next_client( server, client )
  serverId   : identifier;
  server     : resPtr := null;
  clientRef  : reference;
  client     : aClientID := 0;
begin
  expect( tinyserve_get_next_client_t );
  ParseFirstServerParameter( tinyserve_get_next_client_t, serverId );
  ParseLastOutParameter( tinyserve_get_next_client_t, clientRef, tinyserve_client_id_t );
  if isExecutingCommand then
     findServer( identifiers( serverId ).store.value, server );
     if server /= null then
        getNextClient( server.tinyserve_server, client );
        assignParameter( clientRef, storage'( to_unbounded_string( client'img ), noMetaLabel ) );
     end if;
  end if;
end ParseTSGetNextClient;

procedure ParseTSGetListenerSocket is
  -- Syntax: tinyserve.get_listener_socket( server, client )
  serverId   : identifier;
  server     : resPtr := null;
  clientRef  : reference;
  client     : aClientID;
begin
  expect( tinyserve_get_listener_socket_t );
  ParseFirstServerParameter( tinyserve_get_listener_socket_t, serverId );
  ParseLastOutParameter( tinyserve_get_listener_socket_t, clientRef, tinyserve_client_id_t );
  if isExecutingCommand then
     findServer( identifiers( serverId ).store.value, server );
     if server /= null then
        getListenerSocket( server.tinyserve_server, client );
        assignParameter( clientRef, storage'( to_unbounded_string( client'img ), noMetaLabel ) );
     end if;
  end if;
end ParseTSGetListenerSocket;

procedure ParseTSCountClients( result : out storage; kind : out identifier ) is
  -- Syntax: count := tinyserve.count_clients( server )
  serverId   : identifier;
  server     : resPtr := null;
  clients    : natural;
begin
  kind := integer_t;
  expect( tinyserve_count_clients_t );
  ParseFirstServerParameter( tinyserve_count_clients_t, serverId );
  if isExecutingCommand then
     findServer( identifiers( serverId ).store.value, server );
     if server /= null then
        clients := countClients( server.tinyserve_server );
        result := storage'( to_unbounded_string( clients'img ), noMetaLabel );
     end if;
  end if;
end ParseTSCountClients;

procedure ParseTSGetFDSetSize( result : out storage; kind : out identifier ) is
  fdsize : interfaces.C.int;
begin
  kind := integer_t;
  expect( tinyserve_get_fdset_size_t );
  if isExecutingCommand then
     fdsize := getFDSetSize;
     result := storage'( to_unbounded_string( fdsize'img ), nometaLabel );
  end if;
end ParseTSGetFDSetSize;

procedure ParseTSClientMightNotBlockOnWrite( result : out storage; kind : out identifier ) is
begin
  kind := boolean_t;
  result := nullStorage;
end ParseTSClientMightNotBlockOnWrite;

procedure ParseTSEstablish is
begin
  null;
end ParseTSEstablish;

procedure ParseTSIsTimeout( result : out storage; kind : out identifier ) is
begin
  kind := boolean_t;
  result := nullStorage;
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

  declareIdent( tinyserve_socket_server_t,   "tinyserve.socket_server", variable_t, typeClass );
  identifiers( tinyserve_socket_server_t ).usage := limitedUsage;
  identifiers( tinyserve_socket_server_t ).resource := true;

  declareIdent( tinyserve_client_id_t, "tinyserve.client_id", integer_t, typeClass );

  -- TODO: allow multiple listeners
  declareProcedure( tinyserve_new_socket_server_t, "tinyserve.new_socket_server", ParseTSNewSocketServer'access );
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
