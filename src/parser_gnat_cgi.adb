------------------------------------------------------------------------------
-- GNAT CGI Package Parser                                                  --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2017 Free Software Foundation            --
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

--with ada.text_io; use ada.text_io;

with ada.strings.unbounded,
    world,
    scanner,
    parser;
use ada.strings.unbounded,
    world,
    scanner,
    parser;

package body parser_gnat_cgi is

------------------------------------------------------------------------------
-- CGI package identifiers
------------------------------------------------------------------------------

gnat_cgi_default_header_t  : identifier;
gnat_cgi_method_type_t     : identifier;
gnat_cgi_method_type_get_t : identifier;
gnat_cgi_method_type_post_t : identifier;

gnat_cgi_metavariable_name_t : identifier;
gnat_cgi_mvn_Auth_Type_t : identifier;
gnat_cgi_mvn_Content_Length_t : identifier;
gnat_cgi_mvn_Content_Type_t : identifier;
gnat_cgi_mvn_Document_Root_t : identifier;
gnat_cgi_mvn_Gateway_Interface_t : identifier;
gnat_cgi_mvn_HTTP_Accept_t : identifier;
gnat_cgi_mvn_HTTP_Accept_Encoding_t : identifier;
gnat_cgi_mvn_HTTP_Accept_Language_t : identifier;
gnat_cgi_mvn_HTTP_Connection_t : identifier;
gnat_cgi_mvn_HTTP_Cookie_t : identifier;
gnat_cgi_mvn_HTTP_Extension_t : identifier;
gnat_cgi_mvn_HTTP_From_t : identifier;
gnat_cgi_mvn_HTTP_Host_t : identifier;
gnat_cgi_mvn_HTTP_Referer_t : identifier;
gnat_cgi_mvn_HTTP_User_Agent_t : identifier;
gnat_cgi_mvn_Path_t : identifier;
gnat_cgi_mvn_Path_Info_t : identifier;
gnat_cgi_mvn_Path_Translated_t : identifier;
gnat_cgi_mvn_Query_String_t : identifier;
gnat_cgi_mvn_Remote_Addr_t : identifier;
gnat_cgi_mvn_Remote_Host_t : identifier;
gnat_cgi_mvn_Remote_Port_t : identifier;
gnat_cgi_mvn_Remote_Ident_t : identifier;
gnat_cgi_mvn_Remote_User_t : identifier;
gnat_cgi_mvn_Request_Method_t : identifier;
gnat_cgi_mvn_Request_URI_t : identifier;
gnat_cgi_mvn_Script_Filename_t : identifier;
gnat_cgi_mvn_Script_Name_t : identifier;
gnat_cgi_mvn_Server_Addr_t : identifier;
gnat_cgi_mvn_Server_Admin_t : identifier;
gnat_cgi_mvn_Server_Name_t : identifier;
gnat_cgi_mvn_Server_Port_t : identifier;
gnat_cgi_mvn_Server_Protocol_t : identifier;
gnat_cgi_mvn_Server_Signature_t : identifier;
gnat_cgi_mvn_Server_Software_t : identifier;
gnat_cgi_put_header_t : identifier;
gnat_cgi_ok_t : identifier;
gnat_cgi_method_t : identifier;
gnat_cgi_metavariable_t : identifier;
gnat_cgi_metavariable_exists_t : identifier;
gnat_cgi_url_t : identifier;
gnat_cgi_argument_count_t : identifier;
gnat_cgi_value_t : identifier;
gnat_cgi_key_exists_t : identifier;
gnat_cgi_key_t : identifier;
gnat_cgi_cookie_put_header_t : identifier;
gnat_cgi_cookie_ok_t : identifier;
gnat_cgi_cookie_count_t : identifier;
gnat_cgi_cookie_value_t : identifier;
gnat_cgi_cookie_exists_t : identifier;
gnat_cgi_cookie_key_t : identifier;
gnat_cgi_cookie_set_t : identifier;
gnat_cgi_debug_text_output_t : identifier;
gnat_cgi_debug_html_output_t : identifier;

right_bracket : unbounded_string := to_unbounded_string( ")" );

procedure ParseSingleStringExpression( expr_val : out unbounded_string;
  expr_type : out identifier ) is
begin
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
end ParseSingleStringExpression;

------------------------------------------------------------------------------
-- PARSE THE GNAT CGI PACKAGE
------------------------------------------------------------------------------


procedure not_configured is
begin
  err( "gnat.cgi support not configured" );
end not_configured;

pragma warnings( Off );
-- Hide unused parameters warnings

procedure ParseGnatCGIPutHeader is
begin
  not_configured;
end ParseGnatCGIPutHeader;


procedure ParseGnatCGIOK( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIOK;


procedure ParseGnatCGIMethod( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIMethod;


procedure ParseGnatCGIMetavariable( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIMetavariable;


procedure ParseGnatCGIMetavariableExists( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIMetavariableExists;


procedure ParseGnatCGIURL( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIURL;


procedure ParseGnatCGIArgumentCount( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIArgumentCount;


procedure ParseGnatCGIValue( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIValue;


procedure ParseGnatCGIKeyExists( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIKeyExists;


procedure ParseGnatCGIKey( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIKey;


procedure ParseGnatCGICookiePutHeader is
begin
  not_configured;
end ParseGnatCGICookiePutHeader;


procedure ParseGnatCGICookieOK( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGICookieOK;


procedure ParseGnatCGICookieCount( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGICookieCount;


procedure ParseGnatCGICookieValue( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGICookieValue;


procedure ParseGnatCGICookieExists( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGICookieExists;


procedure ParseGnatCGICookieKey( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGICookieKey;


procedure ParseGnatCGICookieSet is
begin
  not_configured;
end ParseGnatCGICookieSet;


procedure ParseGnatCGIDebugTextOutput( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIDebugTextOutput;


procedure ParseGnatCGIDebugHTMLOutput( result : out unbounded_string; kind : out identifier ) is
begin
  not_configured;
end ParseGnatCGIDebugHTMLOutput;


------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupGnatCGI is
begin
  declareNamespace( "gnat.cgi" );
  declareStandardConstant( gnat_cgi_default_header_t, "gnat.cgi.default_header", string_t, "unknown" );
  declareIdent( gnat_cgi_method_type_t, "gnat.cgi.method_type", root_enumerated_t, typeClass );

  declareStandardConstant( gnat_cgi_method_type_get_t, "gnat.cgi.method_type.get", gnat_cgi_method_type_t, "0" );
  declareStandardConstant( gnat_cgi_method_type_post_t, "gnat.cgi.method_type.post", gnat_cgi_method_type_t, "1" );

  declareIdent( gnat_cgi_metavariable_name_t, "gnat.cgi.metavariable_name", root_enumerated_t, typeClass );
  declareStandardConstant( gnat_cgi_mvn_Auth_Type_t, "gnat.cgi.metavariable_name.auth_type", gnat_cgi_metavariable_name_t, "0" );
  declareStandardConstant( gnat_cgi_mvn_Content_Length_t, "gnat.cgi.metavariable_name.content_length", gnat_cgi_metavariable_name_t, "1" );
  declareStandardConstant( gnat_cgi_mvn_Content_Type_t,"gnat.cgi.metavariable_name.content_type",  gnat_cgi_metavariable_name_t, "2" );
  declareStandardConstant( gnat_cgi_mvn_Document_Root_t, "gnat.cgi.metavariable_name.document_root", gnat_cgi_metavariable_name_t, "3" );
  declareStandardConstant( gnat_cgi_mvn_Gateway_Interface_t, "gnat.cgi.metavariable_name.gateway_interface", gnat_cgi_metavariable_name_t, "4" );
  declareStandardConstant( gnat_cgi_mvn_HTTP_Accept_t, "gnat.cgi.metavariable_name.http_accept", gnat_cgi_metavariable_name_t, "5" );
  declareStandardConstant( gnat_cgi_mvn_HTTP_Accept_Encoding_t, "gnat.cgi.metavariable_name.http_accept_encoding", gnat_cgi_metavariable_name_t, "6" );
  declareStandardConstant( gnat_cgi_mvn_HTTP_Accept_Language_t, "gnat.cgi.metavariable_name.http_accept_language", gnat_cgi_metavariable_name_t, "7" );
  declareStandardConstant( gnat_cgi_mvn_HTTP_Connection_t, "gnat.cgi.metavariable_name.http_connection", gnat_cgi_metavariable_name_t, "8" );
  declareStandardConstant( gnat_cgi_mvn_HTTP_Cookie_t, "gnat.cgi.metavariable_name.http_cookie", gnat_cgi_metavariable_name_t, "9" );
  declareStandardConstant( gnat_cgi_mvn_HTTP_Extension_t, "gnat.cgi.metavariable_name.http_extension", gnat_cgi_metavariable_name_t, "10" );
  declareStandardConstant( gnat_cgi_mvn_HTTP_From_t, "gnat.cgi.metavariable_name.http_from", gnat_cgi_metavariable_name_t, "11" );
  declareStandardConstant( gnat_cgi_mvn_HTTP_Host_t, "gnat.cgi.metavariable_name.http_host", gnat_cgi_metavariable_name_t, "12" );
  declareStandardConstant( gnat_cgi_mvn_HTTP_Referer_t, "gnat.cgi.metavariable_name.http_referer", gnat_cgi_metavariable_name_t, "13" );
  declareStandardConstant( gnat_cgi_mvn_HTTP_User_Agent_t, "gnat.cgi.metavariable_name.http_user_agent", gnat_cgi_metavariable_name_t, "14" );
  declareStandardConstant( gnat_cgi_mvn_Path_t, "gnat.cgi.metavariable_name.path", gnat_cgi_metavariable_name_t, "15" );
  declareStandardConstant( gnat_cgi_mvn_Path_Info_t, "gnat.cgi.metavariable_name.path_info", gnat_cgi_metavariable_name_t, "16" );
  declareStandardConstant( gnat_cgi_mvn_Path_Translated_t, "gnat.cgi.metavariable_name.path_translated", gnat_cgi_metavariable_name_t, "17" );
  declareStandardConstant( gnat_cgi_mvn_Query_String_t, "gnat.cgi.metavariable_name.query_string", gnat_cgi_metavariable_name_t, "18" );
  declareStandardConstant( gnat_cgi_mvn_Remote_Addr_t, "gnat.cgi.metavariable_name.remote_addr", gnat_cgi_metavariable_name_t, "19" );
  declareStandardConstant( gnat_cgi_mvn_Remote_Host_t, "gnat.cgi.metavariable_name.remote_host", gnat_cgi_metavariable_name_t, "20" );
  declareStandardConstant( gnat_cgi_mvn_Remote_Port_t, "gnat.cgi.metavariable_name.remote_port", gnat_cgi_metavariable_name_t, "21" );
  declareStandardConstant( gnat_cgi_mvn_Remote_Ident_t, "gnat.cgi.metavariable_name.remote_ident", gnat_cgi_metavariable_name_t, "22" );
  declareStandardConstant( gnat_cgi_mvn_Remote_User_t, "gnat.cgi.metavariable_name.remote_user", gnat_cgi_metavariable_name_t, "23" );
  declareStandardConstant( gnat_cgi_mvn_Request_Method_t, "gnat.cgi.metavariable_name.request_method", gnat_cgi_metavariable_name_t, "24" );
  declareStandardConstant( gnat_cgi_mvn_Request_URI_t, "gnat.cgi.metavariable_name.request_uri", gnat_cgi_metavariable_name_t, "25" );
  declareStandardConstant( gnat_cgi_mvn_Script_Filename_t, "gnat.cgi.metavariable_name.script_filename", gnat_cgi_metavariable_name_t, "26" );
  declareStandardConstant( gnat_cgi_mvn_Script_Name_t, "gnat.cgi.metavariable_name.script_name", gnat_cgi_metavariable_name_t, "27" );
  declareStandardConstant( gnat_cgi_mvn_Server_Addr_t, "gnat.cgi.metavariable_name.server_addr", gnat_cgi_metavariable_name_t, "28" );
  declareStandardConstant( gnat_cgi_mvn_Server_Admin_t, "gnat.cgi.metavariable_name.server_admin", gnat_cgi_metavariable_name_t, "29" );
  declareStandardConstant( gnat_cgi_mvn_Server_Name_t, "gnat.cgi.metavariable_name.server_name", gnat_cgi_metavariable_name_t, "30" );
  declareStandardConstant( gnat_cgi_mvn_Server_Port_t, "gnat.cgi.metavariable_name.server_port", gnat_cgi_metavariable_name_t, "31" );
  declareStandardConstant( gnat_cgi_mvn_Server_Protocol_t, "gnat.cgi.metavariable_name.server_protocol", gnat_cgi_metavariable_name_t, "32" );
  declareStandardConstant( gnat_cgi_mvn_Server_Signature_t, "gnat.cgi.metavariable_name.server_signature", gnat_cgi_metavariable_name_t, "33" );
  declareStandardConstant( gnat_cgi_mvn_Server_Software_t, "gnat.cgi.metavariable_name.server_software", gnat_cgi_metavariable_name_t, "34" );

  declareProcedure( gnat_cgi_put_header_t, "gnat.cgi.put_header", ParseGnatCGIPutHeader'access );
  declareFunction( gnat_cgi_ok_t, "gnat.cgi.ok", ParseGnatCGIOK'access );
  declareFunction( gnat_cgi_method_t, "gnat.cgi.method", ParseGnatCGIMethod'access );
  declareFunction( gnat_cgi_metavariable_t, "gnat.cgi.metavariable", ParseGnatCGIMetavariable'access );
  declareFunction( gnat_cgi_metavariable_exists_t, "gnat.cgi.metavariable_exists", ParseGnatCGIMetavariableExists'access );
  declareFunction( gnat_cgi_url_t, "gnat.cgi.url", ParseGnatCGIURL'access );
  declareFunction( gnat_cgi_argument_count_t, "gnat.cgi.argument_count", ParseGnatCGIArgumentCount'access );
  declareFunction( gnat_cgi_value_t, "gnat.cgi.value", ParseGnatCGIValue'access );
  declareFunction( gnat_cgi_key_exists_t, "gnat.cgi.key_exists", ParseGnatCGIKeyExists'access );
  declareFunction( gnat_cgi_key_t, "gnat.cgi.key", ParseGnatCGIKey'access );

  -- Gnat.Cgi.Cookie

  declareProcedure( gnat_cgi_cookie_put_header_t, "gnat.cgi.cookie.put_header", ParseGnatCGICookiePutHeader'access );
  declareFunction( gnat_cgi_cookie_ok_t, "gnat.cgi.cookie.ok", ParseGnatCGICookieOK'access );
  declareFunction( gnat_cgi_cookie_count_t, "gnat.cgi.cookie.count", ParseGnatCGICookieCount'access );
  declareFunction( gnat_cgi_cookie_value_t, "gnat.cgi.cookie.value", ParseGnatCGICookieValue'access );
  declareFunction( gnat_cgi_cookie_exists_t, "gnat.cgi.cookie.exists", ParseGnatCGICookieExists'access );
  declareFunction( gnat_cgi_cookie_key_t, "gnat.cgi.cookie.key", ParseGnatCGICookieKey'access );
  declareProcedure( gnat_cgi_cookie_set_t, "gnat.cgi.cookie.set", ParseGnatCGICookieSet'access );

  -- Gnat.Cgi.Debug

  declareFunction( gnat_cgi_debug_text_output_t, "gnat.cgi.debug.text_output", ParseGnatCGIDebugTextOutput'access );
  declareFunction( gnat_cgi_debug_html_output_t, "gnat.cgi.debug.html_output", ParseGnatCGIDebugHTMLOutput'access );
  declareNamespaceClosed( "gnat.cgi" );
end StartupGnatCGI;

procedure ShutdownGnatCGI is
begin
  null;
end ShutdownGnatCGI;


end parser_gnat_cgi;
