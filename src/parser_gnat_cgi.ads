------------------------------------------------------------------------------
-- GNAT CGI Package Parser                                                  --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2013 Free Software Foundation            --
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

with ada.strings.unbounded, world;
use  ada.strings.unbounded, world;

-- GCC Ada 4.3.1 will throw an exception if BUSH isn't running as a CGI program
-- (This is, of course, a mistake in the design of Gnat.CGI, not BUSH.)
--with gnat.cgi;

package parser_gnat_cgi is

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

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupGnatCGI;
procedure ShutdownGnatCGI;

------------------------------------------------------------------------------
-- PARSE THE CGI PACKAGE
------------------------------------------------------------------------------

procedure ParseGnatCGIPutHeader;
procedure ParseGnatCGIOK( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGIMethod( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGIMetavariable( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGIMetavariableExists( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGIURL( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGIArgumentCount( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGIValue( result : out unbounded_string; kind : out identifier );
procedure parseGnatCGIKeyExists( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGIKey( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGICookiePutHeader;
procedure ParseGnatCGICookieOK( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGICookieCount( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGICookieValue( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGICookieExists( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGICookieKey( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGICookieSet;
procedure ParseGnatCGIDebugTextOutput( result : out unbounded_string; kind : out identifier );
procedure ParseGnatCGIDebugHTMLOutput( result : out unbounded_string; kind : out identifier );

end parser_gnat_cgi;
