------------------------------------------------------------------------------
-- CGI Package Parser                                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2013 Free Software Foundation              --
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

with ada.strings.unbounded, cgi, world;
use  ada.strings.unbounded, world;

package parser_cgi is

------------------------------------------------------------------------------
-- CGI package identifiers
------------------------------------------------------------------------------

cgi_cgi_method_type_t  : identifier;  -- CGI_Method_Type enumerated
cgi_get_t              : identifier;
cgi_post_t             : identifier;
cgi_unknown_t          : identifier;
cgi_parsing_errors_t   : identifier;
cgi_input_received_t   : identifier;
cgi_is_index_t         : identifier;
cgi_cgi_method_t       : identifier;
cgi_value_t            : identifier;
cgi_key_exists_t       : identifier;
cgi_key_count_t        : identifier;
cgi_argument_count_t   : identifier;
cgi_key_t              : identifier;
cgi_key_value_t        : identifier;
cgi_key_value_exists_t : identifier;
cgi_put_cgi_header_t   : identifier;
cgi_put_html_head_t    : identifier;
cgi_put_html_heading_t : identifier;
cgi_put_html_tail_t    : identifier;
cgi_put_error_message_t : identifier;
cgi_put_variables_t    : identifier;
cgi_my_url_t           : identifier;
cgi_get_environment_t  : identifier;
cgi_line_count_t       : identifier;
cgi_line_count_of_value_t : identifier;
cgi_line_t             : identifier;
cgi_value_of_line_t    : identifier;
cgi_url_decode_t       : identifier;
cgi_url_encode_t       : identifier;
cgi_html_encode_t      : identifier;
cgi_set_cookie_t       : identifier;
cgi_cookie_value_t     : identifier;
cgi_cookie_count_t     : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupCGI;
procedure ShutdownCGI;

------------------------------------------------------------------------------
-- PARSE THE CGI PACKAGE
------------------------------------------------------------------------------

-- procedure ParseParsing_Errors( result : out unbounded_string; kind : out identifier );
-- procedure ParseInput_Received( result : out unbounded_string; kind : out identifier );
-- procedure ParseIs_Index( result : out unbounded_string; kind : out identifier );
-- procedure ParseCGI_Method( result : out unbounded_string; kind : out identifier );
-- procedure ParseValue( result : out unbounded_string; kind : out identifier );
-- procedure ParseKey_Exists( result : out unbounded_string; kind : out identifier );
-- procedure ParseKey_Count( result : out unbounded_string; kind : out identifier );
-- procedure ParseCGIArgument_Count( result : out unbounded_string; kind : out identifier );
-- procedure ParseKey( result : out unbounded_string; kind : out identifier );
-- procedure ParseKeyValue( result : out unbounded_string; kind : out identifier );
-- procedure ParseKey_Value_Exists( result : out unbounded_string; kind : out identifier );
-- procedure ParsePut_CGI_Header;
-- procedure ParsePut_HTML_Head;
-- procedure ParsePut_HTML_Heading;
-- procedure ParsePut_HTML_Tail;
-- procedure ParsePut_Error_Message;
-- procedure ParsePut_Variables;
-- procedure ParseMy_URL( result : out unbounded_string; kind : out identifier );
-- procedure ParseLine_Count( result : out unbounded_string; kind : out identifier );
-- procedure ParseLine_Count_Of_Value( result : out unbounded_string; kind : out identifier );
-- procedure ParseCGILine( result : out unbounded_string; kind : out identifier );
-- procedure ParseValue_Of_Line( result : out unbounded_string; kind : out identifier );
-- procedure ParseURL_Decode( result : out unbounded_string; kind : out identifier );
-- procedure ParseURL_Encode( result : out unbounded_string; kind : out identifier );
-- procedure ParseHTML_Encode( result : out unbounded_string; kind : out identifier );
-- procedure ParseSet_Cookie;
-- procedure ParseCookie_Value( result : out unbounded_string; kind : out identifier );
-- procedure ParseCookie_Count( result : out unbounded_string; kind : out identifier );

end parser_cgi;
