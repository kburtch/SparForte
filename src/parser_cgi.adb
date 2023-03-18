------------------------------------------------------------------------------
-- CGI Package Parser                                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
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

--with ada.text_io;use ada.text_io;

with ada.strings.unbounded,
    cgi,
    pegasoft,
    world,
    scanner,
    scanner.communications,
    parser;
use ada.strings.unbounded,
    world,
    pegasoft,
    scanner,
    scanner.communications,
    parser;

package body parser_cgi is

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
--cgi_get_environment_t  : identifier;
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


-----------------------------------------------------------------------------
--  PARSE PARSING ERRORS
--
-- cgi.parsing_errors return boolean
-- True if Error on Parse.
-----------------------------------------------------------------------------

procedure ParseParsing_Errors( result : out unbounded_string; kind : out identifier ) is
begin
  kind := boolean_t;
  expect( cgi_parsing_errors_t );
  if isExecutingCommand then
     result := to_bush_boolean( cgi.parsing_errors );
  end if;
end ParseParsing_Errors;


-----------------------------------------------------------------------------
--  PARSE INPUT RECEIVED
--
-- cgi.input_received return boolean
-- True if Input Received.
-----------------------------------------------------------------------------

procedure ParseInput_Received( result : out unbounded_string; kind : out identifier ) is
begin
  kind := boolean_t;
  expect( cgi_input_received_t );
  if isExecutingCommand then
     result := to_bush_boolean( cgi.input_received );
  end if;
end ParseInput_Received;


-----------------------------------------------------------------------------
--  PARSE IS INDEX
--
-- cgi.is_index return boolean
-- True if an Isindex request made. An "Isindex" request is turned
-- into a Key of "isindex" at position 1, with Value(1) as the actual
-- query.
-------------------------------------------------------------------------------

procedure ParseIs_Index( result : out unbounded_string; kind : out identifier ) is
begin
  kind := boolean_t;
  expect( cgi_is_index_t );
  if isExecutingCommand then
     result := to_bush_boolean( cgi.is_index );
  end if;
end ParseIs_Index;


-----------------------------------------------------------------------------
--  PARSE CGI METHOD
--
-- cgi.cgi_method return CGI_Method_Type
-- Report the CGI Method; where possible, don't depend on this.
-- type CGI_Method_Type is (Get, Post, Unknown);
-----------------------------------------------------------------------------

procedure ParseCGI_Method( result : out unbounded_string; kind : out identifier ) is
begin
  kind := cgi_cgi_method_type_t;
  expect( cgi_cgi_method_t );
  if isExecutingCommand then
     result := To_Unbounded_String( integer'image( cgi.cgi_method_type'pos( cgi.CGI_Method ) )(2)&"" );
  end if;
end ParseCGI_Method;


-----------------------------------------------------------------------------
--  PARSE VALUE
--
-- function ParseValue(Key : in Unbounded_String; Index : in Positive := 1;
--               Required : in Boolean := False) return Unbounded_String is
-- Access data as an associative array - given a key, return its value.
-- The Key value is case-sensitive.
-- If a key is required but not present, raise Constraint_Error;
-- otherwise a missing key's value is considered to be "".
-- These routines find the Index'th value of that key (normally the first one).
-----------------------------------------------------------------------------

procedure ParseValue( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string := to_unbounded_string( " 1" ); -- default
  expr_type2: identifier;
  expr_val3 : unbounded_string := identifiers( false_t ).value.all;
  expr_type3: identifier;
begin
  kind := string_t;
  expect( cgi_value_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseExpression( expr_val2, expr_type2 );
        if uniTypesOk( expr_type2, positive_t ) then
           if token = symbol_t and identifiers( token ).value.all = "," then
              getNextToken;
              ParseExpression( expr_val3, expr_type3 );
              if uniTypesOk( expr_type3, boolean_t ) then
                 null;
              end if;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
       result := cgi.Value( expr_val,
         positive'value( to_string( expr_val2 ) ),
         expr_val3 = identifiers( true_t ).value.all );
     exception when constraint_error =>
         err( +"key does not exist" );
     when others =>
         err_exception_raised;
     end;
  end if;
end ParseValue;


-----------------------------------------------------------------------------
--  PARSE KEY EXISTS
--
-- function ParseKey_Exists(Key : in Unbounded_String; Index : in Positive := 1
--         return Boolean is
-- Was a given key provided?
-----------------------------------------------------------------------------

procedure ParseKey_Exists( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2: identifier;
begin
  kind := boolean_t;
  expect( cgi_key_exists_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     expectParameterComma;
     ParseExpression( expr_val2, expr_type2 );
     if uniTypesOk( expr_type2, positive_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  if isExecutingCommand then
     result := to_bush_boolean(
          cgi.key_exists( expr_val, positive'value( to_string( expr_val2 ) ) )
       );
-- RESULT SHOULD BE NUMERIC BOOLEAN, NOT STRING.  UTIL FUNCTION FOR THIS?
  end if;
end ParseKey_Exists;


-----------------------------------------------------------------------------
--  PARSE KEY COUNT
--
--function ParseKey_Count(Key : in Unbounded_String) return Natural is
-- How many of a given key were provided?
-----------------------------------------------------------------------------

procedure ParseKey_Count( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := natural_t;
  expect( cgi_key_count_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     result := to_unbounded_string( cgi.key_count( expr_val )'img );
  end if;
end ParseKey_Count;


-----------------------------------------------------------------------------
--  CGI ARGUMENT COUNT
--
-- Access data as an ordered list (it was sent as Key=Value);
-- Keys and Values may be retrieved from Position (1 .. Argument_Count).
-- Constraint_Error will be raised if Position<1 or Position>Argument_Count
-- function ParseArgument_Count return Natural is
-- 0 means no data sent.
-----------------------------------------------------------------------------

procedure ParseCGIArgument_Count( result : out unbounded_string; kind : out identifier ) is
begin
   kind := natural_t;
  expect( cgi_argument_count_t );
  if isExecutingCommand then
     result := to_unbounded_string( natural'image( cgi.Argument_Count ));
  end if;
end ParseCGIArgument_Count;


-----------------------------------------------------------------------------
--  PARSE KEY
--
-- function ParseKey(Position : in Positive) return Unbounded_String is
-----------------------------------------------------------------------------

procedure ParseKey( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := string_t;
  expect( cgi_key_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, positive_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       result := cgi.key( positive( to_numeric( expr_val ) ) );
     exception when constraint_error =>
       err( +"no key at this position" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseKey;


-----------------------------------------------------------------------------
--  PRASE KEY VALUE
--
-- function ParseKeyValue(Position : in Positive) return Unbounded_String is
-----------------------------------------------------------------------------

procedure ParseKeyValue( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := string_t;
  expect( cgi_key_value_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, positive_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       result := cgi.value( positive( to_numeric( expr_val ) ) );
     exception when constraint_error =>
       err( +"no key at this position" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseKeyValue;


-----------------------------------------------------------------------------
-- The following are helpful subprograms to simplify use of CGI.
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE KEY VALUE EXISTS
--
--function ParseKey_Value_Exists(Key : in Unbounded_String;
--                          Value : in Unbounded_String) return Boolean is
-- Returns True if a given Key has exactly Value as one of its values.
-----------------------------------------------------------------------------

procedure ParseKey_Value_Exists( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
  expr2_val  : unbounded_string := to_unbounded_string( "1" );
  expr2_type : identifier;
begin
  kind := boolean_t;
  expect( cgi_key_value_exists_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     expectParameterComma;
     ParseExpression( expr2_val, expr2_type );
     if uniTypesOk( expr2_type, uni_string_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  if isExecutingCommand then
     result := to_bush_boolean( cgi.key_value_exists( expr_val, expr2_val ) );
  end if;
end ParseKey_Value_Exists;


-----------------------------------------------------------------------------
-- Useful output routines:
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE PUT CGI HEADER
--
-- procedure ParsePut_CGI_Header(Header : in String := "Content-type: text/html") is
-- Put CGI Header to Current_Output, followed by two carriage returns.
-- This header determines what the program's reply type is.
-- Default is to return a generated HTML document.
-- Warning: Make calls to Set_Cookie before calling this procedure!
-----------------------------------------------------------------------------

procedure ParsePut_CGI_Header is
  expr_val  : unbounded_string := to_unbounded_string( "Content-Type: text/html" );
  expr_type : identifier;
begin
  expect( cgi_put_cgi_header_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     ParseExpression( expr_val, expr_type );
     if uniTypesOk( expr_type, uni_string_t ) then
         null;
     end if;
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     cgi.put_cgi_header( to_string( expr_val ) );
  end if;
end ParsePut_CGI_Header;


-----------------------------------------------------------------------------
--  PARSE PUT HTML HEAD
--
--procedure ParsePut_HTML_Head(Title : in String; Mail_To : in String := "") is
-- Puts to Current_Output an HTML header with title "Title".  This is:
--   <html><head><title> _Title_ </title>
--   <link rev="made" href="mailto:  _Mail_To_ ">
--   </head><body>
-- If Mail_To is omitted, the "made" reverse link is omitted.
-----------------------------------------------------------------------------

procedure ParsePut_HTML_Head is
  expr_val  : unbounded_string;
  expr_type : identifier;
  expr2_val  : unbounded_string := null_unbounded_string;
  expr2_type : identifier;
begin
  expect( cgi_put_html_head_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     if token = symbol_t and identifiers( token ).value.all = "," then
        expectParameterComma;
        ParseExpression( expr2_val, expr2_type );
        if uniTypesOk( expr2_type, uni_string_t ) then
           null;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     cgi.put_HTML_head( to_string( expr_val ), to_string( expr2_val ) );
  end if;
end ParsePut_HTML_Head;


-----------------------------------------------------------------------------
--  PARSE PUT HTML HEADIG
--
-- procedure ParsePut_HTML_Heading(Title : in String; Level : in Positive) is
-- Put an HTML heading at the given level with the given text.
-- If level=1, this puts:  <h1>Title</h1>.
-----------------------------------------------------------------------------

procedure ParsePut_HTML_Heading is
  expr_val  : unbounded_string;
  expr_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
begin
  expect( cgi_put_html_heading_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     expectParameterComma;
     ParseExpression( level_val, level_type );
     if baseTypesOk( level_type, positive_t ) then
        null;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
        cgi.put_HTML_heading( to_string( expr_val ),
           positive( to_numeric( level_val ) ) );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParsePut_HTML_Heading;


-----------------------------------------------------------------------------
--  PARSE PUT HTML TAIL
--
-- This is called at the end of an HTML document. It puts to Current_Output:
--   </body></html>
-----------------------------------------------------------------------------

procedure ParsePut_HTML_Tail is
begin
  expect( cgi_put_html_tail_t );
  if isExecutingCommand then
     cgi.put_HTML_tail;
  end if;
end ParsePut_HTML_Tail;


-----------------------------------------------------------------------------
--  PARSE PUT ERROR MESSAGE
--
--procedure ParsePut_Error_Message(Message : in String) is
-- Put to Current_Output an error message.
-- This Puts an HTML_Head, an HTML_Heading, and an HTML_Tail.
-- Call "Put_CGI_Header" before calling this.
-----------------------------------------------------------------------------

procedure ParsePut_Error_Message is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  expect( cgi_put_error_message_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     null;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     cgi.put_error_message( to_string( expr_val ) );
  end if;
end ParsePut_Error_Message;


-----------------------------------------------------------------------------
--  PARSE PUT VARIABLES
--
-- Put to Current_Output all of the CGI variables as an HTML-formatted String.
-----------------------------------------------------------------------------

procedure ParsePut_Variables is
begin
  expect( cgi_put_variables_t );
  if isExecutingCommand then
     cgi.put_variables;
  end if;
end ParsePut_Variables;


-----------------------------------------------------------------------------
-- Miscellaneous Routines:
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE MY URL
--
--function ParseMy_URL return String is
-- Returns the URL of this script.
-----------------------------------------------------------------------------

procedure ParseMy_URL( result : out unbounded_string; kind : out identifier ) is
begin
  kind := string_t;
  expect( cgi_my_url_t );
  result := to_unbounded_string( cgi.my_url );
end ParseMy_URL;

--function ParseGet_Environment(Variable : in String) return String is
-- Return the given environment variable's value.
-- Returns "" if the variable does not exist.
-- Not implemented: already available in BUSH through pragma import

-- Multi-Line data support:


-----------------------------------------------------------------------------
--  PARSE LINE COUNT
--
--function ParseLine_Count (Value : in String) return Natural is
-- Given a value that may have multiple lines, count the lines.
-- Returns 0 if Value is the empty/null string (i.e., length=0)
-----------------------------------------------------------------------------

procedure ParseLine_Count( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := natural_t;
  expect( cgi_line_count_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     null;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := to_unbounded_string( cgi.line_count( to_string(expr_val))'img);
  end if;
end ParseLine_Count;


-----------------------------------------------------------------------------
--  PARSE LINE COUNT OF VALUE
--
--function ParseLine_Count_of_Value (Key : String) return Natural is
-- Given a Key which has a Value that may have multiple lines,
-- count the lines.  Returns 0 if Key's Value is the empty/null
-- string (i.e., length=0) or if there's no such Key.
-- This is the same as Line_Count(Value(Key)).
-----------------------------------------------------------------------------

procedure ParseLine_Count_Of_Value( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := natural_t;
  expect( cgi_line_count_of_value_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     null;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := to_unbounded_string( cgi.line_count_of_value(
       to_string(expr_val))'img);
  end if;
end ParseLine_Count_of_Value;


-----------------------------------------------------------------------------
--  PARSE CGI LINE
--
--function ParseLine (Value : in String; Position : in Positive) return String
-- Given a value that may have multiple lines, return the given line.
-- If there's no such line, raise Constraint_Error.
-----------------------------------------------------------------------------

procedure ParseCGILine (result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
  pos_val  : unbounded_string;
  pos_type : identifier;
begin
  kind := string_t;
  expect( cgi_line_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     expectParameterComma;
     ParseExpression( pos_val, pos_type );
     if baseTypesOk( pos_type, positive_t ) then
        null;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
        result := to_unbounded_string( cgi.line( to_string( expr_val ),
           positive( to_numeric( pos_val ) ) ) );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseCGILine;


-----------------------------------------------------------------------------
--  PARSE VALUE OF LINE
--
--function ParseValue_of_Line (Key : String; Position : Positive)
--                        return String is
-- Given a Key which has a Value that may have multiple lines,
-- return the given line.  If there's no such line, raise Constraint_Error.
-- If there's no such Key, return the null string.
-- This is the same as Line(Value(Key), Position).
-----------------------------------------------------------------------------

procedure ParseValue_of_Line( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
  pos_val  : unbounded_string;
  pos_type : identifier;
begin
  kind := string_t;
  expect( cgi_value_of_line_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     expectParameterComma;
     ParseExpression( pos_val, pos_type );
     if baseTypesOk( pos_type, positive_t ) then
        null;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
        result := to_unbounded_string( cgi.value_of_line( to_string( expr_val),
           positive( to_numeric( pos_val ) ) ) );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseValue_of_Line;


-----------------------------------------------------------------------------
-- Encoding and Decoding functions:
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE URL DECODE
--
--function ParseURL_Decode(Data : in Unbounded_String;
--                Translate_Plus : Boolean := True) return Unbounded_String is
-- In the given string, convert pattern %HH into alphanumeric characters,
-- where HH is a hex number. Since this encoding only permits values
-- from %00 to %FF, there's no need to handle 16-bit characters.
-- If "Translate_Plus" is True, translate '+' to ' '.
-- Returns the decoded value (instead of tranlating in-place)
-- NOTE: procedure version not implemented
-----------------------------------------------------------------------------

procedure ParseURL_Decode( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
  bool_val  : unbounded_string := identifiers( true_t ).value.all;
  bool_type : identifier;
begin
  kind := string_t;
  expect( cgi_url_decode_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseExpression( bool_val, bool_type );
        if baseTypesOk( bool_type, boolean_t ) then
           null;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
        result := cgi.URL_Decode( expr_val, bool_val = identifiers( true_t ).value.all );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseURL_Decode;


-----------------------------------------------------------------------------
--  PARSE URL ENCODE
--
--function ParseURL_Encode(Data : in Unbounded_String;
--                    Translate_Plus : Boolean := False)
--         return Unbounded_String is
-- Same as procedure, but returns a new Unbounded_String.
-----------------------------------------------------------------------------

procedure ParseURL_Encode( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
  bool_val  : unbounded_string := identifiers( false_t ).value.all;
  bool_type : identifier;
begin
  kind := string_t;
  expect( cgi_url_encode_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseExpression( bool_val, bool_type );
        if baseTypesOk( bool_type, boolean_t ) then
           null;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
        result := cgi.URL_Encode( expr_val, bool_val = identifiers( true_t
).value.all );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseURL_Encode;


-----------------------------------------------------------------------------
--  PARSE HTML ENCODE
--
--function ParseHTML_Encode(Data : in Unbounded_String) return Unbounded_String
-- is
-- Given string, perform HTML encoding, so the text can be included
-- in an HTML file.  This means '&' becomes '&amp;', '<' becomes '&lt;',
-- '>' becomes '&gt;', and '"' becomes '&quot;'.
-- All other characters are untouched.
-- ALL VARIABLE DATA sent from the application should be filtered through
-- HTML_Encode unless it's already in HTML format or you know that
-- it can't have these special characters.
-- Even if the data appears to have come from the user, it should be filtered;
-- the user may be unknowingly clicking though a malicious link.
-- Same as procedure, but returns a new value.
-----------------------------------------------------------------------------

procedure ParseHTML_Encode( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := string_t;
  expect( cgi_html_encode_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     null;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := cgi.HTML_encode( expr_val );
  end if;
end ParseHTML_Encode;


-----------------------------------------------------------------------------
-- Cookie handling subprograms
-- (Note that cookies are automatically read when the program starts):
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE SET COOKIE
--
--procedure ParseSet_Cookie(Key : String;
--                     Value : String;
--                     Expires : String := "";
--                     --Path: String := Get_Environment("PATH_INFO");
--                     --Domain: String := Get_Environment("SERVER_NAME");
--                     Secure: Boolean := False ) is
-- Sets a cookie value; call this BEFORE calling Put_CGI_Header.
-- If you don't want to send values for Expires, Path, or Domain,
-- just make them "".
-----------------------------------------------------------------------------

procedure ParseSet_Cookie is
  key_val      : unbounded_string;
  key_type     : identifier;
  cookie_val   : unbounded_string;
  cookie_type  : identifier;
  path_val     : unbounded_string := null_unbounded_string;
  path_type    : identifier;
  domain_val   : unbounded_string := null_unbounded_string;
  domain_type  : identifier;
  expires_val  : unbounded_string := null_unbounded_string;
  expires_type : identifier;
  secure_val   : unbounded_string := identifiers( false_t ).value.all;
  secure_type  : identifier;
begin
  -- lookup defaults
  findIdent( to_unbounded_string( "PATH_INFO" ), path_type );
  if path_type /= eof_t then
     path_val := identifiers( path_type ).value.all;
  end if;
  findIdent( to_unbounded_string( "SERVER_NAME" ), domain_type );
  if domain_type /= eof_t then
     domain_val := identifiers( domain_type ).value.all;
  end if;
  expect( cgi_set_cookie_t );
  expect( symbol_t, "(" );
  ParseExpression( key_val, key_type );
  if baseTypesOK( key_type, string_t ) then
     expectParameterComma;
  end if;
  ParseExpression( cookie_val, cookie_type );
  if baseTypesOK( cookie_type, string_t ) then
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseExpression( expires_val, expires_type );
        if baseTypesOK( expires_type, string_t ) then
  if token = symbol_t and identifiers( token ).value.all = "," then
     getNextToken;
     ParseExpression( path_val, path_type );
     if baseTypesOK( path_type, string_t ) then
        if token = symbol_t and identifiers( token ).value.all = "," then
           getNextToken;
           ParseExpression( domain_val, domain_type );
           if baseTypesOK( domain_type, string_t ) then

           if token = symbol_t and identifiers( token ).value.all = "," then
              getNextToken;
              ParseExpression( secure_val, secure_type );
              if baseTypesOK( secure_type, boolean_t ) then
                 null;
              end if;
           end if;
        end if;
     end if;
  end if;
  end if;
  end if;
  end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     cgi.set_cookie( to_string( key_val ),
       to_string( cookie_val ),
       to_string( expires_val ),
       to_string( path_val ),
       to_string( domain_val ),
       (secure_val = identifiers( true_t ).value.all) );
  end if;
end ParseSet_Cookie;


-----------------------------------------------------------------------------
--  PARSE COOKIE VALUE
--
--function ParseCookie_Value(Key : in Unbounded_String;
-- Index : in Positive := 1;
-- Required : in Boolean := False)
-- return Unbounded_String is
-----------------------------------------------------------------------------

procedure ParseCookie_Value( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
  pos_val  : unbounded_string := to_unbounded_string( " 1" );
  pos_type : identifier;
  bool_val  : unbounded_string := identifiers( false_t ).value.all;
  bool_type : identifier;
begin
  kind := string_t;
  expect( cgi_cookie_value_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_string_t ) then
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseExpression( pos_val, pos_type );
        if baseTypesOk( pos_type, positive_t ) then
           if token = symbol_t and identifiers( token ).value.all = "," then
              getNextToken;
              ParseExpression( bool_val, bool_type );
              if baseTypesOk( bool_type, boolean_t ) then
                 null;
              end if;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
        result := cgi.cookie_value( expr_val,
           positive( to_numeric( pos_val ) ),
           bool_val = identifiers( true_t ).value.all );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseCookie_Value;


-----------------------------------------------------------------------------
--  PARSE COOKIE COUNT
--
-- Returns the number of cookies (0 if none)
-----------------------------------------------------------------------------

procedure ParseCookie_Count( result : out unbounded_string; kind : out identifier ) is
begin
  kind := natural_t;
  expect( cgi_cookie_count_t );
  if isExecutingCommand then
     result := to_unbounded_string( cgi.cookie_count'img );
  end if;
end ParseCookie_Count;

-----------------------------------------------------------------------------
-- Housekeeping
-----------------------------------------------------------------------------

procedure StartupCGI is
begin
  declareNamespace( "cgi" );
  declareIdent( cgi_cgi_method_type_t, "cgi.cgi_method_type", root_enumerated_t,
    typeClass );
  declareStandardConstant( cgi_get_t, "cgi.get", cgi_cgi_method_type_t, "0" );
  declareStandardConstant( cgi_post_t, "cgi.post", cgi_cgi_method_type_t, "1" );
  declareStandardConstant( cgi_unknown_t, "cgi.unknown", cgi_cgi_method_type_t,
    "2" );

  declareFunction( cgi_parsing_errors_t, "cgi.parsing_errors", ParseParsing_Errors'access );
  declareFunction( cgi_input_received_t, "cgi.input_received", ParseInput_Received'access );
  declareFunction( cgi_is_index_t, "cgi.is_index", ParseIs_Index'access );
  declareFunction( cgi_cgi_method_t, "cgi.cgi_method", ParseCGI_Method'access );
  declareFunction( cgi_value_t, "cgi.value", ParseValue'access );
  declareFunction( cgi_key_exists_t, "cgi.key_exists", ParseKey_Exists'access );
  declareFunction( cgi_key_count_t, "cgi.key_count", ParseKey_Count'access );
  declareFunction( cgi_argument_count_t, "cgi.argument_count", ParseCGIArgument_Count'access );
  declareFunction( cgi_key_t, "cgi.key", ParseKey'access );
  declareFunction( cgi_key_value_t, "cgi.key_value", ParseKeyValue'access );
  declareFunction( cgi_key_value_exists_t, "cgi.key_value_exists", ParseKey_Value_Exists'access );
  declareProcedure( cgi_put_cgi_header_t, "cgi.put_cgi_header", ParsePut_CGI_Header'access );
  declareProcedure( cgi_put_html_head_t, "cgi.put_html_head", ParsePut_HTML_Head'access );
  declareProcedure( cgi_put_html_heading_t, "cgi.put_html_heading", ParsePut_HTML_Heading'access );
  declareProcedure( cgi_put_html_tail_t, "cgi.put_html_tail", ParsePut_HTML_Tail'access );
  declareProcedure( cgi_put_error_message_t, "cgi.put_error_message", ParsePut_Error_Message'access );
  declareProcedure( cgi_put_variables_t, "cgi.put_variables", ParsePut_Variables'access );
  declareFunction( cgi_my_url_t, "cgi.my_url", ParseMy_URL'access );
  --declareFunction( cgi_get_environment_t, "cgi.get_environment" );
  declareFunction( cgi_line_count_t, "cgi.line_count", ParseLine_Count'access );
  declareFunction( cgi_line_count_of_value_t, "cgi.line_count_of_value", ParseLine_Count_Of_Value'access );
  declareFunction( cgi_line_t, "cgi.line", ParseCGILine'access );
  declareFunction( cgi_value_of_line_t, "cgi.value_of_line", ParseValue_Of_Line'access );
  declareFunction( cgi_url_decode_t, "cgi.url_decode", ParseURL_Decode'access );
  declareFunction( cgi_url_encode_t, "cgi.url_encode", ParseURL_Encode'access );
  declareFunction( cgi_html_encode_t, "cgi.html_encode", ParseHTML_Encode'access );
  declareProcedure( cgi_set_cookie_t, "cgi.set_cookie", ParseSet_Cookie'access );
  declareFunction( cgi_cookie_value_t, "cgi.cookie_value", ParseCookie_Value'access );
  declareFunction( cgi_cookie_count_t, "cgi.cookie_count", ParseCookie_Count'access );
  declareNamespaceClosed( "cgi" );
end StartupCGI;

procedure ShutdownCGI is
begin
  null;
end ShutdownCGI;

end parser_cgi;
