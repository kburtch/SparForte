------------------------------------------------------------------------------
-- CGI Package Parser                                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2026 Free Software Foundation              --
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
    symbol_table,
    message_strings,
    value_conversion,
    scanner,
    scanner.communications,
    parser_params,
    parser,
    parser_tio;
use ada.strings.unbounded,
    world,
    symbol_table,
    message_strings,
    value_conversion,
    pegasoft,
    scanner,
    scanner.communications,
    parser_params,
    parser,
    parser_tio;

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
--  PARSE PARSING ERRORS                                  (built-in function)
--
-- AdaScript Syntax: b := cgi.parsing_errors
--       Ada Target: CGI.Parsing_Errors
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.parsing_errors
-----------------------------------------------------------------------------

procedure ParseParsing_Errors( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := cgi_parsing_errors_t;
begin
  kind := boolean_t;
  expect( subprogramId );

  if isExecutingCommand then
     result := storage'( to_spar_boolean( cgi.parsing_errors ), noMetaLabel, sparMetaLabels );
  end if;
end ParseParsing_Errors;


-----------------------------------------------------------------------------
--  PARSE INPUT RECEIVED                                  (built-in function)
--
-- AdaScript Syntax: b := cgi.input_received
--       Ada Target: CGI.Input_Received
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.input_received
-----------------------------------------------------------------------------

procedure ParseInput_Received( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := cgi_input_received_t;
begin
  kind := boolean_t;
  expect( subprogramId );

  if isExecutingCommand then
     result := storage'( to_spar_boolean( cgi.input_received ), noMetaLabel, sparMetaLabels );
  end if;
end ParseInput_Received;


-----------------------------------------------------------------------------
--  PARSE IS INDEX                                        (built-in function)
--
-- AdaScript Syntax: b := cgi.is_index
--       Ada Target: CGI.Is_Index
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.is_index
-------------------------------------------------------------------------------

procedure ParseIs_Index( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := cgi_is_index_t;
begin
  kind := boolean_t;
  expect( subprogramId );

  if isExecutingCommand then
     result := storage'( to_spar_boolean( cgi.is_index ), noMetaLabel, sparMetaLabels );
  end if;
end ParseIs_Index;


-----------------------------------------------------------------------------
--  PARSE CGI METHOD                                      (built-in function)
--
-- AdaScript Syntax: m := cgi.cgi_method
--       Ada Target: CGI.CGI_Method
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.cgi_method
-----------------------------------------------------------------------------

procedure ParseCGI_Method( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := cgi_cgi_method_t;
begin
  kind := cgi_cgi_method_type_t;
  expect( subprogramId );

  if isExecutingCommand then
     result := storage'( To_Unbounded_String( integer'image( cgi.cgi_method_type'pos( cgi.CGI_Method ) )(2)&"" ),
        noMetaLabel, sparMetaLabels );
  end if;
end ParseCGI_Method;


-----------------------------------------------------------------------------
--  PARSE VALUE                                           (built-in function)
--
-- AdaScript Syntax: s := cgi.value( v, i, b )
--       Ada Target: CGI.Value
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.value
-----------------------------------------------------------------------------

procedure ParseValue( result : out storage; kind : out identifier ) is
  keyExpr  : storage;
  keyType  : identifier;
  idxExpr  : storage := storage'( to_unbounded_string( " 1" ), noMetaLabel, sparMetaLabels ); -- default
  idxType  : identifier;
  reqExpr  : storage := storage'( identifiers( false_t ).store.value, noMetaLabel, sparMetalabels );
  reqType  : identifier;
  subprogramId : constant identifier := cgi_value_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( keyExpr, keyType );
  if uniTypesOk( keyType, uni_string_t ) then
     if token = symbol_t and identifiers( token ).store.value = "," then
        getNextToken;
        ParseExpression( idxExpr, idxType );
        if uniTypesOk( idxType, positive_t ) then
           if token = symbol_t and identifiers( token ).store.value = "," then
              getNextToken;
              ParseExpression( reqExpr, reqType );
              if uniTypesOk( reqType, boolean_t ) then
                 null;
              end if;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     if metaLabelOk( subprogramId, keyExpr ) and metaLabelOk( subprogramId, idxExpr ) then
        begin
          result := storage'( cgi.Value( keyExpr.value,
            positive'value( to_string( idxExpr.value ) ),
            reqExpr.value = identifiers( true_t ).store.value ), noMetaLabel, keyExpr.policyMetaLabels);
        exception when constraint_error =>
            err( +"key does not exist" );
        when others =>
            err_exception_raised;
        end;
     end if;
  end if;
end ParseValue;


-----------------------------------------------------------------------------
--  PARSE KEY EXISTS                                      (built-in function)
--
-- AdaScript Syntax: b := cgi.key_exists( k, i )
--       Ada Target: CGI.Key_Exists
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.key_exists
-----------------------------------------------------------------------------

procedure ParseKey_Exists( result : out storage; kind : out identifier ) is
  keyExpr  : storage;
  keyType  : identifier;
  idxExpr  : storage;
  idxType : identifier;
  subprogramId : constant identifier := cgi_key_exists_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseFirstStringParameter( subprogramId, keyExpr, KeyType );
  ParseLastNumericParameter( subprogramId, idxExpr, idxtype, positive_t );

  if isExecutingCommand then
     if metaLabelOk( subprogramId, keyExpr ) and metaLabelOK( subprogramId, idxExpr ) then
        result := storage'( to_spar_boolean(
             cgi.key_exists( keyExpr.value, positive'value( to_string( idxExpr.value ) ) )
           ), noMetaLabel, sparMetaLabels );
-- RESULT SHOULD BE NUMERIC BOOLEAN, NOT STRING.  UTIL FUNCTION FOR THIS?
     end if;
  end if;
end ParseKey_Exists;


-----------------------------------------------------------------------------
--  PARSE KEY COUNT                                       (built-in function)
--
-- AdaScript Syntax: n := cgi.key_count( k )
--       Ada Target: CGI.Key_Count
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.key_count
-----------------------------------------------------------------------------

procedure ParseKey_Count( result : out storage; kind : out identifier ) is
  keyExpr  : storage;
  keyType  : identifier;
  subprogramId : constant identifier := cgi_key_count_t;
begin
  kind := natural_t;
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, keyExpr, keyType );

  if isExecutingCommand then
     if metaLabelOk( subprogramId, keyExpr ) then
        result := storage'( to_unbounded_string( cgi.key_count( keyExpr.value )'img ),
           noMetaLabel, sparMetaLabels );
     end if;
  end if;
end ParseKey_Count;


-----------------------------------------------------------------------------
--  CGI ARGUMENT COUNT                                    (built-in function)
--
-- AdaScript Syntax: n := cgi.argument_count
--       Ada Target: CGI.Argument_Count
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.argument_count
-----------------------------------------------------------------------------

procedure ParseCGIArgument_Count( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := cgi_argument_count_t;
begin
  kind := natural_t;
  expect( subprogramId );

  if isExecutingCommand then
     result := storage'( to_unbounded_string( natural'image( cgi.Argument_Count )), noMetaLabel, sparMetaLabels );
  end if;
end ParseCGIArgument_Count;


-----------------------------------------------------------------------------
--  PARSE KEY                                             (built-in function)
--
-- AdaScript Syntax: s := cgi.key( p )
--       Ada Target: CGI.Key
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.key
-----------------------------------------------------------------------------

procedure ParseKey( result : out storage; kind : out identifier ) is
  posExpr  : storage;
  posType  : identifier;
  subprogramId : constant identifier := cgi_key_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( posExpr, posType );
  if baseTypesOk( posType, positive_t ) then
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     begin
       result := storage'( cgi.key( positive( to_numeric( posExpr.value ) ) ), noMetaLabel, sparMetaLabels );
     exception when constraint_error =>
       err( +"no key at this position" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseKey;


-----------------------------------------------------------------------------
--  PARSE KEY VALUE                                       (built-in function)
--
-- AdaScript Syntax: s := cgi.key_value( p )
--       Ada Target: CGI.Value
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.key_value
-----------------------------------------------------------------------------

procedure ParseKeyValue( result : out storage; kind : out identifier ) is
  posExpr  : storage;
  posType : identifier;
  subprogramId : constant identifier := cgi_key_value_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( posExpr, posType );
  if baseTypesOk( posType, positive_t ) then
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     if metaLabelOk( subprogramId, posExpr ) then
        begin
          result := storage'( cgi.value( positive( to_numeric( posExpr.value ) ) ), noMetaLabel, sparMetaLabels );
        exception when constraint_error =>
          err( +"no key at this position" );
        when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseKeyValue;


-----------------------------------------------------------------------------
-- The following are helpful subprograms to simplify use of CGI.
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE KEY VALUE EXISTS                                (built-in function)
--
-- AdaScript Syntax: b := cgi.key_value_exists( v, s )
--       Ada Target: CGI.Key_Value_Exists
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.key_value_exists
-----------------------------------------------------------------------------

procedure ParseKey_Value_Exists( result : out storage; kind : out identifier ) is
  keyExpr       : storage;
  keyType       : identifier;
  keyValueExpr  : storage := storage'( to_unbounded_string( "1" ), noMetaLabel, sparMetaLabels );
  keyValueType  : identifier;
  subprogramId  : constant identifier := cgi_key_value_exists_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( keyExpr, keyType );
  if uniTypesOk( keyType, uni_string_t ) then
     expectParameterComma;
     ParseExpression( keyValueExpr, keyValueType );
     if uniTypesOk( keyValueType, uni_string_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;

  if isExecutingCommand then
     if metaLabelOk( subprogramId, keyExpr ) and metaLabelOk( subprogramId, keyValueExpr ) then
        result := storage'( to_spar_boolean( cgi.key_value_exists( keyExpr.value, keyValueExpr.value ) ),
          noMetaLabel, sparMetaLabels );
     end if;
  end if;
end ParseKey_Value_Exists;


-----------------------------------------------------------------------------
-- Useful output routines:
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE PUT CGI HEADER                                 (built-in procedure)
--
-- AdaScript Syntax: cgi.put_cgi_header[( h )]
--       Ada Target: CGI.Put_CGI_Header
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.put_cgi_header
-----------------------------------------------------------------------------

procedure ParsePut_CGI_Header is
  headerExpr : storage := storage'( to_unbounded_string( "Content-Type: text/html" ), noMetaLabel, sparMetaLabels );
  headerType : identifier;
  subprogramId : constant identifier := cgi_put_cgi_header_t;
begin
  expect( subprogramId );
  -- headerExpr is optional
  if token = symbol_t and identifiers( token ).store.value = "(" then
     expect( symbol_t, "(" );
     ParseExpression( headerExpr, headerType );
     if uniTypesOk( headerType, uni_string_t ) then
         null;
     end if;
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     if metaLabelOk( subprogramId, identifiers( standard_output_t ).sstorage ) then
        begin
          cgi.put_cgi_header( to_string( headerExpr.value ) );
        exception when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParsePut_CGI_Header;


-----------------------------------------------------------------------------
--  PARSE PUT HTML HEAD                                  (built-in procedure)
--
-- AdaScript Syntax: cgi.put_html_head( t, m )
--       Ada Target: CGI.Put_HTML_Head
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.put_html_head
-----------------------------------------------------------------------------

procedure ParsePut_HTML_Head is
  webPageTitleExpr : storage;
  webPageTitleType : identifier;
  mailToExpr       : storage := nullStorage;
  mailToType       : identifier;
  subprogramId : constant identifier := cgi_put_html_head_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  -- webPageTitleExpr.value can be an empty string although an empty string
  -- is not useful
  parseExpression( webPageTitleExpr, webPageTitleType );
  if uniTypesOk( webPageTitleType, uni_string_t ) then
     if token = symbol_t and identifiers( token ).store.value = "," then
        -- authorEmailExpr.value can be an empty string although an empty
        -- string is not useful
        expectParameterComma;
        parseExpression( mailToExpr, mailToType );
        if uniTypesOk( mailToType, uni_string_t ) then
           null;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     if metaLabelOk( subprogramId, webPageTitleExpr ) and metaLabelOk( subprogramId, mailToExpr ) and
        metaLabelOk( subprogramId, identifiers( standard_output_t ).sstorage ) then
           begin
             cgi.put_HTML_head( to_string( webPageTitleExpr.value ), to_string( mailToExpr.value ) );
           exception when others =>
             err_exception_raised;
           end;
     end if;
  end if;
end ParsePut_HTML_Head;


-----------------------------------------------------------------------------
--  PARSE PUT HTML HEADING                               (built-in procedure)
--
-- AdaScript Syntax: cgi.put_html_heading( s, l )
--       Ada Target: CGI.Put_HTML_Heading
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.put_html_heading
-----------------------------------------------------------------------------

procedure ParsePut_HTML_Heading is
  titleExpr  : storage;
  titleType : identifier;
  levelExpr  : storage;
  levelType : identifier;
  subprogramId : constant identifier := cgi_put_html_heading_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( titleExpr, titleType );
  if uniTypesOk( titleType, uni_string_t ) then
     expectParameterComma;
     ParseExpression( levelExpr, levelType );
     if baseTypesOk( levelType, positive_t ) then
        null;
     end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     if metaLabelOk( subprogramId, titleExpr ) and metaLabelOk( subprogramId, levelExpr ) and
        metaLabelOk( subprogramId, identifiers( standard_output_t ).sstorage ) then
           begin
              cgi.put_HTML_heading( to_string( titleExpr.value ),
                 positive( to_numeric( levelExpr.value ) ) );
           exception when others =>
              err_exception_raised;
           end;
     end if;
  end if;
end ParsePut_HTML_Heading;


-----------------------------------------------------------------------------
--  PARSE PUT HTML TAIL                                  (built-in procedure)
--
-- AdaScript Syntax: cgi.put_html_tail
--       Ada Target: CGI.Put_HTML_Tail
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.put_html_tail
-----------------------------------------------------------------------------

procedure ParsePut_HTML_Tail is
  subprogramId : constant identifier := cgi_put_html_tail_t;
begin
  expect( subprogramId );

  if isExecutingCommand then
     if metaLabelOk( subprogramId, identifiers( standard_output_t ).sstorage ) then
        begin
           cgi.put_HTML_tail;
        exception when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParsePut_HTML_Tail;


-----------------------------------------------------------------------------
--  PARSE PUT ERROR MESSAGE                              (built-in procedure)
--
-- AdaScript Syntax: cgi.put_html_tail
--       Ada Target: CGI.Put_Error_Message
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.put_html_tail
-----------------------------------------------------------------------------

procedure ParsePut_Error_Message is
  messageExpr : storage;
  messageType : identifier;
  subprogramId : constant identifier := cgi_put_error_message_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( messageExpr, messageType );
  if uniTypesOk( messageType, uni_string_t ) then
     null;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     if metaLabelOk( subprogramId, messageExpr ) and metaLabelOk( subprogramId, identifiers( standard_output_t ).sstorage ) then
        begin
           cgi.put_error_message( to_string( messageExpr.value ) );
        exception when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParsePut_Error_Message;


-----------------------------------------------------------------------------
--  PARSE PUT VARIABLES                                  (built-in procedure)
--
-- AdaScript Syntax: cgi.put_variables
--       Ada Target: CGI.Put_Variables
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.put_variables
-----------------------------------------------------------------------------

procedure ParsePut_Variables is
  subprogramId : constant identifier := cgi_put_variables_t;
begin
  expect( subprogramId );

  if isExecutingCommand then
     if metaLabelOk( subprogramId, identifiers( standard_output_t ).sstorage ) then
        begin
           cgi.put_variables;
        exception when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParsePut_Variables;


-----------------------------------------------------------------------------
-- Miscellaneous Routines:
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE MY URL                                          (built-in function)
--
-- AdaScript Syntax: s := cgi.my_url
--       Ada Target: CGI.My_URL
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.my_url
-----------------------------------------------------------------------------

procedure ParseMy_URL( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := cgi_my_url_t;
begin
  kind := string_t;
  expect( subprogramId );

  if isExecutingCommand then
     begin
        result := storage'( to_unbounded_string( cgi.my_url ), noMetaLabel, sparMetaLabels );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseMy_URL;

--function ParseGet_Environment(Variable : in String) return String is
-- Not implemented: already available in SparForte through pragma import

-- Multi-Line data support:


-----------------------------------------------------------------------------
--  PARSE LINE COUNT                                      (built-in function)
--
-- AdaScript Syntax: n := cgi.line_count( s )
--       Ada Target: CGI.Line_Count
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.line_count
-----------------------------------------------------------------------------

procedure ParseLine_Count( result : out storage; kind : out identifier ) is
  keyExpr : storage;
  keyType : identifier;
  subprogramId : constant identifier := cgi_line_count_t;
begin
  kind := natural_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( keyExpr, keyType );
  if uniTypesOk( keyType, uni_string_t ) then
     null;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     if metaLabelOk( subprogramId, keyExpr ) then
        begin
           result := storage'( to_unbounded_string( cgi.line_count( to_string(keyExpr.value))'img),
              noMetaLabel, keyExpr.policyMetaLabels );
        exception when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParseLine_Count;


-----------------------------------------------------------------------------
--  PARSE LINE COUNT OF VALUE                             (built-in function)
--
-- AdaScript Syntax: n := cgi.line_count_of_value( v )
--       Ada Target: CGI.Line_Count_of_Value
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.line_count_of_value
-----------------------------------------------------------------------------

procedure ParseLine_Count_Of_Value( result : out storage; kind : out identifier ) is
  keyExpr : storage;
  keyType : identifier;
  subprogramId : constant identifier := cgi_line_count_of_value_t;
begin
  kind := natural_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( keyExpr, keyType );
  if uniTypesOk( keyType, uni_string_t ) then
     null;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     if metaLabelOk( subprogramId, keyExpr ) then
        begin
           result := storage'( to_unbounded_string( cgi.line_count_of_value(
             to_string(keyExpr.value))'img), noMetaLabel, keyExpr.policyMetaLabels );
        exception when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParseLine_Count_of_Value;


-----------------------------------------------------------------------------
--  PARSE CGI LINE                                        (built-in function)
--
-- AdaScript Syntax: l := cgi.line( s, p )
--       Ada Target: CGI.Line
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.line
-----------------------------------------------------------------------------

procedure ParseCGILine (result : out storage; kind : out identifier ) is
  keyExpr : storage;
  keyType : identifier;
  posExpr : storage;
  posType : identifier;
  subprogramId : constant identifier := cgi_line_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( keyExpr, keyType );
  if uniTypesOk( keyType, uni_string_t ) then
     expectParameterComma;
     ParseExpression( posExpr, posType );
     if baseTypesOk( posType, positive_t ) then
        null;
     end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     if metaLabelOk( subprogramId, keyExpr ) and metaLabelOk( subprogramId, posExpr ) then
        begin
           result := storage'( to_unbounded_string( cgi.line( to_string( keyExpr.value ),
              positive( to_numeric( posExpr.value ) ) ) ),
              noMetaLabel,
              resolveEffectiveMetaLabels( kind, keyExpr, posExpr ) );
        exception when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParseCGILine;


-----------------------------------------------------------------------------
--  PARSE VALUE OF LINE                                   (built-in function)
--
-- AdaScript Syntax: s := cgi.value( k, i, b )
--       Ada Target: CGI.Value_of_Line
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.value_of_line
-----------------------------------------------------------------------------

procedure ParseValue_of_Line( result : out storage; kind : out identifier ) is
  keyExpr  : storage;
  keyType  : identifier;
  posExpr  : storage;
  posType  : identifier;
  subprogramId : constant identifier := cgi_value_of_line_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( keyExpr, keyType );
  if uniTypesOk( keyType, uni_string_t ) then
     expectParameterComma;
     ParseExpression( posExpr, posType );
     if baseTypesOk( posType, positive_t ) then
        null;
     end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     if metaLabelOK( subprogramId, keyExpr ) and metaLabelOK( subprogramId, posExpr ) then
        begin
           result := storage'( to_unbounded_string( cgi.value_of_line( to_string( keyExpr.value ),
              positive( to_numeric( posExpr.value ) ) ) ),
              noMetaLabel,
              resolveEffectiveMetaLabels( kind, keyExpr, posExpr ) );
        exception when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParseValue_of_Line;


-----------------------------------------------------------------------------
-- Encoding and Decoding functions:
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE URL DECODE                                      (built-in function)
--
-- AdaScript Syntax: s := cgi.url_decode( u, b )
--       Ada Target: CGI.URL_Decode
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.url_decode
-- NOTE: procedure version of CGI.URL_Decode not implemented
-----------------------------------------------------------------------------

procedure ParseURL_Decode( result : out storage; kind : out identifier ) is
  dataExpr : storage;
  dataType : identifier;
  boolExpr : storage := storage'( identifiers( true_t ).store.value, noMetaLabel, sparMetaLabels );
  boolType : identifier;
  subprogramId : constant identifier := cgi_url_decode_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( dataExpr, dataType );
  if uniTypesOk( dataType, uni_string_t ) then
     if token = symbol_t and identifiers( token ).store.value = "," then
        getNextToken;
        ParseExpression( boolExpr, boolType );
        if baseTypesOk( boolType, boolean_t ) then
           null;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     if metaLabelOK( subprogramId, dataExpr ) and metaLabelOK( subprogramId, boolExpr ) then
        begin
           result := storage'( cgi.URL_Decode( dataExpr.value, boolExpr.value = identifiers( true_t ).store.value ),
              noMetaLabel,
              resolveEffectiveMetaLabels( kind, dataExpr, boolExpr ) );
        exception when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParseURL_Decode;


-----------------------------------------------------------------------------
--  PARSE URL ENCODE                                      (built-in function)
--
-- AdaScript Syntax: u := cgi.url_encode( s, b )
--       Ada Target: CGI.URL_Encode
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.url_encode
-- Same as procedure, but returns a new Unbounded_String.
-----------------------------------------------------------------------------

procedure ParseURL_Encode( result : out storage; kind : out identifier ) is
  dataExpr : storage;
  dataType : identifier;
  boolExpr : storage := storage'( identifiers( false_t ).store.value, noMetaLabel, sparMetaLabels );
  boolType : identifier;
  subprogramId : constant identifier := cgi_url_encode_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( dataExpr, dataType );
  if uniTypesOk( dataType, uni_string_t ) then
     if token = symbol_t and identifiers( token ).store.value = "," then
        getNextToken;
        ParseExpression( boolExpr, boolType );
        if baseTypesOk( boolType, boolean_t ) then
           null;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     begin
        result := storage'( cgi.URL_Encode( dataExpr.value, boolExpr.value = identifiers( true_t
).store.value ),
           noMetaLabel,
           resolveEffectiveMetaLabels( kind, dataExpr, boolExpr ) );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseURL_Encode;


-----------------------------------------------------------------------------
--  PARSE HTML ENCODE                                     (built-in function)
--
-- AdaScript Syntax: h := cgi.html_encode( s )
--       Ada Target: CGI.HTML_Encode
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.html_encode
-----------------------------------------------------------------------------

procedure ParseHTML_Encode( result : out storage; kind : out identifier ) is
  dataExpr : storage;
  dataType : identifier;
  subprogramId : constant identifier := cgi_html_encode_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( dataExpr, dataType );
  if uniTypesOk( dataType, uni_string_t ) then
     null;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     if metaLabelOK( subprogramId, dataExpr ) then
        begin
          result := storage'( cgi.HTML_encode( dataExpr.value ),
             noMetaLabel, dataExpr.policyMetaLabels );
        exception when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseHTML_Encode;


-----------------------------------------------------------------------------
-- Cookie handling subprograms
-- (Note that cookies are automatically read when the program starts):
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE SET COOKIE                                     (built-in procedure)
--
-- AdaScript Syntax: cgi.set_cookie( k, s, x, p, d, b )
--       Ada Target: CGI.Set_Cookie
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.set_cookie
-----------------------------------------------------------------------------

procedure ParseSet_Cookie is
  keyExpr      : storage;
  keyType      : identifier;
  cookieExpr   : storage;
  cookieType   : identifier;
  pathExpr     : storage := nullStorage;
  pathType     : identifier;
  domainExpr   : storage := nullStorage;
  domainType   : identifier;
  expiresExpr  : storage := nullStorage;
  expiresType  : identifier;
  secureExpr   : storage := storage'( identifiers( false_t ).store.value, noMetaLabel, sparMetaLabels );
  secureType   : identifier;
  subprogramId : constant identifier := cgi_set_cookie_t;
begin
  -- lookup defaults
  findIdent( to_unbounded_string( "PATH_INFO" ), pathType );
  if pathType /= eof_t then
     pathExpr.value := identifiers( pathType ).store.value;
  end if;
  findIdent( to_unbounded_string( "SERVER_NAME" ), domainType );
  if domainType /= eof_t then
     domainExpr.value:= identifiers( domainType ).store.value;
  end if;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( keyExpr, keyType );
  if baseTypesOK( keyType, string_t ) then
     expectParameterComma;
  end if;
  ParseExpression( cookieExpr, cookieType );
  if baseTypesOK( cookieType, string_t ) then
     if token = symbol_t and identifiers( token ).store.value = "," then
        getNextToken;
        ParseExpression( expiresExpr, expiresType );
        if baseTypesOK( expiresType, string_t ) then
  if token = symbol_t and identifiers( token ).store.value = "," then
     getNextToken;
     ParseExpression( pathExpr, pathType );
     if baseTypesOK( pathType, string_t ) then
        if token = symbol_t and identifiers( token ).store.value = "," then
           getNextToken;
           ParseExpression( domainExpr, domainType );
           if baseTypesOK( domainType, string_t ) then

           if token = symbol_t and identifiers( token ).store.value = "," then
              getNextToken;
              ParseExpression( secureExpr, secureType );
              if baseTypesOK( secureType, boolean_t ) then
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
     cgi.set_cookie( to_string( keyExpr.value ),
       to_string( cookieExpr.value ),
       to_string( expiresExpr.value ),
       to_string( pathExpr.value ),
       to_string( domainExpr.value ),
       (secureExpr.value = identifiers( true_t ).store.value) );
  end if;
end ParseSet_Cookie;


-----------------------------------------------------------------------------
--  PARSE COOKIE VALUE                                    (built-in function)
--
-- AdaScript Syntax: s := cgi.cookie_value( c [, p] )
--       Ada Target: CGI.Cookie_Value
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.cookie_value
-----------------------------------------------------------------------------

procedure ParseCookie_Value( result : out storage; kind : out identifier ) is
  keyExpr  : storage;
  keyType  : identifier;
  posExpr  : storage := storage'( to_unbounded_string( " 1" ), noMetaLabel, sparMetaLabels );
  posType  : identifier;
  boolExpr : storage := storage'( identifiers( false_t ).store.value, noMetaLabel, sparMetaLabels );
  boolType : identifier;
  subprogramId : constant identifier := cgi_cookie_value_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( keyExpr, keyType );
  if uniTypesOk( keyType, uni_string_t ) then
     if token = symbol_t and identifiers( token ).store.value = "," then
        getNextToken;
        ParseExpression( posExpr, posType );
        if baseTypesOk( posType, positive_t ) then
           if token = symbol_t and identifiers( token ).store.value = "," then
              getNextToken;
              ParseExpression( boolExpr, boolType );
              if baseTypesOk( boolType, boolean_t ) then
                 null;
              end if;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     if metaLabelOK( subprogramId, keyExpr ) and metaLabelOk( subprogramId, posExpr ) then
        begin
           result := storage'( cgi.cookie_value( keyExpr.value,
              positive( to_numeric( posExpr.value ) ),
              boolExpr.value = identifiers( true_t ).store.value ),
                 noMetaLabel,
                 resolveEffectiveMetaLabels( kind, keyExpr, posExpr ) );
        exception when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParseCookie_Value;


-----------------------------------------------------------------------------
--  PARSE COOKIE COUNT                                    (built-in function)
--
-- AdaScript Syntax: n := cgi.cookie_count
--       Ada Target: CGI.Cookie_Count
--   GNAT Spec File: adacgi-1.6/cgi.ads
--   SparForte Docs: doc/pkg_cgi.html#cgi.cookie_count
-----------------------------------------------------------------------------

procedure ParseCookie_Count( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := cgi_cookie_count_t;
begin
  kind := natural_t;
  expect( subprogramId );
  if isExecutingCommand then
     result := storage'( to_unbounded_string( cgi.cookie_count'img ), noMetaLabel, sparMetaLabels );
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
