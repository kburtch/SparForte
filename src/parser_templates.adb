------------------------------------------------------------------------------
-- Templates Package Parser                                                 --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

--with text_io;use text_io;

with
    ada.strings.unbounded,
    world,
    scanner.communications,
    parser_params;
use
    ada.strings.unbounded,
    world,
    scanner,
    scanner.communications,
    parser_params;

package body parser_templates is

------------------------------------------------------------------------------
-- Template package identifiers
------------------------------------------------------------------------------

templates_set_http_status_t      : identifier;
templates_set_http_location_t    : identifier;
templates_put_template_header_t  : identifier;
templates_has_put_template_header_t : identifier;

------------------------------------------------------------------------------
-- Template package subprograms
------------------------------------------------------------------------------

procedure ParseTemplatesSetHTTPStatus is
  -- Set the HTTP result code (e.g. 200 OK)
  -- Syntax: templates.set_http_status( n );
  -- Ada:    N/A
  exprVal : unbounded_string;
  exprKind : identifier;
begin
  expect( templates_set_http_status_t );
  ParseSingleNumericParameter( templates_set_http_status_t, exprVal, exprKind );
  baseTypesOK( exprKind, natural_t );
  if isExecutingCommand then
     if templateHeader.templateHeaderSent then
        err( "HTTP header already sent" );
     else
        begin
           templateHeader.status := httpStatusCodes( to_numeric( exprVal ) );
        exception when constraint_error =>
           err( "status code is out-of-range" &
                httpStatusCodes'first'img & " .." &
                httpStatusCodes'last'img );
        end;
     end if;
  end if;
end ParseTemplatesSetHTTPStatus;

procedure ParseTemplatesSetHTTPLocation is
  -- Set the HTTP result location field
  -- Syntax: templates.set_http_location( s );
  -- Ada:    N/A
  exprVal : unbounded_string;
  exprKind : identifier;
begin
  expect( templates_set_http_location_t );
  ParseSingleStringParameter( templates_set_http_location_t, exprVal, exprKind );
  baseTypesOK( exprKind, string_t );
  if isExecutingCommand then
     if templateHeader.templateHeaderSent then
        err( "HTTP header already sent" );
     elsif length( exprval ) = 0 then
        err( "HTTP location string is empty" );
     else
        begin
           templateHeader.location := exprVal;
        exception when storage_error =>
           err( "out of memory" );
        when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseTemplatesSetHTTPLocation;

procedure ParseTemplatesPutTemplateHeader is
  -- Syntax: templates.put_template_header
  -- Ada:    N/A
begin
  expect( templates_put_template_header_t );
  if isExecutingCommand then
     begin
       putTemplateHeader( templateHeader );
     exception when constraint_error =>
       err( "constraint error - did you use pragma template?" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseTemplatesPutTemplateHeader;

procedure ParseTemplatesHasPutTemplateHeader( result : out unbounded_string;
  kind : out identifier ) is
  -- Syntax: b := templates.has_put_template_header
  -- Ada:    N/A
begin
  expect( templates_has_put_template_header_t );
  kind := boolean_t;
  if isExecutingCommand then
     result := to_bush_boolean( templateHeader.templateHeaderSent );
  end if;
end ParseTemplatesHasPutTemplateHeader;

------------------------------------------------------------------------------
-- Housekeeping
------------------------------------------------------------------------------

procedure StartupTemplates is
begin
  declareNamespace( "templates" );

  declareProcedure( templates_set_http_status_t, "templates.set_http_status", ParseTemplatesSetHTTPStatus'access );
  declareProcedure( templates_set_http_location_t, "templates.set_http_location", ParseTemplatesSetHTTPLocation'access );
  declareProcedure( templates_put_template_header_t, "templates.put_template_header", ParseTemplatesPutTemplateHeader'access );
  declareFunction( templates_has_put_template_header_t, "templates.has_put_template_header", ParseTemplatesHasPutTemplateHeader'access );

  declareNamespaceClosed( "templates" );
end StartupTemplates;

procedure ShutdownTemplates is
begin
  null;
end ShutdownTemplates;

end parser_templates;
