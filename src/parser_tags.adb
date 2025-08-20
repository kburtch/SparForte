------------------------------------------------------------------------------
-- Tags   Package Parser                                                    --
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
    ada.strings.unbounded,
    world,
    symbol_table,
    message_strings,
    value_conversion,
    scanner,
    scanner.communications,
    parser;
use
    ada.strings.unbounded,
    world,
    symbol_table,
    message_strings,
    value_conversion,
    scanner,
    scanner.communications,
    parser;

package body parser_tags is

------------------------------------------------------------------------------
-- tags package identifiers
------------------------------------------------------------------------------

tags_has_tags_t       : identifier;
tags_contains_t       : identifier;
tags_get_tags_image_t : identifier;


------------------------------------------------------------------------------
--  HAS TAGS
--
-- Syntax: b := tags.has_tags( ex )
-- Ada:    N/A
------------------------------------------------------------------------------

procedure ParseTagsHasTags( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := tags_has_tags_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( expr, expr_type );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := storage'( to_spar_boolean( expr.metaLabel /= noMetaLabel ), noMetaLabel );
  end if;
end ParseTagsHasTags;


------------------------------------------------------------------------------
--  TAGS CONTAINS
--
-- Syntax: b := tags.contains( x, t)
-- Ada:    N/A
------------------------------------------------------------------------------

procedure ParseTagsContains( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  tagId : identifier;
  subprogramId : constant identifier := tags_contains_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( expr, expr_type );
  expect( symbol_t, "," );
  if identifiers( token ).kind = meta_t then
     ParseIdentifier( tagId );
     expect( symbol_t, ")" );
  else
     err( +"meta tag expected" );
  end if;
  if isExecutingCommand then
     result := storage'( to_spar_boolean( expr.metaLabel = tagid ), noMetaLabel );
  end if;
end ParseTagsContains;


------------------------------------------------------------------------------
--  TAGS GET TAGS IMAGE
--
-- Syntax: s := tags.get_tags_image( ex )
-- Ada:    N/A
------------------------------------------------------------------------------

procedure ParseTagsGetTagsImage( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := tags_get_tags_image_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( expr, expr_type );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := storage'( identifiers( expr.metaLabel ).name, noMetaLabel );
  end if;
end ParseTagsGetTagsImage;


------------------------------------------------------------------------------
-- Housekeeping
------------------------------------------------------------------------------


procedure StartupTags is
begin
  declareNamespace( "tags" );
  declareFunction( tags_has_tags_t, "tags.has_tags", ParseTagsHasTags'access );
  declareFunction( tags_contains_t, "tags.contains", ParseTagsContains'access );
  declareFunction( tags_get_tags_image_t, "tags.get_tags_image",ParseTagsGetTagsImage'access );
  declareNamespaceClosed( "tags" );
end StartupTags;

procedure ShutdownTags is
begin
  null;
end ShutdownTags;

end parser_tags;
