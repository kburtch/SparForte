------------------------------------------------------------------------------
-- Tags   Package Parser                                                    --
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

use world.metaLabelHashedSet;

package body parser_tags is

------------------------------------------------------------------------------
-- tags package identifiers
------------------------------------------------------------------------------

tags_has_unit_t           : identifier;
tags_contains_unit_t      : identifier;
tags_get_unit_image_t     : identifier;
tags_has_policies_t       : identifier;
tags_contains_policy_t    : identifier;
tags_get_policies_image_t : identifier;


-----------------------------------------------------------------------------
--  PARSE TAGS HAS UNIT                                   (built-in function)
--
-- AdaScript Syntax: b := tags.has_unit( x );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_tags.html#tags.has_unit
-----------------------------------------------------------------------------

procedure ParseTagsHasUnit( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := tags_has_unit_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( expr, expr_type );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := storage'( to_spar_boolean( expr.unitMetaLabel /= noMetaLabel ), noMetaLabel, noMetaLabels );
  end if;
end ParseTagsHasUnit;


-----------------------------------------------------------------------------
--  PARSE TAGS CONTAINS UNIT                              (built-in function)
--
-- AdaScript Syntax: b := tags.contains_unit( x, t ));
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_tags.html#tags.contains_unit
-----------------------------------------------------------------------------

procedure ParseTagsContainsUnit( result : out storage; kind : out identifier ) is
  expr         : storage;
  expr_type    : identifier;
  tagId        : metaLabelID;
  limitedVarId : identifier;
  subprogramId : constant identifier := tags_contains_unit_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  -- An arbitrary expression can be tested.  However, expressions cannot
  -- contain a limited variable, such as file or directory resource, and
  -- those also need to be checked.  Abstract variables do not exist.
  if identifiers( token ).usage = limitedUsage then
     Parseidentifier( limitedVarId );
     expr := identifiers( limitedVarId ).store.all;
  else
     ParseExpression( expr, expr_type );
  end if;
  expect( symbol_t, "," );
  ParseIdentifier( tagId );
  if class_ok( tagId, unitMetaClass ) then
     expect( symbol_t, ")" );
  else
     err( +"unit value meta tag expected" );
  end if;
  if isExecutingCommand then
     result := storage'( to_spar_boolean( expr.unitMetaLabel = tagid ), noMetaLabel, noMetaLabels );
  end if;
end ParseTagsContainsUnit;


-----------------------------------------------------------------------------
--  PARSE TAGS GET UNIT IMAGE                             (built-in function)
--
-- AdaScript Syntax: b := tags.get_unit_image( x );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_tags.html#tags.get_unit_image
-----------------------------------------------------------------------------

procedure ParseTagsGetUnitImage( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := tags_get_unit_image_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( expr, expr_type );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := storage'( identifiers( expr.unitMetaLabel ).name, noMetaLabel, noMetaLabels );
  end if;
end ParseTagsGetUnitImage;


-----------------------------------------------------------------------------
--  PARSE TAGS HAS POLICIES                               (built-in function)
--
-- AdaScript Syntax: b := tags.has_policies( x );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_tags.html#tags.has_policies
-----------------------------------------------------------------------------

procedure ParseTagsHasPolicies( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := tags_has_policies_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( expr, expr_type );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := storage'( to_spar_boolean( expr.policyMetaLabels /= noMetaLabels ), noMetaLabel, noMetaLabels );
  end if;
end ParseTagsHasPolicies;


-----------------------------------------------------------------------------
--  PARSE TAGS CONTAINS POLICY                            (built-in function)
--
-- AdaScript Syntax: b := tags.contains_policy( x, t ));
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_tags.html#tags.contains_policy
-----------------------------------------------------------------------------

procedure ParseTagsContainsPolicy( result : out storage; kind : out identifier ) is
  expr         : storage;
  expr_type    : identifier;
  tagId        : identifier;
  limitedVarId : identifier;
  subprogramId : constant identifier := tags_contains_policy_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  -- An arbitrary expression can be tested.  However, expressions cannot
  -- contain a limited variable, such as file or directory resource, and
  -- those also need to be checked.  Abstract variables do not exist.
  if identifiers( token ).usage = limitedUsage then
     Parseidentifier( limitedVarId );
     expr := identifiers( limitedVarId ).store.all;
  else
     ParseExpression( expr, expr_type );
  end if;
  expect( symbol_t, "," );
  ParseIdentifier( tagId );
  if class_ok( tagId, policyMetaClass ) then
     expect( symbol_t, ")" );
  else
     err( +"policy value meta tag expected" );
  end if;
  if isExecutingCommand then
     result := storage'( to_spar_boolean( expr.policyMetaLabels.contains( tagid ) ), noMetaLabel, noMetaLabels );
  end if;
end ParseTagsContainsPolicy;


-----------------------------------------------------------------------------
--  PARSE TAGS GET POLICIES IMAGE                         (built-in function)
--
-- AdaScript Syntax: b := tags.get_policies_image( x );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_tags.html#tags.get_policies_image
-----------------------------------------------------------------------------

procedure ParseTagsGetPoliciesImage( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := tags_get_policies_image_t;
begin
  kind := string_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( expr, expr_type );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := storage'( image( expr.policyMetaLabels ), noMetaLabel, noMetaLabels );
  end if;
end ParseTagsGetPoliciesImage;

-- Future: Class tag handling here


------------------------------------------------------------------------------
-- Housekeeping
------------------------------------------------------------------------------


procedure StartupTags is
begin
  declareNamespace( "tags" );
  declareFunction( tags_has_unit_t, "tags.has_unit", ParseTagsHasUnit'access );
  declareFunction( tags_contains_unit_t, "tags.contains_unit", ParseTagsContainsUnit'access );
  declareFunction( tags_get_unit_image_t, "tags.get_unit_image",ParseTagsGetUnitImage'access );
  declareFunction( tags_has_policies_t, "tags.has_policies", ParseTagsHasPolicies'access );
  declareFunction( tags_contains_policy_t, "tags.contains_policy", ParseTagsContainsPolicy'access );
  declareFunction( tags_get_policies_image_t, "tags.get_policies_image",ParseTagsGetPoliciesImage'access );
  declareNamespaceClosed( "tags" );
end StartupTags;

procedure ShutdownTags is
begin
  null;
end ShutdownTags;

end parser_tags;
