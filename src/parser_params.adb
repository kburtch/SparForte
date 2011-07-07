------------------------------------------------------------------------------
-- Memcache Parckage Parser                                                 --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2011 Free Software Foundation              --
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

with ada.text_io;
use ada.text_io;
with gen_list,
    ada.strings.unbounded,
    world,
    scanner,
    parser_aux,
    parser;
use ada.strings,
    ada.strings.unbounded,
    world,
    scanner,
    parser_aux,
    parser;

package body parser_params is

-----------------------------------------------------------------------------

discard_result : boolean;

-- parser has Out/InOut Param and should be moved here

--  PARSE SINGLE STRING PARAMETER
--
-- Expect a parameter with a single string expression.  If there is no expected
-- type, assume it's a universal string type.

procedure ParseSingleStringParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t  ) is
begin
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if isExecutingCommand then
     expr_val := castToType( expr_val, expected_type );
  end if;
  discard_result := baseTypesOk( expr_type, expected_type );
  expect( symbol_t, ")" );
end ParseSingleStringParameter;


--  PARSE FIRST IN OUT PARAMETER
--
-- Expect a first parameter that is a numeric expression.  If there is no expected
-- type, assume it's a universal string type.

procedure ParseFirstInOutParameter( param_id : out identifier; expected_type : identifier  ) is
begin
  expect( symbol_t, "(" );
  ParseIdentifier( param_id ); -- in out
  --ParseExpression( expr_val, expr_type );
  --if isExecutingCommand then
  --   expr_val := castToType( expr_val, expected_type );
  --end if;
  discard_result := baseTypesOk( identifiers( param_id ).kind, expected_type );
end ParseFirstInOutParameter;


--  PARSE SINGLE IN OUT PARAMETER
--
-- Expect a first parameter that is a numeric expression.  If there is no expected
-- type, assume it's a universal string type.

procedure ParseSingleInOutParameter( param_id : out identifier; expected_type : identifier  ) is
begin
  expect( symbol_t, "(" );
  ParseIdentifier( param_id ); -- in out
  --ParseExpression( expr_val, expr_type );
  --if isExecutingCommand then
  --   expr_val := castToType( expr_val, expected_type );
  --end if;
  discard_result := baseTypesOk( identifiers( param_id ).kind, expected_type );
  expect( symbol_t, ")" );
end ParseSingleInOutParameter;


--  PARSE FIRST STRING PARAMETER
--
-- Expect a first parameter that is string expression.  If there is no expected
-- type, assume it's a universal string type.

procedure ParseFirstStringParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t ) is
begin
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if isExecutingCommand then
     expr_val := castToType( expr_val, expected_type );
  end if;
  discard_result := baseTypesOk( expr_type, expected_type );
end ParseFirstStringParameter;


--  PARSE NEXT STRING PARAMETER
--
-- Expect another parameter that is string expression.  If there is no expected
-- type, assume it's a universal string type.

procedure ParseNextStringParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t ) is
begin
  expect( symbol_t, "," );
  ParseExpression( expr_val, expr_type );
  if isExecutingCommand then
     expr_val := castToType( expr_val, expected_type );
  end if;
  discard_result := baseTypesOk( expr_type, expected_type );
end ParseNextStringParameter;


--  PARSE LAST STRING PARAMETER
--
-- Expect another parameter that is string expression.  If there is no expected
-- type, assume it's a universal string type.

procedure ParseLastStringParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t ) is
begin
  expect( symbol_t, "," );
  ParseExpression( expr_val, expr_type );
  if isExecutingCommand then
     expr_val := castToType( expr_val, expected_type );
  end if;
  discard_result := baseTypesOk( expr_type, expected_type );
  expect( symbol_t, ")" );
end ParseLastStringParameter;


--  PARSE LAST ENUM PARAMETER
--
-- Expect another parameter that is an enum expression.

procedure ParseLastEnumParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier ) is
begin
  expect( symbol_t, "," );
  ParseExpression( expr_val, expr_type );
  -- no cast to type
  discard_result := baseTypesOk( expr_type, expected_type );
  expect( symbol_t, ")" );
end ParseLastEnumParameter;


--  PARSE SINGLE NUMERIC PARAMETER
--
-- typeTypesOk not yet implemented here

procedure ParseSingleNumericParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_numeric_t ) is
begin
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if isExecutingCommand then
     expr_val := castToType( expr_val, expected_type );
  end if;
  discard_result := intTypesOk( expr_type, expected_type );
  expect( symbol_t, ")" );
end ParseSingleNumericParameter;

--  PARSE FIRST NUMERIC PARAMETER
--

procedure ParseFirstNumericParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_numeric_t ) is
begin
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if isExecutingCommand then
     expr_val := castToType( expr_val, expected_type );
  end if;
  discard_result := intTypesOk( expr_type, expected_type );
end ParseFirstNumericParameter;


--  PARSE NEXT NUMERIC PARAMETER
--

procedure ParseNextNumericParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_numeric_t ) is
begin
  expect( symbol_t, "," );
  ParseExpression( expr_val, expr_type );
  if isExecutingCommand then
     expr_val := castToType( expr_val, expected_type );
  end if;
  discard_result := intTypesOk( expr_type, expected_type );
end ParseNextNumericParameter;


--  PARSE LAST NUMERIC PARAMETER
--

procedure ParseLastNumericParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_numeric_t ) is
begin
  expect( symbol_t, "," );
  ParseExpression( expr_val, expr_type );
  if isExecutingCommand then
     expr_val := castToType( expr_val, expected_type );
  end if;
  discard_result := intTypesOk( expr_type, expected_type );
  expect( symbol_t, ")" );
end ParseLastNumericParameter;

end parser_params;
