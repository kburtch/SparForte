------------------------------------------------------------------------------
-- Parameter Parser                                                         --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
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

with ada.strings.unbounded,
     ada.numerics.float_random,
     world;
use  ada.strings.unbounded,
     world;

package parser_params is

------------------------------------------------------------------------------
-- Parameter references
------------------------------------------------------------------------------

procedure AssignParameter( ref : in reference; value : unbounded_string );
pragma inline( AssignParameter );
-- assign a value to the variable or array indicated by ref

procedure GetParameterValue( ref : in reference; value : out unbounded_string );
pragma inline( GetParameterValue );
-- return the value of the variable or array indicated by ref

------------------------------------------------------------------------------
-- Renaming Declarations
------------------------------------------------------------------------------

procedure ParseRenamingReference( ref : out renamingReference; expectedType : identifier );
-- parse a reference used in a "renames" clause in a declaration

------------------------------------------------------------------------------
-- Unchecked Parameters
------------------------------------------------------------------------------

procedure ParseNextGenItemParameter(
  subprogram : identifier;
  expr_val : out unbounded_string;
  expr_type : out identifier;
  expected_type : identifier := uni_string_t );

procedure ParseLastGenItemParameter(
  subprogram : identifier;
  expr_val : out unbounded_string;
  expr_type : out identifier;
  expected_type : identifier := uni_string_t );

procedure ParseGenItemParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t );

------------------------------------------------------------------------------
-- String Parameters
------------------------------------------------------------------------------

procedure ParseStringParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t  );

procedure ParseSingleStringParameter(
  subprogram : identifier;
  expr_val : out unbounded_string;
  expr_type : out identifier;
  expected_type : identifier := uni_string_t  );

procedure ParseFirstStringParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t );

procedure ParseNextStringParameter(
  subprogram : identifier;
  expr_val : out unbounded_string;
  expr_type : out identifier;
  expected_type : identifier := uni_string_t );

procedure ParseLastStringParameter(
  subprogram : identifier;
  expr_val : out unbounded_string;
  expr_type : out identifier;
  expected_type : identifier := uni_string_t );

------------------------------------------------------------------------------
-- Enumerated Parameters
------------------------------------------------------------------------------

procedure ParseEnumParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier );

procedure ParseSingleEnumParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier );

procedure ParseFirstEnumParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier );

procedure ParseNextEnumParameter(
  subprogram : identifier;
  expr_val : out unbounded_string;
  expr_type : out identifier;
  expected_type : identifier );

procedure ParseLastEnumParameter(
  subprogram : identifier;
  expr_val : out unbounded_string;
  expr_type : out identifier;
  expected_type : identifier );

------------------------------------------------------------------------------
-- Numeric Parameters
------------------------------------------------------------------------------

procedure ParseSingleNumericParameter(
  subprogram : identifier;
  expr_val : out unbounded_string;
  expr_type : out identifier;
  expected_type : identifier := uni_numeric_t );

procedure ParseFirstNumericParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_numeric_t );

procedure ParseNextNumericParameter(
  subprogram : identifier;
  expr_val : out unbounded_string;
  expr_type : out identifier;
  expected_type : identifier := uni_numeric_t );

procedure ParseLastNumericParameter(
  subprogram : identifier;
  expr_val : out unbounded_string;
  expr_type : out identifier;
  expected_type : identifier := uni_numeric_t );

procedure ParseNumericParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_numeric_t );

------------------------------------------------------------------------------
-- In/Out Parameters
------------------------------------------------------------------------------

procedure ParseSingleInOutParameter(
  subprogram : identifier;
  param_id : out identifier;
  expected_type : identifier  );


procedure ParseFirstInOutParameter( param_id : out identifier; expected_type : identifier  );

procedure ParseNextInOutParameter(
  subprogram : identifier;
  param_id : out identifier;
  expected_type : identifier  );

procedure ParseLastInOutParameter(
  subprogram : identifier;
  param_id : out identifier;
  expected_type : identifier  );

procedure ParseInOutParameter( ref : out reference );
-- TODO: should modify others to also use a reference

procedure ParseLastInOutRecordParameter( subprogram : identifier; param_id : out identifier );

------------------------------------------------------------------------------
-- Instantiated Generics Parameters
------------------------------------------------------------------------------

procedure ParseInOutInstantiatedParameter( param_id : out identifier; expected_type : identifier  );

procedure ParseSingleInOutInstantiatedParameter(
  subprogram : identifier;
  param_id : out identifier;
  expected_type : identifier );

procedure ParseFirstInOutInstantiatedParameter( param_id : out identifier; expected_type : identifier  );

procedure ParseNextInOutInstantiatedParameter(
  subprogram : identifier;
  param_id : out identifier;
  expected_type : identifier  );

procedure ParseLastInOutInstantiatedParameter(
  subprogram : identifier;
  param_id : out identifier;
  expected_type : identifier  );

------------------------------------------------------------------------------
-- Out Parameters
------------------------------------------------------------------------------

procedure ParseOutParameter( ref : out reference; defaultType : identifier );

procedure ParseSingleOutParameter( ref : out reference; defaultType : identifier );

procedure ParseFirstOutParameter( ref : out reference; defaultType : identifier );

procedure ParseNextOutParameter(
  subprogram : identifier;
  ref : out reference;
  defaultType : identifier );

procedure ParseLastOutParameter(
  subprogram : identifier;
  ref : out reference;
  defaultType : identifier );

end parser_params;

