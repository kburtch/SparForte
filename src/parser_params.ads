------------------------------------------------------------------------------
-- Parameter Parser                                                         --
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

with ada.strings.unbounded, ada.numerics.float_random, world;
use ada.strings.unbounded, world;

package parser_params is


------------------------------------------------------------------------------
-- String Parameters
------------------------------------------------------------------------------

procedure ParseSingleStringParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t  );

procedure ParseFirstStringParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t );

procedure ParseNextStringParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t );

procedure ParseLastStringParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_string_t );

------------------------------------------------------------------------------
-- Enumerated Parameters
------------------------------------------------------------------------------

procedure ParseLastEnumParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier );

------------------------------------------------------------------------------
-- Numeric Parameters
------------------------------------------------------------------------------

procedure ParseSingleNumericParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_numeric_t );

procedure ParseFirstNumericParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_numeric_t );

procedure ParseNextNumericParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_numeric_t );

procedure ParseLastNumericParameter( expr_val : out unbounded_string;
  expr_type : out identifier; expected_type : identifier := uni_numeric_t );

------------------------------------------------------------------------------
-- In/Out Parameters
------------------------------------------------------------------------------

procedure ParseSingleInOutParameter( param_id : out identifier; expected_type : identifier  );

procedure ParseFirstInOutParameter( param_id : out identifier; expected_type : identifier  );

------------------------------------------------------------------------------
-- Out Parameters
------------------------------------------------------------------------------

end parser_params;

