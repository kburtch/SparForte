------------------------------------------------------------------------------
-- BUSH Numerics Package Parser                                             --
--                                                                          --
-- Part of BUSH                                                             --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2005 Ken O. Burtch & FSF                 --
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
-- CVS: $Id: parser_numerics.ads,v 1.2 2005/02/11 02:59:27 ken Exp $

with ada.strings.unbounded, ada.numerics.float_random, world;
use ada.strings.unbounded, world;

package parser_numerics is

------------------------------------------------------------------------------
-- Numerics package identifiers
------------------------------------------------------------------------------

numerics_e_t : identifier;  -- numerics constants
numerics_log2_e_t : identifier;
numerics_log10_e_t : identifier;
numerics_ln10_t : identifier;
numerics_ln2_t : identifier;
numerics_pi_t : identifier;
numerics_pi_by_2_t : identifier;
numerics_pi_by_4_t : identifier;
numerics_pi_under_1_t : identifier;
numerics_pi_under_2_t : identifier;
numerics_sqrt_pi_under_2_t : identifier;
numerics_sqrt_2_t : identifier;
numerics_sqrt_2_under_1_t : identifier;

random_t       : identifier;      -- built-in ada.numerics.float_random.random
shift_left_t   : identifier;      -- Interfaces.shift_left
shift_right_t  : identifier;      -- Interfaces.shift_right
rotate_left_t  : identifier;      -- Interfaces.rotate_left
rotate_right_t : identifier;      -- Interfaces.rotate_right
shift_right_arith_t : identifier; -- Interfaces.shift_right_arithmetic
sqrt_t       : identifier; -- ada.numerics.long_elementary_functions
log_t        : identifier;
exp_t        : identifier;
sin_t        : identifier;
cos_t        : identifier;
tan_t        : identifier;
cot_t        : identifier;
arcsin_t     : identifier;
arccos_t     : identifier;
arctan_t     : identifier;
arccot_t     : identifier;
sinh_t       : identifier;
cosh_t       : identifier;
tanh_t       : identifier;
coth_t       : identifier;
arcsinh_t    : identifier;
arccosh_t    : identifier;
arctanh_t    : identifier;
arccoth_t    : identifier;
floor_t      : identifier;  -- numeric attributes
ceiling_t    : identifier;
rounding_t : identifier;
unbiased_rounding_t : identifier;
truncation_t : identifier;
remainder_t  : identifier;
exponent_t   : identifier;
fraction_t   : identifier;
leading_part_t : identifier;
copy_sign_t  : identifier;
max_t        : identifier;
min_t        : identifier;
machine_t    : identifier;
scaling_t    : identifier;
value_t      : identifier;
pos_t        : identifier;
sturges_t    : identifier;    -- other
numerics_md5_t : identifier;
serial_t     : identifier;
rnd_t        : identifier;
odd_t        : identifier;
even_t       : identifier;
numerics_re_t : identifier;
numerics_im_t : identifier;
numerics_set_re_t : identifier;
numerics_set_im_t : identifier;
numerics_argument_t : identifier;
numerics_modulus_t : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupNumerics;
procedure ShutdownNumerics;

------------------------------------------------------------------------------
-- PARSE THE NUMERICS PACKAGE
------------------------------------------------------------------------------

procedure ParseNumericsAbs( result : out unbounded_string );
procedure ParseNumericsRandom( result : out unbounded_string );
procedure ParseNumericsShiftLeft( result : out unbounded_string );
procedure ParseNumericsShiftRight( result : out unbounded_string );
procedure ParseNumericsRotateLeft( result : out unbounded_string );
procedure ParseNumericsRotateRight( result : out unbounded_string );
procedure ParseNumericsASR( result : out unbounded_string );
procedure ParseNumericsSqrt( result : out unbounded_string );
procedure ParseNumericsLog( result : out unbounded_string );
procedure ParseNumericsExp( result : out unbounded_string );
procedure ParseNumericsSin( result : out unbounded_string );
procedure ParseNumericsCos( result : out unbounded_string );
procedure ParseNumericsTan( result : out unbounded_string );
procedure ParseNumericsCot( result : out unbounded_string );
procedure ParseNumericsArcSin( result : out unbounded_string );
procedure ParseNumericsArcCos( result : out unbounded_string );
procedure ParseNumericsArcTan( result : out unbounded_string );
procedure ParseNumericsArcCot( result : out unbounded_string );
procedure ParseNumericsSinH( result : out unbounded_string );
procedure ParseNumericsCosH( result : out unbounded_string );
procedure ParseNumericsTanH( result : out unbounded_string );
procedure ParseNumericsCoth( result : out unbounded_string );
procedure ParseNumericsArcSinH( result : out unbounded_string );
procedure ParseNumericsArcCosH( result : out unbounded_string );
procedure ParseNumericsArcTanH( result : out unbounded_string );
procedure ParseNumericsArcCotH( result : out unbounded_string );
procedure ParseNumericsFloor( result : out unbounded_string );
procedure ParseNumericsCeiling( result : out unbounded_string );
procedure ParseNumericsRounding( result : out unbounded_string );
procedure ParseNumericsUnbiasedRounding( result : out unbounded_string );
procedure ParseNumericsTruncation( result : out unbounded_string );
procedure ParseNumericsRemainder( result : out unbounded_string );
procedure ParseNumericsExponent( result : out unbounded_string );
procedure ParseNumericsFraction( result : out unbounded_string );
procedure ParseNumericsLeadingPart( result : out unbounded_string );
procedure ParseNumericsCopySign( result : out unbounded_string );
procedure ParseNumericsSturges( result : out unbounded_string );
procedure ParseNumericsMax( result : out unbounded_string );
procedure ParseNumericsMin( result : out unbounded_string );
procedure ParseNumericsMachine( result : out unbounded_string );
procedure ParseNumericsScaling( result : out unbounded_string );
procedure ParseNumericsValue( result : out unbounded_string );
procedure ParseNumericsPos( result : out unbounded_string );
procedure ParseNumericsMd5( result : out unbounded_string );
procedure ParseNumericsSerial( result : out unbounded_string );
procedure ParseNumericsRnd( result : out unbounded_string );
procedure ParseNumericsOdd( result : out unbounded_string );
procedure ParseNumericsEven( result : out unbounded_string );
procedure ParseNumericsRe( result : out unbounded_string );
procedure ParseNumericsIm( result : out unbounded_string );
procedure ParseNumericsSetRe;
procedure ParseNumericsSetIm;
--procedure ParseNumericsSetComposeFromCartisian( result : out unbounded_string );
procedure ParseNumericsModulus( result : out unbounded_string );
procedure ParseNumericsArgument( result : out unbounded_string );
--procedure ParseNumericsComposeFromPolar( result : out unbounded_string );
--procedure ParseNumericsConjugate( result : out unbounded_string );

end parser_numerics;
