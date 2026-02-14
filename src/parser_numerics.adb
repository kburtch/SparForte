------------------------------------------------------------------------------
-- Numerics Parckage Parser                                                 --
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

--with ada.text_io; use ada.text_io;

with ada.numerics.float_random,
    ada.strings,
    interfaces,
    gnat.sha1,
    gnat.sha224,
    gnat.sha256,
    gnat.sha512,
    --pegasoft.user_io,
    pegasoft.numerics,
    symbol_table,
    message_strings,
    value_conversion,
    scanner,
    scanner.communications,
    parser_params,
    parser,
    md5;
use interfaces,
    pegasoft,
    --pegasoft.user_io,
    pegasoft.numerics,
    pegasoft.numerics.elementary_functions,
    pegasoft.numerics.complex_types,
    symbol_table,
    message_strings,
    value_conversion,
    scanner,
    scanner.communications,
    parser_params,
    parser,
    md5;

package body parser_numerics is

serialNumber : numericValue := 0.0;

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
hash_of_t         : identifier;
sdbm_hash_of_t    : identifier;
fnv_hash_of_t     : identifier;
murmur_hash_of_t  : identifier;
sha1_digest_of_t   : identifier;
sha224_digest_of_t : identifier;
sha256_digest_of_t : identifier;
sha512_digest_of_t : identifier;
shannon_entropy_of_t : identifier;



-----------------------------------------------------------------------------

--discard_result : boolean;

-----------------------------------------------------------------------------

procedure ParseNumericsRandom( result : out storage; kind : out identifier ) is
  -- Syntax: random
  -- Source: Ada.Numerics.Float_Random.Random
  subprogramId : constant identifier := random_t;
begin
  kind := float_t;
  result := nullStorage;
  expect( subprogramId );
  if isExecutingCommand then
     result.value := to_unbounded_string( numericValue( Ada.Numerics.Float_Random.Random(
       random_generator ) ) );
     result.unitMetaLabel := noMetaLabel;
     result.policyMetaLabels := sparMetaLabels;
  end if;
end ParseNumericsRandom;

procedure ParseNumericsShiftLeft( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.shift_left( x, b )
  -- Source: Interfaces
  expr_st   : storage;
  expr_type : identifier;
  amt_st    : storage;
  amt_type  : identifier := eof_t;
  subprogramId : constant identifier := shift_left_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, amt_st, amt_type, natural_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'( to_unbounded_string( numericValue( shift_left(
              unsigned_64( to_numeric( expr_st.value ) ),
              natural( to_numeric( amt_st.value ) )
           ) ) ), noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsShiftLeft;

procedure ParseNumericsShiftRight( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.shift_right( x, b )
  -- Source: Interfaces
  expr_st   : storage;
  expr_type : identifier;
  amt_st    : storage;
  amt_type  : identifier := eof_t;
  subprogramId : constant identifier := shift_right_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, amt_st, amt_type, natural_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'( to_unbounded_string( numericValue( shift_right(
              unsigned_64( to_numeric( expr_st.value ) ),
              natural( to_numeric( amt_st.value ) )
           ) ) ), noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsShiftRight;

procedure ParseNumericsRotateLeft( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.rotate_left( x, b )
  -- Source: Interfaces
  expr_st   : storage;
  expr_type : identifier;
  amt_st    : storage;
  amt_type  : identifier := eof_t;
  subprogramId : constant identifier := rotate_left_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, amt_st, amt_type, natural_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'( to_unbounded_string( numericValue( rotate_left(
              unsigned_64( to_numeric( expr_st.value ) ),
              natural( to_numeric( amt_st.value) )
           ) ) ), noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsRotateLeft;

procedure ParseNumericsRotateRight( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.rotate_right( x, b )
  -- Source: Interfaces
  expr_st   : storage;
  expr_type : identifier;
  amt_st    : storage;
  amt_type  : identifier := eof_t;
  subprogramId : constant identifier := rotate_right_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, amt_st, amt_type, natural_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'( to_unbounded_string( numericValue( rotate_right(
              unsigned_64( to_numeric( expr_st.value ) ),
              natural( to_numeric( amt_st.value) )
           ) ) ), noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsRotateRight;

procedure ParseNumericsASR( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.shift_right_arithmetic( x, b )
  -- Source: Interfaces
  expr_st   : storage;
  expr_type : identifier;
  amt_st    : storage;
  amt_type  : identifier := eof_t;
  subprogramId : constant identifier := shift_right_arith_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, amt_st, amt_type, natural_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'(to_unbounded_string( numericValue( shift_right_arithmetic(
              unsigned_64( to_numeric( expr_st.value ) ),
              natural( to_numeric( amt_st.value ) )
           ) ) ), noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsASR;

procedure ParseNumericsSqrt( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.sqrt( expr );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Sqrt
  expr_st   : storage;
  expr_type : identifier;
  subprogramId : constant identifier := sqrt_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'( to_unbounded_string( sqrt( to_numeric( expr_st.value ) ) ),
              noMetaLabel, expr_st.policyMetaLabels );
        end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsSqrt;

procedure ParseNumericsLog( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.log( expr [,base] )
  -- Source: Ada.Numerics.Long_Elementary_Functions.Log
  expr_st   : storage;
  expr_type : identifier;
  base_st   : storage;
  base_type : identifier := eof_t;
  subprogramId : constant identifier := log_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, base_st, base_type );
  else
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           if base_type = eof_t then
              result := storage'( to_unbounded_string( log( to_numeric( expr_st.value ) ) ),
                noMetaLabel, expr_st.policyMetaLabels );
           else
              result := storage'( to_unbounded_string( log( to_numeric( expr_st.value ),
                to_numeric( base_st.value ) ) ), noMetaLabel, expr_st.policyMetaLabels );
           end if;
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsLog;

procedure ParseNumericsExp( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.exp( expr );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Exp
  expr_st   : storage;
  expr_type : identifier;
  subprogramId : constant identifier := exp_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'( to_unbounded_string( exp( to_numeric( expr_st.value) ) ),
              noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when constraint_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the expression value" ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"is out-of-range",
          obstructorNotes => nullMessageStrings
     );
  when others =>
     err_exception_raised;
  end;
end ParseNumericsExp;

procedure ParseNumericsSin( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.sin( expr, [,cycle] );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Sin
  expr_st    : storage;
  expr_type  : identifier;
  cycle_st   : storage;
  cycle_type : identifier := eof_t;
  subprogramId : constant identifier := sin_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cycle_st, cycle_type );
  else
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           if cycle_type = eof_t then
              result := storage'(to_unbounded_string( sin( to_numeric( expr_st.value ) ) ),
                 noMetaLabel, expr_st.policyMetaLabels );
           else
              result := storage'( to_unbounded_string( sin( to_numeric( expr_st.value ),
                to_numeric( cycle_st.value ) ) ), noMetaLabel, expr_st.policyMetaLabels );
           end if;
        end if;
     end if;
  exception when ada.numerics.argument_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the cycle value" ) & em_value( cycle_st.value ),
          subjectType => cycle_type,
          reason  => +"should be greater than zero because",
          obstructorNotes => pl( "argument error was raised" )
     );
  when others =>
     err_exception_raised;
  end;
end ParseNumericsSin;

procedure ParseNumericsCos( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.cos( expr, [,cycle] );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Cos
  expr_st    : storage;
  expr_type  : identifier;
  cycle_st   : storage;
  cycle_type : identifier := eof_t;
  subprogramId : constant identifier := cos_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cycle_st, cycle_type );
  else
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          if cycle_type = eof_t then
             result := storage'( to_unbounded_string( cos( to_numeric( expr_st.value ) ) ),
                noMetaLabel, expr_st.policyMetaLabels );
          else
             result := storage'( to_unbounded_string( cos( to_numeric( expr_st.value ),
                to_numeric( cycle_st.value ) ) ), noMetaLabel, expr_st.policyMetaLabels );
          end if;
       end if;
    end if;
  exception when ada.numerics.argument_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the cycle value" ) & em_value( cycle_st.value ),
          subjectType => cycle_type,
          reason  => +"should be greater than zero because",
          obstructorNotes => pl( "argument error was raised" )
     );
  when others =>
     err_exception_raised;
  end;
end ParseNumericsCos;

procedure ParseNumericsTan( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.tan( expr, [,cycle] );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Tan
  expr_st    : storage;
  expr_type  : identifier;
  cycle_st   : storage;
  cycle_type : identifier := eof_t;
  subprogramId : constant identifier := tan_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cycle_st, cycle_type );
  else
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          if cycle_type = eof_t then
             result := storage'( to_unbounded_string( tan( to_numeric( expr_st.value ) ) ),
                noMetaLabel, expr_st.policyMetaLabels );
          else
             result := storage'( to_unbounded_string( tan( to_numeric( expr_st.value ),
               to_numeric( cycle_st.value ) ) ), noMetaLabel, expr_st.policyMetaLabels );
          end if;
       end if;
    end if;
  exception when ada.numerics.argument_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the cycle value" ) & em_value( cycle_st.value ),
          subjectType => cycle_type,
          reason  => +"should be greater than zero because",
          obstructorNotes => pl( "argument error was raised" )
     );
  when constraint_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the expression value" ) & em_value( expr_st.value ),
          subjectType => cycle_type,
          reason  => +"is an odd multiple of the quarter cycle or an overflow occurred because",
          obstructorNotes => pl( "constraint error was raised" )
     );
  when others =>
     err_exception_raised;
  end;
end ParseNumericsTan;

procedure ParseNumericsCot( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.cot( expr, [,cycle] );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Cot
  expr_st    : storage;
  expr_type  : identifier;
  cycle_st   : storage;
  cycle_type : identifier := eof_t;
  subprogramId : constant identifier := cot_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cycle_st, cycle_type );
  else
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          if cycle_type = eof_t then
             result := storage'( to_unbounded_string( cot( to_numeric( expr_st.value ) ) ),
                noMetaLabel, expr_st.policyMetaLabels );
          else
             result := storage'( to_unbounded_string( cot( to_numeric( expr_st.value ),
               to_numeric( cycle_st.value ) ) ), noMetaLabel, expr_st.policyMetaLabels );
          end if;
       end if;
    end if;
  exception when ada.numerics.argument_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the cycle value" ) & em_value( cycle_st.value ),
          subjectType => cycle_type,
          reason  => +"should be greater than zero because",
          obstructorNotes => pl( "argument error was raised" )
     );
  when constraint_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the expression value" ) & em_value( expr_st.value ),
          subjectType => cycle_type,
          reason  => +"is zero or an multiple of the half cycle or an overflow occurred because",
          obstructorNotes => pl( "constraint error was raised" )
     );
  when others =>
     err_exception_raised;
  end;
end ParseNumericsCot;

procedure ParseNumericsArcSin( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.ArcSin( expr, [,cycle] );
  -- Source: Ada.Numerics.Long_Elementary_Functions.ArcSin
  expr_st   : storage;
  expr_type   : identifier;
  cycle_st   : storage;
  cycle_type : identifier := eof_t;
  subprogramId : constant identifier := arcsin_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cycle_st, cycle_type );
  else
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          if cycle_type = eof_t then
             result := storage'( to_unbounded_string( arcsin( to_numeric( expr_st.value ) ) ),
                noMetaLabel, expr_st.policyMetaLabels );
          else
             result := storage'( to_unbounded_string( arcsin( to_numeric( expr_st.value),
               to_numeric( cycle_st.value ) ) ), noMetaLabel, expr_st.policyMetaLabels );
          end if;
       end if;
    end if;
  exception when ada.numerics.argument_error =>
     if to_numeric( cycle_st.value ) <= 0.0 then
        err( context => subprogramId,
          subjectNotes => pl( "the cycle value" ) & em_value( cycle_st.value ),
          subjectType => cycle_type,
          reason  => +"should be greater than zero because",
          obstructorNotes => pl( "argument error was raised" )
        );
     else
        err( context => subprogramId,
          subjectNotes => pl( "the expression value" ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"should be in the range -1..1 or an overflow occurred because",
          obstructorNotes => pl( "argument error was raised" )
        );
     end if;
  when others =>
     err_exception_raised;
  end;
end ParseNumericsArcSin;

procedure ParseNumericsArcCos( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.ArcCos( expr [,cycle] );
  -- Source: Ada.Numerics.Long_Elementary_Functions.ArcCos
  expr_st    : storage;
  expr_type  : identifier;
  cycle_st   : storage;
  cycle_type : identifier := eof_t;
  subprogramId : constant identifier := arccos_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cycle_st, cycle_type );
  else
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          if cycle_type = eof_t then
             result := storage'( to_unbounded_string( arccos( to_numeric( expr_st.value ) ) ),
                noMetaLabel, expr_st.policyMetaLabels );
          else
             result := storage'( to_unbounded_string( arccos( to_numeric( expr_st.value ),
               to_numeric( cycle_st.value ) ) ), noMetaLabel, expr_st.policyMetaLabels );
          end if;
       end if;
    end if;
  exception when ada.numerics.argument_error =>
     if to_numeric( cycle_st.value ) <= 0.0 then
        err( context => subprogramId,
          subjectNotes => pl( "the cycle value" ) & em_value( cycle_st.value ),
          subjectType => cycle_type,
          reason  => +"should be greater than zero because",
          obstructorNotes => pl( "argument error was raised" )
        );
     else
        err( context => subprogramId,
          subjectNotes => pl( "the expression value" ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"should be in the range -1..1 or an overflow occurred because",
          obstructorNotes => pl( "argument error was raised" )
        );
     end if;
  when others =>
     err_exception_raised;
  end;
end ParseNumericsArcCos;

procedure ParseNumericsArcTan( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.ArcTan( expr, expr2, [,cycle] );
  -- Source: Ada.Numerics.Long_Elementary_Functions.ArcTan
  -- note: second parameter is not optional in mine but is in Ada
  expr_st    : storage;
  expr_type  : identifier;
  expr2_st   : storage;
  expr2_type : identifier;
  cycle_st   : storage;
  cycle_type : identifier := eof_t;
  subprogramId : constant identifier := arctan_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseNextNumericParameter( subprogramId, expr2_st, expr2_type );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cycle_st, cycle_type );
  else
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st, expr2_st ) then
          if cycle_type = eof_t then
             result := storage'( to_unbounded_string( arctan( to_numeric( expr_st.value ),
               to_numeric( expr2_st.value ) ) ),
               noMetaLabel,
               resolveEffectiveMetaLabels( expr_type, expr_st, expr2_st ) );
          else
             result := storage'( to_unbounded_string( arctan( to_numeric( expr_st.value ),
               to_numeric( expr2_st.value ), to_numeric( cycle_st.value ) ) ),
               noMetaLabel,
               resolveEffectiveMetaLabels( expr_type, expr_st, expr2_st ) );
          end if;
       end if;
    end if;
  exception when ada.numerics.argument_error =>
        err( context => subprogramId,
          subjectNotes => pl( "the expression value " ) & em_value( expr_st.value ) &
            pl( " and " ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"both cannot be zero because",
          obstructorNotes => pl( "argument_error was raised" )
        );
  when constraint_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the expression value " ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"cannot be 1 or -1 or an overflow occurred because",
          obstructorNotes => pl( "constraint_error was raised" )
     );
  when others =>
     err_exception_raised;
  end;
end ParseNumericsArcTan;

procedure ParseNumericsArcCot( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.ArcCot( expr, expr2, [,cycle] );
  -- Source: Ada.Numerics.Long_Elementary_Functions.ArcCot
  -- note: second parameter is not optional in mine but is in Ada
  expr_st    : storage;
  expr_type  : identifier;
  expr2_st   : storage;
  expr2_type : identifier;
  cycle_st   : storage;
  cycle_type : identifier := eof_t;
  subprogramId : constant identifier := arccot_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseNextNumericParameter( subprogramId, expr2_st, expr2_type );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastNumericParameter( subprogramId, cycle_st, cycle_type );
  else
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st, expr2_st ) then
          if cycle_type = eof_t then
             result := storage'( to_unbounded_string( arccot( to_numeric( expr_st.value ),
               to_numeric( expr2_st.value ) ) ), noMetaLabel, resolveEffectiveMetaLabels( expr_type, expr_st, expr2_st ) );
          else
             result := storage'( to_unbounded_string( arccot( to_numeric( expr_st.value ),
               to_numeric( expr2_st.value ), to_numeric( cycle_st.value ) ) ),
               noMetaLabel, resolveEffectiveMetaLabels( expr_type, expr_st, expr2_st ) );
          end if;
       end if;
    end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsArcCot;

procedure ParseNumericsSinH( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.sinh( expr );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Sinh
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := sinh_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( sinh( to_numeric( expr_st.value ) ) ),
              noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsSinH;

procedure ParseNumericsCosH( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.cosh( expr );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Cosh
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := cosh_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( cosh( to_numeric( expr_st.value ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsCosH;

procedure ParseNumericsTanH( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.Tanh( expr );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Tanh
  expr_st   : storage;
  expr_type : identifier;
  subprogramId : constant identifier := tanh_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( tanh( to_numeric( expr_st.value) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsTanH;

procedure ParseNumericsCoth( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.coth( expr );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Coth
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := coth_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( coth(  to_numeric( expr_st.value ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when constraint_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the expression value " ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"cannot be zero or an overflow occurred because",
          obstructorNotes => pl( "constraint_error was raised" )
     );
  when others =>
    err_exception_raised;
  end;
end ParseNumericsCotH;

procedure ParseNumericsArcSinH( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.arcsinh( expr );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Arcsinh
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := arcsinh_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( arcsinh( to_numeric( expr_st.value ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsArcSinH;

procedure ParseNumericsArcCosH( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.arccosh( expr );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Arccosh
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := arccosh_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( arccosh( to_numeric( expr_st.value ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when ada.numerics.argument_error =>
        err( context => subprogramId,
          subjectNotes => pl( "the expression value " ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"is less than one because",
          obstructorNotes => pl( "argument_error was raised" )
        );
  when constraint_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the expression value " ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"cannot be one or an overflow occurred because",
          obstructorNotes => pl( "constraint_error was raised" )
     );
  when others =>
    err_exception_raised;
  end;
end ParseNumericsArcCosH;

procedure ParseNumericsArcTanH( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.arctanh( expr );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Arctanh
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := arctanh_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( arctanh( to_numeric( expr_st.value ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when ada.numerics.argument_error =>
        err( context => subprogramId,
          subjectNotes => pl( "the expression value " ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"should be in the range -1..1 because",
          obstructorNotes => pl( "argument_error was raised" )
        );
  when constraint_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the expression value " ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"cannot be one or an overflow occurred because",
          obstructorNotes => pl( "constraint_error was raised" )
     );
  when others =>
    err_exception_raised;
  end;
end ParseNumericsArcTanH;

procedure ParseNumericsArcCotH( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.arccoth( expr );
  -- Source: Ada.Numerics.Long_Elementary_Functions.Arccosh
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := arccoth_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( arccoth( to_numeric( expr_st.value ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when ada.numerics.argument_error =>
        err( context => subprogramId,
          subjectNotes => pl( "the expression value " ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"is less than one because",
          obstructorNotes => pl( "argument_error was raised" )
        );
  when constraint_error =>
     err( context => subprogramId,
          subjectNotes => pl( "the expression value " ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"cannot be one or an overflow occurred because",
          obstructorNotes => pl( "constraint_error was raised" )
     );
  when others =>
    err_exception_raised;
  end;
end ParseNumericsArcCotH;

procedure ParseNumericsFloor( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.floor( expr );
  -- Source: Ada 'floor attribute
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := floor_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( numericValue'floor( to_numeric( expr_st.value ) ) ),
            noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsFloor;

procedure ParseNumericsCeiling( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.ceiling( expr );
  -- Source: Ada 'ceiling attribute
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := ceiling_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( numericValue'ceiling( to_numeric( expr_st.value ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsCeiling;

procedure ParseNumericsRounding( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.rounding( expr );
  -- Source: Ada 'rounding attribute
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := rounding_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( numericValue'rounding( to_numeric( expr_st.value ) ) ),
          noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsRounding;

procedure ParseNumericsUnbiasedRounding( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.unbiased_rounding( expr );
  -- Source: Ada 'unbiased_rounding attribute
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := unbiased_rounding_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( numericValue'unbiased_rounding( to_numeric( expr_st.value ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsUnbiasedRounding;

procedure ParseNumericsTruncation( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.truncation( expr );
  -- Source: Ada 'truncation attribute
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := truncation_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
         result := storage'( to_unbounded_string( numericValue'truncation( to_numeric( expr_st.value ) ) ),
            noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsTruncation;

procedure ParseNumericsRemainder( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.remainder( expr, expr2 );
  -- Source: Ada 'remainder attribute
  expr_st : storage;
  expr_type : identifier;
  expr2_st : storage;
  expr2_type : identifier;
  subprogramId : constant identifier := remainder_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, expr2_st, expr2_type );
  begin
     if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) and metaLabelOk( remainder_t, expr2_st )  then
          result := storage'( to_unbounded_string( numericValue'remainder( to_numeric( expr_st.value ),
            to_numeric( expr2_st.value ) ) ), noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsRemainder;

procedure ParseNumericsExponent( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.exponent( expr );
  -- Source: Ada 'exponent attribute
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := exponent_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( numericValue( numericValue'exponent( to_numeric( expr_st.value ) ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsExponent;

procedure ParseNumericsFraction( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.fraction( expr );
  -- Source: Ada 'fraction attribute
  expr_st : storage;
  expr_type : identifier;
  subprogramId : constant identifier := fraction_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( numericValue'fraction( to_numeric( expr_st.value ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsFraction;

procedure ParseNumericsLeadingPart( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.leading_part( expr, expr2 );
  -- Source: Ada 'leading_part attribute
  expr_st : storage;
  expr_type : identifier;
  expr2_st : storage;
  expr2_type : identifier;
  subprogramId : constant identifier := leading_part_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, expr2_st, expr2_type );
  begin
     if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) and metaLabelOk( leading_part_t, expr2_st ) then
          result := storage'( to_unbounded_string( numericValue'leading_part( to_numeric( expr_st.value ),
             integer( to_numeric( expr2_st.value ) ) ) ), noMetaLabel, expr_st.policyMetaLabels );
       end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsLeadingPart;

procedure ParseNumericsCopySign( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.copy_sign( expr, expr2 );
  -- Source: Ada 'copy_sign attribute
  expr_st : storage;
  expr_type : identifier;
  expr2_st : storage;
  expr2_type : identifier;
  subprogramId : constant identifier := copy_sign_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, expr2_st, expr2_type );
  begin
     if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st, expr2_st ) then
          result := storage'( to_unbounded_string( numericValue'copy_sign( to_numeric( expr_st.value ),
            to_numeric( expr2_st.value ) ) ), noMetaLabel, resolveEffectiveMetaLabels( kind, expr_st, expr2_st ) );
       end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsCopySign;

procedure ParseNumericsSturges( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.sturges( low, high, total );
  -- Source: SparForte builtin, Sturge's method
  lo_st : storage;
  lo_type : identifier;
  hi_st : storage;
  hi_type : identifier;
  total_st : storage;
  total_type : identifier;
  lo, hi, total : numericValue;
  subprogramId : constant identifier := sturges_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, lo_st, lo_type );
  ParseNextNumericParameter( subprogramId, hi_st, hi_type );
  ParseLastNumericParameter( subprogramId, total_st, total_type );
  begin
     if isExecutingCommand then
       if metaLabelOk( subprogramId, lo_st, hi_st, total_st ) then
          lo := to_numeric( lo_st.value );
          hi := to_numeric( hi_st.value );
          total := to_numeric( total_st.value );
          result := storage'( to_unbounded_string(
             numericValue'rounding(
                (hi-lo) / 1.0+log( total )
             )
          ), noMetaLabel, resolveEffectiveMetaLabels( kind, lo_st, hi_st, total_st ) );
       end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsSturges;

procedure ParseNumericsMax( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.max( expr, expr2 );
  -- Source: Ada 'max attribute
  expr_st : storage;
  expr_type : identifier;
  expr2_st : storage;
  expr2_type : identifier;
  subprogramId : constant identifier := max_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, expr2_st, expr2_type );
  begin
     if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st, expr2_st ) then
          result := storage'( to_unbounded_string( numericValue'max( to_numeric( expr_st.value ),
             to_numeric( expr2_st.value ) ) ), noMetaLabel, resolveEffectiveMetaLabels(kind, expr_st, expr2_st ) );
       end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsMax;

procedure ParseNumericsMin( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.max( expr, expr2 );
  -- Source: Ada 'max attribute
  expr_st : storage;
  expr_type : identifier;
  expr2_st : storage;
  expr2_type : identifier;
  subprogramId : constant identifier := min_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, expr2_st, expr2_type );
  begin
     if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st, expr2_st ) then
          result := storage'( to_unbounded_string( numericValue'min( to_numeric( expr_st.value ),
             to_numeric( expr2_st.value ) ) ), noMetaLabel, resolveEffectiveMetaLabels(kind, expr_st, expr2_st ) );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsMin;

procedure ParseNumericsMachine( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.machine( expr );
  -- Source: Ada 'machine attribute
  expr_st    : storage;
  expr_type  : identifier;
  subprogramId : constant identifier := machine_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st ) then
          result := storage'( to_unbounded_string( numericValue'machine( to_numeric( expr_st.value ) ) ),
             noMetaLabel, expr_st.policyMetaLabels );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsMachine;

procedure ParseNumericsScaling( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.scaling( expr, expr2 );
  -- Source: Ada 'scaling attribute
  expr_st    : storage;
  expr_type  : identifier;
  expr2_st   : storage;
  expr2_type : identifier;
  subprogramId : constant identifier := scaling_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId,  expr_st, expr_type );
  ParseLastNumericParameter( subprogramId, expr2_st, expr2_type, integer_t );
  begin
     if isExecutingCommand then
       if metaLabelOk( subprogramId, expr_st, expr2_st ) then
          result := storage'( to_unbounded_string( numericValue'scaling( to_numeric( expr_st.value ),
            integer( to_numeric( expr2_st.value ) ) ) ),
            noMetaLabel, resolveEffectiveMetaLabels(kind, expr_st, expr2_st ) );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsScaling;

procedure ParseNumericsValue( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.value( expr );
  -- Source: Ada 'value attribute
  expr_st    : storage;
  expr_type  : identifier;
  subprogramId : constant identifier := value_t;
begin
  kind := uni_numeric_t;
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, expr_st, expr_type, string_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           --result := trim( storage'( to_unbounded_string( to_numeric( expr_st.value ) ), left );
           result := storage'( to_unbounded_string( to_numeric( expr_st.value ) ),
              noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception
  when constraint_error =>
     err( context => subprogramId,
          subjectNotes => pl( qp( "the string value " ) ) & em_value( expr_st.value ),
          subjectType => expr_type,
          reason  => +"is not a valid number because",
          obstructorNotes => pl( "constraint_error was raised" )
     );
  when storage_error =>
     err( +"storage_error exception raised" );
  when ada.strings.index_error =>
     if expr_st.value = null_unbounded_string then
        err(
          context => subprogramId,
          subjectNotes => pl( qp( "the numeric value" ) ),
          reason => +"cannot be calculated on",
          obstructorNotes => +"an empty string"
        );
     else
        err(
          context => subprogramId,
          subjectNotes => pl( qp( "the numeric value" ) ),
          reason => +"raised a index_error on the string value",
          obstructorNotes => em_value( expr_st.value ),
          obstructortype => expr_type
        );
     end if;
  when others =>
     err_exception_raised;
  end;
end ParseNumericsValue;

procedure ParseNumericsPos( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.pos( character );
  -- Source: Ada 'pos attribute
  expr_st    : storage;
  expr_type  : identifier;
  subprogramId : constant identifier := pos_t;
begin
  kind := positive_t;
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, expr_st, expr_type, character_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'( to_unbounded_string( character'pos( Element( expr_st.value, 1 ) )'img ),
              noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsPos;

procedure ParseNumericsAbs( result : out storage ) is
  -- Syntax: abs( n );
  -- Source: Ada abs function
  expr_st    : storage;
  expr_type  : identifier;
  subprogramId : constant identifier := abs_t;
begin
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'(
              to_unbounded_string( abs( to_numeric( expr_st.value ) ) ),
              noMetaLabel, expr_st.policyMetaLabels
           );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsAbs;

procedure ParseNumericsMd5( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.md5( s );
  -- Source: MD5.Disgest_To_Text
  expr_st    : storage;
  expr_type  : identifier;
  C          : Context;
  FP         : Fingerprint;
  subprogramId : constant identifier := numerics_md5_t;
begin
  kind := string_t;
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, expr_st, expr_type, string_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           Init( C );
          Update( C, to_string( expr_st.value ) );
          Final( C, FP );
          result := storage'( to_unbounded_string( Digest_To_Text( FP ) ),
             noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsMd5;

procedure ParseNumericsSerial( result : out storage; kind : out identifier ) is
  -- Syntax: serial
  -- Source: N/A
  subprogramId : constant identifier := serial_t;
begin
  kind := natural_t;
  result := nullStorage;
  expect( subprogramId );
  if isExecutingCommand then
     result.value := to_unbounded_string( serialNumber );
     if serialNumber = maxInteger then
        serialNumber := 0.0;
     else
        serialNumber := serialNumber + 1.0;
     end if;
  end if;
end ParseNumericsSerial;

procedure ParseNumericsRnd( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.rnd( n );
  -- Source: N/A
  expr_st     : storage;
  expr_type   : identifier;
  subprogramId : constant identifier := rnd_t;
begin
  kind := positive_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type, positive_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           -- from pegasoft.numerics
           result := storage'( to_unbounded_string( numericValue( rnd( positive( to_numeric( expr_st.value ) ) ) ) ),
              noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsRnd;

procedure ParseNumericsOdd( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.odd( n );
  -- Source: N/A
  expr_st    : storage;
  expr_type  : identifier;
  subprogramId : constant identifier := odd_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type, integer_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'( to_spar_boolean( integer( to_numeric( expr_st.value ) ) mod 2 = 1 ),
              noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsOdd;

procedure ParseNumericsEven( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.even( n );
  -- Source: N/A
  expr_st    : storage;
  expr_type  : identifier;
  subprogramId : constant identifier := even_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr_st, expr_type, integer_t );
  begin
     if isExecutingCommand then
        if metaLabelOk( subprogramId, expr_st ) then
           result := storage'( to_spar_boolean( integer( to_numeric( expr_st.value ) ) mod 2 = 0 ),
              noMetaLabel, expr_st.policyMetaLabels );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsEven;

procedure ParseNumericsRe( result : out storage; kind : out identifier ) is
  -- Syntax: r := numerics.re( n );
  -- Source: N/A
  record_id : identifier;
  c : complex;
  real_t      : identifier;
  img_t       : identifier;
  subprogramId : constant identifier := numerics_re_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, complex_t ) then
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
       findField( record_id, 1, real_t );
       findField( record_id, 2, img_t );
       if metaLabelOk( subprogramId, identifiers( real_t ).store.all ) then
          c.re := to_numeric( identifiers( real_t ).store.value );
          c.im := to_numeric( identifiers( img_t ).store.value );
          result := storage'( to_unbounded_string( numericValue( Re( c ) ) ),
            noMetaLabel, identifiers( real_t ).store.policyMetaLabels );
       end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsRe;

procedure ParseNumericsIm( result : out storage; kind : out identifier ) is
  -- Syntax: r := numerics.im( n );
  -- Source: N/A
  record_id : identifier;
  c : complex;
  real_t      : identifier;
  img_t       : identifier;
  subprogramId : constant identifier := numerics_im_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, complex_t ) then
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
       findField( record_id, 1, real_t );
       findField( record_id, 2, img_t );
       if metaLabelOk( subprogramId, identifiers( img_t ).store.all ) then
          c.re := to_numeric( identifiers( real_t ).store.value );
          c.im := to_numeric( identifiers( img_t ).store.value );
          result := storage'( to_unbounded_string( numericValue( Im( c ) ) ),
             noMetaLabel, identifiers( img_t ).store.policyMetaLabels );
       end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsIm;

procedure ParseNumericsSetRe is
  -- Syntax: numerics.set_re( n );
  -- Source: N/A
  expr_st : storage;
  expr_type : identifier;
  record_id : identifier;
  c : complex;
  real_t      : identifier;
  img_t       : identifier;
  subprogramId : constant identifier := numerics_set_re_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, complex_t ) then
     expectParameterComma;
     ParseExpression( expr_st, expr_type );
     if baseTypesOk( expr_type, long_float_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  begin
     if isExecutingCommand then
       findField( record_id, 1, real_t );
       findField( record_id, 2, img_t );
       if metaLabelOk( subprogramId, identifiers( real_t ).store.all ) and
          metaLabelOk( subprogramId, expr_st ) then
          c.re := to_numeric( identifiers( real_t ).store.value );
          c.im := to_numeric( identifiers( img_t ).store.value );
          Set_Re( c, to_numeric( expr_st.value ) );
          identifiers( real_t ).store.value := to_unbounded_string( numericValue( c.re ) );
          identifiers( real_t ).store.unitMetaLabel := expr_st.unitMetaLabel;
          identifiers( real_t ).store.policyMetaLabels := expr_st.policyMetaLabels;
       end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsSetRe;

procedure ParseNumericsSetIm is
  -- Syntax: numerics.set_im( n );
  -- Source: N/A
  expr_st     : storage;
  expr_type   : identifier;
  record_id   : identifier;
  real_t      : identifier;
  img_t       : identifier;
  c : complex;
  subprogramId : constant identifier := numerics_set_im_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, complex_t ) then
     expectParameterComma;
     ParseExpression( expr_st, expr_type );
     if baseTypesOk( expr_type, long_float_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  begin
     if isExecutingCommand then
       findField( record_id, 1, real_t );
       findField( record_id, 2, img_t );
       if metaLabelOk( subprogramId, identifiers( img_t ).store.all ) and
          metaLabelOk( subprogramId, expr_st ) then
          c.re := to_numeric( identifiers( real_t ).store.value );
          c.im := to_numeric( identifiers( img_t ).store.value );
          Set_Im( c, to_numeric( expr_st.value ) );
          identifiers( img_t ).store.value := to_unbounded_string( numericValue( c.Im ) );
          identifiers( img_t ).store.unitMetaLabel := expr_st.unitMetaLabel;
          identifiers( img_t ).store.policyMetaLabels := expr_st.policyMetaLabels;
       end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsSetIm;

-- Can't return a new complex expression
--
--procedure ParseNumericsSetComposeFromCartisian( result : out storage ) is
  -- Syntax: r := numerics.compose_from_cartisian( r [, i] );
  -- Source: N/A
  --expr_st   : unbounded_string;
  --expr_type  : identifier;
--begin
  --expect( _t );
  --expect( symbol_t, "(" );
  --ParseExpression( expr_st, expr_type );
  --if baseTypesOk( expr_type, complex_t ) then
     --expect( symbol_t, ")" );
  --end if;
  --begin
     --if isExecutingCommand then
       --result := to_numeric( expr_st.value );
     --end if;
  --exception when others =>
     --err_exception_raised;
  --end;
--end ParseNumericsComposeFromCartisian;

procedure ParseNumericsModulus( result : out storage; kind : out identifier ) is
  -- Syntax: r := numerics.modulus( n );
  -- Source: N/A
  record_id   : identifier;
  c : complex;
  real_t      : identifier;
  img_t       : identifier;
  subprogramId : constant identifier := numerics_modulus_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, complex_t ) then
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
       findField( record_id, 1, real_t );
       findField( record_id, 2, img_t );
       if metaLabelOk( subprogramId, identifiers( real_t ).store.all, identifiers( img_t ).store.all ) then
          c.re := to_numeric( identifiers( real_t ).store.value );
          c.im := to_numeric( identifiers( img_t ).store.value );
          result := storage'( to_unbounded_string( numericValue( Modulus( c ) ) ),
             noMetaLabel,
             resolveEffectiveMetaLabels(
                long_float_t,
                identifiers( real_t ).store.all,
                identifiers( img_t ).store.all
             )
         );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsModulus;

-- abs is a renaming of modulus

procedure ParseNumericsArgument( result : out storage; kind : out identifier ) is
  -- Syntax: r := numerics.argument( n [,c] );
  -- Source: N/A
  record_id   : identifier;
  cycle_st    : storage;
  cycle_type  : identifier;
  has_cycle   : boolean := false;
  c : complex;
  real_t      : identifier;
  img_t       : identifier;
  subprogramId : constant identifier := numerics_argument_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, complex_t ) then
     if token = symbol_t and identifiers( token ).store.value = "," then
        getNextToken;
        ParseExpression( cycle_st, cycle_type );
        has_cycle := true;
     end if;
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
       findField( record_id, 1, real_t );
       findField( record_id, 2, img_t );
       c.re := to_numeric( identifiers( real_t ).store.value );
       c.im := to_numeric( identifiers( img_t ).store.value );
       if metaLabelOk( subprogramId, identifiers( real_t ).store.all, identifiers( img_t ).store.all ) then
          if has_cycle then
             result := storage'( to_unbounded_string( numericValue( Argument( c, to_numeric( cycle_st.value ) ) ) ),
                noMetaLabel,
                resolveEffectiveMetaLabels(
                   long_float_t,
                   identifiers( real_t ).store.all,
                   identifiers( img_t ).store.all
                )
             );
          else
             result := storage'( to_unbounded_string( numericValue( Argument( c ) ) ),
                noMetaLabel,
                resolveEffectiveMetaLabels(
                   long_float_t,
                   identifiers( real_t ).store.all,
                   identifiers( img_t ).store.all
                )
             );
          end if;
       end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseNumericsArgument;

-- Can't return record expressions
--procedure ParseNumericsComposeFromPolar( result : out storage ) is
  -- Syntax: r := numerics.compose_from_polar( n ,a );
  -- Source: N/A
  --expr_st   : unbounded_string;
  --expr_type  : identifier;
--begin
  --expect( _t );
  --expect( symbol_t, "(" );
  --ParseExpression( expr_st, expr_type );
  --if baseTypesOk( expr_type, complex_t ) then
     --expect( symbol_t, ")" );
  --end if;
  --begin
     --if isExecutingCommand then
--       result := to_numeric( expr_st.value );
 --    end if;
  --exception when others =>
   --  err_exception_raised;
  --end;
--end ParseNumericsComposeFromPolar;

-- Can't return a record expression
--procedure ParseNumericsConjugate( result : out storage ) is
  -- Syntax: r := numerics.conjugate( n );
  -- Source: N/A
  --expr_st   : unbounded_string;
  --expr_type  : identifier;
--begin
  --expect( _t );
  --expect( symbol_t, "(" );
  --ParseExpression( expr_st, expr_type );
  --if baseTypesOk( expr_type, complex_t ) then
     --expect( symbol_t, ")" );
  --end if;
  --begin
     --if isExecutingCommand then
       --result := to_numeric( expr_st.value );
     --end if;
  --exception when others =>
     --err_exception_raised;
  --end;
--end ParseNumericsConjugate;

procedure ParseNumericsHashOf( result : out storage; kind : out identifier) is
  -- Syntax: numerics.hash_of( s, l )
  expr1_st    : storage;
  expr1_type  : identifier;
  expr2_st    : storage;
  expr2_type  : identifier;
  subprogramId : constant identifier := hash_of_t;
begin
  kind := natural_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr1_st, expr1_type, string_t );
  ParseLastNumericParameter( subprogramId, expr2_st, expr2_type, natural_t );
  declare
    limit : hash_integer;
  begin
    if isExecutingCommand then
       if metaLabelOK( subprogramId, expr1_st, expr2_st ) then
          limit := hash_integer( to_numeric( expr2_st.value ) );
          result := storage'( to_unbounded_string( numericValue( hash_of( expr1_st.value, limit ) ) ),
                noMetaLabel,
                resolveEffectiveMetaLabels( kind, expr1_st, expr2_st ) );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsHashOf;

procedure ParseNumericsSdbmHashOf( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.sdbm_hash_of( s, l )
  expr1_st    : storage;
  expr1_type  : identifier;
  expr2_st    : storage;
  expr2_type  : identifier;
  subprogramId : constant identifier := sdbm_hash_of_t;
begin
  kind := natural_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr1_st, expr1_type, string_t );
  ParseLastNumericParameter( subprogramId, expr2_st, expr2_type, natural_t );
  declare
    limit : hash_integer;
  begin
    if isExecutingCommand then
       if metaLabelOK( subprogramId, expr1_st, expr2_st ) then
          limit := hash_integer( to_numeric( expr2_st.value ) );
          result := storage'( to_unbounded_string( numericValue( sdbm_hash_of( expr1_st.value, limit ) ) ),
                noMetaLabel,
                resolveEffectiveMetaLabels( kind, expr1_st, expr2_st ) );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsSdbmHashOf;

procedure ParseNumericsFnvHashOf( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.fnv_hash_of( s, l )
  expr1_st    : storage;
  expr1_type  : identifier;
  expr2_st    : storage;
  expr2_type  : identifier;
  subprogramId : constant identifier := fnv_hash_of_t;
begin
  kind := natural_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr1_st, expr1_type, string_t );
  ParseLastNumericParameter( subprogramId, expr2_st, expr2_type, natural_t );
  declare
    limit  : hash_integer;
  begin
    if isExecutingCommand then
       if metaLabelOK( subprogramId, expr1_st, expr2_st ) then
          limit := hash_integer( to_numeric( expr2_st.value ) );
          result := storage'( to_unbounded_string( numericValue( fnv_hash_of( expr1_st.value, limit ) ) ),
             noMetaLabel,
             resolveEffectiveMetaLabels( kind, expr1_st, expr2_st ) );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsFnvHashOf;

procedure ParseNumericsMurmurHashOf( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.murmur_hash_of( s, l )
  expr1_st    : storage;
  expr1_type  : identifier;
  expr2_st    : storage;
  expr2_type  : identifier;
  subprogramId : constant identifier := murmur_hash_of_t;
begin
  kind := natural_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, expr1_st, expr1_type, string_t );
  ParseLastNumericParameter( subprogramId, expr2_st, expr2_type, natural_t );
  declare
    limit  : hash_integer;
  begin
    if isExecutingCommand then
       if metaLabelOK( subprogramId, expr1_st, expr2_st ) then
       limit := hash_integer( to_numeric( expr2_st.value ) );
       result := storage'( to_unbounded_string( numericValue( murmur_hash_of( expr1_st.value, limit ) ) ),
          noMetaLabel,
          resolveEffectiveMetaLabels( kind, expr1_st, expr2_st ) );
    end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseNumericsMurmurHashOf;

procedure ParseNumericsSHA1DigestOf( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.sha1_digest_of( s );
  -- Source: GNAT.SHA1.Digest
  src_st   : storage;
  src_type : identifier;
  subprogramId : constant identifier := sha1_digest_of_t;
begin
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, src_st, src_type );
  kind := src_type;
  if isExecutingCommand then
     if metaLabelOK( subprogramId, src_st ) then
        result := storage'( to_unbounded_string( Gnat.SHA1.Digest( to_string( src_st.value ) ) ),
           noMetaLabel,
           src_st.policyMetaLabels );
     end if;
  end if;
end ParseNumericsSHA1DigestOf;

procedure ParseNumericsSHA224DigestOf( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.sha224_digest_of( s );
  -- Source: GNAT.SHA224.Digest
  src_st   : storage;
  src_type : identifier;
  subprogramId : constant identifier := sha224_digest_of_t;
begin
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, src_st, src_type );
  kind := src_type;
  if isExecutingCommand then
     if metaLabelOK( subprogramId, src_st ) then
        result := storage'( to_unbounded_string( Gnat.SHA224.Digest( to_string( src_st.value ) ) ),
           noMetaLabel,
           src_st.policyMetaLabels );
     end if;
  end if;
end ParseNumericsSHA224DigestOf;

procedure ParseNumericsSHA256DigestOf( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.sha256_digest_of( s );
  -- Source: GNAT.SHA256.Digest
  src_st   : storage;
  src_type : identifier;
  subprogramId : constant identifier := sha256_digest_of_t;
begin
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, src_st, src_type );
  kind := src_type;
  if isExecutingCommand then
     if metaLabelOK( subprogramId, src_st ) then
        result := storage'( to_unbounded_string( Gnat.SHA256.Digest( to_string( src_st.value ) ) ),
           noMetaLabel,
           src_st.policyMetaLabels );
     end if;
  end if;
end ParseNumericsSHA256DigestOf;

procedure ParseNumericsSHA512DigestOf( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.sha512_digest_of( s );
  -- Source: GNAT.SHA512.Digest
  src_st   : storage;
  src_type : identifier;
  subprogramId : constant identifier := sha512_digest_of_t;
begin
  expect( subprogramId );
  ParseSingleStringParameter( sha512_digest_of_t, src_st, src_type );
  kind := src_type;
  if isExecutingCommand then
     if metaLabelOK( sha512_digest_of_t, src_st ) then
        result := storage'( to_unbounded_string( Gnat.SHA512.Digest( to_string( src_st.value ) ) ),
           noMetaLabel,
           src_st.policyMetaLabels );
     end if;
  end if;
end ParseNumericsSHA512DigestOf;

procedure ParseNumericsShannonEntropyOf( result : out storage; kind : out identifier ) is
  -- Syntax: numerics.shannon_entropy_of( s );
  -- Source: N/A
  src_st   : storage;
  src_type : identifier;
  subprogramId : constant identifier := shannon_entropy_of_t;
begin
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, src_st, src_type );
  kind := long_float_t;
  if isExecutingCommand then
     if metaLabelOK( subprogramId, src_st ) then
        result := storage'( to_unbounded_string( shannon_entropy_of( src_st.value ) ),
           noMetaLabel,
           src_st.policyMetaLabels );
     end if;
  end if;
end ParseNumericsShannonEntropyOf;

procedure StartupNumerics is
begin

  declareNamespace( "numerics" );

  -- Numerics Package identifiers

  declareFunction( random_t, "numerics.random", ParseNumericsRandom'access );
  declareFunction( shift_left_t, "numerics.shift_left", ParseNumericsShiftLeft'access );
  declareFunction( shift_right_t, "numerics.shift_right", ParseNumericsShiftRight'access );
  declareFunction( rotate_left_t, "numerics.rotate_left", ParseNumericsRotateLeft'access );
  declareFunction( rotate_right_t, "numerics.rotate_right", ParseNumericsRotateRight'access );
  declareFunction( shift_right_arith_t, "numerics.shift_right_arithmetic", ParseNumericsASR'access );
  declareFunction( sqrt_t, "numerics.sqrt", ParseNumericsSqrt'access );
  declareFunction( log_t, "numerics.log", ParseNumericsLog'access );
  declareFunction( exp_t, "numerics.exp", ParseNumericsExp'access );
  declareFunction( sin_t, "numerics.sin", ParseNumericsSin'access );
  declareFunction( cos_t, "numerics.cos", ParseNumericsCos'access );
  declareFunction( tan_t, "numerics.tan", ParseNumericsTan'access );
  declareFunction( cot_t, "numerics.cot", ParseNumericsCot'access );
  declareFunction( arcsin_t, "numerics.arcsin", ParseNumericsArcSin'access );
  declareFunction( arccos_t, "numerics.arccos", ParseNumericsArcCos'access );
  declareFunction( arctan_t, "numerics.arctan", ParseNumericsArcTan'access );
  declareFunction( arccot_t, "numerics.arccot", ParseNumericsArcCot'access );
  declareFunction( sinh_t, "numerics.sinh", ParseNumericsSinH'access );
  declareFunction( cosh_t, "numerics.cosh", ParseNumericsCosH'access );
  declareFunction( tanh_t, "numerics.tanh", ParseNumericsTanH'access );
  declareFunction( coth_t, "numerics.coth", ParseNumericsCotH'access );
  declareFunction( arcsinh_t, "numerics.arcsinh", ParseNumericsArcSinH'access );
  declareFunction( arccosh_t, "numerics.arccosh", ParseNumericsArcCosH'access );
  declareFunction( arctanh_t, "numerics.arctanh", ParseNumericsArcTanH'access );
  declareFunction( arccoth_t, "numerics.arccoth", ParseNumericsArcCotH'access );
  declareFunction( floor_t, "numerics.floor", ParseNumericsFloor'access );
  declareFunction( ceiling_t, "numerics.ceiling", ParseNumericsCeiling'access );
  declareFunction( rounding_t, "numerics.rounding", ParseNumericsRounding'access );
  declareFunction( unbiased_rounding_t, "numerics.unbiased_rounding", ParseNumericsUnbiasedRounding'access );
  declareFunction( truncation_t, "numerics.truncation", ParseNumericsTruncation'access );
  declareFunction( remainder_t, "numerics.remainder", ParseNumericsRemainder'access );
  declareFunction( exponent_t, "numerics.exponent", ParseNumericsExponent'access );
  declareFunction( fraction_t, "numerics.fraction", ParseNumericsFraction'access );
  declareFunction( leading_part_t, "numerics.leading_part", ParseNumericsLeadingPart'access );
  declareFunction( copy_sign_t, "numerics.copy_sign", ParseNumericsCopySign'access );
  declareFunction( sturges_t, "numerics.sturges", ParseNumericsSturges'access );
  declareFunction( max_t, "numerics.max", ParseNumericsMax'access );
  declareFunction( min_t, "numerics.min", ParseNumericsMin'access );
  declareFunction( machine_t, "numerics.machine", ParseNumericsMachine'access );
  declareFunction( scaling_t, "numerics.scaling", ParseNumericsScaling'access );
  declareFunction( value_t, "numerics.value", ParseNumericsValue'access );
  declareFunction( pos_t, "numerics.pos", ParseNumericsPos'access );
  declareFunction( numerics_md5_t, "numerics.md5", ParseNumericsMd5'access );
  declareFunction( serial_t, "numerics.serial", ParseNumericsSerial'access );
  declareFunction( rnd_t, "numerics.rnd", ParseNumericsRnd'access );
  declareFunction( odd_t, "numerics.odd", ParseNumericsOdd'access );
  declareFunction( even_t, "numerics.even", ParseNumericsEven'access );
  declareFunction( numerics_re_t, "numerics.re", ParseNumericsRe'access );
  declareFunction( numerics_im_t, "numerics.im", ParseNumericsIm'access );
  declareProcedure( numerics_set_re_t, "numerics.set_re", ParseNumericsSetRe'access );
  declareProcedure( numerics_set_im_t, "numerics.set_im", ParseNumericsSetIm'access );
  declareFunction( numerics_argument_t, "numerics.argument", ParseNumericsArgument'access );
  declareFunction( numerics_modulus_t, "numerics.modulus", ParseNumericsModulus'access );
  declareFunction( hash_of_t, "numerics.hash_of", ParseNumericsHashOf'access );
  declareFunction( sdbm_hash_of_t, "numerics.sdbm_hash_of", ParseNumericsSdbmHashOf'access );
  declareFunction( fnv_hash_of_t, "numerics.fnv_hash_of", ParseNumericsFnvHashOf'access );
  declareFunction( murmur_hash_of_t, "numerics.murmur_hash_of", ParseNumericsMurmurHashOf'access );
  declareFunction( sha1_digest_of_t,  "numerics.sha1_digest_of", ParseNumericsSHA1DigestOf'access );
  declareFunction( sha224_digest_of_t,  "numerics.sha224_digest_of", ParseNumericsSHA224DigestOf'access );
  declareFunction( sha256_digest_of_t,  "numerics.sha256_digest_of", ParseNumericsSHA256DigestOf'access );
  declareFunction( sha512_digest_of_t,  "numerics.sha512_digest_of", ParseNumericsSHA512DigestOf'access );
  declareFunction( shannon_entropy_of_t,  "numerics.shannon_entropy_of", ParseNumericsShannonEntropyOf'access );

  -- Numerics package constants
  -- There's are derived from the GNU standard C library math constants

  declareStandardConstant( numerics_e_t, "numerics.e", long_float_t,
      "2.7182818284590452354" );
  declareStandardConstant( numerics_log2_e_t, "numerics.log2_e", long_float_t,
      "1.4426950408889634074" );
  declareStandardConstant( numerics_log10_e_t, "numerics.log10_e", long_float_t,
      "0.43429448190325182765" );
  declareStandardConstant( numerics_ln10_t, "numerics.ln2", long_float_t,
      "0.69314718055994530942" );
  declareStandardConstant( numerics_ln2_t, "numerics.ln10", long_float_t,
      "2.30258509299404568402" );
  declareStandardConstant( numerics_pi_t, "numerics.pi", long_float_t,
      "3.14159265358979323846" );
  declareStandardConstant( numerics_pi_by_2_t, "numerics.pi_by_2", long_float_t,
      "1.57079632679489661923" );
  declareStandardConstant( numerics_pi_by_4_t, "numerics.pi_by_4", long_float_t,
      "0.78539816339744830962" );
  declareStandardConstant( numerics_pi_under_1_t, "numerics.pi_under_1", long_float_t,
      "0.31830988618379067154" );
  declareStandardConstant( numerics_pi_under_2_t, "numerics.pi_under_2", long_float_t,
      "0.63661977236758134308" );
  declareStandardConstant( numerics_sqrt_pi_under_2_t, "numerics.sqrt_pi_under_2",
      long_float_t, "1.12837916709551257390" );
  declareStandardConstant( numerics_sqrt_2_t, "numerics.sqrt_2", long_float_t,
      "1.4142135623730950488" );
  declareStandardConstant( numerics_sqrt_2_under_1_t, "numerics.sqrt_2_under_1", long_float_t,
      "0.70710678118654752440" );

  declareNamespaceClosed( "numerics" );
end StartupNumerics;

procedure ShutdownNumerics is
begin
  null;
end ShutdownNumerics;

end parser_numerics;
