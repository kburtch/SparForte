------------------------------------------------------------------------------
-- Units Package Parser                                                     --
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
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

--with ada.text_io;use ada.text_io;

with ada.strings.unbounded,
    pegasoft,
    world,
    symbol_table,
    message_strings,
    scanner.communications,
    parser_params;
use ada.strings.unbounded,
    pegasoft,
    world,
    symbol_table,
    message_strings,
    scanner,
    scanner.communications,
    parser_params;

package body parser_units is

------------------------------------------------------------------------------
-- Units package identifiers
------------------------------------------------------------------------------

units_inches2mm_t  : identifier;
units_feet2cm_t    : identifier;
units_yards2m_t    : identifier;
units_miles2km_t   : identifier;
units_mm2inches_t  : identifier;
units_cm2inches_t  : identifier;
units_m2yards_t    : identifier;
units_km2miles_t   : identifier;
units_sqin2sqcm_t  : identifier;
units_sqft2sqm_t   : identifier;
units_sqyd2sqm_t   : identifier;
units_acres2hectares_t : identifier;
units_sqcm2sqin_t  : identifier;
units_sqm2sqft_t   : identifier;
units_sqm2sqyd_t   : identifier;
units_sqkm2sqmiles_t : identifier;
units_hectares2acres_t : identifier;
units_oz2grams_t   : identifier;
units_lb2kg_t      : identifier;
units_tons2tonnes_t : identifier;
units_grams2oz_t   : identifier;
units_kg2lb_t      : identifier;
units_tonnes2tons_t : identifier;
units_ly2pc_t      : identifier;
units_pc2ly_t      : identifier;
units_floz2ml_t    : identifier;
units_pints2l_t    : identifier;
units_gal2l_t      : identifier;
units_ml2floz_t    : identifier;
units_l2quarts_t   : identifier;
units_l2gal_t      : identifier;
units_f2c_t        : identifier;
units_c2f_t        : identifier;
units_k2c_t        : identifier;
units_c2k_t        : identifier;
units_usfloz2ml_t  : identifier;
units_usfloz2floz_t: identifier;
units_ml2usfloz_t  : identifier;
units_floz2usfloz_t: identifier;
units_usdrygal2l_t : identifier;
units_l2usdrygal_t : identifier;
units_usliqgal2l_t : identifier;
units_l2usliqgal_t : identifier;
units_troz2g_t     : identifier;
units_g2troz_t     : identifier;
units_cucm2floz_t  : identifier;
units_floz2cucm_t  : identifier;
units_cucm2usfloz_t : identifier;
units_usfloz2cucm_t : identifier;
units_bytes2MB_t   : identifier;
units_MB2bytes_t   : identifier;


-----------------------------------------------------------------------------
-- Utilities
-----------------------------------------------------------------------------


procedure ParseSimpleConversion( subprogramId : identifier; result : out storage;
  expr : in storage;
  factor   : in numericValue ) is
-- Many conversions are simple multiplication.  Using this procedure
-- cuts the (binary) size of this package in half.
begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr ) then
          result := storage'( to_unbounded_string( to_numeric( expr.value ) * factor ), expr.metaLabel );
       end if;
    end if;
exception when others =>
    err_exception_raised;
end ParseSimpleConversion;


-----------------------------------------------------------------------------
-- Package Subprograms
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  PARSE UNITS INCHES 2 MM                               (built-in function)
--
-- AdaScript Syntax: units.inches2mm( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.inches2mm
--       Conversion: inches * 25.4
-----------------------------------------------------------------------------

procedure ParseUnitsInches2mm( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_inches2mm_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( units_inches2mm_t, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 25.4 );
end ParseUnitsInches2mm;


-----------------------------------------------------------------------------
--  PARSE UNITS FEET 2 CM                                 (built-in function)
--
-- AdaScript Syntax: units.feet2cm( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.feet2cm
--       Conversion: feet * 30.48
-----------------------------------------------------------------------------

procedure ParseUnitsFeet2cm( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_feet2cm_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 30.48 );
end ParseUnitsFeet2cm;


-----------------------------------------------------------------------------
--  PARSE UNITS YARDS 2 M                                 (built-in function)
--
-- AdaScript Syntax: units.yards2m( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.yards2m
--       Conversion: yards * 0.9144
-----------------------------------------------------------------------------

procedure ParseUnitsYards2m( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_yards2m_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.9144 );
end ParseUnitsYards2m;


-----------------------------------------------------------------------------
--  PARSE UNITS MILES 2 KM                                (built-in function)
--
-- AdaScript Syntax: units.miles2km( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.miles2km
--       Conversion: miles * 1.60934
-----------------------------------------------------------------------------

procedure ParseUnitsMiles2km( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_miles2km_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 1.60934 );
end ParseUnitsMiles2km;


-----------------------------------------------------------------------------
--  PARSE UNITS MM 2 INCHES                               (built-in function)
--
-- AdaScript Syntax: units.mm2inches( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.mm2inches
--       Conversion: mm * 0.03937
-----------------------------------------------------------------------------

procedure ParseUnitsMM2Inches( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_mm2inches_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.03937 );
end ParseUnitsMM2Inches;


-----------------------------------------------------------------------------
--  PARSE UNITS CM 2 INCHES                               (built-in function)
--
-- AdaScript Syntax: units.cm2inches( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.cm2inches
--       Conversion: cm * 0.3937
-----------------------------------------------------------------------------

procedure ParseUnitsCm2Inches( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_cm2inches_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.3937 );
end ParseUnitsCm2Inches;


-----------------------------------------------------------------------------
--  PARSE UNITS M 2 YARDS                                 (built-in function)
--
-- AdaScript Syntax: units.m2yards( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.m2yards
--       Conversion: m * 1.0936
-----------------------------------------------------------------------------

procedure ParseUnitsM2Yards( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_m2yards_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 1.0936 );
end ParseUnitsM2Yards;


-----------------------------------------------------------------------------
--  PARSE UNITS KM 2 MILES                                (built-in function)
--
-- AdaScript Syntax: units.m2yards( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.km2miles
--       Conversion: km * 0.62137
-----------------------------------------------------------------------------

procedure ParseUnitsKm2Miles( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_km2miles_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.62137 );
end ParseUnitsKm2Miles;


-----------------------------------------------------------------------------
--  PARSE UNITS SQ IN 2 SQ CM                             (built-in function)
--
-- AdaScript Syntax: units.sqin2sqcm( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.sqin2sqcm
--       Conversion: sq in * 6.4516
-----------------------------------------------------------------------------

procedure ParseUnitsSqIn2SqCm( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_sqin2sqcm_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 6.4516 );
end ParseUnitsSqIn2SqCm;


-----------------------------------------------------------------------------
--  PARSE UNITS SQ FT 2 SQ M                              (built-in function)
--
-- AdaScript Syntax: units.sqft2sqm( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.sqft2sqm
--       Conversion: sq ft * 0.092903
-----------------------------------------------------------------------------

procedure ParseUnitsSqFt2SqM( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_sqft2sqm_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.092903 );
end ParseUnitsSqFt2SqM;


-----------------------------------------------------------------------------
--  PARSE UNITS SQ YD 2 SQ M                              (built-in function)
--
-- AdaScript Syntax: units.sqyd2sqm( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.sqyd2sqm
--       Conversion: sq yd * 0.836127
-----------------------------------------------------------------------------

procedure ParseUnitsSqYd2SqM( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_sqyd2sqm_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.836127 );
end ParseUnitsSqYd2SqM;


-----------------------------------------------------------------------------
--  PARSE UNITS ACRES 2 HECTARES                          (built-in function)
--
-- AdaScript Syntax: units.acres2hectares( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.acres2hectares
--       Conversion: acres * 0.40486
-----------------------------------------------------------------------------

procedure ParseUnitsAcres2Hectares( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_acres2hectares_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.40486 );
end ParseUnitsAcres2Hectares;


-----------------------------------------------------------------------------
--  PARSE UNITS SQ CM 2 SQ IN                             (built-in function)
--
-- AdaScript Syntax: units.sqcm2sqin( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.sqcm2sqin
--       Conversion: sq cm * 0.155
-----------------------------------------------------------------------------

procedure ParseUnitsSqCm2SqIn( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_sqcm2sqin_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.155 );
end ParseUnitsSqCm2SqIn;


-----------------------------------------------------------------------------
--  PARSE UNITS SQ M 2 SQ FT                              (built-in function)
--
-- AdaScript Syntax: units.sqm2sqft( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.sqcm2sqin
--       Conversion: sq m * 10.7639
-----------------------------------------------------------------------------

procedure ParseUnitsSqM2SqFt( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_sqm2sqft_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 10.7639 );
end ParseUnitsSqM2SqFt;


-----------------------------------------------------------------------------
--  PARSE UNITS SQ M 2 SQ YD                              (built-in function)
--
-- AdaScript Syntax: units.sqm2sqyd( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.sqm2sqyd
--       Conversion: sq m * 1.19599
-----------------------------------------------------------------------------

procedure ParseUnitsSqM2SqYd( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_sqm2sqyd_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 1.19599 );
end ParseUnitsSqM2SqYd;


-----------------------------------------------------------------------------
--  PARSE UNITS SQ KM 2 SQ MILES                          (built-in function)
--
-- AdaScript Syntax: units.sqkm2sqmiles( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.sqkm2sqmiles
--       Conversion: sq km * 0.38611
-----------------------------------------------------------------------------

procedure ParseUnitsSqKm2SqMiles( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_sqkm2sqmiles_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.38611 );
end ParseUnitsSqKm2SqMiles;


-----------------------------------------------------------------------------
--  PARSE UNITS HECTARES 2 ACRES                          (built-in function)
--
-- AdaScript Syntax: units.hectares2acres( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.hectares2acres
--       Conversion: hectares * 2.471
-----------------------------------------------------------------------------

procedure ParseUnitsHectares2Acres( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_hectares2acres_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 2.471 );
end ParseUnitsHectares2Acres;


-----------------------------------------------------------------------------
--  PARSE UNITS LY 2 PC                                   (built-in function)
--
-- AdaScript Syntax: units.ly2pc( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.ly2pc
--       Conversion: ly * 0.3066
-----------------------------------------------------------------------------

procedure ParseUnitsLy2Pc( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_ly2pc_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.3066 );
end ParseUnitsLy2Pc;


-----------------------------------------------------------------------------
--  PARSE UNITS PC 2 LY                                   (built-in function)
--
-- AdaScript Syntax: units.pc2ly( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.pc2ly
--       Conversion: pc * 3.2616
-----------------------------------------------------------------------------

procedure ParseUnitsPc2Ly( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_pc2ly_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 3.2616 );
end ParseUnitsPc2Ly;


-----------------------------------------------------------------------------
--  PARSE UNITS OZ 2 GRAMS                                (built-in function)
--
-- AdaScript Syntax: units.oz2grams( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.oz2grams
--       Conversion: oz * 28.349
-----------------------------------------------------------------------------

procedure ParseUnitsOz2Grams( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_oz2grams_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 28.349 );
end ParseUnitsOz2Grams;


-----------------------------------------------------------------------------
--  PARSE UNITS LB 2 KG                                   (built-in function)
--
-- AdaScript Syntax: units.lb2kg( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.lb2kg
--       Conversion: lb * 0.45359
-----------------------------------------------------------------------------

procedure ParseUnitsLb2Kg( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_lb2kg_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.45359 );
end ParseUnitsLb2Kg;


-----------------------------------------------------------------------------
--  PARSE UNITS TONS 2 TONNES                             (built-in function)
--
-- AdaScript Syntax: units.tons2tonnes( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.tons2tonnes
--       Conversion: tons * 1.01605
-----------------------------------------------------------------------------

procedure ParseUnitsTons2Tonnes( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_tons2tonnes_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 1.01605 );
end ParseUnitsTons2Tonnes;


-----------------------------------------------------------------------------
--  PARSE UNITS TONNES 2 TONS                             (built-in function)
--
-- AdaScript Syntax: units.tonnes2tons( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.tonnes2tons
--       Conversion: tonnes * 0.3527
-----------------------------------------------------------------------------

procedure ParseUnitsGrams2Oz( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_grams2oz_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.3527 );
end ParseUnitsGrams2Oz;


-----------------------------------------------------------------------------
--  PARSE UNITS KG 2 LB                                   (built-in function)
--
-- AdaScript Syntax: units.kg2lb( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.kg2lb
--       Conversion: kg * 2.2046
-----------------------------------------------------------------------------

procedure ParseUnitsKg2Lb( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_kg2lb_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 2.2046 );
end ParseUnitsKg2Lb;


-----------------------------------------------------------------------------
--  PARSE UNITS TONNES 2 TONS                             (built-in function)
--
-- AdaScript Syntax: units.tonnes2tons( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.tonnes2tons
--       Conversion: tonnes * 0.9842
-----------------------------------------------------------------------------

procedure ParseUnitsTonnes2Tons( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_tonnes2tons_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.9842 );
end ParseUnitsTonnes2Tons;


-----------------------------------------------------------------------------
--  PARSE UNITS FL OZ 2 ML                                (built-in function)
--
-- AdaScript Syntax: units.floz2mls( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.floz2ml
--       Conversion: fl oz * 28.4131
-----------------------------------------------------------------------------

procedure ParseUnitsFlOz2Ml( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_floz2ml_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 28.4131 );
end ParseUnitsFlOz2Ml;


-----------------------------------------------------------------------------
--  PARSE UNITS US FL OZ 2 ML                             (built-in function)
--
-- AdaScript Syntax: units.floz2mls( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.floz2mls
--       Conversion: U.S. fl oz * 29.57
-----------------------------------------------------------------------------

procedure ParseUnitsUSFlOz2Ml( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_usfloz2ml_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 29.57 );
end ParseUnitsUSFlOz2Ml;


-----------------------------------------------------------------------------
--  PARSE UNITS US FL OZ 2 FL OZ                          (built-in function)
--
-- AdaScript Syntax: units.usfloz2floz( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.usfloz2floz
--       Conversion: U.S. fl oz * 1.041
-----------------------------------------------------------------------------

procedure ParseUnitsUSFlOz2FlOz( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_usfloz2floz_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 1.041 );
end ParseUnitsUSFlOz2FlOz;


-----------------------------------------------------------------------------
--  PARSE UNITS PINTS 2 L                                 (built-in function)
--
-- AdaScript Syntax: units.pints2l( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.pints2l
--       Conversion: pints * 0.568
-----------------------------------------------------------------------------

procedure ParseUnitsPints2L( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_pints2l_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.568 );
end ParseUnitsPints2L;


-----------------------------------------------------------------------------
--  PARSE UNITS GAL 2 L                                   (built-in function)
--
-- AdaScript Syntax: units.gal2l( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.gal2l
--       Conversion: gal * 4.546
-----------------------------------------------------------------------------

procedure ParseUnitsGal2L( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_gal2l_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 4.546 );
end ParseUnitsGal2L;


-----------------------------------------------------------------------------
--  PARSE UNITS ML 2 FL OZ                                (built-in function)
--
-- AdaScript Syntax: units.ml2floz( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.ml2floz
--       Conversion: ml * 0.03519
-----------------------------------------------------------------------------

procedure ParseUnitsMl2FlOz( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_ml2floz_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.03519 );
end ParseUnitsMl2FlOz;


-----------------------------------------------------------------------------
--  PARSE UNITS ML 2 US FL OZ                             (built-in function)
--
-- AdaScript Syntax: units.ml2usfloz( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.ml2usfloz
--       Conversion: ml * 0.033815
-----------------------------------------------------------------------------

procedure ParseUnitsMl2USFlOz( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_ml2usfloz_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.033815 );
end ParseUnitsMl2USFlOz;


-----------------------------------------------------------------------------
--  PARSE UNITS FL OZ 2 US FL OZ                          (built-in function)
--
-- AdaScript Syntax: units.floz2usfloz( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.floz2usfloz
--       Conversion: ml * 0.961
-----------------------------------------------------------------------------

procedure ParseUnitsFlOz2USFlOz( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_floz2usfloz_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.961 );
end ParseUnitsFlOz2USFlOz;


-----------------------------------------------------------------------------
--  PARSE UNITS L 2 QUARTS                                (built-in function)
--
-- AdaScript Syntax: units.l2quarts( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.l2quarts
--       Conversion: l * 0.8795
-----------------------------------------------------------------------------

procedure ParseUnitsL2Quarts( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_l2quarts_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.8795 );
end ParseUnitsL2Quarts;


-----------------------------------------------------------------------------
--  PARSE UNITS L 2 GAL                                   (built-in function)
--
-- AdaScript Syntax: units.l2gal( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.l2gal
--       Conversion: l * 0.21997
-----------------------------------------------------------------------------

procedure ParseUnitsL2Gal( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_l2gal_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.21997 );
end ParseUnitsL2Gal;


-----------------------------------------------------------------------------
--  PARSE UNITS F 2 C                                     (built-in function)
--
-- AdaScript Syntax: units.f2c( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.f2c
--       Conversion: f = 5/9*(c-32)
-----------------------------------------------------------------------------

procedure ParseUnitsF2C( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_f2c_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr ) then
          result := storage'( to_unbounded_string( 5.0 / 9.0 * (to_numeric( expr.value ) - 32.0 ) ), expr.metaLabel );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseUnitsF2C;


-----------------------------------------------------------------------------
--  PARSE UNITS C 2 F                                     (built-in function)
--
-- AdaScript Syntax: units.c2f( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.c2f
--       Conversion: c = 9/5f+32
-----------------------------------------------------------------------------

procedure ParseUnitsC2F( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_c2f_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr ) then
          result := storage'( to_unbounded_string( 9.0 / 5.0 * to_numeric( expr.value ) + 32.0 ), expr.metaLabel );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseUnitsC2F;


-----------------------------------------------------------------------------
--  PARSE UNITS K 2 C                                     (built-in function)
--
-- AdaScript Syntax: units.k2c( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.k2c
--       Conversion: c = k-273.15
-----------------------------------------------------------------------------

procedure ParseUnitsK2C( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_k2c_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr ) then
          result := storage'( to_unbounded_string( to_numeric( expr.value ) - 273.15 ), expr.metaLabel );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseUnitsK2C;


-----------------------------------------------------------------------------
--  PARSE UNITS C 2 K                                     (built-in function)
--
-- AdaScript Syntax: units.c2k( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.c2k
--       Conversion: k = c+273.15
-----------------------------------------------------------------------------

procedure ParseUnitsC2K( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_c2k_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr ) then
          result := storage'( to_unbounded_string( to_numeric( expr.value ) + 273.15 ), expr.metaLabel );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseUnitsC2K;


-----------------------------------------------------------------------------
--  PARSE UNITS US DRY GAL 2 L                            (built-in function)
--
-- AdaScript Syntax: units.usdrygal2l( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.usdrygal2l
--       Conversion: dg * 4.4049
-----------------------------------------------------------------------------

procedure ParseUnitsUSDryGal2L( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_usdrygal2l_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 4.4049 );
end ParseUnitsUSDryGal2L;


-----------------------------------------------------------------------------
--  PARSE UNITS L 2 US DRY GAL                            (built-in function)
--
-- AdaScript Syntax: units.l2usdrygal( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.l2usdrygal
--       Conversion: liters * 0.22702
-----------------------------------------------------------------------------

procedure ParseUnitsL2USDryGal( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_l2usdrygal_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.22702 );
end ParseUnitsL2USDryGal;


-----------------------------------------------------------------------------
--  PARSE UNITS US LIQ GAL 2 L                            (built-in function)
--
-- AdaScript Syntax: units.usliqgal2l( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.units.usliqgal2l
--       Conversion: lg * 3.7854
-----------------------------------------------------------------------------

procedure ParseUnitsUSLiqGal2L( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_usliqgal2l_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 3.7854 );
end ParseUnitsUSLiqGal2L;


-----------------------------------------------------------------------------
--  PARSE UNITS L 2 US LIQ GAL                            (built-in function)
--
-- AdaScript Syntax: units.l2usliqgal( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.units.l2usliqgal
--       Conversion: l * 0.26417
-----------------------------------------------------------------------------

procedure ParseUnitsL2USLiqGal( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_l2usliqgal_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.26417 );
end ParseUnitsL2USLiqGal;


-----------------------------------------------------------------------------
--  PARSE UNITS TR OZ 2 G                                 (built-in function)
--
-- AdaScript Syntax: units.troz2g( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.units.troz2g
--       Conversion: toz * 31.1035
-----------------------------------------------------------------------------

procedure ParseUnitsTrOz2G( result : out storage; kind : out identifier ) is
  -- Syntax: units.troz2g( expr );
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_troz2g_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 31.1035 );
end ParseUnitsTrOz2G;


-----------------------------------------------------------------------------
--  PARSE UNITS TR G 2 OZ                                 (built-in function)
--
-- AdaScript Syntax: units.g2troz( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.units.g2troz
--       Conversion: g * 0.03215
-----------------------------------------------------------------------------

procedure ParseUnitsG2TrOz( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_g2troz_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.03215 );
end ParseUnitsG2TrOz;


-----------------------------------------------------------------------------
--  PARSE UNITS CU CM 2 FL OZ                             (built-in function)
--
-- AdaScript Syntax: units.cucm2floz( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.units.cucm2floz
--       Conversion: cucm * 0.03519
-----------------------------------------------------------------------------

procedure ParseUnitsCuCm2FlOz( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_cucm2floz_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.03519 );
end ParseUnitsCuCm2FlOz;


-----------------------------------------------------------------------------
--  PARSE UNITS FL OZ 2 CU CM                             (built-in function)
--
-- AdaScript Syntax: units.floz2cucm( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.units.floz2cucm
--       Conversion: oz * 28.413
-----------------------------------------------------------------------------

procedure ParseUnitsFlOz2CuCm( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_floz2cucm_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 28.413 );
end ParseUnitsFlOz2CuCm;


-----------------------------------------------------------------------------
--  PARSE UNITS FL OZ 2 US FL OZ                          (built-in function)
--
-- AdaScript Syntax: units.floz2usfloz( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.units.floz2usfloz
--       Conversion: floz * 0.3381
-----------------------------------------------------------------------------

procedure ParseUnitsCuCm2USFlOz( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_cucm2usfloz_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 0.3381 );
end ParseUnitsCuCm2USFlOz;


-----------------------------------------------------------------------------
--  PARSE UNITS US FL OZ 2 CU CM                          (built-in function)
--
-- AdaScript Syntax: units.usfloz2cucm( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.units.usfloz2cucm
--       Conversion: floz * 29.5735
-----------------------------------------------------------------------------

procedure ParseUnitsUSFlOz2CuCm( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_usfloz2cucm_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  ParseSimpleConversion( subprogramId, result, expr, 29.5735 );
end ParseUnitsUSFlOz2CuCm;


-----------------------------------------------------------------------------
--  PARSE UNITS BYTES 2 MB                                (built-in function)
--
-- AdaScript Syntax: units.bytes2mb( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.units.bytes2mb
--       Conversion: bytes / 1024 / 1024
-----------------------------------------------------------------------------

procedure ParseUnitsBytes2MB( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_bytes2mb_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr ) then
          result := storage'( to_unbounded_string( to_numeric( expr.value )/1024.0/1024.0 ), expr.metaLabel );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseUnitsBytes2MB;


-----------------------------------------------------------------------------
--  PARSE UNITS MB 2 BYTES                                (built-in function)
--
-- AdaScript Syntax: units.mb2bytes( expr );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_units.html#units.units.mb2bytes
--       Conversion: mb * 1024 * 1024
-----------------------------------------------------------------------------

procedure ParseUnitsMB2Bytes( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type : identifier;
  subprogramId : constant identifier := units_mb2bytes_t;
begin
  kind := long_float_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type );
  begin
    if isExecutingCommand then
       if metaLabelOk( subprogramId, expr ) then
          result := storage'( to_unbounded_string( to_numeric( expr.value ) *1024.0*1024.0 ), expr.metaLabel );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseUnitsMB2Bytes;

procedure StartupUnits is
begin
  declareNamespace( "units" );
  declareFunction( units_inches2mm_t, "units.inches2mm", ParseUnitsInches2MM'access );
  declareFunction( units_feet2cm_t, "units.feet2cm", ParseUnitsFeet2CM'access );
  declareFunction( units_yards2m_t, "units.yards2m", ParseUnitsYards2M'access );
  declareFunction( units_miles2km_t, "units.miles2km", ParseUnitsMiles2Km'access );
  declareFunction( units_mm2inches_t, "units.mm2inches", ParseUnitsMM2Inches'access );
  declareFunction( units_cm2inches_t, "units.cm2inches", ParseUnitsCM2Inches'access );
  declareFunction( units_m2yards_t, "units.m2yards", ParseUnitsM2Yards'access );
  declareFunction( units_km2miles_t, "units.km2miles", ParseUnitsKm2Miles'access );
  declareFunction( units_sqin2sqcm_t, "units.sqin2sqcm", ParseUnitsSqIn2SqCm'access );
  declareFunction( units_sqft2sqm_t, "units.sqft2sqm", ParseUnitsSqFt2SqM'access );
  declareFunction( units_sqyd2sqm_t, "units.sqyd2sqm", ParseUnitsSqYd2SqM'access );
  declareFunction( units_acres2hectares_t, "units.acres2hectares", ParseUnitsAcres2Hectares'access );
  declareFunction( units_sqcm2sqin_t, "units.sqcm2sqin", ParseUnitsSqCm2SqIn'access );
  declareFunction( units_sqm2sqft_t, "units.sqm2sqft", ParseUnitsSqM2SqFt'access );
  declareFunction( units_sqm2sqyd_t, "units.sqm2sqyd", ParseUnitsSqM2SqYd'access );
  declareFunction( units_sqkm2sqmiles_t, "units.sqkm2sqmiles", ParseUnitsSqKm2SqMiles'access );
  declareFunction( units_hectares2acres_t, "units.hectares2acres", ParseUnitsHectares2Acres'access );
  declareFunction( units_oz2grams_t, "units.oz2grams", ParseUnitsOz2Grams'access );
  declareFunction( units_lb2kg_t, "units.lb2kg", ParseUnitsLb2Kg'access );
  declareFunction( units_tons2tonnes_t, "units.tons2tonnes", ParseUnitsTons2Tonnes'access );
  declareFunction( units_grams2oz_t, "units.grams2oz", ParseUnitsGrams2Oz'access );
  declareFunction( units_kg2lb_t, "units.kg2lb", ParseUnitsKg2Lb'access );
  declareFunction( units_tonnes2tons_t, "units.tonnes2tons", ParseUnitsTonnes2Tons'access );
  declareFunction( units_ly2pc_t, "units.ly2pc", ParseUnitsLy2Pc'access );
  declareFunction( units_pc2ly_t, "units.pc2ly", ParseUnitsPc2Ly'access );
  declareFunction( units_floz2ml_t, "units.floz2ml", ParseUnitsFlOz2Ml'access );
  declareFunction( units_pints2l_t, "units.pints2l", ParseUnitsPints2L'access );
  declareFunction( units_gal2l_t, "units.gal2l", ParseUnitsGal2L'access );
  declareFunction( units_ml2floz_t, "units.ml2floz", ParseUnitsMl2FlOz'access );
  declareFunction( units_l2quarts_t, "units.l2quarts", ParseUnitsL2Quarts'access );
  declareFunction( units_l2gal_t, "units.l2gal", ParseUnitsL2Gal'access );
  declareFunction( units_f2c_t, "units.f2c", ParseUnitsF2C'access );
  declareFunction( units_c2f_t, "units.c2f", ParseUnitsC2F'access );
  declareFunction( units_k2c_t, "units.k2c", ParseUnitsK2C'access );
  declareFunction( units_c2k_t, "units.c2k", ParseUnitsC2K'access );
  declareFunction( units_usfloz2ml_t, "units.usfloz2ml", ParseUnitsUSFlOz2Ml'access );
  declareFunction( units_usfloz2floz_t, "units.usfloz2floz", ParseUnitsUSFlOz2FlOz'access );
  declareFunction( units_ml2usfloz_t, "units.ml2usfloz", ParseUnitsMl2USFlOz'access );
  declareFunction( units_floz2usfloz_t, "units.floz2usfloz", ParseUnitsFlOz2USFlOz'access );
  declareFunction( units_usdrygal2l_t, "units.usdrygal2l", ParseUnitsUSDryGal2L'access );
  declareFunction( units_l2usdrygal_t, "units.l2usdrygal", ParseUnitsL2USDryGal'access );
  declareFunction( units_usliqgal2l_t, "units.usliqgal2l", ParseUnitsUSLiqGal2L'access );
  declareFunction( units_l2usliqgal_t, "units.l2usliqgal", ParseUnitsL2USLiqGal'access );
  declareFunction( units_troz2g_t, "units.troz2g", ParseUnitsTrOz2G'access );
  declareFunction( units_g2troz_t, "units.g2troz", ParseUnitsG2TrOz'access );
  declareFunction( units_cucm2floz_t, "units.cucm2floz", ParseUnitsCuCm2FlOz'access );
  declareFunction( units_floz2cucm_t, "units.floz2cucm", ParseUnitsFlOz2CuCm'access );
  declareFunction( units_cucm2usfloz_t, "units.cucm2usfloz", ParseUnitsCuCm2USFlOz'access );
  declareFunction( units_usfloz2cucm_t, "units.usfloz2cucm", ParseUnitsUSFlOz2CuCm'access );
  declareFunction( units_bytes2mb_t, "units.bytes2mb", ParseUnitsBytes2MB'access );
  declareFunction( units_mb2bytes_t, "units.mb2bytes", ParseUnitsMB2Bytes'access );
  declareNamespaceClosed( "units" );
end StartupUnits;

procedure ShutdownUnits is
begin
  null;
end ShutdownUnits;

end parser_units;

