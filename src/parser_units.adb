------------------------------------------------------------------------------
-- Units Package Parser                                                     --
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

--with ada.text_io;
--use ada.text_io;
with world, scanner,
    parser_aux,
    parser_params,
    parser;
use world, scanner,
    parser_aux,
    parser_params,
    parser;

package body parser_units is


procedure ParseSimpleConversion( result : out unbounded_string;
  expr_val : in unbounded_string;
  factor   : in long_float ) is
-- Many conversions are simple multiplication.  Using this procedure
-- cuts the (binary) size of this package in half.
begin
    if isExecutingCommand then
       result := to_unbounded_string( to_numeric( expr_val ) * factor );
    end if;
exception when others =>
    err( "exception raised" );
end ParseSimpleConversion;

procedure ParseUnitsInches2mm( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.inches2mm( expr );
  -- Conversion: inches * 25.4
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_inches2mm_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 25.4 );
end ParseUnitsInches2mm;

procedure ParseUnitsFeet2cm( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.feet2cm( expr );
  -- Conversion: feet * 30.48
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_feet2cm_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 30.48 );
end ParseUnitsFeet2cm;

procedure ParseUnitsYards2m( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.yards2m( expr );
  -- Conversion: yards * 0.9144
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_yards2m_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.9144 );
end ParseUnitsYards2m;

procedure ParseUnitsMiles2km( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.miles2km( expr );
  -- Conversion: miles * 1.60934
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_miles2km_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 1.60934 );
end ParseUnitsMiles2km;

procedure ParseUnitsMM2Inches( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.mm2inches( expr );
  -- Conversion: mm * 0.03937
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_mm2inches_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.03937 );
end ParseUnitsMM2Inches;

procedure ParseUnitsCm2Inches( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.cm2inches( expr );
  -- Conversion: cm2 * 0.3937
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_cm2inches_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.3937 );
end ParseUnitsCm2Inches;

procedure ParseUnitsM2Yards( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.m2yards( expr );
  -- Conversion:  m * 1.0936
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_m2yards_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 1.0936 );
end ParseUnitsM2Yards;

procedure ParseUnitsKm2Miles( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.km2miles( expr );
  -- Conversion: km * 0.62137
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_km2miles_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.62137 );
end ParseUnitsKm2Miles;

procedure ParseUnitsSqIn2SqCm( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.km2miles( expr );
  -- Conversion: sq in * 6.4516
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_sqin2sqcm_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 6.4516 );
end ParseUnitsSqIn2SqCm;

procedure ParseUnitsSqFt2SqM( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.sqft2sqm( expr );
  -- Conversion: sq ft * 0.092903
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_sqft2sqm_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.092903 );
end ParseUnitsSqFt2SqM;

procedure ParseUnitsSqYd2SqM( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.sqyd2sqm( expr );
  -- Conversion: sq yd * 0.836127
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_sqyd2sqm_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.836127 );
end ParseUnitsSqYd2SqM;

procedure ParseUnitsAcres2Hectares( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.acres2hectares( expr );
  -- Conversion: acres * 0.40486
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_acres2hectares_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.40486 );
end ParseUnitsAcres2Hectares;

procedure ParseUnitsSqCm2SqIn( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.sqcm2sqin( expr );
  -- Conversion: sq cm * 0.155
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_sqcm2sqin_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.155 );
end ParseUnitsSqCm2SqIn;

procedure ParseUnitsSqM2SqFt( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.sqm2sqft( expr );
  -- Conversion: sq m * 10.7639
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_sqm2sqft_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 10.7639 );
end ParseUnitsSqM2SqFt;

procedure ParseUnitsSqM2SqYd( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.sqm2sqyd( expr );
  -- Conversion: sq m * 1.19599
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_sqm2sqyd_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 1.19599 );
end ParseUnitsSqM2SqYd;

procedure ParseUnitsSqKm2SqMiles( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.sqkm2sqmiles( expr );
  -- Conversion: sq km * 0.38611
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_sqkm2sqmiles_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.38611 );
end ParseUnitsSqKm2SqMiles;

procedure ParseUnitsHectares2Acres( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.sqkm2sqm( expr );
  -- Conversion: hectares * 2.471
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_hectares2acres_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 2.471 );
end ParseUnitsHectares2Acres;

procedure ParseUnitsLy2Pc( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.Ly2Pc( expr );
  -- Conversion: ly * 0.3066
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_ly2pc_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.3066 );
end ParseUnitsLy2Pc;

procedure ParseUnitsPc2Ly( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.Pc2Ly( expr );
  -- Conversion: pc * 3.2616
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_pc2ly_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 3.2616 );
end ParseUnitsPc2Ly;

procedure ParseUnitsOz2Grams( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.oz2grams( expr );
  -- Conversion: oz * 28.349
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_oz2grams_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 28.349 );
end ParseUnitsOz2Grams;

procedure ParseUnitsLb2Kg( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.lb2kg( expr );
  -- Conversion: lb * 0.45359
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_lb2kg_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.45359 );
end ParseUnitsLb2Kg;

procedure ParseUnitsTons2Tonnes( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.tons2tonnes( expr );
  -- Conversion: tons * 1.01605
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_tons2tonnes_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 1.01605 );
end ParseUnitsTons2Tonnes;

procedure ParseUnitsGrams2Oz( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.tons2tonnes( expr );
  -- Conversion: grams * 0.3527
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_grams2oz_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.3527 );
end ParseUnitsGrams2Oz;

procedure ParseUnitsKg2Lb( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.kg2lb( expr );
  -- Conversion: kg * 2.2046
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_kg2lb_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 2.2046 );
end ParseUnitsKg2Lb;

procedure ParseUnitsTonnes2Tons( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.tonnes2tons( expr );
  -- Conversion: tonnes * 0.9842
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_tonnes2tons_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.9842 );
end ParseUnitsTonnes2Tons;

procedure ParseUnitsFlOz2Ml( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.floz2ml( expr );
  -- Conversion: fl oz * 28.4131
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_floz2ml_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 28.4131 );
end ParseUnitsFlOz2Ml;

procedure ParseUnitsUSFlOz2Ml( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.usfloz2ml( expr );
  -- Conversion: U.S. fl oz * 29.57
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_usfloz2ml_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 29.57 );
end ParseUnitsUSFlOz2Ml;

procedure ParseUnitsUSFlOz2FlOz( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.usfloz2oz( expr );
  -- Conversion: U.S. fl oz * 1.041
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_usfloz2floz_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 1.041 );
end ParseUnitsUSFlOz2FlOz;

procedure ParseUnitsPints2L( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.pints2l( expr );
  -- Conversion: pints * 0.568
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_pints2l_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.568 );
end ParseUnitsPints2L;

procedure ParseUnitsGal2L( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.gal2l( expr );
  -- Conversion: gal * 4.546
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_gal2l_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 4.546 );
end ParseUnitsGal2L;

procedure ParseUnitsMl2FlOz( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.ml2floz( expr );
  -- Conversion: ml * 0.03519
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_ml2floz_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.03519 );
end ParseUnitsMl2FlOz;

procedure ParseUnitsMl2USFlOz( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.ml2usfloz( expr );
  -- Conversion: ml * 0.033815
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_ml2usfloz_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.033815 );
end ParseUnitsMl2USFlOz;

procedure ParseUnitsFlOz2USFlOz( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.floz2usfloz( expr );
  -- Conversion: ml * 0.961
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_floz2usfloz_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.961 );
end ParseUnitsFlOz2USFlOz;

procedure ParseUnitsL2Quarts( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.l2quarts( expr );
  -- Conversion: l * 0.8795
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_l2quarts_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.8795 );
end ParseUnitsL2Quarts;

procedure ParseUnitsL2Gal( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.l2gal( expr );
  -- Conversion: l * 0.21997
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_l2gal_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.21997 );
end ParseUnitsL2Gal;

procedure ParseUnitsF2C( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.f2c( expr );
  -- Conversion: f = 5/9*(c-32)
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_f2c_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( 5.0 / 9.0 * (to_numeric( expr_val ) - 32.0 ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseUnitsF2C;

procedure ParseUnitsC2F( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.c2f( expr );
  -- Conversion: c = 9/5f+32
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_c2f_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( 9.0 / 5.0 * to_numeric( expr_val ) + 32.0 );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseUnitsC2F;

procedure ParseUnitsK2C( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.k2c( expr );
  -- Conversion: c = k-273.15
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_k2c_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( to_numeric( expr_val ) - 273.15 );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseUnitsK2C;

procedure ParseUnitsC2K( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.c2f( expr );
  -- Conversion: k = c+273.15
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_c2k_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( to_numeric( expr_val ) + 273.15 );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseUnitsC2K;

procedure ParseUnitsUSDryGal2L( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.usdrygal2l( expr );
  -- Conversion: dg * 4.4049
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_usdrygal2l_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 4.4049 );
end ParseUnitsUSDryGal2L;

procedure ParseUnitsL2USDryGal( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.l2usdrygal( expr );
  -- Conversion: liters * 0.22702
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_l2usdrygal_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.22702 );
end ParseUnitsL2USDryGal;

procedure ParseUnitsUSLiqGal2L( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.usliqgal2l( expr );
  -- Conversion: lg * 3.7854
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_usliqgal2l_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 3.7854 );
end ParseUnitsUSLiqGal2L;

procedure ParseUnitsL2USLiqGal( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.l2usliqgal( expr );
  -- Conversion: l * 0.26417
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_l2usliqgal_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.26417 );
end ParseUnitsL2USLiqGal;

procedure ParseUnitsTrOz2G( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.troz2g( expr );
  -- Conversion: toz * 31.1035
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_troz2g_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 31.1035 );
end ParseUnitsTrOz2G;

procedure ParseUnitsG2TrOz( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.g2troz( expr );
  -- Conversion: g * 0.03215
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_g2troz_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.03215 );
end ParseUnitsG2TrOz;

procedure ParseUnitsCuCm2FlOz( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.cucm2floz( expr );
  -- Conversion: cucm * 0.03519
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_cucm2floz_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.03519 );
end ParseUnitsCuCm2FlOz;

procedure ParseUnitsFlOz2CuCm( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.floz2cucm( expr );
  -- Conversion: oz * 28.413
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_floz2cucm_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 28.413 );
end ParseUnitsFlOz2CuCm;

procedure ParseUnitsCuCm2USFlOz( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.cucm2usfloz( expr );
  -- Conversion: cucm * 0.3381
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_cucm2usfloz_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 0.3381 );
end ParseUnitsCuCm2USFlOz;

procedure ParseUnitsUSFlOz2CuCm( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.usfloz2cucm( expr );
  -- Conversion: floz * 29.5735
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_usfloz2cucm_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  ParseSimpleConversion( result, expr_val, 29.5735 );
end ParseUnitsUSFlOz2CuCm;

procedure ParseUnitsBytes2MB( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.bytes2mb( expr );
  -- Conversion: bytes / 1024 / 1024
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_bytes2mb_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( to_numeric( expr_val )/1024.0/1024.0 );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseUnitsBytes2MB;

procedure ParseUnitsMB2Bytes( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: units.mb2bytes( expr );
  -- Conversion: mb * 1024 * 1024
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := long_float_t;
  expect( units_mb2bytes_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( to_numeric( expr_val ) *1024.0*1024.0 );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseUnitsMB2Bytes;

procedure StartupUnits is
begin
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
end StartupUnits;

procedure ShutdownUnits is
begin
  null;
end ShutdownUnits;

end parser_units;

