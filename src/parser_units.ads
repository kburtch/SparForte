------------------------------------------------------------------------------
-- BUSH Units Package Parser                                                --
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
-- CVS: $Id: parser_units.ads,v 1.2 2005/02/11 02:59:29 ken Exp $

with ada.strings.unbounded, world;
use ada.strings.unbounded, world;

package parser_units is

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

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupUnits;
procedure ShutdownUnits;

------------------------------------------------------------------------------
-- PARSE THE UNITS PACKAGE
------------------------------------------------------------------------------

procedure ParseUnitsInches2mm( result : out unbounded_string );
  -- Syntax: units.inches2mm( expr );
  -- Conversion: inches * 25.4

procedure ParseUnitsFeet2cm( result : out unbounded_string );
  -- Syntax: units.feet2cm( expr );
  -- Conversion: feet * 30.48

procedure ParseUnitsYards2m( result : out unbounded_string );
  -- Syntax: units.yards2m( expr );
  -- Conversion: yards * 0.9144

procedure ParseUnitsMiles2km( result : out unbounded_string );
  -- Syntax: units.miles2km( expr );
  -- Conversion: miles * 1.60934

procedure ParseUnitsMM2Inches( result : out unbounded_string );
  -- Syntax: units.mm2inches( expr );
  -- Conversion: mm * 0.03937

procedure ParseUnitsCm2Inches( result : out unbounded_string );
  -- Syntax: units.cm2inches( expr );
  -- Conversion: cm2 * 0.3937

procedure ParseUnitsM2Yards( result : out unbounded_string );
  -- Syntax: units.m2yards( expr );
  -- Conversion:  m * 1.0936

procedure ParseUnitsKm2Miles( result : out unbounded_string );
  -- Syntax: units.km2miles( expr );
  -- Conversion: km * 0.62137

procedure ParseUnitsSqIn2SqCm( result : out unbounded_string );
  -- Syntax: units.km2miles( expr );
  -- Conversion: sq in * 6.4516

procedure ParseUnitsSqFt2SqM( result : out unbounded_string );
  -- Syntax: units.sqft2sqm( expr );
  -- Conversion: sq ft * 0.092903

procedure ParseUnitsSqYd2SqM( result : out unbounded_string );
  -- Syntax: units.sqyd2sqm( expr );
  -- Conversion: sq yd * 0.836127

procedure ParseUnitsAcres2Hectares( result : out unbounded_string );
  -- Syntax: units.acres2hectares( expr );
  -- Conversion: acres * 0.40486

procedure ParseUnitsSqCm2SqIn( result : out unbounded_string );
  -- Syntax: units.sqcm2sqin( expr );
  -- Conversion: sq cm * 0.155

procedure ParseUnitsSqM2SqFt( result : out unbounded_string );
  -- Syntax: units.sqm2sqft( expr );
  -- Conversion: sq m * 10.7639

procedure ParseUnitsSqM2SqYd( result : out unbounded_string );
  -- Syntax: units.sqm2sqyd( expr );
  -- Conversion: sq m * 1.19599

procedure ParseUnitsSqKm2SqMiles( result : out unbounded_string );
  -- Syntax: units.sqkm2sqmiles( expr );
  -- Conversion: sq km * 0.38611

procedure ParseUnitsHectares2Acres( result : out unbounded_string );
  -- Syntax: units.sqkm2sqm( expr );
  -- Conversion: hectares * 2.471

procedure ParseUnitsOz2Grams( result : out unbounded_string );
  -- Syntax: units.oz2grams( expr );
  -- Conversion: oz * 28.349

procedure ParseUnitsLb2Kg( result : out unbounded_string );
  -- Syntax: units.lb2kg( expr );
  -- Conversion: lb * 0.45359

procedure ParseUnitsTons2Tonnes( result : out unbounded_string );
  -- Syntax: units.tons2tonnes( expr );
  -- Conversion: tons * 1.01605

procedure ParseUnitsGrams2Oz( result : out unbounded_string );
  -- Syntax: units.tons2tonnes( expr );
  -- Conversion: grams * 0.3527

procedure ParseUnitsKg2Lb( result : out unbounded_string );
  -- Syntax: units.kg2lb( expr );
  -- Conversion: kg * 2.2046

procedure ParseUnitsTonnes2Tons( result : out unbounded_string );
  -- Syntax: units.tonnes2tons( expr );
  -- Conversion: tonnes * 0.9842

procedure ParseUnitsLy2Pc( result : out unbounded_string );
  -- Syntax: units.ly2pc( expr );
  -- Conversion: ly * 0.3066

procedure ParseUnitsPc2Ly( result : out unbounded_string );
  -- Syntax: units.pc2ly( expr );
  -- Conversion: pc * 3.2616

procedure ParseUnitsFlOz2Ml( result : out unbounded_string );
  -- Syntax: units.floz2ml( expr );
  -- Conversion: fl oz * 28.4131

procedure ParseUnitsUSFlOz2Ml( result : out unbounded_string );
  -- Syntax: units.floz2ml( expr );
  -- Conversion: fl oz * 29.57

procedure ParseUnitsUSFlOz2FlOz( result : out unbounded_string );
  -- Syntax: units.floz2ml( expr );
  -- Conversion: fl oz * 1.041

procedure ParseUnitsPints2L( result : out unbounded_string );
  -- Syntax: units.pints2l( expr );
  -- Conversion: pints * 0.568 

procedure ParseUnitsGal2L( result : out unbounded_string );
  -- Syntax: units.gal2l( expr );
  -- Conversion: gal * 4.546

procedure ParseUnitsMl2FlOz( result : out unbounded_string );
  -- Syntax: units.ml2floz( expr );
  -- Conversion: ml * 0.03519

procedure ParseUnitsMl2USFlOz( result : out unbounded_string );
  -- Syntax: units.ml2floz( expr );
  -- Conversion: ml * 0.033815

procedure ParseUnitsFlOz2USFlOz( result : out unbounded_string );
  -- Syntax: units.ml2floz( expr );
  -- Conversion: ml * 0.961

procedure ParseUnitsL2Quarts( result : out unbounded_string );
  -- Syntax: units.l2quarts( expr );
  -- Conversion: l * 0.8795

procedure ParseUnitsL2Gal( result : out unbounded_string );
  -- Syntax: units.l2gal( expr );
  -- Conversion: l * 0.21997

procedure ParseUnitsF2C( result : out unbounded_string );
  -- Syntax: units.f2c( expr );
  -- Conversion: f = 5/9*c-32

procedure ParseUnitsC2F( result : out unbounded_string );
  -- Syntax: units.c2f( expr );
  -- Conversion: c= 9/5f+32

procedure ParseUnitsK2C( result : out unbounded_string );
  -- Syntax: units.k2c( expr );
  -- Conversion: c = k-273.15

procedure ParseUnitsC2K( result : out unbounded_string );
  -- Syntax: units.c2k( expr );
  -- Conversion: k = c+273.15

procedure ParseUnitsUSDryGal2L( result : out unbounded_string );
  -- Syntax: units.usdrygal2l( expr );
  -- Conversion: dg * 4.4049

procedure ParseUnitsL2USDryGal( result : out unbounded_string );
  -- Syntax: units.l2usdrygal( expr );
  -- Conversion: liters * 0.22702

procedure ParseUnitsUSLiqGal2L( result : out unbounded_string );
  -- Syntax: units.usliqgal2l( expr );
  -- Conversion: lg * 3.7854

procedure ParseUnitsL2USLiqGal( result : out unbounded_string );
  -- Syntax: units.l2usliqgal( expr );
  -- Conversion: l * 0.26417

procedure ParseUnitsTrOz2G( result : out unbounded_string );
  -- Syntax: units.troz2g( expr );
  -- Conversion: toz * 31.1035

procedure ParseUnitsG2TrOz( result : out unbounded_string );
  -- Syntax: units.g2troz( expr );
  -- Conversion: g * 0.03215

procedure ParseUnitsCuCm2FlOz( result : out unbounded_string );
  -- Syntax: units.cucm2floz( expr );
  -- Conversion: cucm * 0.03519

procedure ParseUnitsFlOz2CuCm( result : out unbounded_string );
  -- Syntax: units.floz2cucm( expr );
  -- Conversion: floz * 28.413

procedure ParseUnitsCuCm2USFlOz( result : out unbounded_string );
  -- Syntax: units.cucm2usfloz( expr );
  -- Conversion: cucm * 0.3381

procedure ParseUnitsUSFlOz2CuCm( result : out unbounded_string );
  -- Syntax: units.usfloz2cucm( expr );
  -- Conversion: floz * 29.5735

procedure ParseUnitsBytes2MB( result : out unbounded_string );
  -- Syntax: units.bytes2mb( expr );
  -- Conversion: bytes / 1024 / 1024

procedure ParseUnitsMB2Bytes( result : out unbounded_string );
  -- Syntax: units.mb2bytes( expr );
  -- Conversion: mb * 1024 * 1024

end parser_units;

