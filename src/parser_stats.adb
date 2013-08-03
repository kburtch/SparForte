------------------------------------------------------------------------------
-- Stats Package Parser                                                     --
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

with gnat.bubble_sort_a,
     gnat.heap_sort_a,
     ada.numerics.float_random,
     ada.numerics.long_elementary_functions,
     bush_os,
     string_util,
     world,
     scanner_arrays,
     parser,
     parser_aux;
use  ada.numerics.long_elementary_functions,
     bush_os,
     string_util,
     world,
     scanner_arrays,
     parser,
     parser_aux;

package body parser_stats is


---------------------------------------------------------
-- PARSE THE STATS PACKAGE
---------------------------------------------------------

procedure ParseStatsMax( f : out unbounded_string; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  array_id : arrayID;
  max : long_float;
  max_string : unbounded_string;
begin
  expect( stats_max_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( array_id );
     last  := lastBound( array_id );
     max_string := arrayElement( array_id, first);
     max := to_numeric( max_string );
     for i in first+1..last loop
         if to_numeric( arrayElement( array_id, i) ) > max then
            max_string := arrayElement( array_id, i);
            max := to_numeric( max_string );
         end if;
     end loop;
     f := max_string;
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsMax;

procedure ParseStatsMin( f : out unbounded_string; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  array_id : arrayID;
  min : long_float;
  min_string : unbounded_string;
begin
  expect( stats_min_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( array_id );
     last  := lastBound( array_id );
     min_string := arrayElement( array_id, first);
     min := to_numeric( min_string );
     for i in first+1..last loop
         if to_numeric( arrayElement( array_id, i) ) < min then
            min_string := arrayElement( array_id, i);
            min := to_numeric( min_string );
         end if;
     end loop;
     f := min_string;
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsMin;

procedure ParseStatsSum( f : out unbounded_string; kind : out identifier ) is
-- should really be in a stats package
  var_id : identifier;
  first, last : long_integer;
  array_id : arrayID;
  sum : long_float;
begin
  expect( stats_sum_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( array_id );
     last  := lastBound( array_id );
     sum := 0.0;
     for i in first..last loop
         sum := sum + to_numeric( arrayElement( array_id, i) );
     end loop;
     f := to_unbounded_string( sum );
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsSum;

procedure ParseStatsAverage( f : out unbounded_string; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  len    : long_integer;
  array_id : arrayID;
  sum : long_float;
begin
  expect( stats_average_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( array_id );
     last  := lastBound( array_id );
     len   := last-first+1;
     sum := 0.0;
     for i in first..last loop
         sum := sum + to_numeric( arrayElement( array_id, i) );
     end loop;
     f := to_unbounded_string( sum / long_float( len ) );
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsAverage;

procedure ParseStatsVariance( f : out unbounded_string; kind : out identifier ) is
  var_id   : identifier;
  first, last : long_integer;
  len      : long_integer;
  array_id : arrayID;
  sum      : long_float;
  diff     : long_float;
  mean     : long_float;
  sum_diff_sq : long_float;
begin
  expect( stats_variance_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( array_id );
     last  := lastBound( array_id );
     len   := last-first+1;
     sum := 0.0;
     for i in first..last loop
         sum := sum + to_numeric( arrayElement( array_id, i) );
     end loop;
     mean := sum / long_float( len );
     sum_diff_sq := 0.0;
     for i in first..last loop
         diff := to_numeric( arrayElement( array_id, i ) ) - mean;
         sum_diff_sq := sum_diff_sq + diff * diff;
     end loop;
     f := to_unbounded_string( sum_diff_sq / long_float( len-1 ) );
     -- kind   := identifiers( var_id ).kind;
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsVariance;

procedure ParseStatsStandardDeviation( f : out unbounded_string; kind : out identifier ) is
  var_id   : identifier;
  first, last : long_integer;
  len      : long_integer;
  array_id : arrayID;
  sum      : long_float;
  diff     : long_float;
  mean     : long_float;
  sum_diff_sq : long_float;
begin
  expect( stats_standard_deviation_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if identifiers( var_id ).class = typeClass or identifiers( var_id ).class = subClass then
     var_id := getBaseType( var_id );
     if not identifiers( var_id ).list then
        err( "Array or array type expected" );
     end if;
  elsif not (class_ok( var_id, otherClass ) and identifiers( var_id ).list) then
     err( "Array or array type expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := firstBound( array_id );
     last  := lastBound( array_id );
     len   := last-first+1;
     sum := 0.0;
     for i in first..last loop
         sum := sum + to_numeric( arrayElement( array_id, i) );
     end loop;
     mean := sum / long_float( len );
     sum_diff_sq := 0.0;
     for i in first..last loop
         diff := to_numeric( arrayElement( array_id, i ) ) - mean;
         sum_diff_sq := sum_diff_sq + diff * diff;
     end loop;
     f := to_unbounded_string( sqrt( sum_diff_sq / long_float( len-1 ) ) );
     -- kind   := identifiers( var_id ).kind;
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsStandardDeviation;

-------------------------------------------------------------------------------
-- Housekeeping
-------------------------------------------------------------------------------

procedure StartupStats is
begin
  declareFunction( stats_average_t, "stats.average", ParseStatsAverage'access );
  declareFunction( stats_max_t, "stats.max", ParseStatsMax'access );
  declareFunction( stats_min_t, "stats.min", ParseStatsMin'access );
  declareFunction( stats_standard_deviation_t, "stats.standard_deviation", ParseStatsStandardDeviation'access );
  declareFunction( stats_sum_t, "stats.sum", ParseStatsSum'access );
  declareFunction( stats_variance_t, "stats.variance", ParseStatsVariance'access );
end StartupStats;

procedure ShutdownStats is
begin
  null;
end ShutdownStats;

end parser_stats;
