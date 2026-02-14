------------------------------------------------------------------------------
-- Stats Package Parser                                                     --
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
with ada.text_io; use ada.text_io;

with gnat.source_info,
     ada.strings.unbounded,
     pegasoft.numerics,
     scanner.communications,
     world,
     symbol_table,
     message_strings,
     scanner,
     parser;
use  ada.strings.unbounded,
     pegasoft,
     pegasoft.numerics.elementary_functions,
     world,
     symbol_table,
     message_strings,
     scanner,
     scanner.communications,
     parser;

package body parser_stats is

------------------------------------------------------------------------------
-- Stats package identifiers
------------------------------------------------------------------------------

stats_average_t  : identifier;
stats_max_t      : identifier;
stats_min_t      : identifier;
stats_standard_deviation_t : identifier;
stats_sum_t      : identifier;
stats_variance_t : identifier;

---------------------------------------------------------
-- PARSE THE STATS PACKAGE
---------------------------------------------------------

procedure ParseStatsMax( result : out storage; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  -- array_id : arrayID;
  max : numericValue;
  max_string : unbounded_string;
begin
  kind := universal_t;
  expect( stats_max_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( +"Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     kind   := identifiers( identifiers( var_id ).kind ).kind;
     begin
        first := identifiers( var_id ).aStorage'first;
        last  := identifiers( var_id ).aStorage'last;
        if last >= first then
           max_string := identifiers( var_id ).aStorage( first ).value;
           max := to_numeric( max_string );
           if metaLabelOk( stats_max_t, identifiers( var_id ).aStorage( first ) ) then
              result.unitMetaLabel := identifiers( var_id ).aStorage( first ).unitMetaLabel;
              result.policyMetaLabels := identifiers( var_id ).aStorage( first ).policyMetaLabels;
           end if;
           for i in first+1..last loop
               if metaLabelOk( stats_max_t, identifiers( var_id ).aStorage( i ) ) then
                  if to_numeric( identifiers( var_id ).aStorage( i ).value ) > max then
                     max_string := identifiers( var_id ).aStorage( i ).value;
                     max := to_numeric( max_string );
                     result.unitMetaLabel := identifiers( var_id ).aStorage( i ).unitMetaLabel;
                     result.policyMetaLabels := resolveEffectiveMetaLabels(
                        kind,
                        result,
                        identifiers( var_id ).aStorage( i )
                     );
                  end if;
               end if;
           end loop;
           result.value := max_string;
        else
           result := storage'( to_unbounded_string( 0 ), noMetaLabel, noMetaLabels );
           err( +"array is empty" );
        end if;
     exception when CONSTRAINT_ERROR =>
        err( pl( "constraint_error : index out of range " & identifiers( var_id ).aStorage'first'img & " .. " & identifiers( var_id ).aStorage'last'img ) );
     when STORAGE_ERROR =>
        err( pl( gnat.source_info.source_location & ": internal error : storage error raised when maxing array" ) );
     end;
  end if;
end ParseStatsMax;

procedure ParseStatsMin( result : out storage; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  -- array_id : arrayID;
  min : numericValue;
  min_string : unbounded_string;
begin
  kind := universal_t;
  expect( stats_min_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( +"Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     begin
        first := identifiers( var_id ).aStorage'first;
        last  := identifiers( var_id ).aStorage'last;
        if last >= first then
           min_string := identifiers( var_id ).aStorage( first ).value;
           min := to_numeric( min_string );
           if metaLabelOk( stats_min_t, identifiers( var_id ).aStorage( first ) ) then
              result.unitMetaLabel := identifiers( var_id ).aStorage( first ).unitMetaLabel;
              result.policyMetaLabels := identifiers( var_id ).aStorage( first ).policyMetaLabels;
           end if;
           for i in first+1..last loop
               if metaLabelOk( stats_min_t, identifiers( var_id ).aStorage( i ) ) then
                  if to_numeric( identifiers( var_id ).aStorage( i ).value ) < min then
                     min_string := identifiers( var_id ).aStorage( i ).value;
                     min := to_numeric( min_string );
                     result.unitMetaLabel := identifiers( var_id ).aStorage( i ).unitMetaLabel;
                     result.policyMetaLabels := resolveEffectiveMetaLabels(
                        kind,
                        result,
                        identifiers( var_id ).aStorage( i )
                     );
                  end if;
               end if;
           end loop;
           result.value := min_string;
        else
           result := storage'( to_unbounded_string( 0 ), noMetaLabel, noMetaLabels );
           err( +"array is empty" );
        end if;
     exception when CONSTRAINT_ERROR =>
        err( pl( "constraint_error : index out of range " & identifiers( var_id ).aStorage'first'img & " .. " & identifiers( var_id ).aStorage'last'img ) );
     when STORAGE_ERROR =>
        err( pl( gnat.source_info.source_location & ": internal error : storage error raised when minning array" ) );
     end;
     kind := identifiers( identifiers( var_id ).kind ).kind;
  end if;
end ParseStatsMin;

procedure ParseStatsSum( result : out storage; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  -- array_id : arrayID;
  sum : numericValue;
begin
  kind := universal_t;
  expect( stats_sum_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( +"Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     begin
        first := identifiers( var_id ).aStorage'first;
        last  := identifiers( var_id ).aStorage'last;
        sum := 0.0;
        if last >= first then
           if metaLabelOk( stats_sum_t, identifiers( var_id ).aStorage( first ) ) then
              result.unitMetaLabel := identifiers( var_id ).aStorage( first ).unitMetaLabel;
              result.policyMetaLabels := identifiers( var_id ).aStorage( first ).policyMetaLabels;
           end if;
           for i in first..last loop
               if metaLabelOk( stats_sum_t, identifiers( var_id ).aStorage( i ) ) then
                  sum := sum + to_numeric( identifiers( var_id ).aStorage( i ).value );
                  result.unitMetaLabel := identifiers( var_id ).aStorage( i ).unitMetaLabel;
                  result.policyMetaLabels := resolveEffectiveMetaLabels(
                     kind,
                     result,
                     identifiers( var_id ).aStorage( i )
                  );
               end if;
           end loop;
           result.value := to_unbounded_string( sum );
        else
           result := storage'( to_unbounded_string( 0 ), noMetaLabel, noMetaLabels );
           err( +"array is empty" );
        end if;
     exception when CONSTRAINT_ERROR =>
        err( pl( "constraint_error : index out of range " & identifiers( var_id ).aStorage'first'img & " .. " & identifiers( var_id ).aStorage'last'img ) );
     when STORAGE_ERROR =>
        err( pl( gnat.source_info.source_location & ": internal error : storage error raised when minning array" ) );
     end;
     kind := identifiers( identifiers( var_id ).kind ).kind;
  end if;
end ParseStatsSum;

procedure ParseStatsAverage( result : out storage; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  len    : long_integer;
  --array_id : arrayID;
  sum : numericValue;
begin
  kind := universal_t;
  expect( stats_average_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( +"Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     --array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     begin
        first := identifiers( var_id ).aStorage'first;
        last  := identifiers( var_id ).aStorage'last;
        len   := last-first+1;
        sum := 0.0;
        if last >= first then
           if metaLabelOk( stats_average_t, identifiers( var_id ).aStorage( first ) ) then
              result.unitMetaLabel := identifiers( var_id ).aStorage( first ).unitMetaLabel;
              result.policyMetaLabels := identifiers( var_id ).aStorage( first ).policyMetaLabels;
           end if;
           for i in first..last loop
               if metaLabelOk( stats_average_t, identifiers( var_id ).aStorage( i ) ) then
                  sum := sum + to_numeric( identifiers( var_id ).aStorage( i ).value );
                  result.unitMetaLabel := identifiers( var_id ).aStorage( i ).unitMetaLabel;
                  result.policyMetaLabels := resolveEffectiveMetaLabels(
                     kind,
                     result,
                     identifiers( var_id ).aStorage( i )
                  );
               end if;
           end loop;
           result.value := to_unbounded_string( sum / numericValue( len ) );
        else
           result := storage'( to_unbounded_string(0), noMetaLabel, noMetaLabels );
           err( +"array is empty" );
        end if;
     exception when CONSTRAINT_ERROR =>
        err( pl( "constraint_error : index out of range " & identifiers( var_id ).aStorage'first'img & " .. " & identifiers( var_id ).aStorage'last'img ) );
     when STORAGE_ERROR =>
        err( pl( gnat.source_info.source_location & ": internal error : storage error raised when summing array" ) );
     end;
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  end if;
end ParseStatsAverage;

procedure ParseStatsVariance( result : out storage; kind : out identifier ) is
  var_id   : identifier;
  first, last : long_integer;
  len      : long_integer;
  -- array_id : arrayID;
  sum      : numericValue;
  diff     : numericValue;
  mean     : numericValue;
  sum_diff_sq : numericValue;
begin
  kind := universal_t;
  expect( stats_variance_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( +"Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     begin
        first := identifiers( var_id ).aStorage'first;
        last  := identifiers( var_id ).aStorage'last;
        len   := last-first+1;
        sum := 0.0;
        -- a variance on one item will result in divide by zero
        if last > first then
           if metaLabelOk( stats_variance_t, identifiers( var_id ).aStorage( first ) ) then
              result.unitMetaLabel := identifiers( var_id ).aStorage( first ).unitMetaLabel;
              result.policyMetaLabels := identifiers( var_id ).aStorage( first ).policyMetaLabels;
           end if;
           for i in first..last loop
               if metaLabelOk( stats_variance_t, identifiers( var_id ).aStorage( i ) ) then
                 sum := sum + to_numeric( identifiers( var_id ).aStorage( i ).value );
                 result.unitMetaLabel := identifiers( var_id ).aStorage( i ).unitMetaLabel;
                 result.policyMetaLabels := resolveEffectiveMetaLabels(
                     kind,
                     result,
                     identifiers( var_id ).aStorage( i )
                  );
               end if;
           end loop;
           mean := sum / numericValue( len );
           sum_diff_sq := 0.0;
           for i in first..last loop
               -- meta labels assumed to be ok because of previous loop
               diff := to_numeric( identifiers( var_id ).aStorage( i ).value ) - mean;
               sum_diff_sq := sum_diff_sq + diff * diff;
           end loop;
           result.value := to_unbounded_string( sum_diff_sq / numericValue( len-1 ) );
        else
           result := storage'( to_unbounded_string( 0 ), noMetaLabel, noMetaLabels );
           err( +"array is empty or one element" );
        end if;
     exception when CONSTRAINT_ERROR =>
        err( pl( "constraint_error : index out of range " & identifiers( var_id ).aStorage'first'img & " .. " & identifiers( var_id ).aStorage'last'img ) );
     when STORAGE_ERROR =>
        err( pl( gnat.source_info.source_location & ": internal error : storage error raised when calculating variance" ) );
     end;
     -- kind   := identifiers( var_id ).kind;
     kind := identifiers( identifiers( var_id ).kind ).kind;
  end if;
end ParseStatsVariance;

procedure ParseStatsStandardDeviation( result : out storage; kind : out identifier ) is
  var_id   : identifier;
  first, last : long_integer;
  len      : long_integer;
  -- array_id : arrayID;
  sum      : numericValue;
  diff     : numericValue;
  mean     : numericValue;
  sum_diff_sq : numericValue;
begin
  expect( stats_standard_deviation_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( +"Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := identifiers( var_id ).aStorage'first;
     last  := identifiers( var_id ).aStorage'last;
     len   := last-first+1;
     sum := 0.0;
     kind   := identifiers( identifiers( var_id ).kind ).kind;
     -- a deviation on one item will result in divide by zero
     if last > first then
        if metaLabelOk( stats_standard_deviation_t, identifiers( var_id ).aStorage( first ) ) then
           -- if the initial meta label is not set, the resolve will throw an
           -- exception.  So skip the main loops if we cannot access the first
           -- element of the array.
           result.unitMetaLabel := identifiers( var_id ).aStorage( first ).unitMetaLabel;
           result.policyMetaLabels := identifiers( var_id ).aStorage( first ).policyMetaLabels;
           for i in first..last loop
               if metaLabelOk( stats_standard_deviation_t, identifiers( var_id ).aStorage( i ) ) then
                  sum := sum + to_numeric( identifiers( var_id ).aStorage( i ).value );
                  result.unitMetaLabel := identifiers( var_id ).aStorage( i ).unitMetaLabel;
                  result.policyMetaLabels := resolveEffectiveMetaLabels(
                     kind,
                     result,
                     identifiers( var_id ).aStorage( i )
                  );
               end if;
           end loop;
           mean := sum / numericValue( len );
           sum_diff_sq := 0.0;
           for i in first..last loop
               diff := to_numeric( identifiers( var_id ).aStorage( i ).value) - mean;
               sum_diff_sq := sum_diff_sq + diff * diff;
           end loop;
        end if;
        result.value := to_unbounded_string( sqrt( sum_diff_sq / numericValue( len-1 ) ) );
     else
        result := storage'( to_unbounded_string( 0 ), noMetaLabel, noMetaLabels );
        err( +"array is empty or one element" );
     end if;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsStandardDeviation;

-------------------------------------------------------------------------------
-- Housekeeping
-------------------------------------------------------------------------------

procedure StartupStats is
begin
  declareNamespace( "stats" );
  declareFunction( stats_average_t, "stats.average", ParseStatsAverage'access );
  declareFunction( stats_max_t, "stats.max", ParseStatsMax'access );
  declareFunction( stats_min_t, "stats.min", ParseStatsMin'access );
  declareFunction( stats_standard_deviation_t, "stats.standard_deviation", ParseStatsStandardDeviation'access );
  declareFunction( stats_sum_t, "stats.sum", ParseStatsSum'access );
  declareFunction( stats_variance_t, "stats.variance", ParseStatsVariance'access );
  declareNamespaceClosed( "stats" );
end StartupStats;

procedure ShutdownStats is
begin
  null;
end ShutdownStats;

end parser_stats;
