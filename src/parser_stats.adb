------------------------------------------------------------------------------
-- Stats Package Parser                                                     --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2016 Free Software Foundation              --
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
     parser,
     parser_aux;
use  ada.numerics.long_elementary_functions,
     bush_os,
     string_util,
     world,
     parser,
     parser_aux;

--with ada.text_io; use ada.text_io;

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

procedure ParseStatsMax( f : out unbounded_string; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  -- array_id : arrayID;
  max : long_float;
  max_string : unbounded_string;
begin
  expect( stats_max_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     begin
        first := identifiers( var_id ).avalue'first;
        last  := identifiers( var_id ).avalue'last;
        if last > first then
           max_string := identifiers( var_id ).avalue( first );
           max := to_numeric( max_string );
           for i in first+1..last loop
               if to_numeric( identifiers( var_id ).avalue( i ) ) > max then
                  max_string := identifiers( var_id ).avalue( i );
                  max := to_numeric( max_string );
               end if;
           end loop;
           f := max_string;
        else
           f := to_unbounded_string( 0 );
           err( "array is empty" );
        end if;
     exception when CONSTRAINT_ERROR =>
        err( "constraint_error : index out of range " & identifiers( var_id ).avalue'first'img & " .. " & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when maxing array" );
     end;
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsMax;

procedure ParseStatsMin( f : out unbounded_string; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  -- array_id : arrayID;
  min : long_float;
  min_string : unbounded_string;
begin
  expect( stats_min_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     begin
        first := identifiers( var_id ).avalue'first;
        last  := identifiers( var_id ).avalue'last;
        if last > first then
           min_string := identifiers( var_id ).avalue( first );
           min := to_numeric( min_string );
           for i in first+1..last loop
               if to_numeric( identifiers( var_id ).avalue( i ) ) < min then
                  min_string := identifiers( var_id ).avalue( i );
                  min := to_numeric( min_string );
               end if;
           end loop;
           f := min_string;
        else
           f := to_unbounded_string( 0 );
           err( "array is empty" );
        end if;
     exception when CONSTRAINT_ERROR =>
        err( "constraint_error : index out of range " & identifiers( var_id ).avalue'first'img & " .. " & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when minning array" );
     end;
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsMin;

procedure ParseStatsSum( f : out unbounded_string; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  -- array_id : arrayID;
  sum : long_float;
begin
  expect( stats_sum_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     begin
        first := identifiers( var_id ).avalue'first;
        last  := identifiers( var_id ).avalue'last;
        sum := 0.0;
        if last > first then
           for i in first..last loop
               sum := sum + to_numeric( identifiers( var_id ).avalue( i ) );
           end loop;
           f := to_unbounded_string( sum );
        else
           f := to_unbounded_string( 0 );
           err( "array is empty" );
        end if;
     exception when CONSTRAINT_ERROR =>
        err( "constraint_error : index out of range " & identifiers( var_id ).avalue'first'img & " .. " & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when minning array" );
     end;
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsSum;

procedure ParseStatsAverage( f : out unbounded_string; kind : out identifier ) is
  var_id : identifier;
  first, last : long_integer;
  len    : long_integer;
  --array_id : arrayID;
  sum : long_float;
begin
  expect( stats_average_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     --array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     begin
        first := identifiers( var_id ).avalue'first;
        last  := identifiers( var_id ).avalue'last;
        len   := last-first+1;
        sum := 0.0;
        if last > first then
           for i in first..last loop
               sum := sum + to_numeric( identifiers( var_id ).avalue( i ) );
           end loop;
           f := to_unbounded_string( sum / long_float( len ) );
        else
           f := to_unbounded_string(0);
           err( "array is empty" );
        end if;
     exception when CONSTRAINT_ERROR =>
        err( "constraint_error : index out of range " & identifiers( var_id ).avalue'first'img & " .. " & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when summing array" );
     end;
     kind   := identifiers( identifiers( var_id ).kind ).kind;
  elsif syntax_check then
     kind := universal_t; -- type is not known during syntax check
  end if;
end ParseStatsAverage;

procedure ParseStatsVariance( f : out unbounded_string; kind : out identifier ) is
  var_id   : identifier;
  first, last : long_integer;
  len      : long_integer;
  -- array_id : arrayID;
  sum      : long_float;
  diff     : long_float;
  mean     : long_float;
  sum_diff_sq : long_float;
begin
  expect( stats_variance_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     begin
        first := identifiers( var_id ).avalue'first;
        last  := identifiers( var_id ).avalue'last;
        len   := last-first+1;
        sum := 0.0;
        if last > first then
           for i in first..last loop
               sum := sum + to_numeric( identifiers( var_id ).avalue( i ) );
           end loop;
           mean := sum / long_float( len );
           sum_diff_sq := 0.0;
           for i in first..last loop
               diff := to_numeric( identifiers( var_id ).avalue( i ) ) - mean;
               sum_diff_sq := sum_diff_sq + diff * diff;
           end loop;
           f := to_unbounded_string( sum_diff_sq / long_float( len-1 ) );
        else
           f := to_unbounded_string( 0 );
           err( "array is empty" );
        end if;
     exception when CONSTRAINT_ERROR =>
        err( "constraint_error : index out of range " & identifiers( var_id ).avalue'first'img & " .. " & identifiers( var_id ).avalue'last'img );
     when STORAGE_ERROR =>
        err( "internal error : storage error raised when calculating variance" );
     end;
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
  -- array_id : arrayID;
  sum      : long_float;
  diff     : long_float;
  mean     : long_float;
  sum_diff_sq : long_float;
begin
  expect( stats_standard_deviation_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if not (class_ok( var_id, varClass ) and identifiers( var_id ).list) then
     err( "Array expected" );
  end if;
  if uniTypesOK( identifiers( var_id ).kind, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( var_id ).value ) );
     first := identifiers( var_id ).avalue'first;
     last  := identifiers( var_id ).avalue'last;
     len   := last-first+1;
     sum := 0.0;
     if last > first then
        for i in first..last loop
            sum := sum + to_numeric( identifiers( var_id ).avalue( i ) );
        end loop;
        mean := sum / long_float( len );
        sum_diff_sq := 0.0;
        for i in first..last loop
            diff := to_numeric( identifiers( var_id ).avalue( i ) ) - mean;
            sum_diff_sq := sum_diff_sq + diff * diff;
        end loop;
        f := to_unbounded_string( sqrt( sum_diff_sq / long_float( len-1 ) ) );
     else
        f := to_unbounded_string( 0 );
        err( "array is empty" );
     end if;
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
