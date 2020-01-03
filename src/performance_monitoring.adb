------------------------------------------------------------------------------
-- Performance Monitoring                                                   --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2020 Free Software Foundation              --
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

with ada.calendar,
     ada.text_io;

use ada.calendar,
    ada.text_io;

package body performance_monitoring is

type hash_integer is mod 2**32;

function String_Hash( key : unbounded_string ) return Hash_Position is
-- FVN hash (see parser_numerics)
  hash   : hash_integer := 16#711c9dc5#; -- was 16#8
  k      : hash_integer;
  limit  : hash_integer;
begin
  limit := hash_integer( hash_position'last );
  for data in 1..length(key)-3 loop
      k := character'pos( element(key, data) ) +
           character'pos( element(key, data+1) ) * 256 +     -- 8
           character'pos( element(key, data+2) ) * 65536 +   -- 16
           character'pos( element(key, data+3) ) * 16777216; -- 24
       hash := hash xor k;
       hash := hash * 16#01000193#;
  end loop;
  hash := (hash mod limit);
  return hash_position( hash );
end String_Hash;

-- PUT PERF SUMMARY
--
-- Display static code metrics and performance stats
-----------------------------------------------------------------------------

procedure put_perf_summary is
  realTime  : duration;
  rate      : natural;
  linesBlock: natural;
  commentsBlock : natural;
  branchesBlock : natural;
begin

  -- Summarize Results

  realTime := perfStats.endTime - perfStats.startTime;
  rate     := natural( duration( perfstats.lineCnt ) / realTime );
  begin
    linesBlock := natural( perfStats.loc / perfStats.numBlocks );
  exception when constraint_error =>
    linesBlock := 0;
  end;
  begin
    commentsBlock := natural( perfStats.numComments / perfStats.numBlocks );
  exception when constraint_error =>
    commentsBlock := 0;
  end;
  begin
    branchesBlock := natural( perfStats.numBranches / perfStats.numBlocks );
  exception when constraint_error =>
    branchesBlock := 0;
  end;

  -- Static Metrics
  --
  -- These are metrics gathered from looking at the source code without
  -- executing it.  That is, SparForte is in syntax checking mode.  This
  -- includes lines of code, number of functions, etc.

  new_line;
  put_line( "Static Metrics" );
  new_line;
  put( "LOC:        " );
  put_line( perfStats.loc'img );
  put( "Procedures: " );
  put_line( perfStats.numProcs'img );
  put( "Functions:  " );
  put_line( perfStats.numFuncs'img );
  put( "Blocks:     " );
  put_line( perfStats.numBlocks'img );
  put( "Structure:  " );
  put( linesBlock'img );
  put_line( " Lines/Block" );
  put( "Comments:   " );
  put_line( perfStats.numComments'img );
  put( "Commenting: " );
  put( commentsBlock'img );
  put_line( " Lines/Block" );
  put( "Branches:   " );
  put_line( perfStats.numBranches'img );
  put( "Branching:  " );
  put( branchesBlock'img );
  put_line( " Branches/Block" );

  -- Performance Stats
  --
  -- Only available when the script executes
  -- That is, SparForte is not in syntax check mode.

  if not syntaxOpt then
     new_line;
     put_line( "Performance Stats" );
     new_line;
     put( "Lines Read: " );
     put_line( perfStats.lineCnt'img );
     put( "Run Time:   " );
     put( realTime'img );
     put_line( " Secs" );
     put( "Throughput: " );
     put( rate'img );
     put_line( " Lines/Sec" );
  end if;
end put_perf_summary;

end performance_monitoring;

