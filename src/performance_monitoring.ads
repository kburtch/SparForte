------------------------------------------------------------------------------
-- Performance Monitoring                                                   --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2019 Free Software Foundation              --
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

with ada.strings.unbounded,
     ada.calendar,
     Gnat.Dynamic_HTables,
     world;
use  ada.strings.unbounded,
     ada.calendar,
     world;

package performance_monitoring is


-- Hash Table (not used yet)

type Hash_Position is new integer range 0.. 65_535;

function String_Hash( key : unbounded_string ) return Hash_Position;

package dynamic_string_hash_tables is
   new Gnat.Dynamic_HTables.Simple_HTable(
      Hash_Position,
      unbounded_string,
      null_unbounded_string,
      unbounded_string,
      String_Hash,
      "="
);

-- Perf Stats (Performance Stats)
--
-- This is used to record static code analysis results and performance
-- statistics.  These can be used to evaluate designs.
-- See jdepend/pdepend for examples of what should be here.
--
-- It would also be good to add code coverage here.

type performanceStats is record
  startTime : time;
  endTime   : time;
  lineCnt   : line_count := 0;        -- executed lines
  loc       : natural := 0;           -- lines of code
  numProcs  : natural := 0;           -- number of procedures
  numFuncs  : natural := 0;           -- number of functions
  numBlocks : natural := 0;           -- number of begins
  -- code coverage (not done yet)
  lines     : dynamic_string_hash_tables.Instance;
end record;

perfStats : performanceStats;

-- PUT PERF SUMMARY
--
-- Display static code analysis results and performance summary
-----------------------------------------------------------------------------

procedure put_perf_summary;

end performance_monitoring;

