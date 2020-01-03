------------------------------------------------------------------------------
-- Pipes, Job Control and Running Programs                                  --
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

with ada.strings.unbounded,
    spar_os.exec,
    world;
use ada.strings.unbounded,
    spar_os,
    spar_os.exec,
    world;


package jobs is


-----------------------------------------------------------------------------
-- Command Hash
-----------------------------------------------------------------------------

procedure clearCommandHash;
-- Clear the command pathname hash table


-----------------------------------------------------------------------------
-- Run the specified command using the given arguments.
--   Cmd is the pathname to run (it doesn't have to be a full path)
--   ap is the list of arguments for the program
--   background is true if the program should run in the background
--   cache is true if the program path should be cached (not implemented)
-----------------------------------------------------------------------------

-- Basic run (no pipeline)

procedure run( cmd : unbounded_string;
   paramToken : identifier;
   ap : argumentListPtr;
   success : out boolean;
   background : boolean := false;
   cache : boolean := true );

-- Run with pipelines

procedure run_inpipe( cmd : unbounded_string;
   paramToken : identifier;
   ap : argumentListPtr;
   success : out boolean;
   background : boolean := false;
   cache : boolean := true;
   pipeStderr : boolean := false );
-- Run this program, attaching it to the the previous program executed
-- with run_frompipe or run_bothpipe, terminating the pipeline.

procedure run_frompipe( cmd : unbounded_string;
   paramToken : identifier;
   ap : argumentListPtr;
   success : out boolean;
   background : boolean := false;
   cache : boolean := true );
-- Run this program, starting a new pipeline, with an outgoing pipe to
-- be attached to the another program using run_inpipe or run_bothpipe.

procedure run_bothpipe( cmd : unbounded_string;
   paramToken : identifier;
   ap : argumentListPtr;
   success : out boolean;
   background : boolean := false;
   cache : boolean := true;
   pipeStderr : boolean := false );
-- Run this program which exists in in the middle of a pipeline, neither
-- the first program nor the last.


-----------------------------------------------------------------------------
-- pipeline recovery
-----------------------------------------------------------------------------

procedure closePipeline;
-- close off open pipes


-----------------------------------------------------------------------------
-- Waiting for Running Programs
-----------------------------------------------------------------------------

procedure wait4LastJob;
-- wait for last background command to finish

procedure wait4Children;
-- wait for completed child processes

procedure updateJobStatus;
-- check all jobs for their current status

procedure putJobList;
-- display the job list

end jobs;

