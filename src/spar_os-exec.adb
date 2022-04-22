------------------------------------------------------------------------------
-- Spar OS/Exec - Fork off an Operating System Command                      --
-- This version is for UNIX/Linux Commands                                  --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2017 Free Software Foundation              --
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

with ada.text_io;
use ada.text_io;

with unchecked_deallocation;
with gnat.source_info;


package body spar_os.exec is


-- C String Version of Arguments

type C_argumentList is array( natural range <> ) of System.Address;

package argConvert is new System.Address_To_Access_Conversions( argumentPtr );
-- convert between argumentPtr and C addresses (to determine null address)

-----------------------------------

procedure freeArgList is new unchecked_deallocation( argumentList,
  argumentListPtr );
-- Deallocate an argument list (but not the elements within it)

procedure free( ap : in out argumentListPtr ) is
  -- deallocate an entire arugment list
begin
  for i in ap.all'range loop
      free( ap.all(i) );
  end loop;
  freeArgList( ap );
end free;

function Convert_To_C_Args( path_address : system.address; args : argumentList )
   return C_argumentList is
   C_Args : C_argumentList( 0..args'length+1 );
-- Create a C strings argument list from a null-terminated Ada string
-- argument list.  The final address in the C list is a null pointer.
-- This is the parameter syntax for exec().
-- There is no additional memory allocated (ie. no need to deallocate
-- the C list).
begin
   C_Args( 0 ) :=  path_address;
   for i in args'range loop
       C_Args( i ) :=  args( i ).all'address;
   end loop;
   C_Args( C_Args'last ) := ArgConvert.To_Address( null );
   return C_Args;
end Convert_To_C_Args;

procedure spawn( fullPath : unbounded_string; ap : argumentListPtr;
   status : out integer; noReturn : boolean := false ) is
-- Run a command.  If noreturn is true, the program never returns from
-- this call.  Otherwise, a new process is forked and the program waits
-- for the command to finish.
     path       : string := To_String( fullPath ) & ASCII.NUL;
     myPID      : aPID;                                    -- fork process id
     waitResult : aPID;                                    -- wait result
     execResult : integer;                                 -- exec result
begin
   if not C_is_executable_file( path ) then
      status := 192;
      return; -- whatever errno is created by CIEF
   end if;
   C_reset_errno;                                          -- clear any error first

   if noreturn then                                    -- no fork needed?
         execResult := execv( Path,
             Convert_To_C_Args( Path'address, ap.all )'address );
      return;
   end if;

   myPID := fork;                                      -- create new process
   if myPID < 0 then                                   -- error?  it's
      return;                                          -- returned in errno
   elsif myPID = 0 then

      -- This is the child process.  It should never return.

      execResult := execv( Path,
          Convert_To_C_Args( Path'address, ap.all )'address );
      if execResult < 0 then
         put_line( standard_error, gnat.source_info.source_location &
             ": spar_os.exec: failed to execute command, " &
             "error #" & C_errno'img );
      else
         put_line( standard_error, gnat.source_info.source_location &
             ": spar_os.exec: execv unexpectedly returned" );
      end if;

      raise PROGRAM_ERROR; -- this process must die

      -- child process block

   else

      -- This is parent process.  It waits until the child is finished.
      -- myPID is the process id of the child.

      loop
        waitpid( waitResult, myPID, status, 0 );       -- wait for child
        if waitResult = -1 and C_errno = EINTR then    -- interrupted syscall?
           null;                                       -- continue
        elsif waitResult = -1 then                     -- some other error?
           return;                                     -- return it
        elsif waitResult > 0 then                      -- a pid?
           C_reset_errno;                              -- no error
           exit;                                       -- waiting done
        end if;                                        -- otherwise continue
      end loop;
   end if;

end spawn;

end spar_os.exec;
