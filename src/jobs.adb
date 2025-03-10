------------------------------------------------------------------------------
-- Pipelines, Job Control and Running Programs                              --
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

with interfaces.c,
    ada.text_io,
    ada.strings.fixed,
    gnat.source_info,
    compiler,
    scanner.communications,
    builtins.help,
    pegasoft.strings,
    pegasoft.user_io,
    parser_aux, -- for DoQuit
    pegasoft.gen_list,
    world;
use interfaces.c,
    ada.text_io,
    ada.strings.fixed,
    pegasoft.user_io,
    pegasoft.strings,
    builtins,
    compiler,
    scanner,
    scanner.communications,
    parser_aux,
    world;

package body jobs is


-- Pipe Stack
--
-- We need to keep track of the list of pipes (maximum 20)

type aPipeStack is array(1..20) of aPipe;
pipeStack    : aPipeStack;
pipeStackTop : natural := 0;


-- Jobs List
--
-- Background tasks (jobs started with '&' or in a pipeline)
-- are stored in a linked list called "jobs".  When a job
-- finishes, it is removed from the list.

type aJobStatus is ( running, stopped, unknown );
type aJob is record
   pid    : aPID;               -- process id of the background task
   cmd    : unbounded_string;   -- the command name
   status : aJobStatus;         -- status as of last check
   quiet  : boolean := false;   -- true => no job control messages
end record;

function ">=" ( left, right : aJob ) return boolean is
  -- sorting criteria (not actually used by BUSH but needed
  -- for instantiation of the generic gen_list package).
begin
  return left.pid >= right.pid;
end ">=";

package jobList is new pegasoft.gen_list( aJob, "=", ">=" );
jobs : jobList.List;            -- our list of jobs


-----------------------------------------------------------------------------
--  FIND CMD PATH
--
-- Determine the location of the command "cmd".  If a path
-- was specified in cmd, check for and expand a leading tilde.
-- If no path, then we need to do a search of the PATH variable.
-----------------------------------------------------------------------------

procedure findCmdPath( cmd : unbounded_string; fullpath : out unbounded_string ) is

   slashPos : integer;          -- slashes in the command path
   colonPos : integer;          -- colons in the PATH variable
   pathId   : identifier;       -- PATH in identifiers

   cmdpath  : unbounded_string; -- working copy of the cmd path
   prefixes : unbounded_string; -- PATH prefix list
   prefix   : unbounded_string; -- prefix being tested
begin
   cmdPath := cmd;
   slashPos := index( cmd, "/" );
   if slashPos > 0 then
      if Element( cmdPath, 1 ) = '~' then
         Delete( cmdPath, 1, 1 );
         Insert( cmdPath, 1, to_string( current_working_directory ) );
      end if;
      fullPath := cmdPath;
      if not C_is_executable_file( to_string( fullPath ) & ASCII.NUL ) then
         fullPath := null_unbounded_string;
      end if;
   else
      findIdent( to_unbounded_string( "PATH" ), pathId );
      if pathId /= eof_t then
         prefixes := identifiers( pathId ).value.all;
         while length( prefixes ) > 0 loop
            colonPos := Index( prefixes, ":" );
            if colonPos = 0 then
               prefix := prefixes;
               prefixes := null_unbounded_string;
            else
               prefix := to_unbounded_string(
                  slice( prefixes, 1, colonPos-1 ) );
               Delete( prefixes, 1, colonPos );
            end if;
            if length( prefix ) = 0 then
               fullPath := cmdPath;
            elsif Element( prefix, length( prefix ) ) = '/' then
               fullPath := prefix & cmdPath;
            else
               fullPath := prefix & '/' & cmdPath;
            end if;
            if C_is_executable_file( to_string( fullPath ) & ASCII.NUL ) then
               exit;
            else
               fullPath := null_unbounded_string;
            end if;
         end loop;
      end if;
   end if;
end findCmdPath;


-----------------------------------------------------------------------------
--  COMMAND HASH TABLE
--
-- This table stores the location of external operating system commands.  If
-- the command path is stored in this table, no search is required to find it.
-----------------------------------------------------------------------------

cmdHashMax : constant positive := 999;
type cmdHashEntry is record
     cmd      : unbounded_string;
     fullPath : unbounded_string;
     builtin  : boolean;
end record;
pragma pack( cmdHashEntry );

type cmdHashArray is array( 1..cmdHashMax ) of cmdHashEntry;
pragma pack( cmdHashArray );

cmdHash    : cmdHashArray;


-----------------------------------------------------------------------------
--  HASH OF
--
-- Simple hash function.  Given a string, compute a position to store it in
-- the command hash table.
-----------------------------------------------------------------------------

function hashOf( cmd : unbounded_string ) return natural is
  sum : natural := 0;
begin
  for i in 1..length( cmd ) loop
      sum := ( ( sum + (character'pos( element( cmd, i ) ) * i) )
             mod cmdHashMax )+1;
  end loop;
  return sum;
end hashOf;


-----------------------------------------------------------------------------
--  ADD COMMAND HASH
--
-- Add the path to a command to the command hash table.  If there is a hash
-- collision, ignore the new entry.
-----------------------------------------------------------------------------

procedure addCommandHash( cmd, full : unbounded_string; builtin : boolean ) is
  bin : natural := 0;
begin
  bin := hashOf( cmd );
  if builtin and length( cmdHash( bin ).fullPath ) /= 0 then
    put_line_retry( standard_error, gnat.source_info.source_location & ": internal error: builtin command hash clash for builtin "
        & toEscaped( cmd ) );
  else
    cmdHash( bin ).cmd := cmd;
    cmdHash( bin ).fullPath := full;
    cmdHash( bin ).builtin := builtin;
  end if;
end addCommandHash;


-----------------------------------------------------------------------------
--  GET HASH INFO
--
-- Lookup a command in the hash table.  Returns null information if no entry.
-----------------------------------------------------------------------------

procedure getHashInfo( cmd : unbounded_string; hte : out cmdHashEntry ) is
begin
  hte := cmdHash( hashOf( cmd ) );
  if hte.cmd /= cmd then
     hte.cmd := null_unbounded_string;
     hte.fullPath := null_unbounded_string;
     hte.builtin := false;
  end if;
end getHashInfo;


-----------------------------------------------------------------------------
--  CLEAR COMMAND HASH
--
-- Erase the record of the paths to commands.  Add the BUSH built-ins to the
-- list, marking them as such.
-----------------------------------------------------------------------------

procedure clearCommandHash is
begin
  for i in cmdHashArray'range loop
      cmdHash( i ) := cmdHashEntry'( null_unbounded_string,
                                     null_unbounded_string,
                                     false );
  end loop;
  addCommandHash( to_unbounded_string( "cd" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "clear" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "env" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "help" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "history" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "jobs" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "pwd" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "step" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "trace" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "umask" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "unset" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "wait" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "alter" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "delete" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "insert" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "select" ),
      null_unbounded_string, true );
  addCommandHash( to_unbounded_string( "update" ),
      null_unbounded_string, true );
end clearCommandHash;


-----------------------------------------------------------------------------
--  SHOW RUN ERROR
--
-- user-friendly error replies.  Probably could be even better with
-- stat() and special messages.
-----------------------------------------------------------------------------

procedure showRunError( code : integer ) is
begin
  case code is
  when E2BIG   => err( contextNotes => contextInCommand,
                       subjectNotes => subjectOS,
                       reason => +"has reached its",
                       obstructorNotes => em( "arguments limit" )
                  );
  when ENOEXEC => err( contextNotes => contextInCommand,
                       subjectNotes => pl( qp( "this file" ) ),
                       reason => +"cannot be",
                       obstructorNotes => +"an executable program"
                  );
  when EMLINK  => err( contextNotes => contextInCommand,
                       subjectNotes => pl( qp( "the pathanme" ) ),
                       reason => +"is a link with",
                       obstructorNotes => em( "a circular path" )
                  );
  when ENFILE  => err( contextNotes => contextInCommand,
                       subjectNotes => subjectOS,
                       reason => +"has reached its",
                       obstructorNotes => em( "file limit" )
                  );
  when EMFILE  => err( contextNotes => contextInCommand,
                       subjectNotes => subjectOS,
                       reason => +"has reached its",
                       obstructorNotes => em( "open file limit" )
                  );
  when EINVAL  => err( contextNotes => contextInCommand,
                       subjectNotes => subjectOS,
                       reason => +"cannot run",
                       obstructorNotes => em( "a corrupt file" )
                  );
  when EISDIR  => err( contextNotes => contextInCommand,
                       subjectNotes => subjectScriptInterpreter,
                       reason => +"was a path to",
                       obstructorNotes => em( "a directory" )
                  );
  when ELIBBAD => err( contextNotes => contextInCommand,
                       subjectNotes => subjectScriptInterpreter,
                       reason => +"could not be run",
                       obstructorNotes => nullMessageStrings
                  );
  when EACCES  => err( contextNotes => pl( "At " & gnat.source_info.source_location &
                           " while " ) & contextInCommand,
                       subjectNotes => pl( qp( "the run() syscall family" ) ),
                       reason => +"had an internal error because they",
                       obstructorNotes => em( "could not access the program" )
                  );
  when EPERM   => err( contextNotes => pl( "At " & gnat.source_info.source_location &
                           " while " ) & contextInCommand,
                       subjectNotes => pl( qp( "the run() syscall family" ) ),
                       reason => +"had an internal error because they",
                       obstructorNotes => em( "do not have permission to run the program" )
                  );
  when ENOTDIR => err( contextNotes => pl( "At " & gnat.source_info.source_location &
                           " while " ) & contextInCommand,
                       subjectNotes => pl( qp( "the run() syscallfamily" ) ),
                       reason => +"had an internal error because",
                       obstructorNotes => em( "the pathname was good when a directory in it was wrong" )
                  );
  when ECHILD  => err( contextNotes => pl( "At " & gnat.source_info.source_location &
                           " while " ) & contextInCommand,
                       subjectNotes => pl( qp( "the run() syscall family" ) ),
                       reason => +"had an internal error because",
                       obstructorNotes => em( "spawn() failed to catch an ECHILD signal" )
                  );
  when ENOENT  => err( contextNotes => pl( "At " & gnat.source_info.source_location &
                           " while " ) & contextInCommand,
                       subjectNotes => +"spawn()",
                       reason => +"had an internal error because",
                       obstructorNotes => em( "the expected file was missing" )
                  );
  when 0       => err( contextNotes => pl( "At " & gnat.source_info.source_location &
                           " while " ) & contextInCommand,
                       subjectNotes => pl( qp( "the run() syscall family" ) ),
                       reason => +"had an internal error because",
                       obstructorNotes => em( "they reported an error when the error code has no error" )
                  );
 -- New to more recent Linux
  when EIO     => err( contextNotes => contextInCommand,
                       subjectNotes => subjectOS,
                       reason => +"could not run because the file was damaged by",
                       obstructorNotes => em( "an I/O error" )
                  );
  --when ETXTBSY => err( "The file is open for writing" );
  --when ELOOP   => err( "Too many symbolic link dereferences" );
  when ENAMETOOLONG => err( contextNotes => contextInCommand,
                       subjectNotes => subjectOS,
                       reason => +"could not run the file because",
                       obstructorNotes => em( "the filename is too long" )
                  );
                  --err( +"The filename is too long" );
  when EAGAIN  => err( contextNotes => contextInCommand,
                       subjectNotes => subjectOS,
                       reason => +"has reached its",
                       obstructorNotes => em( "esource limit" )
                  );
                  -- err( +"The process may be above its resource limit" );
  when EFAULT  => err( contextNotes => pl( "At " & gnat.source_info.source_location &
                           " while " ) & contextInCommand,
                       subjectNotes => +"spawn()",
                       reason => +"had an internal error because",
                       obstructorNotes => em( "arguments or environment" &
                       " pointers point outside address space" )
                  );
  when others  => -- out of memory, IO error
       err( contextNotes => contextInCommand,
                       subjectNotes => pl( qp( "the run() syscall family" ) ),
                       reason => +"had an error regarding",
                       obstructorNotes => getEmOSError( code )
                  );
  end case;
end showRunError;


-----------------------------------------------------------------------------
--  ERR CMD NOT FOUND
--
-- To explain why something was not found, check to see if it could be a
-- procedure.  Also check to see if it was hashed or not.
-----------------------------------------------------------------------------

procedure err_cmd_not_found( cmd : unbounded_string ) is
  id : identifier;
  che : cmdHashEntry;
  prefixStr : unbounded_string;
  -- length hard-coded here and in builtins.adb
  buffer : string(1..4096);
  current_working_directory : unbounded_string;
begin
  -- Get the current working directory
  -- It is possible a file path might use confidential information so we
  -- won't show it in production mode.

  --if not boolean( MaintenanceOpt ) then
  C_reset_errno;
  getcwd( buffer, buffer'length );
  if C_errno = 0 then
     current_working_directory :=
        toEscaped( to_unbounded_string( buffer( buffer'first..index( buffer, ASCII.NUL & "" ) - 1 ) ) ) ;
     current_working_directory := "in " & to_unbounded_string( toSecureData( to_string( current_working_directory ) ) );
  end if;
  --end if;

  -- Check to see if there's a procedure with the same name

  findIdent( cmd, id );
  if id /= eof_t then
     prefixStr := to_unbounded_string( "the command " );
  else
     prefixStr := to_unbounded_string( "a procedure or command " );
  end if;

  -- Check the command hash

  getHashInfo( cmd, che );

  if length( che.fullPath ) /= 0 or che.builtin then
     err(
        contextNotes => unb_pl( current_working_directory ),
        subjectNotes => unb_pl( prefixStr ) & pl( "'" ) & em_esc( cmd ) & pl( "'" ),
        reason       => +"was " & em( "not found" ),
        obstructorNotes => nullMessageStrings,
        remedy       => +"it was moved and is no longer in the PATH directory list, or you need to clear the command hash by restart SparForte or pragma no_command_hash"
     );
  elsif id /= eof_t then
     err(
        contextNotes => unb_pl( current_working_directory ),
        subjectNotes => unb_pl( prefixStr ) & pl( "'" ) & em_esc( cmd )  & pl( "'" ),
        reason       => +"was " & em( "not found" ),
        obstructorNotes => nullMessageStrings,
        remedy       => +"it was spelled wrong or a directory is missing in the PATH directory list"
     );
  else
     err(
        contextNotes => unb_pl( current_working_directory ),
        subjectNotes => unb_pl( prefixStr ) & pl( "'" ) & em_esc( cmd ) & pl( "'" ),
        reason       => +"was " & em( "not found" ),
        obstructorNotes => nullMessageStrings,
        remedy       => +"it was spelled wrong or a directory is missing in the PATH directory list"
     );
  end if;
end err_cmd_not_found;


-----------------------------------------------------------------------------
--  SPAWN COMMAND OR RUN BUILTIN
--
-- Run an external command or a BUSH built-in.  When the command is finished,
-- control will not be returned.
-----------------------------------------------------------------------------

procedure spawnCommandOrRunBuiltin(
   cmd        : unbounded_string;
   fullPath   : unbounded_string;
   ap         : argumentListPtr;                            -- C string args
   noReturn   : boolean;
   success    : out boolean ) is                            -- true on success

   procedure noReturnForBuiltin is
   begin
     if noReturn then
        -- inputMode := breakout;                  -- pretend we're in breakout
        -- error_found := true;                    -- suppress errors
        DoQuit;
     end if;
   end noReturnForBuiltin;

   status : integer;

begin
  success := true;
  if    cmd = "cd" then      builtins.cd( ap );            noReturnForBuiltin;
  elsif cmd = "clear" then   builtins.clear( ap );         noReturnForBuiltin;
  elsif cmd = "env" then     builtins.env( ap );           noReturnForBuiltin;
  elsif cmd = "help" then    builtins.help.help( ap );     noReturnForBuiltin;
  elsif cmd = "history" then builtins.do_history( ap );    noReturnForBuiltin;
  elsif cmd = "jobs" then    builtins.jobs( ap );          noReturnForBuiltin;
  elsif cmd = "pwd" then     builtins.pwd( ap );           noReturnForBuiltin;
  elsif cmd = "step" then    builtins.step( ap );          noReturnForBuiltin;
  elsif cmd = "trace" then   builtins.do_trace( ap );      noReturnForBuiltin;
  elsif cmd = "umask" then   builtins.umask( ap );         noReturnForBuiltin;
  elsif cmd = "unset" then   builtins.unset( ap );         noReturnForBuiltin;
  elsif cmd = "wait" then    builtins.wait( ap );          noReturnForBuiltin;
  elsif cmd = "alter" then   builtins.alter( ap );         noReturnForBuiltin;
  elsif cmd = "delete" then  builtins.delete( ap );        noReturnForBuiltin;
  elsif cmd = "insert" then  builtins.insert( ap );        noReturnForBuiltin;
  elsif cmd = "select" then  builtins.SQLselect( ap );     noReturnForBuiltin;
  elsif cmd = "update" then  builtins.update( ap );        noReturnForBuiltin;
  else

   -- Got here?  Must be an external command

   if restriction_no_external_commands then
      err( contextNotes => contextInCommand,
           subjectNotes => unb_em( cmd ),
           reason => +"looks like an external command that",
           obstructorNotes => +"could not be run",
           remedy => +"there is typing mistake or external commands not allowed with " &
              em( "restriction( no_external_commands )" ),
           seeAlso => seePragmas
      );
      return;
   end if;
     C_reset_errno;                                             -- assume OK
     spawn( fullPath, ap, status, noreturn => noReturn );
     last_status := aStatusCode( C_WEXITSTATUS( status ) );
     -- should never return, but just in case...
     if C_errno > 0 then
        showRunError( C_errno );
        success := false;
     end if;
     if noReturn then
       done := true;
       exit_block := true;
     end if;
  end if;
end spawnCommandOrRunBuiltin;


-----------------------------------------------------------------------------
--  RUN
--
-- Run an external command or a Spar built-in without any pipeline.  If
-- background is true, run the program in the background.
-----------------------------------------------------------------------------

procedure run( cmd : unbounded_string;
   ap : argumentListPtr;
   success : out boolean;
   background : boolean := false ) is
   fullpath : unbounded_string;
   che  : cmdHashEntry;

   myPID    : aPID;
   newJob   : aJob;
begin

   -- Locate the command

   getHashInfo( cmd, che );
   fullPath := che.fullPath;
   if length( che.fullPath ) = 0 and not che.builtin then
      findCmdPath( cmd, fullPath );
      if not no_command_hash and then length( fullPath ) > 0 then
         addCommandHash( cmd, fullPath, builtin => false );
      end if;
   end if;

   if length( fullPath ) = 0 and not che.builtin then
      cmdpos := cmdpos - 1;
      err_cmd_not_found( cmd );
      Success := false;
   else
      if Trace then
         put_trace( to_string( fullPath ) );
      end if;
      if background then
         myPID := fork;
         if myPID < 0 then
            err( contextNotes => contextInCommand,
                 subjectNotes => subjectOS,
                 reason => +"is unable to run the command because",
                 obstructorNotes => +"it could not start a new process"
            );
            Success := false;
         elsif myPID = 0 then -- child
            spawnCommandOrRunBuiltin(
              cmd, fullPath, ap, noReturn => true, success => success );
         else -- parent
            lastChild := myPID;
            newJob.pid := myPID;
            newJob.cmd := cmd;
            newJob.status := running;
            -- if not input mode, always quiet
            -- GCC 7.4 reports a conversion warning but this is not true
            newJob.quiet := not boolean( inputMode = interactive ) or boolean( inputMode = breakout );
            jobList.Insert( jobs, newJob );
            if not newJob.quiet then
               put_line_retry( cmd & " (" & myPID'img & ") has started" );
            end if;
            Success := true;
         end if;
      else
         spawnCommandOrRunBuiltin(
            cmd, fullPath, ap, noReturn => false, success => success );
      end if;
   end if;
end run;


-----------------------------------------------------------------------------
--  RUN IN-PIPE
--
-- Run an external command or a SparForte built-in at the start of a pipeline.
-- If background is true, run the program in the background.
-----------------------------------------------------------------------------

procedure run_inpipe( cmd : unbounded_string;
   ap : argumentListPtr;
   success : out boolean;
   background : boolean := false;
   pipeStderr : boolean := false ) is

   fullpath : unbounded_string;
   che  : cmdHashEntry;
   myPID    : aPID;
   newJob   : aJob;
   result   : integer;
   closeResult : int;
begin

   -- setup the pipeline

   pipeStackTop := pipeStack'first;
   pipe( result, pipeStack( pipeStackTop ) );
   if result /= 0 then
      err( contextNotes => +"in " & em( to_string( cmd ) ),
           subjectNotes => subjectOS,
           reason => +"cannot create a pipe because pipe() returned",
           obstructorNotes => getEmOSError,
           remedy => +"too many files are open"
      );
      pipeStackTop := pipeStackTop-1;
      Success := false;
      return;
   end if;

   -- Locate the command

   getHashInfo( cmd, che );
   fullPath := che.fullPath;
   if length( che.fullPath ) = 0 and not che.builtin then
      findCmdPath( cmd, fullPath );
      if not no_command_hash and then length( fullPath ) > 0 then
         addCommandHash( cmd, fullPath, builtin => false );
      end if;
   end if;

   -- Run the Command

   if length( fullPath ) = 0 and not che.builtin then
      cmdpos := cmdpos - 1;
      err_cmd_not_found( cmd );
      Success := false;
   else
      if background then
         myPID := fork;                      -- start a new process
         if myPID < 0 then                   -- failed?
            err( contextNotes => +"in " & em( to_string( cmd ) ),
                 subjectNotes => subjectOS,
                 reason => +"is unable to run the command because",
                 obstructorNotes => +"it could not start a new process"
            );
            Success := false;
         elsif myPID = 0 then                -- in child process?
            -- a background process is not the login shell anymore.  If we fail
            -- to clear this flag, the subprocess will fail to terminate without
            -- "logout".
            isLoginShell := false;
            -- We do not close stdout first because it's a potential race
            -- condition. dup2 will implicitly close the target.
            -- closeResult := close( stdout );                 -- redirect stdout to pipe
<<retry1b>>
            result := integer( dup2( pipeStack( pipeStackTop )( intoPipe ), stdout ) );
            if result < 0 then               -- failed?
              if C_errno = EINTR then
                 goto retry1b;
              end if;
              err( contextNotes => pl( "At " & gnat.source_info.source_location &
                           " while in " ) & em( to_string( cmd ) ),
                   subjectNotes => subjectOS,
                   reason => +"cannot create a pipe because syscall dup2() returned",
                   obstructorNotes => getEmOSError
              );
              Success := false;
              return;
            end if;
            -- Stderr = Stdout, we should pipe it as well.
            if pipeStderr then
<<retry1c>>
               result := integer( dup2( pipeStack( pipeStackTop )( intoPipe ), stderr ) );
               if result < 0 then               -- failed?
                  if C_errno = EINTR then
                     goto retry1c;
                  end if;
                  err( contextNotes => pl( "At " & gnat.source_info.source_location &
                           " while in " ) & em( to_string( cmd ) ),
                       subjectNotes => subjectOS,
                       reason => +"cannot create a pipe because syscall dup2() returned",
                       obstructorNotes => getEmOSError
                  );
                  Success := false;
                  return;
                end if;
            end if;
            -- close EINTR is a diagnostic message.  Do not handle.
            closeResult := close( pipeStack( pipeStackTop )( intoPipe ) ); -- copied this
            -- close EINTR is a diagnostic message.  Do not handle.
            closeResult := close( pipeStack( pipeStackTop )( outOfPipe ) ); -- not using pipe output
            -- close EINTR is a diagnostic message.  Do not handle.
            if trace then
               put_trace( to_string( fullPath ) &
                  " output attached to input end of pipe fd" &
                  pipeStack( pipeStackTop )( intoPipe )'img );
            end if;

            spawnCommandOrRunBuiltin(
              cmd, fullPath, ap, noReturn => true, success => success );
            -- This normally never returns.  However, it can return in the
            -- of a broken pipe signal (such as "| head").  In this case,
            -- we treat the command as finished and abort processing in the
            -- child shell for this command.
            done := true;
            token := eof_t;
         else                                -- parent process?
            lastChild := myPID;
            newJob.pid := myPID;             -- record the job
            newJob.cmd := cmd;               -- in the job table
            newJob.status := running;
            newJob.quiet := true;
            jobList.Insert( jobs, newJob );
            Success := true;
         end if;
      else
         err(
             contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while in " ) & em( to_string( cmd ) ),
             subjectNotes => subjectInterpreter,
             reason => +"had an internal error because",
             obstructorNotes => pl( "pipelines must be run in background" )
         );
      end if;
   end if;
end run_inpipe;


-----------------------------------------------------------------------------
--  RUN FROM-PIPE
--
-- Run an external command or a BUSH built-in at the end of a pipeline.  If
-- background is true, run the program in the background.
-----------------------------------------------------------------------------

procedure run_frompipe( cmd : unbounded_string;
   ap      : argumentListPtr;
   success : out boolean;
   background : boolean := false ) is
   fullpath : unbounded_string;
   che      : cmdHashEntry;
   oldStdin : aFileDescriptor;
   result   : integer;
   closeResult : int;
begin

   -- Locate the command

   getHashInfo( cmd, che );
   fullPath := che.fullPath;
   if length( che.fullPath ) = 0 and not che.builtin then
      findCmdPath( cmd, fullPath );
      if not no_command_hash and then length( fullPath ) > 0 then
         addCommandHash( cmd, fullPath, builtin => false );
      end if;
   end if;

   -- Run the command

   if length( fullPath ) = 0 and not che.builtin then
      cmdpos := cmdpos - 1;
      err_cmd_not_found( cmd );
      Success := false;
      closePipeline;
   else
      if background then                    -- should never be
         err(
             contextNotes => pl( "At " & gnat.source_info.source_location &
                " while in " ) & em( to_string( cmd ) ),
             subjectNotes => subjectInterpreter,
             reason => +"had an internal error because",
             obstructorNotes => pl( "the final pipeline command must be run in foreground" )
         );
         Success := false;
         closePipeline;
         return;
      end if;

<<retry1>>
      oldStdin := dup( stdin );             -- save original stdin
      if oldStdin < 0 then                  -- not OK? abort
         if C_errno = EINTR then
            goto retry1;
         end if;
         err( contextNotes => +"in " & em( to_string( cmd ) ),
              subjectNotes => subjectOS,
              reason => +"cannot to save standard input because dup() returned",
              obstructorNotes => getEmOSError
         );
         Success := false;
         closePipeline;
         return;
      end if;
      -- We do not close stdin first because it's a potential race
      -- condition. dup2 will implicitly close the target.
      -- closeResult := close( stdin );           -- redirect standard input
<<retry3>>
      result := integer( dup2( pipeStack( pipeStackTop )( outOfPipe), stdin ) );
      if result < 0 then                    -- failed?
         if C_errno = EINTR then
            goto retry3;
         end if;
         err( contextNotes => pl( "At " & gnat.source_info.source_location &
                   " while in " ) & em( to_string( cmd ) ),
              subjectNotes => subjectOS,
              reason => +"cannot create a pipe for input because syscall dup2() returned",
              obstructorNotes => getEmOSError
         );
         Success := false;
         closePipeline;
      else
         -- close all the pipe file descriptors.  We've copied the ones
         -- we need with dup2.
         closePipeline; -- or close-on-exec
         if trace then
            put_trace( to_string( fullPath ) &
               " input attached to output end of pipe fd" &
               pipeStack( pipeStackTop )( outOfPipe )'img );
         end if;
         spawnCommandOrRunBuiltin(
            cmd, fullPath, ap, noReturn => false, success => success );
         closeResult := close( stdin );          -- closing pipe
         -- close EINTR is a diagnostic message.  Do not handle.
      end if;
<<retry5>>
      result := integer( dup2( oldStdin, stdin ) );
      if result < 0 then                    -- failed? show error
         if C_errno = EINTR then
            goto retry5;
         end if;
         err( contextNotes => pl( "At " & gnat.source_info.source_location &
                   " while in " ) & em( to_string( cmd ) ),
             subjectNotes => subjectOS,
             reason => +"was unable to restore old standard input because syscall dup2() returned",
             obstructorNotes => getEmOSError
         );
      end if;
      closeResult := close( oldStdin );        -- discard original stdin copy
      -- close EINTR is a diagnostic message.  Do not handle.

   end if;
end run_frompipe;


-----------------------------------------------------------------------------
--  RUN BOTH-PIPE
--
-- Run an external command or a BUSH built-in at the middle of a pipeline.  If
-- background is true, run the program in the background.
-----------------------------------------------------------------------------

procedure run_bothpipe( cmd : unbounded_string;
   ap : argumentListPtr;
   success : out boolean;
   background : boolean := false;
   pipeStderr : boolean := false ) is
   fullpath : unbounded_string;
   che  : cmdHashEntry;

   myPID    : aPID;
   newJob   : aJob;
   result   : integer;
begin

   -- Setup the pipeline

   pipeStackTop := pipeStackTop+1;             -- prepare for another pipe
   if pipeStackTop > aPipeStack'last then      -- no available pipes?
      err( contextNotes => contextInPipeline,
           subjectNotes => subjectInterpreter,
           reason => +"cannot construct the pipeline because",
           obstructorNotes => em( "the pipeline too long" )
      );
      pipeStackTop := pipeStackTop-1;          -- avoid future constraint
      Success := false;                        -- errors and flag failure
      return;                                  -- and quit
   end if;
   pipe( result, pipeStack( pipeStackTop ) );  -- create pipe
   if result /= 0 then                         -- failed?
      err( contextNotes => contextInPipeline,
           subjectNotes => subjectOS,
           reason => +"cannot create a pipe because pipe() returned",
           obstructorNotes => getEmOSError,
           remedy => +"too many files are open"
      );
      pipeStackTop := pipeStackTop-1;          -- top not valid pipe
      Success := false;                        -- flag failure
      return;                                  -- and quit
   end if;

   -- Locate the command

   getHashInfo( cmd, che );
   fullPath := che.fullPath;
   if length( che.fullPath ) = 0 and not che.builtin then
      findCmdPath( cmd, fullPath );
      if not no_command_hash and then length( fullPath ) > 0 then
         addCommandHash( cmd, fullPath, builtin => false );
      end if;
   end if;

   -- Run the command

   if length( fullPath ) = 0 and not che.builtin then
      cmdpos := cmdpos - 1;
      err_cmd_not_found( cmd );
      Success := false;
   else
      if background then
         myPID := fork;
         if myPID < 0 then
            err( contextNotes => +"in " & em( to_string( cmd ) ),
                 subjectNotes => subjectOS,
                 reason => +"is unable to run the command because",
                 obstructorNotes => +"it could not start a new process"
            );
            Success := false;
         elsif myPID = 0 then
            -- a background process is not the login shell anymore.  If we fail
            -- to clear this flag, the subprocess will fail to terminate without
            -- "logout".
            isLoginShell := false;
            -- We do not close stdin first because it's a potential race
            -- condition. dup2 will implicitly close the target.
            --result := integer( close( stdin ) );
            -- close EINTR is a diagnostic message.  Do not handle.
<<retrydupout>>
            result := integer( dup2( pipeStack( pipeStackTop-1 )( outOfPipe ), stdin ) );
            if result < 0 then
               if C_errno = EINTR then
                  goto retrydupout;
               end if;
              err( contextNotes => pl( "At " & gnat.source_info.source_location &
                       " while in " ) & em( to_string( cmd ) ),
                    subjectNotes => subjectOS,
                    reason => +"cannot create a pipe for standard input because syscall dup2() returned",
                    obstructorNotes => getEmOSError
               );
               Success := false;
               return;
            end if;
            -- We do not close stdout first because it's a potential race
            -- condition. dup2 will implicitly close the target.
            -- result := integer( close( stdout ) );
<<retrydupin>>
            result := integer( dup2( pipeStack( pipeStackTop )( intoPipe ), stdout ) );
            if result < 0 then
               if C_errno = EINTR then
                  goto retrydupin;
               end if;
               err( contextNotes => pl( "At " & gnat.source_info.source_location &
                        " while in " ) & em( to_string( cmd ) ),
                    subjectNotes => subjectOS,
                    reason => +"cannot create a pipe for standard output because syscall dup2() returned",
                    obstructorNotes => getEmOSError
               );
               Success := false;
               return;
            end if;
            -- Stderr = Stdout, we should pipe it as well.
            if pipeStderr then
<<retrydupin2>>
               result := integer( dup2( pipeStack( pipeStackTop )( intoPipe ), stderr ) );
               if result < 0 then               -- failed?
                  if C_errno = EINTR then
                     goto retrydupin2;
                  end if;
                   err( contextNotes => pl( "At " & gnat.source_info.source_location &
                           " while in " ) & em( to_string( cmd ) ),
                       subjectNotes => subjectOS,
                       reason => +"cannot create a pipe for standard error because syscall dup2() returned",
                       obstructorNotes => getEmOSError
                  );
                  Success := false;
                  return;
                end if;
            end if;
            -- close all the pipe file descriptors.  We've copied the ones
            -- we need with dup2.
            closePipeline; -- or close-on-exec

            if trace then
               put_trace( to_string( fullPath ) &
                 " input attached to output end of pipe fd" &
                 pipeStack( pipeStackTop-1 )( outOfPipe )'img );
               put_trace( to_string( fullPath ) &
                 " output attached to input end of pipe fd" &
                 pipeStack( pipeStackTop )( intoPipe )'img );
            end if;

            spawnCommandOrRunBuiltin(
               cmd, fullPath, ap, noReturn => true, success => success );
            -- should never return, but...bail out of shell child process
            -- This normally never returns.  However, it can return in the
            -- of a signal.  In this case, we treat the command as finished
            -- and abort processing in the
            -- child shell for this command.
            done := true;
            token := eof_t;
         else
            lastChild := myPID;
            newJob.pid := myPID;
            newJob.cmd := cmd;
            newJob.status := running;
            newJob.quiet := true;
            jobList.Insert( jobs, newJob );
            Success := true;
         end if;
      else
         err(
             contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while in " ) & em( to_string( cmd ) ),
             subjectNotes => subjectInterpreter,
             reason => +"had an internal error because",
             obstructorNotes => pl( "pipelines must be run in background" )
         );
      end if;
   end if;
end run_bothpipe;

procedure closePipeline is
  res : int;
  -- doesn't reset the top of stack
begin
  for i in 1..pipeStackTop loop
      res := close( pipeStack( i )( intoPipe ) );
      -- close EINTR is a diagnostic message.  Do not handle.
      res := close( pipeStack( i )( outOfPipe ) );
      -- close EINTR is a diagnostic message.  Do not handle.
  end loop;
end closePipeline;

procedure wait4children is
  -- wait for all jobs to finish
  oldJob : aJob;
  pid    : aPID;
  status : integer := 0;
begin
  -- clean up any completed child processes
  pid := 0;
  loop
     oldJob.cmd := null_unbounded_string;
     C_reset_errno;
     wait( pid, status );
     exit when pid = -1 and C_errno /= EINTR;
     for i in 1..jobList.Length( jobs ) loop
         jobList.Find( jobs, i, oldJob );
         if oldJob.pid = pid then
            jobList.Clear( jobs, i );
            exit;
         end if;
     end loop;
     if not oldJob.quiet then
        put_line_retry( oldJob.cmd &" (" & pid'img & ") is finished" );
     end if;
  end loop;
end wait4children;

procedure wait4LastJob is
-- wait for the last job in the job list (used to clean up a pipeline
-- when an error occurs)
  lastJob : aJob;
  status : integer := 0;
  numberOfJobs : long_integer;
  result : aPID;                                  -- wait result code
begin
  numberOfJobs := jobList.Length( jobs );
  if numberOfJobs = 0 then
     return;
  end if;
  jobList.Find( jobs, numberOfJobs, lastJob );
<<retry>>
  C_reset_errno;
  waitpid( result, lastJob.pid, status, 0 );
  if result = -1 and C_errno = EINTR then       -- interrupted syscall?
     goto retry;                              -- retry it
  elsif result = -1 and C_errno /= ECHILD then  -- error if any error
     err( contextNotes => +"waiting for the last job",
          subjectNotes => pl( qp( "job" ) & numberOfJobs'img ),
          reason => +"did not finish because waitpid() returned",
          obstructorNotes => getEmOSError
     );
  end if;
  jobList.Clear( jobs, numberOfJobs );
  if not lastJob.quiet then
     put_line_retry( lastJob.cmd &" (" & lastJob.pid'img & ") is finished" );
  end if;
  last_status := aStatusCode( status mod 256 ); -- is this right?
end wait4LastJob;

procedure updateJobStatus is
  -- update the status for all jobs, displaying changes
  status : integer := 0;                          -- wait status
  result : aPID;                                  -- wait result code
  job    : aJob;                                  -- a BUSH job
--pgid : aPID;
--function getpgid( pid : aPID ) return aPID;
--pragma import( C, getpgid );
begin
  for i in 1..jobList.Length( jobs ) loop         -- for all jobs
      jobList.Find( jobs, i, job );               -- next job
<<retry>>
--      pgid := getpgid( job.pid );
--put_line( "pgid = " & pgid'img );
--put_line( "bush = " & getpid'img );
      C_reset_errno;
      waitpid( result, job.pid, status, WNOHANG+WUNTRACED );
      if result = -1 and C_errno = EINTR then       -- interrupted syscall?
         goto retry;                              -- retry it
      elsif result = -1 and C_errno /= ECHILD then  -- error if any error
         err( contextNotes => +"updating a job status",
              subjectNotes => pl( qp( "job" ) & i'img ),
              reason => +"the status is uncertain because waitpid() returned",
              obstructorNotes => getEmOSError
         );
      elsif result = -1 then                      -- no such job? (ECHILD)
         jobList.Clear( jobs, i );                -- then it's done
          if not job.quiet then
            put_line_retry( job.cmd &" (" & job.pid'img & ") has finished" );
          end if;
      elsif result = 0 then                       -- unable to get status?
         if job.status /= running then            -- show status on a change
            if not job.quiet then
              put_line_retry( job.cmd &" (" & job.pid'img & ") is running" );
            end if;
         end if;
         job.status := running;                   -- still running
      else                                        -- able to get status?
         if job.status /= stopped then            -- show status on a change
            if not job.quiet then
              put_line_retry( job.cmd &" (" & job.pid'img & ") has stopped" );
            end if;
         end if;
         job.status := stopped;                   -- probably stopped
      end if;
  end loop;
end updateJobStatus;

procedure putJobList is
   -- display the job list
  oldJob : aJob;
  numberOfJobs : long_integer;
begin
  updateJobStatus;
  numberOfJobs := jobList.Length( jobs );
  for i in 1..numberOfJobs loop
      jobList.Find( jobs, i, oldJob );
      if not oldJob.quiet then
         put_retry( oldJob.cmd &" (" & oldJob.pid'img & ") is " );
         case oldJob.status is
         when running =>
           put_line_retry( "running" );
         when stopped =>
           put_line_retry( "stopped" );
         when others =>
           put_line_retry( "unknown status" );
         end case;
      end if;
  end loop;
  if numberOfJobs = 1 then
     put_retry( "There is" & jobList.Length( jobs )'img & " job" );
  else
     put_retry( "There are" & jobList.Length( jobs )'img & " jobs" );
  end if;
  put_line_retry( " (including hidden ones)" );
end putJobList;

end jobs;
