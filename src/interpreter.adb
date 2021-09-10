------------------------------------------------------------------------------
-- AdaScript Language Interpreter                                           --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------
pragma ada_2005;

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );
with ada.text_io,
    ada.calendar,
    ada.command_line,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    ada.numerics.float_random,
    ada.calendar,
    gnat.source_info,
    spar_os,
    pegasoft.strings,
    pegasoft.user_io,
    pegasoft.user_io.getline,
    pegasoft.script_io,
    world,
    performance_monitoring,
    reports.test,
    builtins,
    jobs,
    signal_flags,
    compiler,
    scanner,
    parser_aux,
    parser.decl.as,
    parser_tio;
use ada.text_io,
    ada.calendar,
    ada.command_line,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    spar_os,
    pegasoft.user_io,
    pegasoft.script_io,
    world,
    pegasoft.strings,
    performance_monitoring,
    reports.test,
    builtins,
    jobs,
    signal_flags,
    compiler,
    scanner,
    parser_aux,
    parser,
    parser.decl.as,
    parser_tio;

package body interpreter is


------------------------------------------------------------------------------
--  PUT COMMAND PROMPT
--
-- Process the command prompt script if necessary.  Show the command prompt.
-- Update the xterm terminal title if necessary.
------------------------------------------------------------------------------

procedure putCommandPrompt is
  suppressPromptChars : boolean;
  bigArrowIndex : natural;
  termTitle : unbounded_string;
begin

    -- Try to run the prompt script

    suppressPromptChars := false;
    if length( promptScript ) /= 0 then
       CompileRunAndCaptureOutput( promptScript, prompt );
       if error_found then
          put_line( current_error, fullErrorMessage );
          prompt := null_unbounded_string;
       elsif terminalWindowNaming then
          -- set the xterm window title
          termTitle := ASCII.ESC & ada.strings.unbounded.to_unbounded_string( "]2;" );
          for i in 1..length( prompt ) loop
              -- these are the readline codes to ignore the length
              -- of non-printable characters, such as terminal formatting
              -- codes.  Suppress these in the window title.
              if pegasoft.user_io.getline.has_readline then
                 if element( prompt, i ) = ASCII.SOH then
                    suppressPromptChars := true;
                 elsif element( prompt, i ) = ASCII.STX then
                    suppressPromptChars := false;
                 elsif not suppressPromptChars then
                    if is_graphic( element( prompt, i ) ) then
                       termTitle := termTitle & element( prompt, i  );
                    elsif element( prompt, i ) = ASCII.LF then
                       termTitle := termTitle & ' ';
                    elsif element( prompt, i ) = ASCII.CR then
                       termTitle := termTitle & ' ';
                    end if;
                 end if;
              else
                 if is_graphic( element( prompt, i ) ) then
                    termTitle := termTitle & element( prompt, i  );
                 elsif element( prompt, i ) = ASCII.LF then
                    termTitle := termTitle & ' ';
                 elsif element( prompt, i ) = ASCII.CR then
                    termTitle := termTitle & ' ';
                 end if;
              end if;
          end loop;

          -- Drop the big arrow from the xterm title, if a prompt ends with one.

          bigArrowIndex := index( termTitle, "=> " );
          if bigArrowIndex = length( termTitle ) - 2 then
             termTitle := unbounded_slice( termTitle, 1, bigArrowIndex -1 );
          end if;

          -- Strip any trailing spaces

          termTitle := trim( termTitle, ada.strings.right );

          -- Finish the xterm window title

          termTitle := termTitle & ASCII.BEL;

          -- Apply the xterm window title
          put( termTitle );
       end if;
    end if;

    -- If we have no prompt script or if the result was empty
    -- then use (or fall back to) the default prompt.

    if length( prompt ) = 0 then
       if terminalWindowNaming then
          put( ASCII.ESC & "]2;" & "SparForte" & ASCII.BEL  );
       end if;
    end if;
end putCommandPrompt;


------------------------------------------------------------------------------
--  INTERACTIVE SESSION
--
-- Begin an interactive session, processing a set of commands typed in by the
-- user.  Handle SIGINT at the command prompt as well as restoring standard
-- output/etc. as required so that lines can be typed in from the console.
-- Continue until the done flag is set.
--   This is run by parse, as the command line options dictate.  This is also
-- used to start a debugging session on interactive breakout.
------------------------------------------------------------------------------

procedure interactiveSession is
  command : unbounded_string;
  result : aFileDescriptor;
begin
  loop                                        -- repeatedly
    -- A control-c will abort the prompt script, so we need to handle it first.
    -- This will be a control-c from running the last command.
    wasSIGINT := false;                         -- clear sig flag
    updateJobStatus;                          -- cleanup any background jobs
    error_found := false;                     -- no err for prompt
    prompt := null_unbounded_string;

    putCommandPrompt;

    -- Show the command prompt and get the user's input

    --put_bold( prompt );                       -- show prompt in bold
    pegasoft.user_io.getline.getLine( command, prompt, keepHistory => true );  -- read command from keyboard
    if wasSIGINT then                         -- control-c at prompt?
       command := null_unbounded_string;      -- pretend empty command
       wasSIGINT := false;                    -- we handled it
       new_line;                              -- user didn't push enter
    end if;
    -- has user redirected standard in/out/error? then redirect them for
    -- the duration of execution of the commands (and restore the original
    -- values later)
    if currentStandardInput /= originalStandardInput then
       result := dup2( currentStandardInput, stdin );   -- restore stdin
       if result < 0 then
          err( "Unable to redirect standard input" );
       end if;
    end if;
    if currentStandardOutput /= originalStandardOutput then
       result := dup2( currentStandardOutput, stdout ); -- restore stdout
       if result < 0 then
          err( "Unable to redirect standard output" );
       end if;
    end if;
    if currentStandardError /= originalStandardError then
       result := dup2( currentStandardError, stderr );  -- restore stderr
       if result < 0 then
          err( "Unable to redirect standard error" );
       end if;
    end if;
    error_found := false;                               -- no err found (prompt)
    exit_block := false;                                -- not exit-ing a block
    -- fix spacing is not smart enough to remove double spaces inside a cmd.
    -- the cmd may have quotes and spaces may be significant
    fixSpacing( command, inside => false );             -- strip leading/trailing spaces
    -- add an ending ; if one is missing.
    --
    -- in order to do this, we have to take into account a comment (if any).
    -- however, we haven't tokenized the string yet so how do we tell if
    -- a double-minus is quoted or not?  it's not possible to do it easily.
    -- I could add a LF plus ; but then it will appear to be a missing
    -- statement if a ; is actually there before a comment.
    -- so we'll just check for an ending ; and add one if it is not there.
    if length( command ) > 0 then                       -- something there?
       if Element( command, length( command ) ) /= ';' then -- missing ending ;?
          command := command & ";";                     -- it's implicit so add it
       elsif length( command ) > 1 then
          if Element( command, length( command )-1) = '\' then
             command := command & ";";
          end if;
       end if;                                          -- w/space in case shell cmd
       -- Things get a little confusing in breakout mode because we have two
       -- programs: the original script, and the command being typed.  So which
       -- source do we name if an error occurs?
       --
       -- For now, we don't want to change the name of the source line in error
       -- messages
       if inputMode /= breakout then
          sourceFilesList.Clear( SourceFiles );
          sourceFilesList.Push( SourceFiles, aSourceFile'( pos => 0, name => to_unbounded_string( commandLineSource ) ) );
       end if;
       compileCommand( command );
       if not error_found then
          cmdpos := firstScriptCommandOffset;           -- start at first char
          token := identifiers'first;                   -- dummy, replaced by g_n_t
          getNextToken;                                 -- load first token
          while token /= eof_t and not error_found loop
             ParseGeneralStatement;                     -- do the command
          end loop;
          if error_found then
             put_line( standard_error, fullErrorMessage );
             -- Not sure if templates are possible here but...
             if hasTemplate then
                putTemplateHeader( templateHeader );
                put_line( fullTemplateErrorMessage );
             end if;
          end if;
       end if;
    end if;
    -- restore original standard input, output, error for the command line
<<retry1>> result := dup2( originalStandardInput, stdin );   -- restore standard input
    if result < 0 then
       if C_errno = EINTR then
          goto retry1;
       end if;
       err( "Unable to restore standard input" );
    end if;
<<retry2>> result := dup2( originalStandardOutput, stdout ); -- restore standard output
    if result < 0 then
       if C_errno = EINTR then
          goto retry2;
       end if;
       err( "Unable to restore standard output" );
    end if;
<<retry3>>    result := dup2( originalStandardError, stderr );  -- restore standard error
    if result < 0 then
       if C_errno = EINTR then
          goto retry3;
       end if;
       err( "Unable to restore standard error" );
    end if;
    exit when done;                                   -- and stop when done
  end loop;
end interactiveSession;


------------------------------------------------------------------------------
--  INTERPRET POLICY
--
-- Run a policy file.
------------------------------------------------------------------------------

procedure interpretPolicy( policyPath : string ) is
  alreadyCompiled : boolean := false;
  oldScriptFilePath : unbounded_string;
  firstLine : aliased unbounded_string;
  res : int;
begin
  if verboseOpt then
     Put_Trace( "Executing Global Policy" );
  end if;
  --if syntax_check then
  --   if verboseOpt then
  --      Put_Trace( "Checking Syntax" );
  --   end if;
  --else
  --   if verboseOpt then
  --      Put_Trace( "Executing Script" );
  --   end if;
  --end if;
  inputMode := fromScriptFile;                     -- running a script
  if execOpt then                                  -- -e?
     if verboseOpt then
        Put_Trace( "Compiling Byte Code", utf_wristwatch );
     end if;
     sourceFilesList.Clear( SourceFiles );
     sourceFilesList.Queue( SourceFiles, aSourceFile'(pos => 0, name => to_unbounded_string (commandLineSource) ) );
     scriptFilePath := to_unbounded_string( commandLineSource ); -- script name
     compileCommand( to_unbounded_string( policyPath ) ); -- path is really script
  else                                             -- else path is a path
    oldScriptFilePath := scriptFilePath;
    scriptFilePath := to_unbounded_string( policyPath ); -- script name
    if syntax_check then
       -- only register source files during the syntax check, when files
       -- (including include files) are loaded
       sourceFilesList.Clear( SourceFiles );
       sourceFilesList.Queue( SourceFiles, aSourceFile'(pos => 0, name => scriptFilePath) );
    end if;
    scriptFile := open( policyPath & ASCII.NUL, 0, 660 ); -- open script
    -- If this procedure is called twice during a regular script run, once
    -- for the syntax check and once to execute.  If the script name
    -- is the same, don't compile it a second time: assume that the
    -- byte code is good and proceed to parsing.
    alreadyCompiled := scriptFilePath = oldScriptFilePath;

    if scriptFile > 0 then                           -- good?
       error_found := false;                         -- no error found
       exit_block := false;                          -- not exit-ing a block
       if not LineRead( firstLine'access ) then        -- read first line
          err( "unable to read first line of script" );
          goto error;                                -- and interpreting
       end if;
       if not alreadyCompiled then
       -- This was script = null but that breaks code if this procedure
       -- is called twice with different files (e.g. loading profiles).
       --if script = null then
          if verboseOpt then
             Put_Trace( "Compiling Byte Code", utf_wristwatch );
          end if;
          compileScript( firstline );
       --end if;
       end if;
    end if;
  end if;
  if (scriptFile > 0 or boolean(execOpt)) and not done then -- file open or -e?
     if not error_found then
        cmdpos := firstScriptCommandOffset;
        token := identifiers'first;                -- dummy, replaced by g_n_t
        getNextToken;                              -- load first token
        parsePolicy;
        expectSemicolon;
        if token /= eof_t then                     -- unexpected token?
           expect( pragma_t );                     -- say what we expect
        else
           expect( eof_t );                        -- should be nothing else
        end if;
     end if;
  elsif C_errno = 2 then                           -- file not found?
     err( "unable to open policy file '" &
       policyPath & "' : file not found" );
  elsif C_errno /= 0 then                          -- some other error?
     -- use /= 0 in case user aborts with control-c
     err( "unable to open policy file '" &
       policyPath & "' : " & OSerror( C_errno ) );
  end if;
<<error>>
  if error_found then                              -- was there an error?
     if last_status = 0 then                       -- no last command status?
        set_exit_status( Failure );                -- just set to 1
        if trace then
           put_trace("Script exit status is" & failure'img );
        end if;
     else                                          -- otherwise
        set_exit_status( exit_status( last_status ) ); -- return last status
        if trace then
           put_trace("Script exit status is" & last_status'img );
        end if;
     end if;
  end if;
  if not execOpt then                              -- not -e?
     res := close( scriptFile );              -- close the script file
     -- close EINTR is a diagnostic message.  Do not handle.
  end if;

end interpretPolicy;


------------------------------------------------------------------------------
--  INTERPRET CONFIG
--
-- Run a configuration file.
------------------------------------------------------------------------------

procedure interpretConfig( configPath : string ) is
  alreadyCompiled : boolean := false;
  oldScriptFilePath : unbounded_string;
  firstLine : aliased unbounded_string;
  res : int;
  old_identifiers_top : constant identifier := identifiers_top;
begin
  if verboseOpt then
     Put_Trace( "Executing Global Config" );
  end if;
  inputMode := fromScriptFile;                     -- running a script
  if execOpt then                                  -- -e?
     if verboseOpt then
        Put_Trace( "Compiling Byte Code", utf_wristwatch );
     end if;
     sourceFilesList.Clear( SourceFiles );
     sourceFilesList.Queue( SourceFiles, aSourceFile'(pos => 0, name => to_unbounded_string (commandLineSource) ) );
     scriptFilePath := to_unbounded_string( commandLineSource ); -- script name
     compileCommand( to_unbounded_string( configPath ) ); -- path is really script
  else                                             -- else path is a path
    oldScriptFilePath := scriptFilePath;
    scriptFilePath := to_unbounded_string( configPath ); -- script name
    if syntax_check then
       -- only register source files during the syntax check, when files
       -- (including include files) are loaded
       sourceFilesList.Clear( SourceFiles );
       sourceFilesList.Queue( SourceFiles, aSourceFile'(pos => 0, name => scriptFilePath) );
    end if;
    scriptFile := open( configPath & ASCII.NUL, 0, 660 ); -- open script
    -- If this procedure is called twice during a regular script run, once
    -- for the syntax check and once to execute.  If the script name
    -- is the same, don't compile it a second time: assume that the
    -- byte code is good and proceed to parsing.
    alreadyCompiled := scriptFilePath = oldScriptFilePath;

    if scriptFile > 0 then                           -- good?
       error_found := false;                         -- no error found
       exit_block := false;                          -- not exit-ing a block
       if not LineRead( firstLine'access ) then        -- read first line
          err( "unable to read first line of script" );
          goto error;                                -- and interpreting
       end if;
       if not alreadyCompiled then
       -- This was script = null but that breaks code if this procedure
       -- is called twice with different files (e.g. loading profiles).
       --if script = null then
          if verboseOpt then
             Put_Trace( "Compiling Byte Code", utf_wristwatch );
          end if;
          compileScript( firstline );
       --end if;
       end if;
    end if;
  end if;
  if (scriptFile > 0 or boolean(execOpt)) and not done then -- file open or -e?
     if not error_found then
        cmdpos := firstScriptCommandOffset;
        token := identifiers'first;                -- dummy, replaced by g_n_t
        getNextToken;                              -- load first token
        parseConfig;
        expectSemicolon;
        expect( eof_t );                        -- should be nothing else
     end if;
  elsif C_errno = 2 then                             -- file not found?
     err( "unable to open config file '" &
       configPath & "' : file not found" );
  elsif C_errno /= 0 then                          -- some other error?
     -- use /= 0 in case user aborts with control-c
     err( "unable to open config file '" &
       configPath & "' : " & OSerror( C_errno ) );
  end if;
<<error>>
  if error_found then                              -- was there an error?
     if last_status = 0 then                       -- no last command status?
        set_exit_status( Failure );                -- just set to 1
        if trace then
           put_trace("Script exit status is" & failure'img );
        end if;
     else                                          -- otherwise
        set_exit_status( exit_status( last_status ) ); -- return last status
        if trace then
           put_trace("Script exit status is" & last_status'img );
        end if;
     end if;
  else                                             -- no error?
     -- treat all the identifiers declared in the configuration as used
     -- as programmers may have no say in when the are declared of if
     -- their program uses them yet
     for i in old_identifiers_top..identifiers_top-1 loop
         put_trace( "global config declared " & to_string( identifiers( i ).name ) );
         identifiers( i ).wasReferenced := true;
         --identifiers( i ).referencedByThread := mainThread;
     end loop;
     --set_exit_status( 0 );                         -- return no error
     --if trace then                                 -- -x? show 0 exit status
     --   put_trace("Script exit status is 0" );
     --end if;
  end if;
  if not execOpt then                              -- not -e?
     res := close( scriptFile );              -- close the script file
     -- close EINTR is a diagnostic message.  Do not handle.
  end if;
end interpretConfig;


------------------------------------------------------------------------------
--  INTERPRET SCRIPT
--
-- Load a script, compile byte code, perform a syntax check (if needed) and
-- execute the script (if needed).  If -e, scriptPath is a string of commands.
-- This is run by parse, as the command line options dictate.
--
-- scriptPath: a script to run, or a string of commands (-e).
------------------------------------------------------------------------------

procedure SPAR_interpretScript( C_scriptPath : C_path ) is
begin
  -- TODO: incomplete, no syntax check checking
  interpreterPhase := executing;
  interpretScript( To_Ada( C_scriptPath ) );
end SPAR_interpretScript;

procedure interpretScript( scriptPath : string ) is
  alreadyCompiled : boolean := false;
  oldScriptFilePath : unbounded_string;
  firstLine : aliased unbounded_string;
  res : int;
  scriptDir : unbounded_string;
  startTime : Time;
  endTime   : Time;
  realTime  : duration;
begin
  if verboseOpt then
     case interpreterPhase is
     when checking =>
        startTime := Clock;
        Put_Trace( "Checking Syntax", utf_wristwatch );
     when executing =>
        Put_Trace( "Executing Commands", utf_checkmark );
     when others =>
        err( "internal error: unexpected interpreter phase" );
     end case;
  end if;
  inputMode := fromScriptFile;                     -- running a script
  if execOpt then                                  -- -e?
     if verboseOpt then
        Put_Trace( "Compiling Byte Code", utf_wristwatch );
     end if;
     sourceFilesList.Clear( SourceFiles );
     sourceFilesList.Queue( SourceFiles, aSourceFile'(pos => 0, name => to_unbounded_string (commandLineSource) ) );
     scriptFilePath := to_unbounded_string( commandLineSource ); -- script name
     if scriptPath(scriptPath'last) /= ';' then -- the path is really commands for -e
        compileCommand( to_unbounded_string( scriptPath ) & ';' );
     else
        compileCommand( to_unbounded_string( scriptPath ) );
     end if;
  else                                             -- else path is a path
    oldScriptFilePath := scriptFilePath;
    scriptFilePath := to_unbounded_string( scriptPath ); -- script name
    if syntax_check then
       -- only register source files during the syntax check, when files
       -- (including include files) are loaded
       sourceFilesList.Clear( SourceFiles );
       sourceFilesList.Queue( SourceFiles, aSourceFile'(pos => 0, name => scriptFilePath) );
    end if;
    -- TODO: The script file must be opened to determine the ending.  But
    -- the file doesn't need to be read if already compiled.  It would be
    -- better if the suffix was determined in such a way that the file
    -- doesn't need to be opened a second time.
    scriptFile := open( scriptPath & ASCII.NUL, 0, 660 ); -- open script
    if scriptFile < 1 then                           -- error?
       scriptFilePath := to_unbounded_string( scriptPath ) & ".sp";   -- try name with ".sp"
       scriptFile := open( scriptPath & ".sp" & ASCII.NUL, 0, 660 );
       if scriptFile < 1 then                           -- error?
          scriptFilePath := to_unbounded_string( scriptPath ) & ".bush";   -- try name with ".bush"
          scriptFile := open( scriptPath & ".bush" & ASCII.NUL, 0, 660 );
       end if;
    end if;
    -- Only run an acceptable file
    if scriptFile > 0 then
       scriptDir := dirname( scriptFilePath );
       if not C_is_secure_dir( to_string( scriptDir ) & ASCII.NUL ) then
         err( "the script directory " & optional_yellow( to_string( scriptDir ) ) & " is either not readable, is world writable, is not a directory" );
         goto error;
       elsif not C_is_includable_file( to_string( scriptFilePath ) & ASCII.NUL ) then
         err( "the script file " & optional_yellow( to_string( scriptFilePath ) ) & " is either not readable, is world writable, is not a regular file or is empty" );
         goto error;
       end if;
    end if;

    -- This procedure is called twice during a regular script run: once
    -- for the syntax check and once to execute.  If the script name
    -- is the same, don't compile it a second time: assume that the
    -- byte code is good and proceed to parsing.
    alreadyCompiled := scriptFilePath = oldScriptFilePath;

    if scriptFile > 0 then                           -- good?
       error_found := false;                         -- no error found
       exit_block := false;                          -- not exit-ing a block
       if not LineRead( firstLine'access ) then        -- read first line
          err( "unable to read first line of script" );
          goto error;                                -- and interpreting
       end if;
       if not alreadyCompiled then
       -- This was script = null but that breaks code if this procedure
       -- is called twice with different files (e.g. loading profiles).
       --if script = null then
          if verboseOpt then
             Put_Trace( "Compiling Byte Code", utf_wristwatch );
          end if;
          compileScript( firstline );
       --end if;
       end if;
    end if;
  end if;
  if (scriptFile > 0 or boolean(execOpt)) and not done then -- file open or -e?
     if perfOpt then
        if syntax_check then
           perfStats.startTime := ada.calendar.clock;
        end if;
     end if;
     parse;
     if perfOpt then
        if syntax_check then
           staticByteCodeAnalysis;
        else
           perfStats.endTime := ada.calendar.clock;
        end if;
     end if;
  elsif C_errno = 2 then                             -- file not found?
     err( "unable to open script file '" &
       scriptPath & "' : file not found" );
  elsif C_errno /= 0 then                          -- some other error?
     -- use /= 0 in case user aborts with control-c
     err( "unable to open script file '" &
       scriptPath & "' : " & OSerror( C_errno ) );
  end if;
<<error>>
  if error_found then                              -- was there an error?
     if last_status = 0 then                       -- no last command status?
        set_exit_status( Failure );                -- just set to 1
        if trace then
           put_trace("Script exit status is" & failure'img );
        end if;
     else                                          -- otherwise
        set_exit_status( exit_status( last_status ) ); -- return last status
        if trace then
           put_trace("Script exit status is" & last_status'img );
        end if;
     end if;
  else                                             -- no error?
     set_exit_status( 0 );                         -- return no error
     if trace then                                 -- -x? show 0 exit status
        put_trace("Script exit status is 0" );
     end if;
  end if;
  if not execOpt then                              -- not -e?
     res := close( scriptFile );              -- close the script file
     -- close EINTR is a diagnostic message.  Do not handle.
  end if;

  if verboseOpt then
     case interpreterPhase is
     when checking =>
        endTime := Clock;
        realTime := endTime - startTime;
        Put_Trace( "Done checking syntax in" & realTime'img & " sec", utf_checkmark );
     when executing =>
        null; -- Put_Trace( "Done executing Commands", utf_checkmark );
     when others =>
        err( "internal error: unexpected interpreter phase" );
     end case;
  end if;
end interpretScript;


------------------------------------------------------------------------------
--  INTERPRET COMMANDS
--
-- Compile string of commands into byte code, perform a syntax check (if
-- needed) and execute the script (if needed).
-- This is run by parse, as the command line options dictate.
-- This will output error messages.
-- This is used when SparForte is called as a library.  It is not used by
-- the spar command.
------------------------------------------------------------------------------

procedure interpretCommands( commandString : unbounded_string ) is
begin
  case interpreterPhase is
  when checking =>
     if verboseOpt then
        Put_Trace( "Checking Syntax", utf_wristwatch );
     end if;
  when executing =>
     if verboseOpt then
        Put_Trace( "Executing Commands", utf_checkmark );
     end if;
  when others =>
     err( "internal error: unexpected interpreter phase" );
  end case;
  scriptFilePath := to_unbounded_string( commandLineSource ); -- "script" name
  sourceFilesList.Clear( SourceFiles );
  sourceFilesList.Push( SourceFiles, aSourceFile'( pos => 0, name => basename( scriptFilePath ) ) );
  if verboseOpt then
     Put_Trace( "Compiling Byte Code", utf_wristwatch );
  end if;
  compileCommand( commandString );
  parse;
  if error_found then                              -- was there an error?
     put_line( standard_error, fullErrorMessage );
     -- may or may not have a template at this point, so check
     if hasTemplate then
        putTemplateHeader( templateHeader );
        put_line( fullTemplateErrorMessage );
     end if;
     if last_status = 0 then                       -- no last command status?
        set_exit_status( Failure );                -- just set to 1
        if trace then
           put_trace("Command string exit status is" & failure'img );
        end if;
     else                                          -- otherwise
        set_exit_status( exit_status( last_status ) ); -- return last status
        if trace then
           put_trace("Command string exit status is" & last_status'img );
        end if;
     end if;
  else                                             -- no error?
     set_exit_status( 0 );                         -- return no error
     if trace then
        put_trace("Command string exit status is 0" );
     end if;
  end if;
end interpretCommands;

procedure interpretCommands( commandString : string ) is
begin
  -- TODO: incomplete, no syntax check checking
  interpreterPhase := executing;
  interpretCommands( to_unbounded_string( commandString ) );
end interpretCommands;

procedure SPAR_interpretCommands( C_commandString : C_cmds ) is
begin
  -- TODO: incomplete, no syntax check checking
  interpreterPhase := executing;
  interpretCommands( To_Ada( C_commandString ) );
end SPAR_interpretCommands;


------------------------------------------------------------------------------
--  SET STANDARD VARIABLES
--
-- Define variables that cannot be setup by the scanner.
------------------------------------------------------------------------------

procedure SetStandardVariables is
begin

  -- Standard_Input, Standard_Output and Standard_Error have the
  -- form of a typical file variable.

  DoInitFileVariableFields( standard_input_t, originalStandardInput,
    "<standard input>", in_file_t );
  DoInitFileVariableFields( standard_output_t, originalStandardOutput,
    "<standard output>", out_file_t );
  DoInitFileVariableFields( standard_error_t, originalStandardError,
    "<standard error>", out_file_t );

  -- Current_Input, Current_Output and Current_Error are aliases for
  -- another file (by default, Standard_Input/Output/Error ).

  identifiers( current_input_t ).value.all :=
    to_unbounded_string( standard_input_t'img );
  identifiers( current_output_t ).value.all :=
    to_unbounded_string( standard_output_t'img );
  identifiers( current_error_t ).value.all :=
    to_unbounded_string( standard_error_t'img );

end SetStandardVariables;


------------------------------------------------------------------------------
-- Startup Files
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  DO GLOBAL POLICY
--
-- Run /etc/sparforte_policy for enforcing architectural policies.  Displays
-- any error message.
--
-- I decided against a per-project policy file because such files should be
-- in a project's home directory and read-only the the developers.  It
-- would be difficult to load a policy based on the script name as different
-- scripts could share the same name.  I can revisit this later.
------------------------------------------------------------------------------

procedure doGlobalPolicy is
  save_rshOpt  : commandLineOption;              -- for executing policy
  save_execOpt : commandLineOption;              -- for executing policy
  res  : int;
  scriptDir : unbounded_string;
begin
  save_rshOpt := rshOpt;                         -- if restricted shell
  save_execOpt := execOpt;                       -- if cmd line script
  rshOpt := false;                               -- turn off for profile
  scriptFile := open( globalPolicyPath & ASCII.NUL, 0, 660 ); -- open script
  if scriptFile > 0 then                         -- good?
     res := close( scriptFile );            -- close test fd
     -- close EINTR is a diagnostic message.  Do not handle.
  end if;
  if C_errno /= ENOENT then
     if C_errno = 0 then
        scriptDir := dirname( to_unbounded_string( globalPolicyPath ) );
        if not C_is_secure_dir( to_string( scriptDir ) & ASCII.NUL ) then
          err( "the global policy directory " & optional_yellow( to_string( scriptDir ) ) & " is either not readable, is world writable, is not a directory" );
          raise BAD_PROFILE with "global policy directory " & optional_yellow( globalPolicyPath ) & " is either not readable, is world writable, is not a directory";
        elsif not C_is_includable_file( globalPolicyPath & ASCII.NUL ) then
          err( "global policy file " & optional_yellow( globalPolicyPath ) & " is either not readable, is world writable, is not a regular file or is empty" );
          raise BAD_PROFILE with "global policy file '" & globalPolicyPath & "' is either not readable, is world writable, is not a regular file or is empty";
        end if;
        sourceFilesList.Push( SourceFiles, aSourceFile'( pos => 0, name => basename( to_unbounded_string( globalPolicyPath ) ) ) );
        interpreterPhase := executing;               -- we are executing
        interpretPolicy( globalPolicyPath ); -- run policy script
     end if;
  end if;
  if error_found then
     put_line( standard_error, fullErrorMessage );
  end if;
  rshOpt := save_rshOpt;                          -- restore rsh setting
  execOpt := save_execOpt;                        -- restore -e setting
end doGlobalPolicy;


------------------------------------------------------------------------------
--  DO GLOBAL CONFIG
--
-- Run /etc/sparforte_config for declaring configuration options.  Displays
-- any error message.
--
-- I decided against a per-project config file because such files should be
-- in a project's home directory and read-only the the developers.  It
-- would be difficult to load a config based on the script name as different
-- scripts could share the same name.  Also, configs could reflect the SDLC
-- stage.  I can revisit this later.
------------------------------------------------------------------------------

procedure doGlobalConfig is
  save_rshOpt  : commandLineOption;              -- for executing config
  save_execOpt : commandLineOption;              -- for executing config
  res  : int;
  scriptDir : unbounded_string;
begin
  save_rshOpt := rshOpt;                         -- if restricted shell
  save_execOpt := execOpt;                       -- if cmd line script
  rshOpt := false;                               -- turn off for profile
  C_reset_errno;
  scriptFile := open( globalConfigPath & ASCII.NUL, 0, 660 ); -- open script
  if scriptFile > 0 then                         -- good?
     res := close( scriptFile );            -- close test fd
     -- close EINTR is a diagnostic message.  Do not handle.
  end if;
  if C_errno /= ENOENT then
     if C_errno = 0 then
        scriptDir := dirname( to_unbounded_string( globalConfigPath ) );
        if not C_is_secure_dir( to_string( scriptDir ) & ASCII.NUL ) then
          err( "the global config directory " & optional_yellow( to_string( scriptDir ) ) & " is either not readable, is world writable, is not a directory" );
          raise BAD_PROFILE with "global config directory " & optional_yellow( globalConfigPath ) & " is either not readable, is world writable, is not a directory";
        elsif not C_is_includable_file( globalConfigPath & ASCII.NUL ) then
          err( "global config file " & optional_yellow( globalConfigPath ) & " is either not readable, is world writable, is not a regular file or is empty" );
          raise BAD_PROFILE with "global config file '" & globalConfigPath & "' is either not readable, is world writable, is not a regular file or is empty";
        end if;
        sourceFilesList.Push( SourceFiles, aSourceFile'( pos => 0, name => basename( to_unbounded_string( globalConfigPath ) ) ) );
        interpreterPhase := executing;               -- we are executing
        interpretConfig( globalConfigPath ); -- run config script
     end if;
  end if;
  if error_found then
     put_line( standard_error, fullErrorMessage );
  end if;
  rshOpt := save_rshOpt;                          -- restore rsh setting
  execOpt := save_execOpt;                        -- restore -e setting
end doGlobalConfig;


------------------------------------------------------------------------------
--  DO GLOBAL PROFILE
--
-- Run /etc/sparforte_profile for setting up interactive scripts.  Displays
-- any error message.
------------------------------------------------------------------------------

procedure doGlobalProfile is
  save_rshOpt  : commandLineOption;              -- for executing profile
  save_execOpt : commandLineOption;              -- for executing profile
  res : int;
  scriptDir : unbounded_string;
begin
  save_rshOpt := rshOpt;                         -- if restricted shell
  save_execOpt := execOpt;                       -- if cmd line script
  rshOpt := false;                               -- turn off for profile
  C_reset_errno;
  scriptFile := open( globalProfilePath & ASCII.NUL, 0, 660 ); -- open script
  if scriptFile > 0 then                         -- good?
     res := close( scriptFile );            -- close test fd
     -- close EINTR is a diagnostic message.  Do not handle.
  end if;
  if C_errno /= ENOENT then
     if C_errno = 0 then
        scriptDir := dirname( to_unbounded_string( globalProfilePath ) );
        if not C_is_secure_dir( to_string( scriptDir ) & ASCII.NUL ) then
          err( "the global profile directory " & optional_yellow( to_string( scriptDir ) ) & " is either not readable, is world writable, is not a directory" );
          raise BAD_PROFILE with "global profile directory " & optional_yellow( globalProfilePath ) & " is either not readable, is world writable, is not a directory";
        elsif not C_is_includable_file( globalProfilePath & ASCII.NUL ) then
          err( "global profile file " & optional_yellow( globalProfilePath ) & " is either not readable, is world writable, is not a regular file or is empty" );
          raise BAD_PROFILE with "global profile file '" & globalProfilePath & "' is either not readable, is world writable, is not a regular file or is empty";
        end if;
        sourceFilesList.Push( SourceFiles, aSourceFile'( pos => 0, name => basename( to_unbounded_string( globalProfilePath ) ) ) );
        interpreterPhase := executing;               -- we are executing
        interpretScript( globalProfilePath ); -- run login script
     end if;
  end if;
  if error_found then
     put_line( standard_error, fullErrorMessage );
  end if;
  rshOpt := save_rshOpt;                          -- restore rsh setting
  execOpt := save_execOpt;                        -- restore -e setting
end doGlobalProfile;


------------------------------------------------------------------------------
--  DO LOCAL PROFILE
--
-- Run ~/.sparforte_profile for setting up interactive scripts.  Displays
-- any error message.
------------------------------------------------------------------------------

procedure doLocalProfile is
  save_rshOpt  : commandLineOption;              -- for executing profile
  save_execOpt : commandLineOption;              -- for executing profile
  home_id      : identifier;                     -- HOME variable
  res          : int;
  profilePath  : unbounded_string;
  scriptDir : unbounded_string;
begin
  save_rshOpt := rshOpt;                   -- if restricted shell
  save_execOpt := execOpt;                 -- if cmd line script
  rshOpt := false;                         -- turn off for profile
  findIdent( to_unbounded_string( "HOME" ), home_id );
  if home_id /= eof_t then                     -- HOME defined?
     profilePath := identifiers( home_id ).value.all;
     if Element( profilePath, length( profilePath ) ) /= '/' then
        profilePath := profilePath & "/";
     end if;
     profilePath := profilePath & ".sparforte_profile";
     C_reset_errno;
     scriptFile := open( to_string( profilePath ) & ASCII.NUL, 0, 660 ); -- open script
     if scriptFile > 0 then                       -- profile opened OK?
        res := close( scriptFile );               -- close test fd
        -- close EINTR is a diagnostic message.  Do not handle.
     end if;
     if C_errno /= ENOENT then
        if C_errno = 0 then
           scriptDir := dirname( profilePath );
           if not C_is_secure_dir( to_string( scriptDir ) & ASCII.NUL ) then
             err( "the local profile directory " & optional_yellow( to_string( scriptDir ) ) & " is either not readable, is world writable, is not a directory" );
             raise BAD_PROFILE with "local profile directory " & optional_yellow( to_string( profilePath ) ) & " is either not readable, is world writable, is not a directory";
           elsif not C_is_includable_file( to_string( profilePath ) & ASCII.NUL ) then
             err( "the local profile file " & optional_yellow( to_string( profilePath ) ) & " is either not readable, is world writable, is not a regular file or is empty" );
             raise BAD_PROFILE with "local profile file '" & to_string( profilePath ) & "' is either not readable, is world writable, is not a regular file or is empty";
           end if;
           sourceFilesList.Push( SourceFiles, aSourceFile'( pos => 0, name => basename( profilePath ) ) );
           interpreterPhase := executing;               -- we are executing
           interpretScript( to_string( profilePath ) ); -- do profile
        end if;
     end if;
  end if;
  if error_found then
     put_line( standard_error, fullErrorMessage );
  end if;
  rshOpt := save_rshOpt;                          -- restore rsh setting
  execOpt := save_execOpt;                        -- restore -e setting
end doLocalProfile;


------------------------------------------------------------------------------
-- Starting the Script Interpreter
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  CHECK AND INTERPRET SCRIPT
--
-- Perform a syntax check and run the script.  Display any post-run errors
-- and set the exit status.
--
-- Use Interpret instead to load profiles/policies or start an interactive
-- session.
------------------------------------------------------------------------------

procedure checkAndInterpretScript( fullScriptPath : string ) is
begin
  if tail( ada.strings.unbounded.to_unbounded_string( fullScriptPath ), 3 ) = ".sh" then
     put_line( standard_error, ".sh probably means " & fullScriptPath & " (if it exists) is not a SparForte script" );
     set_exit_status( Failure );
     return;
  elsif syntaxOpt then                               -- -c / --check?
     syntax_check := true;                           -- check syntax only
     interpreterPhase := checking;                   -- we are checking
     interpretScript( fullScriptPath );    -- check the script
  -- no syntax check obsolete with BUSH 2.0 (syntax check stage is needed to
  -- load "include" files)
  --elsif nosyntaxOpt then                             -- -n / --nocheck?
  --   if traceOpt then                                -- -x / --trace?
  --      trace := true;                               -- turn on trace
  --   end if;
  --   interpretScript( Argument( OptionOffset ) );    -- run the script
     if error_found then
        put_line( standard_error, fullErrorMessage );
     end if;
  else
     syntax_check := true;                           -- check syntax only
     interpreterPhase := checking;                   -- we are checking
     interpretScript( fullScriptPath );    -- check the script
     if not error_found then                         -- no errors?
        resetScanner;                                -- discard declarations
        SetStandardVariables;                        -- built-in elaboration
        if traceOpt then                             -- -x?
           trace := true;                            -- turn on trace
        end if;
        interpreterPhase := executing;               -- we are executing
        interpretScript( fullScriptPath ); -- run the script
     end if;
     if testOpt then
        if wasTestErrorOrFailure then
           err( "tests failed" );
        end if;
     end if;
     -- display any error message
     if error_found then
        put_line( standard_error, fullErrorMessage );
        -- may or may not have a template at this point, so check
        if hasTemplate then
           putTemplateHeader( templateHeader );
           put_line( fullTemplateErrorMessage );
        end if;
     end if;
     if length( depreciatedMsg ) > 0 then            -- pragma depreciated?
        if hasTemplate then
           putTemplateHeader( templateHeader );
        end if;
        warn( to_string( depreciatedMsg ) );
     end if;
     if restriction_annotations_not_optional and not annotationsFound then
        -- pragma restriction( annotations_not_optional )
        if hasTemplate then
           putTemplateHeader( templateHeader );
        end if;
        warn( "annotations are required but missing" );
     end if;
     if restriction_no_annotate_todos and annotationTodoFound then
        -- pragma restriction( annotations_no_todos )
        if hasTemplate then
           putTemplateHeader( templateHeader );
        end if;
        warn( "annotation todo found but none expected" );
     end if;
     if processingTemplate and not error_found then  -- doing a template
        if verboseOpt then
           Put_Trace( "Processing template " & to_string( templatePath ) );
        end if;
        begin
           putTemplateHeader( templateHeader );
           processTemplate;
        exception
        when STATUS_ERROR =>
           err( "cannot open template " & optional_yellow( to_string( templatePath ) ) &
               " - file may be locked" );
        when NAME_ERROR =>
           err( "template " & optional_yellow( to_string( templatePath ) ) &
               " doesn't exist or is not readable" );
        when MODE_ERROR =>
           err( gnat.source_info.source_location &
                ": internal error: mode error on template " & optional_yellow( to_string( templatePath ) ) );
        when END_ERROR =>
           err( gnat.source_info.source_location &
                ": internal error: end of file reached on template " & optional_yellow( to_string( templatePath ) ) );
        -- when others =>
       --  err( "unable to open template " & optional_yellow( to_string( templatePath ) ) );
        end;
        -- Not sure this makes sense in a template, but we'll include it here.
        if testOpt then
           if wasTestErrorOrFailure then
              err( "tests failed" );
           end if;
        end if;
        if error_found then
           put_line( standard_error, fullErrorMessage );
           -- always has template if we get here
           put_line( fullTemplateErrorMessage );
        end if;
        -- if there was a formal script with a main program, the main program
        -- block is left un-pulled for use by the template.  We can now
        -- pull it.
        if blocks_top /= block'first then
           pullBlock;
        end if;
     end if;
  end if;

  -- Show performance statistics

  if perfOpt then
     put_perf_summary;
  end if;

  -- Apply the return error status

  if error_found and last_status = 0 then
     last_status := 192;
  end if;
  Set_Exit_Status( Exit_Status( last_status ) );

end checkAndInterpretScript;


------------------------------------------------------------------------------
--  INTERPRET
--
-- Begin executing things.  Specifically, set up the environmental flags based
-- on the command line options, run the .profile script(s) and then interpret
-- commands or start an interactive session.
--
-- This is the procedure executed by the spar command.
------------------------------------------------------------------------------

procedure interpret is
begin
  if optionOffset = 0 then                       -- no arguments or '-'?
     if isLoginShell or boolean(profileOpt) then -- login shell? find profile
        doGlobalProfile;                         -- or --profile option
        if not error_found then
           doLocalProfile;
        end if;
     end if;
     inputMode := interactive;                          -- we're interactive
     if traceOpt then                                   -- -x / --trace?
        trace := true;                                  -- turn on trace
     end if;
     interactiveSession;                                -- start the session
  else
     doGlobalPolicy;                                -- architecture policies
     doGlobalConfig;                                -- sysadmin configs
     if not error_found then
        checkAndInterpretScript( Argument( OptionOffset ) );
     end if;
  end if;
end interpret;

------------------------------------------------------------------------------
-- Housekeeping
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  START INTERPRETER
--
-- Startup this package, performing any set up tasks.  In this case, set up
-- standard input, standard output and standard error so they can be redirected.
-- Also, initialize the random number generator.
--
-- The original stdin/out/err must be copied to prevent them from being
-- closed and lost.  They are needed at the shell prompt to draw the
-- prompt and to read typing by the user.  For output redirection in
-- scripts or commands, we'll always work with duplicates of the
-- originals, leaving the originals open for the shell prompt.
--
-- When copied, these are probably fd 3,4,5, but will record what dup
-- returns to be safe.  For example, if fd 0 is closed (by dup2) the file
-- actually will remain open until fd 3 is also closed.
--
-- currentStandardInput/Output/Error represent the file stdin/out/err has
-- been redirected to by Set_Input/Output/Error.  If none, the value is
-- 0,1,2 respectively.
--
-- The standard files standard_input, standard_output and standard_error
-- represent the original stdin/out/err...ie the copied fd 4,5,6.
------------------------------------------------------------------------------

procedure startInterpreter is
  pwd : identifier;
begin
  originalStandardInput := dup( stdin );              -- record stdin
  originalStandardOutput := dup( stdout );            -- record stdout
  originalStandardError := dup( stderr );             -- record stderr
  currentStandardInput := originalStandardInput;      -- no redirection, stdin
  currentStandardOutput := originalStandardOutput;    -- no redirection, stdout
  currentStandardError := originalStandardError;      -- no redirection, stderr

  SetStandardVariables;                               -- built-in elaboration
  Ada.Numerics.Float_Random.Reset( random_generator );-- reset RND generator
  clearCommandHash;                                   -- initialize cmd table

  -- initialize the current_working_directory
  -- assign PWD the value.  We can't rely on the imported value from the
  -- environment to be accurate as we may be running in a new directory.

  findPwd;
  findIdent( to_unbounded_string( "PWD" ), pwd );
  if pwd /= eof_t then
     identifiers( pwd ).value.all := current_working_directory;
  end if;
  pegasoft.user_io.getline.startupGetline( optionOffset );
end startInterpreter;


------------------------------------------------------------------------------
--  SHUTDOWN INTERPRETER
--
-- Shut down this package, performing any cleanup tasks.  In this case, none.
------------------------------------------------------------------------------

procedure shutdownInterpreter is
begin
  pegasoft.user_io.getline.shutdownGetline;
end shutdownInterpreter;

end interpreter;
