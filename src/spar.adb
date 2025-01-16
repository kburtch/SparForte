------------------------------------------------------------------------------
-- Main Program                                                             --
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

with ada.text_io,
     ada.strings.unbounded,
     ada.command_line,
     world.splashes,
     compiler,
     scanner.communications,
     pegasoft.user_io,
     pegasoft.strings,
     parser,
     interpreter,
     signal_flags;

use ada.text_io,
    ada.strings.unbounded,
    ada.command_line,
    world,
    world.splashes,
    compiler,
    scanner,
    scanner.communications,
    pegasoft.user_io,
    pegasoft.strings,
    parser,
    interpreter,
    signal_flags;

--with string_util; use string_util;

--with spar_interface;

-- pragma Optimize( space );

procedure spar is
  term_id : identifier;
  libraryPathNext : boolean := false;
  wasCoding : boolean := false;
  wasDesign : boolean := false;
  wasMaintenance : boolean := false;
  wasTest : boolean := false;
  i : positive := 1;
begin
  startSignalFlags;

  -- set scanner variable to reflect whether or not this is a login
  -- shell

  isLoginShell := Command_Name(1) = '-';

  -- test phase mode is now the default

  testOpt := true;

  -- collect command line options

  if Argument_Count = 1 then
     if Argument(1) = "-h" or Argument( 1 ) = "--help" then
        Put_Line( "SparForte usage" );
        Put_Line( "spar [-bcCdDeghilLmprtvVx] [-Ld|-L d] [--break][--check][--debug][--exec][--gcc-errors][--login][--verbose][--version][--perf][--restricted][--session s][--coding|--design|--maintenance|--test][--trace][--] [script [param1 ...] ]" );
        Put_Line( "  --break or -b       - enable breakout debugging prompt" );
        Put_Line( "  --check or -c       - syntax check the script but do not run" );
        Put_Line( "  --coding or -C      - development phase mode" );
        Put_Line( "  --color or --colour - enable coloured messages and UTF-8 icons" );
        Put_Line( "  --debug or -d       - enable pragma assert and pragma debug" );
        Put_Line( "  --design or -D      - design phase mode" );
        Put_Line( "  --exec or -e        - script is a string containing SparForte commands" );
        Put_Line( "  --gcc-errors or -g  - simple GCC-style errors (good for IDEs)" );
        Put_Line( "  --help or -h        - show this help" );
        Put_Line( "  --import-all or -i  - import all environment variables" );
        Put_Line( "  -Ld or -L d         - add directory d to the separate files search list" );
        Put_Line( "                        (may be repeated)" );
        Put_Line( "  --login or -l       - simulate a login shell" );
        Put_Line( "  --maintenance or -m - maintenance phase mode" );
        Put_Line( "  --profile or -P     - run the profile script when not logging in" );
        Put_Line( "  --pref or -p        - show performance stats" );
        Put_Line( "  --quiet or -q       - brief error messages" );
        Put_Line( "  --restricted or -r  - restricted shell mode" );
        Put_Line( "  --session s         - start a session with the name s" );
        Put_Line( "  --test or -t        - test phase mode (default)" );
        Put_Line( "  --trace or -x       - show script lines as they run" );
        Put_Line( "  --verbose or -v     - show shell activity" );
        Put_Line( "  --version or -V     - show version" );
        Put_Line( "  --                  - explicitly end shell options" );
        Set_Exit_Status( 0 );
        return;
     elsif Argument(1) = "-V" or Argument( 1 ) = "--version" then
        displayVersionSplash;
        Set_Exit_Status( 0 );
        return;
     end if;
  end if;
  if Argument_Count > 0 then
     optionOffset := 1;
     libraryPathNext := false;
     -- for i in 1..Argument_Count loop
     while i <= Argument_Count loop
         if libraryPathNext then
            if Argument(i)'length = 0 then
               Put_Line( standard_error, Command_Name & ": missing argument for -L" );
               Set_Exit_Status( 192 );
               return;
            end if;
            if length( libraryPath ) > 0 then
               libraryPath := libraryPath & ":";
            end if;
            libraryPath := libraryPath & to_unbounded_string( Argument(i) );
            libraryPathNext := false;

         -- Interpret Long (--) Arguments

         elsif Argument(i) = "--break" then
            breakoutOpt := true;
         elsif Argument(i) = "--check" then
            syntaxOpt := true;
         elsif Argument(i) = "--coding" then
            testOpt := false;
            wasCoding := true;
         elsif Argument(i) = "--color" then
            colourOpt := true;
         elsif Argument(i) = "--colour" then
            colourOpt := true;
         elsif Argument(i) = "--debug" then
            debugOpt := true;
         elsif Argument(i) = "--design" then
            testOpt   := false;
            designOpt := true;
            wasDesign := true;
         elsif Argument(i) = "--exec" then
            execOpt := true;
         elsif Argument(i) = "--gcc-errors" then
            gccOpt := true;
         elsif Argument(i) = "--help" then
            Put_Line( standard_error, Command_Name & ": --help should appear by itself" );
            Set_Exit_Status( 192 );
            return;
         elsif Argument(i) = "--import-all" then
            importOpt := true;
         elsif Argument(i) = "--login" then
            isLoginShell := true;
         elsif Argument(i) = "--maintenance" then
            testOpt   := false;
            maintenanceOpt := true;
            wasMaintenance := true;
         elsif Argument(i) = "--perf" then
            perfOpt := true;
         elsif Argument(i) = "--profile" then
            profileOpt := true;
         elsif Argument(i) = "--quiet" then
            quietOpt := true;
         elsif Argument(i) = "--restricted" then
            rshOpt := true;
         elsif Argument(i) = "--test" then
            testOpt := true;
            wasTest := true;
         elsif Argument(i) = "--session" then
            -- For the session, we need a session name
            if i < Argument_Count then
               optionOffset := optionOffset + 1;
               i := i + 1;
               if sessionName /= "" then
                  Put_Line( standard_error, Command_Name & ": --session was used more than once" );
                  Set_Exit_Status( 192 );
                  return;
              elsif not Is_Alphanumeric(to_unbounded_string( Argument(i) ) ) then
                  Put_Line( standard_error, Command_Name & ": --session name is not alphanumeric" );
                  Set_Exit_Status( 192 );
                  return;
              else
                  sessionName := to_unbounded_string( Argument(i) );
              end if;
            else
               Put_Line( standard_error, Command_Name & ": --session should have a name" );
               Set_Exit_Status( 192 );
               return;
            end if;
         elsif Argument(i) = "--trace" then
            traceOpt := true;
         --elsif Argument(i) = "--tabsize" then
         --   tabsizeOpt := true;
         elsif Argument(i) = "--verbose" then
            verboseOpt := true;
         elsif Argument(i) = "--version" then
            Put_Line( standard_error, Command_Name & ": --version should appear by itself" );
            Set_Exit_Status( 192 );
            return;
         elsif Argument(i) = "--" then
            optionOffset := optionOffset + 1;
            exit;
         elsif Argument(i)'length = 0 then
            if execOpt then
               Put_Line( standard_error, Command_Name & ": empty script string" );
            else
               Put_Line( standard_error, Command_Name & ": empty script name" );
            end if;
            Set_Exit_Status( 192 );
            return;
         elsif Argument(i)(1) = '-' then

            -- Interpret 1 or More Individual Characters

            declare
               Args : constant string := Argument(i);
            begin
               if Args'length < 2 then
                  Put_Line( standard_error, Command_Name & ": missing options: " & Argument(i) );
                  Set_Exit_Status( 1 );
                  return;
               end if;
               -- -L has a weird format: the directory is immediately attached
               -- to the "L"

               if Args(2) = 'L' then
                   if Args'length = 2 then
                      libraryPathNext := true;
                   else
                      if length( libraryPath ) > 0 then
                         libraryPath := libraryPath & ":";
                      end if;
                   end if;
                   libraryPath := libraryPath & to_unbounded_string( Args(3..Args'last) );
               else
               for letter in 2..Args'last loop
                   if Args(letter) = 'b' then
                      breakoutOpt := true;
                   elsif Args(letter) = 'c' then
                      syntaxOpt := true;
                   elsif Args(letter) = 'C' then
                      testOpt := false;
                      wasCoding := true;
                   elsif Args(letter) = 'd' then
                      debugOpt := true;
                   elsif Args(letter) = 'D' then
                      testOpt   := false;
                      designOpt := true;
                      wasDesign := true;
                   elsif Args(letter) = 'e' then
                      execOpt := true;
                   elsif Args(letter) = 'h' then
                      Put_Line( standard_error, Command_Name & ": -h should appear by itself" );
                      Set_Exit_Status( 192 );
                      return;
                   elsif Args(letter) = 'i' then
                      importOpt := true;
                   elsif Args(letter) = 'l' then
                      isLoginShell := true;
                   elsif Args(letter) = 'm' then
                      testOpt   := false;
                      maintenanceOpt := true;
                      wasMaintenance := true;
                   elsif Args(letter) = 'p' then
                      perfOpt := true;
                   elsif Args(letter) = 'P' then
                      profileOpt := true;
                   elsif Args(letter) = 'q' then
                      quietOpt := true;
                   elsif Args(letter) = 'g' then
                      gccOpt := true;
                   elsif Args(letter) = 'r' then
                      rshOpt := true;
                   elsif Args(letter) = 't' then
                      testOpt   := false;
                      testOpt := true;
                      wasTest := true;
                   elsif Args(letter) = 'v' then
                      verboseOpt := true;
                   elsif Argument(i) = "-V" then
                      Put_Line( standard_error, Command_Name & ": -V should appear by itself" );
                      Set_Exit_Status( 192 );
                      return;
                   --elsif Argument(i) = "-w" then
                   --   tabsizeOpt := true;
                   elsif Args(letter) = 'x' then
                      traceOpt := true;
                   else
                      Put_Line( standard_error, Command_Name & ": unknown option: " & Argument(i) );
                      Set_Exit_Status( 192 );
                      return;
                   end if;
               end loop;
            end if;
            end;
         else
            exit;
         end if;
         optionOffset := optionOffset + 1;
         i := i + 1;
     end loop;
  end if;

  -- Missing arguments / commands

  if libraryPathNext then
     Put_Line( standard_error, Command_Name & ": missing argument for -L" );
     Set_Exit_Status( 192 );
     return;
  end if;
  if boolean(syntaxOpt) and
     optionOffset > Argument_Count then
     Put_Line( standard_error, Command_Name & ": missing command name" );
     Set_Exit_Status( 192 );
     return;
  end if;
  if optionOffset > Argument_Count then
     optionOffset := 0;
  end if;

  -- Illegal option switch combinations

  if (wasDesign and wasCoding) or (wasDesign and wasMaintenance) or (wasDesign and wasTest) or (wasCoding and wasMaintenance) or (wasCoding and wasTest) or (wasMaintenance and wasTest) then
     Put_Line( standard_error, Command_Name & ": only one of --design, --maintenance or --test is allowed" );
     Set_Exit_Status( 192 );
     return;
  end if;
  if isLoginShell and boolean( profileOpt ) then
     Put_Line( standard_error, Command_Name & ": only one of --login and --profile is allowed" );
     Set_Exit_Status( 192 );
     return;
  end if;

  -- Initialize
  --
  -- Start the byte code compiler, scanner.  Setup the display.  Start the
  -- parser.

  startCommunications;
  startCompiler;
  startScanner;
  findIdent( to_unbounded_string( "TERM" ), term_id );
  checkDisplay( identifiers( term_id ).value.all );
  startParser;
  startInterpreter;

  -- replace initialization message with copyright notice (if on
  -- a terminal and not running a script)

  if optionOffset = 0 then
     displayCopyrightSplash; -- located in user_io
  end if;

  -- Run SparForte: load profile, etc. and run script or start command line

  interpret;

  -- Shutdown

  shutdownInterpreter;
  shutdownParser;
  shutdownScanner;
  shutdownCompiler;
  shutdownSignalFlags;
  shutdownCommunications;

end spar;
