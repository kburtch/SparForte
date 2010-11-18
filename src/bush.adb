------------------------------------------------------------------------------
-- BUSH Main Program                                                        --
--                                                                          --
-- Part of BUSH                                                             --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2005 Ken O. Burtch & FSF                 --
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
-- CVS: $Id: bush.adb,v 1.2 2005/02/11 02:59:20 ken Exp $

with ada.text_io,
    ada.strings.unbounded.text_io,
    ada.command_line,
    bush_os,
    world,
    scanner,
    user_io,
    parser,
    signal_flags;
use ada.text_io,
   ada.strings.unbounded,
   ada.strings.unbounded.text_io,
   ada.command_line,
   bush_os,
   world,
   scanner, 
   user_io,
   parser,
   signal_flags;

pragma Optimize( space );

procedure bush is
  term_id : identifier;
  libraryPathNext : boolean := false;
begin
  startSignalFlags;
  
  -- set scanner variable to reflect whether or not this is a login
  -- shell

  isLoginShell := Command_Name(1) = '-';

  -- collect command line options
  
  if Argument_Count = 1 then
     if Argument(1) = "-h" or Argument( 1 ) = "--help" then
        Put_Line( "BUSH (Business Shell) usage" );
        Put_Line( "bush [-bcdeghilnrvVx] [-Ld|-L d] [--break][--check][--debug][--exec][--gcc-errors][--login][--no-check][--verbose][--version][--restricted][--trace][--] [script [param1 ...] ]" );
        Put_Line( "  --break or -b      - enable breakout debugging prompt" );
        Put_Line( "  --check or -c      - syntax check the script but do not run" );
        Put_Line( "  --debug or -d      - enable pragma assert and pragma debug" );
        Put_Line( "  --exec or -e       - script is a string containing BUSH commands" );
        Put_Line( "  --gcc-errors or -g - simple GCC-style errors (good for IDEs)" );
        Put_Line( "  --help or -h       - show this help" );
        Put_Line( "  --import-all or -i - import all environment variables" );
        Put_Line( "   -Ld or -L d       - add directory d to the separate files search list" );
        Put_Line( "                       (may be repeated)" );
        Put_Line( "  --login or -l      - simulate a login shell" );
        Put_Line( "  --no-check or -n   - run script without checking syntax" );
        Put_Line( "  --restricted or -r - restricted shell mode" );
        Put_Line( "  --trace or -x      - show script lines as they run" );
        Put_Line( "  --verbose or -v    - show shell activity" );
        Put_Line( "  --version or -V    - show version" );
        Put_Line( "  --                 - explicitly end shell options" );
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
     for i in 1..Argument_Count loop

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
         elsif Argument(i) = "--debug" then
            debugOpt := true;
         elsif Argument(i) = "--exec" then
            execOpt := true;
         elsif Argument(i) = "--help" then
            Put_Line( standard_error, Command_Name & ": --help should appear by itself" );
            Set_Exit_Status( 192 );
            return;
         elsif Argument(i) = "--import-all" then
            importOpt := true;
         elsif Argument(i) = "--login" then
            isLoginShell := true;
         elsif Argument(i) = "--no-check" then
            nosyntaxOpt := true;
         elsif Argument(i) = "--restricted" then
            rshOpt := true;
         elsif Argument(i) = "--verbose" then
            verboseOpt := true;
         elsif Argument(i) = "--version" then
            Put_Line( standard_error, Command_Name & ": --version should appear by itself" );
            Set_Exit_Status( 192 );
            return;
         elsif Argument(i) = "--trace" then
            traceOpt := true;
         elsif Argument(i) = "--gcc-errors" then
            gccOpt := true;
         elsif Argument(i) = "--" then
            optionOffset := optionOffset + 1;
            exit;
         elsif Argument(i)'length = 0 then
            Put_Line( standard_error, Command_Name & ": empty script name" );
            Set_Exit_Status( 192 );
            return;
         elsif Argument(i)(1) = '-' then

            -- Interpret 1 or More Individual Characters

            declare
               Args : string := Argument(i);
            begin
               if Args'length < 2 then
                  Put_Line( standard_error, Command_Name & ": missing options: " & Argument(i) );
                  Set_Exit_Status( 1 );
                  return;
               end if;
               -- -L has a wierd format: the directory is immediately attached
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
                   elsif Args(letter) = 'd' then
                      debugOpt := true;
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
                   elsif Args(letter) = 'n' then
                      nosyntaxOpt := true;
                   elsif Args(letter) = 'g' then
                      gccOpt := true;
                   elsif Args(letter) = 'r' then
                      rshOpt := true;
                   elsif Args(letter) = 'v' then
                      verboseOpt := true;
                   elsif Argument(i) = "-V" then
                      Put_Line( standard_error, Command_Name & ": -V should appear by itself" );
                      Set_Exit_Status( 192 );
                      return;
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
     end loop;
  end if;
  if libraryPathNext then
     Put_Line( standard_error, Command_Name & ": missing argument for -L" );
     Set_Exit_Status( 192 );
     return;
  end if;
  if syntaxOpt and nosyntaxOpt then
     Put_Line( standard_error, Command_Name & ": -c and -n cannot be used together" );
     Set_Exit_Status( 192 );
     return;
  elsif (boolean(syntaxOpt) or boolean(nosyntaxOpt)) and
     optionOffset > Argument_Count then
     Put_Line( standard_error, Command_Name & ": missing command name" );
     Set_Exit_Status( 192 );
     return;
  end if;
  if optionOffset > Argument_Count then
     optionOffset := 0;
  end if;

  -- initialize

  startScanner;
  findIdent( to_unbounded_string( "TERM" ), term_id );
  checkDisplay( identifiers( term_id ).value );
  startParser;

  -- replace initialization message with copyright notice (if on
  -- a terminal and not running a script)

  if optionOffset = 0 then
     displayCopyrightSplash; -- located in user_io
  end if;

  -- run

  interpret;

  -- Apply the return error status

  if error_found then
     last_status := 192;
  end if;
  Set_Exit_Status( Exit_Status( last_status ) );

  -- shutdown

  shutdownParser;
  shutdownScanner;
  shutdownSignalFlags;

end bush;
