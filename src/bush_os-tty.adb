------------------------------------------------------------------------------
-- BUSH OS/TTY - Terminal Emulation Information                             --
-- This version is for UNIX/Linux Commands                                  --
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
-- CVS: $Id: bush_os-tty.adb,v 1.2 2005/02/11 02:59:22 ken Exp $

with ada.text_io; -- for DEBUG
use ada.text_io; -- for DEBUG
with Interfaces.C;

with unchecked_deallocation;

with bush_os.exec, signal_flags, world;
use  bush_os.exec, signal_flags, world;

package body bush_os.tty is


-- tput terminfo codes (ie. Linux)

tinfo_normal : aliased string := "sgr0"  & ASCII.NUL;
tinfo_bold   : aliased string := "bold"  & ASCII.NUL;
tinfo_inverse: aliased string := "smso"  & ASCII.NUL;
tinfo_cleop  : aliased string := "ed"    & ASCII.NUL;
tinfo_cleol  : aliased string := "el"    & ASCII.NUL;
tinfo_up     : aliased string := "cuu1"  & ASCII.NUL;
tinfo_right  : aliased string := "cuf1"  & ASCII.NUL;
tinfo_bel    : aliased string := "bel"   & ASCII.NUL;
tinfo_reset  : aliased string := "reset" & ASCII.NUL;
tinfo_clear  : aliased string := "clear" & ASCII.NUL;
tinfo_lines  : aliased string := "lines" & ASCII.NUL;
tinfo_cols   : aliased string := "cols"  & ASCII.NUL;

-- tput termcap codes (ie. FreeBSD)

tcap_normal : aliased string := "me"    & ASCII.NUL;
tcap_bold   : aliased string := "md"    & ASCII.NUL;
tcap_inverse: aliased string := "so"    & ASCII.NUL;
tcap_cleop  : aliased string := "cd"    & ASCII.NUL;
tcap_cleol  : aliased string := "ce"    & ASCII.NUL;
tcap_up     : aliased string := "up"    & ASCII.NUL;
tcap_right  : aliased string := "nd"    & ASCII.NUL;
tcap_bel    : aliased string := "bl"    & ASCII.NUL;
tcap_reset  : aliased string := "reset" & ASCII.NUL;
tcap_clear  : aliased string := "clear" & ASCII.NUL;
tcap_lines  : aliased string := "li"    & ASCII.NUL;
tcap_cols   : aliased string := "co"    & ASCII.NUL;

lastTerm    : unbounded_string := to_unbounded_string( "<undefined>" );
-- type of terminal as of last attribute update

tput_path1  : unbounded_string := to_unbounded_string( "/bin/tput" );
tput_path2  : unbounded_string := to_unbounded_string( "/usr/bin/tput" );

procedure free_list is new unchecked_deallocation( argumentList,
   argumentListPtr );
-- Free in spawn package also tries deleting memory which we are
--using

function tput( attr : termAttributes ) return unbounded_string is
-- Run tput command to extract a terminal control code from
-- the terminfo database.  Return the resulting code.  To do
-- this, we mimic the system() command by redirecting stdout to
-- a temp file and calling spawm to run the comand.
  tputResults   : string := "/tmp/bushXXXXXX" & ASCII.NUL;
  result        : aFileDescriptor;
  tputResultsFD : aFileDescriptor;
  oldStdout     : aFileDescriptor;
  ap            : argumentListPtr;
  ttyCode       : unbounded_string := null_unbounded_string;
  amountRead    : long_integer;
  ch            : character := ASCII.NUL;
  -- NUL to stop compiler warning
  term_id       : identifier;
  intResult     : integer;
begin

  -- Redirect standard output to a temp file
  
  mkstemp( tputResultsFD, tputResults );
  if tputResultsFD < 0 then
     put_line( standard_error, "Bush.TTY: Unable to make temp file" );
     put_line( standard_error, "BUSH.TTY: Error # " & C_errno'img );
     return null_unbounded_string;
  end if;

  oldstdout := dup( stdout );
  if oldstdout < 0 then
     close( tputResultsFD );
     put_line( standard_error, "Bush.TTY: Unable to save stdout" );
     put_line( standard_error, "Bush.TTY: Error # " & C_errno'img );
     return null_unbounded_string;
  end if;

  result := dup2( tputResultsFD, stdout );
  if result < 0 then
     close( tputResultsFD );
     close( oldstdout );
     put_line( standard_error, "Bush.TTY: Unable to redirect stdout" );
     put_line( standard_error, "Bush.TTY: Error # " & C_errno'img );
     return null_unbounded_string;
  end if;

  -- Create the argument list for tput command
  
  ap := new ArgumentList( 1..1 );

  if tput_style = "terminfo" then
     case attr is
     when normal =>  ap( 1 ) := tinfo_normal'access;
     when bold =>    ap( 1 ) := tinfo_bold'access;
     when inverse => ap( 1 ) := tinfo_inverse'access;
     when cleop =>   ap( 1 ) := tinfo_cleop'access;
     when cleol =>   ap( 1 ) := tinfo_cleol'access;
     when up =>      ap( 1 ) := tinfo_up'access;
     when right =>   ap( 1 ) := tinfo_right'access;
     when bel =>     ap( 1 ) := tinfo_bel'access;
     when reset =>   ap( 1 ) := tinfo_reset'access;
     when clear =>   ap( 1 ) := tinfo_clear'access;
     when lines =>   ap( 1 ) := tinfo_lines'access;
     when cols =>    ap( 1 ) := tinfo_cols'access;
     when others =>
       put_line( standard_output, "Internal error: no such tcap code" );
     end case;
  elsif tput_style = "termcap" then
     case attr is
     when normal =>  ap( 1 ) := tcap_normal'access;
     when bold =>    ap( 1 ) := tcap_bold'access;
     when inverse => ap( 1 ) := tcap_inverse'access;
     when cleop =>   ap( 1 ) := tcap_cleop'access;
     when cleol =>   ap( 1 ) := tcap_cleol'access;
     when up =>      ap( 1 ) := tcap_up'access;
     when right =>   ap( 1 ) := tcap_right'access;
     when bel =>     ap( 1 ) := tcap_bel'access;
     when reset =>   ap( 1 ) := tcap_reset'access;
     when clear =>   ap( 1 ) := tcap_clear'access;
     when lines =>   ap( 1 ) := tcap_lines'access;
     when cols =>    ap( 1 ) := tcap_cols'access;
     when others =>
       put_line( standard_output, "Internal error: no such tcap code" );
       ap( 1 ) := tcap_normal'access; -- prevent exception
     end case;
  else
       put_line( standard_output, "Internal error: unknown tput_style" );
       ap( 1 ) := tcap_normal'access; -- prevent exception
  end if;
 
  -- Export TERM variable

  findIdent( to_unbounded_string( "TERM" ), term_id );
  declare
     termString : string := "TERM=" & to_string( identifiers( term_id ).value ) & ASCII.NUL;
     status : integer;
  begin
     findIdent( to_unbounded_string( "TERM" ), term_id );
     if term_id /= eof_t then
        intResult := putenv( termString );
     end if;
 
     -- Run tput and restore stdout when done

     C_reset_errno; 
     spawn( tput_path1, ap, status, noReturn => false );
     if C_errno > 0 then
        spawn( tput_path2, ap, status, noReturn => false );
     end if;
  end;
  free_list( ap );
  if C_errno > 0 then
     result := dup2( oldstdout, stdout );
     close( oldstdout );
     close( tputResultsFD );
     put_line( standard_error, to_string( "Unable to find/run " & tput_path1 ) );
     return null_unbounded_string;
  end if;
  result := dup2( oldstdout, stdout );
  close( oldstdout );
  close( tputResultsFD );

  -- Un-export TERM
 -- if term_id /= eof_t then
 --    intResult := putenv( "TERM" & ASCII.NUL );
 -- end if;
  
  -- Read results
  
  tputResultsFD := open( tputResults & ASCII.NUL, 0, 660 );
  if tputResultsFD > 0 then
     loop
        read( amountRead, tputResultsFD, ch, 1 );
        if amountRead < 0 then
           if C_errno /= EINTR and C_errno /= EAGAIN then
              ttyCode := null_unbounded_string;
              exit;
           end if;
        elsif amountRead > 0 then
           ttyCode := ttyCode & ch;
        else
           exit;
        end if;
     end loop;
     close( tputResultsFD );
     result := aFileDescriptor( unlink( tputResults & ASCII.NUL ) );
  end if;

  return ttyCode;

  exception when others =>
       put( standard_error, "BUSH.Tty: Contraint thrown for " );
       put_line( standard_error, attr'img );
       return ttyCode;
end tput;

 
-- Attribute Procedures

procedure updateTtyAttributes( thisTerm : unbounded_string ) is
-- update the term array with the attributes for the display.
-- Run this procedure on BUSH startup or when the reset/clear
-- commands are used (in case the TERM variable has changed).
begin
  if lastTerm = thisTerm then
     return;
  end if;
  for attr in termAttributes loop
      term( attr ) := tput( attr );
  end loop;
  lastTerm := thisTerm;
end updateTtyAttributes;

procedure updateDisplayInfo is
-- update the displayInfo record with the display dimensions.
-- Run this procedure at startup or when a SIGWINCH is
-- detected. If ioctl() fails, try tput)
  res     : integer;
  ttyFile : aFileDescriptor;
begin
  displayInfo.row := 0; -- defaults
  displayInfo.col := 0;

  -- Get the terminal display dimensions

  ttyFile := open( "/dev/tty" & ASCII.NUL, 0, 660 );
  if ttyFile >= 0 then
     ioctl_TIOCGWINSZ( res, ttyFile, TIOCGWINSZ, displayInfo );
     close( ttyFile );
  end if;

  -- No tty device or other problem?  Get the defaults from tput

  if res < 0 or displayInfo.row <= 0 then
     displayInfo.row := short_integer'value( to_string( tput( lines ) ) );
  end if;
  if res < 0 or displayInfo.col <= 1 then
     displayInfo.col := short_integer'value( to_string( tput( cols ) ) );
  end if;

  -- Still no good?  Default to 80x24 display

  if displayInfo.row <= 0 or displayInfo.col <= 1 then
     displayInfo.row := 24;
     displayInfo.col := 80;
  end if;

  exception when others =>
    -- This occurs on templates where there is no terminal.  Just assume 80x24
    -- put_line( standard_error, "Internal error: exception thrown" );
    -- put_line( standard_error, "Assuming 80x24 and no special capabilities" );
    displayInfo.row := 24;
    displayInfo.col := 80;

end updateDisplayInfo;


-- Basic TTY I/O

procedure simpleGetKey( ch : out character ) is
-- read a (raw) key from the keyboard without echoing to display
  tio        : termios;         -- tty setting for raw input
  oldtio     : termios;         -- previous tty settings
  amountRead : long_integer;    -- characters read
  ttyFile    : aFileDescriptor; -- our controlling tty, for ioctl
  res        : integer;         -- results
begin
  ch := ASCII.NUL;
  if isatty( stdin ) = 0 then                      -- not a tty?
     read( amountRead, stdin, ch, 1 );             -- read a character
     if amountRead = 0 then                        -- nothing read?
        ch := ASCII.EOT;                           -- return a control-d
     end if;
  else                                         
     -- read character here (non-canonical, set by termios
     -- MIN = 1  & TIME = 0).  Disable special chars separately
     -- strip high bit.  Normally, canonical + 0 min + 0 time
     ttyFile := open( "/dev/tty" & ASCII.NUL, 0, 660 );
     if ttyFile < 0 then
        put( standard_error, "getKey: unable to read keyboard settings - " );
        put( standard_error, "open /dev/tty failed - " );
        put_line( standard_error, "error " & bush_os.C_errno'img );
        raise CONSTRAINT_ERROR;                   -- for lack of a better error
     else
        ioctl_getattr( res, ttyFile, TCGETATTR, tio );
        if res /= 0 then
           put( standard_error, "getKey: unable to load keyboard settings - " 
);
           put_line( standard_error, "ioctl /dev/tty TCGETATTR failed" );
           raise CONSTRAINT_ERROR;                -- for lack of a better error
        else
           oldtio := tio;
           -- these are based on "Linux Application Development"
           tio.c_lflag := tio.c_lflag and not ICANON;  -- no special chars
           tio.c_lflag := tio.c_lflag and not ECHO;    -- no print
           tio.c_lflag := tio.c_lflag and not ECHONL;  -- ditto
           tio.c_lflag := tio.c_lflag and not ECHOCTL; -- ditto
           tio.cc_min := character'val(1);   -- 1 character
           tio.cc_time := character'val(0);  -- no timeout
           -- these are based on Bash readline library, rltty.c
           -- this assumes no software flow control
           tio.c_iflag := tio.c_iflag and not IXON;    -- ctrl-s through
           tio.c_iflag := tio.c_iflag and not IXOFF;   -- ctrl-q through
           tio.c_iflag := tio.c_iflag and not IXANY;   -- shouldn't be needed
           tio.c_iflag := tio.c_iflag and not ICRNL;   -- no CR->NL conversion
           tio.c_iflag := tio.c_iflag and not INLCR;   -- or vice versa
           -- ISIG and some other stuff done by readline not
           -- done here.  Use ISIG to allow ctrl-c through.  For testing,
           -- leave alone.
           ioctl_setattr( res, ttyFile, TCSETATTR, tio ); -- raw read mode
           if res /= 0 then                            -- very unlikely
              put_line( "ioctl_set failed" );          -- but check anyway
              -- probably should raise an exception here
           else
              res := tcdrain( stdout );            -- flush pending output
<<retry>>     read( amountRead, stdin, ch, 1 );    -- read a character
              if amountRead = 0 then               -- nothing read?
                 ch := ASCII.EOT;                  -- return a control-d
              elsif amountRead < 0 then            -- error?
                 if bush_os.C_errno = EINTR and not wasSIGINT then
                    -- interrupted by signal (other than SIGINT)
                    goto retry;                    -- then try again
                 end if;
              end if;
              ioctl_setattr( res, ttyFile, TCSETATTR, oldtio ); -- restore settings
           end if;
        end if;
        close( ttyfile );
     end if;
     -- international character support now
     -- if ch > ASCII.DEL then
     --   -- constrain to lower ASCII character set
     --   ch := character'val( character'pos( ch ) - 128 );
     --end if;
  end if;
end simpleGetKey;

procedure simpleBeep is
-- ring the bell on the terminal (ie. send a control-G)
  result : integer;
  amountWritten : long_integer;
  ch : character;
begin
  for i in 1..length( term( bel ) ) loop
      ch := element( term( bel ), i );
      write( amountWritten, stderr, ch, 1 );
  end loop;
  result := tcdrain( stderr );
end simpleBeep;

end bush_os.tty;
