------------------------------------------------------------------------------
-- Communications and Errors                                                --
--                                                                          --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
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
use  ada.text_io;

with gnat.source_info,
     ada.characters.handling,
     ada.strings.fixed,
     ada.strings.unbounded.text_io,
     pegasoft.script_io,
     pegasoft.strings,
     pegasoft.user_io,
     spar_os.tty,
     world.utf8,
     -- TODO: references own parent.  This is messy to sort out.  Will
     -- deal with it later.
     scanner;

use  ada.characters.handling,
     ada.strings.unbounded.text_io,
     ada.strings.fixed,
     pegasoft.strings,
     pegasoft.script_io,
     pegasoft.user_io,
     spar_os,
     spar_os.tty,
     world.utf8,
     scanner;

package body scanner.communications is

templateErrorHeader : constant unbounded_string := to_unbounded_string( "SparForte says" );


-----------------------------------------------------------------------------
--
-- SUPPORT FUNCTIONS FOR MESSAGES
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PUT LINE RETRY
--
-- On some operating systems, put_line can raise a device_error when an
-- interrupted system call occurs.
-- TODO: this is a workaround.  A better solution is needed
-- TODO: should probably use these functions throughout
-----------------------------------------------------------------------------

procedure put_line_retry( output_file : file_type; s : string ) is
  retryCnt : natural := 0;
begin
  loop
     begin
        put_line( output_file, s );
        retryCnt := 2;
     exception when device_error =>
        retryCnt := retryCnt + 1;
     end;
     exit when retryCnt >= 2;
  end loop;
end put_line_retry;

procedure put_line_retry( s : string ) is
begin
  put_line_retry( standard_output, s );
end put_line_retry;

procedure put_line_retry( output_file : file_type; us : unbounded_string ) is
begin
  put_line_retry( output_file, to_string( us ) );
end put_line_retry;

procedure put_line_retry( us : unbounded_string ) is
begin
  put_line_retry( standard_output, to_string( us ) );
end put_line_retry;


-----------------------------------------------------------------------------
--  PUT RETRY
--
-- On some operating systems, put_line can raise a device_error when an
-- interrupted system call occurs.
-- TODO: this is a workaround.  A better solution is needed
-- TODO: should probably use these functions throughout
-----------------------------------------------------------------------------

procedure put_retry( output_file : file_type; s : string ) is
  retryCnt : natural := 0;
begin
  loop
     begin
        put( output_file, s );
        retryCnt := 2;
     exception when device_error =>
        retryCnt := retryCnt + 1;
     end;
     exit when retryCnt >= 2;
  end loop;
end put_retry;

procedure put_retry( s : string ) is
begin
  put_retry( standard_output, s );
end put_retry;

procedure put_retry( output_file : file_type; us : unbounded_string ) is
begin
  put_retry( output_file, to_string( us ) );
end put_retry;

procedure put_retry( us : unbounded_string ) is
begin
  put_retry( standard_output, to_string( us ) );
end put_retry;


-----------------------------------------------------------------------------
--  GET ERROR ICON
--
-- Return an appropriate error message icon, depending on the display context.
-- With --colour on the comand line, show a UTF icon.  When in a template,
-- the icon depends on the template.
-----------------------------------------------------------------------------

function getErrorIcon return messageStrings is
  errorIcon : messageStrings;
begin
  if not gccOpt then
     -- For different documents, fill in the error icon below
     -- An HTML template MIGHT use UTF-8 but we don't know for certain
     if hasTemplate then
        case templateHeader.templateType is
        when others =>
           null;
        end case;
     elsif colourOpt then
        errorIcon.templateMessage := to_unbounded_string( utf_ballot );
     end if;
  end if;
  -- no icon for text messages
  return errorIcon;
end getErrorIcon;


-----------------------------------------------------------------------------
--  GET CARET ICON
--
-- Return an appropriate caret message icon, depending on the display context.
-- With --colour on the comand line, show a UTF icon.  When in a template,
-- the icon depends on the template.
-----------------------------------------------------------------------------

function getCaretIcon return messageStrings is
  caretIcon : messageStrings;
begin
  if not gccOpt then
     -- For different documents, fill in the error icon below
     -- An HTML template MIGHT use UTF-8 but we don't know for certain
     if hasTemplate then
        case templateHeader.templateType is
        when others =>
           caretIcon.templateMessage := to_unbounded_string( "^" );
        end case;
     elsif colourOpt then
        caretIcon.templateMessage := to_unbounded_string( utf_triangle );
     else
        caretIcon.templateMessage := to_unbounded_string( "^" );
     end if;
     caretIcon.gccMessage := to_unbounded_string( "^" );
  end if;
  return caretIcon;
end getCaretIcon;


-----------------------------------------------------------------------------
--  GET LEFT CARET ICON
--
-- Return an appropriate caret message icon, depending on the display context.
-- With --colour on the comand line, show a UTF icon.  When in a template,
-- the icon depends on the template.
-----------------------------------------------------------------------------

function getLeftCaretIcon return messageStrings is
  caretIcon : messageStrings;
begin
  if not gccOpt then
     -- For different documents, fill in the error icon below
     -- An HTML template MIGHT use UTF-8 but we don't know for certain
     if hasTemplate then
        case templateHeader.templateType is
        when others =>
           caretIcon.templateMessage := to_unbounded_string( "^" );
        end case;
     elsif colourOpt then
        caretIcon.templateMessage := to_unbounded_string( utf_left );
     else
        caretIcon.templateMessage := to_unbounded_string( "^" );
     end if;
     caretIcon.gccMessage := to_unbounded_string( "^" );
  end if;
  return caretIcon;
end getLeftCaretIcon;


-----------------------------------------------------------------------------
--  GET RIGHT CARET ICON
--
-- Return an appropriate caret message icon, depending on the display context.
-- With --colour on the comand line, show a UTF icon.  When in a template,
-- the icon depends on the template.
-----------------------------------------------------------------------------

function getRightCaretIcon return messageStrings is
  caretIcon : messageStrings;
begin
  if not gccOpt then
     -- For different documents, fill in the error icon below
     -- An HTML template MIGHT use UTF-8 but we don't know for certain
     if hasTemplate then
        case templateHeader.templateType is
        when others =>
           caretIcon.templateMessage := to_unbounded_string( "^" );
        end case;
     elsif colourOpt then
        caretIcon.templateMessage := to_unbounded_string( utf_right );
     else
        caretIcon.templateMessage := to_unbounded_string( "^" );
     end if;
     caretIcon.gccMessage := to_unbounded_string( "^" );
  end if;
  return caretIcon;
end getRightCaretIcon;


-----------------------------------------------------------------------------
--  GET HORIZONTAL LINE ICON
--
-- Return an appropriate horizontal line, depending on the display context.
-- With --colour on the comand line, show a UTF character.  When in a
-- template, the icon depends on the template.
-----------------------------------------------------------------------------

function getHorizontalLineIcon( width : natural ) return messageStrings is
  lineIcon : messageStrings;
begin
  if not gccOpt then
     -- For different documents, fill in the error icon below
     -- An HTML template MIGHT use UTF-8 but we don't know for certain
     if hasTemplate then
        case templateHeader.templateType is
        when others =>
           lineIcon.templateMessage := to_unbounded_string( width * "-" );
        end case;
     elsif colourOpt then
        lineIcon.templateMessage := to_unbounded_string( width * utf_horizontalLineOnly );
     else
        lineIcon.templateMessage := to_unbounded_string( width * "-" );
     end if;
     lineIcon.gccMessage := to_unbounded_string( width * "-" );
  end if;
  return lineIcon;
end getHorizontalLineIcon;


-----------------------------------------------------------------------------
--  GET STACK TRACE
--
-- Return the active nested blocks for the current execution context in the
-- form of "x in y in z...".
-----------------------------------------------------------------------------

function getStackTrace return messageStrings is
  stackTrace : messageStrings;
begin
  if blocks_top > blocks'first then                            -- in a block?
     for i in reverse blocks'first..blocks_top-1 loop             -- show the
         -- while we could suppress "in" on the first block, if the block
         -- is the only block, you'd just get the block name.
         stackTrace := stackTrace & pl( " in " );                 -- traceback
         stackTrace := stackTrace & unb_pl( ToEscaped( blocks( i ).blockName ) );
     end loop;
  else                                                        -- if no blocks
     stackTrace := pl( "in script" );                     -- just "in script"
  end if;
  return stackTrace;
end getStackTrace;


-----------------------------------------------------------------------------
--  GET COMMAND POINTER
--
-- Return a pointer to the message in the line where an event occurred.  This
-- includes the indent to the given position, draws the pointer in either
-- ASCII or UTF characters, and a trailing UTF icon intended to go before
-- the message (if in colour mode).  For a shell word, provide a non-zero
-- offset to the position on the line.
-----------------------------------------------------------------------------

function getLinePointer( iconText : messageStrings; firstPos, lastPos : natural;
     wordOffset : natural := 0 ) return messageStrings is
  cmdPointer : messageStrings;
begin
  if script /= null then
     -- special case: a shell word offsets to the word position
     if wordOffset > 0 then
        cmdPointer :=
           unb_pl( ada.strings.unbounded.to_unbounded_string( (wordOffset-1) * " " ) );
        cmdPointer := cmdPointer & getCaretIcon;
     else
        -- the indent (first character is zero indent)
        cmdPointer :=
           unb_pl( ada.strings.unbounded.to_unbounded_string( (firstPos-1) * " " ) );
        -- the underline, if a range of characters
        if lastpos > firstpos then
           cmdPointer := cmdPointer & getLeftCaretIcon;
           cmdPointer := cmdPointer &
                 getHorizontalLineIcon(lastpos-firstPos-1);
           cmdPointer := cmdPointer & getRightCaretIcon;
        else
           -- the point, if a single character
           cmdPointer := cmdPointer & getCaretIcon;
       end if;
       -- the space after the pointer
       cmdPointer := cmdPointer & pl( " " );
    end if;
  end if;
  -- the message icon
  if iconText.templateMessage /= "" then
     cmdPointer := cmdPointer & iconText & pl( " " );
  end if;
  return cmdPointer;
end getLinePointer;


-----------------------------------------------------------------------------
--  GET GCC FORMAT ERROR MESSAGE
--
-- Return an error message in GCC format based on the given context.
-----------------------------------------------------------------------------

function getGCCFormatErrorMessage( lineno, firstpos, fileno : natural; msg : messageStrings ) return messageStrings is
  gccMsg      : unbounded_string;
  lineStr     : unbounded_string;
  firstPosStr : unbounded_string;
  sfr         : aSourceFile;
begin
  lineStr := to_unbounded_string( lineno'img );             -- remove leading
  if length( lineStr ) > 0 then                             -- space (if any)
     if element( lineStr, 1 ) = ' ' then
        delete( lineStr, 1, 1 );
     end if;
  end if;
  firstposStr := to_unbounded_string( firstpos'img );
  if length( firstposStr ) > 0 then                             -- here, too
     if element( firstposStr, 1 ) = ' ' then
        delete( firstposStr, 1, 1 );
     end if;
  end if;
  sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
  gccMsg := sfr.name
     & ":" & lineStr
     & ":" & firstposStr
     & ":";                                                  -- no traceback
     gccMsg := gccMsg & ' ';                                  -- token start
     gccMsg := gccMsg & msg.gccMessage;
  return unb_pl( gccMsg );
end getGCCFormatErrorMessage;


-----------------------------------------------------------------------------
--  GET GCC FORMAT ERROR MESSAGE
--
-- Return the message location.
-----------------------------------------------------------------------------

function getSparFormatMessageHeader( lineno, firstpos, distance_percent,
     fileno : natural ) return messageStrings is
  sparHeader : messageStrings;
  sfr        : aSourceFile;
begin
  sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
  sparHeader := unb_pl( sfr.name
     & ":" & lineno'img
     & ":" & firstpos'img
     & ":" & distance_percent'img & "%"
     & ":");
  return sparHeader;
end getSparFormatMessageHeader;


-----------------------------------------------------------------------------
--
-- BASIC ERROR MESSAGES (and similar messages)
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  GET SCRIPT POSITION MESSAGE
--
-- This is the form of a notification used in breakout mode for displaying
-- messages to standard error like "break" or "resuming", including the
-- script position.
-----------------------------------------------------------------------------

function get_script_execution_position( msg : messageStrings; utf_icon : string ) return unbounded_string is
  discardCmdLine    : messageStrings;
  firstpos   : natural;
  lastpos    : natural;
  lineno     : natural;
  distance_percent : natural;
  fileno     : natural;
  gccFormatMsg : messageStrings;
  needGccVersion : boolean := false;
  fullErrorMessage : messageStrings;
  -- this hides the global fullErrorMessage
begin
  -- Only create the abbreviated GCC-style error message if we need it
  --
  -- In the case of templates, we need both the Gcc version and the non-Gcc
  -- version of the error message.  In a CGI script that isn't a template,
  -- regular errors are reported back.

  needGccVersion := boolean( gccOpt ) or hasTemplate;

  -- Returns the current token position and the line number.
  -- We're not using cmdline.

  if script /= null then
     getCommandLine( discardCmdLine, firstpos, lastpos, lineno,
        distance_percent, fileno );
  else
     firstPos := 1;
     lastPos := 1;
  end if;

  -- Clear any old error messages from both the screen error and the
  -- template error (if one exists)

  fullErrorMessage := nullMessageStrings;

  -- Generate a Gcc-formatted error message (if we need one)

  if needGccVersion then
     if script /= null then
        gccFormatMsg := getGCCFormatErrorMessage(lineno, firstpos, fileno, msg );
      end if;
  end if;

  -- If in a script (that is, a non-interactive input mode) then
  -- show the location and traceback.  Otherwise, if we're just at
  -- the command prompt, don't bother with the location/traceback.

  if inputMode /= interactive and inputMode /= breakout then
     if script /= null then
        -- For the regular format, show the location and traceback in script
        fullErrorMessage := getSparFormatMessageHeader(lineno, firstpos,
          distance_percent, fileno ) & getStackTrace;
        fullErrorMessage := getNewLine & fullErrorMessage & getNewLine;
     end if;
  end if;

  -- header : stack trace <-- this is compete
  -- source line <-- doing this
  -- message

  -- Second, add the line the error occurred in

  declare
     formattedPreviousCmdLine : messageStrings;
     formattedCmdline : messageStrings;
  begin
    getTwoCommandLines( formattedPreviousCmdLine, formattedCmdline, firstpos,
       lastpos, lineno, distance_percent, fileno);
    if formattedPreviousCmdLine /= nullMessageStrings then
       fullErrorMessage := fullErrorMessage & formattedPreviousCmdLine & getNewLine;
    end if;
    fullErrorMessage := fullErrorMessage & formattedCmdline;
  end;

  -- header : stack trace <-- this is compete
  -- source line <-- this is complete
  -- message <-- doing this

  -- Third, add the error message
  --
  -- Draw the underline error pointer
  -- Pick the output format based on the user's preference and the template
  -- type.  If the user requests GCC format, use it even when in a template.

  if gccOpt then
     fullErrorMessage := gccFormatMsg;
  else
     fullErrorMessage := fullErrorMessage & getNewLine;
     fullErrorMessage := fullErrorMessage & getLinePointer( getErrorIcon, firstPos, lastPos );
     fullErrorMessage := fullErrorMessage & msg;
  end if;

  -- header : stack trace <-- this is compete
  -- source line <-- this is complete
  -- message <-- this is complete

  -- If we are in any mode of the development cycle except maintenance
  -- mode, create an error message to display.  If we're in maintenance
  -- mode, create an error message only if debug is enabled.

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        fullErrorMessage.templateMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & fullErrorMessage.templateMessage & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        fullErrorMessage.templateMessage := "/* " & templateErrorHeader & " " & fullErrorMessage.templateMessage &  " */";
     when xmlTemplate =>
        fullErrorMessage.templateMessage := "<!-- " & templateErrorHeader & " " & fullErrorMessage.templateMessage & " -->";
     when textTemplate =>
        fullErrorMessage.templateMessage := fullErrorMessage.templateMessage;
     when tomlTemplate | yamlTemplate =>
        fullErrorMessage.templateMessage := "# " & fullErrorMessage.templateMessage;
     when noTemplate | jsonTemplate =>
        null;
     end case;
     -- In the case of the template, the error output must always
     -- be in gcc format (a single line) for the web server log.
     --
     -- This affects exception handling since HTML output for template
     -- will differ from error message in exceptions package.  Also,
     -- format this for Apache by stripping out the boldface or
     -- other effects.
     --
     return gccFormatMsg.gccMessage;
  end if;
  return fullErrorMessage.templateMessage;
end get_script_execution_position;


-----------------------------------------------------------------------------
--  ERR SHELL
--
-- Stop execution and record an compile-time or run-time error.  Format the
-- error according to the user's preferences and set the error_found flag.
--
-- Only display the first error/exception encounted.
-----------------------------------------------------------------------------

procedure err_shell( msg : messageStrings; wordOffset : natural ) is
  discardCmdLine    : messageStrings;
  firstpos   : natural;
  lastpos    : natural;
  lineno     : natural;
  distance_percent : natural;
  fileno     : natural;
  gccFormatMsg : messageStrings;
  needGccVersion : boolean := false;
begin

  -- Already displayed one error or script is complete?
  -- Don't record any more errors.

  if error_found or done then
     return;
  end if;

  -- Only create the abbreviated GCC-style error message if we need it
  --
  -- In the case of templates, we need both the Gcc version and the non-Gcc
  -- version of the error message.  In a CGI script that isn't a template,
  -- regular errors are reported back.

  needGccVersion := boolean( gccOpt ) or hasTemplate;

  -- Returns the current token position and the line number.
  -- We're not using cmdline.

  if script /= null then
     getCommandLine( discardCmdLine, firstpos, lastpos, lineno,
        distance_percent, fileno );
  else
     firstPos := 1;
     lastPos := 1;
  end if;

  -- Clear any old error messages from both the screen error and the
  -- template error (if one exists)

  fullErrorMessage := nullMessageStrings;

  -- Generate a Gcc-formatted error message (if we need one)

  if needGccVersion then
     if script /= null then
        gccFormatMsg := getGCCFormatErrorMessage(lineno, firstpos, fileno, msg );
      end if;
  end if;

  -- If in a script (that is, a non-interactive input mode) then
  -- show the location and traceback.  Otherwise, if we're just at
  -- the command prompt, don't bother with the location/traceback.

  if inputMode /= interactive and inputMode /= breakout then
     if script /= null then
        -- For the regular format, show the location and traceback in script
        fullErrorMessage := getSparFormatMessageHeader(lineno,
          firstpos, distance_percent, fileno ) & getStackTrace;
        fullErrorMessage := fullErrorMessage & getNewLine;
     end if;
  end if;

  -- header : stack trace <-- this is compete
  -- source line <-- doing this
  -- message

  -- Second, add the line the error occurred in

  declare
     formattedPreviousCmdLine : messageStrings;
     formattedCmdline : messageStrings;
  begin
    getTwoCommandLines( formattedPreviousCmdLine, formattedCmdline, firstpos,
       lastpos, lineno, distance_percent, fileno);
    if formattedPreviousCmdLine /= nullMessageStrings then
       fullErrorMessage := fullErrorMessage & formattedPreviousCmdLine & getNewLine;
    end if;
    fullErrorMessage := fullErrorMessage & formattedCmdline;
  end;

  -- header : stack trace <-- this is compete
  -- source line <-- this is complete
  -- message <-- doing this

  -- Third, add the error message
  --
  -- Draw the underline error pointer
  -- Pick the output format based on the user's preference and the template
  -- type.  If the user requests GCC format, use it even when in a template.

  if gccOpt then
     fullErrorMessage := gccFormatMsg;
  else
     fullErrorMessage := fullErrorMessage & getNewLine;
     fullErrorMessage := fullErrorMessage & getLinePointer( getErrorIcon, firstPos, lastPos, wordOffset );
     fullErrorMessage := fullErrorMessage & msg;
  end if;

  -- header : stack trace <-- this is compete
  -- source line <-- this is complete
  -- message <-- this is complete

  -- If we are in any mode of the development cycle except maintenance
  -- mode, create an error message to display.  If we're in maintenance
  -- mode, create an error message only if debug is enabled.

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        fullErrorMessage.templateMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & fullErrorMessage.templateMessage & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";

     when cssTemplate | jsTemplate =>
        fullErrorMessage.templateMessage := "/* " & templateErrorHeader & " " & fullErrorMessage.templateMessage &  " */";
     when xmlTemplate =>
        fullErrorMessage.templateMessage := "<!-- " & templateErrorHeader & " " & fullErrorMessage.templateMessage & " -->";
     when textTemplate =>
        fullErrorMessage.templateMessage := fullErrorMessage.templateMessage;
     when tomlTemplate | yamlTemplate =>
        fullErrorMessage.templateMessage := "# " & fullErrorMessage.templateMessage;
     when noTemplate | jsonTemplate =>
        null;
     end case;
  end if;

     -- In the case of the template, the error output must always
     -- be in gcc format (a single line) for the web server log.
     --
     -- This affects exception handling since HTML output for template
     -- will differ from error message in exceptions package.  Also,
     -- format this for Apache by stripping out the boldface or
     -- other effects.

     fullErrorMessage.gccMessage := gccFormatMsg.gccMessage;

  -- Show that this is an error, not an exception

  error_found := true;                                          -- flag error
  err_exception.name := null_unbounded_string;            -- not an exception
  last_status := 0;

  -- If trace mode is enabled, show this as the point in the execution
  -- where the error occurred.

  if traceOpt then
     put_trace_error( "error: " & to_string( msg.gccMessage ), boolean( gccOpt ), boolean( colourOpt ) );
  end if;
end err_shell;


-----------------------------------------------------------------------------
--  ERR
--
-- Stop execution and record an compile-time or run-time error.  Format the
-- error according to the user's preferences and set the error_found flag.
--
-- Only display the first error/exception encounted.
-----------------------------------------------------------------------------

procedure err( msg : messageStrings ) is
begin
   err_shell( msg, 0 );
end err;


-----------------------------------------------------------------------------
--  ERR SYMBOL TABLE OVERFLOW
--
-- Report an error but structure is based on the "reporter questions", to
-- describe the context around the error as briefly as possible.  This
-- version represents an inconsistency between two identifiers.
-----------------------------------------------------------------------------

procedure err_symbol_table_overflow  is
begin
  err( inv( "there are too many identifiers for SparForte to handle (symbol table overflow)" ) );
end err_symbol_table_overflow;


-----------------------------------------------------------------------------
-- ERR STYLE
--
-- Display a style error.  It is not an error if the script is unstructured.
-----------------------------------------------------------------------------

procedure err_style( msg : messageStrings ) is
begin
   if scriptType = structured then
      err_shell( +"style issue: " & msg, 0 );
   end if;
end err_style;


-----------------------------------------------------------------------------
--  ERR EXCEPTION RAISED
--
-- General message when raising on when others =>
-----------------------------------------------------------------------------

procedure err_exception_raised is
begin
  err( +"an unexpected exception was raised" );
end err_exception_raised;


-----------------------------------------------------------------------------
--  ERR RENAMING
--
-- Show an error message for something disallowed for renaming
-----------------------------------------------------------------------------

procedure err_renaming( ident : identifier ) is
  renaming_id : identifier;
begin
  for i in reverse ident+1..identifiers_top-1 loop
       if not identifiers(i).deleted then
           if identifiers(i).renaming_of = ident then
               renaming_id := i;
                exit;
            end if;
        end if;
  end loop;
  if identifiers( ident ).renamed_count = 1 then
     err( +"renaming " &
          name_em( renaming_id ) &
          pl( " still refers to " ) &
          name_em( ident ) );
  else
     err( +"renaming " &
          name_em( renaming_id ) &
          pl( " (and others) still refer to " ) &
          name_em( ident) );
  end if;
end err_renaming;


-----------------------------------------------------------------------------
--  RAISE EXCEPTION
--
-- Like err, but for exceptions.  Stop execution and report a run-time
-- exception.  Set the error_found and exception-related global values.
-----------------------------------------------------------------------------

procedure raise_exception( msg : messageStrings ) is
  discardCmdline    : messageStrings;
  firstpos   : natural;
  lastpos    : natural;
  distance_percent : natural;
  lineStr    : unbounded_string;
  firstposStr : unbounded_string;
  lineno     : natural;
  fileno     : natural;
  outLine    : unbounded_string;
  gccOutLine : unbounded_string;
  needGccVersion : boolean := false;
  gccFormatMsg : messageStrings;
begin

  -- Already displayed one error or script is complete?
  -- Don't display any more

  if error_found or done then
     return;
  end if;

  -- Only create the abbreviated GCC-style error message if we need it
  --
  -- In the case of templates, we need both the Gcc version and the non-Gcc
  -- version of the error message.  In a CGI script that isn't a template,
  -- regular errors are reported back.

  needGccVersion := boolean( gccOpt ) or hasTemplate;

  -- Returns the current token position and the line number.
  --
  -- Exceptions cannot happen without a script so there is no script /= null
  -- test.

  getCommandLine( discardCmdline, firstpos, lastpos, lineno, distance_percent,
     fileno );

  -- Clear any old error messages from both the screen error and the
  -- template error (if one exists)

  fullErrorMessage := nullMessageStrings;

  -- Generate a Gcc-formatted error message (if we need one)

  if needGccVersion then
     gccFormatMsg := getGCCFormatErrorMessage(lineno, firstpos, fileno, msg );
  end if;

  -- If in a script (that is, a non-interactive input mode) then
  -- show the location and traceback.  Otherwise, if we're just at
  -- the command prompt, don't bother with the location/traceback.

  if inputMode /= interactive and inputMode /= breakout then
        -- For the regular format, show the location and traceback in script
        fullErrorMessage := getSparFormatMessageHeader(lineno, firstpos,
          distance_percent, fileno ) & getStackTrace;
     fullErrorMessage := fullErrorMessage & getNewLine;
  end if;

  -- For the normal version, we must follow the traceback with the
  -- message, error underline and show the exception message.

  -- Second, add the line the error occurred in

  declare
     formattedPreviousCmdLine : messageStrings;
     formattedCmdline : messageStrings;
  begin
    getTwoCommandLines( formattedPreviousCmdLine, formattedCmdline, firstpos,
       lastpos, lineno, distance_percent, fileno);
    if formattedPreviousCmdLine /= nullMessageStrings then
       fullErrorMessage := fullErrorMessage & formattedPreviousCmdLine & getNewLine;
    end if;
    fullErrorMessage := fullErrorMessage & formattedCmdline;
  end;

  -- Draw the underline error pointer

  if gccOpt then
     fullErrorMessage := gccFormatMsg;
  else
     fullErrorMessage := fullErrorMessage & getNewLine;
     fullErrorMessage := fullErrorMessage & getLinePointer( getErrorIcon, firstPos, lastPos );
     fullErrorMessage := fullErrorMessage & msg;
  end if;

  -- Even for a template, if the user selected gccOpt specifically,
  -- use it.

  -- Pick which format the user wants for the full message.

  if hasTemplate and ( boolean( debugOpt or not maintenanceOpt ) ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        fullErrorMessage.templateMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #9F6000; background-color: #FEEFB3; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-weight:bold; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#9f6000; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">!</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #9F6000; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & fullErrorMessage.templateMessage & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        fullErrorMessage.templateMessage := "/* " & templateErrorHeader & " " & fullErrorMessage.templateMessage &  " */";
     when xmlTemplate =>
        fullErrorMessage.templateMessage := "<!-- " & templateErrorHeader & " " & fullErrorMessage.templateMessage & " -->";
     when tomlTemplate | yamlTemplate =>
        fullErrorMessage.templateMessage := "# " & fullErrorMessage.templateMessage;
     when noTemplate | textTemplate | jsonTemplate =>
        null;
     end case;
     -- In the case of the template, the error output must always
     -- be in gcc format (a single line) for the web server log.
     --
     -- This affects exception handling since HTML output for template
     -- will differ from error message in exceptions package.  Also,
     -- format this for Apache by stripping out the boldface or
     -- other effects.
     --
     --fullErrorMessage := gccFormatMsg;

  end if;

  -- Show that this is an exception, not an error.  Do not erase
  -- err_exception.name.  The ParseRaise, etc. procedure will set
  -- err_exception.

  error_found := true;                                          -- flag error

  -- If trace mode is enabled, show this as the point in the execution
  -- where the error occurred.

  if traceOpt then
     put_trace_error( "exception: " & to_string( fullErrorMessage.gccMessage ), boolean( gccOpt ), boolean( colourOpt ) );
  end if;
end raise_exception;


-----------------------------------------------------------------------------
--  ERR TEST RESULT
--
-- Show a failed result from pragma test_result
-----------------------------------------------------------------------------

procedure err_test_result is
  discardCmdline    : messageStrings;
  firstpos   : natural;
  lastpos    : natural;
  lineStr    : unbounded_string;
  firstposStr : unbounded_string;
  lineno     : natural;
  distance_percent : natural;
  fileno     : natural;
  outLine    : unbounded_string;
  gccOutLine : messageStrings;
  needGccVersion : boolean := false;
  msg        : constant messageStrings := +"test failed";
  ourFullErrorMessage : messageStrings;
  ourFullTemplateErrorMessage : unbounded_string;
  gccFormatMsg : messageStrings;
begin
  -- determine if gcc format is requested or required
  needGccVersion := boolean( gccOpt ) or hasTemplate;

  -- Get the command line position.  We don't care about the command
  -- line.  Script should always exist so a null script check is not done.

  getCommandLine( discardCmdline, firstpos, lastpos, lineno, distance_percent, fileno );

  -- Generate a Gcc-formatted error message (if we need one)

  if needGccVersion then
     if script /= null then
        gccFormatMsg := getGCCFormatErrorMessage(lineno, firstpos, fileno, msg );
      end if;
  end if;

  -- If in a script (that is, a non-interactive input mode) then
  -- show the location and traceback.  Otherwise, if we're just at
  -- the command prompt, don't bother with the location/traceback.

  if inputMode /= interactive and inputMode /= breakout then
     if script /= null then
        -- For the regular format, show the location and traceback in script
        ourFullErrorMessage := getSparFormatMessageHeader(lineno,
          firstpos, distance_percent, fileno ) & getStackTrace;
     end if;
     ourFullErrorMessage := ourFullErrorMessage & getNewLine;
  end if;

  declare
     formattedCmdline : messageStrings;
  begin
    getCommandLine( formattedCmdline, firstpos, lastpos, lineno, distance_percent, fileno );
    ourFullErrorMessage := ourFullErrorMessage & formattedCmdline;
  end;

  if gccOpt then
     ourFullErrorMessage := gccFormatMsg;
  else
     ourFullErrorMessage := ourFullErrorMessage & getNewLine;
     ourFullErrorMessage := ourFullErrorMessage & getLinePointer( getErrorIcon, firstPos, lastPos );
     ourFullErrorMessage := ourFullErrorMessage & msg;
  end if;

  -- If we are in any mode of the development cycle except maintenance
  -- mode, create an error message to display.  If we're in maintenance
  -- mode, create an error message only if debug is enabled.

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        ourFullTemplateErrorMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & ourFullErrorMessage.templateMessage & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        ourFullTemplateErrorMessage := "/* " & templateErrorHeader & " " & ourFullErrorMessage.templateMessage &  " */";
     when xmlTemplate =>
        ourFullTemplateErrorMessage := "<!-- " & templateErrorHeader & " " & ourFullErrorMessage.templateMessage & " -->";
     when tomlTemplate | yamlTemplate =>
        ourFullTemplateErrorMessage := "# " & ourFullErrorMessage.templateMessage;
     when noTemplate | textTemplate | jsonTemplate =>
        null;
     end case;
     -- In the case of the template, the error output must always
     -- be in gcc format (a single line) for the web server log.
     --
     -- This affects exception handling since HTML output for template
     -- will differ from error message in exceptions package.  Also,
     -- format this for Apache by stripping out the boldface or
     -- other effects.
     --
     -- TODO: document this
     ourFullErrorMessage := gccFormatMsg;
  end if;

  -- Show that this is an error, not an exception

  -- this doesn't raise an exception or error
  -- err_exception.name := null_unbounded_string;            -- not an exception
  -- last_status := 0;

  -- It is redundanct to output the message when tracing since it is always
  -- output when it occurs anyway...it's not an error or exception.

  -- Show the test result message immediately

  put_line_retry( standard_error, ourFullErrorMessage.gccMessage );
  -- may or may not have a template at this point, so check
  if hasTemplate then
     putTemplateHeader( templateHeader );
     put_line_retry( ourFullTemplateErrorMessage );
  end if;

end err_test_result;


-----------------------------------------------------------------------------
--  WARN
--
-- Issue a warning.  This is done immediately, is not formatted by gcc-style
-- preference and is not stored.
-----------------------------------------------------------------------------

procedure warn( msg : messageStrings ) is
  location : unbounded_string;
  fullMsg  : unbounded_string;
begin
  location := scriptFilePath & ":" & getLineNo'img & ": ";
  fullMsg  := location & "warning--" & msg.templateMessage;

  put_line_retry( standard_error, fullMsg );

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        put_retry( "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & fullMsg & "</p>" &
           "</div>" &
           "</div>" &
           "<br />" );
     when cssTemplate | jsTemplate =>
        put_retry( "/* " & templateErrorHeader & " " & fullMsg &  " */" );
     when xmlTemplate =>
        put_retry( "<!-- " & templateErrorHeader & " " & fullMsg & " -->" );
     when tomlTemplate | yamlTemplate =>
        put_retry( "# " & fullMsg );
     when noTemplate | textTemplate | jsonTemplate =>
        put_retry( fullMsg );
     end case;
  end if;
end warn;


-----------------------------------------------------------------------------
--  ERR PREVIOUS
--
-- Same as err below, but don't hilight the current token because the error
-- actually happened before it.  Just mark start of current token.
-----------------------------------------------------------------------------

procedure err_previous( msg : messageStrings ) is
  savepos : integer;
begin
  savepos := lastpos;     -- save current token's last character position
  lastpos := firstpos;    -- token length is one to produce a single '^'
  err( msg );             -- show message pointing at first character
  lastpos := savepos;     -- restore current token's last character position
end err_previous;


-----------------------------------------------------------------------------
-- ERR (English)
--
-- Report an error but structure is based on the "reporter questions", to
-- describe the context around the error as briefly as possible.
--
-- Parameter         Meaning                   Example
-- [subject]         The token in question     -
-- [subjectKind]     The type of token         -
-- [where]           Token alternate location  "(" declared at ")"
-- [what]            The situation             "For the in / not in range"
-- [whatKind]        The situation data type   universal_numeric
-- [why]             What the language wants   the range expects '..'
-- [how (to remedy)] Possible solutions        (Perhaps ...) -
--
-- See also expect functions.
--
-- It is a difficult balance to provide context without making the error
-- message too long.  I've noticed some other languages have tried to create
-- more descriptive error messages, or spend a lot of effort on the "why"
-- (how to correct the ere).  My goal here is to provide more context of
-- why a certain token is expected, rather than the correction.
--
-- In this general version, context, subject and obstructor are supplied as
-- text.  The text can have mark-up.  Each of these may be optional.
-----------------------------------------------------------------------------

procedure err(
    userLanguage    : englishUserLanguage;
    contextNotes    : messageStrings;                  -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : messageStrings;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructorNotes : messageStrings;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
  msg       : messageStrings;
  blockName : unbounded_string;
  needMore  : boolean := false;
  subjectLocationNeedsParen : boolean := false;
begin

  -- The context
  --
  -- If an identifier is provided, use it first.  Otherwise use textual notes.
  -- If no context was provided, use the current block name for the context.

  if not quietOpt then
     if contextNotes = nullMessageStrings then
        if blocks_top > block'first then
           blockName := getBlockName( blocks_top-1 );
        end if;
        if blockName /= null_unbounded_string then
           msg := unb_pl( "In " & blockName );
        end if;
     elsif Is_Upper( head( contextNotes.gccMessage, 1 ) ) then
        msg := contextNotes;
     else
        msg := pl( "While " ) & contextNotes;
     end if;

     if contextType /= eof_t then
        msg := msg & pl( " (" ) &
         ( em( to_string( toEscaped( AorAN( identifiers( contextType ).name ) ) ) ) &
         pl( ")" ) );
     elsif msg /= nullMessageStrings then
       msg := msg & pl( "," );
     end if;
  end if;

  -- Subject
  --
  -- This is the identifier the error refers to.  If no identifier is provided,
  -- use textual notes.

  if subjectNotes /= nullMessageStrings then
     if msg /= nullMessageStrings then
        msg := msg & pl(" ") & subjectNotes;
     else
        -- if not context, upper-case first letter of subject notes
        -- However, we can't tell what the first letter is because these are
        -- formatted by this point.  Upper-case must happen earlier.
        --msg.templateMessage := msg.templateMessage &
        --   ToUpper( Head( subjectNotes.templateMessage, 1 ) & "" ) &
        --   Slice( subjectNotes.templateMessage, 2, length( subjectNotes.templateMessage ) ) ;
        --msg.gccMessage     := msg.gccMessage &
        --   ToUpper( Head( subjectNotes.gccMessage, 1 ) & "" ) &
        --   Slice( subjectNotes.gccMessage, 2, length( subjectNotes.gccMessage ) ) ;
        msg := msg & subjectNotes;
     end if;
  end if;

  if subjectType /= eof_t then
     -- if the type is keyword, it's not really meaningful
     if subjectType /= keyword_t then
        if subjectType = new_t then
           msg := msg & pl( " (" ) & em( "not declared" );
        else
           msg := msg & pl( " (" ) & em( to_string( toEscaped( AorAN( identifiers( subjectType ).name ) ) ) );
        end if;
        if subjectLocation = nullMessageStrings then
           msg := msg & pl( ")" );
        end if;
     else
       subjectLocationNeedsParen := true;
     end if;
     if subjectLocation /= nullMessageStrings then
        if subjectType = eof_t or subjectLocationNeedsParen then
           msg := msg & pl( " (at " );
        else
           msg := msg & pl( ", at " );
        end if;
        msg := msg & subjectLocation & pl( ")" );
     end if;
  elsif subjectLocation /= nullMessageStrings then
     msg := msg & pl( " (at " ) & subjectLocation & pl( ")" );
  end if;

  -- Reason
  --
  -- The error message.

  if msg /= nullMessageStrings then
     msg := msg & pl( " " );
  end if;
  msg := msg & reason;

  -- Obstructor
  --
  -- The identifier causing the error for the subject.  If no identifier is
  -- given, use the textual notes.

  if obstructorNotes /= nullMessageStrings then
     if msg /= nullMessageStrings then
        msg := msg & pl( " " );
     end if;
    msg := msg & obstructorNotes;
  end if;
  if obstructorType /= eof_t then
     -- In the interest of brevity, do not show the type unless it is
     -- different than the subject type.  If the subject and obstructor
     -- types are the same, they're probably not related to the error,
     -- or it's implied that they are the same..
     declare
        obstructorTypeNeeded : boolean := false;
     begin
        if subjectType = eof_t then
           obstructorTypeNeeded := true;
        elsif obstructorType /= subjectType then
           obstructorTypeNeeded := true;
        end if;
        if obstructorTypeNeeded then
           msg := msg & pl( " (" ) & ( em( to_string( toEscaped( AorAN( identifiers( obstructorType ).name ) ) ) ) & pl( ")" ) );
        end if;
     end;
  end if;

  -- Remedy
  --
  -- Suggestions to correct the problem.  In quiet mode, include (more)
  -- with the message.

  if remedy /= nullMessageStrings then
     if not boolean(quietOpt) then
        msg := msg & pl( ". Perhaps " ) & remedy;
     else
        needMore := true;
     end if;
  end if;

  -- See Also
  --
  -- Usually a reference to the documentation.

  if seeAlso /= nullMessageStrings then
     if not boolean(quietOpt) then
         msg := msg & pl( ". See also " ) & seeAlso;
     else
         needMore := true;
     end if;
  end if;

  if needMore then
     msg := msg & pl( ". (More)" );
  end if;

  err( msg );
end err;

-----------------------------------------------------------------------------
--  ERR (English)
--
-- Variation for context, subject, obstructor all identifiers
-----------------------------------------------------------------------------

procedure err(
    userLanguage    : englishUserLanguage;
    context         : identifier;              -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
  contextNotes    : messageStrings;
  subjectNotes    : messageStrings;
  obstructorNotes : messageStrings;
begin
  if context /= eof_t then
    if context < keywords_top then
      contextNotes := pl( "In this " ) & em( to_string( toEscaped( identifiers( context ).name ) ) );
    else
      contextNotes := pl( "In " ) & em( to_string( toEscaped( identifiers( context ).name ) ) );
    end if;
  end if;

  subjectNotes := em( to_string( toCtrlEscaped( identifiers( subject ).name ) ) );
  if obstructor = eof_t then
     obstructorNotes := em( "end of file" );
  else
     obstructorNotes := em( to_string( toCtrlEscaped( identifiers( obstructor ).name ) ) );
  end if;

  err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => contextNotes,
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => subjectNotes,
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => obstructorNotes,
    remedy          => remedy,
    seeAlso         => seeAlso
  );
end err;

-----------------------------------------------------------------------------
--  ERR (English)
--
-- Variation for context notes, subject identifier, obstructor identifier
------------------------------------------------------------------------------

procedure err(
    userLanguage    : englishUserLanguage;
    contextNotes    : messageStrings := nullMessageStrings;            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
  subjectNotes    : messageStrings;
  obstructorNotes : messageStrings;
begin
   subjectNotes := em( to_string( toCtrlEscaped( identifiers( subject ).name ) ) );
   if obstructor = eof_t then
      obstructorNotes := em( "end of file" );
   else
      obstructorNotes := em( to_string( toCtrlEscaped( identifiers( obstructor ).name ) ) );
   end if;

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => contextNotes,
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => subjectNotes,
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => obstructorNotes,
    remedy          => remedy,
    seeAlso         => seeAlso
  );
end err;

-----------------------------------------------------------------------------
--  ERR (English)
--
-- Variation for context identifier, subject notes, obstructor identifier
------------------------------------------------------------------------------

procedure err(
    userLanguage    : englishUserLanguage;
    context         : identifier;              -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : messageStrings;                   -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
  contextNotes    : messageStrings;
  obstructorNotes : messageStrings;
begin
  if context /= eof_t then
    if context < keywords_top then
      contextNotes := pl( "In this " ) & em( to_string( toCtrlEscaped( identifiers( context ).name ) ) );
    else
      contextNotes := pl( "In " ) & em( to_string( toCtrlEscaped( identifiers( context ).name ) ) );
    end if;
  end if;
   if obstructor = eof_t then
      obstructorNotes := em( "end of file" );
   else
      obstructorNotes := em( to_string( toCtrlEscaped( identifiers( obstructor ).name ) ) );
   end if;

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => contextNotes,
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => subjectNotes,
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => obstructorNotes,
    remedy          => remedy,
    seeAlso         => seeAlso
  );
end err;

-----------------------------------------------------------------------------
--  ERR (English)
--
-- Variation for context identifier, subject identifier, obstructor notes
------------------------------------------------------------------------------

procedure err(
    userLanguage    : englishUserLanguage;
    context         : identifier;              -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructorNotes : messageStrings;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
  contextNotes    : messageStrings;
  subjectNotes    : messageStrings;
begin
  if context /= eof_t then
    if context < keywords_top then
      contextNotes := pl( "In this " ) & em( to_string( toCtrlEscaped( identifiers( context ).name ) ) );
    else
      contextNotes := pl( "In " ) & em( to_string( toCtrlEscaped( identifiers( context ).name ) ) );
    end if;
  end if;
   subjectNotes := em( to_string( toCtrlEscaped( identifiers( subject ).name ) ) );

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => contextNotes,
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => subjectNotes,
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => obstructorNotes,
    remedy          => remedy,
    seeAlso         => seeAlso
  );
end err;

-----------------------------------------------------------------------------
--  ERR (English)
--
-- Variation for context notes, subject notes, obstructor identifier
------------------------------------------------------------------------------

procedure err(
    userLanguage    : englishUserLanguage;
    contextNotes    : messageStrings := nullMessageStrings;            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : messageStrings;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
  obstructorNotes : messageStrings;
begin
   obstructorNotes := em( to_string( toCtrlEscaped( identifiers( obstructor ).name ) ) );
   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => contextNotes,
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => subjectNotes,
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => obstructorNotes,
    remedy          => remedy,
    seeAlso         => seeAlso
  );
end err;

-----------------------------------------------------------------------------
--  ERR (English)
--
-- Variation for context notes, subject identifier, obstructor notes
------------------------------------------------------------------------------

procedure err(
    userLanguage    : englishUserLanguage;
    contextNotes    : messageStrings := nullMessageStrings;            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructorNotes : messageStrings;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
  subjectNotes    : messageStrings;
begin
   subjectNotes := em( to_string( toCtrlEscaped( identifiers( subject ).name ) ) );

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => contextNotes,
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => subjectNotes,
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => obstructorNotes,
    remedy          => remedy,
    seeAlso         => seeAlso
  );
end err;

-----------------------------------------------------------------------------
--  ERR (English)
--
-- Variation for context identifier, subject notes, obstructor notes
------------------------------------------------------------------------------

procedure err(
    userLanguage    : englishUserLanguage;
    context         : identifier;              -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : messageStrings;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructorNotes : messageStrings;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
  contextNotes    : messageStrings;
begin
  if context /= eof_t then
    if context < keywords_top then
      contextNotes := pl( "In this " ) & em( to_string( toCtrlEscaped( identifiers( context ).name ) ) );
    else
      contextNotes := pl( "In " ) & em( to_string( toCtrlEscaped( identifiers( context ).name ) ) );
    end if;
  end if;

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => contextNotes,
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => subjectNotes,
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => obstructorNotes,
    remedy          => remedy,
    seeAlso         => seeAlso
  );
end err;


------------------------------------------------------------------------------
-- New-style Errors: Error Functions
------------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- ERR (Wrapper)
--
-- Report an error but structure is based on the "reporter questions", to
-- describe the context around the error as briefly as possible.  Choose an
-- error subprogram based on the user's language.
-----------------------------------------------------------------------------

procedure err(
    contextNotes    : messageStrings := nullMessageStrings;            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : messageStrings;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructorNotes : messageStrings;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
begin
    err(
       userLanguage => userLanguage.all,
       contextNotes => contextNotes,
       contextType => contextType,
       subjectNotes => subjectNotes,
       subjectType => subjectType,
       subjectLocation => subjectLocation,
       reason => reason,
       obstructorNotes => obstructorNotes,
       obstructorType => obstructorType,
       remedy => remedy,
       seeAlso         => seeAlso
    );
end err;

-----------------------------------------------------------------------------
--  ERR (Wrapper)
--
-- Variation for context, subject, obstructor all identifiers
-----------------------------------------------------------------------------

procedure err(
    context         : identifier;              -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
begin
   err(
       userLanguage    => userLanguage.all,
       context         => context,
       contextType     => contextType,
       subject         => subject,
       subjectType     => subjectType,
       subjectLocation => subjectLocation,
       reason          => reason,
       obstructor      => obstructor,
       obstructorType  => obstructorType,
       remedy          => remedy,
       seeAlso         => seeAlso
  );
end err;


-----------------------------------------------------------------------------
--  ERR (Wrapper)
--
-- Variation for context notes, subject identifier, obstructor identifier
------------------------------------------------------------------------------

procedure err(
    contextNotes    : messageStrings := nullMessageStrings;            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
begin
   err(
       userLanguage    => userLanguage.all,
       contextNotes    => contextNotes,
       contextType     => contextType,
       subject         => subject,
       subjectType     => subjectType,
       subjectLocation => subjectLocation,
       reason          => reason,
       obstructor      => obstructor,
       obstructorType  => obstructorType,
       remedy          => remedy,
       seeAlso         => seeAlso
  );
end err;

-----------------------------------------------------------------------------
--  ERR (Wrapper)
--
-- Variation for context identifier, subject notes, obstructor identifier
------------------------------------------------------------------------------

procedure err(
    context         : identifier;              -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : messageStrings;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
begin
   err(
       userLanguage    => userLanguage.all,
       context         => context,
       contextType     => contextType,
       subjectNotes    => subjectNotes,
       subjectType     => subjectType,
       subjectLocation => subjectLocation,
       reason          => reason,
       obstructor      => obstructor,
       obstructorType  => obstructorType,
       remedy          => remedy,
       seeAlso         => seeAlso
  );
end err;

-----------------------------------------------------------------------------
--  ERR (Wrapper)
--
-- Variation for context identifier, subject identifier, obstructor notes
------------------------------------------------------------------------------

procedure err(
    context         : identifier;              -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructorNotes : messageStrings;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
begin
   err(
       userLanguage    => userLanguage.all,
       context         => context,
       contextType     => contextType,
       subject         => subject,
       subjectType     => subjectType,
       subjectLocation => subjectLocation,
       reason          => reason,
       obstructorNotes => obstructorNotes,
       obstructorType  => obstructorType,
       remedy          => remedy,
       seeAlso         => seeAlso
  );
end err;


-----------------------------------------------------------------------------
--  ERR (Wrapper)
--
-- Variation for context notes, subject notes, obstructor identifier
------------------------------------------------------------------------------

procedure err(
    contextNotes    : messageStrings := nullMessageStrings;            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : messageStrings;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
begin
   err(
       userLanguage    => userLanguage.all,
       contextNotes    => contextNotes,
       contextType     => contextType,
       subjectNotes    => subjectNotes,
       subjectType     => subjectType,
       subjectLocation => subjectLocation,
       reason          => reason,
       obstructor      => obstructor,
       obstructorType  => obstructorType,
       remedy          => remedy,
       seeAlso         => seeAlso
  );
end err;


-----------------------------------------------------------------------------
--  ERR (Wrapper)
--
-- Variation for context notes, subject identifier, obstructor notes
------------------------------------------------------------------------------

procedure err(
    contextNotes    : messageStrings := nullMessageStrings;            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructorNotes : messageStrings;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
begin
   err(
       userLanguage    => userLanguage.all,
       contextNotes    => contextNotes,
       contextType     => contextType,
       subject         => subject,
       subjectType     => subjectType,
       subjectLocation => subjectLocation,
       reason          => reason,
       obstructorNotes => obstructorNotes,
       obstructorType  => obstructorType,
       remedy          => remedy,
       seeAlso         => seeAlso
  );
end err;


-----------------------------------------------------------------------------
--  ERR (Wrapper)
--
-- Variation for context identifier, subject notes, obstructor notes
------------------------------------------------------------------------------

procedure err(
    context         : identifier;              -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : messageStrings;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : messageStrings := nullMessageStrings;            -- where it was
    reason          : messageStrings;                  -- problem description
    obstructorNotes : messageStrings;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : messageStrings := nullMessageStrings;            -- suggested solutions
    seeAlso         : messageStrings := nullMessageStrings ) is
begin
   err(
       userLanguage    => userLanguage.all,
       context         => context,
       contextType     => contextType,
       subjectNotes    => subjectNotes,
       subjectType     => subjectType,
       subjectLocation => subjectLocation,
       reason          => reason,
       obstructorNotes => obstructorNotes,
       obstructorType  => obstructorType,
       remedy          => remedy,
       seeAlso         => seeAlso
  );
end err;


-----------------------------------------------------------------------------
--  EXPECT
--
-- Check for the specified identifier.  If the current token matches,
-- get the next token, otherwise show an error.
-----------------------------------------------------------------------------
-- TODO: to be deprecated

procedure expect( expected_token : identifier ) is
begin
  if token /= expected_token then
     if expected_token = keyword_t then
        err( +"keyword expected" );
     elsif expected_token = number_t then
        err( +"number expected" );
     elsif expected_token = strlit_t then
        err( +"string literal expected" );
     elsif expected_token = symbol_t then
        err( +"symbol expected" );
     else
        err( pl( to_string( identifiers( expected_token ).name ) & " expected" ) );
     end if;
  end if;
  getNextToken;
end expect;


-----------------------------------------------------------------------------
--  EXPECT
--
-- Check for the specified identifier and value.  If the current token
-- and its value matches, get the next token, otherwise show an error.
-----------------------------------------------------------------------------
-- TODO: deprecated - to be removed

procedure expect( expected_token : identifier; value : string ) is
begin
  if token /= expected_token then
     -- these are special tokens that won't display meaningful names
     if expected_token = keyword_t then
        err( +"keyword expected" );
     elsif expected_token = number_t then
        err( +"number expected" );
     elsif expected_token = strlit_t then
        err( +"string literal expected" );
     elsif expected_token = symbol_t then
        err( +"symbol expected" );
     elsif expected_token = eof_t then
        err( +"end of script expected" );
     else
        err( pl( to_string( identifiers( expected_token ).name ) & " expected" ) );
     end if;
  end if;
  if value /= to_string( identifiers( token ).value.all ) then
      err( pl( "'" & value & "' expected" ) );
  end if;
  getNextToken;
end expect;


-----------------------------------------------------------------------------
--  EXPECT SYMBOL
--
-- For a given punctuation symbol, read it if is present.  Otherwise, show
-- an error for an unexpected symbol.  The other "expect" procedure
-- involving punctuation symbols extend this procedure.
-----------------------------------------------------------------------------

procedure expectSymbol(
    expectedValue   : string;
    expectPlural    : boolean := false;
    context         : identifier;
    subject         : identifier;
    subjectType     : identifier := eof_t;
    subjectLocation : messageStrings := nullMessageStrings;
    reason          : messageStrings := nullMessageStrings;
    remedy          : messageStrings := nullMessageStrings ) is
  expectOrExpects : unbounded_string := to_unbounded_string( "expect" );
begin
  if token /= symbol_t or else expectedValue /= to_string( identifiers( token ).value.all ) then

     if not expectPlural then
        expectOrExpects := expectOrExpects & "s";
     end if;
     err(
        userLanguage => userLanguage.all,
        context => context,
        subject => subject,
        subjectType => subjectType,
        subjectLocation => subjectLocation,
        reason => reason & pl( " " ) &  unb_em( expectOrExpects ),
        obstructorNotes =>  em( to_string( toEscaped( to_unbounded_string( "'" & expectedValue & "'" ) ) ) ),
        remedy => remedy
     );
  end if;
  getNextToken;
end expectSymbol;

procedure expectSymbol(
    expectedValue   : string;
    expectPlural    : boolean := false;
    contextNotes    : messageStrings := nullMessageStrings;
    subject         : identifier;
    subjectType     : identifier := eof_t;
    subjectLocation : messageStrings := nullMessageStrings;
    reason          : messageStrings := nullMessageStrings;
    remedy          : messageStrings := nullMessageStrings ) is
  expectOrExpects : unbounded_string := to_unbounded_string( "expect" );
begin
  if token /= symbol_t or else expectedValue /= to_string( identifiers( token ).value.all ) then

     if not expectPlural then
        expectOrExpects := expectOrExpects & "s";
     end if;
     err(
        userLanguage => userLanguage.all,
        contextNotes => contextNotes,
        subject => subject,
        subjectType => subjectType,
        subjectLocation => subjectLocation,
        reason => reason & pl( " " ) &  unb_em( expectOrExpects ),
        obstructorNotes =>  em( to_string( toEscaped( to_unbounded_string( "'" & expectedValue & "'" ) ) ) ),
        remedy => remedy
     );
  end if;
  getNextToken;
end expectSymbol;

procedure expectSymbol(
    expectedValue   : string;
    expectPlural    : boolean := false;
    context         : identifier;
    subjectType     : identifier := eof_t;
    subjectLocation : messageStrings := nullMessageStrings;
    subjectNotes    : messageStrings := nullMessageStrings;
    reason          : messageStrings := nullMessageStrings;
    remedy          : messageStrings := nullMessageStrings ) is
  expectOrExpects : unbounded_string := to_unbounded_string( "expect" );
begin
  if token /= symbol_t or else expectedValue /= to_string( identifiers( token ).value.all ) then

     if not expectPlural then
        expectOrExpects := expectOrExpects & "s";
     end if;
     err(
        userLanguage => userLanguage.all,
        context => context,
        subjectNotes=> subjectNotes,
        subjectType => subjectType,
        subjectLocation => subjectLocation,
        reason => reason & pl( " " ) &  unb_em( expectOrExpects ),
        obstructorNotes =>  em( to_string( toEscaped( to_unbounded_string( "'" & expectedValue & "'" ) ) ) ),
        remedy => remedy
     );
  end if;
  getNextToken;
end expectSymbol;

procedure expectSymbol(
    expectedValue   : string;
    expectPlural    : boolean := false;
    contextNotes    : messageStrings := nullMessageStrings;
    subjectType     : identifier := eof_t;
    subjectLocation : messageStrings := nullMessageStrings;
    subjectNotes    : messageStrings := nullMessageStrings;
    reason          : messageStrings := nullMessageStrings;
    remedy          : messageStrings := nullMessageStrings ) is
  expectOrExpects : unbounded_string := to_unbounded_string( "expect" );
begin
  if token /= symbol_t or else expectedValue /= to_string( identifiers( token ).value.all ) then

     if not expectPlural then
        expectOrExpects := expectOrExpects & "s";
     end if;
     err(
        userLanguage => userLanguage.all,
        contextNotes => contextNotes,
        subjectNotes=> subjectNotes,
        subjectType => subjectType,
        subjectLocation => subjectLocation,
        reason => reason & pl( " " ) &  unb_em( expectOrExpects ),
        obstructorNotes =>  em( to_string( toEscaped( to_unbounded_string( "'" & expectedValue & "'" ) ) ) ),
        remedy => remedy
     );
  end if;
  getNextToken;
end expectSymbol;


-----------------------------------------------------------------------------
--  EXPECT IDENTIFIER
--
-----------------------------------------------------------------------------

procedure expectIdentifier( what, receivedDescription : string ) is
begin
  if what /= "" then
     err( +"an " & em( "identifier") & pl( " for " & what & " was expected but this looks like " ) & em( receivedDescription ) );
  else
     err( +"an " & em( "identifier") & pl( " was expected but this looks like " ) & em( receivedDescription ) );
  end if;
end expectIdentifier;


-----------------------------------------------------------------------------
-- Missing Round Bracket / Paranthesis
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  EXPECT STATEMENT SEMICOLON
--
-- Read a semicolon, if it is present.  Otherwise, show an error related to
-- a missing semicolon at the end of a statement.
-----------------------------------------------------------------------------

procedure expectStatementSemicolon( context : identifier := eof_t ) is
begin
  -- in interactive modes, a comment will hide the semi-colon automatically
  -- added by SparForte.  This can also happen with `..`
  if token = eof_t then
     expectSymbol(
       expectedValue => ";",
       context => context,
       reason =>+ "the end of statement",
       remedy => +"a comment or unescaped '--' has hidden a ';'"
     );
  end if;
  -- Common typo
  if token = symbol_t and identifiers( token ).value.all = ":" then
     expectSymbol(
       expectedValue => ";",
       context => context,
       reason => +"':' could be a mistake because the end of the statement"
     );
  else
     expectSymbol(
       expectedValue => ";",
       context => context,
       reason => +"the end of the statement"
    );
  end if;
end expectStatementSemicolon;

procedure expectStatementSemicolon( contextNotes : messageStrings := nullMessageStrings ) is
begin
  -- in interactive modes, a comment will hide the semi-colon automatically
  -- added by SparForte.  This can also happen with `..`
  if token = eof_t then
     expectSymbol(
       expectedValue => ";",
       contextNotes => contextNotes,
       reason => +"the end of statement",
       remedy => +"a comment or unescaped '--' has hidden a ';'"
     );
  end if;
  -- Common typo
  if token = symbol_t and identifiers( token ).value.all = ":" then
     expectSymbol(
       expectedValue => ";",
       contextNotes => contextNotes,
       reason => +"':' could be a mistake because the end of the statement"
     );
  else
     expectSymbol(
       expectedValue => ";",
       contextNotes => contextNotes,
       reason => +"the end of the statement"
    );
  end if;
end expectStatementSemicolon;


-----------------------------------------------------------------------------
--  EXPECT DECLARATION SEMICOLON
--
-- Read a semicolon, if it is present.  Otherwise, show an error related to
-- a missing semicolon at the end of a declaration.
-----------------------------------------------------------------------------

procedure expectDeclarationSemicolon( context : identifier := eof_t ) is
  myContextNotes : messageStrings;
begin
  if context /= eof_t then
     myContextNotes := pl( "in the declaration of " ) &
       name_em( context );
  end if;

  -- Common typo
  if token = symbol_t and identifiers( token ).value.all = ":" then
     expectSymbol(
       expectedValue => ";",
       contextNotes => myContextNotes,
       reason => +"':' could be a mistake because the end of the declaration"
     );
  else
     expectSymbol(
       expectedValue => ";",
       contextNotes => myContextNotes,
       reason => +"the end of the declaration"
    );
  end if;
end expectDeclarationSemicolon;

procedure expectDeclarationSemicolon( contextNotes : messageStrings := nullMessageStrings ) is
  myContextNotes : constant messageStrings := contextNotes;
begin

  -- Common typo
  if token = symbol_t and identifiers( token ).value.all = ":" then
     expectSymbol(
       expectedValue => ";",
       contextNotes => myContextNotes,
       reason => +"':' could be a mistake because the end of the declaration"
     );
  else
     expectSymbol(
       expectedValue => ";",
       contextNotes => myContextNotes,
       reason => +"the end of the declaration"
    );
  end if;
end expectDeclarationSemicolon;


-----------------------------------------------------------------------------
--  EXPECT RETURN SEMICOLON
--
-- Read a semicolon, if it is present.  Otherwise, show an error related to
-- a missing semicolon at the end of a return.  The context of the return
-- is the block it is in.
-----------------------------------------------------------------------------

procedure expectReturnSemicolon is
  blockName : unbounded_string;
  myContextNotes : messageStrings;
begin

  -- Get the name of the current block, the one this return applies to.

  if blocks_top > block'first then
     blockName := getBlockName( blocks_top-1 );
  end if;
  if blockName /= null_unbounded_string then
     myContextNotes := unb_pl( "in this return for " & blockName );
  else
     myContextNotes := +"in this return";
  end if;

  -- Common typo
  if token = symbol_t and identifiers( token ).value.all = ":" then
     expectSymbol(
       expectedValue => ";",
       contextNotes => myContextNotes,
       reason => +"':' could be a mistake because the end of the statement"
     );
  else
     expectSymbol(
       expectedValue => ";",
       contextNotes => myContextNotes,
       reason => +"the end of the statement"
    );
  end if;
end expectReturnSemicolon;

-- TODO: more


-----------------------------------------------------------------------------
-- Missing Commas
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  EXPECT PARAMETER COMMA
--
-- Read a comma, if it is present.  Otherwise, show an error related to a
-- missing comma in a parameter list of a subprogram.
-----------------------------------------------------------------------------

procedure expectParameterComma( subprogram : identifier := eof_t ) is
  contextNotes : messageStrings;
begin

  -- If we know the name of the subprogram, include it in the error message.
  -- This is useful if there are several nested function calls and SparForte
  -- explains which function it is concerned about.

  if subprogram = eof_t then
     contextNotes := +"in the parameter list";
  end if;

  -- Semi-colon may not happen because it may abort earlier due to
  -- end-of-function.
  if token = symbol_t and identifiers( token ).value.all = ";" then
     if subprogram = eof_t then
        expectSymbol(
          contextNotes => contextNotes,
          expectedValue => ",",
          reason => +"';' could be a mistake because the parameters"
        );
     else
        expectSymbol(
          context => subprogram,
          expectedValue => ",",
          reason => +"';' could be a mistake because the parameters"
        );
     end if;
  elsif token = symbol_t and identifiers( token ).value.all = "." then
     if subprogram = eof_t then
        expectSymbol(
          contextNotes => contextNotes,
          expectedValue => ",",
          reason => +"'.' could be a mistake because the parameters"
        );
     else
        expectSymbol(
          context => subprogram,
          expectedValue => ",",
          reason => +"'.' could be a mistake because the parameters"
        );
     end if;
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     if subprogram = eof_t then
        expectSymbol(
          expectedValue => ",",
          contextNotes => contextNotes,
          reason => +"there are too few parameters because the list"
        );
     else
        expectSymbol(
          expectedValue => ",",
          context => subprogram,
          reason => +"there are too few parameters because the list"
        );
     end if;
  else
     if subprogram = eof_t then
        expectSymbol(
          contextNotes => contextNotes,
          expectedValue => ",",
          reason  => +"to separate parameters the list"
        );
     else
        expectSymbol(
          context => subprogram,
          expectedValue => ",",
          reason  => +"to separate parameters the list"
        );
     end if;
  end if;
end expectParameterComma;


-----------------------------------------------------------------------------
--  EXPECT PRAGMA COMMA
--
-- Read a comma, if it is present.  Otherwise, show an error related to a
-- missing comma in a parameter list of a pragma.
-----------------------------------------------------------------------------

procedure expectPragmaComma is
  contextNotes : constant string := "in the pragma parameter list";
begin
  if token = symbol_t and identifiers( token ).value.all = ";" then
     expectSymbol(
       contextNotes => pl( contextNotes ),
       expectedValue => ",",
       reason => +"';' could be a mistake because the list"
     );
  elsif token = symbol_t and identifiers( token ).value.all = "." then
     expectSymbol(
       expectedValue => ",",
       contextNotes => pl( contextNotes ),
       reason => +"'.' could be a mistake because the list"
     );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expectSymbol(
       expectedValue => ",",
       contextNotes => pl( contextNotes ),
       reason => +"there are too few parameters because the list"
     );
  else
     expectSymbol(
       contextNotes => pl( contextNotes ),
       expectedValue => ",",
       reason  => +"to separate parameters the list"
     );
  end if;
end expectPragmaComma;


-----------------------------------------------------------------------------
-- Missing Round Bracket / Paranthesis
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  EXPECT PARAMETER OPEN
--
-- Read an open paranthesis, if it is present.  Otherwise, show an error
-- related to a missing open paranthesis in a parameter list of a subprogram.
-----------------------------------------------------------------------------

procedure expectParameterOpen( subprogram : identifier := eof_t ) is
  contextNotes : unbounded_string;
begin
  if subprogram = eof_t then
     contextNotes := to_unbounded_string( "in the parameter list" );
  end if;

  if token = symbol_t and identifiers( token ).value.all = ";" then
     expectSymbol(
       expectedValue => "(",
       context => subprogram,
       reason => +"';' looks like parameters are missing because the list"
     );
  else
     if contextNotes = "" then
        expectSymbol(
          expectedValue => "(",
          expectPlural => true,
          context => subprogram,
          subjectNotes => +"the parameters",
          reason => +"are starting and"
        );
     else
        expectSymbol(
          expectedValue => "(",
          expectPlural => true,
          contextNotes => unb_pl( contextNotes ),
          subjectNotes => +"the parameters",
          reason => +"are starting and"
        );
     end if;
  end if;
  if token = symbol_t and identifiers( token ).value.all = ")" then
     if contextNotes = "" then
        err( context => subprogram,
             subjectNotes => +"the parameters",
             obstructorNotes => +"",
             reason => +"are missing",
             remedy => +"omit the parentheses if there are no required parameters"
       );
     else
        err( contextNotes => unb_pl( contextNotes ),
             subjectNotes => +"the parameters",
             obstructorNotes => +"",
             reason => +"are missing",
             remedy => +"omit the parentheses if there are no required parameters"
        );
    end if;
  end if;
end expectParameterOpen;


-----------------------------------------------------------------------------
--  EXPECT PARAMETER CLOSE
--
-- Read a close paranthesis, if it is present.  Otherwise, show an error
-- related to a missing close paranthesis in a parameter list of a
-- subprogram.
-----------------------------------------------------------------------------

procedure expectParameterClose( subprogram : identifier := eof_t ) is
  contextNotes : unbounded_string;
begin
  if subprogram = eof_t then
     contextNotes := to_unbounded_string( "in the parameter list" );
  end if;

  if token = symbol_t and identifiers( token ).value.all = ";" then
     if contextNotes = "" then
        expectSymbol(
          expectedValue => ")",
          context => subprogram,
          reason => +"';' looks like a symbol is missing because the list"
        );
     else
        expectSymbol(
          expectedValue => ")",
          contextNotes => unb_pl( contextNotes ),
          reason => +"';' looks like a symbol is missing because the list"
        );
     end if;
  elsif token = symbol_t and identifiers( token ).value.all = "," then
     if contextNotes = "" then
        expectSymbol(
          expectedValue => ")",
          context => subprogram,
          reason => +"',' looks like too many parameters because the list"
        );
     else
        expectSymbol(
          expectedValue => ")",
          contextNotes => unb_pl( contextNotes ),
          reason => +"',' looks like too many parameters because the list"
        );
     end if;
  else
     if contextNotes = "" then
        expectSymbol(
          expectedValue => ")",
          context => subprogram,
          reason => +"the end of the parameters"
        );
     else
        expectSymbol(
          expectedValue => ")",
          contextNotes => unb_pl( contextNotes ),
          reason => +"the end of the parameters"
        );
     end if;
  end if;
end expectParameterClose;


-----------------------------------------------------------------------------
--  EXPECT PRAGMA PARAMETER OPEN
--
-- Read an open paranthesis, if it is present.  Otherwise, show an error
-- related to a missing open paranthesis in a parameter list of a subprogram.
-- Unlike subprograms, pragmas have a kind string, not an id.
-----------------------------------------------------------------------------

procedure expectPragmaParameterOpen( pragmaKind : messageStrings ) is
  contextNotes : messageStrings;
begin
  if pragmaKind = nullMessageStrings then
     contextNotes := pl( "in the pragma parameter list" );
  else
     contextNotes := pl( "in the pragma " ) & pragmaKind & pl( " parameter list" );
  end if;

  if token = symbol_t and identifiers( token ).value.all = ";" then
     expectSymbol(
       expectedValue => "(",
       contextNotes => contextNotes,
       reason => +"';' looks like parameters are missing because the list"
     );
  else
     expectSymbol(
       expectedValue => "(",
       expectPlural => true,
       contextNotes => contextNotes,
       subjectNotes => +"the parameters",
       reason => +"are starting and"
     );
  end if;
end expectPragmaParameterOpen;


-----------------------------------------------------------------------------
--  EXPECT PRAGMA PARAMETER CLOSE
--
-- Read a close paranthesis, if it is present.  Otherwise, show an error
-- related to a missing close paranthesis in a parameter list of a
-- subprogram.
-----------------------------------------------------------------------------

procedure expectPragmaParameterClose( pragmaKind : messageStrings ) is
  contextNotes : messageStrings;
begin
  if pragmaKind = nullMessageStrings then
     contextNotes := pl( "in the pragma parameter list" );
  else
     contextNotes := pl( "in the pragma " ) & pragmaKind & pl( " parameter list" );
  end if;

  if token = symbol_t and identifiers( token ).value.all = ";" then
     expectSymbol(
       expectedValue => ")",
       contextNotes => contextNotes,
       reason => +"';' looks like a symbol is missing because the list"
     );
  elsif token = symbol_t and identifiers( token ).value.all = "," then
     expectSymbol(
       expectedValue => ")",
       contextNotes => contextNotes,
       reason => +"',' looks like too many parameters because the list"
     );
  else
     expectSymbol(
       expectedValue => ")",
       contextNotes => contextNotes,
       reason => +"the end of the pragma parameters"
     );
  end if;
end expectPragmaParameterClose;


-----------------------------------------------------------------------------
-- Use of Non-Ada 95 Feature under Pragma Ada 95
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  EXPECT SPARFORTE
--
-- Something is expected that is AdaScript syntax.  If pragma ada_95, or some
-- future pragma on language features, is set, then report an error.
-----------------------------------------------------------------------------

procedure expectAdaScript(
    context         : identifier;              -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier := eof_t;     -- which
    remedy          : messageStrings := nullMessageStrings ) is        -- suggested solutions
begin
  if onlyAda95 then
     err(
        userLanguage => userLanguage.all,
        context => context,
        contextType => contextType,
        subject => subject,
        reason => +"is not compatible with",
        obstructorNotes => em( "pragma ada_95" ),
        remedy => remedy
     );
  end if;
  expect( subject );
end expectAdaScript;

procedure expectAdaScript(
    contextType     : identifier := eof_t;     -- associated type (if any)
    contextNotes    : messageStrings := nullMessageStrings;            -- notes
    subject         : identifier := eof_t;     -- which
    remedy          : messageStrings := nullMessageStrings ) is        -- suggested solutions
begin
  if onlyAda95 then
     err(
        userLanguage => userLanguage.all,
        contextType => contextType,
        contextNotes => contextNotes,
        subject => subject,
        reason => +"is not compatible with",
        obstructorNotes => em( "pragma ada_95" ),
        remedy => remedy
     );
  end if;
  expect( subject );
end expectAdaScript;


-----------------------------------------------------------------------------
--  EXPECT SPARFORTE DIFFERENCES
--
-- like expectSparForte, must makes it clear Ada and SparForte both have the
-- subject but they may vary in parameters or function/procedure.
-----------------------------------------------------------------------------

procedure expectAdaScriptDifferences(
    context         : identifier;              -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier := eof_t;     -- which
    remedy          : messageStrings := nullMessageStrings ) is        -- suggested solutions
begin
  if onlyAda95 then
     err(
        userLanguage => userLanguage.all,
        context => context,
        contextType => contextType,
        subject => subject,
        reason => +"has differences with Ada's version and is not fully compatible with" ,
        obstructorNotes => em( "pragma ada_95" ),
        remedy => remedy
     );
  end if;
  expect( subject );
end expectAdaScriptDifferences;

procedure expectAdaScriptDifferences(
    contextType     : identifier := eof_t;     -- associated type (if any)
    contextNotes    : messageStrings := nullMessageStrings;            -- notes
    subject         : identifier := eof_t;     -- which
    remedy          : messageStrings := nullMessageStrings ) is        -- suggested solutions
begin
  if onlyAda95 then
     err(
        userLanguage => userLanguage.all,
        contextType => contextType,
        contextNotes => contextNotes,
        subject => subject,
        reason => +"has differences with Ada's version and is not fully compatible with" ,
        obstructorNotes => em( "pragma ada_95" ),
        remedy => remedy
     );
  end if;
  expect( subject );
end expectAdaScriptDifferences;


begin
  -- the user language is hard coded, for now, to English
  userLanguage  := new englishUserLanguage;
end scanner.communications;

