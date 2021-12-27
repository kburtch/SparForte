------------------------------------------------------------------------------
-- Communications and Errors                                                --
--                                                                          --
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
     -- TODO: references own parent.  This is messy to sort out.  Will
     -- deal with it later.
     scanner;

use  ada.characters.handling,
     ada.strings.unbounded.text_io,
     ada.strings.fixed,
     pegasoft.strings,
     pegasoft.script_io,
     pegasoft.user_io,
     spar_os.tty,
     scanner;

package body scanner.communications is

templateErrorHeader : constant unbounded_string := to_unbounded_string( "SparForte says" );


-----------------------------------------------------------------------------
--  CONVERT TO HTML
--
-- Change an error message so that it is formatted as HTML
-----------------------------------------------------------------------------

function convertToHTML( oldString : unbounded_string ) return unbounded_string is
   s : unbounded_string := oldString;
   p : natural;

   --  STRIP TERMINAL CHARS
   --
   -- Search for and remove occurrences of terminal control sequences.  If
   -- the control sequence is nothing, then do nothing.  A timeout prevents
   -- infinite loops.  The string s is updated with the changes.
   --------------------------------------------------------------------------

   procedure stripTerminalChars(
         s : in out unbounded_string;
         controlSeq : unbounded_string;
         replacementHTML : string ) is
      timeout : natural;
   begin
      if controlSeq = "" then
         return;
      end if;
      timeout := 0;
      loop
         p := index( s, to_string( controlSeq ) );
      exit when p = 0 or timeout = 10;
         delete( s, p, p - 1 + length( controlSeq ) );
         insert( s, p, replacementHTML );
         timeout := timeout + 1;
      end loop;
   end stripTerminalChars;

begin

   -- replace end-of-lines / spaces
   -- do this first as the spans we may insert have spaces

   p := 1;
   while p <= length( s ) loop
      if element( s, p ) = ASCII.LF then
         delete( s, p, p );
         insert( s, p, "<br>" );
      elsif element( s, p ) = ' ' then
         delete( s, p, p );
         insert( s, p, "&nbsp;" );
      end if;
      p := p + 1;
   end loop;

   -- Replace boldface on

   stripTerminalChars(  s, term( bold ), "<span style=""font-weight:bold"">" );
   stripTerminalChars(  s, term( inverse ), "<span style=""font-style:italic"">" );
   stripTerminalChars(  s, term( normal ), "</span>" );

   -- Colour sequences: convert to bold and italic.  Green does nothing.
   -- White is treated as closing a colour.

   stripTerminalChars(  s, term( green ), "<span>" );
   stripTerminalChars(  s, term( yellow ), "<span style=""font-weight:bold"">" );
   stripTerminalChars(  s, term( red ), "<span style=""font-style:italic"">" );
   stripTerminalChars(  s, term( white ), "</span>" );

   return s;
end convertToHTML;


-----------------------------------------------------------------------------
--  CONVERT TO PLAIN TEXT
--
-- Change an error message so that it is formatted as plain text
-- for a web server log file.  By default, all line feeds are removed.
-----------------------------------------------------------------------------

type plainTextOptions is ( no_lf, with_lf );

function convertToPlainText( oldString : unbounded_string; options : plainTextOptions := no_lf ) return unbounded_string is
  s : unbounded_string := oldString;
  p : natural;
  timeout : natural;
begin

  -- remove any end-of-lines to ensure message is on one line

  if options = no_lf then
     p := 1;
     while p <= length( s ) loop
        if element( s, p ) = ASCII.LF then
           delete( s, p, p );
           insert( s, p, " " );
        end if;
        p := p + 1;
     end loop;
  end if;

  -- remove boldface on
  -- If not running in a tty, the term value may be empty.

  if term( bold ) /= null_unbounded_string then
     timeout := 0;
     loop
        p := index( s, to_string( term( bold ) ) );
     exit when p = 0 or timeout = 10;
        delete( s, p, p - 1 + length( term( bold ) ) );
        timeout := timeout + 1;
     end loop;
  end if;

  -- remove inverse on
  -- If not running in a tty, the term value may be empty.

  if term( inverse ) /= null_unbounded_string then
     timeout := 0;
     loop
        p := index( s, to_string( term( inverse ) ) );
     exit when p = 0 or timeout = 10;
        delete( s, p, p - 1 + length( term( inverse ) ) );
        timeout := timeout + 1;
     end loop;
  end if;

  -- remove boldface/inverse off
  -- If not running in a tty, the term value may be empty.

  if term( normal ) /= null_unbounded_string then
     timeout := 0;
     loop
        p := index( s, to_string( term( normal ) ) );
     exit when p = 0 or timeout = 10;
        delete( s, p, p - 1 + length( term( normal ) ) );
        timeout := timeout + 1;
     end loop;
  end if;

  return s;
 end convertToPlainText;


-----------------------------------------------------------------------------
--  GET SCRIPT POSITION MESSAGE
--
-----------------------------------------------------------------------------

function get_script_execution_position( msg : string ) return unbounded_string is
  cmdline    : unbounded_string;
  firstpos   : natural;
  lastpos    : natural;
  lineStr    : unbounded_string;
  firstposStr : unbounded_string;
  lineno     : natural;
  fileno     : natural;
  outLine    : unbounded_string;
  gccOutLine : unbounded_string;
  sfr        : aSourceFile;
  needGccVersion : boolean := false;
  fullErrorMessage : unbounded_string;
begin
  -- Only create the abbreviated GCC-style error message if we need it
  --
  -- In the case of templates, we need both the Gcc version and the non-Gcc
  -- version of the error message.  In a CGI script that isn't a template,
  -- regular errors are reported back.

  needGccVersion := boolean( gccOpt ) or hasTemplate;

  -- Decode a copy of the command line to show the error.  Also returns
  -- the current token position and the line number.

  if script /= null then
     getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
  else
     -- can't use optional_inverse here because the text will be
     -- escaped later
     cmdLine := to_unbounded_string( "<No executable line to show> in <no script loaded>" );
  end if;

  -- Clear any old error messages from both the screen error and the
  -- template error (if one exists)

  fullErrorMessage := null_unbounded_string;

  -- If in a script (that is, a non-interactive input mode) then
  -- show the location and traceback.  Otherwise, if we're just at
  -- the command prompt, don't bother with the location/traceback.

  if inputMode /= interactive and inputMode /= breakout then

  -- Get the location information.  If gcc option, strip the leading
  -- blanks form the location information.  Use outLine to generate a full
  -- line of text because individual Put's are shown as individual lines
  -- in Apache's error logs for templates...a real mess.
  --
  -- The basic GCC message will be recorded in a separate "out line"
  -- as we may need both message formats for a web template.

     if script /= null then

        if needGccVersion then                            -- gcc style?
           lineStr := to_unbounded_string( lineno'img );  -- remove leading
           if length( lineStr ) > 0 then                  -- space (if any)
              if element( lineStr, 1 ) = ' ' then
                 delete( lineStr, 1, 1 );
              end if;
           end if;
           firstposStr := to_unbounded_string( firstpos'img );
           if length( firstposStr ) > 0 then              -- here, too
              if element( firstposStr, 1 ) = ' ' then
                 delete( firstposStr, 1, 1 );
              end if;
           end if;
           sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
           gccOutLine := sfr.name
             & ":" & lineStr
             & ":" & firstposStr
             & ":";                                       -- no traceback
           gccOutLine := gccOutLine & ' ';                -- token start
           gccOutLine := gccOutLine & msg;
        end if;

        -- For the regular format, show the location and traceback

        sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
        outLine := sfr.name                               -- location
           & ":" & lineno'img
           & ":" & firstpos'img
           & ": ";

        -- TODO: we're using UNIX eof's but should ideally be o/s
        -- independent

        if blocks_top > blocks'first then                 -- in a block?
           for i in reverse blocks'first..blocks_top-1 loop -- show the
               if i /= blocks_top-1 then                  -- simplified
                  outLine := outLine & " in ";            -- traceback
               end if;
               outLine := outLine & ToEscaped( blocks( i ).blockName );
           end loop;
           fullErrorMessage := outLine & ASCII.LF;
           outLine := null_unbounded_string;
        else                                              -- if no blocks
           outLine := outLine & "in script";              -- just say
           fullErrorMessage := outLine & ASCII.LF;        -- "in script"
           outLine := null_unbounded_string;
        end if;
     else
        -- no script?
        outLine := null_unbounded_string;
     end if; -- a script exists
  end if;

  -- For the normal version, we must follow the traceback with the
  -- message, error underline and show the error message.
  -- Output only full lines to avoid messy Apache error logs.
  --
  -- First, add the line the error occurred in

  fullErrorMessage := fullErrorMessage & toEscaped( cmdline );

  -- Draw the underline error pointer

  if script /= null then
     outLine := outLine & ada.strings.unbounded.to_string( (firstPos-1) * " " );      -- indent
     outLine := outLine & '^';                                  -- token start
     if lastpos > firstpos then                                 -- multi chars?
        outLine := outLine & ada.strings.unbounded.to_string( (lastpos-firstPos-1) * "-" );
        outLine := outLine & '^';                               -- token end
     end if;
     outLine := outLine & ' ';                                  -- token start
  end if;
  outLine := outLine & msg;

  -- Even for a template, if the user selected gccOpt specifically,
  -- use it.

  -- Pick which format the user wants for the full message.
  --
  -- TODO: we're using UNIX eof's but should ideally be o/s
  -- independent

  if gccOpt then
     fullErrorMessage := gccOutLine;
  else
     fullErrorMessage := fullErrorMessage & ASCII.LF & outLine;
  end if;

  -- If we are in any mode of the development cycle except maintenance
  -- mode, create an error message to display.  If we're in maintenance
  -- mode, create an error message only if debug is enabled.

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        fullErrorMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & convertToHTML( fullErrorMessage ) & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        fullErrorMessage := "/* " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) &  " */";
     when xmlTemplate =>
        fullErrorMessage := "<!-- " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) & " -->";
     when textTemplate =>
        fullErrorMessage := convertToPlainText( fullErrorMessage, with_lf );
     when yamlTemplate =>
        fullErrorMessage := "# " & convertToPlainText( fullErrorMessage );
     when noTemplate | jsonTemplate =>
        fullErrorMessage := convertToPlainText( fullErrorMessage );
     end case;
     -- In the case of the template, the error output must always
     -- be in gcc format (a single line) for the web server log.
     --
     -- This affects exception handling since HTML output for template
     -- will differ from error message in exceptions package.  Also,
     -- format this for Apache by stripping out the boldface or
     -- other effects.
     --
     fullErrorMessage := ConvertToPlainText( gccOutLine );
  end if;
  return fullErrorMessage;
end get_script_execution_position;


-----------------------------------------------------------------------------
--  ERR SHELL
--
-- Stop execution and record an compile-time or run-time error.  Format the
-- error according to the user's preferences and set the error_found flag.
--
-- Only display the first error/exception encounted.
-----------------------------------------------------------------------------

procedure err_shell( msg : string; wordOffset : natural ) is
  cmdline    : unbounded_string;
  firstpos   : natural;
  lastpos    : natural;
  lineStr    : unbounded_string;
  firstposStr : unbounded_string;
  lineno     : natural;
  fileno     : natural;
  outLine    : unbounded_string;
  gccOutLine : unbounded_string;
  sfr        : aSourceFile;
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

  -- Decode a copy of the command line to show the error.  Also returns
  -- the current token position and the line number.

  if script /= null then
     getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
  else
     -- can't use optional_inverse here because the text will be
     -- escaped later
     cmdLine := to_unbounded_string( "<No executable line to show> in <no script loaded>" );
  end if;

  -- Clear any old error messages from both the screen error and the
  -- template error (if one exists)

  fullErrorMessage := null_unbounded_string;
  fullTemplateErrorMessage := null_unbounded_string;

  -- If in a script (that is, a non-interactive input mode) then
  -- show the location and traceback.  Otherwise, if we're just at
  -- the command prompt, don't bother with the location/traceback.

  if inputMode /= interactive and inputMode /= breakout then

  -- Get the location information.  If gcc option, strip the leading
  -- blanks form the location information.  Use outLine to generate a full
  -- line of text because individual Put's are shown as individual lines
  -- in Apache's error logs for templates...a real mess.
  --
  -- The basic GCC message will be recorded in a separate "out line"
  -- as we may need both message formats for a web template.

     if script /= null then

        if needGccVersion then                            -- gcc style?
           lineStr := to_unbounded_string( lineno'img );  -- remove leading
           if length( lineStr ) > 0 then                  -- space (if any)
              if element( lineStr, 1 ) = ' ' then
                 delete( lineStr, 1, 1 );
              end if;
           end if;
           firstposStr := to_unbounded_string( firstpos'img );
           if length( firstposStr ) > 0 then              -- here, too
              if element( firstposStr, 1 ) = ' ' then
                 delete( firstposStr, 1, 1 );
              end if;
           end if;
           sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
           gccOutLine := sfr.name
             & ":" & lineStr
             & ":" & firstposStr
             & ":";                                       -- no traceback
           gccOutLine := gccOutLine & ' ';                -- token start
           gccOutLine := gccOutLine & msg;
        end if;

        -- For the regular format, show the location and traceback

        sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
        outLine := sfr.name                               -- location
           & ":" & lineno'img
           & ":" & firstpos'img
           & ": ";

        -- TODO: we're using UNIX eof's but should ideally be o/s
        -- independent

        if blocks_top > blocks'first then                 -- in a block?
           for i in reverse blocks'first..blocks_top-1 loop -- show the
               if i /= blocks_top-1 then                  -- simplified
                  outLine := outLine & " in ";            -- traceback
               end if;
               outLine := outLine & ToEscaped( blocks( i ).blockName );
           end loop;
           fullErrorMessage := outLine & ASCII.LF;
           outLine := null_unbounded_string;
        else                                              -- if no blocks
           outLine := outLine & "in script";              -- just say
           fullErrorMessage := outLine & ASCII.LF;        -- "in script"
           outLine := null_unbounded_string;
        end if;
     else
        -- no script?
        outLine := null_unbounded_string;
     end if; -- a script exists
  end if;

  -- For the normal version, we must follow the traceback with the
  -- message, error underline and show the error message.
  -- Output only full lines to avoid messy Apache error logs.
  --
  -- First, add the line the error occurred in

  fullErrorMessage := fullErrorMessage & toEscaped( cmdline );

  -- Draw the underline error pointer
  -- If it's a shell word with an offset into the word, just show a single
  -- caret.

  if script /= null then
     outLine := outLine & ada.strings.unbounded.to_unbounded_string( (firstPos-1) * " " );      -- indent
     if wordOffset > 0 then
        outLine := outLine & ada.strings.unbounded.to_unbounded_string( (wordOffset-1) * " " );
        outLine := outLine & '^';
     else
        if lastpos > firstpos then                              -- multi chars?
           if colourOpt then
              outLine := outLine & utf_left;                         -- token start
              outLine := outLine & ada.strings.unbounded.to_unbounded_string( (lastpos-firstPos-1) * utf_horizontalLineOnly );
              outLine := outLine & utf_right;                       -- token end
           else
              outLine := outLine & '^';
              outLine := outLine & ada.strings.unbounded.to_unbounded_string( (lastpos-firstPos-1) * "-" );
              outLine := outLine & '^';
           end if;
        else
           if colourOpt then
              outLine := outLine & utf_triangle;                         -- token start
           else
              outLine := outLine & '^';
           end if;
        end if;
     end if;
     outLine := outLine & ' ';                                  -- token start
  end if;

  if colourOpt then
     outLine := outLine & utf_ballot & " " & msg;
  else
     outLine := outLine & msg;
  end if;

  -- Even for a template, if the user selected gccOpt specifically,
  -- use it.

  -- Pick which format the user wants for the full message.
  --
  -- TODO: we're using UNIX eof's but should ideally be o/s
  -- independent

  if gccOpt then
     fullErrorMessage := gccOutLine;
  else
     fullErrorMessage := fullErrorMessage & ASCII.LF & outLine;
  end if;

  -- If we are in any mode of the development cycle except maintenance
  -- mode, create an error message to display.  If we're in maintenance
  -- mode, create an error message only if debug is enabled.

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        fullTemplateErrorMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & convertToHTML( fullErrorMessage ) & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        fullTemplateErrorMessage := "/* " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) &  " */";
     when xmlTemplate =>
        fullTemplateErrorMessage := "<!-- " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) & " -->";
     when textTemplate =>
        fullTemplateErrorMessage := convertToPlainText( fullErrorMessage, with_lf );
     when yamlTemplate =>
        fullTemplateErrorMessage := "# " & convertToPlainText( fullErrorMessage );
     when noTemplate | jsonTemplate =>
        fullTemplateErrorMessage := convertToPlainText( fullErrorMessage );
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
     fullErrorMessage := ConvertToPlainText( gccOutLine );
  end if;

  -- Show that this is an error, not an exception

  error_found := true;                                          -- flag error
  err_exception.name := null_unbounded_string;            -- not an exception
  last_status := 0;

  -- If trace mode is enabled, show this as the point in the execution
  -- where the error occurred.

  if traceOpt then
     put_trace_error( "error: " & msg );
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

procedure err( msg : string ) is
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
  err( optional_red( "there are too many identifiers for SparForte to handle (symbol table overflow)" ) );
end err_symbol_table_overflow;


-----------------------------------------------------------------------------
-- ERR STYLE
--
-- Display a style error.  It is not an error if the script is unstructured.
-----------------------------------------------------------------------------

procedure err_style( msg : string ) is
begin
   if scriptType = structured then
      err_shell( "style issue: " & msg, 0 );
   end if;
end err_style;


-----------------------------------------------------------------------------
--  ERR EXCEPTION RAISED
--
-- General message when raising on when others =>
-----------------------------------------------------------------------------

procedure err_exception_raised is
begin
  err( "an unexpected exception was raised" );
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
     err( "renaming " &
          optional_yellow( to_string( identifiers( renaming_id ).name ) ) &
          " still refers to " &
          optional_yellow( to_string( identifiers( ident ).name ) ) );
  else
     err( "renaming " &
          optional_yellow( to_string( identifiers( renaming_id ).name ) ) &
          " (and others) still refer to " &
          optional_yellow( to_string( identifiers( ident ).name ) ) );
  end if;
end err_renaming;


-----------------------------------------------------------------------------
--  RAISE EXCEPTION
--
-- Like err, but for exceptions.  Stop execution and report a run-time
-- exception.  Set the error_found and exception-related global values.
-----------------------------------------------------------------------------

procedure raise_exception( msg : string ) is
  cmdline    : unbounded_string;
  firstpos   : natural;
  lastpos    : natural;
  lineStr    : unbounded_string;
  firstposStr : unbounded_string;
  lineno     : natural;
  fileno     : natural;
  outLine    : unbounded_string;
  gccOutLine : unbounded_string;
  sfr        : aSourceFile;
  needGccVersion : boolean := false;
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

  -- Decode a copy of the command line to show the error.  Also returns
  -- the current token position and the line number.

  getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );

  -- Clear any old error messages from both the screen error and the
  -- template error (if one exists)

  fullErrorMessage := null_unbounded_string;
  fullTemplateErrorMessage := null_unbounded_string;

  -- If in a script (that is, a non-interactive input mode) then
  -- show the location and traceback.  Otherwise, if we're just at
  -- the command prompt, don't bother with the location/traceback.

  if inputMode /= interactive and inputMode /= breakout then

  -- Get the location information.  If gcc option, strip the leading
  -- blanks form the location information.  Use outLine to generate a full
  -- line of text because individual Put's are shown as individual lines
  -- in Apache's error logs for templates...a real mess.
  --
  -- The basic GCC message will be recorded in a separate "out line"
  -- as we may need both message formats for a web template.

     if needGccVersion then                            -- gcc style?
        lineStr := to_unbounded_string( lineno'img );  -- remove leading
        if length( lineStr ) > 0 then                  -- space (if any)
           if element( lineStr, 1 ) = ' ' then
              delete( lineStr, 1, 1 );
           end if;
        end if;
        firstposStr := to_unbounded_string( firstpos'img );
        if length( firstposStr ) > 0 then              -- here, too
           if element( firstposStr, 1 ) = ' ' then
              delete( firstposStr, 1, 1 );
           end if;
        end if;
        sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
        gccOutLine := sfr.name
          & ":" & lineStr
          & ":" & firstposStr
          & ":";                  -- no traceback
        gccOutLine := gccOutLine & ' ';                -- token start
        gccOutLine := gccOutLine & msg;
     end if;

     -- For the regular format, show the location and traceback

     sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
     outLine := sfr.name               -- otherwise
        & ":" & lineno'img
        & ":" & firstpos'img
        & ": ";

     -- TODO: we're using UNIX eof's but should ideally be o/s
     -- independent

     if blocks_top > blocks'first then                     -- in a block?
        for i in reverse blocks'first..blocks_top-1 loop   -- show the
            if i /= blocks_top-1 then                      -- simplified
               outLine := outLine & " in ";                -- traceback
            end if;
            outLine := outLine & ToEscaped( blocks( i ).blockName );
        end loop;
        fullErrorMessage := outLine & ASCII.LF;
        outLine := null_unbounded_string;
     else                                                  -- otherwise
        outLine := outLine & "in script";                  -- just say
        fullErrorMessage := outLine & ASCII.LF;            -- "in script"
        outLine := null_unbounded_string;
     end if;
  end if;

  -- For the normal version, we must follow the traceback with the
  -- message, error underline and show the exception message.
  -- Output only full lines to avoid messy Apache error logs.
  --
  -- First, add the line the exception occurred in.  As a precaution,
  -- escape the command line.
  fullErrorMessage := fullErrorMessage & toEscaped( cmdline );

  -- Draw the underline error pointer

  outLine := outLine & ada.strings.unbounded.to_string( (firstPos-1) * " " );      -- indent
  outLine := outLine & '^';                                  -- token start
  if lastpos > firstpos then                                 -- multi chars?
     outLine := outLine & ada.strings.unbounded.to_string( (lastpos-firstPos-1) * "-" );
     outLine := outLine & '^';                               -- token end
  end if;
  outLine := outLine & ' ';                                  -- token start
  outLine := outLine & msg;
  -- DEBUG
  -- fullErrorMessage := fullErrorMessage & ASCII.LF & outLine;

  -- Even for a template, if the user selected gccOpt specifically,
  -- use it.

  -- Pick which format the user wants for the full message.
  --
  -- TODO: we're using UNIX eof's but should ideally be o/s
  -- independent

  if gccOpt then
     fullErrorMessage := gccOutLine;
  else
     fullErrorMessage := fullErrorMessage & ASCII.LF & outLine;
  end if;

  if hasTemplate and ( boolean( debugOpt or not maintenanceOpt ) ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        fullTemplateErrorMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #9F6000; background-color: #FEEFB3; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-weight:bold; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#9f6000; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">!</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #9F6000; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & convertToHTML( fullErrorMessage ) & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        fullTemplateErrorMessage := "/* " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) &  " */";
     when xmlTemplate =>
        fullTemplateErrorMessage := "<!-- " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) & " -->";
     when yamlTemplate =>
        fullTemplateErrorMessage := "# " & convertToPlainText( fullErrorMessage );
     when noTemplate | textTemplate | jsonTemplate =>
        fullTemplateErrorMessage := convertToPlainText( fullErrorMessage );
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
     fullErrorMessage := ConvertToPlainText( gccOutLine );

  end if;

  -- Show that this is an exception, not an error.  Do not erase
  -- err_exception.name.  The ParseRaise, etc. procedure will set
  -- err_exception.

  error_found := true;                                          -- flag error

  -- If trace mode is enabled, show this as the point in the execution
  -- where the error occurred.

  if traceOpt then
     put_trace_error( "exception: " & msg );
  end if;
end raise_exception;


-----------------------------------------------------------------------------
--  ERR TEST RESULT
--
-- Show a failed result from pragma test_result
-----------------------------------------------------------------------------

procedure err_test_result is
  cmdline    : unbounded_string;
  firstpos   : natural;
  lastpos    : natural;
  lineStr    : unbounded_string;
  firstposStr : unbounded_string;
  lineno     : natural;
  fileno     : natural;
  outLine    : unbounded_string;
  gccOutLine : unbounded_string;
  sfr        : aSourceFile;
  needGccVersion : boolean := false;
  msg        : constant unbounded_string := to_unbounded_string( "test failed" );
  ourFullErrorMessage : unbounded_string;
  ourFullTemplateErrorMessage : unbounded_string;
begin
  -- determine if gcc format is requested or required
  needGccVersion := boolean( gccOpt ) or hasTemplate;

  -- get the command line
  if script /= null then
     getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
  else
     raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location &
         ": internal error: script unexpectly empty";
  end if;

  if inputMode /= interactive and inputMode /= breakout then
        if needGccVersion then                            -- gcc style?
           lineStr := to_unbounded_string( lineno'img );  -- remove leading
           if length( lineStr ) > 0 then                  -- space (if any)
              if element( lineStr, 1 ) = ' ' then
                 delete( lineStr, 1, 1 );
              end if;
           end if;
           firstposStr := to_unbounded_string( firstpos'img );
           if length( firstposStr ) > 0 then              -- here, too
              if element( firstposStr, 1 ) = ' ' then
                 delete( firstposStr, 1, 1 );
              end if;
           end if;
           sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
           gccOutLine := sfr.name
             & ":" & lineStr
             & ":" & firstposStr
             & ":";                                       -- no traceback
           gccOutLine := gccOutLine & ' ';                -- token start
           gccOutLine := gccOutLine & msg;
        end if;


        sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
        outLine := sfr.name                               -- location
           & ":" & lineno'img
           & ":" & firstpos'img
           & ": ";
        -- TODO: we're using UNIX eof's but should ideally be o/s
        -- independent
        if blocks_top > blocks'first then                 -- in a block?
           for i in reverse blocks'first..blocks_top-1 loop -- show the
               if i /= blocks_top-1 then                  -- simplified
                  outLine := outLine & " in ";            -- traceback
               end if;
               outLine := outLine & ToEscaped( blocks( i ).blockName );
           end loop;
           ourFullErrorMessage := outLine & ASCII.LF;
           outLine := null_unbounded_string;
        else                                              -- if no blocks
           outLine := outLine & "in script";              -- just say
           ourFullErrorMessage := outLine & ASCII.LF;        -- "in script"
           outLine := null_unbounded_string;
        end if;
  end if;

  ourFullErrorMessage := ourFullErrorMessage & toEscaped( cmdline );

  -- Draw the underline error pointer

  outLine := outLine & ada.strings.unbounded.to_string( (firstPos-1) * " " );      -- indent
  outLine := outLine & '^';                                  -- token start
  if lastpos > firstpos then                                 -- multi chars?
     outLine := outLine & ada.strings.unbounded.to_string( (lastpos-firstPos-1) * "-" );
     outLine := outLine & '^';                               -- token end
  end if;
  outLine := outLine & ' ';                                  -- token start
  outLine := outLine & msg;

  -- Even for a template, if the user selected gccOpt specifically,
  -- use it.

  -- Pick which format the user wants for the full message.
  --
  -- TODO: we're using UNIX eof's but should ideally be o/s
  -- independent

  if gccOpt then
     ourFullErrorMessage := gccOutLine;
  else
     ourFullErrorMessage := ourFullErrorMessage & ASCII.LF & outLine;
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
           "<p>" & convertToHTML( ourFullErrorMessage ) & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        ourFullTemplateErrorMessage := "/* " & templateErrorHeader & " " & convertToPlainText( ourFullErrorMessage ) &  " */";
     when xmlTemplate =>
        fullTemplateErrorMessage := "<!-- " & templateErrorHeader & " " & convertToPlainText( ourFullErrorMessage ) & " -->";
     when yamlTemplate =>
        fullTemplateErrorMessage := "# " & convertToPlainText( ourFullErrorMessage );
     when noTemplate | textTemplate | jsonTemplate =>
        ourFullTemplateErrorMessage := convertToPlainText( ourFullErrorMessage );
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
     ourFullErrorMessage := ConvertToPlainText( gccOutLine );
  end if;

  -- Show that this is an error, not an exception

  -- this doesn't raise an exception or error
  -- err_exception.name := null_unbounded_string;            -- not an exception
  -- last_status := 0;

  -- It is redundanct to output the message when tracing since it is always
  -- output when it occurs anyway...it's not an error or exception.

  -- Show the test result message immediately

  put_line( standard_error, ourFullErrorMessage );
  -- may or may not have a template at this point, so check
  if hasTemplate then
     putTemplateHeader( templateHeader );
     put_line( ourFullTemplateErrorMessage );
  end if;

  -- Originally:
  --
  -- put( standard_error, scriptFilePath );
  -- put( standard_error, ":" );
  -- put( standard_error, getLineNo'img );
  -- put( standard_error, ": " );
  -- if gccOpt then
  --     put_line( standard_error, "test failed" );
  -- else
  --     put_line( standard_error, to_string( getCommandLine ) );
  --     put_line( standard_error, "^ test failed" );
  -- end if;
end err_test_result;


-----------------------------------------------------------------------------
--  WARN
--
-- Issue a warning.  This is done immediately, is not formatted by gcc-style
-- preference and is not stored.
-----------------------------------------------------------------------------

procedure warn( msg : string ) is
  location : unbounded_string;
  fullMsg  : unbounded_string;
begin
  location := scriptFilePath & ":" & getLineNo'img & ": ";
  fullMsg  := location & "warning--" & msg;

  put_line( standard_error, fullMsg );

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        put( "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & convertToHTML( fullMsg ) & "</p>" &
           "</div>" &
           "</div>" &
           "<br />" );
     when cssTemplate | jsTemplate =>
        put( "/* " & templateErrorHeader & " " & convertToPlainText( fullMsg ) &  " */" );
     when xmlTemplate =>
        put( "<!-- " & templateErrorHeader & " " & convertToPlainText( fullMsg ) & " -->" );
     when yamlTemplate =>
        put( "# " & convertToPlainText( fullMsg ) );
     when noTemplate | textTemplate | jsonTemplate =>
        put( convertToPlainText( fullMsg ) );
     end case;
  end if;
end warn;


-----------------------------------------------------------------------------
--  ERR PREVIOUS
--
-- Same as err below, but don't hilight the current token because the error
-- actually happened before it.  Just mark start of current token.
-----------------------------------------------------------------------------

procedure err_previous( msg : string ) is
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
    contextNotes    : string;                  -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : string;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructorNotes : string;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
  msg       : unbounded_string;
  blockName : unbounded_string;
  needV     : boolean := false;
begin

  -- The context
  --
  -- If an identifier is provided, use it first.  Otherwise use textual notes.
  -- If no context was provided, use the current block name for the context.

  if contextNotes = "" then
     if blocks_top > block'first then
        blockName := getBlockName( blocks_top-1 );
     end if;
     if blockName /= null_unbounded_string then
        msg := "In " & blockName;
     end if;
  elsif Is_Upper( contextNotes( contextNotes'first ) ) then
     msg := to_unbounded_string( contextNotes );
  else
     msg := "While " & to_unbounded_string( contextNotes );
  end if;

  if contextType /= eof_t then
     msg := msg & " (" & ( optional_yellow( to_string( toEscaped( AorAN( identifiers( contextType ).name ) ) ) ) & ")" );
  elsif msg /= "" then
    msg := msg & ",";
  end if;

  -- Subject
  --
  -- This is the identifier the error refers to.  If no identifier is provided,
  -- use textual notes.

  if subjectNotes /= "" then
     if msg /= "" then
        msg := msg & " " & subjectNotes;
     else
        -- if not context, upper-case first letter of subject notes
        msg := msg & ToUpper( to_unbounded_string( subjectNotes( subjectNotes'first ) & "" ) );
        msg := msg & subjectNotes( subjectNotes'first+1..subjectNotes'last );
     end if;
  end if;

  if subjectType /= eof_t then
     msg := msg & " (" & ( optional_yellow( to_string( toEscaped( AorAN( identifiers( subjectType ).name ) ) ) ) );
     if subjectLocation = "" then
        msg := msg & ")";
     end if;
  end if;
  if subjectLocation /= "" then
     if subjectType = eof_t then
        msg := msg & " (";
     else
        msg := msg & ", ";
     end if;
     msg := msg & subjectLocation & ")";
  end if;

  -- Reason
  --
  -- The error message.

  if msg /= "" then
     msg := msg & " ";
  end if;
  msg := msg & reason;

  -- Obstructor
  --
  -- The identifier causing the error for the subject.  If no identifier is
  -- given, use the textual notes.

  if obstructorNotes /= "" then
     if msg /= "" then
        msg := msg & " ";
     end if;
    msg := msg & to_unbounded_string( obstructorNotes );
  end if;
  if obstructorType /= eof_t then
     msg := msg & " (" & ( optional_yellow( to_string( toEscaped( AorAN( identifiers( obstructorType ).name ) ) ) ) & ")" );
  end if;

  -- Remedy
  --
  -- Suggestions to correct the problem.

  if remedy /= "" then
     if boolean(verboseOpt) then
        msg := msg & ". Perhaps " & remedy;
     else
        needV := true;
     end if;
  end if;

  -- See Also
  --
  -- Usually a reference to the documentation.

  if remedy /= "" then
     if boolean(verboseOpt) then
        if seeAlso /= "" then
           msg := msg & ". See also " & seeAlso;
        end if;
     else
        needV := true;
     end if;
  end if;

  if needV then
     msg := msg & ". (More with -v)";
  end if;

  err( to_string( msg ) );
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
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
  contextNotes    : unbounded_string;
  subjectNotes    : unbounded_string;
  obstructorNotes : unbounded_string;
begin
  if context /= eof_t then
    if context < keywords_top then
      contextNotes := "In this " & to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( context ).name ) ) ) );
    else
      contextNotes := "In " & to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( context ).name ) ) ) );
    end if;
  end if;

   subjectNotes := to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( subject ).name ) ) ) );
   obstructorNotes := to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( obstructor ).name ) ) ) );

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => to_string( contextNotes ),
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => to_string( subjectNotes ),
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => to_string( obstructorNotes ),
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
    contextNotes    : string := "";            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
  subjectNotes    : unbounded_string;
  obstructorNotes : unbounded_string;
begin
   subjectNotes := to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( subject ).name ) ) ) );
   obstructorNotes := to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( obstructor ).name ) ) ) );

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => contextNotes,
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => to_string( subjectNotes ),
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => to_string( obstructorNotes ),
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
    subjectNotes    : string;                   -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
  contextNotes    : unbounded_string;
  obstructorNotes : unbounded_string;
begin
  if context /= eof_t then
    if context < keywords_top then
      contextNotes := "In this " & to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( context ).name ) ) ) );
    else
      contextNotes := "In " & to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( context ).name ) ) ) );
    end if;
  end if;
   obstructorNotes := to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( obstructor ).name ) ) ) );

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => to_string( contextNotes ),
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => subjectNotes,
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => to_string( obstructorNotes ),
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
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructorNotes : string;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
  contextNotes    : unbounded_string;
  subjectNotes    : unbounded_string;
begin
  if context /= eof_t then
    if context < keywords_top then
      contextNotes := "In this " & to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( context ).name ) ) ) );
    else
      contextNotes := "In " & to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( context ).name ) ) ) );
    end if;
  end if;
   subjectNotes := to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( subject ).name ) ) ) );

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => to_string( contextNotes ),
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => to_string( subjectNotes ),
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
    contextNotes    : string := "";            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : string;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
  obstructorNotes : unbounded_string;
begin
   obstructorNotes := to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( obstructor ).name ) ) ) );
   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => contextNotes,
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => subjectNotes,
    reason          => reason,
    obstructorType  => obstructorType,
    obstructorNotes => to_string( obstructorNotes ),
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
    contextNotes    : string := "";            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructorNotes : string;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
  subjectNotes    : unbounded_string;
begin
   subjectNotes := to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( subject ).name ) ) ) );

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => contextNotes,
    subjectType     => subjectType,
    subjectLocation => subjectLocation,
    subjectNotes    => to_string( subjectNotes ),
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
    subjectNotes    : string;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructorNotes : string;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
  contextNotes    : unbounded_string;
begin
  if context /= eof_t then
    if context < keywords_top then
      contextNotes := "In this " & to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( context ).name ) ) ) );
    else
      contextNotes := "In " & to_unbounded_string( optional_yellow( to_string( toEscaped( identifiers( context ).name ) ) ) );
    end if;
  end if;

   err(
    userLanguage    => userLanguage,
    contextType     => contextType,
    contextNotes    => to_string( contextNotes ),
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
    contextNotes    : string := "";            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : string;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructorNotes : string;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
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
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
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
    contextNotes    : string := "";            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
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
    subjectNotes    : string;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
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
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructorNotes : string;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
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
    contextNotes    : string := "";            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subjectNotes    : string;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructor      : identifier;              -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
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
    contextNotes    : string := "";            -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    subject         : identifier;              -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructorNotes : string;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
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
    subjectNotes    : string;                  -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    reason          : string;                  -- problem description
    obstructorNotes : string;                  -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    remedy          : string := "";            -- suggested solutions
    seeAlso         : string := "" ) is
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
        err( "keyword expected" );
     elsif expected_token = number_t then
        err( "number expected" );
     elsif expected_token = strlit_t then
        err( "string literal expected" );
     elsif expected_token = symbol_t then
        err( "symbol expected" );
     else
        err( to_string( identifiers( expected_token ).name ) & " expected" );
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
-- TODO: to be deprecated

procedure expect( expected_token : identifier; value : string ) is
begin
  if token /= expected_token then
     if expected_token = keyword_t then
        err( "keyword expected" );
     elsif expected_token = number_t then
        err( "number expected" );
     elsif expected_token = strlit_t then
        err( "string literal expected" );
     elsif expected_token = symbol_t then
        err( "symbol expected" );
     else
        err( to_string( identifiers( expected_token ).name ) & " expected" );
     end if;
  end if;
  if value /= to_string( identifiers( token ).value.all ) then
      err( "'" & value & "' expected" );
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
    subjectLocation : string := "";
    reason          : string := "";
    remedy          : string := "" ) is
  msg             : unbounded_string;
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
        reason => reason & " " &  optional_yellow( to_string( expectOrExpects ) ),
        obstructorNotes =>  optional_yellow( to_string( toEscaped( to_unbounded_string( "'" & expectedValue & "'" ) ) ) ),
        remedy => remedy
     );
  end if;
  getNextToken;
end expectSymbol;

procedure expectSymbol(
    expectedValue   : string;
    expectPlural    : boolean := false;
    contextNotes    : string := "";
    subject         : identifier;
    subjectType     : identifier := eof_t;
    subjectLocation : string := "";
    reason          : string := "";
    remedy          : string := "" ) is
  msg             : unbounded_string;
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
        reason => reason & " " &  optional_yellow( to_string( expectOrExpects ) ),
        obstructorNotes =>  optional_yellow( to_string( toEscaped( to_unbounded_string( "'" & expectedValue & "'" ) ) ) ),
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
    subjectLocation : string := "";
    subjectNotes    : string := "";
    reason          : string := "";
    remedy          : string := "" ) is
  msg             : unbounded_string;
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
        reason => reason & " " &  optional_yellow( to_string( expectOrExpects ) ),
        obstructorNotes =>  optional_yellow( to_string( toEscaped( to_unbounded_string( "'" & expectedValue & "'" ) ) ) ),
        remedy => remedy
     );
  end if;
  getNextToken;
end expectSymbol;

procedure expectSymbol(
    expectedValue   : string;
    expectPlural    : boolean := false;
    contextNotes    : string := "";
    subjectType     : identifier := eof_t;
    subjectLocation : string := "";
    subjectNotes    : string := "";
    reason          : string := "";
    remedy          : string := "" ) is
  msg             : unbounded_string;
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
        reason => reason & " " &  optional_yellow( to_string( expectOrExpects ) ),
        obstructorNotes =>  optional_yellow( to_string( toEscaped( to_unbounded_string( "'" & expectedValue & "'" ) ) ) ),
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
     err( "an " & optional_yellow( "identifier") & " for " & what & " was expected but this looks like " & optional_yellow( receivedDescription ) );
  else
     err( "an " & optional_yellow( "identifier") & " was expected but this looks like " & optional_yellow( receivedDescription ) );
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
       reason => "the end of statement",
       remedy => "a comment or unescaped '--' has hidden a ';'"
     );
  end if;
  -- Common typo
  if token = symbol_t and identifiers( token ).value.all = ":" then
     expectSymbol(
       expectedValue => ";",
       context => context,
       reason => "':' could be a mistake because the end of the statement"
     );
  else
     expectSymbol(
       expectedValue => ";",
       context => context,
       reason => "the end of the statement"
    );
  end if;
end expectStatementSemicolon;

procedure expectStatementSemicolon( contextNotes : string := "" ) is
begin
  -- in interactive modes, a comment will hide the semi-colon automatically
  -- added by SparForte.  This can also happen with `..`
  if token = eof_t then
     expectSymbol(
       expectedValue => ";",
       contextNotes => contextNotes,
       reason => "the end of statement",
       remedy => "a comment or unescaped '--' has hidden a ';'"
     );
  end if;
  -- Common typo
  if token = symbol_t and identifiers( token ).value.all = ":" then
     expectSymbol(
       expectedValue => ";",
       contextNotes => contextNotes,
       reason => "':' could be a mistake because the end of the statement"
     );
  else
     expectSymbol(
       expectedValue => ";",
       contextNotes => contextNotes,
       reason => "the end of the statement"
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
  myContextNotes : unbounded_string;
begin
  if context /= eof_t then
     myContextNotes := to_unbounded_string( "in the declaration of " & optional_yellow( to_string( identifiers( context ).name ) ) );
  end if;

  -- Common typo
  if token = symbol_t and identifiers( token ).value.all = ":" then
     expectSymbol(
       expectedValue => ";",
       contextNotes => to_string( myContextNotes ),
       reason => "':' could be a mistake because the end of the declaration"
     );
  else
     expectSymbol(
       expectedValue => ";",
       contextNotes => to_string( myContextNotes ),
       reason => "the end of the declaration"
    );
  end if;
end expectDeclarationSemicolon;

procedure expectDeclarationSemicolon( contextNotes : string := "" ) is
  myContextNotes : unbounded_string := to_unbounded_string( contextNotes );
begin

  -- Common typo
  if token = symbol_t and identifiers( token ).value.all = ":" then
     expectSymbol(
       expectedValue => ";",
       contextNotes => to_string( myContextNotes ),
       reason => "':' could be a mistake because the end of the declaration"
     );
  else
     expectSymbol(
       expectedValue => ";",
       contextNotes => to_string( myContextNotes ),
       reason => "the end of the declaration"
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
  myContextNotes : unbounded_string;
begin

  -- Get the name of the current block, the one this return applies to.

  if blocks_top > block'first then
     blockName := getBlockName( blocks_top-1 );
  end if;
  if blockName /= null_unbounded_string then
     myContextNotes := "in this return for " & blockName;
  else
     myContextNotes := to_unbounded_string( "in this return" );
  end if;

  -- Common typo
  if token = symbol_t and identifiers( token ).value.all = ":" then
     expectSymbol(
       expectedValue => ";",
       contextNotes => to_string( myContextNotes ),
       reason => "':' could be a mistake because the end of the statement"
     );
  else
     expectSymbol(
       expectedValue => ";",
       contextNotes => to_string( myContextNotes ),
       reason => "the end of the statement"
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
  contextNotes : unbounded_string;
begin

  -- If we know the name of the subprogram, include it in the error message.
  -- This is useful if there are several nested function calls and SparForte
  -- explains which function it is concerned about.

  if subprogram = eof_t then
     contextNotes := to_unbounded_string( "in the parameter list" );
  end if;

  -- Semi-colon may not happen because it may abort earlier due to
  -- end-of-function.
  if token = symbol_t and identifiers( token ).value.all = ";" then
     if subprogram = eof_t then
        expectSymbol(
          contextNotes => to_string( contextNotes ),
          expectedValue => ",",
          reason => "';' could be a mistake because the parameters");
     else
        expectSymbol(
          context => subprogram,
          expectedValue => ",",
          reason => "';' could be a mistake because the parameters");
     end if;
  elsif token = symbol_t and identifiers( token ).value.all = "." then
     if subprogram = eof_t then
        expectSymbol(
          contextNotes => to_string( contextNotes ),
          expectedValue => ",",
          reason => "'.' could be a mistake because the parameters");
     else
        expectSymbol(
          context => subprogram,
          expectedValue => ",",
          reason => "'.' could be a mistake because the parameters");
     end if;
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     if subprogram = eof_t then
        expectSymbol(
          expectedValue => ",",
          contextNotes => to_string( contextNotes ),
          reason => "there are too few parameters because the list"
        );
     else
        expectSymbol(
          expectedValue => ",",
          context => subprogram,
          reason => "there are too few parameters because the list"
        );
     end if;
  else
     if subprogram = eof_t then
        expectSymbol(
          contextNotes => to_string( contextNotes ),
          expectedValue => ",",
          reason  => "to separate parameters the list"
        );
     else
        expectSymbol(
          context => subprogram,
          expectedValue => ",",
          reason  => "to separate parameters the list"
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
       contextNotes => contextNotes,
       expectedValue => ",",
       reason => "';' could be a mistake because the list"
     );
  elsif token = symbol_t and identifiers( token ).value.all = "." then
     expectSymbol(
       expectedValue => ",",
       contextNotes => contextNotes,
       reason => "'.' could be a mistake because the list"
     );
  elsif token = symbol_t and identifiers( token ).value.all = ")" then
     expectSymbol(
       expectedValue => ",",
       contextNotes => contextNotes,
       reason => "there are too few parameters because the list"
     );
  else
     expectSymbol(
       contextNotes => contextNotes,
       expectedValue => ",",
       reason  => "to separate parameters the list"
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
       reason => "';' looks like parameters are missing because the list"
     );
  else
     if contextNotes = "" then
        expectSymbol(
          expectedValue => "(",
          expectPlural => true,
          context => subprogram,
          subjectNotes => "the parameters",
          reason => "are starting and"
        );
     else
        expectSymbol(
          expectedValue => "(",
          expectPlural => true,
          contextNotes => to_string( contextNotes ),
          subjectNotes => "the parameters",
          reason => "are starting and"
        );
     end if;
  end if;
  if token = symbol_t and identifiers( token ).value.all = ")" then
     if contextNotes = "" then
        err( context => subprogram,
             subjectNotes => "the parameters",
             obstructorNotes => "",
             reason => "are missing",
             remedy => "omit the parentheses if there are no required parameters"
       );
     else
        err( contextNotes => to_string( contextNotes ),
             subjectNotes => "the parameters",
             obstructorNotes => "",
             reason => "are missing",
             remedy => "omit the parentheses if there are no required parameters"
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
          reason => "';' looks like a symbol is missing because the list"
        );
     else
        expectSymbol(
          expectedValue => ")",
          contextNotes => to_string( contextNotes ),
          reason => "';' looks like a symbol is missing because the list"
        );
     end if;
  elsif token = symbol_t and identifiers( token ).value.all = "," then
     if contextNotes = "" then
        expectSymbol(
          expectedValue => ")",
          context => subprogram,
          reason => "')' looks like too many parameters because the list"
        );
     else
        expectSymbol(
          expectedValue => ")",
          contextNotes => to_string( contextNotes ),
          reason => "')' looks like too many parameters because the list"
        );
     end if;
  else
     if contextNotes = "" then
        expectSymbol(
          expectedValue => ")",
          context => subprogram,
          reason => "the end of the parameters"
        );
     else
        expectSymbol(
          expectedValue => ")",
          contextNotes => to_string( contextNotes ),
          reason => "the end of the parameters"
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

procedure expectPragmaParameterOpen( pragmaKind : string ) is
  contextNotes : unbounded_string;
begin
  if pragmaKind = "" then
     contextNotes := to_unbounded_string( "in the pragma parameter list" );
  else
     contextNotes := to_unbounded_string( "in the pragma " & pragmaKind & " parameter list" );
  end if;

  if token = symbol_t and identifiers( token ).value.all = ";" then
     expectSymbol(
       expectedValue => "(",
       contextNotes => to_string( contextNotes ),
       reason => "';' looks like parameters are missing because the list"
     );
  else
     expectSymbol(
       expectedValue => "(",
       expectPlural => true,
       contextNotes => to_string( contextNotes ),
       subjectNotes => "the parameters",
       reason => "are starting and"
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

procedure expectPragmaParameterClose( pragmaKind : string ) is
  contextNotes : unbounded_string;
begin
  if pragmaKind = "" then
     contextNotes := to_unbounded_string( "in the pragma parameter list" );
  else
     contextNotes := to_unbounded_string( "in the pragma " & pragmaKind & " parameter list" );
  end if;

  if token = symbol_t and identifiers( token ).value.all = ";" then
     expectSymbol(
       expectedValue => ")",
       contextNotes => to_string( contextNotes ),
       reason => "';' looks like a symbol is missing because the list"
     );
  elsif token = symbol_t and identifiers( token ).value.all = "," then
     expectSymbol(
       expectedValue => ")",
       contextNotes => to_string( contextNotes ),
       reason => "')' looks like too many parameters because the list"
     );
  else
     expectSymbol(
       expectedValue => ")",
       contextNotes => to_string( contextNotes ),
       reason => "the end of the pragma parameters"
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
    remedy          : string := "" ) is        -- suggested solutions
begin
  if onlyAda95 then
     err(
        userLanguage => userLanguage.all,
        context => context,
        contextType => contextType,
        subject => subject,
        reason => "is not compatible with",
        obstructorNotes => optional_yellow( "pragma ada_95" ),
        remedy => remedy
     );
  end if;
  expect( subject );
end expectAdaScript;

procedure expectAdaScript(
    contextType     : identifier := eof_t;     -- associated type (if any)
    contextNotes    : string := "";            -- notes
    subject         : identifier := eof_t;     -- which
    remedy          : string := "" ) is        -- suggested solutions
begin
  if onlyAda95 then
     err(
        userLanguage => userLanguage.all,
        contextType => contextType,
        contextNotes => contextNotes,
        subject => subject,
        reason => "is not compatible with",
        obstructorNotes => optional_yellow( "pragma ada_95" ),
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
    remedy          : string := "" ) is        -- suggested solutions
begin
  if onlyAda95 then
     err(
        userLanguage => userLanguage.all,
        context => context,
        contextType => contextType,
        subject => subject,
        reason => "has differences with Ada's version and is not fully compatible with" ,
        obstructorNotes => optional_yellow( "pragma ada_95" ),
        remedy => remedy
     );
  end if;
  expect( subject );
end expectAdaScriptDifferences;

procedure expectAdaScriptDifferences(
    contextType     : identifier := eof_t;     -- associated type (if any)
    contextNotes    : string := "";            -- notes
    subject         : identifier := eof_t;     -- which
    remedy          : string := "" ) is        -- suggested solutions
begin
  if onlyAda95 then
     err(
        userLanguage => userLanguage.all,
        contextType => contextType,
        contextNotes => contextNotes,
        subject => subject,
        reason => "has differences with Ada's version and is not fully compatible with" ,
        obstructorNotes => optional_yellow( "pragma ada_95" ),
        remedy => remedy
     );
  end if;
  expect( subject );
end expectAdaScriptDifferences;


begin
  -- the user language is hard coded, for now, to English
  userLanguage  := new englishUserLanguage;
end scanner.communications;

