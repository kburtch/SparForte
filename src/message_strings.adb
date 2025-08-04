------------------------------------------------------------------------------
-- SparForte markup messages and escaping functions                         --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with CGI,
     pegasoft.strings,
     pegasoft.user_io,
     spar_os,
     symbol_table;
use  CGI,
     spar_os,
     pegasoft.strings,
     pegasoft.user_io,
     symbol_table;

package body message_strings is

--
-- Message "mark up" functions
--
------------------------------------------------------------------------------


--  ESCAPE CHAR (local)
--
-- Render a character for the output format, escaping or converting
-- characters as needed.  Currently, the escaping does not support UTF-8.
----------------------------------------------------------------------------

function escapeChar( ch : character ) return messageStrings is
  newText : messageStrings;
begin
  case templateHeader.templateType  is
  when htmlTemplate  | xmlTemplate | wmlTemplate =>
    -- spaces and line feeds are special cases
    if ch = ' ' then
       newText.templateMessage := to_unbounded_string( "&nbsp;" );
    elsif ch = ASCII.LF then
       newText.templateMessage := to_unbounded_string( "<br>" );
    else
       newText.templateMessage := to_unbounded_string(
         CGI.html_encode(
           to_string(
             ToCtrlEscaped(
               to_unbounded_string( "" & ch )
             )
           )
         )
       );
    end if;
  when jsonTemplate =>
    newText.templateMessage := toJSONEscaped( to_unbounded_string( "" & ch ) );
  when YAMLTemplate =>
    -- this escapes comments, quotes, newlines.  It is not exhaustive.
    if ch = '#' then
       newText.templateMessage := to_unbounded_string( "\#" );
    elsif ch = ASCII.LF then
       newText.templateMessage := to_unbounded_string( "\n" );
    elsif ch = ASCII.CR then
       newText.templateMessage := to_unbounded_string( "\r" );
    elsif ch = ASCII.Quotation then
       newText.templateMessage := to_unbounded_string( "\""" );
    elsif ch = ''' then
       newText.templateMessage := to_unbounded_string( "\'" );
    else
       newText.templateMessage := ToCtrlEscaped( to_unbounded_string( "" & ch ) );
    end if;
  when others => -- including noTemplate, textTemplate escape control chars
     newText.templateMessage := ToCtrlEscaped( to_unbounded_string( "" & ch ) );
  end case;
  -- the text alternative also escapes control characters
       newText.gccMessage := ToCtrlEscaped( to_unbounded_string( "" & ch ) );
  return newText;
end escapeChar;

-----------------------------------------------------------------------------
--  GET NEW LINE
--
-- Return a new line as appropriate for the output context.
-----------------------------------------------------------------------------

function getNewLine return messageStrings is
  nl : messageStrings;
begin
  if hasTemplate then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
         nl.templateMessage := to_unbounded_string( "<br>" & ASCII.CR & ASCII.LF );
     when tomlTemplate | yamlTemplate =>
         nl.templateMessage := to_unbounded_string( eol_characters & "# " );
     when others =>
         nl.templateMessage := to_unbounded_string( eol_characters );
     end case;
   else
     nl.templateMessage := to_unbounded_string( eol_characters );
   end if;
   nl.gccMessage := to_unbounded_string( eol_characters );
   return nl;
end getNewLine;


-----------------------------------------------------------------------------
-- Concatenation
-----------------------------------------------------------------------------

function "&"( left, right : messageStrings ) return messageStrings is
  new_strings : messageStrings;
  --temp : messageStrings;
begin

  new_strings.templateMessage := left.templateMessage;

  -- Text wrap on concatenation
  --
  -- This assumes messages are built left to right
  --
  -- Base the length of the message on the number of characters in the
  -- text message.  If the new text exceeds the wrap point, move to the
  -- next line and advance to the next wrap point (i.e. 80 + 80 = 160 chars)

  --if not hasTemplate or templateheader.templateType = noTemplate then
     --if left.wrapPoint > 0 then
     --   new_strings.wrapPoint := left.wrapPoint;
     --elsif right.wrapPoint > 0 then
     --   new_strings.wrapPoint := right.wrapPoint;
     --else
     --  new_strings.wrapPoint := integer( displayInfo.col );
     --end if;

     --if length( left.textMessage & right.textMessage ) > new_strings.wrapPoint then
        -- TODO: may not be perfect
     --   new_strings.wrapPoint := length(left.textMessage) + integer( displayInfo.col );
     --   temp := getNewLine;
     --   new_strings.templateMessage := new_strings.templateMessage & temp.templateMessage;
     --   --end loop;
     --end if;
  --end if;
  new_strings.templateMessage := new_strings.templateMessage & right.templateMessage;

  new_strings.gccMessage     := left.gccMessage & right.gccMessage;
--put_line( "wrap point = " & new_strings.wrapPoint'img & ";" & new_strings.templateMessage ); -- DEBUG
  return new_strings;
end "&";


-----------------------------------------------------------------------------
--  PL (plain text)
--
-- Return the text escaped for the current output context.
-----------------------------------------------------------------------------

function pl( s : string ) return messageStrings is
  msg : messageStrings;
begin
  for i in s'range loop
      msg := msg & escapeChar( s(i) );
  end loop;
  if gccOpt then
      msg.templateMessage := msg.gccMessage;
  end if;
  return msg;
end pl;

function pl( c : character ) return messageStrings is
begin
  return pl( "" & c );
end pl;


-----------------------------------------------------------------------------
--  UNB (unbounded string) PL (plain)
--
-- This does not use overloading because of the ambiguity that often happens
-- in strings expressions.
-----------------------------------------------------------------------------

function unb_pl( us : unbounded_string ) return messageStrings is
begin
  return pl( to_string( us ) );
end unb_pl;


-----------------------------------------------------------------------------
--  INV (inverse)
--
-- Return the text as red, inverse or italic in the current output context.
-- If GCC errors, return as-is.  If console, return as red or bold.  If an
-- HTML template, italic (not bold).
-----------------------------------------------------------------------------

function inv( s : string ) return messageStrings is
begin
  if gccOpt then
     return messageStrings'(
        to_unbounded_string(s),
        to_unbounded_string(s)
     );
  elsif hasTemplate then
     case templateHeader.templateType is
     when htmlTemplate =>
        return messageStrings'(
           to_unbounded_string( "<span style=""font-style:italic"">" &
             CGI.html_encode( s ) & "</span>" ),
           to_unbounded_string( s )
        );
     when others =>
        return messageStrings'(
           to_unbounded_string(s),
           to_unbounded_string(s)
        );
     end case;
  elsif colourOpt then
     return messageStrings'(
        to_unbounded_string( red( s ) ),
        to_unbounded_string( s )
     );
  end if;
  return messageStrings'(
     to_unbounded_string( inverse( s ) ),
     to_unbounded_string( s )
  );
end inv;


-----------------------------------------------------------------------------
--  OK (success text)
--
-- Return the text as green in the current output context.
-- If GCC errors, return as-is.  If console, return as green or bold.  If an
-- HTML template, bold.
-----------------------------------------------------------------------------

function ok( s : string ) return messageStrings is
begin
  if gccOpt then
     return messageStrings'(
        to_unbounded_string(s),
        to_unbounded_string(s)
     );
  elsif hasTemplate then
     case templateHeader.templateType is
     when htmlTemplate =>
        return messageStrings'(
           to_unbounded_string( "<span style=""font-style:bold"">" &
             CGI.html_encode( s ) & "</span>" ),
           to_unbounded_string( s )
        );
     when others =>
        return messageStrings'(
           to_unbounded_string(s),
           to_unbounded_string(s)
        );
     end case;
  elsif colourOpt then
     return messageStrings'(
        to_unbounded_string( green( s ) ),
        to_unbounded_string( s )
     );
  end if;
  return messageStrings'(
     to_unbounded_string( inverse( s ) ),
     to_unbounded_string( s )
  );
end ok;


-----------------------------------------------------------------------------
--  EM (emphasis)
--
-- Return the text as emphasized in the current output context.  If GCC
-- errors, return as-is.  If console, returns as colour or bold.  If
-- a HTML template, bold (not italics).
-----------------------------------------------------------------------------

function em( s : string ) return messageStrings is
begin
  if gccOpt then
     return messageStrings'(
        to_unbounded_string(s),
        to_unbounded_string(s)
     );
  elsif hasTemplate then
     case templateHeader.templateType is
     when htmlTemplate =>
        return messageStrings'(
            to_unbounded_string( "<span style=""font-weight:bold"">"  &
            CGI.html_encode( s ) &
            "</span>" ),
            to_unbounded_string(s)
        );
     when others =>
        return messageStrings'(
           to_unbounded_string(s),
           to_unbounded_string(s)
        );
     end case;
  elsif colourOpt then
     return messageStrings'(
        to_unbounded_string( yellow( s ) ),
        to_unbounded_string( s )
     );
  end if;
  return messageStrings'(
     to_unbounded_string( bold( s ) ),
     to_unbounded_string( s )
  );
end em;


-----------------------------------------------------------------------------
--  UNB (unbounded string) EM (emphasis)
--
-- This does not use overloading because of the ambiguity that often happens
-- in strings expressions.
-----------------------------------------------------------------------------

function unb_em( us : unbounded_string ) return messageStrings is
begin
  return em( to_string( us ) );
end unb_em;


-----------------------------------------------------------------------------
--  EM (emphasis) plus ESC (escape)
--
-----------------------------------------------------------------------------

function em_esc( s : unbounded_string ) return messageStrings is
begin
  return em( to_string( toEscaped( s ) ) );
end em_esc;

function em_esc( c : character ) return messageStrings is
begin
  return em_esc( to_unbounded_string( "" & c ) );
end em_esc;


-----------------------------------------------------------------------------
--  EM (emphasis) VALUE
--
-- combines bold, secure data and escaped.  A null value will
-- return double single quotes.
-----------------------------------------------------------------------------

function em_value( s : unbounded_string ) return messageStrings is
begin
 if s = "" then
     return pl( "''" );
  else
     return em( toSecureData( to_string( toCtrlEscaped( s ) ) ) );
  end if;
end em_value;


-----------------------------------------------------------------------------
--  QP (Quiet Proper)
--
-- Uppercase the first letter of the string if quiet option is in use.
-- The string should then passed to pl() or em() to turn into a message string.
-- This depends on English being the display language as other languages may
-- differ but this has not taken the display language into account.
-----------------------------------------------------------------------------

function qp( s : string ) return string is
  -- TODO: this should be more efficient and not use an unbounded string
  unb : constant unbounded_string := to_unbounded_string( s );
begin
  if quietOpt then
     if s /= "" then
        return to_string( ToUpper( Head( unb, 1 ) ) & Slice( unb, 2, length( unb ) ) );
     end if;
  end if;
  return s;
end qp;

-----------------------------------------------------------------------------
--  (identifier) NAME EM (emphasis)
--
-- This happens a lot.
-----------------------------------------------------------------------------

function name_em( id : identifier ) return messageStrings is
begin
  return em( to_string( identifiers( id ).name ) );
end name_em;

end message_strings;

