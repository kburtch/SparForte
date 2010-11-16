with Ada.Strings.Unbounded;  use  Ada.Strings.Unbounded;

package CGI is
-- This package is an Ada 95 interface to the "Common Gateway Interface" (CGI).
-- This package makes it easier to create Ada programs that can be
-- invoked by World-Wide-Web HTTP servers using the standard CGI interface.
-- CGI is rarely referred to by its full name, so the package name is short.
-- General information on CGI is at "http://hoohoo.ncsa.uiuc.edu/cgi/"
-- and "http://w3.org/CGI".

-- Developed by (C) David A. Wheeler (dwheeler@dwheeler.com) June 1995-2000.
-- This is version 1.6.

-- For more information, see adacgi's home page at
--    http://www.dwheeler.com/adacgi
-- The worldwide name of the entire set of components is "adacgi",
-- while the name of this particular Ada package is "CGI".

-- LICENSE PREAMBLE:
-- AdaCGI is an open source (free software) library.
-- AdaCGI is released using the LGPL license along with an exception and
-- clarification. You can use this library to develop proprietary programs;
-- if you don't make changes to this library, there is no problem and you
-- can use it as you wish.
--
-- If you _DO_ make changes to this library and use the result, then those
-- changes must be made available to users for further use and distribution.
-- In short, you are not allowed to make changes to the library and make
-- the resulting library proprietary.  If you use the modified library
-- for a server on the Internet, this means everyone on the Internet must
-- be able to get the modified version of the library.
--
-- Naturally, you can also make open source programs through this license,
-- and making open source programs using this library is strongly
-- encouraged (but not required).

-- LICENSE:
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License (LGPL) as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,
-- Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files simply instantiate generics from
-- this unit, or you simply link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU Lesser General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file  might be covered by the GNU Lesser General Public License.
--
-- As a clarification, when this library is used in a server serving
-- a client (such as a user browser invoking a server program),
-- for purposes of this license the client is executing
-- the program, even when this execution occurs remotely.
-- After all, the client sends the program input, causes the program to
-- execute, and receives program output.
-- As a consequence, if you make any modifications to this library,
-- you MUST make those modifications available to all users of clients
-- under the terms of this license.  As described in the LGPL, those
-- clients then have certain rights to redistribute the library.


-- This library was inspired by a perl binding by Steven E. Brenner at
--   "http://www.bio.cam.ac.uk/web/form.html"
-- and another perl binding by L. Stein at
--   "http://www-genome.wi.mit.edu/ftp/pub/software/WWW/cgi_docs.html"
-- A different method for interfacing binding Ada with CGI is to use the
-- "Un-CGI" interface at "http://www.hyperion.com/~koreth/uncgi.html".

-- This package automatically loads information from CGI on program start-up.
-- It loads information sent from "Get" or "Post" methods and automatically
-- splits the data into a set of variables that can be accessed by position or
-- by name.  An "Isindex" request is translated into a request with a single
-- key named "isindex" with its Value as the query value.

-- This package provides two data access methods:
-- 1) As an associative array; simply provide the key name and the
--    value associated with that key will be returned.
-- 2) As a sequence of key-value pairs, indexed from 1 to Argument_Count.
--    This is similar to Ada library Ada.Command_Line.
-- The main access routines support both String and Unbounded_String.

-- See the documentation file for more information and sample programs.

function Parsing_Errors return Boolean;  -- True if Error on Parse.
function Input_Received return Boolean;  -- True if Input Received.
function Is_Index       return Boolean;  -- True if an Isindex request made.
  -- An "Isindex" request is turned into a Key of "isindex" at position 1,
  -- with Value(1) as the actual query.

-- Report the CGI Method; where possible, don't depend on this.
type CGI_Method_Type is (Get, Post, Unknown);
function CGI_Method return CGI_Method_Type;

-- Access data as an associative array - given a key, return its value.
-- The Key value is case-sensitive.
-- If a key is required but not present, raise Constraint_Error;
-- otherwise a missing key's value is considered to be "".
-- These routines find the Index'th value of that key (normally the first one).
function Value(Key : in Unbounded_String; Index : in Positive := 1;
               Required : in Boolean := False) return Unbounded_String;
function Value(Key : in String; Index : in Positive := 1;
               Required : in Boolean := False) return String;
function Value(Key : in Unbounded_String; Index : in Positive := 1;
              Required : in Boolean := False) return String;
function Value(Key : in String; Index : in Positive := 1;
               Required : in Boolean := False) return Unbounded_String;

-- Was a given key provided?
function Key_Exists(Key : in String; Index : in Positive := 1) return Boolean;
function Key_Exists(Key : in Unbounded_String; Index : in Positive := 1)
         return Boolean;

-- How many of a given key were provided?
function Key_Count(Key : in String) return Natural;
function Key_Count(Key : in Unbounded_String) return Natural;


-- Access data as an ordered list (it was sent as Key=Value);
-- Keys and Values may be retrieved from Position (1 .. Argument_Count).
-- Constraint_Error will be raised if Position<1 or Position>Argument_Count
function Argument_Count return Natural;          -- 0 means no data sent.
function Key(Position : in Positive) return Unbounded_String;
function Key(Position : in Positive) return String;
function Value(Position : in Positive) return Unbounded_String;
function Value(Position : in Positive) return String;

-- The following are helpful subprograms to simplify use of CGI.

function Key_Value_Exists(Key : in Unbounded_String;
                          Value : in Unbounded_String) return Boolean;
function Key_Value_Exists(Key : in String;
                          Value : in String) return Boolean;
-- Returns True if a given Key has exactly Value as one of its values.

-- Iterators: through all CGI values of a given key, or all CGI values.
generic
  with procedure Evaluate (Value : in Unbounded_String);
  procedure Iterate_Key (Key : in String);

generic
  with procedure Evaluate (Key   : in Unbounded_String;
                           Value : in Unbounded_String);
  procedure Iterate_CGI;


-- Useful output routines:

procedure Put_CGI_Header(Header : in String := "Content-type: text/html");
-- Put CGI Header to Current_Output, followed by two carriage returns.
-- This header determines what the program's reply type is.
-- Default is to return a generated HTML document.
-- Warning: Make calls to Set_Cookie before calling this procedure!

procedure Put_HTML_Head(Title : in String; Mail_To : in String := "");
-- Puts to Current_Output an HTML header with title "Title".  This is:
--   <html><head><title> _Title_ </title>
--   <link rev="made" href="mailto:  _Mail_To_ ">
--   </head><body>
-- If Mail_To is omitted, the "made" reverse link is omitted.

procedure Put_HTML_Heading(Title : in String; Level : in Positive);
-- Put an HTML heading at the given level with the given text.
-- If level=1, this puts:  <h1>Title</h1>.

procedure Put_HTML_Tail;
-- This is called at the end of an HTML document. It puts to Current_Output:
--   </body></html>

procedure Put_Error_Message(Message : in String);
-- Put to Current_Output an error message.
-- This Puts an HTML_Head, an HTML_Heading, and an HTML_Tail.
-- Call "Put_CGI_Header" before calling this.

procedure Put_Variables;
-- Put to Current_Output all of the CGI variables as an HTML-formatted String.


-- Miscellaneous Routines:

function My_URL return String; -- Returns the URL of this script.

function Get_Environment(Variable : in String) return String;
-- Return the given environment variable's value.
-- Returns "" if the variable does not exist.


-- Multi-Line data support:

function Line_Count (Value : in String) return Natural;
-- Given a value that may have multiple lines, count the lines.
-- Returns 0 if Value is the empty/null string (i.e., length=0)
 
function Line_Count_of_Value (Key : String) return Natural;
-- Given a Key which has a Value that may have multiple lines,
-- count the lines.  Returns 0 if Key's Value is the empty/null
-- string (i.e., length=0) or if there's no such Key.
-- This is the same as Line_Count(Value(Key)).

function Line (Value : in String; Position : in Positive)
               return String;
-- Given a value that may have multiple lines, return the given line.
-- If there's no such line, raise Constraint_Error.

function Value_of_Line (Key : String; Position : Positive)
                        return String;
-- Given a Key which has a Value that may have multiple lines,
-- return the given line.  If there's no such line, raise Constraint_Error.
-- If there's no such Key, return the null string.
-- This is the same as Line(Value(Key), Position).


-- Encoding and Decoding functions:

procedure URL_Decode(Data : in out Unbounded_String;
                     Translate_Plus : Boolean := True);
-- In the given string, convert pattern %HH into alphanumeric characters,
-- where HH is a hex number. Since this encoding only permits values
-- from %00 to %FF, there's no need to handle 16-bit characters.
-- If "Translate_Plus" is True, translate '+' to ' '.

function URL_Decode(Data : in Unbounded_String;
                    Translate_Plus : Boolean := True) return Unbounded_String;
-- Returns the decoded value (instead of tranlating in-place)

procedure URL_Encode(Data : in out Unbounded_String;
                     Translate_Plus : Boolean := False);
-- Given a string, encode to %HH all characters not URL-safe;
-- if "Translate_Plus" is True, translate '+' to ' '.

function URL_Encode(Data : in Unbounded_String;
                    Translate_Plus : Boolean := False)
         return Unbounded_String;
-- Same as procedure, but returns a new Unbounded_String.

procedure HTML_Encode(Data : in out Unbounded_String);
-- Given string, perform HTML encoding, so the text can be included
-- in an HTML file.  This means '&' becomes '&amp;', '<' becomes '&lt;',
-- '>' becomes '&gt;', and '"' becomes '&quot;'.
-- All other characters are untouched.
-- ALL VARIABLE DATA sent from the application should be filtered through
-- HTML_Encode unless it's already in HTML format or you know that
-- it can't have these special characters.
-- Even if the data appears to have come from the user, it should be filtered;
-- the user may be unknowingly clicking though a malicious link.

function HTML_Encode(Data : in Unbounded_String) return Unbounded_String;
function HTML_Encode(Data : in String) return String;
-- Same as procedure, but returns a new value.


-- Cookie handling subprograms
-- (Note that cookies are automatically read when the program starts):

procedure Set_Cookie(Key : String;
                     Value : String;
                     Expires : String := "";
                     Path: String := Get_Environment("PATH_INFO");
                     Domain: String := Get_Environment("SERVER_NAME");
                     Secure: Boolean := False );
-- Sets a cookie value; call this BEFORE calling Put_CGI_Header.
-- If you don't want to send values for Expires, Path, or Domain,
-- just make them "".

function Cookie_Value(Key : in Unbounded_String; Index : in Positive := 1;
   Required : in Boolean := False)
   return Unbounded_String;
function Cookie_Value(Key : in String; Index : in Positive := 1;
   Required : in Boolean := False)
   return String;
function Cookie_Value(Key : in String; Index : in Positive := 1;
   Required : in Boolean := False)
   return Unbounded_String;
function Cookie_Value(Key : in Unbounded_String; Index : in Positive := 1;
   Required : in Boolean := False)
   return String;
function Cookie_Value(Position : in Positive) return Unbounded_String;
function Cookie_Value(Position : in Positive) return String;

function Cookie_Count return Natural;
-- Returns the number of cookies (0 if none)

end CGI;

