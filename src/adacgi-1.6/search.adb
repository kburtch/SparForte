with CGI, Text_IO, Ada.Integer_Text_IO, Ada.Strings.Unbounded,
     Ada.Characters.Handling, Ada.Strings.Maps.Constants, Ustrings;
use  CGI, Text_IO, Ada.Integer_Text_IO, Ada.Strings.Unbounded,
     Ada.Characters.Handling, Ada.Strings.Maps.Constants, Ustrings;

procedure Search is
-- Search for a requested search string in a requested text file;
-- the request and reply use the Common Gateway Interface (CGI) to
-- an HTTP server, and then on to a user of a World Wide Web (WWW) browser.
-- It's basically a web application version of "grep", with security features
-- that let the server select which files a user can search.

-- If a search string _and_ file name is sent, a search result is returned.
-- Otherwise, the program will reply with a form to fill out.
-- If some information (such as the file to search) is provided,
-- the form is specialized returned to "remember" the previous values sent.

-- This program can search many different files, but each file _MUST_ be
-- listed in the file "srchlist". The format for the "srchlist" file
-- is a list of lines with the following format:
--   local_file_name,User_Name
-- Here's an example of an entry in "srchlist":
--   /public/addresses/phone,Phone List
-- Srchlist lines beginning with "#" are comment lines and are ignored.

-- To run this program directly (without an HTTP server), set the
-- environment variable REQUEST_METHOD to "GET" and the variable
-- QUERY_STRING to the query values, such as "" or
-- "file=Phone%20List&query=David&casesensitive=no".

-- Returning specialized forms takes a little extra effort since
-- package CGI doesn't automatically do this; a higher-level interface
-- than package CGI could simplify handling partially completed forms.


-- Copyright (C) 1995-2000 David A. Wheeler
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA





  -- CHANGE THIS LINE TO WHEREVER YOUR SEARCH LIST FILE IS:
  Search_List_Filename : constant String := "/home/httpd/srchlist";


  Search_List : File_Type;

  function User_Name(S : Unbounded_String) return Unbounded_String is
    X : Natural;
  begin
    X := Index(S, ",");
    return To_Unbounded_String(Slice(S, X+1, Length(S)));
  end User_Name;

  function Real_File_Name(S : Unbounded_String) return String is
    Line : Unbounded_String;
  begin
    while not End_Of_File(Search_List) loop
      Ustrings.Get_Line(Search_List, Line);
      if Element(Line, 1) /= '#'  then
        if User_Name(Line) = S then
          return Slice(Line, 1, Index(Line, ",") - 1);
        end if;
      end if;
    end loop;
    return "";
  end Real_File_Name;

  procedure Put_Matches(Filename : String; Pattern : String;
                        Case_Sensitive : Boolean) is
    Found_Something : Boolean := False;
    Found_Here : Boolean := False;
    Line     : Unbounded_String;
    Match_To : Unbounded_String;
    Search_File : File_Type;
    Clean_Pattern : String(1 .. Pattern'Length);
    -- Given Filename, pattern, and case sensitivity, put matches.
    -- This is currently implemented in a slow, inefficient way, but it's
    -- sufficient for the purpose for non-monstrous files.
  begin
    if not Case_Sensitive then
      Clean_Pattern := To_Lower(Pattern);
    else
      Clean_Pattern := Pattern;
    end if;
    if Filename = "" then
      Put_Line("<p><i>Sorry, that's not a searchable file.</i><p>");
      return;
    end if;
    if Pattern = "" then
      Put_Line("<p><i>Sorry, empty patterns are not permitted.</i><p>");
      return;
    end if;
    Open(Search_File, In_File, Filename);

    while not End_Of_File(Search_File) loop
      Get_Line(Search_File, Line);
      if not Case_Sensitive then
        Match_To := Translate(Line, Lower_Case_Map);
        Found_Here := (Index(Match_To, Clean_Pattern) /= 0);
      else
        Found_Here := (Index(Line, Clean_Pattern) /= 0);
      end if;
      if Found_Here then
        Put_Line(HTML_Encode(Line));
        Found_Something := True;
      end if;
    end loop;

    if not Found_Something then
      Put_Line("<i>No matches found</i>");
    end if;
  exception
    when Name_Error =>
        Put_Line("<i>File to search is not available</i>");
  end Put_Matches;

  procedure Process_Query is
    User_File_To_Search : constant String := CGI.Value("file");
    -- Note that users can't pick the filename; instead, the user provides
    -- the "file" value, and the program controls the conversion to a filename.
    -- That way, users can't view arbitrary files.
    File_To_Search : constant String := Real_File_Name(U(User_File_To_Search));
    Pattern    : constant String := Value("query");
    Case_Sensitive : Boolean := False;
    Case_Sensitivity : constant String := Value ("casesensitive");
  begin
    Put_HTML_Head("Query Result");
    Put_HTML_Heading("Query Result", 1);
    Put_Line(String'("<p>The search for <i>" & HTML_Encode(Value("query"))));
    Put_Line(String'("</i> in file <i>" & HTML_Encode(Value("file")) & "</i>"));

    if Case_Sensitivity = "yes" then
      Case_Sensitive := True;
      Put_Line(" in a case-sensitive manner");
    end if;

    Put_Line("produced the following result:<p>");
    Put_Line("<pre>");
    Flush;
    Put_Matches(File_To_Search, Pattern, Case_Sensitive);
    Put_Line("</pre>");
  end Process_Query;

  procedure Put_Select_List is
    Line : Unbounded_String;
    First_Option : Boolean := True;
    Number_Of_Options : Natural := 0;
  begin
    -- Put a Selection list of legal filenames out.

    -- Count the number of options (non-comment lines).
    while not End_Of_File(Search_List) loop
      Get_Line(Search_List, Line);
      if Element(Line, 1) /= '#'  then
        Number_Of_Options := Number_Of_Options + 1;
      end if;
    end loop;
    Reset(Search_List);

    Put("<select name=""file"" size=""");
    Put(Number_Of_Options, Width => 0);
    Put_Line(""">");

    while not End_Of_File(Search_List) loop
      Get_Line(Search_List, Line);
      if Element(Line, 1) /= '#'  then
        Put("<option value=""");
        Put(HTML_Encode(User_Name(Line)));
        Put('"');
        if First_Option then
          Put(" selected");
          First_Option := False;
        end if;
        Put('>');
        Put_Line(HTML_Encode(User_Name(Line)));
      end if;
    end loop;
    Put_Line("</select> <p>");
  end Put_Select_List;

  function Open_Search_List return Boolean is
  begin
    Open(Search_List, In_File, Search_List_Filename);
    return True;
  exception
    when Name_Error =>
        Put_Error_Message("Search List File is not available");
        return False;
    when Others =>
        Put_Error_Message("Search List File could not be opened");
        return False;
  end Open_Search_List;


  procedure Generate_Blank_Form is
    Query_String : constant String := CGI.Value ("query");
    File_Value   : constant String := CGI.Value ("file");
  begin
    Put_HTML_Head("Text Search Form");
    Put_HTML_Heading("Text Search Form", 1);
    Put_Line("<p>You may search for a text phrase");
    Put_Line(" from any of the given files.<p>");

    Put_Line("<form method=""POST"">");

    Put_Line("What do you want to search for:<p>");
    Put("<input name=""query"" size=""40""");

    -- if query was set, use it as the default.
    if Query_String /= "" then
       Put(" value=""");
       -- We encoding this so queries with '<', '"', etc work correctly.
       Put(String'(HTML_Encode(Value("query"))));
       Put('"');
    end if;
    Put_Line("><p>");

    -- If file was set, save it in the form and note it as a fixed value.
    -- Otherwise, let the user pick the file to search.
    if Key_Exists("file") and File_Value /= "" then
       Put("<input type=""hidden"" name=""file"" value=""");
       Put(String'(HTML_Encode(Value("file")) & """>"));
       Put("<p>You will be searching file <i>");
       Put(String'(HTML_Encode(Value("file"))));
       Put_Line("</i><p>");
    else
       Put_Line("Where do you want to search?<p>");
       Put_Select_List;
    end if;

    -- If "casesensitive" set, save it in the form (invisibly).
    -- Otherwise, let the user choose.
    if Key_Exists("casesensitive") then
      Put("<input type=""hidden"" name=""casesensitive"" value=""");
      Put(String'(HTML_Encode(Value("casesensitive"))));
      Put_Line(""">");
    else
      Put_Line("Do you want this search to be case-sensitive?");
      Put_Line("<dl>");
      Put_Line("<dd><input type=""radio"" name=""casesensitive"" " &
               "value=""yes""> <i>Yes.</i>");
      Put_Line("<dd><input type=""radio"" name=""casesensitive"" " &
               "value=""no"" checked> <i>No.</i>");
      Put_Line("</dl>");
    end if;


    Put_Line("<p> <input type=""submit"" value=""Submit Query"">");
    Put_Line("<input type=""reset""> ");
    Put_Line("</form>");
  end Generate_Blank_Form;

begin
  Put_CGI_Header;
  if not Open_Search_List then
    return; -- Can't open search list, don't go any further.
  end if;

  if Key_Exists("query") and Key_Exists("file") then
    Process_Query;
  else
    Generate_Blank_Form;
  end if;

  Put_HTML_Tail;
  Close(Search_List);
end Search;

