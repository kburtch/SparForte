with Ada.Strings.Maps, Ada.Characters.Handling, Interfaces.C.Strings, Text_IO;
use  Ada.Strings.Maps, Ada.Characters.Handling, Interfaces.C.Strings, Text_IO;
with Ada.Strings.Maps.Constants;
use  Ada.Strings.Maps.Constants;

package body CGI is
-- This package is an Ada 95 interface to the "Common Gateway Interface" (CGI).
-- This package makes it easier to create Ada programs that can be
-- invoked by HTTP servers using CGI.

-- Developed by David A. Wheeler, dwheeler@dwheeler.com, (C) 1995-2000.
-- See the specification file for the license.

-- The following are key types and constants.

type Key_Value_Pair is record
   Key, Value : Unbounded_String;
   end record;

type Key_Value_Sequence is array(Positive range <>) of Key_Value_Pair;
type Access_Key_Value_Sequence is access Key_Value_Sequence;

Cookie_Data:  Access_Key_Value_Sequence;

Ampersands    : constant Character_Set      := To_Set('&');
Equals        : constant Character_Set      := To_Set('=');
Plus_To_Space : constant Character_Mapping  := To_Mapping("+", " ");
Semicolon     : constant Character_Set      := To_Set(';');
Unescaped_URL : constant Character_Set      := Alphanumeric_Set or
                                               To_Set("-_.!~*'()");



-- The following are data internal to this package.

Parsing_Errors_Occurred : Boolean := True;
Is_Index_Request_Made   : Boolean := False; -- Isindex request made?

CGI_Data : Access_Key_Value_Sequence; -- Initially nil.

Actual_CGI_Method : CGI_Method_Type := Get;


-- The following are private "Helper" subprograms.

function Value_Without_Exception(S : chars_ptr) return String is
pragma Inline(Value_Without_Exception);
-- Translate S from a C-style char* into an Ada String.
-- If S is Null_Ptr, return "", don't raise an exception.
begin
  if S = Null_Ptr then return "";
  else return Value(S);
  end if;
end Value_Without_Exception;


function Image(N : Natural) return String is
-- Convert Positive N to a string representation.  This is just like
-- Ada 'Image, but it doesn't put a space in front of it.
 Result : String := Natural'Image(N);
begin
 return Result( 2 .. Result'Length);
end Image;


function Field_End(Data: Unbounded_String; Field_Separator: Character;
               Starting_At : Positive := 1) return Natural is
-- Return the end-of-field position in Data after "Starting_Index",
-- assuming that fields are separated by the Field_Separator.
-- If there's no Field_Separator, return the end of the Data.
begin
  for I in Starting_At .. Length(Data) loop
    if Element(Data, I) = Field_Separator then return I-1; end if;
  end loop; 
  return Length(Data);
end Field_End;


function Hex_Value(H : in String) return Natural is
 -- Given hex string, return its Value as a Natural.
 Value : Natural := 0;
begin
 for P in H'Range loop
   Value := Value * 16;
   if H(P) in '0' .. '9' then Value := Value + Character'Pos(H(P)) -
                                               Character'Pos('0');
   elsif H(P) in 'A' .. 'F' then Value := Value + Character'Pos(H(P)) -
                                               Character'Pos('A') + 10;
   elsif H(P) in 'a' .. 'f' then Value := Value + Character'Pos(H(P)) -
                                               Character'Pos('a') + 10;
   else raise Constraint_Error;
   end if;
 end loop;
 return Value;
end Hex_Value;


procedure URL_Decode(Data : in out Unbounded_String;
                     Translate_Plus : Boolean := True) is
 I : Positive := 1;
 Last_Possible_Position : Integer := Length(Data) - 2;
begin
 -- For URL/URI encoding, see IETF RFC 2396.
 while I <= Last_Possible_Position loop
   if Element(Data, I) = '%' and then
        Is_Hexadecimal_Digit(Element(Data, I+1)) and then
        Is_Hexadecimal_Digit(Element(Data, I+2)) then
     Replace_Element(Data, I, Character'Val(Hex_Value(Slice(Data, I+1, I+2))));
     Delete(Data, I+1, I+2);
     Last_Possible_Position := Last_Possible_Position - 2;
   end if;
   I := I + 1;
 end loop;
 if Translate_Plus then
   Translate(Data, Mapping => Plus_To_Space);
 end if;
end URL_Decode;

function URL_Decode(Data : in Unbounded_String;
                    Translate_Plus : Boolean := True)
         return Unbounded_String is
 Destination : Unbounded_String := Data;
begin
 URL_Decode(Destination); 
 return Destination;
end URL_Decode;

function To_Hex_Char(Number : Natural ) return Character is
begin
 -- Assumes ASCII (or at least continuity in 0..9 and A..F).
 if Number < 10 then
   return Character'Val(Number + Character'Pos('0'));
 else
   return Character'Val(Number + Character'Pos('A') - 10);
 end if;
end To_Hex_Char;

procedure URL_Encode(Data : in out Unbounded_String;
                     Translate_Plus : Boolean := False) is
 I : Positive := 1;
 Current_Character : Character;
 Replacer : String := "%xx"; -- The String we replace with.
 Last_Position : Natural := Length(Data);
begin
 -- For URL/URI encoding, see IETF RFC 2396.
 while I <= Last_Position loop
   Current_Character := Element(Data, I);
   if Translate_Plus and Current_Character = '+' then
     Replace_Element(Data, I, ' ');
   elsif not Is_In(Current_Character, Unescaped_URL) then
     -- Character I isn't safe, replace it:
     Replacer(2) := To_Hex_Char(Character'Pos(Current_Character) / 16);
     Replacer(3) := To_Hex_Char(Character'Pos(Current_Character) mod 16);
     Replace_Slice(Data, I, I, Replacer);
     Last_Position := Last_Position + 2;
   end if;
   I := I + 1;
 end loop;
end URL_Encode;

function URL_Encode(Data : in Unbounded_String;
                    Translate_Plus : Boolean := False)
         return Unbounded_String is
 Destination : Unbounded_String := Data;
begin
 URL_Encode(Destination); 
 return Destination;
end URL_Encode;

procedure HTML_Encode(Data : in out Unbounded_String) is
  I : Positive := 1;
  Current_Length : Natural := Length(Data);
  Current_Character : Character;
begin
 while I <= Current_Length loop
   Current_Character := Element(Data, I);
   -- Note: some old documents recommend translating double quote ("),
   -- but some browsers don't handle its replacement (&quot;) correctly
   -- and there's no need to encode double quote anyway.
   -- This encoder doesn't encode control characters, etc; since they're
   -- unambiguous there's no need to.
   if Current_Character = '&' then
      Replace_Slice(Data, I, I, "&amp;");
      Current_Length := Current_Length + 4;
   elsif Current_Character = '<' then 
      Replace_Slice(Data, I, I, "&lt;");
      Current_Length := Current_Length + 3;
   elsif Current_Character = '>' then 
      Replace_Slice(Data, I, I, "&gt;");
      Current_Length := Current_Length + 3;
   elsif Current_Character = '"' then 
      -- This is problematic, because some very old browsers don't handle
      -- &quot; correctly.  However, we HAVE to do this, because otherwise
      -- attribute values will incorrectly terminate in odd places.
      -- It _is_ in the standard, and current versions of Netscape Navigator,
      -- Microsoft Internet Explorer, and lynx all handle this correctly.
      Replace_Slice(Data, I, I, "&quot;");
      Current_Length := Current_Length + 5;
   end if;
   I := I + 1;
 end loop; 
end HTML_Encode;

function HTML_Encode(Data : in Unbounded_String)
         return Unbounded_String is
 Destination : Unbounded_String := Data;
begin
 HTML_Encode(Destination); 
 return Destination;
end HTML_Encode;

function HTML_Encode(Data : in String) return String is
 Destination : Unbounded_String := To_Unbounded_String(Data);
begin
 HTML_Encode(Destination); 
 return To_String(Destination);
end HTML_Encode;

-- Don't have an HTML_Encode with "in out String", since
-- HTML_Encode needs to be able to change the length of the result.

-- Perhaps someday I'll do "HTML_Decode".  Patches welcome.
-- As far as I can tell, HTML_Decode is a lot less needed.


-- The following are public subprograms.


function Get_Environment(Variable : String) return String is
-- Return the value of the given environment variable.
-- If there's no such environment variable, return an empty string.

  function getenv(Variable : chars_ptr) return chars_ptr;
  pragma Import(C, getenv);
  -- getenv is a standard C library function; see K&R 2, 1988, page 253.
  -- it returns a pointer to the first character; do NOT free its results.

  Variable_In_C_Format : chars_ptr := New_String(Variable);
  Result_Ptr : chars_ptr := getenv(Variable_In_C_Format);
  Result : String := Value_Without_Exception(Result_Ptr);
begin
 Free(Variable_In_C_Format);
 return Result;
end Get_Environment;


function Parsing_Errors return Boolean is
begin
 return Parsing_Errors_Occurred;
end Parsing_Errors;


function Argument_Count return Natural is
begin
  if CGI_Data = null then return 0;
  else                   return CGI_Data.all'Length;
  end if;
end Argument_Count;


function Input_Received return Boolean is
  -- True if Input Received.
begin
  return Argument_Count /= 0; -- Input received if nonzero data entries.
end Input_Received;


function CGI_Method return CGI_Method_Type is
  -- Return Method used to send data.
begin
  return Actual_CGI_Method;
end CGI_Method;


function Is_Index return Boolean is
begin
  return Is_Index_Request_Made;
end Is_Index;


function Value(Key : in Unbounded_String; Index : in Positive := 1;
               Required : in Boolean := False)
         return Unbounded_String is
 My_Index : Positive := 1;
begin
 for I in 1 .. Argument_Count loop
   if CGI_Data.all(I).Key = Key then
      if Index = My_Index then
        return CGI_Data.all(I).Value;
      else
        My_Index := My_Index + 1;
      end if;
   end if;
 end loop;
 -- Didn't find the Key.
 if Required then
   raise Constraint_Error;
 else
   return To_Unbounded_String("");
 end if;
end Value;


function Value(Key : in String; Index : in Positive := 1;
               Required : in Boolean := False)
         return String is
begin
  return To_String(Value(To_Unbounded_String(Key), Index, Required));
end Value;


function Value(Key : in String; Index : in Positive := 1;
               Required : in Boolean := False)
         return Unbounded_String is
begin
  return Value(To_Unbounded_String(Key), Index, Required);
end Value;


function Value(Key : in Unbounded_String; Index : in Positive := 1;
               Required : in Boolean := False)
         return String is
begin
  return To_String(Value(Key, Index, Required));
end Value;


function Key_Exists(Key : in Unbounded_String; Index : in Positive := 1)
         return Boolean is
 My_Index : Positive := 1;
begin
 for I in 1 .. Argument_Count loop
   if CGI_Data.all(I).Key = Key then
      if Index = My_Index then
        return True;
      else
        My_Index := My_Index + 1;
      end if;
   end if;
 end loop;
 return False;
end Key_Exists;

function Key_Exists(Key : in String; Index : in Positive := 1) return Boolean is
begin
 return Key_Exists(To_Unbounded_String(Key), Index);
end Key_Exists;

function Key_Count(Key : in Unbounded_String) return Natural is
 Count : Natural := 0;
begin
 for I in 1 .. Argument_Count loop
   if CGI_Data.all(I).Key = Key then
        Count := Count + 1;
   end if;
 end loop;
 return Count;
end Key_Count;

function Key_Count(Key : in String) return Natural is
begin
  return Key_Count(To_Unbounded_String(Key));
end Key_Count;


function Key_Value_Exists(Key : in Unbounded_String;
                          Value : in Unbounded_String)
         return Boolean is
 My_Index : Positive := 1;
begin
 for I in 1 .. Argument_Count loop
   if CGI_Data.all(I).Key = Key and then
      CGI_Data.all(I).Value = Value then
        return True;
   end if;
 end loop;
 return False;
end Key_Value_Exists;

function Key_Value_Exists(Key : in String;
                          Value : in String)
         return Boolean is
begin
 return Key_Value_Exists(To_Unbounded_String(Key), To_Unbounded_String(Value));
end Key_Value_Exists;

function Key(Position : in Positive) return Unbounded_String is
begin
 return CGI_Data.all(Position).Key;
end Key;


function Key(Position : in Positive) return String is
begin
 return To_String(Key(Position));
end Key;


function Value(Position : in Positive) return Unbounded_String is
begin
 return CGI_Data.all(Position).Value;
end Value;


function Value(Position : in Positive) return String is
begin
 return To_String(Value(Position));
end Value;

procedure Iterate_Key (Key : in String) is
 My_Index : Positive := 1;
begin
 for I in 1 .. Argument_Count loop
   if CGI_Data.all(I).Key = Key then
     Evaluate(CGI_Data.all(I).Value);
   end if;
 end loop;
end Iterate_Key;

procedure Iterate_CGI is
 My_Index : Positive := 1;
begin
 for I in 1 .. Argument_Count loop
     Evaluate(CGI_Data.all(I).Key, CGI_Data.all(I).Value);
 end loop;
end Iterate_CGI;



function My_URL return String is
 -- Returns the URL of this script.
begin
  return "http://" & Get_Environment("SERVER_NAME") &
          Get_Environment("SCRIPT_NAME");
end My_URL;


procedure Put_CGI_Header(Header : in String := "Content-type: text/html") is
-- Put Header to Current_Output, followed by two carriage returns.
-- Default is to return a generated HTML document.
begin
  Put_Line(Header);
  New_Line;
end Put_CGI_Header;


procedure Put_HTML_Head(Title : in String; Mail_To : in String := "") is
begin
  Put_Line("<html><head><title>" & Title & "</title>");
  if Mail_To /= "" then
    Put_Line("<link rev=""made"" href=""mailto:" &  mail_to  & """>");
  end if;
  Put_Line("</head><body>");
end Put_HTML_Head;


procedure Put_HTML_Heading(Title : in String; Level : in Positive) is
-- Put an HTML heading, such as <h1>Title</h1>
begin
  Put_Line("<h" & Image(Level) & ">" & Title & "</h" & Image(Level) & ">");
end Put_HTML_Heading;
 

procedure Put_HTML_Tail is
begin
  Put_Line("</body></html>");
end Put_HTML_Tail;


procedure Put_Error_Message(Message : in String) is
-- Put to Current_Output an error message.
begin
  Put_HTML_Head("Fatal Error Encountered by Script " & My_URL);
  Put_HTML_Heading("Fatal Error: " & Message, 1);
  Put_HTML_Tail;
  New_Line;
  Flush;
end Put_Error_Message;


procedure Put_Variables is
-- Put to Current_Output all of the data as an HTML-formatted String.
begin
 Put_Line("<pre>");
 for I in 1 .. Argument_Count loop
   Put("<b>");
   Put(To_String(HTML_Encode(CGI_Data.all(I).Key)));
   Put("</b>: <i>");
   Put(To_String(HTML_Encode(CGI_Data.all(I).Value)));
   Put_Line("</i>");
 end loop;
 Put_Line("</pre>");
end Put_Variables;



-- Helper routine -
 
function Next_CRLF (S : in String; N : in Natural)
         return Natural
-- Return the location within the string of the next CRLF sequence
-- beginning with the Nth character within the string S;
-- return 0 if the next CRLF sequence is not in the string
is
   I : Natural := N;
begin
   while I < S'LAST loop
      if S(I) = ASCII.CR  and then  S(I+1) = ASCII.LF then
         return I;
      else
         I := I + 1;
      end if;
   end loop;
   return 0;
end;
 
 
 
function Line_Count (Value : in String) return Natural
-- Count the number of lines inside the given string.
-- returns 0 if Key_Value is the empty/null string,
-- i.e., if its length is zero; otherwise, returns
-- the number of "lines" in Key_Value, effectively
-- returning the number of CRLF sequences + 1;
-- for example, both "AB/CDEF//GHI" and "AB/CDEF//"
-- (where / is CRLF) return Line_Count of 4.
is
   Number_of_Lines : Natural := 0;
   I : Natural := Value'FIRST;
begin
   if Value'LENGTH = 0 then
      return 0;
   else
      loop
         I := Next_CRLF (Value, I+1);
      exit when I = 0;
         Number_of_Lines := Number_of_Lines + 1;
      end loop;
      -- Always count the line (either non-null or null) after
      -- the last CRLF as a line
      Number_of_Lines := Number_of_Lines + 1;
      return Number_of_Lines;
   end if;
end;
 

function Line (Value : in String; Position : in Positive)
               return String
-- Return the given line position value.
-- that is separated by the n-1 and the nth CRLF sequence
-- or if there is no nth CRLF sequence, then returns the line
-- delimited by the n-1 CRLF and the end of the string
 
is
   Next : Natural := 1;
   Line_Number : Natural := 0;
   Start_of_Line, End_of_Line : Natural;
begin
   End_of_Line := Next_CRLF (Value, 1);
   if End_of_Line = 0 then
      -- no CRLF sequence on the "line"
      if Position > 1 then
         -- raise an exception if requesting > 1
         raise Constraint_Error;
      else
         -- otherwise, requesting first line
         -- return original string, even if null string
         return Value;
      end if;
   else
      -- There's at least one CRLF on the "line"
      for I in 1..Position loop
         Start_of_Line := Next;
         End_of_Line := Next_CRLF (Value, Next);
         -- normally, the line is Start_of_Line .. End_of_Line-1
         -- if no more CRLFs on line, it's Start_of_Line .. 'LAST
         exit when End_of_Line = 0;
         Line_Number := Line_Number + 1;
         -- skip past the 2 chars, CRLF, to start next search
         Next := End_of_Line + 2;
      end loop;
      -- if we fall out of loop normally, End_of_Line is non-zero
      if End_of_Line > 0 then
         -- and Position had better be equal to Line_Number
         if Position = Line_Number then
            return Value (Start_of_Line .. End_of_Line-1);
         else
            raise Constraint_Error;
         end if;
      else
         -- we exit the loop prematurely because there's not
         -- enough CRLFs in the line,
         -- thus Line_Number is one less than Position
         if Position = Line_Number+1 then
            return Value (Start_of_Line .. Value'LAST);
         else
            raise Constraint_Error;
         end if;
      end if;
 end if;
end Line;
 

function Line_Count_of_Value (Key : String) return Natural is
begin
   if Key_Exists (Key) then
      return Line_Count (Value(Key));
   else
      return 0;
   end if;
end Line_Count_of_Value;


function Value_of_Line (Key : String; Position : Positive) return String is
begin
   if Key_Exists (Key) then
      return Line (Value(Key), Position);
   else
      return "";
   end if;
end Value_of_Line;



-- Initialization routines, including some private procedures only
-- used during initialization.

procedure Set_CGI_Position(Key_Number : in Positive;
                           Datum : in Unbounded_String) is
  Last : Natural := Field_End(Datum, '=');
-- Given a Key number and a datum of the form key=value
-- assign the CGI_Data(Key_Number) the values of key and value.
begin
  CGI_Data.all(Key_Number).Key   := To_Unbounded_String(Slice(Datum, 1, Last));
  CGI_Data.all(Key_Number).Value := To_Unbounded_String(Slice(Datum,
                                                      Last+2, Length(Datum)));
  -- Don't need to translate '+' to ' ', that was done earlier.
  URL_Decode(CGI_Data.all(Key_Number).Key, False);
  URL_Decode(CGI_Data.all(Key_Number).Value, False);
end Set_CGI_Position;


procedure Set_CGI_Data(Raw_Data : in Unbounded_String) is
-- Set CGI_Data using Raw_Data.
  Key_Number : Positive := 1;
  Character_Position : Positive := 1;
  Last : Natural;
begin
 while Character_Position <= Length(Raw_Data) loop
   Last := Field_End(Raw_Data, '&', Character_Position);
   Set_CGI_Position(Key_Number, To_Unbounded_String(
                       Slice(Raw_Data, Character_Position, Last)));
   Character_Position := Last + 2; -- Skip over field separator.
   Key_Number := Key_Number + 1;
 end loop;
end Set_CGI_Data;


procedure Set_Cookie_Position(Key_Number : in Positive;
                              Datum : in Unbounded_String) is
  Last : Natural := Field_End(Datum, '=');
  -- Parse through the cookie raw data and put in the cookie data array
  -- Given a Key number and a datum of the form key=value
  -- assign the Cookie_Data(Key_Number) the values of key and value.
begin
    Cookie_Data.all(Key_Number).Key :=
       To_Unbounded_String(Slice(Datum, 1, Last));
    Cookie_Data.all(Key_Number).Value :=
       To_Unbounded_String(Slice(Datum, Last+2, Length(Datum)));
    -- Version 1.4 automatically URL_decoded cookies.  However, the cookie
    -- spec does not require this, so for accuracy we won't do that and
    -- instead will expose the URL_Decode subprogram to do it.
    -- URL_Decode(Cookie_Data.all(Key_Number).Key, False);
    -- URL_Decode(Cookie_Data.all(Key_Number).Value, False);
end Set_Cookie_Position;


procedure Set_Cookie_Data(Raw_Data : in Unbounded_String) is
  Key_Number : Positive := 1;
  Character_Position : Positive := 1;
  Last : Natural;
  -- Parse through the cookie raw data and put in the cookie data array
begin
  while Character_Position <= Length(Raw_Data) loop
    Last := Field_End(Raw_Data, ';', Character_Position);
    Set_Cookie_Position(Key_Number,
      To_Unbounded_String(Slice(Raw_Data, Character_Position, Last)));
    Character_Position := Last + 2; -- Skip over field separator.
    Key_Number := Key_Number + 1;
  end loop;
end Set_Cookie_Data;


function Cookie_Value(Key : in Unbounded_String; Index : in Positive := 1;
                      Required : in Boolean := False)
  return Unbounded_String is
  My_Index : Positive := 1;
  -- Read the cookie from the browser request,
  -- returns the data or a null pointer if no cookie data
begin
  if Cookie_Data /= null then
    for I in 1 .. Cookie_Data'Last loop
      if Cookie_Data.all(I).Key = Key then
        if Index = My_Index then
          return Cookie_Data.all(I).Value;
        else
          My_Index := My_Index + 1;
        end if;
      end if;
    end loop;
  end if;
  -- Didn't find the Key.
  if Required then
    raise Constraint_Error;
  else
    return To_Unbounded_String("");
  end if;
end Cookie_Value;



function Cookie_Value(Key : in String; Index : in Positive := 1;
                      Required : in Boolean := False) return String is
begin
  return To_String(Cookie_Value(To_Unbounded_String(Key), Index, Required));
end Cookie_Value;


function Cookie_Value(Key : in String; Index : in Positive := 1;
                      Required : in Boolean := False)
  return Unbounded_String is
begin
  return Cookie_Value(To_Unbounded_String(Key), Index, Required);
end Cookie_Value;


function Cookie_Value(Key : in Unbounded_String; Index : in Positive := 1;
                      Required : in Boolean := False) return String is
begin
  return To_String(Value(Key, Index, Required));
end Cookie_Value;

function Cookie_Value(Position : in Positive) return Unbounded_String is
begin
  return Cookie_Data.all(Position).Value;
end Cookie_Value;


function Cookie_Value(Position : in Positive) return String is
begin
  return To_String(Value(Position));
end Cookie_Value;

function Cookie_Count return Natural is
begin
  if Cookie_Data = null then
     return 0;
  else
     return Cookie_Data'Last;
  end if;
end Cookie_Count;

procedure Read_Cookie is
  --Temp_Ptr : Access_Key_Value_Sequence;
  Raw_Data: Unbounded_String;
  Number_of_Cookies : Natural;
begin -- read_cookie
  Raw_Data := To_Unbounded_String(Get_Environment("HTTP_COOKIE"));
  if Raw_Data /= "" then
    if Element(Raw_Data, Length(Raw_Data)) = ';' then
       -- If there's an invalid extra trailing ";", delete it.
       Delete(Raw_Data, Length(Raw_Data), Length(Raw_Data));
    end if;
    Number_of_Cookies :=Ada.Strings.Unbounded.Count(Raw_Data, Semicolon)+1;
    Cookie_Data := new Key_Value_Sequence(1 .. Number_of_Cookies);
    Set_Cookie_Data(Raw_Data);
  else
    Cookie_Data:= null;
  end if;
exception when end_error =>
  put_line( standard_error, "cgi: error reading cookie past end of file" );
end Read_Cookie;


procedure Set_Cookie(Key   : String;
                     Value : String; 
                     Expires : String := ""; 
                     Path  : String := Get_Environment("PATH_INFO"); 
                     Domain: String := Get_Environment("SERVER_NAME");
                     Secure: Boolean := False ) is
  -- Sends a cookie to the browser.
  -- Do this before sending the header for the HTML.
begin
  Put("Set-Cookie: ");
  Put(Key & "=" & Value & ";");
  if Expires /= "" then
    Put("expires=" & Expires & ";");
  end if;
  if Path /= "" then
    Put("path=" & Path  & ";");
  end if;
  if Domain /= "" then
    Put("domain=" & Domain & ";");
  end if;
  if Secure then 
    put_line("secure");
  else
    new_line;
  end if;
end Set_Cookie;


procedure Initialize is
  Raw_Data : Unbounded_String;  -- Initially an empty string (LRM A.4.5(73))
  Request_Method_Text : String := To_Upper(Get_Environment("REQUEST_METHOD"));
  -- Initialize this package, most importantly the CGI_Data variable.
begin
 if Request_Method_Text = "GET" then
    Actual_CGI_Method := Get;
    Raw_Data := To_Unbounded_String(Get_Environment("QUERY_STRING"));
 elsif Request_Method_Text = "POST" then
    Actual_CGI_Method := Post;
    declare
      content_length : integer := Integer'Value(Get_Environment("CONTENT_LENGTH"));
      Raw_Data_String : String(1 .. content_length );
      content_count : integer := 0;
ch : character;
    begin
      -- this is slow but lets me check for errors
      -- Get(Raw_Data_String);
      for i in 1..Integer'Value(Get_Environment("CONTENT_LENGTH")) loop
          Get( ch );
          Raw_Data_String(i) := ch;
          content_count := i;
      end loop;
      Raw_Data := To_Unbounded_String(Raw_Data_String);
    exception when end_error =>
      put_line( standard_error, "adacgi: END_ERROR reading POST data on standard input, expected" & content_length'img & " bytes but read" & content_count'img );
    end;
 else
    Actual_CGI_Method := Unknown;
 end if;

 Translate(Raw_Data, Mapping => Plus_To_Space); -- Convert "+"s to spaces.

 if Length(Raw_Data) > 0 then
   if Index(Raw_Data, Equals) = 0 then
     -- No "=" found, so this is an "Isindex" request.
     Is_Index_Request_Made := True;
     Raw_Data := "isindex=" & Raw_Data;
   end if;
   CGI_Data := new Key_Value_Sequence(1 .. 
                   Ada.Strings.Unbounded.Count(Raw_Data, Ampersands)+1);
   Set_CGI_Data(Raw_Data); 
   Parsing_Errors_Occurred := False;
 end if;

end Initialize;

-- This library automatically parses CGI and cookie input on program start.
-- This is a trade-off, limiting flexibility very slightly
-- (in the weird case where you don't want this auto-initialization)
-- but it eliminates a common error (forgetting to initialize things).
-- If you really don't want auto-initialization, just remove the calls
-- here and make the calls visible in the spec.  Don't forget to call them!

begin
  Initialize;
  Read_Cookie;
end CGI;

