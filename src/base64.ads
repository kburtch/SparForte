  -- Base64 encode/decode & test driver.
  -- Copyright 2001 Tom Moran (tmoran@acm.org, PGP signed tmoran@bix.com),
  -- anyone may use for any purpose.
  -- Expanded by Darren New <dnew@san.rr.com>
  --  Added encode_length, fixed encode comment, added all functions
  --  declared after encode_length.

with Ada.Streams;
with Ada.Text_Io;
with Ada.Streams.Stream_Io;

package Base64 is

  -- RFC 1521, MIME Base64 encode/decode
  -- Assumes Ada.Streams.Stream_Element is a byte.

  procedure Decode(Source  : in     String;
                   Target  :    out Ada.Streams.Stream_Element_Array;
                   Last    :    out Ada.Streams.Stream_Element_Offset);
  -- decode Source into Target(Target'first .. Last)
  -- Note: it may be appropriate to prescan Source for '=',
  -- indicating termination, or for illegitimate characters,
  -- indicating corruption, before calling Decode.

  procedure Encode(Source  : in     Ada.Streams.Stream_Element_Array;
                   Target  :    out String;
                   Last    :    out Natural);
  -- Target is filled in four character increments, except that
  -- a CR-LF pair is inserted before each 77'th character.
  -- Target'length must be at least:
  -- Output_Quad_Count: constant := (Source'length + 2) / 3;
  -- Output_Byte_Count: constant := 4 * Output_Quad_Count;
  -- Target'length = Output_Byte_Count + 2 * (Output_Byte_Count / 76)
  -- Subtract 2 for CRLF if the last line is precisely 76 characters long.
  -- Constraint_Error will be raised if Target isn't long enough.

  function Encode_Length(Length : Ada.Streams.Stream_Element_Offset)
    return Natural;
  -- This returns the Last that will be returned by the Encode
  -- procedure for a stream element array with 'Length = Length.
  -- It basically encapsulates the calculation described under Encode,
  -- except adjusted for a final line 76 characters long not having a CRLF.

  -- The following routines are useful when the data to be encoded or
  -- decoded does not conveniently fit into memory, requiring multiple
  -- calls to Encode or Decode to process it.

  Characters_Per_Line : constant := 76;
  -- As specified in the RFCs.

  Octets_Per_Line : constant := (Characters_Per_Line / 4) * 3;
  -- If you are encoding a large stream in chunks with multiple calls
  -- to Encode, use this. This is the number of octets that will fit on
  -- one line. You do not want to encode (for example) 18 octets, then
  -- try to concatenate the encoding of the next 18 octets, because you will
  -- be left with padding (or worse) in between. If you encode this many
  -- octets, you need to add your own CRLF in between, also, if you want
  -- to split them into lines. If you don't want line breaks, encode this
  -- many octets at a time and concatenate the results, to avoid the
  -- CRLF stuffing that Encode does.

  procedure Decode_Length(Source        : in     String;
                          Source_Last   :    out Natural;
                          Target_Length :    out
                                Ada.Streams.Stream_Element_Offset;
                          Complete      :    out Boolean);
  -- This procedure is useful for decoding a large base64 stream.
  -- Source iteratively takes on undecoded portions of the input stream.
  -- Source_Last returns the last offset into the Source string that
  -- you should pass to Decode. For example, if the Source is 9 characters
  -- long, you would only want to pass in 8 to be decoded, since you should
  -- pass in multiples of four characters to Decode. Also, this will not
  -- set Source_Last such that there are any '=' characters in
  -- Source(Source'First..Source_Last) that are followed by any non-'='
  -- characters. It will also stop at the first '-' character, for the
  -- convenience of MIME decoders. Obviously, Source_Last <= Source'Last.
  -- Target_Length will be set to the number of octets that will be
  -- required to hold the decoding of Source(Source'First..Source_Last).
  -- This may be less than expected if extraneous whitespace appears or
  -- an = or - sign appears. Target_Length is calculated by scanning
  -- the Source characters. Target_Length might be zero.
  -- Complete is set True if Source_Last points to the last character
  -- of the decoding. That is, if Source(Source_Last+1) exists and is
  -- an '=' or '-' sign, then Complete is True. Otherwise, it is False,
  -- indicating that there might be more decoding needed.
  --
  -- Generally, to use this, assuming Source starts at 1:
  -- Read a chunk of base64 character data,
  -- pass it to Decode_Length,
  -- allocate a stream element array of Target_Length size,
  -- decode Source(1..Source_Last) into it,
  -- and save the decoded octets.
  -- Then Remainder := Source'Last - Source_Last;
  --      Source(1..Remainder) := Source(Source_Last..Source'Last);
  --      Read more in, starting at Source(Remainder+1);
  -- Then iterate if Complete is false.

  procedure Encode_Stream(From  : in out Ada.Streams.Stream_Io.File_Type;
                          To    : in out Ada.Text_Io.File_Type);
  -- This takes a Stream_Io file open for reading and a Text_Io open for
  -- writing, and encodes what's on the stream file into the text file.
  -- The text is encoded a line at a time, so whatever line terminator
  -- the Text_Io package uses is output.

  procedure Decode_Stream(From  : in out Ada.Text_Io.File_Type;
                          To    : in out Ada.Streams.Stream_Io.File_Type);
  -- This takes a Text_Io file open for reading and a Stream open for
  -- writing, and decodes what's on the text file into the stream.
  -- This uses Look_Ahead to avoid reading any text on lines
  -- that start with a hyphen. This allows for convenient MIME decoding.
  -- Any line not starting with a hyphen will be read in full.
  -- Any line with 1400 characters or more with raise a Constraint_Error.
  -- Non-terminal lines that don't have a multiple of four valid
  -- characters will likely decode improperly, possibly without detection.
  -- A more robust implementation would read From one character at a time
  -- and use Decode_Length.

end Base64;
