-- $Id: wc-streams-au.ads,v 1.2 2005/02/11 02:59:37 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Interfaces; use Interfaces;
with Ada.Finalization; use Ada.Finalization;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with WC.Streams.Endian;
with WC.Streams.Audio;

use WC.Streams.Endian;
use WC.Streams.Audio;

package WC.Streams.Au is

   --------------------------------------------------
   -- Exceptions :
   --------------------------------------------------
   Bad_State :          exception;
   Bad_Data_Loc :       exception;     -- File has bad Data_Loc field value
   Not_Au_Format :      exception;     -- File is not .au (.snd) format (according to magic)
   Bad_Au_Format :      exception;     -- File has bad .au format
   Unsupported_Format : exception;     -- Unsupported Sound Format
   Not_Supported  :     exception;     -- Function is not supported

   --------------------------------------------------
   -- Stream Data Types :
   --------------------------------------------------
   type Au_Stream_Type is limited private;
   type Au_Stream_Block is new Audio_Stream_Type with private;
   type Au_Stream_Access is access all Au_Stream_Block'Class;

   --------------------------------------------------
   -- Other Au_Stream Types :
   --------------------------------------------------
   type Au_Info is new String(1..4);   -- Optional additional info
   type Au_Mode is (In_Mode, Out_Mode);        

   --------------------------------------------------
   -- Stream Functions :
   --------------------------------------------------
   procedure Open(Strm : in out Au_Stream_Type; Mode : Au_Mode; Name : String);
   procedure Close(Strm : in out Au_Stream_Type);
   function Stream(Au_Strm : Au_Stream_Type) return Audio_Stream;

   --------------------------------------------------
   -- Query Functions :
   --------------------------------------------------
   function Channels(Strm : Au_Stream_Block) return Audio_Channels;
   function Sample_Rate(Strm : Au_Stream_Block) return Audio_Sample_Rate;
   function Sample_Size(Strm : Au_Stream_Block) return Audio_Sample_Size;
   function Sample_Type(Strm : Au_Stream_Block) return Audio_Sample_Type;
   function Offset(Strm : Au_Stream_Block) return Byte_Offset;
   function Info(Strm : Au_Stream_Block) return Au_Info;
   function In_Items(Strm : Au_Stream_Block; Byte_Size : Positive) return Positive;

   --------------------------------------------------
   -- Configuration :
   --------------------------------------------------
   procedure Set_Channels(Strm : in out Au_Stream_Block; Channels : Audio_Channels);
   procedure Set_Sample_Rate(Strm : in out Au_Stream_Block; Sample_Rate : Audio_Sample_Rate);
   procedure Set_Sample_Size(Strm : in out Au_Stream_Block; Sample_Size : Audio_Sample_Size);

   --------------------------------------------------
   -- Action :
   --------------------------------------------------
   procedure Set_Offset(Strm : in out Au_Stream_Block; New_Offset : Byte_Offset);

   --------------------------------------------------
   -- Au_Format (File) Values :
   --------------------------------------------------
   type Snd_Format_Type is (
      Snd_Format_Unspecified,             -- 0
      Snd_Format_Mulaw,                   -- 1
      Snd_Format_Linear_8,                -- 2
      Snd_Format_Linear_16,               -- 3
      Snd_Format_Linear_24,               -- 4
      Snd_Format_Linear_32,               -- 5
      Snd_Format_Float,                   -- 6
      Snd_Format_Double,                  -- 7
      Snd_Format_Indirect,                -- 8
      Snd_Format_Nested,                  -- 9
      Snd_Format_Dsp_Core,                -- 10
      Snd_Format_Dsp_Data_8,              -- 11
      Snd_Format_Dsp_Data_16,             -- 12
      Snd_Format_Dsp_Data_24,             -- 13
      Snd_Format_Dsp_Data_32,             -- 14
      Snd_Format_Display,                 -- 16
      Snd_Format_Mulaw_Squelch,           -- 17
      Snd_Format_Emphasized,              -- 18
      Snd_Format_Compressed,              -- 19
      Snd_Format_Compressed_Emphasized,   -- 20
      Snd_Format_Dsp_Commands,            -- 21
      Snd_Format_Dsp_Commands_Samples,    -- 22
      Snd_Format_Adpcm_G721,              -- 23
      Snd_Format_Adpcm_G722,              -- 24
      Snd_Format_Adpcm_G723_3,            -- 25
      Snd_Format_Adpcm_G723_5,            -- 26
      Snd_Format_Alaw_8                   -- 27
   );
   for Snd_Format_Type'Size use 32;       -- File format stores this value in 32 bits
   for Snd_Format_Type use (              -- The following are File format specific values for Sound Format :
      0,                                  -- Snd_Format_Unspecified
      1,                                  -- Snd_Format_Mulaw
      2,                                  -- Snd_Format_Linear_8
      3,                                  -- Snd_Format_Linear_16
      4,                                  -- Snd_Format_Linear_24
      5,                                  -- Snd_Format_Linear_32
      6,                                  -- Snd_Format_Float
      7,                                  -- Snd_Format_Double
      8,                                  -- Snd_Format_Indirect
      9,                                  -- Snd_Format_Nested
      10,                                 -- Snd_Format_Dsp_Core
      11,                                 -- Snd_Format_Dsp_Data_8
      12,                                 -- Snd_Format_Dsp_Data_16
      13,                                 -- Snd_Format_Dsp_Data_24
      14,                                 -- Snd_Format_Dsp_Data_32
      16,                                 -- Snd_Format_Display
      17,                                 -- Snd_Format_Mulaw_Squelch
      18,                                 -- Snd_Format_Emphasized
      19,                                 -- Snd_Format_Compressed
      20,                                 -- Snd_Format_Compressed_Emphasized
      21,                                 -- Snd_Format_Dsp_Commands
      22,                                 -- Snd_Format_Dsp_Commands_Samples
      23,                                 -- Snd_Format_Adpcm_G721
      24,                                 -- Snd_Format_Adpcm_G722
      25,                                 -- Snd_Format_Adpcm_G723_3
      26,                                 -- Snd_Format_Adpcm_G723_5
      27                                  -- Snd_Format_Alaw_8
   );

   function Last_Au_Format return Snd_Format_Type;

   Au_Magic_Value : constant := 16#2E736E64#; -- .au files start with this magic 32 bit value

private

   type Au_Stream_Ptr is access all Au_Stream_Block;

   type String4 is new String(1..4);      -- For endian conversions here

   --------------------------------------------------
   -- These procedures are for Automatic Endian Conversion:
   --------------------------------------------------
   procedure Read_Snd_Format_Type(Stream : access Root_Stream_Type'Class; Item : out Snd_Format_Type);
   for Snd_Format_Type'Read use Read_Snd_Format_Type;

   --------------------------------------------------
   -- Au_Stream_Type Definition :
   --------------------------------------------------
   type Au_Stream_Type is access all Au_Stream_Block;
   subtype File_Stream is Ada.Streams.Stream_IO.Stream_Access;

   --------------------------------------------------
   -- The Au_CB primarily gives us the Finalization
   -- call. The discriminant Strm is required, so that
   -- the Finalization method can call the method
   -- Ada.Streams.Streams_IO.Close(Strm.File), to
   -- close it when the stream is destroyed.
   --------------------------------------------------
   type Au_CB(Strm : Au_Stream_Ptr) is new controlled -- Strm points back to the Au_Stream_Block defined below
      with null record;
   
   procedure Initialize(CB : in out Au_CB);
   procedure Finalize(CB : in out Au_CB);

   --------------------------------------------------
   -- The bulk of the Au_Stream is contained in this
   -- Root_Stream_Access derived record :
   --------------------------------------------------
   type Au_Stream_Block is new Audio_Stream_Type with
   record
      Au_Str :        Au_Stream_Type;           -- For convenience-- stream pointer for this block (set by Open())
      File :          File_Type;                -- Underlying Ada.Streams.Stream_IO.File_Type
      Str :           File_Stream;              -- Stream(File)
      --
      Format :        Snd_Format_Type     := Snd_Format_Unspecified;
      Channels :      Audio_Channels      := 1; -- # of Audio channels
      Sample_Rate :   Audio_Sample_Rate   := 1; -- Sample Rate
      Sample_Size :   Audio_Sample_Size   := 1; -- Sample size in bits
      Data_Loc :      Byte_Offset         := 0; -- 1-based offset to the wave samples
      Data_Size :     Byte_Offset         := 0; -- # of bytes of samples
      Info :          Au_Info             := "    "; -- Optional additional info
      --
      CB :            Au_CB(Au_Stream_Block'Unchecked_Access); -- Primarily for Finalization use only
   end record;

   procedure Read(Strm : in out Au_Stream_Block; Item : out Stream_Element_Array; Last : out Stream_Element_Offset);
   procedure Write(Strm : in out Au_Stream_Block; Item : Stream_Element_Array);

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-au.ads,v 1.2 2005/02/11 02:59:37 ken Exp $";

end WC.Streams.Au;
