-- $Id: wc-streams-audio.ads,v 1.2 2005/02/11 02:59:38 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Interfaces, Ada.Streams, Ada.Streams.Stream_IO, WC.Streams.Endian;
use Interfaces, Ada.Streams, Ada.Streams.Stream_IO, WC.Streams.Endian;

package WC.Streams.Audio is

   type Audio_Signed is new Integer_8;
   type Audio_Byte is new Unsigned_8;

   subtype Audio_I8 is Audio_Signed;
   subtype Audio_U8 is Audio_Byte;

   --------------------------------------------------
   -- Use these basic data types if you want automatic
   -- endian conversions to occur when reading and
   -- writing to the stream.
   --------------------------------------------------
   subtype Audio_I64 is Endian_I64;
   subtype Audio_I32 is Endian_I32;
   subtype Audio_I24 is Endian_I24;
   subtype Audio_I16 is Endian_I16;
   subtype Audio_U64 is Endian_U64;
   subtype Audio_U32 is Endian_U32;
   subtype Audio_U24 is Endian_U24;
   subtype Audio_U16 is Endian_U16;

   type Audio_Sample_Type is (
      CODEC_Unsupported,            -- Unsupported sample format
      CODEC_Linear_U8,              -- Linear unsigned 8-bit samples
      CODEC_Linear_8,               -- Linear SIGNED 8-bit samples
      CODEC_Linear_16,              -- Linear signed 16-bit samples
      CODEC_Linear_24,              -- Snd_Format_Linear_24
      CODEC_Linear_32,              -- Snd_Format_Linear_32
      CODEC_Mulaw,                  -- Snd_Format_Mulaw
      CODEC_Alaw_8,                 -- Snd_Format_Alaw_8
      CODEC_MS_ADPCM,               -- Microsoft Wave_Format_ADPCM
      CODEC_IMA_ADPCM               -- IMA/DVI ADPCM algorithm
   );

   --------------------------------------------------
   -- The Abstract Audio_Stream_Type :
   --------------------------------------------------
   type Audio_Stream_Type is abstract new Endian_Stream_Type with private;
   type Audio_Stream is access all Audio_Stream_Type'Class;

   --------------------------------------------------
   -- Other Supporting Audio Types :
   --------------------------------------------------
   type Audio_Channels is new Audio_I16 range 1..8;
   type Audio_Sample_Rate is new Audio_I32;
   type Audio_Sample_Size is new Audio_I16 range 1..16;

   type Byte_Offset is new Audio_I32;
   type Byte_Offset_Ptr is access all Byte_Offset;

   --------------------------------------------------
   -- Information Retrieval Functions :
   --------------------------------------------------
   function Channels(Strm : Audio_Stream_Type) return Audio_Channels is abstract;
   function Sample_Rate(Strm : Audio_Stream_Type) return Audio_Sample_Rate is abstract;
   function Sample_Size(Strm : Audio_Stream_Type) return Audio_Sample_Size is abstract;
   function Offset(Strm : Audio_Stream_Type) return Byte_Offset is abstract;
   function Sample_Type(Strm : Audio_Stream_Type) return Audio_Sample_Type is abstract;
   function In_Items(Strm : Audio_Stream_type; Byte_Size : Positive) return Positive is abstract;

   --------------------------------------------------
   -- Configuration Settings (or Set Offset) :
   --------------------------------------------------
   procedure Set_Channels(Strm : in out Audio_Stream_Type; Channels : Audio_Channels) is abstract;
   procedure Set_Sample_Rate(Strm : in out Audio_Stream_Type; Sample_Rate : Audio_Sample_Rate) is abstract;
   procedure Set_Sample_Size(Strm : in out Audio_Stream_Type; Sample_Size : Audio_Sample_Size) is abstract;
   procedure Set_Offset(Strm : in out Audio_Stream_Type; New_Offset : Byte_Offset) is abstract;

private

   type Audio_Stream_Type is abstract new Endian_Stream_Type with null record;

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-audio.ads,v 1.2 2005/02/11 02:59:38 ken Exp $";

end WC.Streams.Audio;
