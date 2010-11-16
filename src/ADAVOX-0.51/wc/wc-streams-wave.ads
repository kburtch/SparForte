-- $Id: wc-streams-wave.ads,v 1.2 2005/02/11 02:59:39 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Tags; use Ada.Tags;

with Interfaces; use Interfaces;
with Ada.Finalization; use Ada.Finalization;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with WC.Streams.Endian;
with WC.Streams.Audio;
with WC.Streams.Buffer;
with WC.Tree_Nodes;

use WC.Streams.Endian;
use WC.Streams.Audio;
use WC.Streams.Buffer;

package WC.Streams.Wave is

   --------------------------------------------------
   -- WAVE Exceptions :
   --------------------------------------------------
   Unsupported_Format:  exception;  -- Unsupported Sound Format
   Not_Riff_Format:     exception;  -- Input file is not in RIFF format     
   Not_Riff_Wave:       exception;  -- This unit is not a RIFF(WAVE) unit   
   Bad_Wave_File:       exception;  -- The WAVE file has format/size problems  
   End_File:            exception;  -- Reached the end of the RIFF file     
   Bug_Alert:           exception;  -- A "fix me" condition      
   Io_Error:            exception;  -- A I/O error occurred      
   Seek_Error:          exception;  -- A lseek(2) operation failed      
   Bad_State:           exception;  -- Wave_CB in the incorrect state      
   Bad_Argument:        exception;  -- Inappropriate argument      

   --------------------------------------------------
   -- Stream Data Types :
   --------------------------------------------------
   type Wave_Stream_Type is limited private;
   type Wave_Stream_Block is new Audio_Stream_type with private;
   type Wave_Stream_Access is access all Wave_Stream_Block'Class;

   type Wave_Mode is (In_Mode,Out_Mode);

   --------------------------------------------------
   -- Stream Functions :
   --------------------------------------------------
   procedure Open(Strm: in out Wave_Stream_Type; Mode: Wave_Mode; Name: String);
   procedure Close(Strm: in out Wave_Stream_Type);
   function Stream(Wave_Strm: Wave_Stream_Type) return Audio_Stream;

   function Channels(Strm: Wave_Stream_Block) return Audio_Channels;
   function Sample_Rate(Strm: Wave_Stream_Block) return Audio_Sample_Rate;
   function Sample_Size(Strm: Wave_Stream_Block) return Audio_Sample_Size;
   function Sample_Type(Strm: Wave_Stream_Block) return Audio_Sample_Type;
   function In_Items(Strm: Wave_Stream_Block; Byte_Size: Positive) return Positive;

   procedure Set_Channels(Strm: in out Wave_Stream_Block; Channels: Audio_Channels);
   procedure Set_Sample_Rate(Strm: in out Wave_Stream_Block; Sample_Rate: Audio_Sample_Rate);
   procedure Set_Sample_Size(Strm: in out Wave_Stream_Block; Sample_Size: Audio_Sample_Size);

   function Offset(Strm: Wave_Stream_Block) return Byte_Offset;
   procedure Set_Offset(Strm: in out Wave_Stream_Block; New_Offset: Byte_Offset);

   --------------------------------------------------
   -- RIFF WAVE Format Types :
   --------------------------------------------------
   type Wave_Format_Type is (
      Wave_Format_Unknown,                -- Microsoft Corporation                16#0000#
      Wave_Format_Pcm,                    -- Microsoft Corporation                16#0001#
      Wave_Format_ADPCM,                  -- Microsoft Corporation                16#0002#
      Wave_Format_Ibm_Cvsd,               -- IBM Corporation                      16#0005#
      Wave_Format_Alaw,                   -- Microsoft Corporation                16#0006#
      Wave_Format_Mulaw,                  -- Microsoft Corporation                16#0007#
      Wave_Format_Oki_Adpcm,              -- OKI                                  16#0010#
      Wave_Format_Ima_Adpcm,              -- Intel Corporation (AKA DVI_ADPCM)    16#0011#
      Wave_Format_Mediaspace_Adpcm,       -- Videologic                           16#0012#
      Wave_Format_Sierra_Adpcm,           -- Sierra Semiconductor Corp            16#0013#
      Wave_Format_G723_Adpcm,             -- Antex Electronics Corporation        16#0014#
      Wave_Format_Digistd,                -- DSP Solutions, Inc.                  16#0015#
      Wave_Format_Digifix,                -- DSP Solutions, Inc.                  16#0016#
      Wave_Format_Yamaha_Adpcm,           -- Yamaha Corporation of America        16#0020#
      Wave_Format_Sonarc,                 -- Speech Compression                   16#0021#
      Wave_Format_Dspgroup_Truespeech,    -- DSP Group, Inc                       16#0022#
      Wave_Format_Echosc1,                -- Echo Speech Corporation              16#0023#
      Wave_Format_Audiofile_Af36,         -- Audiofile, Inc.                      16#0024#
      Wave_Format_Aptx,                   -- Audio Processing Technology          16#0025#
      Wave_Format_Audiofile_Af10,         -- Audiofile, Inc.                      16#0026#
      Wave_Format_Dolby_Ac2,              -- Dolby Laboratories                   16#0030#
      Wave_Format_Gsm610,                 -- Microsoft Corporation                16#0031#
      Wave_Format_Antex_Adpcme,           -- Antex Electronics Corporation        16#0033#
      Wave_Format_Control_Res_Vqlpc,      -- Control Resources Limited            16#0034#
      Wave_Format_Digireal,               -- DSP Solutions, Inc.                  16#0035#
      Wave_Format_Digiadpcm,              -- DSP Solutions, Inc.                  16#0036#
      Wave_Format_Control_Res_Cr10,       -- Control Resources Limited            16#0037#
      Wave_Format_Nms_Vbxadpcm,           -- Natural MicroSystems                 16#0038#
      Wave_Format_G721_Adpcm,             -- Antex Electronics Corporation        16#0040#
      Wave_Format_Mpeg,                   -- Microsoft Corporation                16#0050#
      Wave_Format_Creative_Adpcm,         -- Creative Labs, Inc                   16#0200#
      Wave_Format_Fm_Towns_Snd,           -- Fujitsu Corp.                        16#0300#
      Wave_Format_Oligsm,                 -- Ing C. Olivetti & C., S.p.A.         16#1000#
      Wave_Format_Oliadpcm,               -- Ing C. Olivetti & C., S.p.A.         16#1001#
      Wave_Format_Olicelp,                -- Ing C. Olivetti & C., S.p.A.         16#1002#
      Wave_Format_Olisbc,                 -- Ing C. Olivetti & C., S.p.A.         16#1003#
      Wave_Format_Oliopr                  -- Ing C. Olivetti & C., S.p.A.         16#1004#
   ); 
   for Wave_Format_Type'Size use 16;       -- File format stores this value in 32 bits
   for Wave_Format_Type use (              -- The following are File format specific values for Sound Format :
      16#0000#, -- Wave_Format_Unknown,                Microsoft Corporation
      16#0001#, -- Wave_Format_PCM,                    Microsoft Corporation
      16#0002#, -- Wave_Format_ADPCM,                  Microsoft Corporation
      16#0005#, -- Wave_Format_IBM_Cvsd,               IBM Corporation
      16#0006#, -- Wave_Format_Alaw,                   Microsoft Corporation
      16#0007#, -- Wave_Format_Mulaw,                  Microsoft Corporation
      16#0010#, -- Wave_Format_OKI_ADPCM,              OKI
      16#0011#, -- Wave_Format_IMA_ADPCM,              Intel Corporation (AKA DVI_ADPCM)
      16#0012#, -- Wave_Format_Mediaspace_Adpcm,       Videologic
      16#0013#, -- Wave_Format_Sierra_Adpcm,           Sierra Semiconductor Corp
      16#0014#, -- Wave_Format_G723_Adpcm,             Antex Electronics Corporation
      16#0015#, -- Wave_Format_Digistd,                DSP Solutions, Inc.
      16#0016#, -- Wave_Format_Digifix,                DSP Solutions, Inc.
      16#0020#, -- Wave_Format_Yamaha_Adpcm,           Yamaha Corporation of America
      16#0021#, -- Wave_Format_Sonarc,                 Speech Compression
      16#0022#, -- Wave_Format_Dspgroup_Truespeech,    DSP Group, Inc
      16#0023#, -- Wave_Format_Echosc1,                Echo Speech Corporation
      16#0024#, -- Wave_Format_Audiofile_Af36,         Audiofile, Inc.
      16#0025#, -- Wave_Format_Aptx,                   Audio Processing Technology
      16#0026#, -- Wave_Format_Audiofile_Af10,         Audiofile, Inc.
      16#0030#, -- Wave_Format_Dolby_Ac2,              Dolby Laboratories
      16#0031#, -- Wave_Format_Gsm610,                 Microsoft Corporation
      16#0033#, -- Wave_Format_Antex_Adpcme,           Antex Electronics Corporation
      16#0034#, -- Wave_Format_Control_Res_Vqlpc,      Control Resources Limited
      16#0035#, -- Wave_Format_Digireal,               DSP Solutions, Inc.
      16#0036#, -- Wave_Format_Digiadpcm,              DSP Solutions, Inc.
      16#0037#, -- Wave_Format_Control_Res_Cr10,       Control Resources Limited
      16#0038#, -- Wave_Format_Nms_Vbxadpcm,           Natural MicroSystems
      16#0040#, -- Wave_Format_G721_Adpcm,             Antex Electronics Corporation
      16#0050#, -- Wave_Format_Mpeg,                   Microsoft Corporation
      16#0200#, -- Wave_Format_Creative_Adpcm,         Creative Labs, Inc
      16#0300#, -- Wave_Format_Fm_Towns_Snd,           Fujitsu Corp.
      16#1000#, -- Wave_Format_Oligsm,                 Ing C. Olivetti & C., S.p.A.
      16#1001#, -- Wave_Format_Oliadpcm,               Ing C. Olivetti & C., S.p.A.
      16#1002#, -- Wave_Format_Olicelp,                Ing C. Olivetti & C., S.p.A.
      16#1003#, -- Wave_Format_Olisbc,                 Ing C. Olivetti & C., S.p.A.
      16#1004#  -- Wave_Format_Oliopr,                 Ing C. Olivetti & C., S.p.A.
   ); 

   --------------------------------------------------
   -- DISP Chunk Types :
   --------------------------------------------------
   type Disp_Tag_Type is (
      Unknown,  
      Text,       -- 1 Clipboard text
      Metafile,   -- 2 Meta file
      Unknown_2,  -- 4
      Dib,        -- 8 DIB
      Unknown_3   -- 16
   ); 
   for Disp_Tag_Type'Size use 32;
   for Disp_Tag_Type use (
      0,          -- Unknown
      1,          -- Metafile
      2,          -- Meta file
      4,          -- Unknown_2
      8,          -- DIB
      16          -- Unknown_3
   );

private

   procedure Read_Wave_Format_Type(Stream: access Root_Stream_Type'Class; Item: out Wave_Format_Type);
   procedure Read_Disp_Tag_Type(Stream: access Root_Stream_Type'Class; Item: out Disp_Tag_Type);

   for Wave_Format_Type'Read use Read_Wave_Format_Type;
   for Disp_Tag_Type'Read use Read_Disp_Tag_Type;

   --------------------------------------------------
   -- Internal Package Exceptions :
   --------------------------------------------------
   End_Of_Context:         exception;                  -- Internal exception

   subtype Dyn_String is Ada.Strings.Unbounded.Unbounded_String;
   function Dyn(S: String) return Dyn_String renames Ada.Strings.Unbounded.To_Unbounded_String;
   type String_Ptr is access all String; 

   --------------------------------------------------
   -- RIFF 'word' type :
   --------------------------------------------------
   type Cksize is new Audio_U32;                       -- Chunk Size
   type Fourcc is new String(1..4);                    -- Chunk ID
   Max_Cksize : constant Cksize := Cksize'Last;        -- Maximum Chunk Size
   Root_Context : constant Fourcc := "    ";           -- Special Indication of Root RIFF Context

   --------------------------------------------------
   -- Wave_Stream_Type Definition :
   --------------------------------------------------
   type Wave_Stream_Type is access all Wave_Stream_Block;
   subtype File_Stream is Ada.Streams.Stream_IO.Stream_Access;
   type Wave_Stream_Ptr is access all Wave_Stream_Block;

   --------------------------------------------------
   -- Each Context in a RIFF file, is a starting
   -- file offset, and it's ending offset.
   --------------------------------------------------
   type File_Context_Type is
      record
         Start_Offset:       Byte_Offset := 0;         -- Starting zero based file offset
         End_Offset:         Byte_Offset := 0;         -- Ending zero based file offset
      end record;

   type File_Context_Array is array(Natural range <>) of File_Context_Type;

   --------------------------------------------------
   -- Every chunk in the RIFF file had a header using
   -- this common set of elements :
   --------------------------------------------------
   type Chunk_Hdr is tagged
      record
         Chunk_ID:       Fourcc;                      -- Chunk ID
         Chunk_Size:     Cksize;                      -- Chunk size;
      end record;
   
   type Chunk_Hdr_Ptr is access all Chunk_Hdr;
   type Chunk_Hdr_Access is access all Chunk_Hdr'Class;
    
   function Chunk_Hdr_Class_Input(Stream: access Ada.Streams.Root_Stream_Type'Class) return Chunk_Hdr'Class;
   for Chunk_Hdr'Class'Input use Chunk_Hdr_Class_Input;

   --------------------------------------------------
   -- Riff Form Chunk (Start of Wave File)
   --------------------------------------------------
   type Riff_Form is new Chunk_Hdr with
      record
         Form_Type:      Fourcc;                      -- E.G. "WAVE"
      end record;

   type Riff_Form_Ptr is access all Riff_Form;

   --------------------------------------------------
   -- Coefficient Set (for Wave_Format_ADPCM)
   --------------------------------------------------
   type Coef_Set is 
      record 
         Coef1:                Audio_I16;          -- ADPCM coefficient 1
         Coef2:                Audio_I16;          -- ADPCM coefficient 2
      end record; 

   type Coef_Set_Array is array (Audio_U16 range <>) of Coef_Set;

   --------------------------------------------------
   -- WAVE Format Chunk :
   --------------------------------------------------
   type Format_Chunk(Format_Tag: Wave_Format_Type; Num_Coef: Audio_U16) is new Chunk_Hdr with
      record
         Channels:           Audio_U16;                  -- # of Audio channels
         Sample_Rate:        Audio_U32;                  -- Samples / second
         Avg_Byte_Rate:      Audio_U32;                  -- Average bytes / second
         Block_Align:        Audio_U16;                  -- Data block size
         case Format_Tag is
            when Wave_Format_PCM =>
               Bits_Per_PCM:           Audio_U16;        -- Bits / Sample
            when Wave_Format_ADPCM =>
               Bits_Per_ADPCM:         Audio_U16;        -- Often provided as zero; ADPCM only uses 4 bits
               Samples_Per_Blk:        Audio_U16;        -- # of samples per block
               CB_Size:                Audio_U16;        -- Size in bytes of the extra info in the extended WAVE fmt chunk
               Coef:                   Coef_Set_Array(1..Num_Coef);
            when Wave_Format_ALaw | Wave_Format_MULaw =>
               ULaw_Bits_Per_Sample:   Audio_U16;        -- This is 8 for all companded formats
               ULaw_CB_Size:           Audio_U16;        -- Size in bytes of the extra info (should be zero)
            when Wave_Format_IMA_ADPCM =>
               IMA_Bits_Per_Sample:    Audio_U16;        -- Bits per sample (ranges from 2-5)
               IMA_Cb_Size:            Audio_U16;        -- Size in bytes of the extra info (should be two)
               IMA_Samps_Per_Block:    Audio_U16;        -- Samples / channel / block
            when others =>
               null;
         end case;
   end record;

   type Format_Chunk_Ptr is access all Format_Chunk;

   --------------------------------------------------
   -- Wave Fact Chunk :
   --------------------------------------------------
   type Fact_Chunk is new Chunk_Hdr with
      record
         Sample_Length:                  Audio_U32;      -- Length of the file in "samples"
      end record;

   type Fact_Chunk_Ptr is access all Fact_Chunk;

   --------------------------------------------------
   -- Wave Data Chunk :
   --------------------------------------------------
   type Data_Chunk is new Chunk_Hdr with
      record
         Offset:                     Byte_Offset;    -- Offset in file where the data starts (first byte)
         End_Offset:                 Byte_Offset;    -- Where the data ends (points past end)
      end record;
   
   type Data_Chunk_Ptr is access all Data_Chunk;

   --------------------------------------------------
   -- Wave List Chunk :
   --------------------------------------------------
   type List_Chunk is new Chunk_Hdr with
      record
         List_Type:      Fourcc;
      end record;

   type List_Chunk_Ptr is access all List_Chunk;

   --------------------------------------------------
   -- Wave LIST(INFO) Chunk :
   --------------------------------------------------
   type List_Info_Chunk(N: Integer) is new Chunk_Hdr with
      record
         Info:           String(1..N);
      end record;

   type List_Info_Chunk_Ptr is access all List_Info_Chunk;

   --------------------------------------------------
   -- DISP chunk :
   --------------------------------------------------
   type Disp_Chunk(N: Integer) is new Chunk_Hdr with
      record
         Disp_Tag:       Disp_Tag_Type;  -- Enumerated type
         Info:           String(1..N);   -- Raw bytes or text
      end record;

   type Disp_Chunk_Ptr is access all Disp_Chunk;

   --------------------------------------------------
   -- Any Unsupported Chunk :
   --------------------------------------------------
   type Unsupported_Chunk(N: Integer) is new Chunk_Hdr with
      record
         Raw_Bytes:      String(1..N);           -- Raw un-interpreted data
      end record;

   type Unsupported_Chunk_Ptr is access all Unsupported_Chunk;

   procedure Free(Acc_Ptr: in out Chunk_Hdr_Access);
   type Free_Proc is access procedure(Acc_Ptr: in out Chunk_Hdr_Access);

   package Tree_Pkg is new WC.Tree_Nodes(Chunk_Hdr_Access,Free_Proc);
   use Tree_Pkg;

   --------------------------------------------------
   -- The Wave_CB primarily gives us the Finalization
   -- call. The discriminant Strm is required, so that
   -- the Finalization method can call the method
   -- Ada.Streams.Streams_IO.Close(Strm.File), to
   -- close it when the stream is destroyed.
   --------------------------------------------------
   type Wave_CB(Strm: Wave_Stream_Ptr) is new controlled with      -- Strm points back to the Wave_Stream_Block defined below
      record
         Chunk_Context:      Dyn_String;                         -- Name of the context for this chunk
         Stack_Pointer:      Natural := 0;                       -- Stack pointer
         File_Context:       File_Context_Array(0..4);           -- File context (offsets)
      end record;
   
   procedure Initialize(CB: in out Wave_CB);
   procedure Finalize(CB: in out Wave_CB);

   --------------------------------------------------
   -- The bulk of the Wave_Stream is contained in this
   -- Root_Stream_Access derived record:
   --------------------------------------------------
   type Wave_Stream_Block is new Audio_Stream_Type with
      record
         Wave_Str:       Wave_Stream_Type;                   -- For convenience-- stream pointer for this block (set by Open())
         File:           File_Type;                          -- Underlying Ada.Streams.Stream_IO.File_Type
         Str:            File_Stream;                        -- Stream(File)
         --
         Tree_Context:   Tree_Context_Type;                  -- Current tree node context
         Tree:           aliased Tree_Type(Free'Access);     -- List of file chunks
         --
         Format:         Format_Chunk_Ptr;                   -- Current Format Chunk
         Data:           Data_Chunk_Ptr;                     -- Current Data Chunk
         Fact:           Fact_Chunk_Ptr;                     -- Current Fact Chunk, if any
         --
         CB:             Wave_CB(Wave_Stream_Block'Unchecked_Access); -- Primarily for Finalization use only
         Read_Buf:       Boolean             := False;       -- By default, this is false
         Buffer:         Buf_Stream_Type;                    -- Buf_Stream type
      end record;

   procedure Read(Strm: in out Wave_Stream_Block; Item: out Stream_Element_Array; Last: out Stream_Element_Offset);
   procedure Write(Strm: in out Wave_Stream_Block; Item: Stream_Element_Array);

   function Context(Strm: Wave_Stream_Block) return Dyn_String;
   function Start_Context(Strm: Wave_Stream_Block) return Byte_Offset;
   function End_Context(Strm: Wave_Stream_Block) return Byte_Offset;
   procedure Add_Context(Strm: in out Wave_Stream_Block; New_Context: String; Starting, Ending: Byte_Offset);
   procedure Pop_Context(Strm: in out Wave_Stream_Block);

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-wave.ads,v 1.2 2005/02/11 02:59:39 ken Exp $";

end WC.Streams.Wave;
