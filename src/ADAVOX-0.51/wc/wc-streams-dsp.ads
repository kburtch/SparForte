-- $Id: wc-streams-dsp.ads,v 1.2 2005/02/11 02:59:38 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Streams, Ada.Finalization, Interfaces;
use Ada.Streams, Ada.Finalization, Interfaces;

with WC.Host;
with WC.Host.Glue;
with WC.Streams.Audio;
with WC.Streams.IO_Buffer;

use WC.Host;
use WC.Streams.Audio;
use WC.Streams.IO_Buffer;
use WC.Host.Glue;

package WC.Streams.DSP is

   Bad_State :           exception;
   Bad_Argument :        exception;          -- programming error
   IO_Error :            exception;          -- an I/O operation could not be performed/completed
   Not_Supported :       exception;          -- Functionality is not supported

   --------------------------------------------------
   -- Stream Data Types :
   --------------------------------------------------
   type DSP_Stream_Type is limited private;  -- Type returned by Open()
   type DSP_Stream_Block is new Audio_Stream_Type with private;
   type DSP_Stream_Access is access all DSP_Stream_Block'Class;

   --------------------------------------------------
   -- Other supporting types :
   --------------------------------------------------
   type DSP_Mode is (In_Mode, Out_Mode);
   type DSP_Block_Size is new Natural;

   --------------------------------------------------
   -- Stream Functions :
   --------------------------------------------------
   procedure Open(Strm : in out DSP_Stream_Type; Mode : DSP_Mode; Name : String);
   procedure Close(Strm : in out DSP_Stream_Type);
   function Stream(Strm : DSP_Stream_Type) return Audio_Stream; -- DSP_Stream_Access;

   --------------------------------------------------
   -- Query Functions :
   --------------------------------------------------
   function Channels(Strm : DSP_Stream_Block) return Audio_Channels;
   function Sample_Rate(Strm : DSP_Stream_Block) return Audio_Sample_Rate;
   function Sample_Size(Strm : DSP_Stream_Block) return Audio_Sample_Size;
   function Sample_Type(Strm : DSP_Stream_Block) return Audio_Sample_Type;
   function Block_Size(Strm : DSP_Stream_Block) return DSP_Block_Size;
   function In_Items(Strm : DSP_Stream_Block; Byte_Size : Positive) return Positive;

   --------------------------------------------------
   -- Configuration Functions :
   --------------------------------------------------
   procedure Set_Channels(Strm : in out DSP_Stream_Block; Channels : Audio_Channels);
   procedure Set_Sample_Rate(Strm : in out DSP_Stream_Block; Sample_Rate : Audio_Sample_Rate);
   procedure Set_Sample_Size(Strm : in out DSP_Stream_Block; Sample_Size : Audio_Sample_Size);
   procedure Set_Block_Size(Strm : in out DSP_Stream_Block; Size : DSP_Block_Size);

   --------------------------------------------------
   -- Action Procedures :
   --------------------------------------------------
   procedure Flush(Strm : in out DSP_Stream_Block);

private 

   type DSP_Stream_Type is access all DSP_Stream_Block;

   --------------------------------------------------
   -- The DSP Control Block :
   --------------------------------------------------
   type DSP_Stream_Ptr is access all DSP_Stream_Block;

   type DSP_CB(Strm : DSP_Stream_Ptr) is new Controlled with
   record
      Changed :          Boolean             := True;    -- When true, these values are not set in the device yet
      Channels :         Audio_Channels      := 1;       -- Last established channel count
      Sample_Rate :      Audio_Sample_Rate   := 1;       -- Samples / second
      Sample_Size :      Audio_Sample_Size   := 8;       -- Sample size in bits
   end record;

   procedure Initialize(DSP : in out DSP_CB);
   procedure Finalize(DSP : in out DSP_CB);

   --------------------------------------------------
   -- The DSP Audio Stream :
   --------------------------------------------------
   type DSP_Stream_Block is new Audio_Stream_Type with
   record
      Fd :            File_Descriptor        := 0;           -- This duplicates what is in member dsp
      Mode :          DSP_Mode               := In_Mode;     -- I/O Mode
      Block_Size :    DSP_Block_Size         := 0;           -- Return the block size
      Out_Buf :       IO_Buf;                                -- Output Buffer
      CB :            DSP_CB(DSP_Stream_Block'Unchecked_Access);  -- Control block with finalization
   end record;
   
   --------------------------------------------------
   -- Primitives for the DSP_Stream_Block
   --------------------------------------------------
   procedure Read(Strm : in out DSP_Stream_Block; Item : out Stream_Element_Array; Last : out Stream_Element_Offset);
   procedure Write(Strm : in out DSP_Stream_Block; Item : Stream_Element_Array);

   --------------------------------------------------
   -- Unsupported Primitives (they are abstract) :
   --------------------------------------------------
   function Offset(Strm : DSP_Stream_Block) return Byte_Offset;
   procedure Set_Offset(Strm : in out DSP_Stream_Block; New_Offset : Byte_Offset);

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-dsp.ads,v 1.2 2005/02/11 02:59:38 ken Exp $";

end WC.Streams.DSP;
