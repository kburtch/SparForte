-- $Id: wc-streams-buffer.ads,v 1.2 2005/02/11 02:59:38 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Interfaces; use Interfaces;
with Ada.Finalization; use Ada.Finalization;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;

with WC.Streams.Endian;

use WC.Streams.Endian;

package WC.Streams.Buffer is

   --------------------------------------------------
   -- Stream Data Types :
   --------------------------------------------------
   type Buf_Stream_Type is limited private;
   type Buf_Stream_Block is new Root_Stream_Type with private;
   type Buf_Stream_Access is access all Buf_Stream_Block'Class;
   
   type Root_Stream_Access is access all Root_Stream_Type'Class;

   --------------------------------------------------
   -- Stream Functions :
   --------------------------------------------------
   procedure Open(Strm : in out Buf_Stream_Type; Size : Stream_Element_Offset);
   procedure Close(Strm : in out Buf_Stream_Type);
   function Is_Open(Strm : Buf_Stream_Type) return Boolean;
   function Stream(Strm : Buf_Stream_Type) return Root_Stream_Access;

   procedure Reset(Strm : in out Buf_Stream_Type);
   function Count(Strm : Buf_Stream_Type) return Stream_Element_Offset;

   procedure Read(Strm : in out Buf_Stream_Type; Item : out Stream_Element_Array; Last : out Stream_Element_Offset);
   procedure Write(Strm : in out Buf_Stream_Type; Item : Stream_Element_Array);

private

   type Stream_Element_Array_Ptr is access all Stream_Element_Array;

   --------------------------------------------------
   -- The bulk of the Au_Stream is contained in this
   -- Root_Stream_Access derived record :
   --------------------------------------------------
   type Buf_Stream_Block is new Endian_Stream_Type with
   record
      Size :          Stream_Element_Offset    := 0;     -- Buffer Size
      Buffer :        Stream_Element_Array_Ptr;          -- Buffer
      RX :            Stream_Element_Offset    := 0;     -- Read Index
      WX :            Stream_Element_Offset    := 0;     -- Write Index
   end record;
   
   type Buf_Stream_Type is access all Buf_Stream_Block;    
   
   procedure Free(Strm : in out Buf_Stream_Type);
   procedure Read(Strm : in out Buf_Stream_Block; Item : out Stream_Element_Array; Last : out Stream_Element_Offset);
   procedure Write(Strm : in out Buf_Stream_Block; Item : Stream_Element_Array);

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-buffer.ads,v 1.2 2005/02/11 02:59:38 ken Exp $";

end WC.Streams.Buffer;
