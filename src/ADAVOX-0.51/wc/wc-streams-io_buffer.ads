-- $Id: wc-streams-io_buffer.ads,v 1.2 2005/02/11 02:59:39 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Finalization, Ada.Streams, Ada.Unchecked_Deallocation;
use Ada.Finalization, Ada.Streams;

with WC.Host;
use WC.Host;

package WC.Streams.IO_Buffer is

   Buffer_Exception :   exception;

   type IO_Buf is new Controlled with private;
   type IO_Buf_Ptr is access all IO_Buf;

   procedure Set_Size(Buf : in out IO_Buf; Size : Stream_Element_Offset := 2048);
   procedure Write(Buf : in out IO_Buf; Fd : File_Descriptor);
   procedure Free(Buf : in out IO_Buf_Ptr);
   function Size(Buf : IO_Buf) return Stream_Element_Offset;
   function Is_Full(Buf : IO_Buf) return Boolean;
   function Is_Empty(Buf : IO_Buf) return Boolean;

   procedure Write(Buf : in out IO_Buf; Item : Stream_Element_Array; Last : out Stream_Element_Offset);
   procedure Read(Buf : in out IO_Buf; Item : out Stream_Element_Array; Last : out Stream_Element_Offset);

private

   type Stream_Element_Array_Ptr is access all Stream_Element_Array;

   type IO_Buf is new Controlled with
      record
         Size :      Stream_Element_Offset   := 2048;    -- Size of Buffer
         Offset :    Stream_Element_Offset   := 1;       -- Offset of next free element within buffer
         Rd_Offset : Stream_Element_Offset   := 1;       -- Read offset within the buffer
         Buffer :    Stream_Element_Array_Ptr;           -- The Buffer itself
      end record;

    procedure Finalize(Buf : in out IO_Buf);

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-io_buffer.ads,v 1.2 2005/02/11 02:59:39 ken Exp $";

end WC.Streams.IO_Buffer;
