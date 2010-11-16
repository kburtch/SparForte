-- $Id: wc-streams-io_buffer.adb,v 1.2 2005/02/11 02:59:39 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with ada.text_io;
use ada.text_io;

with System, Ada.Finalization, Ada.IO_Exceptions, Ada.Unchecked_Deallocation, Interfaces;
use System, Ada.Finalization, Ada.IO_Exceptions, Interfaces;

with WC.Host.Glue;
use WC.Host.Glue;

package body WC.Streams.IO_Buffer is

   CVSID : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-io_buffer.adb,v 1.2 2005/02/11 02:59:39 ken Exp $";

   procedure Free(Str_Array : in out Stream_Element_Array_Ptr);

   --------------------------------------------------
   -- Destructor for IO_Buf
   --------------------------------------------------
   procedure Finalize(Buf : in out IO_Buf) is
   begin

      if Buf.Buffer /= null then
         Free(Buf.Buffer);
      end if;

   end Finalize;

   --------------------------------------------------
   -- Establish/Change Buffer Size :
   --------------------------------------------------
   procedure Set_Size(Buf : in out IO_Buf; Size : Stream_Element_Offset := 2048) is
   begin

      if Buf.Buffer = null or else Set_Size.Size /= Buf.Size then
         if Buf.Buffer /= null then
            Free(Buf.Buffer);       -- Discard existing buffer and content
         end if;
         Buf.Size := Set_Size.Size;
         Buf.Buffer := new Stream_Element_Array(1..Buf.Size);
      end if;

      Buf.Offset := 1;
      Buf.Rd_Offset := 1;

      pragma assert(Buf.Buffer /= null);

   end Set_Size;
    
   --------------------------------------------------
   -- Write to the IO_Buf
   --------------------------------------------------
   procedure Write(Buf : in out IO_Buf; Item : Stream_Element_Array; Last : out Stream_Element_Offset)
   is
      Count :      Stream_Element_Offset := Item'Length;
      Used :       Stream_Element_Offset;
      Room :       Stream_Element_Offset;
   begin

      if Buf.Buffer = null then
         Set_Size(Buf);          -- Make sure we have a buffer to write to
      end if;

      Used := Buf.Offset - 1;     -- The amount used up in this buffer
      Room := Buf.Size - Used;    -- The space remaining in the buffer

      if Room = 0 then            -- No space remaining?
         raise Buffer_Exception;
      end if;

      if Count > Room then        -- Test if there is enough space in the buffer
         Count := Room;          -- else only copy what will fill the buffer
      end if;

      Buf.Buffer(Buf.Offset..Buf.Offset+Count-1) := Item(Item'First..Item'First+Count-1);
      Buf.Offset := Buf.Offset + Count;

      Last := Item'First + Count - 1;

   end Write;
    
   --------------------------------------------------
   -- Test if the IO_Buf is full :
   --------------------------------------------------
   function Is_Full(Buf : IO_Buf) return Boolean is
   begin

      return Buf.Offset = Buf.Size + 1;

   end Is_Full;

   --------------------------------------------------
   -- Return True if the IO_Buffer is Empty :
   --------------------------------------------------
   function Is_Empty(Buf : IO_Buf) return Boolean is
   begin

      return Buf.Rd_Offset = Buf.Offset;

   end Is_Empty;

   --------------------------------------------------
   -- Read from the IO_Buf
   --------------------------------------------------
   procedure Read(Buf : in out IO_Buf; Item : out Stream_Element_Array; Last : out Stream_Element_Offset)
   is
      Count :  Stream_Element_Offset := Item'Length;
      Avail :  Stream_Element_Offset;
   begin

      if Buf.Buffer = null then
         raise Buffer_Exception;
      end if;

      Avail := Buf.Offset - Buf.Rd_Offset;

      if Avail < 1 then
         raise Ada.IO_Exceptions.End_Error;
      end if;
        
      if Count > Avail then
         Count := Avail;
      end if;

      Item(Item'First..Item'First+Count-1) := Buf.Buffer(Buf.Rd_Offset..Buf.Rd_Offset+Count-1);
      Buf.Rd_Offset := Buf.Rd_Offset + Count;

      Last := Item'First + Count - 1;

   end Read;
    
   procedure Write(Buf : in out IO_Buf; Fd : File_Descriptor)
   is
      function C_write(Fd : Integer; Buf : System.Address; Cnt : Integer) return Integer;
      pragma import(C,C_write,"write");

      Z :      Integer;
      Count :  Integer := Integer(Buf.Offset - 1);
      Wrote :  Integer := 0;
   begin

      loop
         exit when Count = 0;

         Z := C_write(Integer(Fd),Buf.Buffer(Stream_Element_Offset(1+Wrote))'Address,Integer(Count));

         if Z > 0 then
            Wrote := Wrote + Z;
            Count := Count - Z;
         elsif Fetch_Error /= EINTR then
            raise IO_Error;
         end if;
      end loop;

      Buf.Offset := 1;
      buf.Rd_Offset := 1;

   end Write;

   --------------------------------------------------
   -- Return the Buffer's size :
   --------------------------------------------------
   function Size(Buf : IO_Buf) return Stream_Element_Offset is
   begin

      return Buf.Size;

   end Size;

   --------------------------------------------------
   -- Free an IO_Buf 
   --------------------------------------------------
   procedure Free(Buf : in out IO_Buf_Ptr)
   is
      procedure Free_Buf is new Ada.Unchecked_Deallocation(IO_Buf,IO_Buf_Ptr);
   begin

      Free_Buf(Buf);

   end Free;

   --------------------------------------------------
   -- Free a Stream_Element_Array
   --------------------------------------------------
   procedure Free(Str_Array : in out Stream_Element_Array_Ptr)
   is
      procedure Free_Array is new Ada.Unchecked_Deallocation(Stream_Element_Array,Stream_Element_Array_Ptr);
   begin

      Free_Array(Str_Array);

   end Free;

end WC.Streams.IO_Buffer;
