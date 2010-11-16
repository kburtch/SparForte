-- $Id: wc-streams-buffer.adb,v 1.2 2005/02/11 02:59:38 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Streams; use Ada.Streams;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

package body WC.Streams.Buffer is

   CVSID : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-buffer.adb,v 1.2 2005/02/11 02:59:38 ken Exp $";

   --------------------------------------------------
   -- Open a Buffer Stream :
   --------------------------------------------------
   procedure Open(Strm : in out Buf_Stream_Type; Size : Stream_Element_Offset) is
   begin

      Strm := new Buf_Stream_Block;
      Strm.Size := Open.Size;
      Strm.Buffer := new Stream_Element_Array(1..Strm.Size);
      Strm.RX := Strm.Buffer'First;
      Strm.WX := Strm.Buffer'First;

   end Open;

   --------------------------------------------------
   -- Return True if the Buffer Stream is Open
   --------------------------------------------------
   function Is_Open(Strm : Buf_Stream_Type) return Boolean is
   begin
      return Strm /= null;
   end Is_Open;

   --------------------------------------------------
   -- Close a Buffer Stream :
   --------------------------------------------------
   procedure Close(Strm : in out Buf_Stream_Type) is
   begin

      Free(Strm);

   end Close;

   --------------------------------------------------
   -- Return a Stream Pointer :
   --------------------------------------------------
   function Stream(Strm : Buf_Stream_Type) return Root_Stream_Access is
   begin

      return Root_Stream_Access(Strm);

   end Stream;

   --------------------------------------------------
   -- Read from a Buffer Stream :
   --------------------------------------------------
   procedure Read(Strm : in out Buf_Stream_Block; Item : out Stream_Element_Array; Last : out Stream_Element_Offset)
   is
      Count :  Stream_Element_Offset := Item'Length;
      Avail :  Stream_Element_Offset := Strm.WX - Strm.RX;
   begin

      if Avail <= 0 then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      
      if Count > Avail then
         Count := Avail;
      end if;

      Last := Item'First + Count - 1;

      declare
         X1 :  Stream_Element_Offset := Strm.Buffer'First + Strm.RX - 1;
         X2 :  Stream_Element_Offset := X1 + Count - 1;
      begin

         Item(Item'First..Last) := Strm.Buffer(X1..X2);

      end;

      Strm.RX := Strm.RX + Count;

   end read;
    
   procedure Read(Strm : in out Buf_Stream_Type; Item : out Stream_Element_Array; Last : out Stream_Element_Offset) is
   begin

      Read(Strm.all,Item,Last);

   end Read;

   --------------------------------------------------
   -- Return the Element Count Waiting to be Read :
   --------------------------------------------------
   function Count(Strm : Buf_Stream_Type) return Stream_Element_Offset is
   begin

      return Strm.WX - Strm.RX;

   end Count;

   --------------------------------------------------
   -- Write to a Buffer Stream :
   --------------------------------------------------
   procedure Write(Strm : in out Buf_Stream_Block; Item : Stream_Element_Array)
   is
      Avail :  Stream_Element_Offset := Strm.Size - Strm.WX;
      Count :  Stream_Element_Offset := Item'Length;
   begin

      if Avail < Count then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      declare
         X1 :  Stream_Element_Offset := Strm.Buffer'First + Strm.WX - 1;
         X2 :  Stream_Element_Offset := X1 + Count - 1;
      begin

         Strm.Buffer(X1..X2) := Item;

      end;

      Strm.WX := Strm.WX + Count;

   end Write;
    
   procedure Write(Strm : in out Buf_Stream_Type; Item : Stream_Element_Array) is
   begin

      Write(Strm.all,Item);

   end Write;

   --------------------------------------------------
   -- Reset a Buffer Stream :
   --------------------------------------------------
   procedure Reset(Strm : in out Buf_Stream_Type) is
   begin

      Strm.RX := Strm.Buffer'First;
      Strm.WX := Strm.Buffer'First;

   end Reset;

   --------------------------------------------------
   -- Release a Buffer Stream Object :
   --------------------------------------------------
   procedure Free(Strm : in out Buf_Stream_Type)
   is
      procedure Rele is new Ada.Unchecked_Deallocation(Stream_Element_Array,Stream_Element_Array_Ptr);
      procedure Rele is new Ada.Unchecked_Deallocation(Buf_Stream_Block,Buf_Stream_Type);
   begin

      if Strm.Buffer /= null then
         Rele(Strm.Buffer);
      end if;
      Rele(Strm);

   end Free;

end WC.Streams.Buffer;
