-- $Id: wc-streams-dsp.adb,v 1.2 2005/02/11 02:59:38 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License
                 
with Ada.Text_Io;
use Ada.Text_Io;

with System, Ada.Unchecked_Deallocation;
use System;

package body WC.Streams.DSP is

   CVSID: constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-dsp.adb,v 1.2 2005/02/11 02:59:38 ken Exp $";

   procedure DSP_Set_Channels(Fd: File_Descriptor; Channels: Audio_Channels);
   procedure DSP_Set_Sample_Rate(Fd: File_Descriptor; Sample_Rate: Audio_Sample_Rate);
   procedure DSP_Set_Sample_Size(Fd: File_Descriptor; Sample_Size: Audio_Sample_Size);
   procedure Close(Fd: in out File_Descriptor);

   function DSP_Get_Block_Size(Fd: File_Descriptor) return Integer;

   procedure DSP_Sync(Fd: File_Descriptor);
   procedure DSP_Reset(Fd: File_Descriptor);

   procedure Free is new Ada.Unchecked_Deallocation(DSP_Stream_Block,DSP_Stream_Type);

   --------------------------------------------------
   -- Stream Functions :
   --------------------------------------------------
   procedure Open(Strm: in out DSP_Stream_Type; Mode: DSP_Mode; Name: String) is
      Fd:             File_Descriptor := -1;
      Block_Size:     Integer;
   begin

      Fd := Open(Name,O_WRONLY,0);
      Block_Size := DSP_Get_Block_Size(Fd);

      Strm := new DSP_Stream_Block;
      Strm.Fd := Fd;
      Strm.Mode := Mode;
      Strm.Block_Size := DSP_Block_Size(Block_Size);

      Set_Size(Strm.Out_Buf,Stream_Element_Offset(Strm.Block_Size));           -- Set Buffer Size

   end Open;

   --------------------------------------------------
   -- Close open stream :
   --------------------------------------------------
   procedure Close(Strm: in out DSP_Stream_Type) is
   begin

      Flush(Strm.all);
      Free(Strm);

   end Close;

   --------------------------------------------------
   -- Get a Stream_Access value :
   --------------------------------------------------
   function Stream(Strm: DSP_Stream_Type) return Audio_Stream is
   begin

      return Audio_Stream(Strm);

   end Stream;


   --------------------------------------------------
   -- Return the # of channels
   --------------------------------------------------
   function Channels(Strm: DSP_Stream_Block) return Audio_Channels is
   begin

      return Strm.CB.Channels;

   end Channels;
    
   --------------------------------------------------
   -- Return the sample rate
   --------------------------------------------------
   function Sample_Rate(Strm: DSP_Stream_Block) return Audio_Sample_Rate is
   begin

      return Strm.CB.Sample_Rate;

   end Sample_Rate;

   --------------------------------------------------
   -- Return the sample size
   --------------------------------------------------
   function Sample_Size(Strm: DSP_Stream_Block) return Audio_Sample_Size is
   begin

      return Strm.CB.Sample_Size;

   end Sample_Size;

   --------------------------------------------------
   -- Return the Sample Type :
   --------------------------------------------------
   function Sample_Type(Strm: DSP_Stream_Block) return Audio_Sample_Type is
   begin

      case Sample_Size(Strm) is
         when 8 =>         return CODEC_Linear_8;
         when 16 =>        return CODEC_Linear_16;
         when others =>    return CODEC_Unsupported;
      end case;

   end Sample_Type;

   --------------------------------------------------
   -- Set the # of channels
   --------------------------------------------------
   procedure Set_Channels(Strm: in out DSP_Stream_Block; Channels: Audio_Channels) is
   begin

      Strm.CB.Changed := True;
      Strm.CB.Channels := Channels;
      DSP_Set_Channels(Strm.Fd,Strm.CB.Channels);

   end Set_Channels;

   --------------------------------------------------
   -- Set the sample rate 
   --------------------------------------------------
   procedure Set_Sample_Rate(Strm: in out DSP_Stream_Block; Sample_Rate: Audio_Sample_Rate) is
   begin

      Strm.CB.Changed := True;
      Strm.CB.Sample_Rate := Sample_Rate;
      DSP_Set_Sample_Rate(Strm.Fd,Strm.CB.Sample_Rate);

   end Set_Sample_Rate;

   --------------------------------------------------
   -- Set the sample size
   --------------------------------------------------
   procedure Set_Sample_Size(Strm: in out DSP_Stream_Block; Sample_Size: Audio_Sample_Size) is
   begin

      Strm.CB.Changed := True;
      Strm.CB.Sample_Size := Sample_Size;
      DSP_Set_Sample_Size(Strm.Fd,Strm.CB.Sample_Size);

   end Set_Sample_Size;

   --------------------------------------------------
   -- Return the block size
   --------------------------------------------------
   function Block_Size(Strm: DSP_Stream_Block) return DSP_Block_Size is
   begin

      return Strm.Block_Size;

   end Block_Size;

   --------------------------------------------------
   -- Set the block size 
   --------------------------------------------------
   procedure Set_Block_Size(Strm: in out DSP_Stream_Block; Size: DSP_Block_Size) is
   begin
   
      Strm.Block_Size := Size;

   end Set_Block_Size;

   --------------------------------------------------
   -- Flush out the written sample data :
   --------------------------------------------------
   procedure Flush(Strm: in out DSP_Stream_Block) is
   begin

      Write(Strm.Out_Buf,Strm.Fd);
      DSP_Sync(Strm.Fd);

   end Flush;

   --------------------------------------------------
   -- Initialize a DSP control block
   --------------------------------------------------
   procedure Initialize(DSP: in out DSP_CB) is
   begin

      DSP.Strm.Fd := 0;

   end Initialize;

   --------------------------------------------------
   -- Destroy a DSP control block :
   --------------------------------------------------
   procedure Finalize(DSP: in out DSP_CB) is
   begin

      DSP_Sync(DSP.Strm.Fd);
      Close(DSP.Strm.Fd);

   end Finalize;

   --------------------------------------------------
   -- Unsupported Primitives (they are abstract) :
   --------------------------------------------------
   function Offset(Strm: DSP_Stream_Block) return Byte_Offset is
   begin

      raise Not_Supported;
      return 0;

   end Offset;

   --------------------------------------------------
   -- Offsets are not supported by the DSP :
   --------------------------------------------------
   procedure Set_Offset(Strm: in out DSP_Stream_Block; New_Offset: Byte_Offset) is
   begin

      raise Not_Supported;

   end Set_Offset;

   --------------------------------------------------
   -- Establish the number of sample channels :
   --------------------------------------------------
   procedure DSP_Set_Channels(Fd: File_Descriptor; Channels: Audio_Channels) is
      function C_Ioctl(Fd: Integer; Cmd: Integer; Channels: System.Address) return Integer;
      pragma Import(C,C_Ioctl,"ioctl");

      Z:      Integer;
      Chans:  Integer := Integer(Channels - 1);    -- ioctl uses a zero based value

   begin

      Z := C_Ioctl(Integer(Fd),Sndctl_Dsp_Stereo,Chans'Address);
      if Z = -1 then
         raise IO_Error;
      end if;

   end DSP_Set_Channels;

   --------------------------------------------------
   -- Set the DSP sample size to use :
   --------------------------------------------------
   procedure DSP_Set_Sample_Size(Fd: File_Descriptor; Sample_Size: Audio_Sample_Size) is
      function C_Ioctl(Fd: Integer; Cmd: Integer; Block_Size: System.Address) return Integer;
      pragma Import(C,C_Ioctl,"ioctl");

      Z:              Integer;
      Samp_Size:      Integer := Integer(Sample_Size);
   begin

      Z := C_Ioctl(Integer(Fd),Sndctl_Dsp_Samplesize,Samp_Size'Address);
      if Z = -1 then
         raise IO_Error;
      end if;

   end DSP_Set_Sample_Size;

   --------------------------------------------------
   -- Establish the Sampling Rate :
   --------------------------------------------------
   procedure DSP_Set_Sample_Rate(Fd: File_Descriptor; Sample_Rate: Audio_Sample_Rate) is
      function C_Ioctl(Fd: Integer; Cmd: Integer; Samprt: System.Address) return Integer;
      pragma Import(C,C_Ioctl,"ioctl");

      Z:      Integer;
      Samprt: Integer := Integer(Sample_Rate);
   begin

      Z := C_Ioctl(Integer(Fd),Sndctl_Dsp_Speed,Samprt'Address);
      if Z = -1 then
         raise IO_Error;
      end if;

   end DSP_Set_Sample_Rate;

   --------------------------------------------------
   -- Returns the Block Size for the DSP :
   --------------------------------------------------
   function DSP_Get_Block_Size(Fd: File_Descriptor) return Integer is
      function C_Ioctl(Fd: Integer; Cmd: Integer; Block_Size: System.Address) return Integer;
      pragma Import(C,C_Ioctl,"ioctl");

      Z:          Integer := 0;
      Block_Size: Integer := 0;
   begin

      Z := C_Ioctl(Integer(Fd), Sndctl_Dsp_Getblksize, Block_Size'Address);
      if Z < 0 then
         raise Io_Error;
      end if;
      return Block_Size;

   end DSP_Get_Block_Size;

   --------------------------------------------------
   -- Sync the DSP device :
   --------------------------------------------------
   procedure DSP_Sync(Fd: File_Descriptor) is
      function C_Ioctl(Fd: Integer; Cmd: Integer; Dummy: Integer) return Integer;
      pragma Import(C,C_Ioctl,"ioctl");

      Z:      Integer;
      Zero:   Integer := 0;
   begin

      Z := C_Ioctl(Integer(Fd),Sndctl_Dsp_Sync,Zero);
      if Z = -1 then
         raise Io_Error;
      end if;

   end DSP_Sync;

   --------------------------------------------------
   -- Reset the DSP device :
   --------------------------------------------------
   procedure DSP_Reset(Fd: File_Descriptor) is
      function C_Ioctl(Fd: Integer; Cmd: Integer; Dummy: Integer) return Integer;
      pragma Import(C,C_Ioctl,"ioctl");

      Z:      Integer;
      Zero:   Integer := 0;
   begin

      Z := C_Ioctl(Integer(Fd),Sndctl_Dsp_Reset,Zero);
      if Z = -1 then
         raise Io_Error;
      end if;

   end DSP_Reset;

   --------------------------------------------------
   -- Close the open DSP control block :
   --------------------------------------------------
   procedure Close(Fd: in out File_Descriptor) is
      function C_Close(Fd: Integer) return Integer;
      pragma Import(C,C_Close,"close");

      Z:      Integer;
   begin

      Z := C_Close(Integer(Fd));
      if Z /= 0 then
         raise IO_Error;
      end if;
      
      Fd := 0;                            -- Value -1 would be better for this..

   end Close;

   --------------------------------------------------
   -- Return the # of Items that are Safe to Read :
   --------------------------------------------------
   function In_Items(Strm: DSP_Stream_Block; Byte_Size: Positive) return Positive is
   begin

      return 1;           -- Unknown. Assume 1

   end In_Items;

   --------------------------------------------------
   -- Streams Interface :
   --------------------------------------------------
   -- Write to the stream
   --------------------------------------------------
   procedure Write(Strm: in out DSP_Stream_Block; Item: Stream_Element_Array) is
      X_First:  Stream_Element_Offset := Item'First;
      X_Last:   Stream_Element_Offset := Item'Last;
   begin

      loop
         Write(Strm.Out_Buf,Item(X_First..Item'Last),X_Last);
         if Is_Full(Strm.Out_Buf) then
            Write(Strm.Out_Buf,Strm.Fd);
         end if;
         X_First := X_Last + 1;
         exit when X_Last = Item'Last;
      end loop;

   end Write;

   --------------------------------------------------
   -- Read from the stream
   --------------------------------------------------
   procedure Read(Strm: in out DSP_Stream_Block; Item: out Stream_Element_Array; Last: out Stream_Element_Offset) is
   begin

      raise Not_Supported;

   end Read;

end WC.Streams.DSP;
