-- $Id: wc-streams-au.adb,v 1.2 2005/02/11 02:59:37 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Unchecked_Conversion, Ada.Unchecked_Deallocation;

package body WC.Streams.Au is

   CvsID: constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-au.adb,v 1.2 2005/02/11 02:59:37 ken Exp $";

   subtype Positive_Count is Ada.Streams.Stream_IO.Positive_Count;

   --------------------------------------------------
   -- Since Open() can raise an exception, the open
   -- stream cannot be returned. This is less than
   -- ideal, but we save the *.au format here, so
   -- that a user friendly error message can indicate
   -- what *.au format is not supported.
   --
   -- This value is returned by function Last_Au_Format
   --------------------------------------------------
   Last_Format : Snd_Format_Type := Snd_Format_Unspecified; -- Last format read

   procedure Free is new Ada.Unchecked_Deallocation(Au_Stream_Block,Au_Stream_Ptr);

   --------------------------------------------------
   -- Open a Au_Stream :
   --------------------------------------------------
   procedure Open(Strm : in out Au_Stream_Type; Mode : Au_Mode; Name : String) is
      function Int_To_String4 is new Ada.Unchecked_Conversion(Audio_I32,String4);

      type String_4 is new String(1..4);

      type Hdr_Recd is
      record
         Data_Loc :     Audio_I32;                          -- Data location
         Data_Size :    Audio_I32;                          -- Size (bytes)
         Format :       Snd_Format_Type;
         Samp_Rate :    Audio_I32;                          -- Sample Rate
         Channels :     Audio_I32;                          -- Channels (1 to n)
      end record;

      Actual_Magic :    Unsigned_32;                        -- Read from file with no endian conversion
      Hdr :             Hdr_Recd;                           -- Read with endian conversions
   begin

      begin
         if Mode /= In_Mode then                            -- Only In_Mode is supported
            raise Not_Supported;
         end if;

         Strm := new Au_Stream_Block;                       -- Allocate a new Audio Stream

         Open(Strm.File,In_File,Name);                      -- Open File_Type
         Strm.Str := Stream(Strm.File);                     -- Get the Stream_Access pointer

         begin
            Unsigned_32'Read(Strm.Str,Actual_Magic);        -- Need to read this much unchanged
            Map(Strm.all,Au_Magic_Value,Actual_Magic);
         exception
            when Mapping_Error =>
               raise Not_Au_Format;                         -- This file is definitely not an AU format file
            when others =>
               raise Bad_Au_Format;                         -- Some other bug occurred
         end;

         Hdr_Recd'Read(Strm,Hdr);                           -- The rest should now read OK

         Strm.Format       := Hdr.Format;
         Strm.Channels     := Audio_Channels(Hdr.Channels);
         Strm.Sample_Rate  := Audio_Sample_Rate(Hdr.Samp_Rate);
         Strm.Sample_Size  := 8;                            -- This depends upon format
         Strm.Data_Loc     := Byte_Offset(Hdr.Data_Loc);
         Strm.Data_Size    := Byte_Offset(Hdr.Data_Size);

         if Offset(Strm.all) + 4 <= Strm.Data_Loc then
            Au_Info'Read(Strm,Strm.Info);                   -- Read optional info
         end if;

         if Offset(Strm.all) < Strm.Data_Loc then
            raise Bad_Data_Loc;
         end if;

         if Offset(Strm.all) > Strm.Data_Loc then
            Set_Offset(Strm.all,Strm.Data_Loc);             -- Index() uses 1-based offset
         end if;
      exception
         when Not_Au_Format | Bad_Au_Format =>
            Close(Strm);
            raise;                                          -- Already handled above..
         when others =>
            Close(Strm);
            raise;
      end;

      Last_Format := Strm.Format;                           -- Save for later inquiry

      case Strm.Format is
         when Snd_Format_Mulaw =>            Strm.Sample_Size := 8;
         when Snd_Format_Alaw_8 =>           Strm.Sample_Size := 8;
         when Snd_Format_Linear_8 =>         Strm.Sample_Size := 8;
         when Snd_Format_Linear_16 =>        Strm.Sample_Size := 16;
         when Snd_Format_Linear_24 =>        Strm.Sample_Size := 16;
--       when Snd_Format_Linear_32 =>        Strm.Sample_Size := 32;
--       when Snd_Format_Adpcm_G721 =>
--       when Snd_Format_Adpcm_G722 =>
--       when Snd_Format_Adpcm_G723_3 =>
--       when Snd_Format_Adpcm_G723_5 =>
         when others =>
         Close(Strm);
         raise Unsupported_Format;                          -- This file format is not supported here
      end case;

      Strm.Au_Str := Strm;                                  -- For ease of reference in other places
    
   end Open;

   --------------------------------------------------
   -- Close the Au_Stream :
   --------------------------------------------------
   procedure Close(Strm : in out Au_Stream_Type) is
   begin

      if Strm = null then                                   -- Already closed?
         raise Program_Error;
      end if;

      Strm.Str := null;                                     -- No longer need it's stream pointer
      if Is_Open(Strm.File) then                            -- Normally, the file should still be open..
         Close(Strm.File);                                  -- Close the Ada.Streams.Stream_IO.File_Type
      end if;

      Free(Au_Stream_Ptr(Strm));                            -- Free the Au_Stream_Block
      Strm := null;                                         -- Set the caller's pointer to null

   end Close;

   --------------------------------------------------
   -- Read on our Au_Stream :
   --------------------------------------------------
   procedure Read(
      Strm : in out Au_Stream_Block;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset
   ) is
   begin

      Read(Strm.File,Item,Last);  -- Read from our internal Ada.Streams.Stream_IO.File_Type

   end Read;

   --------------------------------------------------
   -- Write on our Au_Stream :
   --------------------------------------------------
   procedure Write(
      Strm : in out Au_Stream_Block;
      Item : Stream_Element_Array
   ) is
   begin

      Write(Strm.File,Item);      -- Write on our internal Ada.Streams.Stream_IO.File_Type

   end Write;

   --------------------------------------------------
   -- Read and Endian Convert an Snd_Format_Type:
   --------------------------------------------------
   procedure Read_Snd_Format_Type(Stream : access Root_Stream_Type'Class; Item : out Snd_Format_Type)
   is
      function From_I32 is new Ada.Unchecked_Conversion(Audio_I32,Snd_Format_Type);
      I : Audio_I32;
   begin

      Audio_I32'Read(Stream,I);
      Item := From_I32(I);

   end Read_Snd_Format_Type;

   --------------------------------------------------
   -- The Initialize Method :
   --------------------------------------------------
   procedure Initialize(CB : in out Au_CB) is
   begin

      null;  -- Currently all is done in the record default initialization

   end Initialize;

   --------------------------------------------------
   -- The Finalize Method :
   --------------------------------------------------
   procedure Finalize(CB : in out Au_CB) is
   begin

      if Is_Open(CB.Strm.File) then   -- Check if our Ada.Streams.Stream_IO.File_Type is still open..
         Close(CB.Strm.File);         -- .. then close it.
      end if;

   end Finalize;

   --------------------------------------------------
   -- Audio_Stream primitive for Channels
   --------------------------------------------------
   function Channels(Strm : Au_Stream_Block) return Audio_Channels is
   begin

      return Strm.Channels;

   end Channels;
    
   --------------------------------------------------
   -- Audio_Stream primitive for Sample_Rate
   --------------------------------------------------
   function Sample_Rate(Strm : Au_Stream_Block) return Audio_Sample_Rate is
   begin

      return Strm.Sample_Rate;

   end Sample_Rate;

   --------------------------------------------------
   -- Audio_Stream primitive for Sample_Size
   --------------------------------------------------
   function Sample_Size(Strm : Au_Stream_Block) return Audio_Sample_Size is
   begin

      return Strm.Sample_Size;

   end Sample_Size;

   --------------------------------------------------
   -- Return the Sample type to the CODEC :
   --------------------------------------------------
   function Sample_Type(Strm : Au_Stream_Block) return Audio_Sample_Type is
   begin

      case Strm.Format is
         when Snd_Format_Linear_8 =>      return CODEC_Linear_8;
         when Snd_Format_Linear_16 =>     return CODEC_Linear_16;
         when Snd_Format_Linear_24 =>     return CODEC_Linear_24;
         when Snd_Format_Linear_32 =>     return CODEC_Linear_32;
         when Snd_Format_Mulaw =>         return CODEC_Mulaw;
         when Snd_Format_Alaw_8 =>        return CODEC_Alaw_8;
         when others =>                   return CODEC_Unsupported;
      end case;

   end Sample_Type;

   --------------------------------------------------
   -- Return the zero based offset :
   --------------------------------------------------
   function Offset(Strm : Au_Stream_Block) return Byte_Offset is
   begin

      return Byte_Offset( Index(Strm.File) - 1 );

   end Offset;

   --------------------------------------------------
   -- Return the # of Input Items Waiting to be Read :
   --
   -- This helps the CODEC to block requests for better
   -- efficiency.
   --------------------------------------------------
   function In_Items(Strm : Au_Stream_Block; Byte_Size : Positive) return Positive
   is
      Bytes_Remaining : Byte_Offset := Strm.Data_Loc + Strm.Data_Size - Offset(Strm);
      Items_Remaining : Byte_Offset := 0;
   begin

      if Byte_Size <= 16 and then Bytes_Remaining > 4096 then
         Bytes_Remaining := 4096;   -- Keep memory requirements reasonable
      end if;

      Items_Remaining := Bytes_Remaining / Byte_Offset(Byte_Size);

      if Items_Remaining < 1 then
         return 1;                  -- Always return a minimum of 1 item
      end if;

      return Positive( Items_Remaining );

   end In_Items;

   --------------------------------------------------
   -- Set a new file offset :
   --------------------------------------------------
   procedure Set_Offset(Strm : in out Au_Stream_Block; New_Offset : Byte_Offset)
   is
      New_Index : Positive_Count := Positive_Count( New_Offset + 1 );
   begin

      Set_Index(Strm.File,New_Index);

   end Set_Offset;

   --------------------------------------------------
   -- Set the # of channels for recording
   --------------------------------------------------
   procedure Set_Channels(Strm : in out Au_Stream_Block; Channels : Audio_Channels) is
   begin

      raise Not_Supported;

   end Set_Channels;

   --------------------------------------------------
   -- Set the Sample Rate for recording
   --------------------------------------------------
   procedure Set_Sample_Rate(Strm : in out Au_Stream_Block; Sample_Rate : Audio_Sample_Rate) is
   begin

      raise Not_Supported;

   end Set_Sample_Rate;

   --------------------------------------------------
   -- Set the Sample Size for Recording 
   --------------------------------------------------
   procedure Set_Sample_Size(Strm : in out Au_Stream_Block; Sample_Size : Audio_Sample_Size) is
   begin

      raise Not_Supported;

   end Set_Sample_Size;

   --------------------------------------------------
   -- Return Optional Au Header Info :
   --------------------------------------------------
   function Info(Strm : Au_Stream_Block) return Au_Info is
   begin

      return Strm.Info;

   end Info;

   --------------------------------------------------
   -- Return the Format Type :
   --------------------------------------------------
   function Last_Au_Format return Snd_Format_Type is
   begin

      return Last_Format;

   end Last_Au_Format;

   --------------------------------------------------
   -- Return an Au_Stream_Access :
   --------------------------------------------------
   function Stream(Au_Strm : Au_Stream_Type) return Audio_Stream is
   begin

      return Audio_Stream(Au_Strm);

   end Stream;

end WC.Streams.Au;
