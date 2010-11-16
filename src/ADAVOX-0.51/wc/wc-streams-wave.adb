-- $Id: wc-streams-wave.adb,v 1.2 2005/02/11 02:59:39 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Text_IO;
with Interfaces; use Interfaces;
with Ada.Tags; use Ada.Tags;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body WC.Streams.Wave is

   CVSID : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-wave.adb,v 1.2 2005/02/11 02:59:39 ken Exp $";

   type String_4 is new String(1..4);
   subtype Positive_Count is Ada.Streams.Stream_IO.Positive_Count;

   procedure Dump(Chunk : in out Chunk_Hdr_Access; Level : Natural) is
      use Ada.Text_IO;
      Hdr :    Chunk_Hdr_Ptr := Chunk_Hdr_Ptr(Chunk);
   begin
      Put_Line(Level'Img & " " & String(Hdr.Chunk_ID));
   end Dump;

   procedure Dump(Tree : Tree_Type_Ptr) is
   begin
      Traverse_Tree(Tree,Depth_First,Dump'Access);
   end Dump;

   --------------------------------------------------
   -- Free Routines
   --------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Riff_Form,Riff_Form_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(Format_Chunk,Format_Chunk_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(Fact_Chunk,Fact_Chunk_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(Data_Chunk,Data_Chunk_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(List_Chunk,List_Chunk_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(List_Info_Chunk,List_Info_Chunk_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(Disp_Chunk,Disp_Chunk_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(Unsupported_Chunk,Unsupported_Chunk_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(Wave_Stream_Block,Wave_Stream_Type);

   --------------------------------------------------
   -- Seek to the start of the next chunk :
   --------------------------------------------------
   procedure Seek_Next(Stream : Audio_Stream; Start : Byte_Offset; Hdr : Chunk_Hdr) is
      To_Offset :  Byte_Offset := Start + 8 + Byte_Offset(Hdr.Chunk_Size);
      M :          Unsigned_32 := Unsigned_32(To_Offset);
   begin

      if M mod 2 /= 0 then
         To_Offset := To_Offset + 1;     -- Align to the nearest 16 bit word
      end if;
      Set_Offset(Stream.all,To_Offset);

   end Seek_Next;

   --------------------------------------------------
   -- Read the next Chunk from the Input Stream :
   --------------------------------------------------
   function Chunk_Hdr_Class_Input(Stream : access Ada.Streams.Root_Stream_Type'Class) return Chunk_Hdr'Class is
      H :              Chunk_Hdr;
      Chunk_Offset :   Byte_Offset;
      In_Context :     Dyn_String := Context(Wave_Stream_Block(Stream.all));
   begin

      Chunk_Offset := Offset(Wave_Stream_Block(Stream.all));

      if Chunk_Offset mod 2 /= 0 then
         -- Start at the next work aligned boundary (Microsoft format requirement)
         Chunk_Offset := Chunk_Offset + 1;
         Set_Offset(Wave_Stream_Block(Stream.all),Chunk_Offset);
      end if;

      if Chunk_Offset >= End_Context(Wave_Stream_Block(Stream.all)) then
         Set_Offset(Wave_Stream_Block(Stream.all),End_Context(Wave_Stream_Block(Stream.all)));
         Pop_Context(Wave_Stream_Block(Stream.all));
         raise End_Of_Context;               -- Indicate to the caller that we've reached the end of this context
      end if;

      Chunk_Hdr'Read(Stream,H);

      if In_Context = "ROOT" then
         if H.Chunk_ID = "RIFF" then
            declare
               R :      Riff_Form;
               Start :  Byte_Offset := Chunk_Offset + 12;
            begin
               Chunk_Hdr(R) := H;
               Fourcc'Read(Stream,R.Form_Type);
               Add_Context(Wave_Stream_Block(Stream.all),"RIFF",Start,Start + Byte_Offset(R.Chunk_Size) - 4);
               return R;
            end;
         end if;
      elsif In_Context = "ROOT.RIFF" then
         if H.Chunk_ID = "LIST" then
            declare
               R :      List_Chunk;
               C :      String(1..10);
               Start :  Byte_Offset := Chunk_Offset + 12;
            begin
               Chunk_Hdr(R) := H;
               Fourcc'Read(Stream,R.List_Type);
               C := "LIST(" & String(R.List_Type) & ")";
               Add_Context(Wave_Stream_Block(Stream.all),C,Start, Start + Byte_Offset(R.Chunk_Size) - 4);
               return R;
            end;
      elsif H.Chunk_ID = "fmt " then
         declare
            Wave_Type : Wave_Format_Type;
         begin
            Wave_Format_Type'Read(Stream,Wave_Type);
            case Wave_Type is
               when Wave_Format_PCM =>
                  declare
                     R :      Format_Chunk(Wave_Type,0);
                  begin
                     Chunk_Hdr(R) := H;
                     Audio_U16'Read(Stream,R.Channels);
                     Audio_U32'Read(Stream,R.Sample_Rate);
                     Audio_U32'Read(Stream,R.Avg_Byte_Rate);
                     Audio_U16'Read(Stream,R.Block_Align);
                     Audio_U16'Read(Stream,R.Bits_Per_PCM);
                     Seek_Next(Audio_Stream(Stream),Chunk_Offset,H);
                     return R;
                  end;
               when Wave_Format_ADPCM =>
                  declare
                     T :      Format_Chunk(Wave_Type,0);
                     N :      Audio_U16;
                  begin
                     Audio_U16'Read(Stream,T.Channels);
                     Audio_U32'Read(Stream,T.Sample_Rate);
                     Audio_U32'Read(Stream,T.Avg_Byte_Rate);
                     Audio_U16'Read(Stream,T.Block_Align);
                     Audio_U16'Read(Stream,T.Bits_Per_ADPCM);
                     Audio_U16'Read(Stream,T.CB_Size);
                     Audio_U16'Read(Stream,T.Samples_Per_Blk);
                     Audio_U16'Read(Stream,N);       -- # of Coefficients
                     declare
                        R :      Format_Chunk(Wave_Type,N);
                     begin
                        Chunk_Hdr(R) := H;          -- Copy over header parts
                        R.Channels := T.Channels;
                        R.Sample_Rate := T.Sample_Rate;
                        R.Avg_Byte_Rate := T.Avg_Byte_Rate;
                        R.Block_Align := T.Block_Align;
                        R.Bits_Per_ADPCM := T.Bits_Per_ADPCM;
                        R.CB_Size := T.CB_Size;
                        R.Samples_Per_Blk := T.Samples_Per_Blk;

                        for X in R.Coef'Range loop
                           Coef_Set'Read(Stream,R.Coef(X));
                        end loop;

                        Seek_Next(Audio_Stream(Stream),Chunk_Offset,H);
                        return R;
                     end;
                  end;
               when Wave_Format_ALaw | Wave_Format_MULaw =>
                  declare
                     R :      Format_Chunk(Wave_Type,0);
                  begin
                     Chunk_Hdr(R) := H;
                     Audio_U16'Read(Stream,R.Channels);
                     Audio_U32'Read(Stream,R.Sample_Rate);
                     Audio_U32'Read(Stream,R.Avg_Byte_Rate);
                     Audio_U16'Read(Stream,R.Block_Align);
                     Audio_U16'Read(Stream,R.ULaw_Bits_Per_Sample);
                     Audio_U16'Read(Stream,R.ULaw_CB_Size);
                     Seek_Next(Audio_Stream(Stream),Chunk_Offset,H);
                     return R;
                  end;
               when Wave_Format_IMA_ADPCM =>
                  declare
                     R :      Format_Chunk(Wave_Type,0);
                  begin
                     Chunk_Hdr(R) := H;
                     Audio_U16'Read(Stream,R.Channels);
                     Audio_U32'Read(Stream,R.Sample_Rate);
                     Audio_U32'Read(Stream,R.Avg_Byte_Rate);
                     Audio_U16'Read(Stream,R.Block_Align);
                     Audio_U16'Read(Stream,R.IMA_Bits_Per_Sample);
                     Audio_U16'Read(Stream,R.IMA_CB_Size);
                     Audio_U16'Read(Stream,R.IMA_Samps_Per_Block);
                     Seek_Next(Audio_Stream(Stream),Chunk_Offset,H);
                     return R;
                  end;
               when others =>
                  raise Unsupported_Format;
            end case;
         end;
      elsif H.Chunk_ID = "fact" then
         declare
            R :      Fact_Chunk;
         begin
            Chunk_Hdr(R) := H;
            Audio_U32'Read(Stream,R.Sample_Length);
            Seek_Next(Audio_Stream(Stream),Chunk_Offset,H);
            return R;
         end;
      elsif H.Chunk_ID = "data" then
         declare
            R :      Data_Chunk;
         begin
            Chunk_Hdr(R) := H;

            R.Offset := Offset(Wave_Stream_Block(Stream.all));
            R.End_Offset := R.Offset + Byte_Offset(R.Chunk_Size);
            
            Seek_Next(Audio_Stream(Stream),Chunk_Offset,H);
            return R;
         end;
      elsif H.Chunk_ID = "DISP" then
         declare
            R :      Disp_Chunk(Integer(H.Chunk_Size)-4);
         begin
            Chunk_Hdr(R) := H;
            Disp_Tag_Type'Read(Stream,R.Disp_Tag);
            String'Read(Stream,R.Info);
            if R.Disp_Tag = Text then
               -- Eliminate the NUL byte(s), if it is present
               for X in reverse R.Info'Range loop
                  if R.Info(X) /= Nul or else X = R.Info'First then
                     declare
                        R2 :     Disp_Chunk(X);
                     begin
                        Chunk_Hdr(R2) := Chunk_Hdr(R);
                        R2.Disp_Tag := R.Disp_Tag;
                        R2.Info := R.Info(R2.Info'Range);
                        Seek_Next(Audio_Stream(Stream),Chunk_Offset,H);
                        return R2;
                     end;
                  end if;
               end loop;
            end if;
            Seek_Next(Audio_Stream(Stream),Chunk_Offset,H);
            return R;
         end;
      end if;
   elsif In_Context = "ROOT.RIFF.LIST(INFO)" then
      declare
         type Fourcc_Array is array(Unsigned_16 range <>) of Fourcc;
         Tab : constant Fourcc_Array(1..23) := (
               "IARL", "IART", "ICMS", "ICMT", "ICOP", "ICRD", "ICRP", "IDIM", "IDPI", "IENG", "IGNR",
               "IKEY", "ILGT", "IMED", "INAM", "IPLT", "IPRD", "ISBJ", "ISFT", "ISHP", "ISRC", "ISRF",
               "ITCH" );
         R :   List_Info_Chunk(Integer(H.Chunk_Size));
      begin
         for X in Tab'Range loop
            if H.Chunk_ID = Tab(X) then
               Chunk_Hdr(R) := H;
               String'Read(Stream,R.Info);
               for Y in reverse R.Info'Range loop
                  if ( R.Info(Y) /= Nul and R.Info(Y) /= ' ' ) or else ( Y = R.Info'First and R.Info(Y) = ' ' ) then
                     declare
                        R2 : List_Info_Chunk(Y);
                     begin
                        Chunk_Hdr(R2) := H;
                        R2.Info := R.Info(R2.Info'Range);
                        Seek_Next(Audio_Stream(Stream),Chunk_Offset,H);
                        return R2;                  -- R2.Info has precise string length
                     end;
                  end if;
               end loop;
               return R;                               -- This should not happen unless the file data is bad
            end if;
         end loop;
      end;
   end if;

   declare
      Junk :   Unsupported_Chunk(Integer(H.Chunk_Size));
   begin
      Chunk_Hdr(Junk) := H;
      String'Read(Stream,Junk.Raw_Bytes);
      Seek_Next(Audio_Stream(Stream),Chunk_Offset,H);
      return Junk;
   end;

   end Chunk_Hdr_Class_Input;

   --------------------------------------------------
   -- Allocate and Clone a Chunk_Hdr'Class Object :
   --------------------------------------------------
   function Clone(Chunk : Chunk_Hdr'Class) return Chunk_Hdr_Access is
   begin
   
      if Chunk'Tag = Riff_Form'Tag then
         declare
            Form :   Riff_Form_Ptr := new Riff_Form;
         begin
            Form.all := Riff_Form(Chunk);
            return Chunk_Hdr_Access(Form);
         end;
      elsif Chunk'Tag = Format_Chunk'Tag then
         declare
            Fmt_Type :   Wave_Format_Type := Format_Chunk(Chunk).Format_Tag;
            Num_Coef :   Audio_U16 := Format_Chunk(Chunk).Num_Coef;
            Format :     Format_Chunk_Ptr := new Format_Chunk(Fmt_Type,Num_Coef);
         begin
            Format.all := Format_Chunk(Chunk);
         return Chunk_Hdr_Access(Format);
         end;
      elsif Chunk'Tag = Fact_Chunk'Tag then
         declare
            Fact :   Fact_Chunk_Ptr := new Fact_Chunk;
         begin
            Fact.all := Fact_Chunk(Chunk);
            return Chunk_Hdr_Access(Fact);
         end;
      elsif Chunk'Tag = Data_Chunk'Tag then
         declare
            Data :   Data_Chunk_Ptr := new Data_Chunk;
         begin
            Data.all := Data_Chunk(Chunk);
            return Chunk_Hdr_Access(Data);
         end;
      elsif Chunk'Tag = List_Chunk'Tag then
         declare
            List :   List_Chunk_Ptr := new List_Chunk;
         begin
            List.all := List_Chunk(Chunk);
            return Chunk_Hdr_Access(List);
         end;
      elsif Chunk'Tag = List_Info_Chunk'Tag then
         declare
            List_Info : List_Info_Chunk_Ptr := new List_Info_Chunk(List_Info_Chunk(Chunk).N);
         begin
            List_Info.all := List_Info_Chunk(Chunk);
            return Chunk_Hdr_Access(List_Info);
         end;
      elsif Chunk'Tag = Disp_Chunk'Tag then
         declare
            Disp :   Disp_Chunk_Ptr := new Disp_Chunk(Disp_Chunk(Chunk).N);
         begin
            Disp.all := Disp_Chunk(Chunk);
            return Chunk_Hdr_Access(Disp);
         end;
      elsif Chunk'Tag = Unsupported_Chunk'Tag then
         declare
            Junk :   Unsupported_Chunk_Ptr := new Unsupported_Chunk(Unsupported_Chunk(Chunk).N);
         begin
            Junk.all := Unsupported_Chunk(Chunk);
            return Chunk_Hdr_Access(Junk);
         end;
      else
         raise Program_Error;
      end if;

   end Clone;

   --------------------------------------------------
   -- Initialize the Wave_CB :
   --------------------------------------------------
   procedure Initialize(CB : in out Wave_CB) is
   begin

      CB.Chunk_Context := Dyn("");    -- Initialize with a root chunk context
      Open_Context(CB.Strm.Tree'Access,CB.Strm.Tree_Context);

   end Initialize;

   --------------------------------------------------
   -- Finalize the Wave_CB :
   --------------------------------------------------
   procedure Finalize(CB : in out Wave_CB) is
   begin

      if Is_Open(CB.Strm.File) then       -- Check if our Ada.Streams.Stream_IO.File_Type is still open..
         Close_Context(CB.Strm.Tree_Context);

         -- Put_Line("DUMPING.. (CLOSE)");
         -- Dump(CB.Strm.Tree'Access);
         -- Put_Line("END DUMP.");

         Close(CB.Strm.File);            -- .. then close it.
         Close(CB.Strm.Buffer);          -- Close buffer stream
      end if;

   end Finalize;

   --------------------------------------------------
   -- Open a Wave Stream :
   --------------------------------------------------
   procedure Open(Strm : in out Wave_Stream_Type; Mode : Wave_Mode; Name : String)
   is
      function Int_To_String4 is new Ada.Unchecked_Conversion(Audio_I32,String_4);

      type String_4 is new String(1..4);

      type Riff_Header is
         record
            RIFF :      String_4;                               -- "RIFF"
            Size :      Unsigned_32;    
            WAVE :      Unsigned_32;                            -- "WAVE"
         end record;

      Little_Endian :   constant Unsigned_32 := 16#45564157#;   -- Magic # for Little Endian machine for "WAVE"
      Hdr :             Riff_Header;                            -- Read with endian conversions
      Acc_Chunk :       Chunk_Hdr_Access;                       -- Pointer to the current chunk
      End_File :        Boolean := False;
      End_Riff :        Boolean := False;
      
   begin

      Strm := null;                       -- Initialize
      if Mode /= In_Mode then             -- Only In_Mode is supported currenty
         raise Unsupported_Format;
      end if;

      begin
         Strm := new Wave_Stream_Block;              -- Allocate a new Audio Stream

         Open(Strm.File,In_File,Name);               -- Open File_Type
         Strm.Str := Stream(Strm.File);              -- Get the Stream_Access pointer
         Strm.Wave_Str := Strm;                      -- For ease of reference in other places
         Open(Strm.Buffer,256);                      -- Open a Buf_Stream_Type
    
         Add_Context(Strm.all,"ROOT",0,Byte_Offset(Size(Strm.File))); -- Set root file context

         --------------------------------------------------
         -- Read Hdr with no endian conversion :
         --------------------------------------------------
         begin
            Riff_Header'Read(Strm.Str,Hdr);         -- Raw read of chunk header
            Map(Strm.all,Little_Endian,Hdr.WAVE);   -- Map little endian value to host
         exception
            when Mapping_Error =>
               raise Bad_Wave_File;                -- This file is definitely not an AU format file
            when others =>
               raise;
--                raise Bad_Wave_File;                -- Some other bug occurred
         end;

         --------------------------------------------------
         -- Re-read the RIFF Form Header :
         --------------------------------------------------
         Set_Offset(Strm.all,0);                     -- Seek back to the start of the file

         --------------------------------------------------
         -- Read All Chunks :
         --------------------------------------------------
         loop
            End_Riff := False;                      -- Reset flag
            Acc_Chunk := null;                      -- Forget prior object, if any

            begin
               declare
                  Chunk : Chunk_Hdr'Class := Chunk_Hdr'Class'Input(Strm);
               begin
                  Acc_Chunk := Clone(Chunk);          -- Allocate copy and return pointer to chunk
               end;
            exception
               when End_Of_Context =>
                  Ascend(Strm.Tree_Context);                -- Go back up the tree 1 level
                  Attach_Next(Strm.Tree_Context,Right);     -- Next attach on right
                  if Context(Strm.all) = "ROOT.RIFF" then   -- End of RIFF context?
                     End_Riff := True;                      -- Yes, set Flag
                  elsif Context(Strm.all) = "ROOT" then     -- End of File?
                     End_File := True;                      -- Yes, set that indicator
                  end if;
               when others =>
                  raise;                                    -- Bugs?
            end;

            exit when End_File;                             -- Exit loop, if we're at end of file.

            if Acc_Chunk /= null then                       -- Was a chunk read above?
               Attach(Strm.Tree_Context,Acc_Chunk);         -- Yes, attach it to the tree
               if Acc_Chunk'Tag = Riff_Form'Tag or else Acc_Chunk'Tag = List_Chunk'Tag then
                  Attach_Next(Strm.Tree_Context,Child);     -- Attach next node as a child node
               else
                  Attach_Next(Strm.Tree_Context,Right);     -- Attach next node as right node
               end if;
            end if;
         end loop;

      exception
         when others =>                   -- For any exception..
            if Strm /= null then          -- If Strm was allocated..
               Close(Strm);               -- Close and release it..
            end if;
            raise;                        -- Pass it on..
      end;

      --------------------------------------------------
      -- If no errors, check tree for chunk content :
      --------------------------------------------------
      if Strm /= null then
         declare
            type State is ( Need_Riff, Riff_Wave, fmt, data, OK );
            type Dir is ( Right, Down, Up );
      
            S :          State := Need_Riff;
            Chunk :      Chunk_Hdr_Access;
            Go :         Dir := Right;
            Got_Fact :   Boolean := False;
         begin
            Root(Strm.Tree_Context);        -- Reset to the root node of the tree

            loop
               exit when not Is_Current(Strm.Tree_Context);

               Chunk := Current(Strm.Tree_Context);
               Go := Right;

               case S is
                  when Need_Riff | OK =>
                     if Chunk'Tag = Riff_Form'Tag then
                        declare
                           R :  Riff_Form_Ptr := Riff_Form_Ptr(Chunk);
                        begin
                           if R.Form_Type = "WAVE" then
                              S := Riff_Wave;         -- Yes, we got RIFF(WAVE)
                              Go := Down;
                           else
                              Go := Right;
                           end if;
                        end;
                     else
                        Go := Right;
                     end if;
                  when Riff_Wave =>
                     if Chunk'Tag = Format_Chunk'Tag then
                        S := fmt;                       -- Got the expected format chunk
                        if Strm.Format = null then
                           Strm.Format := Format_Chunk_Ptr(Chunk);
                        end if;
                     end if;
                     Go := Right;
                  when fmt =>
                     if Chunk'Tag = Data_Chunk'Tag then
                        S := data;                      -- Got the expected data chunk
                        if Strm.Data = null then
                           Strm.Data := Data_Chunk_Ptr(Chunk);
                        end if;
                     elsif Chunk'Tag = Fact_Chunk'Tag then
                        if not Got_Fact and then Strm.Fact = null then
                           Strm.Fact := Fact_Chunk_Ptr(Chunk);
                           Got_Fact := TRUE;
                        end if;
                     end if;
                     Go := Right;
                  when data =>
                     if Chunk'Tag = Fact_Chunk'Tag then
                        S := OK;                        -- Got everything now
                        Go := Up;
                     
                        if not Got_Fact and then Strm.Fact = null then
                           Strm.Fact := Fact_Chunk_Ptr(Chunk);
                           Got_Fact := TRUE;
                        end if;
                     elsif Is_Right(Strm.Tree_Context) then
                        Go := Right;
                     else
                        S := OK;                        -- We got all but the fact chunk
                        Go := Up;
                        Got_Fact := True;               -- Ignore any following Fact chunks
                     end if;
               end case;

               case Go is
                  when Right =>
                     Right(Strm.Tree_Context);
                  when Down =>
                     Descend(Strm.Tree_Context);
                  when Up =>
                     Ascend(Strm.Tree_Context);
                     Right(Strm.Tree_Context);
               end case;
            end loop;

            if S /= OK and S /= data then
               -- Not a proper wave file
               raise Bad_Wave_File;
            end if;
         exception
            when others =>
               -- Not a proper wave file
               Close(Strm);
               Strm := null;
               raise;
         end;
      end if;

      Locate(Strm.Tree_Context,Chunk_Hdr_Access(Strm.Data));     -- Set Tree Context to start at Data chunk
      Set_Offset(Strm.all,Strm.Data.Offset);                     -- Point to first sample data byte

      --------------------------------------------------
      -- Some formats require that additional information
      -- be provided to the CODEC. The only generic way
      -- to do this without coupling the CODEC to the
      -- implementation specifics of different streams is
      -- to provide this information as the initial part
      -- of the stream. In this way, the CODEC reads the
      -- special parameters off of the stream without
      -- any special function or procedure calls.
      --------------------------------------------------
      case Strm.Format.Format_Tag is
         when Wave_Format_ADPCM =>
            declare
               Buf_Str :    Root_Stream_Access := Stream(Strm.Buffer);
            begin
               Audio_U16'Write(Buf_Str,Strm.Format.Block_Align);       -- Block size
               Audio_U16'Write(Buf_Str,Strm.Format.Bits_Per_ADPCM);    -- Bits per sample
               Audio_U16'Write(Buf_Str,Strm.Format.Samples_Per_Blk);   -- Samples per block
               if Strm.Fact /= null then
                  Audio_U32'Write(Buf_Str,Strm.Fact.Sample_Length);   -- # of samples to play in total
               else
                  Audio_U32'Write(Buf_Str,0);                         -- Zero indicates no Fact was present
               end if;
               Audio_U16'Write(Buf_Str,Strm.Format.Num_Coef);          -- Indicate the # of coefficients
               Coef_Set_Array'Write(Buf_Str,Strm.Format.Coef);         -- Write out Coefficient set
               Strm.Read_Buf := True;                                  -- Indicate that parameter info must be returned first
            end;
         when Wave_Format_IMA_ADPCM =>
            declare
               Buf_Str :    Root_Stream_Access := Stream(Strm.Buffer);
            begin
               Audio_U16'Write(Buf_Str,Strm.Format.Block_Align);           -- Provide CODEC with the input block size (bytes)
               Audio_U16'Write(Buf_Str,Strm.Format.IMA_Bits_Per_Sample);   -- Provide CODEC with the input sample size (bits)
               Audio_U16'Write(Buf_Str,Strm.Format.IMA_Samps_Per_Block);   -- Max samples per block
               Strm.Read_Buf := True;                                      -- Initial reads from the memory buffer
            end;
         when others =>
            null;                                                       -- Many formats require no special parameters
      end case;

   end Open;

   --------------------------------------------------
   -- Close a Wave Stream :
   --------------------------------------------------
   procedure Close(Strm : in out Wave_Stream_Type) is
   begin

      if Strm = null then                          -- Already closed?
         raise Program_Error;
      end if;

      Strm.Str := null;                            -- No longer need it's stream pointer
      if Is_Open(Strm.File) then                   -- Normally, the file should still be open..
         Close(Strm.File);                         -- Close the Ada.Streams.Stream_IO.File_Type
      end if;
      
      if Is_Open(Strm.Buffer) then
         Close(Strm.Buffer);                       -- Close the Buf_Stream
      end if;
      Free(Strm);

   end Close;

   --------------------------------------------------
   -- Return a Stream_Access :
   --------------------------------------------------
   function Stream(Wave_Strm : Wave_Stream_Type) return Audio_Stream is
   begin

      return Audio_Stream(Wave_Strm);

   end Stream;

   --------------------------------------------------
   -- Free based upon Chunk_Hdr_Access Type :
   --------------------------------------------------
   procedure Free(Acc_Ptr : in out Chunk_Hdr_Access) is
   begin

      if Acc_Ptr'Tag = Riff_Form'Tag then
         declare
            Ptr :   Riff_Form_Ptr := Riff_Form_Ptr(Acc_Ptr);
         begin
            Free(Ptr);
         end;
      elsif Acc_Ptr'Tag = Format_Chunk'Tag then
         declare
            Ptr :    Format_Chunk_Ptr := Format_Chunk_Ptr(Acc_Ptr);
         begin
            Free(Ptr);
         end;
      elsif Acc_Ptr'Tag = Fact_Chunk'Tag then
         declare
            Ptr :    Fact_Chunk_Ptr := Fact_Chunk_Ptr(Acc_Ptr);
         begin
            Free(Ptr);
         end;
      elsif Acc_Ptr'Tag = Data_Chunk'Tag then
         declare
            Ptr :    Data_Chunk_Ptr := Data_Chunk_Ptr(Acc_Ptr);
         begin
            Free(Ptr);
         end;
      elsif Acc_Ptr'Tag = List_Chunk'Tag then
         declare
            Ptr :    List_Chunk_Ptr := List_Chunk_Ptr(Acc_Ptr);
         begin
            Free(Ptr);
         end;
      elsif Acc_Ptr'Tag = List_Info_Chunk'Tag then
         declare
            Ptr :    List_Info_Chunk_Ptr := List_Info_Chunk_Ptr(Acc_Ptr);
         begin
            Free(Ptr);
         end;
      elsif Acc_Ptr'Tag = Disp_Chunk'Tag then
         declare
            Ptr :    Disp_Chunk_Ptr := Disp_Chunk_Ptr(Acc_Ptr);
         begin
            Free(Ptr);
         end;
      elsif Acc_Ptr'Tag = Unsupported_Chunk'Tag then
         declare
            Ptr :    Unsupported_Chunk_Ptr := Unsupported_Chunk_Ptr(Acc_Ptr);
         begin
            Free(Ptr);
         end;
      else
         raise Program_Error;
      end if;

      Acc_Ptr := null;

   end Free;

   --------------------------------------------------
   -- Read Procedure for Wave Stream :
   --------------------------------------------------
   procedure Read(Strm : in out Wave_Stream_Block; Item : out Stream_Element_Array; Last : out Stream_Element_Offset) is
   begin

      if Strm.Data = null then
         -- During the Open() call, we simply read directly from the Wave file :
         Read(Strm.File,Item,Last);      -- Read from internal Ada.Streams.Stream_IO.File_Type
         return;
      elsif Strm.Read_Buf then
         if WC.Streams.Buffer.Count(Strm.Buffer) > 0 then
            Read(Strm.Buffer,Item,Last); -- Return special buffered parameter data
            return;                      -- Return the read item!
         else
            Strm.Read_Buf := False;     -- No more special parameter data
         end if;
      end if;

      -- After the Open() succeeds, we only read from the Data Chunk area(s) :
      declare
         Current_Offset :     Byte_Offset := Offset(Strm);
         Bytes_Remaining :    Byte_Offset;
      begin
         if Current_Offset < Strm.Data.End_Offset then
            Bytes_Remaining := Strm.Data.End_Offset - Current_Offset;
            if Bytes_Remaining > Byte_Offset(Item'Length) then
               Bytes_Remaining := Byte_Offset(Item'Length);
            end if;
            Read(Strm.File,Item(Item'First..Item'First + Stream_Element_Offset(Bytes_Remaining) - 1),Last);
         else
            raise Ada.IO_Exceptions.End_Error;
         end if;
      end;

   end Read;
    
   --------------------------------------------------
   -- Write Procedure for the Wave Stream :
   --------------------------------------------------
   procedure Write(Strm : in out Wave_Stream_Block; Item : Stream_Element_Array) is
   begin
      Write(Strm.File,Item);                                      -- Write on internal Ada.Streams.Stream_IO.File_Type
   end Write;
    
   --------------------------------------------------
   -- Return the # of Channels on this Stream :
   --------------------------------------------------
   function Channels(Strm : Wave_Stream_Block) return Audio_Channels is
   begin
      return Audio_Channels(Strm.Format.Channels);
   end Channels;

   --------------------------------------------------
   -- Return the Sample Rate for this Stream :
   --------------------------------------------------
   function Sample_Rate(Strm : Wave_Stream_Block) return Audio_Sample_Rate is
   begin
      return Audio_Sample_Rate(Strm.Format.Sample_Rate);
   end Sample_Rate;

   --------------------------------------------------
   -- Return the Sample Size for this Stream :
   --------------------------------------------------
   function Sample_Size(Strm : Wave_Stream_Block) return Audio_Sample_Size is
   begin

      case Strm.Format.Format_Tag is
         when Wave_Format_PCM =>
            return Audio_Sample_Size(Strm.Format.Bits_Per_PCM);            -- Sample size in bits
         when Wave_Format_ADPCM =>
            return 4;                                   -- Always 4-bits
         when Wave_Format_ALaw | Wave_Format_MULaw =>
            return Audio_Sample_Size(Strm.Format.ULaw_Bits_Per_Sample);    -- Should be 8
         when Wave_Format_IMA_ADPCM =>
            return Audio_Sample_Size(Strm.Format.IMA_Bits_Per_Sample);     -- 2 through 5
         when others =>
            null;
      end case;
      
      if Strm.Fact /= null then
         return Audio_Sample_Size(Strm.Fact.Sample_Length);
      else
         raise Program_Error;
      end if;

   end Sample_Size;
    
   --------------------------------------------------
   -- Return the CODEC format used :
   --------------------------------------------------
   function Sample_Type(Strm : Wave_Stream_Block) return Audio_Sample_Type is
   begin

      case Strm.Format.Format_Tag is
         when Wave_Format_PCM =>
            case Sample_Size(Strm) is
               when 8 =>
                  return CODEC_Linear_U8;
               when 16 =>
                  return CODEC_Linear_16;
               when others =>
                  return CODEC_Unsupported;
            end case;
         when Wave_Format_ADPCM =>
            return CODEC_MS_ADPCM;
         when Wave_Format_ALaw =>
            return CODEC_Alaw_8;
         when Wave_Format_MULaw =>
            return CODEC_Mulaw;
         when Wave_Format_IMA_ADPCM =>
            return CODEC_IMA_ADPCM;
         when others =>
            null;
      end case;
      return CODEC_Unsupported;

   end Sample_Type;

   --------------------------------------------------
   -- Set the # of Channels being used :
   --------------------------------------------------
   procedure Set_Channels(Strm : in out Wave_Stream_Block; Channels : Audio_Channels) is
   begin
      null;
   end Set_Channels;
    
   --------------------------------------------------
   -- Set the Sample Rate being used :
   --------------------------------------------------
   procedure Set_Sample_Rate(Strm : in out Wave_Stream_Block; Sample_Rate : Audio_Sample_Rate) is
   begin
      null;
   end Set_Sample_Rate;
    
   --------------------------------------------------
   -- Set the Sample Size being Used :
   --------------------------------------------------
   procedure Set_Sample_Size(Strm : in out Wave_Stream_Block; Sample_Size : Audio_Sample_Size) is
   begin
      null;
   end Set_Sample_Size;

   --------------------------------------------------
   -- Return the Zero Based Offset :
   --------------------------------------------------
   function Offset(Strm : Wave_Stream_Block) return Byte_Offset is
   begin
      return Byte_Offset( Index(Strm.File) - 1 );
   end Offset;
    
   --------------------------------------------------
   -- Return the # of Items Waiting to be Read :
   --------------------------------------------------
   function In_Items(Strm : Wave_Stream_Block; Byte_Size : Positive) return Positive
   is
      Bytes_Remaining :    Byte_Offset := 0;
      Items_Remaining :    Byte_Offset := 0;
   begin

      if Strm.Data = null then
         return 1;               -- Try for one
      end if;

      Bytes_Remaining := Strm.Data.End_Offset - Offset(Strm);
      if Byte_Size <= 32 and then Bytes_Remaining > 4096 then
         Bytes_Remaining := 4096; -- Keep memory requirements modest
      end if;

      Items_Remaining := Bytes_Remaining / Byte_Offset(Byte_Size);
      if Items_Remaining < 1 then
         return 1;               -- Try for one
      end if;

      return Positive( Items_Remaining );  -- Safe to read this many items without EOF
   end In_Items;

   --------------------------------------------------
   -- Seek to a Zero Based Offset :
   --------------------------------------------------
   procedure Set_Offset(Strm : in out Wave_Stream_Block; New_Offset : Byte_Offset) is
      New_Index : Positive_Count := Positive_Count( New_Offset + 1 );
   begin
      Set_Index(Strm.File,New_Index);
   end Set_Offset;

   --------------------------------------------------
   -- Return the current context of the stream :
   --------------------------------------------------
   function Context(Strm : Wave_Stream_Block) return Dyn_String is
   begin
      return Strm.CB.Chunk_Context;
   end Context;
    
   --------------------------------------------------
   -- Start a new RIFF Context :
   --------------------------------------------------
   function Start_Context(Strm : Wave_Stream_Block) return Byte_Offset is
   begin
      return Strm.CB.File_Context(Strm.CB.Stack_Pointer).Start_Offset;
   end Start_Context;

   --------------------------------------------------
   -- End the current RIFF Context :
   --------------------------------------------------
   function End_Context(Strm : Wave_Stream_Block) return Byte_Offset is
   begin
      return Strm.CB.File_Context(Strm.CB.Stack_Pointer).End_Offset;
   end End_Context;

   --------------------------------------------------
   -- Append to the current context :
   --------------------------------------------------
   procedure Add_Context(Strm : in out Wave_Stream_Block; New_Context : String; Starting, Ending : Byte_Offset) is
   begin

      if Starting > Ending then
         raise Program_Error;
      end if;

      if Context(Strm) = "" then
         Strm.CB.Chunk_Context := Dyn( New_Context );
      else
         Strm.CB.Chunk_Context := Dyn( To_String(Strm.CB.Chunk_Context) & "." & New_Context );
      end if;

      if Strm.CB.Chunk_Context /= "ROOT" then                     -- Don't increment the first time
         Strm.CB.Stack_Pointer := Strm.CB.Stack_Pointer + 1;
      end if;
      Strm.CB.File_Context(Strm.CB.Stack_Pointer).Start_Offset := Starting;
      Strm.CB.File_Context(Strm.CB.Stack_Pointer).End_Offset := Ending;

   end Add_Context;
    
   --------------------------------------------------
   -- Remove (pop) the current context :
   --------------------------------------------------
   procedure Pop_Context(Strm : in out Wave_Stream_Block)
   is
      S :  String(1..Length(Strm.CB.Chunk_Context)) := To_String(Strm.CB.Chunk_Context);
   begin

      if Length(Strm.CB.Chunk_Context) < 1 then
         raise Program_Error;
      end if;

      for X in reverse S'Range loop
         if S(X) = '.' then
            declare
               S2 : String(S'First..X-1);
            begin
               S2 := S(S'First..X-1);
               Strm.CB.Chunk_Context := Dyn(S2);
               exit;
            end;
         end if;
      end loop;
      Strm.CB.Stack_Pointer := Strm.CB.Stack_Pointer - 1;     -- Pop the file context

   end Pop_Context;

   --------------------------------------------------
   -- Read in Wave_Format_Type from a Stream :
   --------------------------------------------------
   procedure Read_Wave_Format_Type(Stream : access Root_Stream_Type'Class; Item : out Wave_Format_Type)
   is
      function From_U16 is new Ada.Unchecked_Conversion(Audio_U16,Wave_Format_Type);
      U : Audio_U16;
   begin

      Audio_U16'Read(Stream,U);
      Item := From_U16(U);

   end Read_Wave_Format_Type;

   --------------------------------------------------
   -- Read in Disp_Tag_Type from a Stream :
   --------------------------------------------------
   procedure Read_Disp_Tag_Type(Stream : access Root_Stream_Type'Class; Item : out Disp_Tag_Type)
   is
      function From_U32 is new Ada.Unchecked_Conversion(Audio_U32,Disp_Tag_Type);
      U : Audio_U32;
   begin

      Audio_U32'Read(Stream,U);
      Item := From_U32(U);

   end Read_Disp_Tag_Type;

end WC.Streams.Wave;
