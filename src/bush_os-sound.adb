-- $Id: bush_os-sound.adb,v 1.2 2005/02/11 02:59:21 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License
-- Modified by K Burtch

with Ada.Text_IO, Ada.Command_Line, Ada.Characters.Handling, Ada.Characters.Latin_1;
with Ada.IO_Exceptions;
with Gnat.Command_Line;

with BC.Containers;
with BC.Containers.Lists;
with BC.Containers.Lists.Single;

with WC.Host, WC.Host.Glue;
with WC.Global_Heap;
with WC.Pathname;
with WC.Streams.Audio;
with WC.Streams.DSP;
with WC.Streams.AU;
with WC.Streams.Wave;
with WC.Streams.Codec;

use Ada.Characters.Handling, Ada.Characters.Latin_1;
use WC.Streams.Audio;
use WC.Streams.DSP;
use WC.Streams.AU;
use WC.Streams.Wave;
use WC.Streams.Codec;

with world, scanner, parser_aux;
use  world, scanner, parser_aux;

package body bush_os.sound is

   Default_CDROM_Path : constant string := "/dev/cdrom";

   cdrom_fd     : aFileDescriptor := -1;

   Default_DSP_Path : constant String := "/dev/dsp";

   Release : constant String := "ADAVOX Release 0.51, Warren W. Gay VE3WWG";

   type String_Ptr is access all String;

   type Source_Type is (
      No_Source,                    -- No source has been specified
      AU_File,                      -- *.au file
      Wave_File                     -- *.wav file
   );

   type Target_Type is (
      Device_DSP                    -- /dev/dsp
   );

   type Source_Request is
      record
         Format :       Source_Type;   -- The type of the sound file
         Pathname :     String_Ptr;    -- The pathname of the sound file
      end record;

   package Containers is new BC.Containers(Item => Source_Request);
   package Lists is new Containers.Lists;
   package SL is new Lists.Single(Storage_Manager => WC.Global_Heap.Pool,Storage => WC.Global_Heap.Storage);
   use Containers, Lists, SL;

   RC :        Ada.Command_Line.Exit_Status := 0;
   Opt_D :     String_Ptr;          -- (-d /dev/dsp) DSP device
   Opt_R :     Integer := Integer'First; -- (-R priority) Default: use low priority

   Play_List : List;

   --------------------------------------------------
   -- REALTIME SCHEDULING (IF ANY)
   --------------------------------------------------
   procedure Real_Time_Scheduling(Priority : Integer) is
      use WC.Host, WC.Host.Glue, Ada.Text_IO;
      Z : Errno_Type := ENOERR;
   begin

      if Have_Real_Time then
         declare
            Hi, Lo : Integer;
            P :      Integer := Priority;
         begin
            Hi := Get_Scheduler_Priority(Sched_FIFO,True);
            Lo := Get_Scheduler_Priority(Sched_FIFO,False);

            if P < Lo then
               P := Lo + 1;
            end if;

            if P > Hi then
               P := Hi;
            end if;

            Z := Set_Scheduler_Policy(Sched_FIFO,P);

            -- Report the error if not WC.host.EPERM
            if Z /= ENOERR and Z /= WC.host.EPERM then
               Put(Standard_Error,"WARNING: ");
               Put(Strerror(Z));
               Put(": sched_setscheduler(0,FIFO,");
               Put(Integer'Image(P));
               Put_Line(")");
            end if;
         end;

      end if;

   end Real_Time_Scheduling;

   --------------------------------------------------
   -- PLAY         
   --------------------------------------------------

   procedure Play( soundFile : unbounded_string; priority : integer := 0) is
      use Ada.Text_IO;
      use Ada.Command_Line;
      use GNAT.Command_Line;

      --Optch :  Character;
      Req :    Source_Request;

   begin
      if priority > 0 then
         Opt_R := priority;
      end if;
      if trace then
         put_trace( "Sound priority " & Opt_R'img );
      end if;
 
      ------------------------------
      -- Set default DSP path, if necessary
      ------------------------------
      if Opt_D = null then
         Opt_D := new String(1..Default_DSP_Path'Length);
         Opt_D.all := Default_DSP_Path;
      end if;
 
      if trace and boolean(verboseOpt) then
         Put_Trace(Release);
      end if;

-- this was a loop but now only one song at a time.
-- put the loop back in to queue songs

      declare
         use WC.Pathname;
         Arg :    String := to_string( soundFile );
         Suf :    String := To_Lower(Suffix(Arg,No_Exceptions));
      begin
         
         --exit when Arg = "";

         if Suf = "au" then
            Req.Format := AU_File;
            Req.Pathname := new String(1..Arg'Length);
            Req.Pathname.all := Arg;
            Append(Play_List,Req);
         elsif Suf = "wav" then
            Req.Format := Wave_File;
            Req.Pathname := new String(1..Arg'Length);
            Req.Pathname.all := Arg;
            Append(Play_List,Req);
         else
            err( "wavplay: Unknown sound file type: " & Arg);
            RC := 2;
         end if;
      end;

      if RC /= 0 then
         return;
      end if;

      ------------------------------
      -- Set realtime priority
      ------------------------------
      Real_Time_Scheduling(Opt_R);  -- Set realtime priority if scheduling enabled
      WC.Host.Squash_Root;          -- Eliminate root if we are running setuid

      ------------------------------
      -- Now iterate through the requests
      ------------------------------
      declare
         Iter :      Iterator'Class := New_Iterator(Play_List);

         DSP :       DSP_Stream_Type;
         Au :        Au_Stream_Type;
         Wav :       Wave_Stream_Type;

         In_Strm :   Audio_Stream;
         Out_Strm :  Audio_Stream;

         Request :   Source_Request;
      begin

         Reset(Iter);
         loop
            exit when Is_Done(Iter);

            ------------------------------
            -- Open output stream
            ------------------------------
            begin
               Open(DSP,Out_Mode,Opt_D.all);
               Out_Strm := Stream(DSP);
            exception
               when WC.Streams.DSP.IO_ERROR =>
                  err( "wavplay: Unable to open sound device: " & Opt_D.all);
                  RC := 4;
                  exit;
            end;

            ------------------------------
            -- Determine Request
            ------------------------------
            Request := Current_Item(Iter);

            ------------------------------
            -- Open Input Stream
            ------------------------------
            begin
               case Request.Format is
                  when AU_File =>
                     Open(Au,In_Mode,Request.Pathname.all);
                     In_Strm := Stream(Au);
                  when Wave_File =>
                     Open(Wav,In_Mode,Request.Pathname.all);
                     In_Strm := Stream(Wav);
                  when others =>
                     err( "wavplay: internal error: unable to handle sound format" );
                     raise Program_Error;
               end case;
            exception
               when ADA.IO_Exceptions.Name_Error =>
                  err( "wavplay: Unable to open sound file: " & Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.Wave.Unsupported_Format | WC.Streams.AU.Unsupported_Format =>
                  err( "wavplay: Unsupported compression scheme: " & Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.Wave.Not_Riff_Format =>
                  err( "wavplay: This is not a RIFF file: " & Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.Wave.Not_Riff_Wave =>
                  err( "wavplay: This is not a RIFF(WAVE) file: " & Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.Wave.Bad_Wave_File | WC.Streams.Wave.End_File =>
                  err( "wavplay: File is corrupt or incomplete: " & Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.Wave.Seek_Error =>
                  err( "wavplay: Seek Error: " & Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.AU.Not_AU_Format =>
                  err( "wavplay: Not a *.au format file: " & Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.AU.Bad_AU_Format | WC.Streams.AU.Bad_Data_Loc =>
                  err( "wavplay: Corrupt *.au format file: " & Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when others =>
                  err( "wavplay: *** THE BUDGIE DIED ***");
                  raise;            -- This category of exceptions represents possible bugs
            end;

            ------------------------------
            -- Play Request
            ------------------------------
            if trace then
               Put_Trace("File        : " & Request.Pathname.all);
            end if;

            ------------------------------
            -- Report Parameters if verbose
            ------------------------------
            if trace then
               declare
                  Chans :        Audio_Channels;      -- # of audio channels
                  Samp_Rate :    Audio_Sample_Rate;   -- Sample rate in samples / second
                  Samp_Size :    Audio_Sample_Size;   -- Sample size in bits
                  Samp_Type :    Audio_Sample_Type;   -- Sample Data Format

               begin

                  Samp_Type := Sample_Type(In_Strm.all);
                  Chans     := Channels(In_Strm.all);
                  Samp_Rate := Sample_Rate(In_Strm.all);
                  Samp_Size := Sample_Size(In_Strm.all);

                  Put_Trace("Info: " & Audio_Sample_Type'image(Samp_Type) &
                    ", " & Audio_Channels'image(Chans) & " channels, " &
                    Audio_Sample_Rate'image(Samp_Rate) & " Hz, " &
                    Audio_Sample_Size'image(Samp_Size) & " bits");
               end;
            end if; -- trace

            Pump(In_Strm,Out_Strm);

            ------------------------------
            -- Close input stream
            ------------------------------
            case Request.Format is
               when AU_File =>      Close(Au);
               when Wave_File =>    Close(Wav);
               when others =>       raise Program_Error;
            end case;

            Close(DSP);

            Next(Iter);
         end loop;

      end;

      if RC /= 0 then
         err( "wavplay: Error #" & RC'img );
      end if;

       Clear( Play_List );

   end Play;

procedure PlayCD( altCdPath : unbounded_string ) is
   CDPath       : unbounded_string;
   playinfo     : cdrom_ti;
   ioctl_result : integer;
   dummy        : aDummyParam := 0;
begin
   if cdrom_fd > 0 then
      stopCD;
      return;
   end if;
   if length( altCdPath ) > 0 then
      CDPath := altCdPath;
   else
      CDPath := to_unbounded_string( Default_CDROM_Path );
   end if;
   cdrom_fd := open( to_string( CDPath ) & ASCII.NUL, O_RDONLY+O_NONBLOCK, 0 );
   if cdrom_fd < 0 then
      err( "Error openning CD-ROM drive: " & OSError( C_errno ) );
      StopCD;
   end if;
   ioctl_cdromstart( ioctl_result, cdrom_fd, CDROMSTART, dummy );
   if ioctl_result < 0 then
      err( "Error spinning up the CDROM drive: " & OSError( C_errno ) );
      StopCD;
   end if;
   playinfo.start_track := 1; -- first track
   playinfo.start_index := 0; -- no effect
   playinfo.end_track := 99;   -- final track (inclusive)
   playinfo.end_index := 0;   -- no effect
   ioctl_playtrkind( ioctl_result, cdrom_fd, CDROMPLAYTRKIND, playinfo );
   if ioctl_result < 0 then
      if C_errno = EINVAL then
         err( "CD is not an audio CD" );
      else
         err( "Error starting audio CD music: " & OSError( C_errno ) );
      end if;
      StopCD;
   end if;
end PlayCD;

procedure StopCD is
   dummy        : aDummyParam := 0;
   ioctl_result : integer;
begin
   if cdrom_fd <= 0 then
      err( "CD-ROM drive is not in use" );
      return;
   end if;
   ioctl_cdromstop( ioctl_result, cdrom_fd, CDROMSTOP, dummy );
   if ioctl_result < 0 then
      err( "Error stopping audio CD: " & OSError( C_errno ) );
   end if;
   close( cdrom_fd );
   cdrom_fd := -1;
end StopCD;

procedure Mute is
begin
   err( "Not yet written" );
end Mute;

procedure Unmute is
begin
   err( "Not yet written" );
end Unmute;

end bush_os.sound;

