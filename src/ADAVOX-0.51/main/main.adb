-- $Id: main.adb,v 1.2 2005/02/11 02:59:35 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

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

package body Main is

   CVSID :     constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/main/main.adb,v 1.2 2005/02/11 02:59:35 ken Exp $";

   package Containers is new BC.Containers(Item => Source_Request);
   package Lists is new Containers.Lists;
   package SL is new Lists.Single(Storage_Manager => WC.Global_Heap.Pool,Storage => WC.Global_Heap.Storage);
   use Containers, Lists, SL;

   RC :        Ada.Command_Line.Exit_Status := 0;
   Opt_H :     Boolean := False;    -- (-h) Help
   Opt_V :     Boolean := False;    -- (-v) Verbose
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

            -- Report the error if not EPERM
            if Z /= ENOERR and Z /= EPERM then
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
   -- COMMAND USAGE
   --------------------------------------------------
   procedure Usage is
      use Ada.Text_IO, Ada.Command_Line;
      use WC, WC.Pathname;

      Cmd :    String := Basename(Command_Name,No_Exceptions);

   begin

      Put_Line(Release);
      New_Line;
      Put("Usage: ");
      Put(Cmd);
      Put_Line(" [-h] [-v] [-a file] [-w file] [-d dsp] [-R p] files...");
      Put_Line("where:");
      Put_Line("   -v            Verbose");
      Put_Line("   -a file       Play file as *.au file");
      Put_Line("   -w file       Play file as *.wav file");
      Put_Line("   -d dsp        Overrides default of /dev/dsp");
      Put_Line("   -R p          Use realtime priority p");
      New_Line;
      Put_Line("   -h            Requests this info.");
      New_Line;
      Put_Line("All remaining files are interpreted according to the");
      Put_Line("file suffix.");
      New_Line;

   end Usage;

   --------------------------------------------------
   -- MAIN PROGRAM
   --------------------------------------------------
   procedure Program is
      use Ada.Text_IO;
      use Ada.Command_Line;
      use GNAT.Command_Line;

      Optch :  Character;
      Req :    Source_Request;

   begin

      ------------------------------
      -- Process Command Line Options
      ------------------------------
      Initialize_Option_Scan(Stop_At_First_Non_Switch => True);

      begin
         loop
            Optch := Getopt("h a: w: v d: R:");

            case Optch is
               when NUL =>
                  exit;
               when 'h' =>
                  Opt_H := True;
               when 'd' =>
                  Opt_D := new String(1..Parameter'Length);
                  Opt_D.all := Parameter;
               when 'v' =>
                  Opt_V := True;
               when 'a' =>
                  Req.Format := AU_File;
                  Req.Pathname := new String(1..Parameter'Length);
                  Req.Pathname.all := Parameter;
                  Append(Play_List,Req);
               when 'w' =>
                  Req.Format := Wave_File;
                  Req.Pathname := new String(1..Parameter'Length);
                  Req.Pathname.all := Parameter;
                  Append(Play_List,Req);
               when 'R' =>
                  begin
                     Opt_R := Integer'Value(Parameter);
                  exception
                     when others =>
                        raise;
                  end;
               when others =>
                  raise Program_Error;
            end case;
         end loop;
      exception
         when Invalid_Switch =>
            RC := 1;
            Put(Standard_Error,"Invalid option: -");
            Put_Line(Standard_Error,Full_Switch);
         when Invalid_Parameter =>
            RC := 1;
            Put(Standard_Error,"Missing argument: -");
            Put_Line(Standard_Error,Full_Switch);
      end;
  
      ------------------------------
      -- Exit if help or errors
      ------------------------------
      if Opt_H or RC /= 0 then
         Usage;
         Set_Exit_Status(RC);
         return;
      end if;

      ------------------------------
      -- Set default DSP path, if necessary
      ------------------------------
      if Opt_D = null then
         Opt_D := new String(1..Default_DSP_Path'Length);
         Opt_D.all := Default_DSP_Path;
      end if;

      ------------------------------
      -- If -v, then show program release
      ------------------------------
      if Opt_V then
         Put_Line(Release);
      end if;

      ------------------------------
      -- Now process all arguments
      ------------------------------
      loop
         declare
            use WC.Pathname;
            Arg :    String := Get_Argument;
            Suf :    String := To_Lower(Suffix(Arg,No_Exceptions));
         begin
            
            exit when Arg = "";

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
               Put(Standard_Error,"Unknown sound file type: ");
               Put_Line(Standard_Error,Arg);
               Put_Line("Use -h to view options for use with files with no suffix.");
               RC := 2;
            end if;
         end;
      end loop;

      if RC /= 0 then
         Set_Exit_Status(RC);    -- Exit if we encountered problems
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
                  Put(Standard_Error,"Unable to open sound device: -d ");
                  Put_Line(Opt_D.all);
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
                     raise Program_Error;
               end case;
            exception
               when ADA.IO_Exceptions.Name_Error =>
                  Put(Standard_Error,"Unable to open file: ");
                  Put_Line(Standard_Error,Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.Wave.Unsupported_Format | WC.Streams.AU.Unsupported_Format =>
                  Put(Standard_Error,"Unsupported compression scheme: ");
                  Put_Line(Standard_Error,Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.Wave.Not_Riff_Format =>
                  Put(Standard_Error,"This is not a RIFF file: ");
                  Put_Line(Standard_Error,Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.Wave.Not_Riff_Wave =>
                  Put(Standard_Error,"This is not a RIFF(WAVE) file: ");
                  Put_Line(Standard_Error,Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.Wave.Bad_Wave_File | WC.Streams.Wave.End_File =>
                  Put(Standard_Error,"File is corrupt or incomplete: ");
                  Put_Line(Standard_Error,Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.Wave.Seek_Error =>
                  Put(Standard_Error,"Seek Error: ");
                  Put_Line(Standard_Error,Request.Pathname.all);
                  Put_Line("This suggests the file may be corrupt, or that the");
                  Put_Line("pathname given is not a seekable device.");
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.AU.Not_AU_Format =>
                  Put(Standard_Error,"Not a *.au format file: ");
                  Put_Line(Standard_Error,Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when WC.Streams.AU.Bad_AU_Format | WC.Streams.AU.Bad_Data_Loc =>
                  Put(Standard_Error,"Corrupt *.au format file: ");
                  Put_Line(Standard_Error,Request.Pathname.all);
                  Close(DSP);
                  RC := 8;
                  exit;
               when others =>
                  Put(Standard_Error,"*** THE BUDGIE DIED ***");
                  raise;            -- This category of exceptions represents possible bugs
            end;

            ------------------------------
            -- Play Request
            ------------------------------
            if Opt_V then
               New_Line;
               Put("File        : " );
               Put_Line(Request.Pathname.all);
            end if;

            ------------------------------
            -- Report Parameters if verbose
            ------------------------------
            if Opt_V then
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

                  Put("Type        : ");
                  Put_Line(Audio_Sample_Type'image(Samp_Type));
                  Put("Channels    :");
                  Put_Line(Audio_Channels'image(Chans));
                  Put("Sample Rate :");
                  Put(Audio_Sample_Rate'image(Samp_Rate));
                  Put_Line(" Hz");
                  Put("Sample Size :");
                  Put(Audio_Sample_Size'image(Samp_Size));
                  Put_Line(" bits");
               end;
            end if;

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

      Set_Exit_Status(RC);

   end Program;

end Main;
