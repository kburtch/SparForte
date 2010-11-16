-- $Id: main.ads,v 1.2 2005/02/11 02:59:35 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

package Main is

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

   procedure Program;               -- The main program

private
   CVSID_S : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/main/main.ads,v 1.2 2005/02/11 02:59:35 ken Exp $";
end Main;
