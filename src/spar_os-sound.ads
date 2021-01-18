------------------------------------------------------------------------------
-- GStreamer interface file                                                 --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  This is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

with ada.strings.unbounded;
use  ada.strings.unbounded;

package spar_os.sound is

   -- GSTREAMER interface
   --
   -- g_streamer.c contains basic C function to start gstreamer
   -- and play sounds.  Errors are returned in the gst_error
   -- array.

   -- initialize gstreamer
   procedure startup_gstreamer;
   pragma import( C, startup_gstreamer );

   -- shutdown gstreamer
   procedure shutdown_gstreamer;
   pragma import( C, shutdown_gstreamer );

   -- play the sound file referenced by the uri, 1 = success
   -- and 0 = fail (see gst_error)
   function play_uri( uri : string ) return integer;
   pragma import( C, play_uri );

   -- last error message from gst_error
   gst_error : array(0..255) of character;
   pragma import( C, gst_error );

------------------------------------------------------------------------------

  --procedure Play( soundFile : unbounded_string; priority : integer := 0 );
  -- Play a WAV or AU sound using AdaVox

  procedure PlayCD( altCdPath : unbounded_string );
  -- Play a music CD

  procedure StopCD;
  -- Stop music CD

end spar_os.sound;
