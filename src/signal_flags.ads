------------------------------------------------------------------------------
-- SparForte Signal Handling                                                --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
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

package signal_flags is

  -- true if the corresponding signal was raised

  wasSIGCHLD  : boolean := false;
  wasSIGINT   : boolean := false;
  wasSIGTERM  : boolean := false;
  wasSIGWINCH : boolean := false;
  wasSIGPIPE  : boolean := false;

  procedure startSignalFlags;
  procedure shutdownSignalFlags;

  procedure addSigtermSignalFlag;
  -- for interactive shells

end signal_flags;
