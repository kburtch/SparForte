------------------------------------------------------------------------------
-- SparForte Signal Handling                                                --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2019 Free Software Foundation              --
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
-- CVS: $Id: signal_flags.adb,v 1.2 2005/02/11 02:59:31 ken Exp $

with Ada.Text_IO, Gnat.Source_Info, spar_os;
use Ada.Text_IO,  Gnat.Source_Info, spar_os;

package body signal_flags is

  procedure startSignalFlags is
  begin
    if not C_install_sigint_handler( wasSIGINT'address ) then
       put_line( standard_error, Gnat.Source_Info.Source_Location & ": failed to install SIGINT handler" );
    end if;
    if not C_install_sigchld_handler( wasSIGCHLD'address ) then
       put_line( standard_error, Gnat.Source_Info.Source_Location & ": failed to install SIGCHLD handler" );
    end if;
    if not C_install_sigwinch_handler( wasSIGWINCH'address ) then
       put_line( standard_error, Gnat.Source_Info.Source_Location & ": failed to install SIGWINCH handler" );
    end if;
    if not C_install_sigpipe_handler( wasSIGPIPE'address ) then
       put_line( standard_error, Gnat.Source_Info.Source_Location & ": failed to install SIGPIPE handler" );
    end if;
  end startSignalFlags;

  procedure shutdownSignalFlags is
  begin
    null;
  end shutdownSignalFlags;

end signal_flags;
