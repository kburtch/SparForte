------------------------------------------------------------------------------
-- Splashes                                                                 --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2024 Free Software Foundation              --
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

with ada.text_io,
     spar_os;
use ada.text_io,
    spar_os;

package body world.splashes is

-----------------------------------------------------------------------------
--  DISPLAY VERSION SPLASH
--
-- display --version message.  This is located here because it
-- uses term attributes.
-----------------------------------------------------------------------------

procedure displayVersionSplash is
begin
  if isatty( stdout ) = 1 then
     Put( "SparForte version " );
     if released then
        Put_Line( version );
     else
        Put_Line( version & " (Build ID " & buildDate & ')' );
     end if;
     Put_Line( copyright );
     Put_Line( "This is free software; see the source for copying conditions." );
     Put_Line( "There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE." );
     --New_Line;

     --path := to_unbounded_string( globalConfigPath );
     --if not C_is_includable_file( globalConfigPath & ASCII.NUL ) then
     --   path := path & " (no file)";
     --end if;
     --Put_line( "* Global Configuration: " & toEscaped( path ) );

     --path := to_unbounded_string( globalPolicyPath );
     --if not C_is_includable_file( globalPolicyPath & ASCII.NUL ) then
     --   path := path & " (no file)";
     --end if;
     --Put_line( "* Global Policy:        " & toEscaped( path ) );

     --path := to_unbounded_string( globalProfilePath );
     --if not C_is_includable_file( globalProfilePath & ASCII.NUL ) then
     --   path := path & " (no file)";
     --end if;
     --Put_line( "* Global Profile:       " & toEscaped( path ) );
  end if;
end displayVersionSplash;


-----------------------------------------------------------------------------
--  DISPLAY COPYRIGHT SPLASH
--
-- display copyright message.  This is located here because it
-- uses term attributes.  Suppress the message on a login shell
-- or if there is no tty.
-- Some of this is now defined in the world.ads file.
-----------------------------------------------------------------------------

procedure displayCopyrightSplash is
begin
  if isatty( stdout ) = 1 and not isLoginShell then
     Put_Line( "Type ""help"" for help" );
  end if;
end displayCopyrightSplash;

end world.splashes;

