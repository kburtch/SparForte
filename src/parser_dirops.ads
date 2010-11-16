------------------------------------------------------------------------------
-- BUSH Directory_Operations Package Parser                                 --
--                                                                          --
-- Part of BUSH                                                             --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2005 Ken O. Burtch & FSF                 --
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
-- CVS: $Id: parser_dirops.ads,v 1.1 2005/08/31 15:10:45 ken Exp $

with ada.strings.unbounded, world;
use  ada.strings.unbounded, world;

package parser_dirops is


------------------------------------------------------------------------------
-- Directory_Operations package identifiers
------------------------------------------------------------------------------

dirops_dir_name_str_t   : identifier;
dirops_dir_separator_t  : identifier;
dirops_change_dir_t     : identifier;
dirops_make_dir_t       : identifier;
dirops_remove_dir_t     : identifier;
dirops_get_current_dir_t : identifier;
dirops_path_name_t      : identifier;
dirops_dir_name_t       : identifier;
dirops_base_name_t      : identifier;
dirops_file_extension_t : identifier;
dirops_file_name_t      : identifier;
dirops_path_style_t     : identifier;
dirops_path_style_unix_t : identifier;
dirops_path_style_dos_t : identifier;
dirops_path_style_system_default_t : identifier;
dirops_format_pathname_t : identifier;
dirops_env_style_t      : identifier;
dirops_env_style_unix_t : identifier;
dirops_env_style_dos_t  : identifier;
dirops_env_style_both_t : identifier;
dirops_env_style_system_default_t : identifier;
dirops_expand_path_t    : identifier;
-- reading directory list not included yet

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupDirOps;
procedure ShutdownDirOps;

------------------------------------------------------------------------------
-- PARSE THE DIRECTORY_OPERATIONS PACKAGE
------------------------------------------------------------------------------

procedure ParseDirOpsDirSeparator( result : out unbounded_string );
procedure ParseDirOpsChangeDir;
procedure ParseDirOpsMakeDir;
procedure ParseDirOpsRemoveDir;
procedure ParseDirOpsGetCurrentDir( result : out unbounded_string );
procedure ParseDirOpsDirName( result : out unbounded_string );
procedure ParseDirOpsBaseName( result : out unbounded_string );
procedure ParseDirOpsFileExtension( result : out unbounded_string );
procedure ParseDirOpsFileName( result : out unbounded_string );
procedure ParseDirOpsFormatPathname( result : out unbounded_string );
procedure ParseDirOpsExpandPath( result : out unbounded_string );

end parser_dirops;
