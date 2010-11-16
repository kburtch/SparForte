------------------------------------------------------------------------------
-- BUSH Files Package Parser                                                --
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
-- CVS: $Id: parser_files.ads,v 1.3 2005/08/31 15:10:45 ken Exp $

with ada.strings.unbounded, world;
use  ada.strings.unbounded, world;

package parser_files is

------------------------------------------------------------------------------
-- Files package identifiers
------------------------------------------------------------------------------

files_exists_t           : identifier;
files_is_absolute_path_t : identifier;
files_is_regular_file_t  : identifier;
files_is_directory_t     : identifier;
files_is_readable_file_t : identifier;
files_is_readable_t      : identifier;
files_is_writable_file_t : identifier;
files_is_writable_t      : identifier;
files_is_executable_file_t : identifier;
files_is_executable_t    : identifier;
files_basename_t         : identifier;
files_dirname_t          : identifier;
files_is_waiting_file_t  : identifier;
files_size_t             : identifier;
files_last_modified_t    : identifier;
files_last_changed_t     : identifier;
files_last_accessed_t    : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupFiles;
procedure ShutdownFiles;

------------------------------------------------------------------------------
-- PARSE THE FILES PACKAGE
------------------------------------------------------------------------------

procedure ParseFileExists( f : out unbounded_string );
procedure ParseIsAbsolutePath( f : out unbounded_string );
procedure ParseIsRegularFile( f : out unbounded_string );
procedure ParseIsDirectory( f : out unbounded_string );
procedure ParseIsWritableFile( f : out unbounded_string );
procedure ParseIsWritable( f : out unbounded_string );
procedure ParseIsExecutableFile( f : out unbounded_string );
procedure ParseIsExecutable( f : out unbounded_string );
procedure ParseIsReadableFile( f : out unbounded_string );
procedure ParseIsReadable( f : out unbounded_string );
procedure ParseDirname( f : out unbounded_string );
procedure ParseBasename( f : out unbounded_string );
procedure ParseIsWaitingFile( f : out unbounded_string );
procedure ParseFileSize( f : out unbounded_string );
procedure ParseFileLastModified( f : out unbounded_string );
procedure ParseFileLastChanged( f : out unbounded_string );
procedure ParseFileLastAccessed( f : out unbounded_string );

end parser_files;
