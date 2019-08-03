------------------------------------------------------------------------------
-- Files Package Parser                                                     --
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

--with text_io; use text_io;

with gnat.io_aux,
     gnat.os_lib,
     ada.strings.unbounded,
     spar_os,
     string_util,
     world,
     scanner.calendar,
     parser_aux,
     parser,
     parser_cal;
use  gnat.io_aux,
     gnat.os_lib,
     ada.strings.unbounded,
     spar_os,
     string_util,
     world,
     scanner,
     scanner.calendar,
     parser_aux,
     parser,
     parser_cal;

package body parser_files is

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

--epoch        : time := time_of( 1970, 1, 1, 0.0 );
--secondsInDay : day_duration := day_duration'last;

---------------------------------------------------------
-- PARSE THE FILES PACKAGE
---------------------------------------------------------


procedure ParseFileExists( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.file_exists( path );
  -- Source: GNAT.IO_Aux.File_Exists
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_exists_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( File_Exists( to_string( file_val ) ) );
  end if;
end ParseFileExists;

procedure ParseIsAbsolutePath( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.is_absolute_path( path );
  -- Source: GNAT.OS_Lib.Is_Absolute_Path
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_absolute_path_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( Is_Absolute_Path( to_string( file_val ) ) );
  end if;
end ParseIsAbsolutePath;

procedure ParseIsRegularFile( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.is_regular_file( path );
  -- Source: GNAT.IO_Aux.Is_Regular_File
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_regular_file_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( Is_Regular_File( to_string( file_val ) ) );
  end if;
end ParseIsRegularFile;

procedure ParseIsDirectory( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.is_directory( path );
  -- Source: GNAT.IO_Aux.Is_Directory
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_directory_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( Is_Directory( to_string( file_val ) & ASCII.NUL ) );
  end if;
end ParseIsDirectory;

procedure ParseIsWritableFile( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.is_writable_file( path );
  -- Source: GNAT.IO_Aux.Is_Writable_File + Is_Regular_File
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_writable_file_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( Is_Writable_File( to_string( file_val ) )
          and Is_Regular_File( to_string( file_val ) ) );
  end if;
end ParseIsWritableFile;

procedure ParseIsWritable( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.is_writable( path );
  -- Source: GNAT.IO_Aux.Is_Writable_File
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_writable_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( Is_Writable_File( to_string( file_val ) ) );
  end if;
end ParseIsWritable;

procedure ParseIsExecutableFile( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.is_executable_file( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_executable_file_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( C_Is_Executable_File( to_string( file_val ) & ASCII.NUL ) );
  end if;
end ParseIsExecutableFile;

procedure ParseIsExecutable( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.is_executable( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_executable_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( C_Is_Executable( to_string( file_val ) & ASCII.NUL ) );
  end if;
end ParseIsExecutable;

procedure ParseIsReadableFile( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.is_readable_file( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_readable_file_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( C_Is_Readable_File( to_string( file_val ) & ASCII.NUL ) );
  end if;
end ParseIsReadableFile;

procedure ParseIsReadable( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.is_readable( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_readable_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( C_Is_Readable( to_string( file_val ) & ASCII.NUL ) );
  end if;
end ParseIsReadable;

procedure ParseFileSize( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.size( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
  filesize  : long_integer;
begin
  kind := long_integer_t;
  expect( files_size_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     filesize := C_File_Length( to_string( file_val ) & ASCII.NUL );
     if filesize >= 0 then
        f := to_unbounded_string( long_integer'image( filesize ) );
     else
        err( "unable to get file size: " & OSError( spar_os.C_errno ) );
     end if;
  end if;
end ParseFileSize;

procedure ParseBasename( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.basename( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := string_t;
  expect( files_basename_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := basename( file_val );
  end if;
end ParseBasename;

procedure ParseDirname( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.dirname( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := string_t;
  expect( files_dirname_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := dirname( file_val );
  end if;
end ParseDirname;

procedure ParseIsWaitingFile( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.is_waiting_file( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_waiting_file_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     f := to_bush_boolean( C_Is_Waiting_File( to_string( file_val ) & ASCII.NUL ) );
  end if;
end ParseIsWaitingFile;

procedure ParseFileLastModified( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.last_modified( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
  year, month, day, seconds : integer;
begin
  kind := cal_time_t;
  expect( files_last_modified_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     C_File_Modify_Time( to_string( file_val ) & ASCII.NUL, year, month, day, seconds );
     if year >= 0 then
        begin -- exception is possible but very unlikely
           f := to_unbounded_string( time'image( time_of( year, month, day, day_duration( seconds ) ) ) );
        exception when others =>
           err( "execption raised when getting file modified time" );
        end;
     else
        err( "unable to get file modify time: " & OSError( spar_os.C_errno ) );
     end if;
  end if;
end ParseFileLastModified;

procedure ParseFileLastChanged( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.last_changed( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
  year, month, day, seconds : integer;
begin
  kind := cal_time_t;
  expect( files_last_changed_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     C_File_Change_Time( to_string( file_val ) & ASCII.NUL, year, month, day, seconds );
     if year >= 0 then
        begin -- exception is possible but very unlikely
           f := to_unbounded_string( time'image( time_of( year, month, day, day_duration( seconds ) ) ) );
        exception when others =>
           err( "execption raised when getting file modified time" );
        end;
     else
        err( "unable to get file change time: " & OSError( spar_os.C_errno ) );
     end if;
  end if;
end ParseFileLastChanged;

procedure ParseFileLastAccessed( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: files.last_accessed( path );
  -- Source: N/A
  file_val  : unbounded_string;
  file_type : identifier;
  year, month, day, seconds : integer;
begin
  kind := cal_time_t;
  expect( files_last_accessed_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     C_File_Access_Time( to_string( file_val ) & ASCII.NUL, year, month, day, seconds );
     if year >= 0 then
        begin -- exception is possible but very unlikely
           f := to_unbounded_string( time'image( time_of( year, month, day, day_duration( seconds ) ) ) );
        exception when others =>
           err( "execption raised when getting file modified time" );
        end;
     else
        err( "unable to get file access time: " & OSError( spar_os.C_errno ) );
     end if;
  end if;
end ParseFileLastAccessed;

procedure StartupFiles is
begin
  declareNamespace( "files" );
  declareFunction( files_exists_t, "files.exists", ParseFileExists'access );
  declareFunction( files_is_absolute_path_t, "files.is_absolute_path", ParseIsAbsolutePath'access );
  declareFunction( files_is_regular_file_t, "files.is_regular_file", ParseIsRegularFile'access );
  declareFunction( files_is_directory_t, "files.is_directory", ParseIsDirectory'access );
  declareFunction( files_is_writable_file_t, "files.is_writable_file", ParseIsWritableFile'access );
  declareFunction( files_is_writable_t, "files.is_writable", ParseIsWritable'access );
  declareFunction( files_is_executable_file_t, "files.is_executable_file", ParseIsExecutableFile'access );
  declareFunction( files_is_executable_t, "files.is_executable", ParseIsExecutable'access );
  declareFunction( files_is_readable_file_t, "files.is_readable_file", ParseIsReadableFile'access );
  declareFunction( files_is_readable_t, "files.is_readable", ParseIsReadable'access );
  declareFunction( files_basename_t, "files.basename", ParseBasename'access );
  declareFunction( files_dirname_t, "files.dirname", ParseDirname'access );
  declareFunction( files_is_waiting_file_t, "files.is_waiting_file", ParseIsWaitingFile'access );
  declareFunction( files_size_t, "files.size", ParseFileSize'access );
  declareFunction( files_last_modified_t, "files.last_modified", ParseFileLastModified'access );
  declareFunction( files_last_changed_t, "files.last_changed", ParseFileLastChanged'access );
  declareFunction( files_last_accessed_t, "files.last_accessed", ParseFileLastAccessed'access );
  declareNamespaceClosed( "files" );
end StartupFiles;

procedure ShutdownFiles is
begin
  null;
end ShutdownFiles;

end parser_files;
