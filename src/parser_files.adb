------------------------------------------------------------------------------
-- Files Package Parser                                                     --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2026 Free Software Foundation              --
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
     pegasoft.strings,
     world,
     symbol_table,
     message_strings,
     value_conversion,
     scanner.calendar,
     scanner.communications,
     parser,
     parser_cal;
use  gnat.io_aux,
     gnat.os_lib,
     ada.strings.unbounded,
     spar_os,
     pegasoft.strings,
     world,
     symbol_table,
     message_strings,
     value_conversion,
     scanner,
     scanner.calendar,
     scanner.communications,
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


-----------------------------------------------------------------------------
--  FILE EXISTS                                           (built-in function)
--
-- AdaScript Syntax: files.exists( path );
--       Ada Target: GNAT.IO_Aux.File_Exists
--   GNAT Spec File: g-io_aux.ads
--   SparForte Docs: doc/pkg_files.html#files.exists
-----------------------------------------------------------------------------

procedure ParseFileExists( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_exists_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_exists_t, fileExpr ) then
     result := storage'( to_spar_boolean( File_Exists( to_string( fileExpr.value ) ) ),
        noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseFileExists;

-----------------------------------------------------------------------------
--  IS ABSOLUTE PATH                                      (built-in function)
--
-- AdaScript Syntax: files.is_absolute_path( path );
--       Ada Target: GNAT.OS_Lib.Is_Absolute_Path
--   GNAT Spec File: g-os_lib.ads (renames s-os_lib.ads)
--   SparForte Docs: doc/pkg_files.html#files.is_absolute_path
-----------------------------------------------------------------------------

procedure ParseIsAbsolutePath( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_absolute_path_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_is_absolute_path_t, fileExpr ) then
        result := storage'( to_spar_boolean( Is_Absolute_Path( to_string( fileExpr.value ) ) ),
           noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseIsAbsolutePath;

-----------------------------------------------------------------------------
--  IS REGULAR FILE                                       (built-in function)
--
-- AdaScript Syntax: files.is_regular_file( path );
--       Ada Target: GNAT.OS_Lib.Is_Regular_File
--   GNAT Spec File: g-os_lib.ads (renames s-os_lib.ads)
--   SparForte Docs: doc/pkg_files.html#files.is_regular_file
-----------------------------------------------------------------------------

procedure ParseIsRegularFile( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_regular_file_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_is_regular_file_t, fileExpr ) then
        result := storage'( to_spar_boolean( Is_Regular_File( to_string( fileExpr.value ) ) ),
           noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseIsRegularFile;

-----------------------------------------------------------------------------
--  IS DIRECTORY                                          (built-in function)
--
-- AdaScript Syntax: files.is_directory( path );
--       Ada Target: GNAT.OS_Lib.Is_Directory
--   GNAT Spec File: g-os_lib.ads (renames s-os_lib.ads)
--   SparForte Docs: doc/pkg_files.html#files.is_directory
-----------------------------------------------------------------------------

procedure ParseIsDirectory( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_directory_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_is_directory_t, fileExpr ) then
        result := storage'( to_spar_boolean( Is_Directory( to_string( fileExpr.value ) ) ),
           noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseIsDirectory;

-----------------------------------------------------------------------------
--  IS WRITABLE FILE                                      (built-in function)
--
-- AdaScript Syntax: files.is_writable_file( path );
--       Ada Target: GNAT.OS_Lib.Is_Writable_File / Is_Regular_File
--   GNAT Spec File: g-os_lib.ads (renames s-os_lib.ads)
--   SparForte Docs: doc/pkg_files.html#files.is_writable_file
-----------------------------------------------------------------------------

procedure ParseIsWritableFile( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_writable_file_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_is_writable_file_t, fileExpr ) then
        result := storage'( to_spar_boolean( Is_Writable_File( to_string( fileExpr.value ) )
          and Is_Regular_File( to_string( fileExpr.value ) ) ), noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseIsWritableFile;

-----------------------------------------------------------------------------
--  IS WRITABLE                                           (built-in function)
--
-- AdaScript Syntax: files.is_writable( path );
--       Ada Target: GNAT.OS_Lib.Is_Writable_File
--   GNAT Spec File: g-os_lib.ads (renames s-os_lib.ads)
--   SparForte Docs: doc/pkg_files.html#files.is_writable
-----------------------------------------------------------------------------

procedure ParseIsWritable( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_writable_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_is_writable_t, fileExpr ) then
        result := storage'( to_spar_boolean( Is_Writable_File( to_string( fileExpr.value ) ) ),
           noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseIsWritable;

-----------------------------------------------------------------------------
--  IS EXECUTABLE FILE                                    (built-in function)
--
-- AdaScript Syntax: files.is_executable_file( path );
--       Ada Target: N/A
--   GNAT Spec File: g-os_lib.ads (renames s-os_lib.ads)
--   SparForte Docs: doc/pkg_files.html#files.is_executable_file
-----------------------------------------------------------------------------

procedure ParseIsExecutableFile( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_executable_file_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_is_executable_file_t, fileExpr ) then
        result := storage'( to_spar_boolean( C_Is_Executable_File( to_string( fileExpr.value ) & ASCII.NUL ) ),
           noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseIsExecutableFile;

-----------------------------------------------------------------------------
--  IS EXECUTABLE                                         (built-in function)
--
-- AdaScript Syntax: files.is_executable( path );
--       Ada Target: N/A
--   GNAT Spec File: g-os_lib.ads (renames s-os_lib.ads)
--   SparForte Docs: doc/pkg_files.html#files.is_executable
-----------------------------------------------------------------------------

procedure ParseIsExecutable( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_executable_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_is_executable_t, fileExpr ) then
        result := storage'( to_spar_boolean( C_Is_Executable( to_string( fileExpr.value ) & ASCII.NUL ) ),
           noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseIsExecutable;

-----------------------------------------------------------------------------
--  IS READABLE FILE                                      (built-in function)
--
-- AdaScript Syntax: files.is_readable_file( path );
--       Ada Target: N/A
--   GNAT Spec File: g-os_lib.ads (renames s-os_lib.ads)
--   SparForte Docs: doc/pkg_files.html#files.is_readable_file
-----------------------------------------------------------------------------

procedure ParseIsReadableFile( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_readable_file_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_is_readable_file_t, fileExpr ) then
        result := storage'( to_spar_boolean( C_Is_Readable_File( to_string( fileExpr.value ) & ASCII.NUL ) ),
           noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseIsReadableFile;

-----------------------------------------------------------------------------
--  IS READABLE                                           (built-in function)
--
-- AdaScript Syntax: files.is_readable( path );
--       Ada Target: N/A
--   GNAT Spec File: g-os_lib.ads (renames s-os_lib.ads)
--   SparForte Docs: doc/pkg_files.html#files.is_readable
-----------------------------------------------------------------------------

procedure ParseIsReadable( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_readable_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_is_readable_t, fileExpr ) then
        result := storage'( to_spar_boolean( C_Is_Readable( to_string( fileExpr.value ) & ASCII.NUL ) ),
            noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseIsReadable;

-----------------------------------------------------------------------------
--  FILE SIZE                                             (built-in function)
--
-- AdaScript Syntax: files.size( path );
--       Ada Target: N/A
--   GNAT Spec File: g-os_lib.ads (renames s-os_lib.ads)
--   SparForte Docs: doc/pkg_files.html#files.size
-----------------------------------------------------------------------------

procedure ParseFileSize( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
  filesize  : long_integer;
begin
  kind := long_integer_t;
  expect( files_size_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_size_t, fileExpr ) then
        filesize := C_File_Length( to_string( fileExpr.value ) & ASCII.NUL );
        if filesize >= 0 then
           result := storage'( to_unbounded_string( long_integer'image( filesize ) ),
              noMetaLabel, fileExpr.policyMetaLabels );
        else
           err( pl( "unable to get file size: " & OSError( spar_os.C_errno ) ) );
        end if;
     end if;
  end if;
end ParseFileSize;

-----------------------------------------------------------------------------
--  BASENAME                                              (built-in function)
--
-- AdaScript Syntax: files.basename( path );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_files.html#files.basename
--          Compare: directory_operations.base_name
-----------------------------------------------------------------------------

procedure ParseBasename( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := string_t;
  expect( files_basename_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_basename_t, fileExpr ) then
        result := storage'( basename( fileExpr.value ), noMetaLabel, fileExpr.policyMetaLabels );
     end if;
  end if;
end ParseBasename;

-----------------------------------------------------------------------------
--  DIRNAME                                               (built-in function)
--
-- AdaScript Syntax: files.dirname( path );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_files.html#files.basename
--          Compare: directory_operations.dirname
-----------------------------------------------------------------------------

procedure ParseDirname( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := string_t;
  expect( files_dirname_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_dirname_t, fileExpr ) then
        result := storage'( dirname( fileExpr.value ), noMetaLabel, fileExpr.policyMetaLabels );
     end if;
  end if;
end ParseDirname;

-----------------------------------------------------------------------------
--  IS WAITING FILE                                        (built-in function)
--
-- AdaScript Syntax: files.is_waiting_file( path );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_files.html#files.is_waiting_file
-----------------------------------------------------------------------------

procedure ParseIsWaitingFile( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
begin
  kind := boolean_t;
  expect( files_is_waiting_file_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_is_waiting_file_t, fileExpr ) then
        result := storage'( to_spar_boolean( C_Is_Waiting_File( to_string( fileExpr.value ) & ASCII.NUL ) ),
           noMetaLabel, noMetaLabels );
     end if;
  end if;
end ParseIsWaitingFile;

-----------------------------------------------------------------------------
--  LAST MODIFIED                                         (built-in function)
--
-- AdaScript Syntax: files.last_modified( path );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_files.html#files.last_modified
-----------------------------------------------------------------------------

procedure ParseFileLastModified( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
  year, month, day, seconds : integer;
begin
  kind := cal_time_t;
  expect( files_last_modified_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_last_modified_t, fileExpr ) then
        C_File_Modify_Time( to_string( fileExpr.value ) & ASCII.NUL, year, month, day, seconds );
        if year >= 0 then
           begin -- exception is possible but very unlikely
              result := storage'( to_unbounded_string( time'image( time_of( year, month, day, day_duration( seconds ) ) ) ),
                 noMetaLabel, fileExpr.policyMetaLabels );
           exception when others =>
              err( +"execption raised when getting file modified time" );
           end;
        else
           err( pl( "unable to get file modify time: " & OSError( spar_os.C_errno ) ) );
        end if;
     end if;
  end if;
end ParseFileLastModified;

-----------------------------------------------------------------------------
--  LAST MODIFIED                                         (built-in function)
--
-- AdaScript Syntax: files.last_changed( path );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_files.html#files.last_changed
-----------------------------------------------------------------------------

procedure ParseFileLastChanged( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
  year, month, day, seconds : integer;
begin
  kind := cal_time_t;
  expect( files_last_changed_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_last_changed_t, fileExpr ) then
        C_File_Change_Time( to_string( fileExpr.value ) & ASCII.NUL, year, month, day, seconds );
        if year >= 0 then
           begin -- exception is possible but very unlikely
              result := storage'( to_unbounded_string( time'image( time_of( year, month, day, day_duration( seconds ) ) ) ),
                 noMetaLabel, fileExpr.policyMetaLabels );
           exception when others =>
              err( +"execption raised when getting file modified time" );
           end;
        else
           err( pl( "unable to get file change time: " & OSError( spar_os.C_errno ) ) );
        end if;
     end if;
  end if;
end ParseFileLastChanged;

-----------------------------------------------------------------------------
--  LAST ACCESSED                                         (built-in function)
--
-- AdaScript Syntax: files.last_accessed( path );
--       Ada Target: N/A
--   GNAT Spec File: N/A
--   SparForte Docs: doc/pkg_files.html#files.last_accessed
-----------------------------------------------------------------------------

procedure ParseFileLastAccessed( result : out storage; kind : out identifier ) is
  fileExpr  : storage;
  file_type : identifier;
  year, month, day, seconds : integer;
begin
  kind := cal_time_t;
  expect( files_last_accessed_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, file_type );
  if baseTypesOK( file_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if metaLabelOk( files_last_accessed_t, fileExpr ) then
        C_File_Access_Time( to_string( fileExpr.value ) & ASCII.NUL, year, month, day, seconds );
        if year >= 0 then
           begin -- exception is possible but very unlikely
              result := storage'( to_unbounded_string( time'image( time_of( year, month, day, day_duration( seconds ) ) ) ),
                 noMetaLabel, fileExpr.policyMetaLabels );
           exception when others =>
              err( +"execption raised when getting file modified time" );
           end;
        else
           err( pl( "unable to get file access time: " & OSError( spar_os.C_errno ) ) );
        end if;
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
