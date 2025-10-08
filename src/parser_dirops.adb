------------------------------------------------------------------------------
-- Directory_Operations Package Parser                                      --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2025 Free Software Foundation              --
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

--with text_io;use text_io;

with gnat.directory_operations,
    Ada.Directories,
    ada.strings.unbounded,
    world,
    symbol_table,
    message_strings,
    value_conversion,
    scanner,
    scanner.communications,
    scanner_res,
    parser_params;
use gnat.directory_operations,
    Ada.Directories,
    ada.strings.unbounded,
    world,
    symbol_table,
    message_strings,
    value_conversion,
    scanner,
    scanner.communications,
    scanner_res,
    parser_params;

package body parser_dirops is

------------------------------------------------------------------------------
-- Directory_Operations package identifiers
------------------------------------------------------------------------------

dirops_dir_type_t       : identifier;
dirops_absolute_dir_name_str_t   : identifier;
dirops_dir_name_str_t   : identifier;
dirops_dir_separator_t  : identifier;
dirops_change_dir_t     : identifier;
dirops_make_dir_t       : identifier;
dirops_remove_dir_t     : identifier;
dirops_get_current_dir_t : identifier;
--dirops_absolute_path_name_t      : identifier;
dirops_path_name_t      : identifier;
dirops_dir_name_t       : identifier;
dirops_absolute_dir_name_t      : identifier;
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
dirops_open_t           : identifier;
dirops_close_t          : identifier;
dirops_is_open_t        : identifier;
dirops_read_t          : identifier;

-----------------------------------------------------------------------------
--  PARSE DIR OPS DIR SEPARATOR                           (built-in function)
--
-- AdaScript Syntax: c := directory_operations.dir_separator
--       Ada Target: GNAT.Directory_Operations.Dir_Separator
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.dir_separator
-----------------------------------------------------------------------------

procedure ParseDirOpsDirSeparator( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := dirops_dir_separator_t;
begin
  kind := character_t;
  expect( subprogramId );
  if isExecutingCommand then
     begin
       result := storage'( null_unbounded_string & dir_separator, noMetaLabel );
     exception when directory_error =>
       err( +"directory not accessible" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseDirOpsDirSeparator;

-----------------------------------------------------------------------------
--  PARSE DIR OPS CHANGE SEPARATOR                       (built-in procedure)
--
-- AdaScript Syntax: directory_operations.change_dir( p )
--       Ada Target: GNAT.Directory_Operations.Change_Dir
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.change_dir
-----------------------------------------------------------------------------

procedure ParseDirOpsChangeDir is
  expr : storage;
  expr_type: identifier;
  pwdId : identifier;
  oldPwdId : identifier;
  subprogramId : constant identifier := dirops_change_dir_t;
begin
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, expr, expr_type, dirops_dir_name_str_t );

  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          Change_Dir( dir_name_str( to_string( expr.value ) ) );
        exception when directory_error =>
          err( +"directory not accessible" );
        when others =>
          err_exception_raised;
        end;
        -- SparForte 3.0
        -- Ada doesn't do any of this but we'll do it for Bourne shell
        -- compatibility.  If we don't, mixing Change_Dir and the cd command
        -- will corrupt the environment variables.  When changing directories,
        -- we update the current working directory.  This is copied from the cd
        -- builtin.
        current_working_directory := expr.value;
        findident( to_unbounded_string( "PWD" ), pwdId );
        findident( to_unbounded_string( "OLDPWD" ), oldPwdId );
        if pwdId /= eof_t then
           if oldPwdId /= eof_t then
              identifiers( oldPwdId ).store.value := identifiers( pwdId ).store.value;
           end if;
           identifiers( pwdId ).store.value := current_working_directory;
        end if;
     end if;
  end if;
end ParseDirOpsChangeDir;

-----------------------------------------------------------------------------
--  PARSE DIR OPS MAKE DIR                               (built-in procedure)
--
-- AdaScript Syntax: directory_operations.make_dir( p )
--       Ada Target: GNAT.Directory_Operations.Make_Dir
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.make_dir
-----------------------------------------------------------------------------

procedure ParseDirOpsMakeDir is
  expr : storage;
  expr_type: identifier;
  subprogramId : constant identifier := dirops_make_dir_t;
begin
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, expr, expr_type, dirops_dir_name_str_t );

  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          Make_Dir( dir_name_str( to_string( expr.value ) ) );
        exception when directory_error =>
          err(
            contextNotes => pl( "in " )  & unb_pl( current_working_directory ),
            subjectNotes => em_value( expr.value ),
            reason => +"could not be selected because",
            obstructorNotes => em( "the directory not accessible or already exists" )
         );
        when others =>
          err_exception_raised;
        end;
      end if;
  end if;
end ParseDirOpsMakeDir;

-----------------------------------------------------------------------------
--  PARSE DIR OPS REMOVE DIR                             (built-in procedure)
--
-- AdaScript Syntax: directory_operations.remove_dir( p [, r] )
--       Ada Target: GNAT.Directory_Operations.Remove_Dir
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.remove_dir
-----------------------------------------------------------------------------

procedure ParseDirOpsRemoveDir is
  expr : storage;
  expr_type: identifier;
  expr2 : storage;
  expr_type2: identifier;
  subprogramId : constant identifier := dirops_remove_dir_t;
begin
  expect( subprogramId );
  ParseFirstStringParameter( subprogramId, expr, expr_type, dirops_dir_name_str_t );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastEnumParameter( subprogramId, expr2, expr_type2, boolean_t );
  else
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     if metaLabelOk( expr ) then
        declare
          recursive : constant boolean := expr2.value = to_unbounded_string( "1" );
        begin
          Remove_Dir( dir_name_str( to_string( expr.value ) ), recursive);
        exception when directory_error =>
          err( +"directory cannot be removed" );
        when others =>
          err_exception_raised;
        end;
      end if;
  end if;
end ParseDirOpsRemoveDir;

-----------------------------------------------------------------------------
--  PARSE DIR OPS REMOVE DIR                             (built-in procedure)
--
-- AdaScript Syntax: directory_operations.remove_dir( p [, r] )
--       Ada Target: GNAT.Directory_Operations.Remove_Dir
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.remove_dir
-----------------------------------------------------------------------------

procedure ParseDirOpsGetCurrentDir( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := dirops_get_current_dir_t;
begin
  kind := dirops_dir_name_str_t;
  expect( subprogramId );

  if isExecutingCommand then
     begin
       result := storage'( to_unbounded_string( get_current_dir ), sparMetaLabel );
     exception when directory_error =>
       err( +"directory not accessible" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseDirOpsGetCurrentDir;

-----------------------------------------------------------------------------
--  PARSE DIR OPS ABSOLUTE DIR NAME                       (built-in function)
--
-- AdaScript Syntax: s := directory_operations.absolute_dir_name( p )
--       Ada Target: Ada.Directories.Full_Name
--   GNAT Spec File: a-direct.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.absolute_dir_name
-----------------------------------------------------------------------------

procedure ParseDirOpsAbsoluteDirName( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type: identifier;
  subprogramId : constant identifier := dirops_absolute_dir_name_t;
begin
  kind := dirops_absolute_dir_name_str_t;
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, expr, expr_type, dirops_path_name_t );

  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          result := storage'( to_unbounded_string( Containing_Directory (
             Full_Name( path_name( to_string( expr.value ))))), expr.metaLabel );
        exception when others =>
          err_exception_raised;
        end;
      end if;
   end if;
end ParseDirOpsAbsoluteDirName;

-----------------------------------------------------------------------------
--  PARSE DIR OPS DIR NAME                                (built-in function)
--
-- AdaScript Syntax: s := directory_operations.dir_name( p )
--       Ada Target: GNAT.Directory_Operations.Dir_Name
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.dir_name
-----------------------------------------------------------------------------

procedure ParseDirOpsDirName( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type: identifier;
  subprogramId : constant identifier := dirops_dir_name_t;
begin
  kind := dirops_dir_name_str_t;
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, expr, expr_type, dirops_path_name_t );

  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          result := storage'( to_unbounded_string( dir_name( path_name( to_string( expr.value ) ) ) ),
            expr.metaLabel );
        exception when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseDirOpsDirName;

-----------------------------------------------------------------------------
--  PARSE DIR OPS BASE NAME                               (built-in function)
--
-- AdaScript Syntax: s := directory_operations.base_name( p [, f] )
--       Ada Target: GNAT.Directory_Operations.Base_Name
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.base_name
-----------------------------------------------------------------------------

procedure ParseDirOpsBaseName( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type: identifier;
  expr2 : storage;
  expr_type2: identifier;
  subprogramId : constant identifier := dirops_base_name_t;
begin
  kind := string_t;
  expect( subprogramId );
  ParseFirstStringParameter( subprogramId, expr, expr_type, dirops_path_name_t );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastStringParameter( subprogramId, expr2, expr_type2, string_t );
  else
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          result := storage'( to_unbounded_string( base_name( path_name( to_string( expr.value ) ), to_string( expr2.value ) ) ),
             expr.metaLabel );
        exception when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseDirOpsBaseName;

-----------------------------------------------------------------------------
--  PARSE DIR OPS FILE EXTENSION                          (built-in function)
--
-- AdaScript Syntax: s := directory_operations.file_extension( p )
--       Ada Target: GNAT.Directory_Operations.File_Extension
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.file_extension
-----------------------------------------------------------------------------

procedure ParseDirOpsFileExtension( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type: identifier;
  subprogramId : constant identifier := dirops_file_extension_t;
begin
  kind := string_t;
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, expr, expr_type, dirops_path_name_t );
  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          result := storage'( to_unbounded_string( file_extension( path_name( to_string( expr.value ) ) ) ),
            expr.metaLabel );
        exception when others =>
          err_exception_raised;
        end;
    end if;
  end if;
end ParseDirOpsFileExtension;

-----------------------------------------------------------------------------
--  PARSE DIR OPS FILE NAME                               (built-in function)
--
-- AdaScript Syntax: s := directory_operations.file_name( p )
--       Ada Target: GNAT.Directory_Operations.File_Name
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.file_name
-----------------------------------------------------------------------------

procedure ParseDirOpsFileName( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type: identifier;
  subprogramId : constant identifier := dirops_file_name_t;
begin
  kind := string_t;
  expect( subprogramId );
  ParseSingleStringParameter( subprogramId, expr, expr_type, dirops_path_name_t );

  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          result := storage'( to_unbounded_string( file_name( path_name( to_string( expr.value ) ) ) ),
             expr.metaLabel );
        exception when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseDirOpsFileName;

-----------------------------------------------------------------------------
--  PARSE DIR OPS FORMAT PATHNAME                         (built-in function)
--
-- AdaScript Syntax: s := directory_operations.format_pathname( p [,t] )
--       Ada Target: GNAT.Directory_Operations.Format_Pathname
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.format_pathname
-----------------------------------------------------------------------------

procedure ParseDirOpsFormatPathname( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type: identifier;
  expr2 : storage;
  expr_type2: identifier;
  subprogramId : constant identifier := dirops_format_pathname_t;
begin
  kind := dirops_path_name_t;
  expect( subprogramId );
  ParseFirstStringParameter( subprogramId, expr, expr_type, dirops_path_name_t );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastEnumParameter( subprogramId, expr2, expr_type2, dirops_path_style_t );
  else
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     if metaLabelOk( expr ) then
        declare
          style : path_style := system_default;
        begin
             -- TODO: not very elegant
             if expr2.value = to_unbounded_string( "0" ) then
                style := UNIX;
             elsif expr2.value = to_unbounded_string( "1" ) then
                style := DOS;
             elsif expr2.value = to_unbounded_string( "2" ) then
                style := System_Default;
             end if;
             result := storage'( to_unbounded_string( format_pathname( path_name( to_string( expr.value ) ), style ) ),
                expr.metaLabel );
        exception when directory_error =>
          err( +"directory not accessible" );
        when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseDirOpsFormatPathname;

-----------------------------------------------------------------------------
--  PARSE DIR OPS EXPAND PATH                             (built-in function)
--
-- AdaScript Syntax: s := directory_operations.expand_path( p [,t] )
--       Ada Target: GNAT.Directory_Operations.Format_Pathname
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.expand_path
-----------------------------------------------------------------------------

procedure ParseDirOpsExpandPath( result : out storage; kind : out identifier ) is
  expr : storage;
  expr_type: identifier;
  expr2 : storage;
  expr_type2: identifier;
  subprogramId : constant identifier := dirops_expand_path_t;
begin
  kind := dirops_path_name_t;
  expect( subprogramId );
  ParseFirstStringParameter( subprogramId, expr, expr_type, dirops_path_name_t );
  if token = symbol_t and identifiers( token ).store.value = "," then
     ParseLastEnumParameter( subprogramId, expr2, expr_type2, dirops_env_style_t );
  else
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     if metaLabelOk( expr ) then
        declare
          style : environment_style := System_Default;
        begin
          -- TODO: not very elegant
          if expr2.value= to_unbounded_string( "0" ) then
             style := UNIX;
          elsif expr2.value = to_unbounded_string( "1" ) then
             style := DOS;
          elsif expr2.value = to_unbounded_string( "2" ) then
             style := Both;
          elsif expr2.value = to_unbounded_string( "3" ) then
             style := System_Default;
          end if;
          result := storage'( to_unbounded_string( expand_path( path_name( to_string( expr.value ) ), style ) ),
             expr.metaLabel );
       exception when directory_error =>
          err( +"directory not accessible" );
       when others =>
          err_exception_raised;
       end;
     end if;
  end if;
end ParseDirOpsExpandPath;

-----------------------------------------------------------------------------
--  PARSE DIR OPS OPEN                                   (built-in procedure)
--
-- AdaScript Syntax: directory_operations.open( d, p )
--       Ada Target: GNAT.Directory_Operations.Open
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.open
-----------------------------------------------------------------------------

procedure ParseDirOpsOpen is
  resId : resHandleId;
  ref : reference;
  expr : storage;
  expr_type: identifier;
  theDir : resPtr;
  subprogramId : constant identifier := dirops_open_t;
begin
  expect( subprogramId );
  ParseFirstOutParameter( subprogramId, ref, dirops_dir_type_t );
  if baseTypesOK( ref.kind, dirops_dir_type_t ) then
     ParseLastStringParameter( subprogramId, expr, expr_type, string_t );
  end if;

  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          if not identifiers( ref.id ).resource then
             identifiers( ref.id ).resource := true;
             declareResource( resId, directory, getIdentifierBlock( ref.id ) );
             AssignParameter( ref, storage'( to_unbounded_string( resId ), expr.metaLabel ) );
             findResource( resId, theDir );
          else
             -- Reuse existing resource
             findResource( to_resource_id( identifiers( ref.id ).store.value ), theDir );
          end if;
          Open( theDir.dir, to_string( expr.value ) );
        exception when DIRECTORY_ERROR =>
          err(
            contextNotes => pl( "in " )  & unb_pl( current_working_directory ),
            subjectNotes => em_value( expr.value ),
            reason => +"could not be opened because",
            obstructorNotes => em( "the directory does not exist" )
         );
        when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseDirOpsOpen;

-----------------------------------------------------------------------------
--  PARSE DIR OPS CLOSE                                  (built-in procedure)
--
-- AdaScript Syntax: directory_operations.close( d )
--       Ada Target: GNAT.Directory_Operations.Close
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.close
-----------------------------------------------------------------------------

procedure ParseDirOpsClose is
  dirRef : reference;
  theDir : resPtr;
  dir    : storage;
  subprogramId : constant identifier := dirops_close_t;
begin
  expect( subprogramId );
  ParseSingleInOutParameter( subprogramId, dirRef, dirops_dir_type_t );
  if isExecutingCommand then
     getParameterValue( dirRef, dir );
     if metaLabelOk( dir ) then
        -- TODO: how do we tell if the directory is open?
        -- if identifiers( dirRef.id ).resource then
           begin
             findResource( to_resource_id( dir.value ), theDir );
             Close( theDir.dir );
           exception when DIRECTORY_ERROR =>
             err( +"directory is not open" );
           when others =>
             err_exception_raised;
           end;
        --else
        --   err( +"directory is not open" );
        --end if;
     end if;
  end if;
end ParseDirOpsClose;

-----------------------------------------------------------------------------
--  PARSE DIR OPS IS OPEN                                 (built-in function)
--
-- AdaScript Syntax: directory_operations.is_open( d )
--       Ada Target: GNAT.Directory_Operations.Close
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.is_open
-----------------------------------------------------------------------------

procedure ParseDirOpsIsOpen( result : out storage; kind : out identifier ) is
  dirRef : reference;
  dir    : storage;
  theDir : resPtr;
  subprogramId : constant identifier := dirops_is_open_t;
begin
  kind := boolean_t;
  expect( subprogramId );
  ParseSingleInOutParameter( subprogramId, dirRef, dirops_dir_type_t );

  if isExecutingCommand then
     getParameterValue( dirRef, dir );
     if metaLabelOk( dir ) then
        -- TODO: probably cannot tell if it's open in an array
        if dir.value /= null_unbounded_string then
           begin
             findResource( to_resource_id( dir.value ), theDir );
             result := storage'( to_spar_boolean( Is_Open( theDir.dir ) ), sparMetaLabel );
           exception when others =>
             err_exception_raised;
           end;
        else
           -- probably not open
           result := storage'( to_spar_boolean( false ),
              dir.metaLabel );
        end if;
     end if;
  end if;
end ParseDirOpsIsOpen;

-----------------------------------------------------------------------------
--  PARSE DIR OPS READ                                   (built-in procedure)
--
-- AdaScript Syntax: directory_operations.read( d, s )
--       Ada Target: GNAT.Directory_Operations.Read
--   GNAT Spec File: g-dirope.ads
--   SparForte Docs: doc/pkg_dirops.html#directory_operations.read
-----------------------------------------------------------------------------

procedure ParseDirOpsRead is
  dirRef   : reference;
  dir      : storage;
  strRef   : reference;
  --lastRef  : reference;
  theDir   : resPtr;
  subprogramId : constant identifier := dirops_read_t;
begin
  expect( subprogramId );
  ParseFirstInOutParameter( subprogramId, dirRef, dirops_dir_type_t );
  ParseLastOutParameter( subprogramId, strRef, string_t );
  --ParseLastOutParameter( lastRef, natural_t );
  if isExecutingCommand then
     getParameterValue( dirRef, dir );
     if metaLabelOk( dir ) then
        declare
          s : string(1..1024);
          last : natural;
        begin
          findResource( to_resource_id( dir.value ), theDir );
          Read( theDir.dir, s, last );
          AssignParameter( strRef, storage'( to_unbounded_string( s(1..Last) ),
             dir.metaLabel ) );
          --AssignParameter( lastRef, to_unbounded_string( last'img ) ); -- STRIP?
        exception when DIRECTORY_ERROR =>
          err( +"directory is not open" );
        when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseDirOpsRead;

-----------------------------------------------------------------------------
--
-- HOUSEKEEPING
--
-----------------------------------------------------------------------------


procedure StartupDirOps is
begin
  declareNamespace( "directory_operations" );
  declareIdent( dirops_dir_name_str_t, "directory_operations.dir_name_dir",
     string_t, subClass );
  declareIdent( dirops_absolute_dir_name_str_t, "directory_operations.absoute_dir_name_dir",
     string_t, subClass );
  declareIdent( dirops_dir_type_t, "directory_operations.dir_type_id",
     positive_t, typeClass );
 identifiers( dirops_dir_type_t ).usage := limitedUsage;
 identifiers( dirops_dir_type_t ).resource := true;

  declareFunction( dirops_dir_separator_t, "directory_operations.dir_separator", ParseDirOpsDirSeparator'access );
  declareProcedure( dirops_change_dir_t, "directory_operations.change_dir", ParseDirOpsChangeDir'access );
  declareProcedure( dirops_make_dir_t, "directory_operations.make_dir", ParseDirOpsMakeDir'access );
  declareProcedure( dirops_remove_dir_t, "directory_operations.remove_dir", ParseDirOpsRemoveDir'access );
  declareFunction( dirops_get_current_dir_t, "directory_operations.get_current_dir", ParseDirOpsGetCurrentDir'access );
  declareIdent( dirops_path_name_t, "directory_operations.path_name",
     string_t, subClass );
  declareFunction( dirops_absolute_dir_name_t, "directory_operations.absolute_dir_name", ParseDirOpsAbsoluteDirName'access );
  declareFunction( dirops_dir_name_t, "directory_operations.dir_name", ParseDirOpsDirName'access );
  declareFunction( dirops_base_name_t, "directory_operations.base_name", ParseDirOpsBaseName'access );
  declareFunction( dirops_file_extension_t, "directory_operations.file_extension", ParseDirOpsFileExtension'access );
  declareFunction( dirops_file_name_t, "directory_operations.file_name", ParseDirOpsFileName'access );
  declareIdent( dirops_path_style_t, "directory_operations.path_style",
     root_enumerated_t, typeClass );
  declareStandardConstant( dirops_path_style_unix_t, "path_style.unix",
     dirops_path_style_t, "0" );
  declareStandardConstant( dirops_path_style_dos_t, "path_style.dos",
     dirops_path_style_t, "1" );
  declareStandardConstant( dirops_path_style_system_default_t, "path_style.system_default",
     dirops_path_style_t, "2" );
  declareFunction( dirops_format_pathname_t, "directory_operations.format_pathname", ParseDirOpsFormatPathname'access );
  declareIdent( dirops_env_style_t, "directory_operations.environment_style",
     root_enumerated_t, typeClass );
  declareStandardConstant( dirops_env_style_unix_t, "environment_style.unix",
     dirops_env_style_t, "0" );
  declareStandardConstant( dirops_env_style_dos_t, "environment_style.dos",
     dirops_env_style_t, "1" );
  declareStandardConstant( dirops_env_style_both_t, "environment_style.both",
     dirops_env_style_t, "2" );
  declareStandardConstant( dirops_env_style_system_default_t, "environment_style.system_default",
     dirops_env_style_t, "3" );
  declareFunction( dirops_expand_path_t, "directory_operations.expand_path", ParseDirOpsExpandPath'access );
  declareProcedure( dirops_open_t, "directory_operations.open", ParseDirOpsOpen'access );
  declareProcedure( dirops_close_t, "directory_operations.close", ParseDirOpsClose'access );
  declareFunction( dirops_is_open_t, "directory_operations.is_open", ParseDirOpsIsOpen'access );
  declareProcedure( dirops_read_t, "directory_operations.read", ParseDirOpsRead'access );
  declareNamespaceClosed( "directory_operations" );
end StartupDirOps;

procedure ShutdownDirOps is
begin
  null;
end ShutdownDirOps;

end parser_dirops;
