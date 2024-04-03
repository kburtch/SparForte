------------------------------------------------------------------------------
-- Directory_Operations Package Parser                                      --
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

--with text_io;use text_io;

with gnat.directory_operations,
    Ada.Directories,
    ada.strings.unbounded,
    world,
    scanner,
    scanner.communications,
    scanner_res,
    parser_params;
use gnat.directory_operations,
    Ada.Directories,
    ada.strings.unbounded,
    world,
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

--procedure ParseSingleDirNameStrExpression( expr_val : out unbounded_string;
--  expr_type : out identifier ) is
--begin
--  expect( symbol_t, "(" );
--  ParseExpression( expr_val, expr_type );
--  if baseTypesOk( expr_type, dirops_dir_name_str_t ) then
--     expect( symbol_t, ")" );
--  end if;
--end ParseSingleDirNameStrExpression;
--
--procedure ParseSinglePathNameExpression( expr_val : out unbounded_string;
--  expr_type : out identifier ) is
--begin
--  expect( symbol_t, "(" );
--  ParseExpression( expr_val, expr_type );
--  if baseTypesOk( expr_type, dirops_path_name_t ) then
--     expect( symbol_t, ")" );
--  end if;
--end ParseSinglePathNameExpression;

procedure ParseDirOpsDirSeparator( result : out unbounded_string; kind : out identifier ) is
  -- Syntax:
  -- Source:
begin
  kind := character_t;
  expect( dirops_dir_separator_t );
  begin
    if isExecutingCommand then
       result := null_unbounded_string & dir_separator;
    end if;
  exception when directory_error =>
    err( +"directory not accessible" );
  when others =>
    err_exception_raised;
  end;
end ParseDirOpsDirSeparator;

procedure ParseDirOpsChangeDir is
  -- Syntax:
  -- Source:
  expr_val : unbounded_string;
  expr_type: identifier;
begin
  expect( dirops_change_dir_t );
  ParseSingleStringParameter( dirops_change_dir_t, expr_val, expr_type, dirops_dir_name_str_t );
  begin
    if isExecutingCommand then
       Change_Dir( dir_name_str( to_string( expr_val ) ) );
    end if;
  exception when directory_error =>
    err( +"directory not accessible" );
  when others =>
    err_exception_raised;
  end;
end ParseDirOpsChangeDir;

procedure ParseDirOpsMakeDir is
  -- Syntax:
  -- Source:
  expr_val : unbounded_string;
  expr_type: identifier;
begin
  expect( dirops_make_dir_t );
  ParseSingleStringParameter( dirops_make_dir_t, expr_val, expr_type, dirops_dir_name_str_t );
  begin
    if isExecutingCommand then
       Make_Dir( dir_name_str( to_string( expr_val ) ) );
    end if;
  exception when directory_error =>
    err( +"directory not accessible" );
  when others =>
    err_exception_raised;
  end;
end ParseDirOpsMakeDir;

procedure ParseDirOpsRemoveDir is
  -- Syntax:
  -- Source:
  expr_val : unbounded_string;
  expr_type: identifier;
  expr_val2 : unbounded_string;
  expr_type2: identifier;
begin
  expect( dirops_remove_dir_t );
  ParseFirstStringParameter( dirops_remove_dir_t, expr_val, expr_type, dirops_dir_name_str_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( dirops_remove_dir_t, expr_val2, expr_type2, boolean_t );
  else
     expect( symbol_t, ")" );
  end if;
  declare
    recursive : constant boolean := expr_val2 = to_unbounded_string( "1" );
  begin
    if isExecutingCommand then
       Remove_Dir( dir_name_str( to_string( expr_val ) ), recursive);
    end if;
  exception when directory_error =>
    err( +"directory cannot be removed" );
  when others =>
    err_exception_raised;
  end;
end ParseDirOpsRemoveDir;

procedure ParseDirOpsGetCurrentDir( result : out unbounded_string; kind : out identifier ) is
  -- Syntax:
  -- Source:
begin
  kind := dirops_dir_name_str_t;
  expect( dirops_get_current_dir_t );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( get_current_dir );
    end if;
  exception when directory_error =>
    err( +"directory not accessible" );
  when others =>
    err_exception_raised;
  end;
end ParseDirOpsGetCurrentDir;

procedure ParseDirOpsAbsoluteDirName( result : out unbounded_string; kind : out identifier ) is
  -- Syntax:
  -- Source:
  expr_val : unbounded_string;
  expr_type: identifier;
begin
  kind := dirops_absolute_dir_name_str_t;
  expect( dirops_absolute_dir_name_t );
  ParseSingleStringParameter( dirops_absolute_dir_name_t, expr_val, expr_type, dirops_path_name_t );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( Containing_Directory (Full_Name( path_name( to_string( expr_val)))));
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseDirOpsAbsoluteDirName;

procedure ParseDirOpsDirName( result : out unbounded_string; kind : out identifier ) is
  -- Syntax:
  -- Source:
  expr_val : unbounded_string;
  expr_type: identifier;
begin
  kind := dirops_dir_name_str_t;
  expect( dirops_dir_name_t );
  ParseSingleStringParameter( dirops_dir_name_t, expr_val, expr_type, dirops_path_name_t );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( dir_name( path_name( to_string( expr_val ) ) ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseDirOpsDirName;

procedure ParseDirOpsBaseName( result : out unbounded_string; kind : out identifier ) is
  -- Syntax:
  -- Source:
  expr_val : unbounded_string;
  expr_type: identifier;
  expr_val2 : unbounded_string;
  expr_type2: identifier;
begin
  kind := string_t;
  expect( dirops_base_name_t );
  ParseFirstStringParameter( dirops_base_name_t, expr_val, expr_type, dirops_path_name_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastStringParameter( dirops_base_name_t, expr_val2, expr_type2, string_t );
  else
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       result := to_unbounded_string( base_name( path_name( to_string( expr_val ) ), to_string( expr_val2 ) ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseDirOpsBaseName;

procedure ParseDirOpsFileExtension( result : out unbounded_string; kind : out identifier ) is
  -- Syntax:
  -- Source:
  expr_val : unbounded_string;
  expr_type: identifier;
begin
  kind := string_t;
  expect( dirops_file_extension_t );
  ParseSingleStringParameter( dirops_file_extension_t, expr_val, expr_type, dirops_path_name_t );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( file_extension( path_name( to_string( expr_val ) ) ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseDirOpsFileExtension;

procedure ParseDirOpsFileName( result : out unbounded_string; kind : out identifier ) is
  -- Syntax:
  -- Source:
  expr_val : unbounded_string;
  expr_type: identifier;
begin
  kind := string_t;
  expect( dirops_file_name_t );
  ParseSingleStringParameter( dirops_file_name_t, expr_val, expr_type, dirops_path_name_t );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( file_name( path_name( to_string( expr_val ) ) ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseDirOpsFileName;

procedure ParseDirOpsFormatPathname( result : out unbounded_string; kind : out identifier ) is
  -- Syntax:
  -- Source:
  expr_val : unbounded_string;
  expr_type: identifier;
  expr_val2 : unbounded_string;
  expr_type2: identifier;
begin
  kind := dirops_path_name_t;
  expect( dirops_format_pathname_t );
  ParseFirstStringParameter( dirops_format_pathname_t, expr_val, expr_type, dirops_path_name_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( dirops_format_pathname_t, expr_val2, expr_type2, dirops_path_style_t );
  else
     expect( symbol_t, ")" );
  end if;
  declare
    style : path_style := system_default;
  begin
    if isExecutingCommand then
       -- not very elegant
       if expr_val2 = to_unbounded_string( "0" ) then
          style := UNIX;
       elsif expr_val2 = to_unbounded_string( "1" ) then
          style := DOS;
       elsif expr_val2 = to_unbounded_string( "2" ) then
          style := System_Default;
       end if;
       result := to_unbounded_string( format_pathname( path_name( to_string( expr_val ) ), style ) );
    end if;
  exception when directory_error =>
    err( +"directory not accessible" );
  when others =>
    err_exception_raised;
  end;
end ParseDirOpsFormatPathname;

procedure ParseDirOpsExpandPath( result : out unbounded_string; kind : out identifier ) is
  -- Syntax:
  -- Source:
  expr_val : unbounded_string;
  expr_type: identifier;
  expr_val2 : unbounded_string;
  expr_type2: identifier;
begin
  kind := dirops_path_name_t;
  expect( dirops_expand_path_t );
  ParseFirstStringParameter( dirops_expand_path_t, expr_val, expr_type, dirops_path_name_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( dirops_expand_path_t, expr_val2, expr_type2, dirops_env_style_t );
  else
     expect( symbol_t, ")" );
  end if;
  declare
    style : environment_style := System_Default;
  begin
    if isExecutingCommand then
       -- not very elegant
       if expr_val2 = to_unbounded_string( "0" ) then
          style := UNIX;
       elsif expr_val2 = to_unbounded_string( "1" ) then
          style := DOS;
       elsif expr_val2 = to_unbounded_string( "2" ) then
          style := Both;
       elsif expr_val2 = to_unbounded_string( "3" ) then
          style := System_Default;
       end if;
       result := to_unbounded_string( expand_path( path_name( to_string( expr_val ) ), style ) );
    end if;
  exception when directory_error =>
    err( +"directory not accessible" );
  when others =>
    err_exception_raised;
  end;
end ParseDirOpsExpandPath;

--procedure ParseDirOpsNewDirType is
--  -- Syntax: dirops.new_dir_type( q )
--  -- Source: N/A
--  resId : resHandleId;
--  ref : reference;
--begin
--  expect( dirops_new_directory_t );
--  ParseSingleOutParameter( ref, dirops_dir_type_t );
--  if baseTypesOK( ref.kind, dirops_dir_type_t ) then
--      null;
--  end if;
--  if isExecutingCommand then
--     identifiers( ref.id ).resource := true;
--     declareResource( resId, directory, blocks_top );
--     AssignParameter( ref, to_unbounded_string( resId ) );
--  end if;
--end ParseDirOpsNewDirType;

procedure ParseDirOpsOpen is
  -- Syntax: dirops.open( d, s )
  -- Source: Open (Dir : out Dir_Type; Dir_Name : Dir_Name_Str);
  resId : resHandleId;
  ref : reference;
  expr_val : unbounded_string;
  expr_type: identifier;
  theDir : resPtr;
begin
  expect( dirops_open_t );
  ParseFirstOutParameter( dirops_open_t, ref, dirops_dir_type_t );
  if baseTypesOK( ref.kind, dirops_dir_type_t ) then
     ParseLastStringParameter( dirops_open_t, expr_val, expr_type, string_t );
  end if;
  if isExecutingCommand then
     begin
       if not identifiers( ref.id ).resource then
          identifiers( ref.id ).resource := true;
          declareResource( resId, directory, getIdentifierBlock( ref.id ) );
          AssignParameter( ref, to_unbounded_string( resId ) );
          findResource( resId, theDir );
       else
          -- Reuse existing resource
          findResource( to_resource_id( identifiers( ref.id ).value.all ), theDir );
       end if;
       Open( theDir.dir, to_string( expr_val ) );
     exception when DIRECTORY_ERROR =>
       err( +"directory does not exist" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseDirOpsOpen;

procedure ParseDirOpsClose is
  -- Syntax: dirops.close( d )
  -- Source: Close (Dir : out Dir_Type);
  dirId : identifier;
  theDir : resPtr;
begin
  expect( dirops_close_t );
  ParseSingleInOutParameter( dirops_close_t, dirId, dirops_dir_type_t );
  if isExecutingCommand then
     if identifiers( dirId ).resource then
        begin
          findResource( to_resource_id( identifiers( dirId ).value.all ), theDir );
          Close( theDir.dir );
        exception when DIRECTORY_ERROR =>
          err( +"directory is not open" );
        when others =>
          err_exception_raised;
        end;
     else
        err( +"directory is not open" );
     end if;
  end if;
end ParseDirOpsClose;

procedure ParseDirOpsIsOpen( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: dirops.is_open( d )
  -- Source: Is_Open (Dir : Dir_Type) return Boolean;
  dirId : identifier;
  theDir : resPtr;
begin
  kind := boolean_t;
  expect( dirops_is_open_t );
  ParseSingleInOutParameter( dirops_is_open_t, dirId, dirops_dir_type_t );
  if isExecutingCommand then
     if identifiers( dirId ).resource then
        begin
          findResource( to_resource_id( identifiers( dirId ).value.all ), theDir );
          result := to_bush_boolean( Is_Open( theDir.dir ) );
        exception when others =>
          err_exception_raised;
        end;
     else
        -- probably not open
        result := to_bush_boolean( false );
     end if;
  end if;
end ParseDirOpsIsOpen;

-- is_open

procedure ParseDirOpsRead is
  -- Syntax: dirops.read( d, s )
  -- Source: Read (Dir : Dir_Type; Str : out String; Last : out Natural)
  dirId : identifier;
  strRef   : reference;
  --lastRef  : reference;
  theDir   : resPtr;
begin
  expect( dirops_read_t );
  ParseFirstInOutParameter( dirops_read_t, dirId, dirops_dir_type_t );
  ParseLastOutParameter( dirops_read_t, strRef, string_t );
  --ParseLastOutParameter( lastRef, natural_t );
  if isExecutingCommand then
     declare
       s : string(1..1024);
       last : natural;
     begin
       findResource( to_resource_id( identifiers( dirId ).value.all ), theDir );
       Read( theDir.dir, s, last );
       AssignParameter( strRef, to_unbounded_string( s(1..Last) ) );
       --AssignParameter( lastRef, to_unbounded_string( last'img ) ); -- STRIP?
     exception when DIRECTORY_ERROR =>
       err( +"directory is not open" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseDirOpsRead;

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
