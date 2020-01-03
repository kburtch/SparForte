------------------------------------------------------------------------------
-- Lock_File Package Parser                                                 --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2020 Free Software Foundation              --
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

with gnat.lock_files,
    ada.strings.unbounded,
    world,
    scanner,
    parser;
use gnat.lock_files,
    ada.strings.unbounded,
    world,
    scanner,
    parser;

package body parser_lock is

------------------------------------------------------------------------------
-- Lock_Files package identifiers
------------------------------------------------------------------------------

locks_lock_t      : identifier;
locks_unlock_t    : identifier;

procedure ParseLockLockFile is
  -- Syntax: Lock_File( dir, file [,wait [,retry] ) or
  --         Lock_File( file, [,wait [,retry] ] )
  expr_val   : unbounded_string;
  expr_type  : identifier;
  dir_val    : unbounded_string;
  file_val   : unbounded_string;
  wait_val   : unbounded_string := to_unbounded_string( "1.0" );
  retry_val  : unbounded_string := to_unbounded_string( natural'last'img );
begin
  expect( locks_lock_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, string_t ) then
     if token = symbol_t and identifiers( token ).value.all = ")" then
        file_val := expr_val;
     else
        expect( symbol_t, "," );
        ParseExpression( file_val, expr_type );
        -- first variation: dir, file [,wait [,retry] ]
        if getUniType( expr_type ) = uni_string_t then
           dir_val := expr_val;
           if token = symbol_t and identifiers( token ).value.all = "," then
              expect( symbol_t, "," );
              ParseExpression( wait_val, expr_type );
              if baseTypesOk( expr_type, duration_t ) then
                 if token = symbol_t and identifiers( token ).value.all = "," then
                    expect( symbol_t, "," );
                    ParseExpression( retry_val, expr_type );
                    if baseTypesOk( expr_type, duration_t ) then
                       null;
                    end if;
                 end if;
              end if;
           end if;
        elsif baseTypesOk( expr_type, duration_t ) then
           -- TODO: could be cleaner...
           -- second variation: file [,wait [,retry] ]
           wait_val := file_val; -- what we read was wait time
           file_val := expr_val; -- and first param is file
           if token = symbol_t and identifiers( token ).value.all = "," then
              expect( symbol_t, "," );
              ParseExpression( retry_val, expr_type );
              if baseTypesOk( expr_type, duration_t ) then
                  null;
              end if;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
       if length( dir_val ) > 0 then
          Lock_File( to_string( dir_val ),
            to_string( file_val ),
            duration( to_numeric( wait_val ) ),
            natural( to_numeric( retry_val ) )
          );
       else
          Lock_File( to_string( file_val ),
            duration( to_numeric( wait_val ) ),
            natural( to_numeric( retry_val ) )
          );
       end if;
     exception when lock_error =>
       err( "File cannot be locked" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseLockLockFile;

procedure ParseLockUnlockFile is
  -- Syntax: unlock_file( file ) or
  --         unlock_file( dir, file )
  dir_val    : unbounded_string := null_unbounded_string;
  file_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( locks_unlock_t );
  expect( symbol_t, "(" );
  ParseExpression( file_val, expr_type );
  if baseTypesOk( expr_type, string_t ) then
     if token = symbol_t and identifiers( token ).value.all = "," then
        expect( symbol_t, "," );
        dir_val := file_val;
        ParseExpression( file_val, expr_type );
        if baseTypesOk( expr_type, string_t ) then
           null;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     if length( dir_val ) > 0 then
        Unlock_File( to_string( dir_val ),
          to_string( file_val ) );
     else
        Unlock_File( to_string( file_val ) );
     end if;
  end if;
end ParseLockUnlockFile;

procedure StartupLockFiles is
begin
  declareNamespace( "lock_files" );
  declareProcedure( locks_lock_t, "lock_files.lock_file", ParseLockLockFile'access );
  declareProcedure( locks_unlock_t, "lock_files.unlock_file", ParseLockUnlockFile'access );
  declareNamespaceClosed( "lock_files" );
end StartupLockFiles;

procedure ShutdownLockFiles is
begin
  null;
end ShutdownLockFiles;

end parser_Lock;
