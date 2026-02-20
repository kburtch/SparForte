------------------------------------------------------------------------------
-- Lock_File Package Parser                                                 --
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

--with ada.text_io;use ada.text_io;

with gnat.lock_files,
    ada.strings.unbounded,
    pegasoft,
    world,
    symbol_table,
    message_strings,
    scanner,
    scanner.communications,
    parser;
use gnat.lock_files,
    ada.strings.unbounded,
    pegasoft,
    world,
    symbol_table,
    message_strings,
    scanner,
    scanner.communications,
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
  expr   : storage;
  expr_type  : identifier;
  dirExpr    : storage;
  fileExpr   : storage;
  waitExpr   : storage := storage'( to_unbounded_string( "1.0" ), noMetaLabel, noMetaLabels );
  retryExpr  : storage := storage'( to_unbounded_string( natural'last'img ), noMetaLabel, noMetaLabels );
begin
  expect( locks_lock_t );
  expect( symbol_t, "(" );
  ParseExpression( expr, expr_type );
  if baseTypesOk( expr_type, string_t ) then
     if token = symbol_t and identifiers( token ).store.value = ")" then
        fileExpr := expr;
     else
        expectParameterComma;
        ParseExpression( fileExpr, expr_type );
        -- first variation: dir, file [,wait [,retry] ]
        if getUniType( expr_type ) = uni_string_t then
           dirExpr := expr;
           if token = symbol_t and identifiers( token ).store.value = "," then
              getNextToken;
              ParseExpression( waitExpr, expr_type );
              if baseTypesOk( expr_type, duration_t ) then
                 if token = symbol_t and identifiers( token ).store.value = "," then
                    getNextToken;
                    ParseExpression( retryExpr, expr_type );
                    if baseTypesOk( expr_type, duration_t ) then
                       null;
                    end if;
                 end if;
              end if;
           end if;
        elsif baseTypesOk( expr_type, duration_t ) then
           -- TODO: could be cleaner...
           -- second variation: file [,wait [,retry] ]
           waitExpr := fileExpr; -- what we read was wait time
           fileExpr := expr; -- and first param is file
           if token = symbol_t and identifiers( token ).store.value = "," then
              getNextToken;
              ParseExpression( retryExpr, expr_type );
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
       if length( dirExpr.value ) > 0 then
          if metaLabelOk( locks_lock_t, dirExpr, fileExpr ) then
             Lock_File( to_string( dirExpr.value ),
               to_string( fileExpr.value ),
               duration( to_numeric( waitExpr.value ) ),
               natural( to_numeric( retryExpr.value ) )
          );
          end if;
       else
          if metaLabelOk( locks_lock_t, fileExpr ) then
            Lock_File( to_string( fileExpr.value ),
               duration( to_numeric( waitExpr.value ) ),
               natural( to_numeric( retryExpr.value ) )
            );
          end if;
       end if;
     exception when lock_error =>
       err( +"File cannot be locked" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseLockLockFile;

procedure ParseLockUnlockFile is
  -- Syntax: unlock_file( file ) or
  --         unlock_file( dir, file )
  dirExpr    : storage := nullStorage;
  fileExpr   : storage;
  expr_type  : identifier;
begin
  expect( locks_unlock_t );
  expect( symbol_t, "(" );
  ParseExpression( fileExpr, expr_type );
  if baseTypesOk( expr_type, string_t ) then
     if token = symbol_t and identifiers( token ).store.value = "," then
        getNextToken;
        dirExpr := fileExpr;
        ParseExpression( fileExpr, expr_type );
        if baseTypesOk( expr_type, string_t ) then
           null;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     if length( dirExpr.value ) > 0 then
          if metaLabelOk( locks_unlock_t, dirExpr, fileExpr ) then
             Unlock_File( to_string( dirExpr.value ),
               to_string( fileExpr.value ) );
          end if;
     else
        if metaLabelOk( locks_unlock_t, fileExpr ) then
           Unlock_File( to_string( fileExpr.value ) );
        end if;
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
