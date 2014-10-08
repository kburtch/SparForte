------------------------------------------------------------------------------
-- Common declarations across most of SparForte/BUSH including              --
-- command line switches and the symbol table.                              --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2011 Free Software Foundation            --
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

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );

with system,
    ada.text_io,
    ada.strings.fixed,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    gnat.source_info,
    bush_os;
    -- scanner_arrays;
use ada.text_io,
    ada.command_line,
    ada.command_line.environment,
    ada.strings.fixed,
    ada.strings.unbounded,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    bush_os;
    -- scanner_arrays;

pragma Optimize( time );

package body world is

localMemcacheClusterInitialized : boolean := false;
distributedMemcacheClusterInitialized : boolean := false;
-- flag: only initialize the memcache clusters once

-----------------------------------------------------------------------------
-- TINY HASH CACHE
--
-- This is a small hash table for caching identifier lookups by findIdent.
-- The goal is to keep the table very small, and the hash function very
-- fast and simple, to cache a subset of the most frequently accessed
-- variables.
--
-- The cnt field is used to mark the age of an entry.  If cnt is different
-- from the current cnt number, then the cache entry has expired.
--
-- To use the tiny id cache (with findIdent), each block must have its own
-- cache.  Otherwise, with one global cache that is cleared on every block
-- change, the cache contents get cleared so often that caching will not
-- effective.
--
-- Since the block table is in the scanner, and findIdent is global, it
-- would mean some source code restructuring to implement this cache.
-----------------------------------------------------------------------------

--procedure resetTinyHashCache is
--begin
--  if currentTinyHashCacheCnt = 1 then
--     for i in actualHash'range loop
--         tinyHashCache( i ).cnt := 0;
--     end loop;
--  end if;
--  currentTinyHashCacheCnt := currentTinyHashCacheCnt + 1;
--end resetTinyHashCache;
--
--procedure setTinyHashCache( s : unbounded_string; h : actualHash; id : identifier ) is
--begin
--   if id /= eof_t then
--      if tinyHashCache( h ).cnt /= currentTinyHashCacheCnt then
--         tinyHashCache( h ).key := s;
--         tinyHashCache( h ).id  := id;
--         tinyHashCache( h ).cnt := currentTinyHashCacheCnt;
--put_line( "cache new - " & to_string( s ) & id'img );
--      end if;
--   end if;
--end setTinyHashCache;

--procedure getTinyHashCache( s : unbounded_string; id : out identifier ; h : out actualHash ) is
--    l : natural;
--   c1 : character;
--   c2 : character;
--begin
--    l := length( s );
--   c1 := element( s, 1 );
--   c2 := element( s, l );
--    h := actualhash(
--           1 + (
--             character'pos( c1 ) * 7 +
--             character'pos( c2 ) * 11 +
--             l * 17
--           ) mod integer( actualHash'last )
--         );
--
--    if tinyHashCache( h ).key = s then
--       if tinyHashCache( h ).cnt = currentTinyHashCacheCnt then
--          id := tinyHashCache( h ).id;
--put_line( "cache hit - " & to_string( s ) & id'img );
--       else
--          id := eof_t;
--       end if;
--    else
--       id := eof_t;
--    end if;
--end getTinyHashCache;

--
-- Symbol Table Utilities
--

procedure declareKeyword( id : out identifier; s : string ) is
-- Initialize a keyword / internal identifier in the symbol table
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow;                               -- raise error
  else                                                          -- else add
     id := identifiers_top;                                     -- return id
     identifiers_top := identifiers_top + 1;                    -- push stack
     declare
       kw : declaration renames identifiers( id );
     begin
       if kw.avalue /= null then
          free( kw.avalue );
       end if;
       kw.name := To_Unbounded_String( s );
       kw.kind := identifier'first;
       kw.value := Null_Unbounded_String;
       kw.class := otherClass;
       -- since keywords are only declared at startup,
       -- the defaults should be OK for remaining fields.
     end;
  end if;
end declareKeyword;

procedure declareFunction( id : out identifier; s : string; cb : aBuiltinFunctionCallback := null ) is
-- Initialize a built-in function identifier in the symbol table
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow;                               -- raise error
  else                                                          -- else add
     id := identifiers_top;                                     -- return id
     identifiers_top := identifiers_top + 1;                    -- push stack
     declare
       kw : declaration renames identifiers( id );
     begin
       if kw.avalue /= null then
          free( kw.avalue );
       end if;
       kw.name := To_Unbounded_String( s );
       kw.kind := identifier'first;
       kw.value := Null_Unbounded_String;
       kw.class := funcClass;
       kw.procCB := null;
       kw.funcCB := cb;
       kw.avalue := null;
     end;
  end if;
end declareFunction;

procedure declareProcedure( id : out identifier; s : string; cb : aBuiltinProcedureCallback := null ) is
-- Initialize a built-in procedure identifier in the symbol table
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow;                               -- raise error
  else                                                          -- else add
     id := identifiers_top;                                     -- return id
     identifiers_top := identifiers_top + 1;                    -- push stack
     declare
       kw : declaration renames identifiers( id );
     begin
       if kw.avalue /= null then
          free( kw.avalue );
       end if;
       kw.name := To_Unbounded_String( s );
       kw.kind := identifier'first;
       kw.value := Null_Unbounded_String;
       kw.class := procClass;
       kw.procCB := cb;
       kw.funcCB := null;
       kw.avalue := null;
     end;
  end if;
end declareProcedure;

procedure findIdent( name : unbounded_string; id : out identifier ) is
-- Return the id of a keyword / identifier in the symbol table.  If
-- it is not found, return the end-of-file token as the id
  i      : identifier;
  p      : identifier;
  save_i : identifier;
  dotPos : natural;
  prefix : unbounded_string;
  --h      : actualHash;
begin
  id := eof_t;                                                  -- assume bad

--put_line( "findIdent: " & to_string( name ) ); -- DEBUG

  -- first, search the local namespace

--put_line( "local search" ); -- DEBUG
  i := identifiers_top-1;
  while i /= eof_t loop
      exit when identifiers( i ).class = namespaceClass;
      if identifiers( i ).name = name and not                -- exists and
         identifiers( i ).deleted then                       -- not deleted?
         id := i;                                            -- return id
         exit;                                               -- we're done
      end if;
      i := i - 1;
  end loop;
  save_i := i;

  -- second, check for
  -- note that in this version of SparForte, the prefix could be for a
  -- record instead of a package
  -- this only affects identifiers with a prefix

  if id = eof_t then
     if length( name ) > 1 then
        dotPos := length( name ) - 1;
        while dotPos > 1 loop
           if element( name, dotPos ) = '.' then
              prefix := unbounded_slice( name, 1, dotPos-1 );
              exit;
           else
              dotPos := dotPos - 1;
           end if;
        end loop;
     end if;

    -- try the tiny hash cache

    --getTinyHashCache( name, id, h );
    --if id /= eof_t then
    --   goto found;
    --end if;

-- put_line( "prefix = " & to_string( prefix ) ); -- debug

     -- for this initial version, we are not assuming nested namespaces
     -- we're only looking to speed up finding idents in built-in packages

     p := save_i;                                                                 -- start after local
     if length( prefix ) > 0 then                                               -- a id prefix?
--put_line( "namespace search" ); -- DEBUG
        while p > identifiers'first loop                                        -- while stuff
           if identifiers( p ).class = namespaceClass then                      -- a ns?
              -- if it is a closed namespace
              if identifiers( p ).openNamespace /= identifiers'first then       -- a ns closed?
--put_line( "closed namespace " & to_string( identifiers( p ).name ) ); -- DEBUG
                 if identifiers( p ).name = prefix then -- TODO: value
--put_line( "FOUND " & to_string( identifiers( p ).name ) ); -- DEBUG
                    i := p-1;
                    -- TODO: with open, this could be a for loop
                    while i > identifiers'first loop
                        exit when identifiers( i ).class = namespaceClass;
                        if identifiers( i ).name = name and not                -- exists and
                           identifiers( i ).deleted then                       -- not deleted?
                           id := i;                                            -- return id
                           --setTinyHashCache( name, h, id );
                           goto found;                                         -- we're done
                        end if;
                        i := i - 1;                                            -- next id
                    end loop;
                    p := i;                                                    -- set pos
                    exit when p = identifiers'first;                           -- bail if none
                 else                                                          -- wrong ns?
                    p := identifiers( p ).nextNamespace;                       -- skip it
                    exit when p = identifiers'first;                           -- bail if none
                 end if;
              else                                                             -- open ns?
                 p := identifiers( p ).nextNamespace;                          -- next ns
                 exit when p = identifiers'first;                              -- bail if none
              end if;
           else                                                                -- global id?
              p := p - 1;                                                      -- look for ns
           end if;
        end loop;
     else
--put_line( "global search" ); -- DEBUG
        -- search for something without a prefix: skip namespace tags as they
        -- only contain identifiers with a prefix.
        while p > identifiers'first loop                                       -- while stuff
           if identifiers( p ).class = namespaceClass then                     -- a ns?
              if identifiers( p ).openNamespace /= identifiers'first then      -- a ns closed?
                 p := identifiers( p ).nextNamespace;                          -- skip it
                 exit when p = identifiers'first;                              -- bail if none
              else                                                             -- open ns?
                 p := p - 1;                                                   -- next id is global
                 exit when p = identifiers'first;                              -- bail if none
              end if;
           else                                                                -- not a ns? then global
              if identifiers( p ).name = name and not                          -- exists and
                 identifiers( p ).deleted then                                 -- not deleted?
--put_line( "FOUND global space " & to_string( identifiers( p ).name ) ); -- DEBUG
                 id := p;                                                      -- return id
                 --setTinyHashCache( name, h, id );
                 goto found;                                                   -- we're done
              end if;
              p := p - 1;                                                      -- else next id
           end if;
        end loop;
     end if;
  end if;

<<found>>

  -- global namespace types like integer will be tested here

  -- fallback
  --
  -- brute-force from where we left off in the local search,
  -- searching everything
  -- new variables, records, pragma names will trigger here
  -- enums like direction.forward will go here since the prefix doesn't match the namespace tags

  if id = eof_t then
--put_line( standard_error, "brute force" ); -- DEBUG
     for i in reverse 1..save_i loop                         -- from local ns
         if i /= eof_t then
            if identifiers( i ).name = name and not          -- exists and
               identifiers( i ).deleted then                 -- not deleted?
               id := i;                                      -- return id
--put_line( standard_error, "internal error: brute force found " & to_string( name ) ); -- DEBUG
               exit;                                         -- we're done
            end if;
         end if;
     end loop;
  end if;
end findIdent;

-- FIND ENUM IMAGE
--
-- Find the name of the enumerated item of enumerated type kind with value
-- val.

procedure findEnumImage( val : unbounded_string; kind : identifier; name : out unbounded_string ) is
  -- found : boolean := false;
begin
  for i in reverse identifiers'first..identifiers_top-1 loop
      if identifiers( i ).class = enumClass then
         if identifiers( i ).kind = kind then
            if identifiers( i ).value = val then
               name := identifiers( i ).name;
               -- found := true;
               exit;
            end if;
         end if;
      end if;
  end loop;
  -- if not found then
  --    name := null_unbounded_string;
  -- end if;
end findEnumImage;

procedure init_env_ident( s : string ) is
-- Declare an operating system environment variable. 's' is the
-- variable string returned by get_env ("var=value" format).
  eqpos : natural := 0; -- position of the '=' in s
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow;                               -- raise error
  else                                                          -- otherwise
     for i in 1..s'length loop                                  -- find '='
         if s(i) = '=' then                                     -- found?
            eqpos := i;                                         -- remember
            exit;                                               -- and done
         end if;
     end loop;
     if identifiers( identifiers_top ).avalue /= null then
        free( identifiers( identifiers_top ).avalue );
     end if;
     identifiers( identifiers_top ) := declaration'(            -- define
       name     => To_Unbounded_String( s(s'first..eqpos-1) ),  -- identifier
       kind     => string_t,
       value    => To_Unbounded_String( s(eqpos+1..s'last ) ),
       class    => varClass,
       import   => true,                                        -- must import
       method   => shell,
       mapping  => none,
       export   => false,
       volatile => false,
       static   => false,
       limit    => false,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       wasReferenced => false,
       procCB => null,
       funcCB => null,
       genKind => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first,
       nextNamespace => identifiers'first,
       parentNamespace => identifiers'first,
       firstBound => 1,
       lastBound => 0,
       avalue => null
     );
     identifiers_top := identifiers_top + 1;                    -- push stack
  end if;
end init_env_ident;

procedure declareIdent( id : out identifier; name : unbounded_string;
  kind : identifier; class : anIdentifierClass := varClass ) is
-- Declare an identifier in the symbol table, specifying name, kind.
-- and (optionally) symbol class.  The id is returned.
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow;                               -- raise error
  else                                                          -- otherwise
     id := identifiers_top;                                     -- return id
     identifiers_top := identifiers_top+1;                      -- push stack
     if identifiers( id ).avalue /= null then
        free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(                         -- define
       name     => name,                                        -- identifier
       kind     => kind,
       value    => Null_Unbounded_String,
       class    => class,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatile => false,
       static   => false,
       limit    => false,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       wasReferenced => false,
       procCB => null,
       funcCB => null,
       genKind => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first,
       nextNamespace => identifiers'first,
       parentNamespace => identifiers'first,
       firstBound => 1,
       lastBound => 0,
       avalue => null
     );
  end if;
end declareIdent;

procedure declareIdent( id : out identifier; name : string;
  kind : identifier; class : anIdentifierClass := varClass ) is
-- Alternate version: use fixed string type for name
begin
  declareIdent( id, to_unbounded_string( name ), kind, class );
end declareIdent;

procedure declareStandardConstant( id : out identifier;
   name : string; kind : identifier; value : string ) is
-- Declare a standard constant in the symbol table.  The id is not
-- returned since we don't change with constants once they are set.
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow;                               -- raise error
  else                                                          -- otherwise
     declare
       sc : declaration renames identifiers( identifiers_top );
     begin
       if sc.avalue /= null then
          free( sc.avalue );
       end if;
       sc.name  := to_unbounded_string( name );                 -- define
       sc.kind  := kind;                                        -- identifier
       sc.value := to_unbounded_string( value );
       sc.class := constClass;
       sc.static := true;                                       -- identifier
       sc.field_of := eof_t;
       -- since this is only called at startup, the default
       -- values for the other fields should be OK
     end;
     id := identifiers_top;
     identifiers_top := identifiers_top+1;                      -- push stack
  end if;
end declareStandardConstant;

procedure declareStandardConstant( name : string; kind : identifier;
  value : string ) is
-- Alternative version: return the symbol table id
  discard_id : identifier;
begin
   declareStandardConstant( discard_id, name, kind, value );    -- declare it
end declareStandardConstant;

procedure declareStandardEnum( id : out identifier;
   name : string; kind : identifier; value : string ) is
-- Declare a standard enum item in the symbol table.  The id is not
-- returned since we don't change with constants once they are set.
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow;                               -- raise error
  else                                                          -- otherwise
     declare
       sc : declaration renames identifiers( identifiers_top );
     begin
       if sc.avalue /= null then
          free( sc.avalue );
       end if;
       sc.name  := to_unbounded_string( name );                 -- define
       sc.kind  := kind;                                        -- identifier
       sc.value := to_unbounded_string( value );
       sc.class := enumClass;
       sc.static := true;                                       -- identifier
       sc.field_of := eof_t;
       -- since this is only called at startup, the default
       -- values for the other fields should be OK
     end;
     id := identifiers_top;
     identifiers_top := identifiers_top+1;                      -- push stack
  end if;
end declareStandardEnum;

procedure updateFormalParameter( id : identifier; kind : identifier;
proc_id : identifier; parameterNumber : integer ) is
-- Update a formal parameter (ie. proc.param).  The id is not
-- returned since we don't change the formal parameters once they are set.
begin
    if parameterNumber = 0 then -- function result? no name suffix
       identifiers(id).name     := identifiers( id ).name;
    else
       identifiers(id).name     := identifiers( proc_id ).name & "." & identifiers( id ).name;
    end if;
    identifiers(id).value    := to_unbounded_string( parameterNumber'img );
    identifiers(id).class    := constClass;
    identifiers(id).kind     := kind;
    identifiers(id).import   := false;
    identifiers(id).export   := false;
    identifiers(id).volatile := false;
    identifiers(id).static   := false;
    identifiers(id).limit    := false;
    identifiers(id).list     := false;
    identifiers(id).field_of := proc_id;
    identifiers(id).inspect  := false;
    identifiers(id).deleted  := false;
end updateFormalParameter;

-- DECLARE ACTUAL PARAMETER
--
-- Declare an actual parameter (ie. param for proc.param) in the symbol
-- table.  proc_id is the id for the subprogram owning the parameter.
-- value is ?
-- symbol_table_overflow exception is raised if there is no more
-- space in the symbol table
-----------------------------------------------------------------------------

procedure declareActualParameter( id : out identifier;
   proc_id : identifier; parameterNumber : integer;
   value : unbounded_string ) is
   paramName : unbounded_string;
begin
  id := eof_t;
  for i in reverse reserved_top..identifiers_top-1 loop
      if identifiers( i ).field_of = proc_id then
         if integer'value( to_string( identifiers( i ).value )) = parameterNumber then
            paramName := identifiers( i ).name;
            paramName := delete( paramName, 1, index( paramName, "." ));
            if identifiers_top = identifier'last then           -- no room?
               raise symbol_table_overflow;                     -- raise error
            else                                                -- otherwise
               id := identifiers_top;                           -- return id
               identifiers_top := identifiers_top+1;            -- push stack
               if identifiers( id ).avalue /= null then
                  free( identifiers( id ).avalue );
               end if;
               identifiers( id ) := declaration'(               -- define
                 name     => paramName,                         -- identifier
                 kind     => identifiers( i ).kind,
                 value    => value,
                 class    => constClass,
                 import   => false,
                 method   => none,
                 mapping  => none,
                 export   => false,
                 volatile => false,
                 static   => false,
                 limit    => false,
                 list     => false,
                 resource => false,
                 field_of => eof_t,
                 inspect  => false,
                 deleted  => false,
                 wasReferenced => false,
                 procCB => null,
                 funcCB => null,
                 genKind => eof_t,
                 genKind2 => eof_t,
                 openNamespace => identifiers'first,
                 nextNamespace => identifiers'first,
                 parentNamespace => identifiers'first,
                 firstBound => 1,
                 lastBound => 0,
                 avalue => null
               );
            end if;
         end if;
      end if;
  end loop;
end declareActualParameter;

-- DECLARE RETURN RESULT
--
-- Declare space for a return result for a user-defined function.  This is
-- effectively param 0 except that the value isn't zero...it's the return
-- result.
-- symbol_table_overflow exception is raised if there is no more
-- space in the symbol table
-----------------------------------------------------------------------------

procedure declareReturnResult( id : out identifier; func_id : identifier ) is
  paramName : unbounded_string;
begin
  paramName := "return result for " & identifiers( func_id ).name;
  if identifiers_top = identifier'last then           -- no room?
     raise symbol_table_overflow;                     -- raise error
  else                                                -- otherwise
     id := identifiers_top;                           -- return id
     identifiers_top := identifiers_top+1;            -- push stack
     if identifiers( id ).avalue /= null then
        free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(               -- define
       name     => paramName,                         -- identifier
       kind     => identifiers( func_id ).kind,
       value    => null_unbounded_string,
       class    => constClass,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatile => false,
       static   => false,
       limit    => false,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       wasReferenced => false,
       procCB => null,
       funcCB => null,
       genKind => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first,
       nextNamespace => identifiers'first,
       parentNamespace => identifiers'first,
       firstBound => 1,
       lastBound => 0,
       avalue => null
     );
  end if;
end declareReturnResult;

-- FIND EXCEPTION
--
-- search for a pre-existing exception with the same name
-- while Ada allows two exceptions with the same name
-- to overshadow each other, differentiating each other
-- using dot notation, it's a hacky solution.  SparForte
-- simply doesn't allow it.
-----------------------------------------------------------------------------

procedure findException( name : unbounded_string; id : out identifier ) is
begin
  id := eof_t;                                                  -- assume bad
  for i in reverse 1..identifiers_top-1 loop                    -- from top
      if i /= eof_t then
         if identifiers( i ).name = name and                    -- exists and
            identifiers( i ).kind = exception_t and not         -- and exception
            identifiers( i ).deleted then                       -- not deleted?
            id := i;                                            -- return id
            exit;                                               -- we're done
         end if;
      end if;
  end loop;
end findException;

-- DECLARE EXCEPTION
--
-- Declare an exception in the symbol table, where name is the exception
-- name.
-- symbol_table_overflow exception is raised if there is no more
-- space in the symbol table
-----------------------------------------------------------------------------

procedure declareException( id : out identifier; name : unbounded_string;
   default_message : unbounded_string; exception_status_code : anExceptionStatusCode ) is
-- Declare an exception.  Check for the existence first with findException.
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow;                               -- raise error
  else                                                          -- otherwise
     id := identifiers_top;                                  -- return id
     identifiers_top := identifiers_top+1;                   -- push stack
     if identifiers( id ).avalue /= null then
        free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(                      -- define
       name     => name,                                     -- identifier
       kind     => exception_t,
       value    => character'val( exception_status_code ) & default_message,
       class    => exceptionClass,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatile => false,
       static   => false,
       limit    => false,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       wasReferenced => false,
       procCB   => null,
       funcCB   => null,
       genKind  => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first,
       nextNamespace => identifiers'first,
       parentNamespace => identifiers'first,
       firstBound => 1,
       lastBound => 0,
       avalue   => null
     );
  end if;
end declareException;

-- DECLARE NAMESPACE
--
-- Declare a namespace entry in the symbol table, where name is the
-- namespace or package name.  This namespace will be a child of the current
-- namespace.  To open peer namespaces, the previous namespace must be closed
-- first.
-- symbol_table_overflow exception is raised if there is no more
-- space in the symbol table
-----------------------------------------------------------------------------

procedure declareNamespace( name : string ) is
  id : identifier;
begin
--put_line( "opening namespace " & name ); -- DEBUG
  if identifiers_top = Identifier'last then
     raise symbol_table_overflow;
  else
     id := identifiers_top;                                  -- return id
     identifiers_top := identifiers_top+1;                   -- push stack
     if identifiers( id ).avalue /= null then
        free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(                      -- define
       name     => to_unbounded_string( name ),              -- identifier
       kind     => identifiers'first,       -- TODO: this is a placeholder
       value    => currentNamespace,
       class    => namespaceClass,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatile => false,
       static   => false,
       limit    => false,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       wasReferenced => false,
       procCB   => null,
       funcCB   => null,
       genKind  => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first, -- TODO
       nextNamespace => lastNamespaceId,
       parentNamespace => currentNamespaceId,
       firstBound => 1,
       lastBound => 0,
       avalue   => null
     );

     -- The position of the last namespace tag (open or closed)
     -- for setting the nextNamespace link
     lastNamespaceId := id;

     currentNamespace := identifiers( id ).name;
     currentNamespaceId := id;

     -- TODO: Find the corresponding open tag based on nesting level
     -- do we need this here?

     -- sym := identifiers_top;
     --identifiers_top := identifiers_top + 1;
     -- reallocateToken( token_id => sym, class => namespaceClass );
     -- id := identifiers( sym );
     --id.class := namespaceClass;
     --currentNamespace := currentNamespace & "." & name;  -- OMIT?
     --id.name := to_unbounded_string( name );
     --id.kind := identifiers( Identifier'first );
     --id.value := new storage( 1..1 );
     --id.value(1) := currentNamespace;

     --id.openNamespace := null;
     --id.parentNamespace := currentNamespacePtr;
     --id.nextNamespace := null;
     -- even better if this is stored as a global someplace so remembers
     -- last one?
     --for tok in reverse identifiers'first..sym-1 loop
     --    if identifiers( tok ).class = namespaceClass then
     --       id.nextNamespace := identifiers( tok );
     --       exit;
     --    end if;
     --end loop;
     --id.nextNamespace := lastNamespace;
     --lastNamespace := id;

     --currentNamespacePtr := id;

     -- DEBUGGING NAMESPACES
     --put_line( "declaring namespace " & to_string( currentNamespace ) ); -- DEBUG
     --if identifiers( id ).parentNamespace = null then
     --   put_line( "   parent - NULL" ); -- DEBUG
     --elsif length( identifiers( id ).parentNamespace.name ) = 0 then
     --   put_line( "   parent - global" ); -- DEBUG
     --else
     --   put_line( "   parent - " & to_string( identifiers( id ).parentNamespace.name ) ); -- DEBUG
     --end if;
     --if identifiers( id ).nextNamespace = null then
     --   put_line( "   next - NULL" ); -- DEBUG
     --elsif length( identifiers( id ).nextNamespace.name ) = 0 then
     --   put_line( "   next - global" ); -- DEBUG
     --else
     --   put_line( "   next - " & to_string( identifiers( id ).nextNamespace.name ) ); -- DEBUG
     --end if;
  end if;
end declareNamespace;

procedure declareGlobalNamespace is
begin
  declareNamespace( "" );
end declareGlobalNamespace;

-- DECLARE NAMESPACE CLOSED
--
-- Declare a namespace entry in the symbol table, where name is the
-- namespace or package name to be closed.
-- symbol_table_overflow exception is raised if there is no more
-- space in the symbol table
-----------------------------------------------------------------------------

procedure declareNamespaceClosed( name : string ) is
  id : identifier;
  nesting : integer;

  --sym     : Identifier;
  --id      : declarationPtr;
  --p       : declarationPtr;
  p       : identifier;
begin
--put_line( "closing namespace " & name ); -- DEBUG
  if identifiers_top = Identifier'last then
     raise symbol_table_overflow;
  else
     id := identifiers_top;                                  -- return id
     identifiers_top := identifiers_top+1;                   -- push stack
     if identifiers( id ).avalue /= null then
        free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(                      -- define
       name     => to_unbounded_string( name ),              -- identifier
       kind     => identifiers'first,       -- TODO: this is a placeholder
       value    => currentNamespace,             -- the previous namespace
       class    => namespaceClass,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatile => false,
       static   => false,
       limit    => false,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       wasReferenced => false,
       procCB   => null,
       funcCB   => null,
       genKind  => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first, -- TODO
       nextNamespace => lastNamespaceId,
       parentNamespace => currentNamespaceId,
       firstBound => 1,
       lastBound => 0,
       avalue   => null
     );
--put_line( "   last namespace was " & to_string( currentNamespace ) ); -- DEBUG

     -- The position of the last namespace tag (open or closed)
     -- for setting the nextNamespace link
     lastNamespaceId := id;

---

  --if identifiers_top = Identifier'last then
  --   raise symbol_table_overflow;
  --else
  --   sym := identifiers_top;
  --   identifiers_top := identifiers_top + 1;
  --   reallocateToken( token_id => sym, class => namespaceClass );
  --   id := identifiers( sym );
  --   id.kind := identifiers( Identifier'first );
  --   id.value := new storage( 1..1 );
     --id.nextNamespace := null;
     -- even better if this is stored as a global someplace so remembers
     -- last one?
     --for tok in reverse identifiers'first..sym-1 loop
     --    if identifiers( tok ).class = namespaceClass then
     --       id.nextNamespace := identifiers( tok );
     --       exit;
     --    end if;
     --end loop;
     --if tail( currentNamespace, name'length ) /= name then
     --   put_line( "unexpected namespace to close: " & name );
     --else
     --   id.name := to_unbounded_string( name );
     --   id.value(1) := currentNamespace;
--put_line( "closing to " & to_string( currentNamespace ) ); -- DEBUG
--        delete( currentNamespace, length( currentNamespace ) - name'length,
--          length( currentNamespace ) );
--put_line( "   closing to namespace " & to_string( currentNamespace ) ); -- DEBUG
--     end if;

     --id.nextNamespace := lastNamespace;
     --lastNamespace := id;

     -- Find the corresponding open tag based on nesting level
     --
     -- if there is no openNamespace link, it is a close tag

--     put_line( "   finding open tag..." ); -- DEBUG
     nesting := -1;
     p := id;
     loop
        p := identifiers( p ).nextNamespace;
        exit when p = identifiers'first;
        if identifiers( p ).openNamespace = identifiers'first then
--           put_line( "   open tag " & to_string( p.name ) ); -- DEBUG
           nesting := nesting + 1;
        else
--           put_line( "   close tag " & to_string( p.name ) ); -- DEBUG
           nesting := nesting -1;
        end if;
        exit when nesting = 0;
     end loop;
     if p /= identifiers'first then
--        if length( p.name ) = 0 then
--           put_line( "   open tag set to global" ); -- DEBUG
--        else
--           put_line( "   open tag set to " & to_string( p.name ) ); -- DEBUG
--        end if;
        identifiers( id ).openNamespace := p;
     else
        put_line( "internal error: open namespace tag not found" );
     end if;
     identifiers( id ).parentNamespace := identifiers( identifiers( id ).openNamespace ).parentNamespace;
--     put_line( "   open tag " & to_string( identifiers( id ).openNamespace.name ) ); -- DEBUG
--     if identifiers( id ).parentNamespace = null then
--        put_line( "   parent - NULL" ); -- DEBUG
--     elsif length( identifiers( id ).parentNamespace.name ) = 0 then
--        put_line( "   parent - global" ); -- DEBUG
--     else
--        put_line( "   parent - " & to_string( identifiers( id ).parentNamespace.name ) ); -- DEBUG
--     end if;
--     if identifiers( id ).nextNamespace = null then
--        put_line( "   next - NULL" ); -- DEBUG
--     elsif length( identifiers( id ).nextNamespace.name ) = 0 then
--        put_line( "   next - global" ); -- DEBUG
--     else
--        put_line( "   next - " & to_string( identifiers( id ).nextNamespace.name ) ); -- DEBUG
--     end if;

     currentNamespaceId := identifiers( id ).parentNamespace;
  end if;
end declareNamespaceClosed;


-- Type Conversions


function to_numeric( s : unbounded_string ) return long_float is
-- Convert an unbounded string to a long float (BUSH's numeric representation)
begin
  if Element( s, 1 ) = '-' then                               -- leading -?
     return long_float'value( to_string( s ) );               -- OK for 'value
  elsif Element( s, 1 ) = ' ' then                            -- leading space?
     return long_float'value( to_string( s ) );               -- OK for 'value
  else                                                        -- otherwise add
     return long_float'value( " " & to_string( s ) );         -- space & 'value
  end if;
end to_numeric;

function to_numeric( id : identifier ) return long_float is
-- Look up an identifier's value and return it as a long float
-- (BUSH's numeric representation).
begin
   return to_numeric( identifiers( id ).value );
end to_numeric;

function to_unbounded_string( f : long_float ) return unbounded_string is
-- Convert a long_float (BUSH's numeric representation) to an
-- unbounded string.  If the value is representable as an integer,
-- it is returned without a decimal part.
  f_trunc : long_float := long_float'truncation( f );
begin

  -- integer value?  Try to return without a decimal part
  -- provided it will fit into a long float's mantissa.

   if f - f_trunc = 0.0 then
      -- There's no guarantee that a long_long_integer will fit into
      -- a long_float's mantissa, so we'll use a decimal type.
      if f <= long_float( integerOutputType'last ) and
         f >= long_float( integerOutputType'first ) then
         return to_unbounded_string( long_long_integer( f )'img );
      end if;
   end if;

  -- Otherwise, return a long float using 'image

   return to_unbounded_string( long_float'image( f ) );
end to_unbounded_string;

function To_Bush_Boolean( AdaBoolean : boolean ) return unbounded_string is
  -- convert an Ada boolean into a BUSH boolean (a string containing
  -- the position, no leading blank).
begin
  return To_Unbounded_String( integer'image( boolean'pos( AdaBoolean ) )(2)&"" );
end To_Bush_Boolean;

-----------------------------------------------------------------------------
-- IS KEYWORD
--
-- TRUE if the identifier is a keyword.
-----------------------------------------------------------------------------

function is_keyword( id : identifier ) return boolean is
-- True if an AdaScript keyword.  Keywords are defined in the
-- first part of the indentifier table.
begin
  --return id < reserved_top;
  return id < keywords_top;
end is_keyword;

-----------------------------------------------------------------------------
-- TO HIGH ASCII
--
-- Set the high bit (bit 8) on a low ASCII (7 bit) character.  Also, same for
-- an identifier to a high ASCII character.
-----------------------------------------------------------------------------

function toHighASCII( ch : character ) return character is
   pragma suppress( RANGE_CHECK );
   -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
   -- exception.  We'll do the range checking manually as a work around...
begin
   if ch > ASCII.DEL then
      put_line( standard_error, Gnat.Source_Info.Source_Location & ": Internal error: cannot set high bit on character" & character'pos( ch )'img );
      raise PROGRAM_ERROR;
   end if;
   return character'val( 128+character'pos( ch ) );
end toHighASCII;

function toHighASCII( id : identifier ) return character is
   pragma suppress( RANGE_CHECK );
   -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
   -- exception.  We'll do the range checking manually as a work around...
begin
   if id > 127 then
      put_line( standard_error, Gnat.Source_Info.Source_Location & ": Internal error: cannot set high bit on identifier number" & id'img );
      raise PROGRAM_ERROR;
   end if;
   return character'val( 128+integer(id) );
end toHighASCII;

--procedure toByteCode( n  : byteCodeNatural;   ch1, ch2 : out character ) is
--begin
--  ch1 := character'val( 128 + integer(n) mod 128 );
--  ch2 := character'val( integer(n) / 128 + 1 );
--exception when constraint_error =>
--  put_line( standard_error, Gnat.Source_Info.Source_Location & ": Internal error: cannot encode natural number" & n'img );
--  raise;
--end toByteCode;

-----------------------------------------------------------------------------
-- TO BYTE CODE
--
-- Encodes an id number into one or two bytes.  Zero is reserved as the
-- end-of-line marker.  The second byte may exceed ASCII 128.  When an
-- identifier is one byte, ch2 is ASCII.NUL
--
-- First    Second Values   Comment
--   1..127 -      -        reserved for ASCII
-- 128..223 -      1..96    first 96 reserved identifiers, one byte
-- 224..255 1..255 96..8192 values  remaining identifiers, two bytes
--
-- The objective is to allow for more than 128 reserved words, while at the
-- same time, keeping the most common reserved words in a single byte.
-----------------------------------------------------------------------------

procedure toByteCode( id : reservedWordRange; ch1, ch2 : out character ) is
begin
  if id <= 96 then
     ch1 := character'val( 128 + integer(id-1) );
     ch2 := ASCII.NUL;
  else
     ch1 := character'val( 224 + ((integer(id-1)- 32) mod 32) );
     ch2 := character'val( (integer(id-1)-32) / 32 + 1 );
  end if;
exception when constraint_error =>
  ch1 := ASCII.DEL;
  ch2 := ASCII.DEL;
  put_line( standard_error, Gnat.Source_Info.Source_Location & ": Internal error: cannot encode natural number" & id'img );
end toByteCode;

function toByteCode( id : reservedWordRange ) return string is
  ch1, ch2 : character;
begin
  toByteCode( id, ch1, ch2 );
  if ch2 = ASCII.NUL then
     return ch1 & "";
  end if;
  return ch1 & ch2;
end toByteCode;

-----------------------------------------------------------------------------
-- TO IDENTIFIER
--
-- Decode a two-character byte code sequence into an identifier.  See
-- toByteCode for the format the characters.  advance is the number of
-- characters decoded (1 = ch1 only, 2 = both characters ).
-----------------------------------------------------------------------------

procedure toIdentifier( ch1, ch2 : character; id : out reservedWordRange; advance : out integer ) is
  i1 : reservedWordRange := character'pos( ch1 );
  i2 : reservedWordRange;
begin
  if i1 < 128 then
     put_line( standard_error, Gnat.Source_Info.Source_Location & ": Internal error: byte code sequence is not an identifier: " & i1'img );
     advance := 1;
  elsif i1 < 224 then
     id := reservedWordRange( i1 - 127 );
     advance := 1;
  else
     i2 := character'pos( ch2 );
     id := reservedWordRange( i1 - 223 ) + 32 * reservedWordRange( i2 );
     advance := 2;
  end if;
end toIdentifier;

-----------------------------------------------------------------------------
-- TO LOW ASCII
--
-- Clear the high bit (bit 8) on a high ASCII (7 bit) character.  Also, same
-- for an identifier to a low ASCII character.
-----------------------------------------------------------------------------

function toLowASCII( ch : character ) return character is
   pragma suppress( RANGE_CHECK );
   -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
   -- exception.  We'll do the range checking manually as a work around...
begin
   if ch <= ASCII.DEL then
      put_line( standard_error, Gnat.Source_Info.Source_Location & ": Internal error: cannot clear high bit on character" & character'pos( ch )'img );
      raise PROGRAM_ERROR;
   end if;
   return character'val( character'pos( ch )-128 );
end toLowASCII;

function toLowASCII( id : identifier ) return character is
   pragma suppress( RANGE_CHECK );
   -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
   -- exception.  We'll do the range checking manually as a work around...
begin
   if id <= 128 then
      put_line( standard_error, Gnat.Source_Info.Source_Location & ": Internal error: cannot clear high bit on identifier number" & id'img );
      raise PROGRAM_ERROR;
   end if;
   return character'val( integer(id)-128 );
end toLowASCII;

-----------------------------------------------------------------------------

function ">"( left, right : aShellWord ) return boolean is
begin
  return left.word > right.word;
end ">";

function ">="( left, right : aSourceFile ) return boolean is
begin
  return left.name >= right.name;
end ">=";

function equal( left, right : aSourceFile ) return boolean is
begin
  return left.pos = right.pos;
end equal;

procedure findField( recordVar : identifier; fieldNumber: natural;
  fieldVar : out identifier ) is
-- Find fieldNumber'th field of a record varable.  This involves looking up the
-- record type, searching for the type's fields, checking the type field's
--  numbers, constructiong the name of the record's field, and finding it in
-- the symbol table.  This is not very fast but it works.
  recordType : identifier;
begin
  recordType := identifiers( recordVar ).kind;
  fieldVar := eof_t;
  for candidateType in reverse reserved_top..identifiers_top-1 loop
      if identifiers( candidateType ).field_of = recordType then
         if integer'value( to_string( identifiers( candidateType ).value )) = fieldNumber then
            declare
               fieldName : unbounded_string;
               field_t   : identifier;
               dotPos    : natural;
            begin
               fieldName := identifiers( candidateType ).name;
               dotPos := length( fieldName );
               while dotPos > 1 loop
                     exit when element( fieldName, dotPos ) = '.';
                     dotPos := dotPos - 1;
               end loop;
               fieldName := delete( fieldName, 1, dotPos );
               fieldName := identifiers( recordVar ).name & "." & fieldName;
               findIdent( fieldName, field_t );
               fieldVar := field_t;
            end;
         end if;
      end if;
  end loop;
end findField;

-- CHECK AND INITIALIZE LOCAL MEMCACHE CLUSTER

procedure checkAndInitializeLocalMemcacheCluster is
begin
  if not localMemcacheClusterInitialized then
     localMemcacheClusterInitialized := true;
     RegisterServer( localMemcacheCluster, to_unbounded_string( "localhost" ), 11211 );
     SetClusterName( localMemcacheCluster, to_unbounded_string( "Local Memcache" ) );
     SetClusterType( localMemcacheCluster, normal );
  end if;
end checkAndInitializeLocalMemcacheCluster;

-- CHECK AND INITIALIZE DISTRIBLED MEMCACHE CLUSTER

procedure checkAndInitializeDistributedMemcacheCluster is
begin
  if not localMemcacheClusterInitialized then
     distributedMemcacheClusterInitialized := true;
     SetClusterName( distributedMemcacheCluster, to_unbounded_string( "Distributed Memcache" ) );
     SetClusterType( distributedMemcacheCluster, normal );
  end if;
end checkAndInitializeDistributedMemcacheCluster;

--  GET IDENTIFIER CLASS NAME
--
-- Return a string description of the class

function getIdentifierClassImage( c : anIdentifierClass ) return string is
begin
  case c is
  when constClass       => return "constant";
  when subClass         => return "subtype";
  when typeClass        => return "type";
  when funcClass        => return "built-in function";
  when userFuncClass    => return "user-defined function";
  when procClass        => return "built-in procedure";
  when userProcClass    => return "user-defined procedure";
  when taskClass        => return "task";
  when mainProgramClass => return "main program";
  when exceptionClass   => return "exception";
  when varClass         => return "variable";
  when namespaceClass   => return "namespace";
  when enumClass        => return "enumerated item";
  when policyClass      => return "policy block";
  when configurationClass => return "configuration block";
  when otherClass       => return "other class";
  end case;
end getIdentifierClassImage;


--  PUT TEMPLATE HEADER
--
-- Output the template header.  Mark it as sent so it isn't sent
-- twice.  This will likely be replaced in the future.
--
-- A bad status results in HTTP 500.  A bad content type results in text.
-- In both cases, a message is written to standard error.
-----------------------------------------------------------------------------

procedure putTemplateHeader( header : in out templateHeaders ) is
  s : unbounded_string;
begin

  if header.templateHeaderSent then
     return;
  end if;
  header.templateHeaderSent := true;

  -- CGI programs cannot directly return HTTP 404.  Instead, it must use
  -- a Status: header

  s := to_unbounded_string( "Status: " & header.status'img & " " );
  case header.status is
  when 100 => s := s & "Continue";
  when 200 => s := s & "OK";
  when 201 => s := s & "Created";
  when 202 => s := s & "Accepted";
  when 203 => s := s & "Non-Authoritative";
  when 204 => s := s & "No Content";
  when 205 => s := s & "Reset Content";
  when 206 => s := s & "Partial Content";
  when 300 => s := s & "Multiple Choices";
  when 301 => s := s & "Moved Permanently";
  when 302 => s := s & "Found";
  when 303 => s := s & "See Other";
  when 304 => s := s & "Not Modified";
  when 305 => s := s & "Use Proxy";
  when 307 => s := s & "Temporary Redirect";
  when 400 => s := s & "Bad Request";
  when 401 => s := s & "Unauthorized";
  when 403 => s := s & "Forbidden";
  when 404 => s := s & "Not Found";
  when 405 => s := s & "Method Not Allowed";
  when 406 => s := s & "Not Acceptable";
  when 407 => s := s & "Proxy Authentication Required";
  when 408 => s := s & "Request Timeout";
  when 409 => s := s & "Conflict";
  when 410 => s := s & "Gone";
  when 411 => s := s & "Length Required";
  when 412 => s := s & "Precondition Failed";
  when 413 => s := s & "Request Entity Too Large";
  when 414 => s := s & "Request-URI Too Long";
  when 415 => s := s & "Unsupported Media Type";
  when 416 => s := s & "Requested Range Not Satisfiable";
  when 417 => s := s & "Expectation Failed";
  when 500 => s := s & "Internal Server Error";
  when 501 => s := s & "Not Implemented";
  when 502 => s := s & "Bad Gateway";
  when 503 => s := s & "Service Unavailable";
  when 504 => s := s & "Gateway Timeout";
  when 505 => s := s & "HTTP Version Not Supported";
  when others => -- includes 500
     put_line( standard_error, "internal error: unknown http status" );
     s := to_unbounded_string( "HTTP/1.1 500 Internal Server Error" );
  end case;
  s := s & ASCII.CR & ASCII.LF;

--  put( s & ASCII.CR & ASCII.LF );

  s := s & to_unbounded_string( "Content-type: " );
  case header.templateType is
  when htmlTemplate => s := s & "text/html";
  when cssTemplate  => s := s & "text/css";
  when jsTemplate   => s := s & "application/x-javascript";
  when jsonTemplate => s := s & "application/json";
  when textTemplate => s := s & "text/plain";
  when xmlTemplate  => s := s & "text/xml";
  when wmlTemplate  => s := s & "text/vnd.wap.wml";
  when others => -- includes textTemplate
     put_line( standard_error, "internal error: unknown template type" );
     s := to_unbounded_string( "Content-type: text/plain" );
  end case;
  s := s & ASCII.CR & ASCII.LF;
  -- put( s & ASCII.CR & ASCII.LF );

  -- Other fields (to be filled in later)

  if length( header.contentLength ) > 0 then
     s := s & "Content-length: " & header.contentLength & ASCII.CR & ASCII.LF;
  end if;

  if length( header.expires ) > 0 then
     s := s & "Expires: " & header.expires & ASCII.CR & ASCII.LF;
  end if;

  if length( header.location ) > 0 then
     s := s & "Location: " & header.location & ASCII.CR & ASCII.LF;
  end if;

  if length( header.pragmaString ) > 0 then
     s := s & "Pragma: " & header.pragmaString & ASCII.CR & ASCII.LF;
  end if;

  if length( header.cookieString ) > 0 then
     s := s & "Set-Cookie: " & header.pragmaString & ASCII.CR & ASCII.LF;
  end if;

  -- The HTTP header ends with a blank line

  put( s & ASCII.CR & ASCII.LF );

end putTemplateHeader;

end world;
