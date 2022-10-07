------------------------------------------------------------------------------
-- Common declarations across most of SparForte/BUSH including              --
-- command line switches and the symbol table.                              --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2022 Free Software Foundation            --
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
pragma ada_2005;

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );

with system,
    ada.text_io,
    ada.strings.unbounded.text_io,
    gnat.source_info;
use ada.text_io,
    ada.command_line,
    ada.command_line.environment,
    ada.strings.unbounded.text_io;

package body world is

localMemcacheClusterInitialized : boolean := false;
distributedMemcacheClusterInitialized : boolean := false;
-- flag: only initialize the memcache clusters once

defaultVolatileExpire : constant time := clock;

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


---> IS EXECUTING COMMAND
--
-- True if OK to execute a statement that does work.
-- That is, the parser isn't skipping the line because of
-- an error or exiting a block.
-----------------------------------------------------------------------------

function isExecutingCommand return boolean is
begin
  return not error_found and not exit_block and not syntax_check;
end isExecutingCommand;

-- The same, but can run during syntax checking.  Used for static
-- expressions.

function isExecutingStaticCommand return boolean is
begin
  return not error_found and not exit_block and not
     (interpreterPhase = executing and syntax_check);
end isExecutingStaticCommand;


-----------------------------------------------------------------------------
-- STORAGE CACHE
--
-- Allocating/deallocating memory is a slow operation.
--
-- To make slightly better use of allocated storage memory, instead of
-- freeing the last storage pointer, store it here so it can be resued.
--
-- This is the same technique I used in my single-linked list library.
--
-- Since only arrays currently use storage pointers, this only affects arrays.
-- I didn't see much of a speed improvement.
-----------------------------------------------------------------------------

storageCache     : storagePtr := null;
storageCacheMiss : natural := 0;

-- CACHE OR FREE STORAGE
--
-- Cache or destory storage pointer sp.
-----------------------------------------------------------------------------

procedure cacheOrFreeStorage( sp : storagePtr ) is
begin
  if storageCache = null then
     storageCache := sp;
  else
     free( storageCache );
     storageCache := sp;
  end if;
end cacheOrFreeStorage;
pragma inline( cacheOrFreeStorage );

-- FIND STORAGE
--
-- Allocate storage space (or get it from the cache) and return a
-- pointer to it.  If what is in the cache is missed three times,
-- forcably discard it.
-----------------------------------------------------------------------------

function findStorage( lbound, ubound : long_integer ) return storagePtr is
  sp : storagePtr;
begin
  if storageCache = null then
     sp := new Storage( lbound..ubound );
     storageCacheMiss := 0;
  else
     if storageCacheMiss >= 3 then
        free( storageCache );
        storageCache := null;
        storageCacheMiss := 0;
     elsif storageCache'first = lbound and
           storageCache'last = ubound then
        sp := storageCache;
        storageCache := null;
        storageCacheMiss := 0;
     else
        sp := new Storage( lbound..ubound );
        storageCacheMiss := storageCacheMiss + 1;
     end if;
  end if;
  return sp;
end findStorage;

function to_string( mode : aParameterPassingMode ) return string is
begin
  case mode is
     when none =>
        return "none";
     when in_mode =>
        return "in";
     when out_mode =>
        return "out";
     when in_out_mode =>
        return "in out";
     when others =>
        raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location &
          ": internal error: unexpected mode ";
  end case;
end to_string;

-- Symbol Table Utilities
--

procedure declareKeyword( id : out identifier; s : string ) is
-- Initialize a keyword / internal identifier in the symbol table
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
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
       kw.value := kw.svalue'access;
       kw.svalue := Null_Unbounded_String;
       -- field_of is used while searching for fields.  It must always be
       -- set to a known value. eof_t may not not defined yet.
       kw.field_of := identifiers'first;
       kw.class := otherClass;
       kw.genKind := identifiers'first;
       kw.genKind2 := identifiers'first;
       -- since keywords are only declared at startup,
       -- the defaults should be OK for remaining fields.
     end;
  end if;
end declareKeyword;

procedure declareFunction( id : out identifier; s : string; cb : aBuiltinFunctionCallback := null ) is
-- Initialize a built-in function identifier in the symbol table
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
  else                                                          -- else add
     id := identifiers_top;                                     -- return id
     identifiers_top := identifiers_top + 1;                    -- push stack
     declare
       func : declaration renames identifiers( id );
     begin
       if func.avalue /= null then
          free( func.avalue );
       end if;
       func.name := To_Unbounded_String( s );
       func.kind := identifier'first;
       func.svalue := Null_Unbounded_String;
       func.value := func.svalue'access;
       func.class := funcClass;
       func.genKind := identifiers'first;
       func.genKind2 := identifiers'first;
       func.procCB := null;
       func.funcCB := cb;
       func.avalue := null;
       -- field_of is used while searching for fields.  It must always be
       -- set to a known value. eof_t may not not defined yet.
       func.field_of := identifier'first;
       func.renaming_of := identifier'first;
       func.renamed_count := 0;
     end;
  end if;
end declareFunction;

procedure declareProcedure( id : out identifier; s : string; cb : aBuiltinProcedureCallback := null ) is
-- Initialize a built-in procedure identifier in the symbol table
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
  else                                                          -- else add
     id := identifiers_top;                                     -- return id
     identifiers_top := identifiers_top + 1;                    -- push stack
     declare
       proc : declaration renames identifiers( id );
     begin
       if proc.avalue /= null then
          free( proc.avalue );
       end if;
       proc.name := To_Unbounded_String( s );
       proc.kind := identifier'first;
       proc.svalue := Null_Unbounded_String;
       proc.value := proc.svalue'access;
       proc.class := procClass;
       proc.genKind := identifiers'first;
       proc.genKind2 := identifiers'first;
       proc.procCB := cb;
       proc.funcCB := null;
       proc.avalue := null;
       -- field_of is used while searching for fields.  It must always be
       -- set to a known value. eof_t may not not defined yet.
       proc.field_of := identifier'first;
       proc.renaming_of := identifier'first;
       proc.renamed_count := 0;
     end;
  end if;
end declareProcedure;

procedure findIdent( name : unbounded_string; id : out identifier ) is
  -- Return the id of a keyword / identifier in the symbol table.  If
  -- it is not found, return the end-of-file token as the id
  -- This function gets called a lot
  -- These make a slight speed improvement
  pragma suppress( range_check );
  pragma suppress( index_check );
  i      : identifier;
  p      : identifier;
  save_i : identifier;
  dotPos : natural := 0;
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

 --put_line( "prefix = " & to_string( prefix ) ); -- debug

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

  -- I THINK THIS IS NO LONGER TRUE:
  -- global namespace types like integer will be tested here

  -- Quick Kludge
  --
  -- If there was no period in the name, don't both with a global search because
  -- you won't find it by searching twice.
  --
  -- dotPos will be zero if it was found in the local search.
  -- Skipping the brute-force search increases speed on my benchmark by 15%
  --
  -- TODO: refactor this.  The fallback should be eliminated, or folded in
  -- above.

  if dotPos <= 1 then
     --put_line( to_string( prefix ) );
     --put_line( dotPos'img );
     --if id = eof_t then
     --   put_line( "no prefix and not found: " & to_string( name ) );
     --else
     --   put_line( "no prefix and found: " & to_string( name ) );
     --end if;
     return;
  end if;

  -- The fallback should only run for records and enumerated types with a
  -- period in their name that we're found in the local search.

  -- fallback
  --
  -- brute-force from where we left off in the local search,
  -- searching everything
  -- records
  -- enums like direction.forward will go here since the prefix doesn't match the namespace tags

  if id = eof_t then
--put_line( standard_error, "brute force" ); -- DEBUG
     for i in reverse 1..save_i loop                         -- from local ns
         if i /= eof_t then                                  -- not this token
            if not identifiers( i ).deleted then             -- not deleted
               if identifiers( i ).name = name then          -- exists and
                  id := i;                                   -- return id
--put_line( standard_error, "internal error: brute force found " & to_string( name ) ); -- DEBUG
                  exit;                                      -- we're done
               end if;
            end if;
         end if;
     end loop;
  end if;
--put_line( standard_error, "findIdent: done" ); -- DEBUG
--if id = eof_t then -- DEBUG
   --put_line( "findIdent: not found" ); -- DEBUG
--end if; -- DEBUG

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
            if identifiers( i ).value.all = val then
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
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
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
       --svalue    => To_Unbounded_String( s(eqpos+1..s'last ) ),
       --value    => svalue'access,
       value    => null,
       class    => varClass,
       import   => true,                                        -- must import
       method   => shell,
       mapping  => none,
       export   => false,
       volatile => none,
       volatileTTL => 0.0,
       volatileExpire => defaultVolatileExpire,
       static   => false,
       usage    => fullUsage,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByThread => noThread,
       writtenOn => 0,
       wasApplied => false,
       wasFactor => false,
       wasCastTo => false,
       procCB => null,
       funcCB => null,
       genKind => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first,
       nextNamespace => identifiers'first,
       parentNamespace => identifiers'first,
       firstBound => 1,
       lastBound => 0,
       contract => null_unbounded_string,
       svalue    => To_Unbounded_String( s(eqpos+1..s'last ) ),
       avalue => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
     );
     -- svalue isn't defined until here
     identifiers( identifiers_top ).value := identifiers( identifiers_top ).svalue'access;
     identifiers_top := identifiers_top + 1;                    -- push stack
  end if;
end init_env_ident;

procedure declareIdent( id : out identifier; name : unbounded_string;
  kind : identifier; class : anIdentifierClass := varClass ) is
-- Declare an identifier in the symbol table, specifying name, kind.
-- and (optionally) symbol class.  The id is returned.
begin
  if identifiers_top = identifier'last then                     -- no room?
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
  else                                                          -- otherwise
     id := identifiers_top;                                     -- return id
     identifiers_top := identifiers_top+1;                      -- push stack
     if identifiers( id ).avalue /= null then
        cacheOrFreeStorage( identifiers( id ).avalue );
        --free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(                         -- define
       name     => name,                                        -- identifier
       kind     => kind,
       value    => null,
       class    => class,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatile => none,
       volatileTTL => 0.0,
       volatileExpire => defaultVolatileExpire,
       static   => false,
       usage    => identifiers( kind ).usage,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByThread => noThread,
       writtenOn => 0,
       wasApplied => false,
       wasFactor => false,
       wasCastTo => false,
       procCB => null,
       funcCB => null,
       genKind => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first,
       nextNamespace => identifiers'first,
       parentNamespace => identifiers'first,
       firstBound => 1,
       lastBound => 0,
       contract => null_unbounded_string,
       svalue => null_unbounded_string,
       avalue => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
     );
     -- svalue isn't defined until here
     identifiers( id ).value := identifiers( id ).svalue'access;
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
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
  else                                                          -- otherwise
     declare
       sc : declaration renames identifiers( identifiers_top );
     begin
       if sc.avalue /= null then
          cacheOrFreeStorage( sc.avalue );
       end if;
       sc.name  := to_unbounded_string( name );                 -- define
       sc.kind  := kind;                                        -- identifier
       sc.svalue := to_unbounded_string( value );
       sc.class := varClass;
       sc.genKind := identifiers'first;
       sc.genKind2 := identifiers'first;
       sc.static := true;                                       -- identifier
       sc.usage := constantUsage;
       sc.field_of := eof_t;
       sc.list := identifiers( kind ).list;
       sc.value := sc.svalue'access;
       sc.writtenByThread := noThread;
       sc.writtenOn := 0;
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
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
  else                                                          -- otherwise
     declare
       sc : declaration renames identifiers( identifiers_top );
     begin
       if sc.avalue /= null then
          cacheOrFreeStorage( sc.avalue );
       end if;
       sc.name  := to_unbounded_string( name );                 -- define
       sc.kind  := kind;                                        -- identifier
       sc.svalue := to_unbounded_string( value );
       sc.class := enumClass;
       sc.genKind := identifiers'first;
       sc.genKind2 := identifiers'first;
       sc.static := true;                                       -- identifier
       sc.usage := fullUsage;
       sc.field_of := eof_t;
       sc.value := sc.svalue'access;
       sc.writtenByThread := noThread;
       sc.writtenOn := 0;
       -- since this is only called at startup, the default
       -- values for the other fields should be OK
     end;
     id := identifiers_top;
     identifiers_top := identifiers_top+1;                      -- push stack
  end if;
end declareStandardEnum;

procedure updateFormalParameter( id : identifier; kind : identifier;
proc_id : identifier; parameterNumber : integer; passingMode : aParameterPassingMode ) is
-- Update a formal parameter (ie. proc.param).  The id is not
-- returned since we don't change the formal parameters once they are set.
begin
    if parameterNumber /= 0 then -- function result? no name suffix
       --identifiers(id).name     := identifiers( id ).name;
    --else
       identifiers(id).name     := identifiers( proc_id ).name & "." & identifiers( id ).name;
    end if;
    identifiers(id).svalue   := to_unbounded_string( parameterNumber'img );
    identifiers(id).class    := formalParamClass;
    identifiers(id).kind     := kind;
    identifiers(id).import   := false;
    identifiers(id).export   := false;
    identifiers(id).volatile := none;
    identifiers(id).static   := false;
    --identifiers(id).usage    := constantUsage;
    identifiers(id).list     := false;
    identifiers(id).field_of := proc_id;
    identifiers(id).inspect  := false;
    identifiers(id).deleted  := false;
    identifiers(id).passingMode  := passingMode;
    identifiers(id).genKind  := identifiers( kind ).genKind;
    identifiers(id).genKind2 := identifiers( kind ).genKind2;
    -- if a limited type is used for a parameter, the formal parameter
    -- must be also limited.
    if identifiers( kind ).usage = limitedUsage then
       identifiers( id ).usage := limitedUsage;
    end if;
end updateFormalParameter;

-- DECLARE USABLE FORMAL PARAMETER
--
-- Declare a usable formal parameter (ie. param for proc.param) in the symbol
-- table.  proc_id is the id for the subprogram owning the parameter.
-- value is ?
-- symbol_table_overflow exception is raised if there is no more
-- space in the symbol table
-- startAt should be identifiers_top -1 for the first iteration
-----------------------------------------------------------------------------

procedure declareUsableFormalParameter(
       id : out identifier;
       proc_id : identifier;
       parameterNumber : integer;
       value : unbounded_string;
       startingAt : in out identifier ) is
   paramName : unbounded_string;
   i : identifier;
   dir : integer;
begin
  id := eof_t;
  -- TODO: improve search of formal parameters
  -- one example is to remember where we left off, since they are probably
  -- together

  -- Search for the formal parameter
  --
  -- Slightly less brutal search than the old one:
  -- for i in reverse reserved_top..startingAt loop
  --
  -- To speed up this search a little, if we don't know where the identifiers
  -- are, start at the most recent and work backwards.  Otherwise, start
  -- where we left off and move forwards.
  --
  -- TODO: handle namespaces would even be better

  if startingAt = identifiers_top - 1 then
     i := startingAt;
     dir := -1;
  else
     i := startingAt;
     dir := +1;
  end if;

  startingAt := eof_t;
  while i >= reserved_top and i < identifiers_top loop
      if identifiers( i ).field_of = proc_id then
         if integer'value( to_string( identifiers( i ).value.all )) = parameterNumber then
            startingAt := identifier( integer( i ) + dir );
            exit;
         end if;
      end if;
      i := identifier( integer( i ) + dir );
  end loop;

  --- not found

  if startingAt = eof_t then
     return;
  end if;

  paramName := identifiers( i ).name;
  paramName := delete( paramName, 1, index( paramName, "." ));
  if identifiers_top = identifier'last then           -- no room?
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
  else                                                -- otherwise
     id := identifiers_top;                           -- return id
     identifiers_top := identifiers_top+1;            -- push stack
     if identifiers( id ).avalue /= null then
        cacheOrFreeStorage( identifiers( id ).avalue );
        --free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(               -- define
                 name     => paramName,                         -- identifier
                 kind     => identifiers( i ).kind,
                 --value    => svalue'access,
                 value    => null,
                 class    => varClass,
                 import   => false,
                 method   => none,
                 mapping  => none,
                 export   => false,
                 volatile => none,
                 volatileTTL => 0.0,
                 volatileExpire => defaultVolatileExpire,
                 static   => false,
                 --usage    => constantUsage,
                 usage    => identifiers( i ).usage,
                 list     => identifiers( identifiers( i ).kind ).list,   -- arrays now supported
                 resource => false,
                 field_of => eof_t,
                 inspect  => false,
                 deleted  => false,
                 specAt   => noSpec,
                 specFile => null_unbounded_string,
                 wasReferenced => false,
                 --referencedByThread => noThread,
                 wasWritten => false,
                 writtenByThread => noThread,
                 writtenOn => 0,
                 wasApplied => false,
                 wasFactor => false,
                 wasCastTo => false,
                 procCB => null,
                 funcCB => null,
                 genKind => identifiers( i ).genKind,
                 genKind2 => identifiers( i ).genKind2,
                 openNamespace => identifiers'first,
                 nextNamespace => identifiers'first,
                 parentNamespace => identifiers'first,
                 firstBound => 1,
                 lastBound => 0,
                 contract => null_unbounded_string,
                 svalue => value,
                 avalue => null,
                 renaming_of => identifier'first,
                 renamed_count => 0,
                 passingMode => identifiers( i ).passingMode
     );
     -- svalue isn't defined until here
     identifiers( id ).value := identifiers( id ).svalue'access;
     -- out or in out can be assigned to
     --if identifiers( i ).passingMode = out_mode or
     --   identifiers( i ).passingMode = in_out_mode then
     --   identifiers( id ).usage := fullUsage;
     --end if;
     if identifiers( i ).passingMode = in_mode and
        identifiers( i ).usage = fullUsage then
        identifiers( id ).usage := constantUsage;
     end if;
  end if;

end declareUsableFormalParameter;

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
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
  else                                                -- otherwise
     id := identifiers_top;                           -- return id
     identifiers_top := identifiers_top+1;            -- push stack
     if identifiers( id ).avalue /= null then
        cacheOrFreeStorage( identifiers( id ).avalue );
        --free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(               -- define
       name     => paramName,                         -- identifier
       kind     => identifiers( func_id ).kind,
       value    => null,
       --value    => svalue'access,
       class    => varClass,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatile => none,
       volatileTTL => 0.0,
       volatileExpire => defaultVolatileExpire,
       static   => false,
       usage    => constantUsage,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByThread => noThread,
       writtenOn => 0,
       wasApplied => false,
       wasFactor => false,
       wasCastTo => false,
       procCB => null,
       funcCB => null,
       genKind => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first,
       nextNamespace => identifiers'first,
       parentNamespace => identifiers'first,
       firstBound => 1,
       lastBound => 0,
       contract => null_unbounded_string,
       svalue => null_unbounded_string,
       avalue => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
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
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
  else                                                          -- otherwise
     id := identifiers_top;                                  -- return id
     identifiers_top := identifiers_top+1;                   -- push stack
     if identifiers( id ).avalue /= null then
        cacheOrFreeStorage( identifiers( id ).avalue );
        --free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(                      -- define
       name     => name,                                     -- identifier
       kind     => exception_t,
       --value    => svalue'access,
       value    => null,
       class    => exceptionClass,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatileTTL => 0.0,
       volatileExpire => defaultVolatileExpire,
       volatile => none,
       static   => false,
       usage    => fullUsage,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByThread => noThread,
       writtenOn => 0,
       wasApplied => false,
       wasFactor => false,
       wasCastTo => false,
       procCB   => null,
       funcCB   => null,
       genKind  => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first,
       nextNamespace => identifiers'first,
       parentNamespace => identifiers'first,
       firstBound => 1,
       lastBound => 0,
       contract => null_unbounded_string,
       svalue    => character'val( exception_status_code ) & default_message,
       avalue   => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
     );
     -- svalue isn't defined until here
     identifiers( id ).value := identifiers( id ).svalue'access;
  end if;
end declareException;

-- DECLARE RENAMING
--
-- Declare a renaming in the symbol table, based on the given reference.
-- This does not setup aggregate renamings, which must be done by the
-- caller.  This also assumes that a new identifier is already created.
-- The canonical identifier will have its renaming count incremented.
-----------------------------------------------------------------------------

procedure declareRenaming( new_id : identifier; canonicalRef :
  renamingReference ) is
begin
  -- assumes a new identifier (kind = new_t) was previously declared

  --if identifiers( new_id ).avalue /= null then
  --   free( identifiers( new_id ).avalue );
  --end if;

  -- To do a renaming, you cannot do a straight copy of identifiers.
  -- The structure is too complicated and it will destroy the name and
  -- array storage, etc.

     identifiers( new_id ) := declaration'(                     -- define
       name     => identifiers( new_id ).name,                  -- identifier
       kind     => identifiers( canonicalRef.id ).kind,
       value    => null,
       class    => identifiers( canonicalRef.id ).class,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatile => none,
       volatileTTL => 0.0,
       volatileExpire => defaultVolatileExpire,
       static   => identifiers( canonicalRef.id ).static,
       usage    => identifiers( new_id ).usage,
       list     => identifiers( canonicalRef.id ).list,
       resource => false,  -- can't really look up a resource
       field_of => identifiers( canonicalRef.id ).field_of,
       inspect  => identifiers( canonicalRef.id ).inspect,
       deleted  => false,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByThread => noThread,
       writtenOn => 0,
       wasApplied => false,
       wasFactor => false,
       wasCastTo => false,
       procCB => identifiers( canonicalRef.id ).procCB,  -- don't apply for variables
       funcCB => identifiers( canonicalRef.id ).funcCB,
       genKind => identifiers( canonicalRef.id ).genKind,
       genKind2 => identifiers( canonicalRef.id ).genKind2,
       openNamespace => identifiers'first,  -- current namespace
       nextNamespace => identifiers'first,
       parentNamespace => identifiers'first,
       firstBound => identifiers( canonicalRef.id ).firstBound,
       lastBound => identifiers( canonicalRef.id ).lastBound,
       contract => null_unbounded_string,
       -- not sure we need to copy svalue as it is not normally
       -- read directly.
       svalue => identifiers( canonicalRef.id ).svalue,
       -- we need to refer to the canonical variables' avalue to
       -- do array bounds tests in the parser...
       avalue => identifiers( canonicalRef.id ).avalue,
       renaming_of => canonicalRef.id,
       renamed_count => 0,
       passingMode => none
     );

     -- The canonical reference returned by ParseRenamingReference
     -- isn't the root canonical variable.  So we must follow the chain
     -- and assign value to the svalue of the root canonical.
     --identifiers( new_id ).value := identifiers( canonicalRef.id ).svalue'access;

     declare
        deref_id : identifier := canonicalRef.id;
        cnt : natural := 1;
     begin
        while identifiers( deref_id ).renaming_of /= identifiers'first loop
           deref_id := identifiers( deref_id ).renaming_of;
           cnt := cnt + 1;
           if cnt > 1000 then
              raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location &
                 ": internal error: infinite renaming loop";
           end if;
        end loop;
        identifiers( new_id ).value := identifiers( deref_id ).svalue'access;
     end;

     -- if the renaming is an array element, the type is the type of the
     -- array element, not the array type itself.  The element type should
     -- just be the "kind".
     if identifiers( canonicalRef.id ).list and canonicalRef.hasIndex then
        identifiers( new_id ).list := false;
     end if;
     identifiers( canonicalRef.id ).renamed_count :=
        identifiers( canonicalRef.id ).renamed_count + 1;

end declareRenaming;

-- DECLARE RECORD FIELDS
--
-- Currently, records are implemented as individual variables.  This must
-- be generated whenever a new record is created.
--
-- Given record r of type t, produce variables r.a, r.b ... modelled on
-- t.a, t.b.

procedure declareRecordFields( parentRecordOfFieldsId, recordBaseTypeId : identifier ) is
   numFields : natural;
   recordTypeId : identifier;
   j : identifier;
begin
   recordTypeId := identifiers( parentRecordOfFieldsId ).kind;

   begin
      if recordBaseTypeId = root_record_t then
         raise SPARFORTE_ERROR with gnat.source_info.source_location &
           "internal errror: unexpected type " &
          to_string( identifiers( recordBaseTypeId ).name );
      end if;
      numFields := natural( to_numeric( identifiers( recordBaseTypeId ).value.all ) );
   exception when others =>
      put_line( standard_error, gnat.source_info.source_location &
        "internal errror: unable to get number of fields for " &
        to_string( identifiers( recordBaseTypeId ).name ) );
      raise;
   end;
   for i in 1..numFields loop

      -- brutal search was ...
      -- for j in reverse 1..identifiers_top-1 loop
      --
      -- As an optimization, the fields are likely located immediately after
      -- the record itself is defined.  Also assumes they are stored
      -- sequentially.  In the future, records will be stored differently.

     j := recordTypeId + 1;
     while j < identifiers_top loop
        -- TODO: should this be the base record type?  subtypes may break it
        if identifiers( j ).field_of = recordTypeId then
           if integer'value( to_string( identifiers( j ).value.all )) = i then
              exit;
           end if;
        end if;
        j := identifier( integer( j ) + 1 );
      end loop;

     -- no more identifiers means we didn't find it.
     if j = identifiers_top then
        raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location &
          ": internal error: record field not found";
     end if;

     declare
        fieldName   : unbounded_string;
        field_id    : identifier;
        dotPos      : natural;
     begin
        fieldName := identifiers( j ).name;
        dotPos := length( fieldName );
        while dotPos > 1 loop
              exit when element( fieldName, dotPos ) = '.';
              dotPos := dotPos - 1;
        end loop;
        fieldName := delete( fieldName, 1, dotPos );
        fieldName := identifiers( parentRecordOfFieldsId ).name &
           "." & fieldName;
        declareIdent( field_id, fieldName, identifiers( j ).kind, varClass );
           -- fields have not been marked as children of the parent
           -- record.  However, to make sure the record is used, it
           -- is convenient to track the field.
        identifiers( field_id ).field_of := parentRecordOfFieldsId;
           -- at least, for now, don't worry if record fields are
           -- declared but not accessed.  We'll just check the
           -- main record identifier.
        if syntax_check then
           identifiers( field_id ).wasReferenced := true;
           --identifiers( field_id ).referencedByThread := noThread;
           identifiers( field_id ).wasWritten := true;
           identifiers( field_id ).writtenByThread := noThread;
           identifiers( field_id ).wasApplied := false;
           identifiers( field_id ).wasFactor := false;
           identifiers( field_id ).wasCastTo := false;
           identifiers( field_id ).writtenOn := 0;
        end if;
     end;
     j := identifier( integer( j ) + 1 );
   end loop;
end declareRecordFields;

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
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
  else
     id := identifiers_top;                                  -- return id
     identifiers_top := identifiers_top+1;                   -- push stack
     if identifiers( id ).avalue /= null then
        cacheOrFreeStorage( identifiers( id ).avalue );
        --free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(                      -- define
       name     => to_unbounded_string( name ),              -- identifier
       kind     => identifiers'first,       -- TODO: this is a placeholder
       value    => null,
       class    => namespaceClass,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatile => none,
       volatileTTL => 0.0,
       volatileExpire => defaultVolatileExpire,
       static   => false,
       usage    => fullUsage,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByThread => noThread,
       writtenOn => 0,
       wasApplied => false,
       wasFactor => false,
       wasCastTo => false,
       procCB   => null,
       funcCB   => null,
       genKind  => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first, -- TODO
       nextNamespace => lastNamespaceId,
       parentNamespace => currentNamespaceId,
       firstBound => 1,
       lastBound => 0,
       contract => null_unbounded_string,
       svalue    => currentNamespace,
       avalue   => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
     );
     -- svalue isn't defined until here
     identifiers( id ).value := identifiers( id ).svalue'access;

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
     raise symbol_table_overflow with Gnat.Source_Info.Source_Location &
       ": too many identifiers";
  else
     id := identifiers_top;                                  -- return id
     identifiers_top := identifiers_top+1;                   -- push stack
     if identifiers( id ).avalue /= null then
        cacheOrFreeStorage( identifiers( id ).avalue );
        --free( identifiers( id ).avalue );
     end if;
     identifiers( id ) := declaration'(                      -- define
       name     => to_unbounded_string( name ),              -- identifier
       kind     => identifiers'first,       -- TODO: this is a placeholder
       --value    => svalue'access,
       value    => null,
       class    => namespaceClass,
       import   => false,
       method   => none,
       mapping  => none,
       export   => false,
       volatile => none,
       volatileTTL => 0.0,
       volatileExpire => defaultVolatileExpire,
       static   => false,
       usage    => fullUsage,
       list     => false,
       resource => false,
       field_of => eof_t,
       inspect  => false,
       deleted  => false,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByThread => noThread,
       writtenOn => 0,
       wasApplied => false,
       wasFactor => false,
       wasCastTo => false,
       procCB   => null,
       funcCB   => null,
       genKind  => eof_t,
       genKind2 => eof_t,
       openNamespace => identifiers'first, -- TODO
       nextNamespace => lastNamespaceId,
       parentNamespace => currentNamespaceId,
       firstBound => 1,
       lastBound => 0,
       contract => null_unbounded_string,
       svalue    => currentNamespace,             -- the previous namespace
       avalue   => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
     );
     -- svalue isn't defined until here
     identifiers( id ).value := identifiers( id ).svalue'access;
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
        put_line( gnat.source_info.source_location & ": internal error: open namespace tag not found" );
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

-- Copy the value and type of one identifier to another
procedure copyValue( to_decl : in out declaration; from_decl : declaration ) is
begin
   to_decl.svalue := from_decl.svalue;
   to_decl.avalue := from_decl.avalue;
   to_decl.list := from_decl.list;
   to_decl.kind := from_decl.kind;
   to_decl.class := from_decl.class;
   to_decl.usage := from_decl.usage;
   to_decl.genKind := from_decl.genKind;
   to_decl.genKind2 := from_decl.genKind2;
end copyValue;

procedure copyValue( to_decl : in out declaration; from_id : identifier ) is
begin
  copyvalue( to_decl, identifiers( from_id ) );
end copyValue;

procedure copyValue( to_id, from_id : identifier ) is
begin
  copyvalue( identifiers( to_id ), identifiers( from_id ) );
end copyValue;


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
   return to_numeric( identifiers( id ).value.all );
end to_numeric;

function to_unbounded_string( f : long_float ) return unbounded_string is
-- Convert a long_float (BUSH's numeric representation) to an
-- unbounded string.  If the value is representable as an integer,
-- it is returned without a decimal part.
  f_trunc : constant long_float := long_float'truncation( f );
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
      raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: cannot set high bit on character" & character'pos( ch )'img;
   end if;
   return character'val( 128+character'pos( ch ) );
end toHighASCII;

function toHighASCII( id : identifier ) return character is
   pragma suppress( RANGE_CHECK );
   -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
   -- exception.  We'll do the range checking manually as a work around...
begin
   if id > 127 then
      raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: cannot set high bit on identifier number" & id'img;
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
  raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: cannot encode natural number" & id'img;
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
  i1 : constant reservedWordRange := character'pos( ch1 );
  i2 : reservedWordRange;
begin
  if i1 < 128 then
    raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: byte code sequence is not an identifier: " & i1'img;
  elsif i1 < 224 then
     id := reservedWordRange( i1 - 127 );
     advance := 1;
  else
     i2 := character'pos( ch2 );
     id := reservedWordRange( i1 - 223 ) + 32 * i2;
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
      raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: cannot clear high bit on character" & character'pos( ch )'img;
   end if;
   return character'val( character'pos( ch )-128 );
end toLowASCII;

function toLowASCII( id : identifier ) return character is
   pragma suppress( RANGE_CHECK );
   -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
   -- exception.  We'll do the range checking manually as a work around...
begin
   if id <= 128 then
      raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: cannot clear high bit on identifier number" & id'img;
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


------------------------------------------------------------------------------
--
-- DESIGN CONSTRAINTS
--
------------------------------------------------------------------------------

function toConstraintMode( s : unbounded_string ) return designConstraintModes is
  constraintMode : designConstraintModes := undefined;
begin
  --return s = "unique" or s = "exclusive" or s = "local";
  if s = "unique" then
     constraintMode := unique;
  elsif s = "file" then
     constraintMode:= file;
  elsif s = "subprogram" then
     constraintMode:= subprogram;
  end if;
  return constraintMode;
end toConstraintMode;

--  DESIGN CONSTRAINT list sorting functions.
--
-- Both category and name matter.
------------------------------------------------------------------------------

-- We're not actually using >=

function ">="( left, right : aDesignConstraint ) return boolean is
begin
  return left.constraint >= right.constraint and left.name >= right.name;
end ">=";

function equal( left, right : aDesignConstraint ) return boolean is
begin
  return left.constraint = right.constraint and left.name = right.name;
end equal;


--  ENFORCED DESIGN CONSTRAINT sorting functions.
--
-- We can have only one constraint so we only check the category.
-- This might change as constraints become more powerful in the future.
------------------------------------------------------------------------------

-- We're not actually using >=

function ">="( left, right : anEnforcedDesignConstraint ) return boolean is
begin
  return left.enforcedFile >= right.enforcedFile and
         left.enforcedUnit = right.enforcedUnit and
         left.constraint >= right.constraint;
end ">=";

function equal( left, right : anEnforcedDesignConstraint ) return boolean is
begin
  return left.enforcedFile = right.enforcedFile and
         left.enforcedUnit = right.enforcedUnit and
         left.constraint = right.constraint;
end equal;

------------------------------------------------------------------------------
--
-- DESIGN AFFINITIES
--
------------------------------------------------------------------------------

function toAffinityMode( s : unbounded_string ) return designAffinityModes is
  affinityMode : designAffinityModes := undefined;
begin
  if s = "file" then
     affinityMode:= file;
  elsif s = "subprogram" then
     affinityMode:= subprogram;
  end if;
  return affinityMode;
end toAffinityMode;


--  DESIGN AFFINITY list sorting functions.
--
-- The affinity matters.
------------------------------------------------------------------------------

-- We're not actually using >=

function ">="( left, right : aDesignAffinity ) return boolean is
begin
  return left.affinity >= right.affinity;
end ">=";

function equal( left, right : aDesignAffinity ) return boolean is
begin
  return left.affinity = right.affinity;
end equal;


--  ENFORCED DESIGN AFFINITY list sorting functions.
--
-- We can have only one affinity so we only check the category.
-- This might change as affinities become more powerful in the future.
------------------------------------------------------------------------------

-- We're not actually using >=

function ">="( left, right : anEnforcedDesignAffinity ) return boolean is
begin
  return left.affinity >= right.affinity;
end ">=";

function equal( left, right : anEnforcedDesignAffinity ) return boolean is
begin
  return left.affinity = right.affinity;
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
         if integer'value( to_string( identifiers( candidateType ).value.all )) = fieldNumber then
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
  when genericTypeClass => return "generic type";
  when formalParamClass => return "formal parameter";
  when userCaseProcClass => return "user-defined case procedure";
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

  -- CGI programs cannot directly return HTTP 404.  e.g. this is bad:
  -- s := to_unbounded_string( "HTTP/1.1" & header.status'img & " " );
  --
  -- Instead, it must use a Status header field.

  s := to_unbounded_string( "Status:" & header.status'img & " " );
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
     --s := to_unbounded_string( "HTTP/1.1 500 Internal Server Error" );
     raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: unknown http status";
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
  when yamlTemplate => s := s & "text/yaml";
  when others => -- includes noTemplate
     --s := to_unbounded_string( "HTTP/1.1 500 Internal Server Error" );
     raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location &
       ": internal error: unknown template type " &
       header.templateType'img;
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
