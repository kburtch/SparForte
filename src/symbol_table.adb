------------------------------------------------------------------------------
-- The Symbol Table.  The identifier type is in the world package.          --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2025 Free Software Foundation            --
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

with ada.text_io,
    gnat.source_info,
    CGI,
    pegasoft.strings,
    pegasoft.user_io;
use ada.text_io,
    ada.command_line,
    ada.command_line.environment,
    pegasoft.strings,
    pegasoft.user_io;

package body symbol_table is

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

-- Storage Primitives
--
-----------------------------------------------------------------------------

function value_equal( left, right : storage ) return boolean is
begin
  return left.value = right.value;
end value_equal;


-----------------------------------------------------------------------------
-- STORAGE CACHE
--
-- Allocating/deallocating memory is a slow operation.
--
-- To make slightly better use of allocated storage memory, instead of
-- freeing the last storage pointer, store it here so it can be reused.
--
-- This is the same technique I used in my single-linked list library.
--
-- Since only arrays currently use storage pointers, this only affects arrays.
-- I didn't see much of a speed improvement.
-----------------------------------------------------------------------------

storageCache     : storageGroupPtr := null;
storageCacheMiss : natural := 0;

-- CACHE OR FREE STORAGE
--
-- Cache or destroy storage pointer sp.
-----------------------------------------------------------------------------

procedure cacheOrFreeStorage( sp : storageGroupPtr ) is
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
-- forcibly discard it.
-----------------------------------------------------------------------------

function findStorage( lbound, ubound : long_integer ) return storageGroupPtr is
  sp : storagegroupPtr;
begin
  if storageCache = null then
     sp := new StorageGroup( lbound..ubound );
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
        sp := new StorageGroup( lbound..ubound );
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
       if kw.astorage /= null then
          free( kw.astorage );
       end if;
       kw.name := To_Unbounded_String( s );
       kw.kind := identifier'first;
       kw.value := kw.sstorage.value'access;
       kw.sstorage.value := Null_Unbounded_String;
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
       if func.astorage /= null then
          free( func.astorage );
       end if;
       func.name := To_Unbounded_String( s );
       func.kind := identifier'first;
       func.sstorage.value := Null_Unbounded_String;
       func.value := func.sstorage.value'access;
       func.class := funcClass;
       func.genKind := identifiers'first;
       func.genKind2 := identifiers'first;
       func.procCB := null;
       func.funcCB := cb;
       func.astorage := null;
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
       if proc.astorage /= null then
          free( proc.astorage );
       end if;
       proc.name := To_Unbounded_String( s );
       proc.kind := identifier'first;
       proc.sstorage.value := Null_Unbounded_String;
       proc.value := proc.sstorage.value'access;
       proc.class := procClass;
       proc.genKind := identifiers'first;
       proc.genKind2 := identifiers'first;
       proc.procCB := cb;
       proc.funcCB := null;
       proc.astorage := null;
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

procedure fixUsableParametersInAbstractSubprogram( sub_id : identifier ) is
-- Mark usable parameters as read/written in a null abstract subprogram
   paramCnt : natural := 0;
begin
   -- count the parameters
   -- this assumes the parameters immediately follow the subprogram
   -- id
   for i in sub_id+1..identifiers_top loop
       exit when identifiers( i ).field_of /= sub_id;
       paramCnt := paramCnt + 1;
   end loop;
   -- the usable parameters are never actually used in a null
   -- subprogram but they must be marked as used/written
   for i in identifier(natural(sub_id)+paramCnt)..identifier(natural(sub_id)+paramCnt*2) loop
       identifiers( i ).wasReferenced := true;
       if identifiers( i ).passingMode = out_mode or
          identifiers( i ).passingMode = in_out_mode then
          identifiers( i ).wasWritten := true;
       end if;
   end loop;
end fixUsableParametersInAbstractSubprogram;

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
     if identifiers( identifiers_top ).astorage /= null then
        free( identifiers( identifiers_top ).astorage );
     end if;
     identifiers( identifiers_top ) := declaration'(            -- define
       name     => To_Unbounded_String( s(s'first..eqpos-1) ),  -- identifier
       kind     => string_t,
       --sstorage    => To_Unbounded_String( s(eqpos+1..s'last ) ),
       --value    => sstorage'access,
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
       declaredAt => noDeclaredAt,
       declaredFile => null_unbounded_string,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByFlow => noDataFlow,
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
       sstorage => storage'(To_Unbounded_String( s(eqpos+1..s'last )), identifiers'first ),
       astorage => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
     );
     -- sstorage isn't defined until here
     identifiers( identifiers_top ).value := identifiers( identifiers_top ).sstorage.value'access;
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
     if identifiers( id ).astorage /= null then
        cacheOrFreeStorage( identifiers( id ).astorage );
        --free( identifiers( id ).astorage );
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
       declaredAt => noDeclaredAt,
       declaredFile => null_unbounded_string,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByFlow => noDataFlow,
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
       sstorage => nullStorage,
       astorage => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
     );
     -- sstorage isn't defined until here
     identifiers( id ).value := identifiers( id ).sstorage.value'access;
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
       if sc.astorage /= null then
          cacheOrFreeStorage( sc.astorage );
       end if;
       sc.name  := to_unbounded_string( name );                 -- define
       sc.kind  := kind;                                        -- identifier
       sc.sstorage := storage'( to_unbounded_string( value ), identifiers'first);
       sc.class := varClass;
       sc.genKind := identifiers'first;
       sc.genKind2 := identifiers'first;
       sc.static := true;                                       -- identifier
       sc.usage := constantUsage;
       sc.field_of := eof_t;
       sc.list := identifiers( kind ).list;
       sc.value := sc.sstorage.value'access;
       sc.writtenByFlow := noDataFlow;
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
       if sc.astorage /= null then
          cacheOrFreeStorage( sc.astorage );
       end if;
       sc.name  := to_unbounded_string( name );                 -- define
       sc.kind  := kind;                                        -- identifier
       sc.sstorage := storage'(to_unbounded_string( value ), identifiers'first);
       sc.class := enumClass;
       sc.genKind := identifiers'first;
       sc.genKind2 := identifiers'first;
       sc.static := true;                                       -- identifier
       sc.usage := fullUsage;
       sc.field_of := eof_t;
       sc.value := sc.sstorage.value'access;
       sc.writtenByFlow := noDataFlow;
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
-- Update a formal parameter (i.e. proc.param).  The id is not
-- returned since we don't change the formal parameters once they are set.
begin
    if parameterNumber /= 0 then -- function result? no name suffix
       --identifiers(id).name     := identifiers( id ).name;
    --else
       identifiers(id).name     := identifiers( proc_id ).name & "." & identifiers( id ).name;
    end if;
    identifiers(id).sstorage   := storage'(to_unbounded_string( parameterNumber'img ), identifiers'first );
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
-- Declare a usable formal parameter (i.e. param for proc.param) in the symbol
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
     if identifiers( id ).astorage /= null then
        cacheOrFreeStorage( identifiers( id ).astorage );
        --free( identifiers( id ).astorage );
     end if;
     identifiers( id ) := declaration'(               -- define
                 name     => paramName,                         -- identifier
                 kind     => identifiers( i ).kind,
                 --value    => sstorage'access,
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
                 declaredAt => noDeclaredAt,
                 declaredFile => null_unbounded_string,
                 specAt   => noSpec,
                 specFile => null_unbounded_string,
                 wasReferenced => false,
                 --referencedByThread => noThread,
                 wasWritten => false,
                 writtenByFlow => noDataFlow,
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
                 sstorage => storage'(value, identifiers'first),
                 astorage => null,
                 renaming_of => identifier'first,
                 renamed_count => 0,
                 passingMode => identifiers( i ).passingMode
     );
     -- sstorage isn't defined until here
     identifiers( id ).value := identifiers( id ).sstorage.value'access;
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
     if identifiers( id ).astorage /= null then
        cacheOrFreeStorage( identifiers( id ).astorage );
        --free( identifiers( id ).astorage );
     end if;
     identifiers( id ) := declaration'(               -- define
       name     => paramName,                         -- identifier
       kind     => identifiers( func_id ).kind,
       value    => null,
       --value    => sstorage'access,
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
       declaredAt => noDeclaredAt,
       declaredFile => null_unbounded_string,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByFlow => noDataFlow,
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
       sstorage => nullStorage,
       astorage => null,
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
-- using dot notation, it's a kludge.  SparForte
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
     if identifiers( id ).astorage /= null then
        cacheOrFreeStorage( identifiers( id ).astorage );
        --free( identifiers( id ).astorage );
     end if;
     identifiers( id ) := declaration'(                      -- define
       name     => name,                                     -- identifier
       kind     => exception_t,
       --value    => sstorage'access,
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
       declaredAt => noDeclaredAt,
       declaredFile => null_unbounded_string,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByFlow => noDataFlow,
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
       sstorage    => storage'(character'val( exception_status_code ) & default_message, identifiers'first),
       astorage   => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
     );
     -- sstorage isn't defined until here
     identifiers( id ).value := identifiers( id ).sstorage.value'access;
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

  --if identifiers( new_id ).astorage /= null then
  --   free( identifiers( new_id ).astorage );
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
       declaredAt => noDeclaredAt,
       declaredFile => null_unbounded_string,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByFlow => noDataFlow,
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
       -- not sure we need to copy sstorage as it is not normally
       -- read directly.
       sstorage => identifiers( canonicalRef.id ).sstorage,
       -- we need to refer to the canonical variables' astorage to
       -- do array bounds tests in the parser...
       astorage => identifiers( canonicalRef.id ).astorage,
       renaming_of => canonicalRef.id,
       renamed_count => 0,
       passingMode => none
     );

     -- The canonical reference returned by ParseRenamingReference
     -- isn't the root canonical variable.  So we must follow the chain
     -- and assign value to the sstorage of the root canonical.
     --identifiers( new_id ).value := identifiers( canonicalRef.id ).sstorage'access;

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
        identifiers( new_id ).value := identifiers( deref_id ).sstorage.value'access;
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
      put_line_retry( standard_error, gnat.source_info.source_location &
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
           identifiers( field_id ).writtenByFlow := noDataFlow;
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
     if identifiers( id ).astorage /= null then
        cacheOrFreeStorage( identifiers( id ).astorage );
        --free( identifiers( id ).astorage );
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
       declaredAt => noDeclaredAt,
       declaredFile => null_unbounded_string,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByFlow => noDataFlow,
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
       sstorage    => storage'(currentNamespace, identifiers'first),
       astorage   => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
     );
     -- sstorage isn't defined until here
     identifiers( id ).value := identifiers( id ).sstorage.value'access;

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
     if identifiers( id ).astorage /= null then
        cacheOrFreeStorage( identifiers( id ).astorage );
        --free( identifiers( id ).astorage );
     end if;
     identifiers( id ) := declaration'(                      -- define
       name     => to_unbounded_string( name ),              -- identifier
       kind     => identifiers'first,       -- TODO: this is a placeholder
       --value    => sstorage'access,
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
       declaredAt => noDeclaredAt,
       declaredFile => null_unbounded_string,
       specAt   => noSpec,
       specFile => null_unbounded_string,
       wasReferenced => false,
       --referencedByThread => noThread,
       wasWritten => false,
       writtenByFlow => noDataFlow,
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
       sstorage    => storage'(currentNamespace, identifiers'first),             -- the previous namespace
       astorage   => null,
       renaming_of => identifier'first,
       renamed_count => 0,
       passingMode => none
     );
     -- sstorage isn't defined until here
     identifiers( id ).value := identifiers( id ).sstorage.value'access;
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
        put_line_retry( gnat.source_info.source_location & ": internal error: open namespace tag not found" );
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
   to_decl.sstorage := from_decl.sstorage;
   to_decl.astorage := from_decl.astorage;
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


-----------------------------------------------------------------------------
-- IS KEYWORD
--
-- TRUE if the identifier is a keyword.
-----------------------------------------------------------------------------

function is_keyword( id : identifier ) return boolean is
-- True if an AdaScript keyword.  Keywords are defined in the
-- first part of the identifier table.
begin
  --return id < reserved_top;
  return id < keywords_top;
end is_keyword;


procedure findField( recordVar : identifier; fieldNumber: natural;
  fieldVar : out identifier ) is
-- Find fieldNumber'th field of a record variable.  This involves looking up the
-- record type, searching for the type's fields, checking the type field's
--  numbers, constructing the name of the record's field, and finding it in
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


-- Type Conversions


function to_numeric( id : identifier ) return numericValue is
-- Look up an identifier's value and return it as a long float
-- (Spar's numeric representation).
begin
   return to_numeric( identifiers( id ).value.all );
end to_numeric;

end symbol_table;
