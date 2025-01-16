------------------------------------------------------------------------------
-- CHAIN UTIL                                                               --
--                                                                          --
-- Part of SparForte                                                        --
-- Designed and Programmed by Ken O. Burtch                                 --
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

with ada.strings.unbounded,
     pegasoft,
     world,
     scanner;
use  ada.strings.unbounded,
     pegasoft,
     world,
     scanner;

package body chain_util is

------------------------------------------------------------------------------
-- The chain util package contains functions related to SparForte chains.
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  IN CHAIN
--
-- Return the type of chain we are in (if any)
------------------------------------------------------------------------------

function in_chain return chain_types is
  contextName : unbounded_string;
  found : chain_types;
  i : block;
begin
  found := none;

  -- when checking blocks, skip blocks that embed in a procedure/function
  -- until we get to the procedure name

  i := blocks_top - 1;
  while i > 1 loop
    if getBlockName( i ) = "begin block" then
       null;
    elsif getBlockName( i ) = "declare block" then
       null;
    elsif getBlockName( i ) = "while loop" then
       null;
    elsif getBlockName( i ) = "loop loop" then
       null;
    elsif getBlockName( i ) = "for loop" then
       null;
    else
       exit;
    end if;
    i := i - 1;
  end loop;

  -- do we have a chain context?  Then we must be in a chain.
  contextName := getBlockName( i ) & " chain";
  -- if we already have a context block, don't create another
  -- TODO: what if in declare block?
  if i > 1 then
     if ( getBlockName( i-1 ) = contextName ) then
        found := subprogram;
     end if;
  end if;
  return found;
end in_chain;


-----------------------------------------------------------------------------
--  CHAIN CONTEXT
--
-- Return the current chain context.  Must be in a chain.
-----------------------------------------------------------------------------

function chain_context return chain_contexts is
  chain_count_id : identifier;
  last_in_chain_id : identifier;
  result : chain_contexts;
begin
  findIdent( chain_count_str, chain_count_id );
  findIdent( last_in_chain_str, last_in_chain_id );
  if to_numeric( identifiers( chain_count_id ).value.all ) = 1.0 then
     result := first;
  elsif to_numeric( identifiers( last_in_chain_id ).value.all ) = 1.0 then
     result := last;
  else
     result := middle;
  end if;
  return result;
end chain_context;

end chain_util;

