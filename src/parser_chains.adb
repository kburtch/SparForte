------------------------------------------------------------------------------
-- Chains Package Parser                                                    --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

--with text_io;use text_io;

with
    ada.strings.unbounded,
    world,
    scanner,
    chain_util;
use
    ada.strings.unbounded,
    world,
    scanner,
    chain_util;

package body parser_chains is

------------------------------------------------------------------------------
-- Exceptions package identifiers
------------------------------------------------------------------------------

chain_context_t         : identifier;
chains_context_first_t  : identifier;
chains_context_middle_t : identifier;
chains_context_last_t   : identifier;
chains_context_not_in_chain_t : identifier;

chains_in_chain_t       : identifier;
chains_chain_context_t  : identifier;
chains_chain_count_t    : identifier;

procedure ParseChainsInChain( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: s := chains.in_chain
  -- Ada:    N/A
begin
  kind := boolean_t;
  expect( chains_in_chain_t );
  if isExecutingCommand then
     result := to_bush_boolean( in_chain /= none );
  end if;
end ParseChainsInChain;

procedure ParseChainsChainContext( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: s := chains.chain_context
  -- Ada:    N/A
begin
  kind := chain_context_t;
  expect( chains_chain_context_t );
  if isExecutingCommand then
     if in_chain = none then
        result := to_unbounded_string( "3" );
     else
        case chain_context is
        when first =>
           result := to_unbounded_string( "0" );
        when last =>
           result := to_unbounded_string( "2" );
        when others =>
           result := to_unbounded_string( "1" );
        end case;
     end if;
  end if;
end ParseChainsChainContext;

procedure ParseChainsChainCount( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: s := chains.chain_count
  -- Ada:    N/A
  chain_count_id : identifier;
begin
  kind := positive_t;
  expect( chains_chain_count_t );
  if isExecutingCommand then
     if in_chain = none then
        err( "not in a chain" );
     else
        findIdent( chain_count_str, chain_count_id );
        result := identifiers( chain_count_id ).value.all;
     end if;
  end if;
end ParseChainsChainCount;

procedure StartupChains is
begin
  declareNamespace( "chains" );
  declareIdent( chain_context_t, "chains.context", root_enumerated_t, typeClass );
  declareStandardConstant( chains_context_first_t, "chains.context_first", chain_context_t, "0" );
  declareStandardConstant( chains_context_middle_t, "chains.context_middle", chain_context_t, "1" );
  declareStandardConstant( chains_context_last_t, "chains.context_last", chain_context_t, "2" );
  declareStandardConstant( chains_context_not_in_chain_t, "chains.not_in_chain", chain_context_t, "3" );

  declareFunction( chains_in_chain_t, "chains.in_chain", ParseChainsInChain'access );
  declareFunction( chains_chain_context_t, "chains.chain_context", ParseChainsChainContext'access );
  declareFunction( chains_chain_count_t, "chains.chain_count",ParseChainsChainCount'access );
  declareNamespaceClosed( "chains" );
end StartupChains;

procedure ShutdownChains is
begin
  null;
end ShutdownChains;

end parser_chains;
