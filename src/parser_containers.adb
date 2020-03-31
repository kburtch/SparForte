------------------------------------------------------------------------------
-- Containers Package Parser                                                --
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

package body parser_containers is

-- There are no AdaScript procedures or functions in this package

-----------------------------------------------------------------------------
-- Housekeeping
-----------------------------------------------------------------------------

procedure StartupContainers is
begin
  declareNamespace( "containers" );
  -- These two are actually mod types but we don't support that yet
  declareIdent( containers_count_type_t, "containers.count_type", natural_t, typeClass );
  declareIdent( containers_hash_type_t, "containers.hash_type", natural_t, typeClass );
  declareNamespaceClosed( "containers" );
end StartupContainers;

procedure ShutdownContainers is
begin
  null;
end ShutdownContainers;

end parser_containers;

