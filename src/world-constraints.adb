------------------------------------------------------------------------------
-- Design Constraints and Affinities                                        --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------


package body world.constraints is

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

end world.constraints;

