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

--with ada.strings.unbounded;
--use  ada.strings.unbounded;
with pegasoft.gen_list;

package world.constraints is

------------------------------------------------------------------------------
-- DESIGN CONSTRAINTS
--
-- A constraint has a mode, category, name and a limit.
------------------------------------------------------------------------------

type designConstraintModes is (
   undefined, unique, file, subprogram
);

function toConstraintMode( s : unbounded_string ) return designConstraintModes;

type aDesignConstraint is record
    mode       : designConstraintModes;
    constraint :  unbounded_string;
    name       : unbounded_string;
    limit      : float;
end record;

function ">="( left, right : aDesignConstraint ) return boolean;

function equal( left, right : aDesignConstraint ) return boolean;

package DesignConstraintLists is new pegasoft.gen_list( aDesignConstraint, equal,  ">=" );

-- value indicating asserted constraint exists

-- value indicating asserted constraint exists

noDesignConstraint : constant natural := 0;

type anEnforcedDesignConstraint is record
    mode         : designConstraintModes;         -- how strict
    enforcedAt   : natural := noDesignConstraint; -- line enforced
    enforcedFile : unbounded_string;              -- file enforced
    enforcedUnit : unbounded_string;              -- subr enforced (local only)
    constraint   : unbounded_string;
    name         : unbounded_string;
    weight       : float;
end record;

function ">="( left, right : anEnforcedDesignConstraint ) return boolean;

function equal( left, right : anEnforcedDesignConstraint ) return boolean;

package EnforcedDesignConstraintLists is new pegasoft.gen_list( anEnforcedDesignConstraint, equal,  ">=" );

package EnforcedLocalDesignConstraintLists is new pegasoft.gen_list( anEnforcedDesignConstraint, equal,  ">=" );

------------------------------------------------------------------------------
-- DESIGN AFFINITIES
--
-- An affinity has a mode, name and goal.
------------------------------------------------------------------------------

type designAffinityModes is (
   undefined, file, subprogram
);

function toAffinityMode( s : unbounded_string ) return designAffinityModes;

type aDesignAffinity is record
    mode       : designAffinityModes;
    affinity   : unbounded_string;
    limit      : float;
end record;

function ">="( left, right : aDesignAffinity ) return boolean;

function equal( left, right : aDesignAffinity ) return boolean;

package DesignAffinityLists is new pegasoft.gen_list( aDesignAffinity, equal,  ">=" );

-- value indicating asserted constraint exists

noDesignAffinity : constant natural := 0;

type anEnforcedDesignAffinity is record
    mode         : designAffinityModes;           -- how strict
    enforcedAt   : natural := noDesignAffinity;   -- line enforced
    enforcedFile : unbounded_string;              -- file enforced
    enforcedUnit : unbounded_string;              -- subr enforced (local only)
    affinity     : unbounded_string;
    weight       : float;
end record;

function ">="( left, right : anEnforcedDesignAffinity ) return boolean;

function equal( left, right : anEnforcedDesignAffinity ) return boolean;

package EnforcedDesignAffinityLists is new pegasoft.gen_list( anEnforcedDesignAffinity, equal,  ">=" );

package EnforcedLocalDesignAffinityLists is new pegasoft.gen_list( anEnforcedDesignAffinity, equal,  ">=" );

end world.constraints;
