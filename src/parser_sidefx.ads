------------------------------------------------------------------------------
-- Parser Side Effects                                                      --
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

with ada.strings.unbounded,
     gen_list,
     world;
use  ada.strings.unbounded,
     world;

package parser_sidefx is

--- Expression Side-effects
--
-- Side-effects refer to any changes in more-global variables that can cause
-- unpredictability when evaluation an expression.  These include.
--
-- 1. Writing to a variable then using it in any active expression.
-- 2. Reading a variable in an active expression then trying to write to it
--    (not actually a side-effect, but could become one if the expression is
--    changed so it executes in a different order).
-- 3. Two "threads" (meaning functions and/or contracts) trying to write to
--    the same global, creating a data race condition.  While ideally no two
--    "threads" should write to a global, this eliminates too much useful
--    functionality, so we instead assume that one "thread" is smart enough
--    to manage multiple writes by itself.
-------------------------------------------------------------------------------


--- Expression Factor Stack
--
-- This is used by checkExpressionFactorVolatilityOnWrite
--
-- Oddly enough, testing for writing then reading is much easier than reading
-- then writing.  With reading then writing, expressions come and go so we
-- need their identifiers in a stack.
-------------------------------------------------------------------------------

type activeExpressionId is record
     id     : identifier;   -- which identifier?
     exprId : line_count;   -- seen in what expression?
end record;

procedure pushExpressionId( id : identifier );
-- Push an identifier which is used in an expression factor to the active
-- expression identifiers stack, associating it with the current expression.
-- (The current expression is found in the global lastExpressionInstruction.)

procedure pullExpressionIds;
-- Discard all identifiers associated with the current expression.  This is
-- done when an expression is finished.
-- (The current expression is found in the global lastExpressionInstruction.)

function isActiveExpressionId( id : identifier ) return boolean;
-- True if id is in use in an active expression.  That is, that it is found in
-- the active expression identifiers stack.

procedure clearActiveExpressionIds;
-- Erase the active expression identifiers stack.


-- Side-effect tests
--
-- These procedures test for a side-effect issues and report an error if one is
-- detected.
-------------------------------------------------------------------------------


procedure checkExpressionFactorVolatility( id: identifier );
-- Check to see if a factor in an expression is read after it was written
-- to after the expression was started.
-- Example: In x := f1() + y, if y was written to by f1(), report an error.
-- This is Case #1.

procedure checkExpressionFactorVolatilityOnWrite( id: identifier );
-- Check to see if an identifier is written after it was read
-- by an expression, while the expression is incomplete.
-- Example: In x := y + f1(), if f1 changes y.
-- This is Case #2.

procedure checkDoubleThreadWrite( id : identifier );
-- Check to see if, at run-time, two different "threads" write to the same
-- unprotected variable.  Also updates writtenByThread.
-- This is Case #3.

--procedure checkDoubleGlobalWrite( id : identifier );
-- A strict check for anything being written twice after an expression
-- is started.

PRIVATE

-- The stack

function activeExpressionIdSort( left, right : activeExpressionId ) return boolean;

package activeExpressionIdLists is new gen_list( activeExpressionId, activeExpressionIdSort, "=" );

end parser_sidefx;
