------------------------------------------------------------------------------
-- Parser Side-Effects                                                      --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2019 Free Software Foundation              --
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

with ada.text_io,
     scanner,
     user_io;
use  ada.text_io,
     scanner,
     user_io;


package body parser_sidefx is

--- The Active Expression Identifier Linked List

activeExpressionIdList : activeExpressionIdLists.List;


--  ACTIVE EXPRESSION ID SORT
--
-- This is the sort function for the stack.  Since we are not actually
-- sorting, this is not used, but is still required to define the list.

function activeExpressionIdSort( left, right : activeExpressionId ) return boolean is
begin
  return right.id >= left.id;
end activeExpressionIdSort;


--- Expression Factor Stack
--
-- This is used by checkExpressionFactorVolatilityOnWrite
-------------------------------------------------------------------------------


--  PUSH EXPRESSION ID
--
-- Push an identifier which is used in an expression factor to the active
-- expression identifiers stack, associating it with the current expression.
-- (The current expression is found in the global lastExpressionInstruction.)
--
-- Skip this when running in maintenance mode because it is expensive and
-- the OnWrite test is more of a design issue.

procedure pushExpressionId( id : identifier ) is
  aei_record : activeExpressionId;
begin
  if not boolean(maintenanceOpt) then
     --put_line( "Push active id = " & id'img ); -- DEBUG
     aei_record.id := id;
     aei_record.exprId := lastExpressionInstruction;
     activeExpressionIdLists.Push( activeExpressionIdList, aei_record );
  end if;
end pushExpressionId;


--  PULL EXPRESSION IDS
--
-- Discard all identifiers associated with the current expression.  This is
-- done when an expression is finished.
-- (The current expression is found in the global lastExpressionInstruction.)
--
-- Skip this when running in maintenance mode because it is expensive and
-- the OnWrite test is more of a design issue.

procedure pullExpressionIds is
  aei_record : activeExpressionId;
begin
  if not boolean(maintenanceOpt) then
     while not activeExpressionidLists.isEmpty( activeExpressionidList ) loop
        activeExpressionIdLists.Pull( activeExpressionIdList, aei_record );
        if aei_record.exprId /= lastExpressionInstruction then
           activeExpressionIdLists.Push( activeExpressionIdList, aei_record );
           exit;
        end if;
        --put_line( "Pulled active id = " & aei_record.id'img ); -- DEBUG
     end loop;
  end if;
end pullExpressionIds;


--  IS ACTIVE EXPRESSION ID
--
-- True if id is in use in an active expression.  That is, that it is found in
-- the active expression identifiers stack.
--
-- Skip this when running in maintenance mode because it is expensive and
-- the OnWrite test is more of a design issue.

function isActiveExpressionId( id : identifier ) return boolean is
  aei_record : activeExpressionId;
  found : boolean := false;
begin
  if not boolean(maintenanceOpt) then
     for i in 1..activeExpressionIdLists.Length( activeExpressionIdList ) loop
         activeExpressionIdLists.Find( activeExpressionIdList, i, aei_record );
         if aei_record.id = id then
            found := true;
         end if;
     end loop;
  end if;
  return found;
end isActiveExpressionId;


--  CLEAR ACTIVE EXPRESSION IDS
--
-- Erase the active expression identifiers stack.

procedure clearActiveExpressionIds is
begin
  activeExpressionIdLists.Clear( activeExpressionIdList );
end clearActiveExpressionIds;


-- Side-effect tests
--
-- These procedures test for a side-effect issues and report an error if one is
-- detected.
-------------------------------------------------------------------------------


-- CHECK EXPRESSION FACTOR VOLATILITY
--
-- Check to see if a factor in an expression is read after it was written
-- to after the expression was started.
--
-- In x := f1() + y, if y was written to by f1(), report an error.

procedure checkExpressionFactorVolatility( id: identifier ) is
begin
  -- if variable exists...
  if id /= eof_t then
     if identifiers( id ).field_of /= eof_t then
        -- we don't track record fields, only the record
        if lastExpressionInstruction < identifiers( identifiers( id ).field_of ).writtenOn then
           err( "side-effects: " & to_string( identifiers( identifiers( id ).field_of ).name ) &
                " was read after written within " &
                "an expression.  Perhaps a copy should be used." );
        end if;
     else
        if lastExpressionInstruction < identifiers( id ).writtenOn then
           err( "side-effects: " & to_string( identifiers( id ).name ) &
                " was read after written to within " &
                "an expression.  Perhaps a copy should be used." );
        end if;
     end if;
  end if;
end checkExpressionFactorVolatility;


-- CHECK EXPRESSION FACTOR VOLATILITY ON WRITE
--
-- Check Expression Factor Volatility (above) requires the write occur before
-- the factor is read.  Run this on writing an identifier.
--
-- In this variation, check to see if the factor was written after it was read.
-- This is not a side-effect, but it indicates that if the expression was
-- re-ordered, it could become a side-effect.
--
-- In x := y + f1(), if f1 changes y, it's not a side-effect because y is not
-- affected.  However, we want to treat it like a side-effect because it could
-- become a side effect if the expression is changed, such as "y + 2 * f1()"
-- causing f1 to be executed before y's value is read.
--
-- In the case of assignments, which have their own subexpression, we don't care
-- if the identifier we're assigning to is read before we write it.  So we discount
-- the assignment's expression with the context parameter.  Context is the
-- instruction containing the assignment.
--
-- Skip this when running in maintenance mode because it is expensive and
-- the OnWrite test is more of a design issue.

procedure checkExpressionFactorVolatilityOnWrite( id: identifier ) is
begin
  -- Skip these tests in maintenance mode as they are expensive.  Ignore
  -- identifiers that are not defined (i.e. eof_t).
  if not boolean(maintenanceOpt) and id /= eof_t then
     if identifiers( id ).field_of /= eof_t then
        -- we don't track record fields, only the record
        if isActiveExpressionId( identifiers( id ).field_of ) then
        --if lastExpressionInstruction <= identifiers( identifiers( id ).field_of ).factorOn then
           err( "side-effects: " & to_string( identifiers( identifiers( id ).field_of ).name ) &
                " was written after read " &
                "within an expression.  Perhaps a copy should be used." );
        end if;
     else
--put_line("Written after read test" );
--put_line( "LEI=" & lastExpressionInstruction'img );
--put_line( "  " & to_string(identifiers(id).name) & ".factorOn=" & identifiers( id ).factorOn'img );
        if isActiveExpressionId( id ) then
        --if lastExpressionInstruction <= identifiers( id ).factorOn then
           err( "side-effects: " & to_string( identifiers( id ).name ) &
                " was written after read " &
                "within an expression.  Perhaps a copy should be used." );
        end if;
     end if;
  end if;
end checkExpressionFactorVolatilityOnWrite;


--  CHECK DOUBLE GLOBAL WRITE
--
-- This is the ownership test.  Check to see if a variable is written to
-- by two different expression-tasks (functions, contracts) during an
-- expression and report an error if at least two use it.

procedure checkDoubleThreadWrite( id : identifier ) is
begin
  -- Is this a record?
  if identifiers( id ).field_of /= eof_t then
     -- Was this written since the start of the expression?
     if lastExpressionInstruction < identifiers( identifiers( id ).field_of ).writtenOn then
        -- If never been written, we don't test yet.
        if identifiers( identifiers( id  ).field_of ).writtenByThread /= noThread then
           -- Is the name of the writer function, contract or task different?
           if identifiers( identifiers( id ).field_of ).writtenByThread /= getThreadName then
              -- Unless it is volatile, it is an error.
              if identifiers( identifiers( id ).field_of ).volatile /= unchecked then
                 err( "side-effects: " & to_string( identifiers( identifiers( id ).field_of ).name &
                      " (in " & optional_bold( to_string( getThreadName ) ) &
                      ") is not unchecked_volatile but is also changed by " &
                      optional_bold( to_string( identifiers( identifiers( id ).field_of ).writtenByThread ) ) ) &
                      ".  Perhaps refactor so one source is a procedure or break up the expression." );
              end if;
           end if;
        end if;
        identifiers( identifiers( id ).field_of ).writtenByThread := getThreadName;
     end if;
  else
     -- Else if this is not a record,
     -- Was this written since the start of the expression?
     if lastExpressionInstruction < identifiers( id ).writtenOn then
        -- If never been written, we don't test yet.
        if identifiers( id ).writtenByThread /= noThread then
           -- Is the name of the writer function, contract or task different?
           if identifiers( id ).writtenByThread /= getThreadName then
              -- Unless it is volatile, it is an error.
              if identifiers( identifiers( id ).field_of ).volatile /= unchecked then
                 err( "side-effects: " & to_string( identifiers( id ).name &
                      " (in " & optional_bold( to_string( getThreadName ) ) &
                      ") is not unchecked_volatile but is also changed by " &
                      optional_bold( to_string( identifiers( id ).writtenByThread ) ) ) &
                      ".  Perhaps refactor so one source is a procedure or break up the expression." );
              end if;
           end if;
        end if;
        identifiers( id ).writtenByThread := getThreadName;
     end if;
  end if;
end checkDoubleThreadWrite;


--  CHECK DOUBLE GLOBAL WRITE
--
-- Double write to any variable during an expression context.
-- Such as x := f1() + f1() where f1 writes global g.  This creates
-- a potential race condition as to the order g is being updated,
-- depending on how the expression is evaluated.
--
-- If we're not in the most-global block and the variable exists
-- in a more global position than the start of our block (i.e.
-- further down in the symbol table), then if it's not a volatile
-- and it has already been written since the beginning of the
-- expression context, show an error.
--
-- This is the strictest test.  It is not currently used.

--procedure checkDoubleGlobalWrite( id : identifier ) is
--begin
--  if identifiers( id ).field_of /= eof_t then
--     if blocks_top > block'first then
--        if not isLocal( id ) then
--           if not identifiers( identifiers( id ).field_of ).volatile then
--              if identifiers( identifiers( id ).field_of ).writtenOn > firstExpressionInstruction then
--                 err( "side-effects: " & to_string( identifiers( identifiers( id ).field_of ).name &
--                      " is not volatile and not local and is written to two or more times" &
--                      " during an expression" ) );
--              end if;
--           end if;
--        end if;
--     end if;
--  else
--    --put_line( "firstExpression = " & firstExpressionInstruction'img ); -- DEBUG
--    --put_line( "identifiers_top = " & identifiers_top'img ); -- DEBUG
--    --put_line( "id = " & var_id'img );
--    if blocks_top > block'first then
--       if not isLocal( id ) then
--    --put_line( "blocks_top = " & blocks_top'img );
--    --put_line( "id's block = " & block( getIdentifierBlock(var_id))'img );
--    --put_line( "is more global" );
--    --put_line( "written on = " & identifiers( var_id ).writtenOn'img );
--          if not identifiers( id ).volatile then
--             if identifiers( id ).writtenOn > firstExpressionInstruction then
--                err( "side-effects: " & to_string( identifiers( id ).name &
--                     " is not volatile and not local but is written to two or more times" &
--                     " during an expression" ) );
--             end if;
--          end if;
--       end if;
--    end if;
--  end if;
--
--end checkDoubleGlobalWrite;

end parser_sidefx;
