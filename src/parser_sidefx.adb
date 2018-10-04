------------------------------------------------------------------------------
-- Parser Side-Effects                                                      --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2018 Free Software Foundation              --
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
     --Interfaces.C,
     --gnat.source_info,
     --string_util,
     --script_io,
     user_io;
     --compiler,
     --parser.decl.as;
use  ada.text_io,
     scanner,
     --nterfaces.C,
     --string_util,
     --script_io,
     user_io;
     --compiler,
     --parser,
     --parser.decl.as;


package body parser_sidefx is


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
           if not identifiers( identifiers( id ).field_of ).volatile then
              err( to_string( identifiers( identifiers( id ).field_of ).name ) & " is not " &
                   optional_bold( "volatile" ) & " but was written to after " &
                   "evaluating the start of the expression" );
           end if;
        end if;
     else
        if lastExpressionInstruction < identifiers( id ).writtenOn then
           if not identifiers( id ).volatile then
              err( to_string( identifiers( id ).name ) & " is not " &
                   optional_bold( "volatile" ) & " but was written to after " &
                   "evaluating the start of the expression" );
           end if;
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

procedure checkExpressionFactorVolatilityOnWrite( id: identifier; context : line_count ) is
begin
  -- if variable exists...
  if id /= eof_t then
     if identifiers( id ).field_of /= eof_t then
        -- we don't track record fields, only the record
        if lastExpressionInstruction < identifiers( identifiers( id ).field_of ).factorOn and
           identifiers( identifiers( id ).field_of ).factorOn < context then
           if not identifiers( identifiers( id ).field_of ).volatile then
              err( to_string( identifiers( identifiers( id ).field_of ).name ) & " is not " &
                   optional_bold( "volatile" ) & " but was written after read " &
                   "within an expression" );
           end if;
        end if;
     else
--put_line( "LEI=" & lastExpressionInstruction'img ); -- DEBUG
--put_line( "factorOn=" & identifiers( id ).factorOn'img ); -- DEBUG
        if lastExpressionInstruction < identifiers( id ).factorOn and
           identifiers( id ).factorOn < context then
           if not identifiers( id ).volatile then
              err( to_string( identifiers( id ).name ) & " is not " &
                   optional_bold( "volatile" ) & " but was written after read " &
                   "within an expression" );
           end if;
        end if;
     end if;
  end if;
end checkExpressionFactorVolatilityOnWrite;


--  CHECK DOUBLE GLOBAL WRITE
--
-- This is the ownership test.  Check to see if a variable is written to
-- by two different expression-tasks (functions, contracts) and report
-- an error if two use it.

procedure checkDoubleThreadWrite( id : identifier ) is
begin
  if identifiers( id ).field_of /= eof_t then
     -- we don't track record fields, only the record
     if identifiers( identifiers( id  ).field_of ).writtenByThread /= noThread then
        if identifiers( identifiers( id ).field_of ).writtenByThread /= getThreadName then
           if not identifiers( identifiers( id ).field_of ).volatile then
              err( to_string( identifiers( identifiers( id ).field_of ).name &
                   " (in " & optional_bold( to_string( getThreadName ) ) &
                   ") is not volatile but is also changed by " &
                   optional_bold( to_string( identifiers( identifiers( id ).field_of ).writtenByThread ) ) ) );
           end if;
        end if;
     end if;
     identifiers( identifiers( id ).field_of ).writtenByThread := getThreadName;
  else
     if identifiers( id ).writtenByThread /= noThread then
        if identifiers( id ).writtenByThread /= getThreadName then
           if not identifiers( identifiers( id ).field_of ).volatile then
              err( to_string( identifiers( id ).name &
                   " (in " & optional_bold( to_string( getThreadName ) ) &
                   ") is not volatile but is also changed by " &
                   optional_bold( to_string( identifiers( id ).writtenByThread ) ) ) );
           end if;
        end if;
     end if;
     identifiers( id ).writtenByThread := getThreadName;
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
-- This is the strictest test.

procedure checkDoubleGlobalWrite( id : identifier ) is
begin
  if identifiers( id ).field_of /= eof_t then
     if blocks_top > block'first then
        if not isLocal( id ) then
           if not identifiers( identifiers( id ).field_of ).volatile then
              if identifiers( identifiers( id ).field_of ).writtenOn > firstExpressionInstruction then
                 err( to_string( identifiers( identifiers( id ).field_of ).name &
                      " is not volatile and not local and is written to two or more times" &
                      " during an expression" ) );
              end if;
           end if;
        end if;
     end if;
  else
    --put_line( "firstExpression = " & firstExpressionInstruction'img ); -- DEBUG
    --put_line( "identifiers_top = " & identifiers_top'img ); -- DEBUG
    --put_line( "id = " & var_id'img );
    if blocks_top > block'first then
       if not isLocal( id ) then
    --put_line( "blocks_top = " & blocks_top'img );
    --put_line( "id's block = " & block( getIdentifierBlock(var_id))'img );
    --put_line( "is more global" );
    --put_line( "written on = " & identifiers( var_id ).writtenOn'img );
          if not identifiers( id ).volatile then
             if identifiers( id ).writtenOn > firstExpressionInstruction then
                err( to_string( identifiers( id ).name &
                     " is not volatile and not local but is written to two or more times" &
                     " during an expression" ) );
             end if;
          end if;
       end if;
    end if;
  end if;

end checkDoubleGlobalWrite;

end parser_sidefx;
