------------------------------------------------------------------------------
-- AdaScript Language Parser                                                --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
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
pragma ada_2005;

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );
with Interfaces.C,
    ada.containers.vectors,
    ada.strings.unbounded.text_io,
    gnat.source_info,
    spar_os.exec,
    pegasoft.strings,
    pegasoft.user_io,
    performance_monitoring,
    builtins,
    jobs,
    signal_flags,
    compiler,
    scanner.communications,
    scanner_res,
    parser.decl.shell, -- sybling package
    parser_aux,
    parser_sidefx,
    parser_params,
    parser_pragmas,
    parser_tio,
    parser_pen,
    interpreter; -- circular relationship for breakout prompt
use Interfaces.C,
    ada.containers,
    ada.strings.unbounded.text_io,
    spar_os,
    spar_os.exec,
    pegasoft.strings,
    pegasoft.user_io,
    performance_monitoring,
    builtins,
    jobs,
    signal_flags,
    compiler,
    scanner.communications,
    scanner_res,
    parser.decl.shell, -- sybling package
    parser_aux,
    parser_sidefx,
    parser_params,
    parser_pragmas,
    parser_tio,
    parser_pen,
    interpreter; -- circular relationship for breakout prompt

with ada.text_io;
use  ada.text_io;

package body parser.decl.as is


-----------------------------------------------------------------------------
--
-- Forwards
--
-----------------------------------------------------------------------------

package vector_identifier_lists is new Ada.Containers.Vectors(
   positive,
   identifier,
   "="
);
procedure SkipBlock( termid1, termid2 : identifier := keyword_t );


-----------------------------------------------------------------------------
--
-- Utilities
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- CHECK VAR USAGE QUALIFIER
--
-- Handle usage qualifiers (record fields)
--
-- At a debug breakout prompt, constant and limited assignment is permitted
-- with a warning to the user.
--
-- When testing, constant and limited assignment is allowed.
--
-- Record fields are a little complicated.  As record are collections of
-- variables, check both the parent record of the file and that record's
-- type to see if it is constant or limited.  If so, treat the field as
-- constant or limited.
-----------------------------------------------------------------------------

procedure checkVarUsageQualifier( var_id : identifier ) is
begin
  -- field of a record?

  if identifiers( var_id ).field_of /= eof_t then

     -- check the usage qualifier of the parent

     case identifiers( identifiers( var_id ).field_of ).usage is
     when abstractUsage =>
        err( gnat.source_info.source_location &
             ": internal error: variables should not have abstract types" );
     when limitedUsage =>
        if isTesting then
           null;
        elsif inputMode = breakout then
           put_trace( "Warning: assigning a new value to a limited record field" );
        else
           err( "limited record fields cannot be assigned a value" );
        end if;
     when constantUsage =>
        if isTesting then
           null;
        elsif inputMode = breakout then
           put_trace( "Warning: assigning a new value to a constant record field" );
        else
           err( "constant record fields cannot be assigned a value" );
        end if;
     when fullUsage =>

     -- check the usage qualifier of the parent's type

        case identifiers(
              identifiers (
                  identifiers( var_id ).field_of
              ).kind
           ).usage is
        when abstractUsage =>
           null; -- don't bother checking
        when limitedUsage =>
           if isTesting then
              null;
           elsif inputMode = breakout then
              put_trace( "Warning: assigning a new value to a limited record field" );
           else
              err( "limited record fields cannot be assigned a value" );
           end if;
        when constantUsage =>
           if isTesting then
              null;
           elsif inputMode = breakout then
              put_trace( "Warning: assigning a new value to a constant record field" );
           else
              err( "constant record fields cannot be assigned a value" );
           end if;
        when fullUsage =>
           null;
        when others =>
           err( gnat.source_info.source_location &
                ": internal error: unexpected usage qualifier" );
        end case;

     when others =>
        err( gnat.source_info.source_location &
             ": internal error: unexpected usage qualifier " &
             identifiers( identifiers( var_id ).field_of ).usage'img );
     end case;

  end if;

  -- Handle usage qualifiers
  --
  -- Check everything here. Including record fields that passed the parent
  -- record tests.

  case identifiers( var_id ).usage is

  when abstractUsage =>
     err( gnat.source_info.source_location &
          ": internal error: variables should not be abstract" );

  when limitedUsage =>
     if isTesting then
        null;
     elsif inputMode = breakout then
        put_trace( "Warning: assigning a new value to a limited variable" );
     else
        err( "limited variables cannot be assigned a value" );
     end if;

  -- constants can only be assigned values if they are specs

  when constantUsage =>
     if isTesting then
        null;
     elsif inputMode = breakout then
        put_trace( "Warning: assigning a new value to a constant variable" );
     elsif identifiers( var_id ).specAt = noSpec then
        err( "constant variables cannot be assigned a value" );
     end if;

  when fullUsage =>
     null;

  when others =>
     err( gnat.source_info.source_location &
          ":  internal error: unexpected usage qualifier " &
          identifiers( var_id ).usage'img );
  end case;
end checkVarUsageQualifier;


-----------------------------------------------------------------------------
--  PARSE IF BLOCK
--
-- Handle an if statement.
-- Syntax: if-block = "if"... "elsif"..."else"..."end if"
-- The handling of an if block is very tricky because the blocks
-- do not only include what is between the parts of the if, but
-- the expressions themselves.  Plus there's the problem of exiting
-- gracefully if an exit statement is encountered.  All of this
-- makes handling this statement more complicated than you might
-- think.
-----------------------------------------------------------------------------

procedure ParseIfBlock is
  expr_val  : unbounded_string;
  expr_type : identifier;
  b : boolean := false;
  handled : boolean := false;
  backup_sc : boolean;
begin
--put_line("ParseIfBlock"); -- DEBUG

  -- if expr then statements

  expect( if_t );                                          -- "if"
  if token = if_t then                                     -- this error is
     err( "redundant " & optional_yellow( "if" ) );          -- from GNAT
  end if;
  ParseExpression( expr_val, expr_type );                  -- expression
  if type_checks_done then
     b := expr_val = "1";                                  -- to real boolean
  elsif not baseTypesOK( boolean_t, expr_type ) then       -- not a bool result?
     err( "boolean expression expected" );
  else                                                     -- else convert bool
     b := expr_val = "1";                                  -- to real boolean
  end if;
  expect( then_t );                                        -- "then"
  if token = then_t then                                   -- this error is
     err( "redundant " & optional_yellow( "then" ) );        -- from GNAT
  end if;
  if b then                                                -- was true?
     ParseBlock( elsif_t, else_t );                        -- handle if block
     handled := true;                                      -- remember we did it
                                                           -- even elsifs and else line
  else                                                     -- otherwise
     SkipBlock( elsif_t, else_t );                         -- skip if block
  end if;

  -- elsif expr then statements

  -- temporarily switch to syntax check mode when required to skip expression

  while token = elsif_t loop                               -- a(nother) elsif?
     if handled then                                       -- already handled?
        backup_sc := syntax_check;                         -- don't exec elsif
        syntax_check := true;                              -- expression
     end if;
     expect( elsif_t );                                    -- "elsif"
     if token = elsif_t then                               -- this error is
        err( "redundant " & optional_yellow( "elsif" ) );    -- from GNAT
     end if;
     ParseExpression( expr_val, expr_type );               -- expression
     if type_checks_done then
        b := expr_val = "1";                               -- to real boolean
     elsif not baseTypesOK( boolean_t, expr_type ) then       -- not bool result?
        err( "boolean expression expected" );
     else                                                  -- else convert bool
        b := expr_val = "1";                               -- to real boolean
     end if;
     if handled then                                       -- already handled?
        syntax_check := backup_sc;                         -- restore flag
     end if;                                               -- for SkipBlock
     expect( then_t );                                     -- "then"
     if token = then_t then                                -- this is from
        err( "redundant " & optional_yellow( "then" ) );     -- GNAT
     end if;
     if b and not handled then                             -- true (and not previously done)
        ParseBlock( elsif_t, else_t );                     -- handle the elsif block
        handled := true;                                   -- remember we did it
     else                                                  -- otherwise
        SkipBlock( elsif_t, else_t );                      -- skip elsif block
     end if;
  end loop;

  -- by this point syntax check mode should be restored (if it was altered)

  -- else statements

  if token = else_t then                                   -- else part?
     if handled then                                       -- already handled?
        backup_sc := syntax_check;                         -- don't exec else
        syntax_check := true;                              -- for --trace
     end if;
     expect( else_t );                                     -- "else"
     if handled then                                       -- already handled?
        syntax_check := backup_sc;                         -- restore flag
     end if;                                               -- for SkipBlock
     if token = else_t then                                -- this is from
        err( "redundant " & optional_yellow( "else" ) );     -- GNAT
     end if;
     if not handled then                                   -- nothing handled yet?
        ParseBlock;                                        -- handle else block
     else                                                  -- otherwise
        SkipBlock;                                         -- skip else block
     end if;
  end if;

  -- end if

  expect( end_t );                                         -- "end if"
  expect( if_t );

--put_line("ParseIfBlock end"); -- DEBUG
end ParseIfBlock;


-----------------------------------------------------------------------------
--  STATIC BLOCK
--
-- Conditionally run code based on a static expression.  Part of static if.
-- Only pragmas, static if's or case's allowed.
-----------------------------------------------------------------------------

procedure ParseStaticBlock( termid1, termid2 : identifier := keyword_t ) is
  -- Syntax: block = "general-stmt [general-stmt...] termid1 | termid2"
begin
  if token = end_t or token = eof_t or token = termid1 or token = termid2 then
     err( "missing statement or command" );
  end if;
  while token /= end_t and token /= eof_t and token /= termid1 and token /= termid2 loop
     if token = pragma_t then
        ParsePragma;
        expectStatementSemicolon( context => pragma_t );
     elsif token = if_t then
        ParseStaticIfBlock;
        expectStatementSemicolon( context => if_t );
     elsif token = case_t then
        ParseStaticCaseBlock;
        expectStatementSemicolon( context => case_t );
     elsif token = null_t then
        expect( null_t );
        expectStatementSemicolon( context => null_t );
     end if;
  end loop;
end ParseStaticBlock;


-----------------------------------------------------------------------------
--  SKIP STATIC BLOCK
--
-- Conditionally skip code based on a static expression.  Part of static if.
-----------------------------------------------------------------------------

procedure SkipStaticBlock( termid1, termid2 : identifier := keyword_t ) is
  old_error : boolean;
  old_skipping : boolean;
begin
  if token = end_t or token = eof_t or token = termid1 or token = termid2 then
     err( "missing statement or command" );
  end if;
  if syntax_check then               -- if we're checking syntax
     ParseStaticBlock( termid1, termid2 ); -- must process the block to look
     return;                         -- for syntax errors
  end if;
  old_error := syntax_check;
  old_skipping := skipping_block;
  syntax_check := true;
  skipping_block := true;
  -- if an error happens in the block, we were skipping it anyway...
  while token /= end_t and token /= eof_t and token /= termid1 and token /= termid2 loop
      ParseBlockExecutablePartStatement; -- step through context
  end loop;
  syntax_check := old_error;
  skipping_block := old_skipping;
end SkipStaticBlock;


-----------------------------------------------------------------------------
-- STATIC IF BLOCK
--
-- Conditionally execute or skip blocks of code based on a static expression.
-- This is tied to parsePolicy: blocks may only contain policy block
-- statements.
-----------------------------------------------------------------------------

procedure ParseStaticIfBlock is
-- Syntax: if-block = "if"... "elsif"..."else"..."end if"
  expr_val  : unbounded_string;
  expr_type : identifier;
  b : boolean := false;
  handled : boolean := false;
  backup_sc : boolean;
begin

  -- The handling of an if block is very tricky because the blocks
  -- do not only include what is between the parts of the if, but
  -- the expressions themselves.  Plus there's the problem of exiting
  -- gracefully if an exit statement is encountered.  All of this
  -- makes handling this statement more complicated than you might
  -- think.

  -- if expr then statements

  expect( if_t );                                          -- "if"
  if token = if_t then                                     -- this error is
     err( "redundant " & optional_yellow( "if" ) );          -- from GNAT
  end if;
  ParseStaticExpression( expr_val, expr_type );            -- expression
  if type_checks_done then
     b := expr_val = "1";                                  -- to real boolean
  elsif not baseTypesOK( boolean_t, expr_type ) then          -- not a bool result?
     err( "boolean expression expected" );
  else                                                     -- else convert bool
     b := expr_val = "1";                                  -- to real boolean
  end if;
  expect( then_t );                                        -- "then"
  if token = then_t then                                   -- this error is
     err( "redundant " & optional_yellow( "then" ) );        -- from GNAT
  end if;
  if b then                                                -- was true?
     ParseStaticBlock( elsif_t, else_t );                  -- handle if block
     handled := true;                                      -- remember we did it
                                                           -- even elsifs and else line
  else                                                     -- otherwise
     SkipStaticBlock( elsif_t, else_t );                   -- skip if block
  end if;

  -- elsif expr then statements

  -- temporarily switch to syntax check mode when required to skip expression

  while token = elsif_t loop                               -- a(nother) elsif?
     if handled then                                       -- already handled?
        backup_sc := syntax_check;                         -- don't exec elsif
        syntax_check := true;                              -- expression
     end if;
     expect( elsif_t );                                    -- "elsif"
     if token = elsif_t then                               -- this error is
        err( "redundant " & optional_yellow( "elsif" ) );    -- from GNAT
     end if;
     ParseStaticExpression( expr_val, expr_type );         -- expression
     if type_checks_done then
        b := expr_val = "1";                               -- to real boolean
     elsif not baseTypesOK( boolean_t, expr_type ) then       -- not bool result?
        err( "boolean expression expected" );
     else                                                  -- else convert bool
        b := expr_val = "1";                               -- to real boolean
     end if;
     if handled then                                       -- already handled?
        syntax_check := backup_sc;                         -- restore flag
     end if;                                               -- for SkipBlock
     expect( then_t );                                     -- "then"
     if token = then_t then                                -- this is from
        err( "redundant " & optional_yellow( "then" ) );     -- GNAT
     end if;
     if b and not handled then                             -- true (and not previously done)
        ParseStaticBlock( elsif_t, else_t );               -- handle the elsif block
        handled := true;                                   -- remember we did it
     else                                                  -- otherwise
        SkipStaticBlock( elsif_t, else_t );                -- skip elsif block
     end if;
  end loop;

  -- by this point syntax check mode should be restored (if it was altered)

  -- else statements

  if token = else_t then                                   -- else part?
     if handled then                                       -- already handled?
        backup_sc := syntax_check;                         -- don't exec else
        syntax_check := true;                              -- for --trace
     end if;
     expect( else_t );                                     -- "else"
     if handled then                                       -- already handled?
        syntax_check := backup_sc;                         -- restore flag
     end if;                                               -- for SkipBlock
     if token = else_t then                                -- this is from
        err( "redundant " & optional_yellow( "else" ) );     -- GNAT
     end if;
     if not handled then                                   -- nothing handled yet?
        ParseStaticBlock;                                  -- handle else block
     else                                                  -- otherwise
        SkipStaticBlock;                                   -- skip else block
     end if;
  end if;

  -- end if

  expect( end_t );                                         -- "end if"
  expect( if_t );

end ParseStaticIfBlock;


-----------------------------------------------------------------------------
--
-- CASE
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE STANDARD CASE HEADER PART
--
-- ... id [, id ...] is
--
-- Parse the list of identifiers after "case".  Consume "is" as well.
-----------------------------------------------------------------------------

procedure ParseStandardCaseHeaderPart(
  test_ids : in out vector_identifier_lists.vector;
  test_len : out count_type
) is
  test_id  : identifier;
begin
  test_len := 0;

 -- expect one or more test identifiers

  loop
      ParseIdentifier( test_id );                          -- identifier to test
      if type_checks_done or else class_ok( test_id, varClass ) then
         if not vector_identifier_lists.contains( test_ids,test_id ) then
            vector_identifier_lists.append( test_ids, test_id );
            identifiers( test_id ).wasReferenced := true;
            test_len := test_len + 1;
         else
            err( optional_yellow( to_string( identifiers( test_id ).name) ) &
                 " is already in the case identifier list" );
         end if;
      end if;
  exit when token /= symbol_t and identifiers( token ).value.all /= ",";
      expectParameterComma;
      if token = is_t then
         err( "missing case identifier" );
      end if;
  end loop;

  if test_len > 1 then
     if onlyAda95 then
        err( "case must have one identifier only with " & optional_yellow( "pragma ada_95" ) );
     end if;
  end if;

  expect( is_t );                                       -- "is"

end ParseStandardCaseHeaderPart;


-----------------------------------------------------------------------------
--  PARSE CASE IN HEADER PART
--
-- ... in id [, id ...] out id [, id ...] is
--
-- Parse the list of identifiers after "in" and after "out".  Consume "is"
-- as well.
-----------------------------------------------------------------------------

procedure ParseCaseInHeaderPart(
  test_ids : in out vector_identifier_lists.vector;
  test_len : out count_type;
  return_ids : in out vector_identifier_lists.vector;
  return_len : out count_type
) is
  test_id   : identifier;
  return_id : identifier;
begin
  test_len := 0;
  return_len := 0;

  if onlyAda95 then
     err( "case in is not allowed with " & optional_yellow( "pragma ada_95" ) );
  end if;
  expect( in_t );                                          -- "in"

 -- expect one or more test identifiers

  loop
      ParseIdentifier( test_id );                          -- identifier to test
      if type_checks_done or else class_ok( test_id, varClass ) then
         vector_identifier_lists.append( test_ids, test_id );
         identifiers( test_id ).wasReferenced := true;
         test_len := test_len + 1;
      end if;
  exit when token /= symbol_t and identifiers( token ).value.all /= ",";
      expectParameterComma;
  end loop;

  -- for a decision table, there is an out part

  expect( out_t );
  loop
      if type_checks_done or else class_ok( token, varClass ) then

         -- TODO: Refactor with ParseAssignment
         -- Not all identifiers can be written to

         checkVarUsageQualifier( token );                       -- no constants

         if identifiers( token ).list then
            err( "array parameters not yet supported" );
         elsif identifiers( getBaseType( identifiers( token ).kind ) ).kind = root_record_t then
            err( "records not yet supported" );
         elsif getBaseType( identifiers( token ).kind ) = command_t then
            err( "commands not yet supported" );
         end if;

         if not error_found then
            return_len := return_len + 1;                       -- count idents

            -- add to list

            ParseIdentifier( return_id );
            vector_identifier_lists.append( return_ids, return_id );
            identifiers( return_id ).wasWritten := true;
         end if;
      end if;
  exit when token /= symbol_t and identifiers( token ).value.all /= ",";
      expectParameterComma;
  end loop;

  expect( is_t );                                       -- "is"

end ParseCaseInHeaderPart;


-----------------------------------------------------------------------------
--  PARSE CASE IN BIG ARROW PART
--
-- "=>" expr [, expr...] ;
--
-- Parse the expression list after the big arrow.  Consume the final
-- semi-colon.
-----------------------------------------------------------------------------

procedure ParseCaseInBigArrowPart(
  return_ids : in out vector_identifier_lists.vector;
  return_len : count_type;
  b2         : boolean;
  handled   : in out boolean
) is
  return_id  : identifier;
  outExpr  : unbounded_string;
  outType  : identifier;
begin
  expect( symbol_t, "=>" );                             -- "=>"
  if b2 and not handled and not exit_block then         -- handled yet?
     if trace then
        put_trace( "executing" );
     end if;
     -- for decision table
     for return_idx in 1..return_len loop
        begin
           return_id := vector_identifier_lists.element( return_ids, positive( return_idx ) );
        exception when constraint_error =>
           err( gnat.source_info.source_location &
             ": internal error: out identifier" & return_idx'img & " does not exist" );
        end;
        ParseExpression( outExpr, outType );
        --if baseTypesOK( identifiers( return_id ).kind, outType ) then
           if isExecutingCommand then

              -- TODO: refactor me with parse assignment

              -- Type testing and casting

              -- try to assign an exception to a universal type.  We need to flag that as
              -- a special case
              if outType = exception_t then
                 err( "exceptions cannot be assigned" );
              elsif type_checks_done or else baseTypesOK( identifiers( return_id ).kind,outType ) then
                 if syntax_check then
                    identifiers( outType ).wasCastTo := true;
                 end if;
                 if isExecutingCommand then
                    outExpr := castToType( outExpr, outtype );
                 end if;
              end if;

              identifiers( return_id ).value.all := outExpr;
              if trace then
                 put_trace( to_string( identifiers( return_id ).name ) & " := " & to_string( identifiers( return_id ).value.all ) );
              end if;
           --end if;
        end if;
        if return_idx < return_len then
           expectParameterComma;
        end if;
     end loop;
     handled := true;                                   -- and remember done
  else
     -- skip expression
     for return_idx in 1..return_len loop
        ParseExpression( outExpr, outType );
        if return_idx < return_len then
           expectParameterComma;
        end if;
     end loop;
  end if;
  expect( symbol_t, ";" );
end ParseCaseInBigArrowPart;


-----------------------------------------------------------------------------
--  PARSE STANDARD CASE BIG ARROW PART
--
-- "=>" statements ;
--
-- Parse the block of statements.  Consume the final semi-colon.
-----------------------------------------------------------------------------

procedure ParseStandardCaseBigArrowPart(
  b1        : boolean;
  handled   : in out boolean
) is
begin
     expect( symbol_t, "=>" );                             -- "=>"
     if b1 and not handled and not exit_block then         -- handled yet?
        ParseBlock( when_t );                           -- if not, handle
        handled := true;                                   -- and remember done
     else
        SkipBlock( when_t );                               -- else skip case
     end if;
end ParseStandardCaseBigArrowPart;


-----------------------------------------------------------------------------
--  PARSE CASE WHEN PART
--
-- when val [|val ...]
--
-- Parse the listof values and determine if the result is true or false.
-----------------------------------------------------------------------------

procedure ParseCaseWhenPart(
  test_ids : in out vector_identifier_lists.vector;
  test_len : count_type;
  b2       : in out boolean
) is
  test_id  : identifier;
  test_idx : count_type;
  case_id  : identifier;
  boxCount : count_type := 0;
  b1       : boolean := false;
begin
  expect( when_t );                                    -- "when"
  if error_found or token = others_t then
     return;
  end if;

  -- this should be ParseConstantIdentifier
  b2 := true;                                          -- assume it succeeds
  test_idx:= 1;                                        -- from first index
  while test_idx <= test_len loop                       -- all test ids
     if token = symbol_t and identifiers( token ).value.all = "=>" then
        err( "missing when condition" );
     end if;
     test_id := vector_identifier_lists.element( test_ids,positive( test_idx ) );
     b1 := false;                                      -- assume case fails
     loop
        if token = strlit_t then                          -- strlit allowed
           if type_checks_done or else uniTypesOK( identifiers( test_id ).kind, uni_string_t ) then
              case_id := token;
              getNextToken;
           end if;
        elsif token = charlit_t then                      -- charlit allowed
           if type_checks_done or else uniTypesOK( identifiers( test_id ).kind, uni_string_t ) then
              case_id := token;
              getNextToken;
           end if;
        elsif token = number_t then                       -- num lit allowed
           if type_checks_done or else uniTypesOk( identifiers( test_id ).kind, uni_numeric_t ) then
              case_id := token;
              getNextToken;
           end if;
        elsif token = symbol_t and identifiers( token ).value.all = "<>" then -- box allowed
           boxCount := boxCount + 1;
           b1 := true;
           case_id := symbol_t;
           -- I don't check for pragma ada_95 here because multiple when
           -- conditions are already not allowed, and a box with single
           -- when condition is already an error for not using when others.
           getNextToken;
        else                                             -- constant allowed
           ParseIdentifier( case_id );                         -- get the case
           if identifiers( case_id ).usage /= constantUsage    -- is constant
              and identifiers( case_id ).class /= enumClass then -- or enum?
              err( "variable not allowed as a case" );         -- error if not
           elsif type_checks_done or else baseTypesOK( identifiers( test_id ).kind,
                 identifiers( case_id ).kind ) then            -- types good?
              null;
           end if;
        end if;
        if not error_found then                         -- OK? check case
           if case_id /= symbol_t then                  -- if not box token
              b1 := b1 or                                  -- against test var
                Ada.Strings.Unbounded.trim( identifiers( test_id ).value.all, Ada.Strings.left ) =
                Ada.Strings.Unbounded.trim( identifiers( case_id ).value.all, Ada.Strings.left );
           end if;
        end if;
        exit when error_found or token /= symbol_t or identifiers( token ).value.all /= "|";
        expect( symbol_t, "|" );                        -- expect alternate
     end loop;
     if not error_found then
        b2 := b2 and b1;
     end if;
     exit when error_found or token /= symbol_t or identifiers( token ).value.all /= ",";
     expectParameterComma;                                      -- expect alternate
     test_idx := test_idx + 1;
     if test_idx > test_len then
        err("too many cases compared to case identifier list" );
     end if;
  end loop;
  if test_idx < test_len then
     err("too few cases compared to case identifier list" );
  end if;
  if test_len = boxCount then
     err( "<> for all conditions should be " & optional_yellow( "when others" ) );
  end if;
end ParseCaseWhenPart;


-----------------------------------------------------------------------------
--  PARSE CASE IN BLOCK
--
-----------------------------------------------------------------------------

procedure ParseCaseInBlock is
  test_ids : vector_identifier_lists.vector;
  test_len : count_type := 0;
  return_ids : vector_identifier_lists.vector;
  return_len : count_type := 0;
  handled  : boolean := false;
  b2       : boolean := false;
begin

  ParseCaseInHeaderPart( test_ids, test_len, return_ids, return_len );

  -- this will have an error during compilation.
  if token /= when_t then                                 -- first when missing?
     expect( when_t );                                    -- force error
  end if;
  while token = when_t loop
     ParseCaseWhenPart( test_ids, test_len, b2 );
     exit when error_found or token = others_t;
     ParseCaseInBigArrowPart( return_ids, return_len, b2, handled );
  end loop;

  -- others part

  if token /= others_t then                                -- a little clearer
     err( "when others expected" );                        -- if pointing at
  end if;                                                  -- end case
  expect( others_t );                                      -- "others"

  ParseCaseInBigArrowPart( return_ids, return_len, true, handled );

end ParseCaseInBlock;


-----------------------------------------------------------------------------
--  PARSE STANDARD CASE BLOCK
--
-- Handle a case statement.
-- Syntax: case-block = "case" ident [,ident] "is"
-- "when" const-ident ["|"...] [, ...]  "=>" ...
-- "when" others =>" ..."
-- "end" "case"
-----------------------------------------------------------------------------


procedure ParseStandardCaseBlock is
  test_ids : vector_identifier_lists.vector;
  test_len : count_type := 0;
  handled  : boolean := false;
  b1       : boolean := false;
begin

  -- expect one or more test identifiers

  ParseStandardCaseHeaderPart( test_ids, test_len );

  if token /= when_t then                                 -- first when missing?
     expect( when_t );                                    -- force error
  end if;
  while token = when_t loop
     ParseCaseWhenPart( test_ids, test_len, b1 );
     exit when error_found or token = others_t;
     ParseStandardCaseBigArrowPart( b1, handled );
  end loop;

  -- others part

  if token /= others_t then                                -- a little clearer
     err( "when others expected" );                        -- if pointing at
  end if;                                                  -- end case
  expect( others_t );                                      -- "others"

  ParseStandardCaseBigArrowPart( true, handled );


  --ParseIdentifier( test_id );                          -- identifier to test
  --if type_checks_done or else class_ok( test_id, varClass ) then
  --   null;
  --end if;

  -- we allow const because parameters are consts in Bush 1.x.

  -- expect( is_t );                                       -- "is"

  --if token /= when_t then                                 -- first when missing?
  --   expect( when_t );                                    -- force error
  --end if;
  --while token = when_t loop
  --   expect( when_t );                                    -- "when"
  --   exit when error_found or token = others_t;
  --   -- this should be ParseConstantIdentifier
  --    b1 := false;                                        -- assume case fails
  --    loop
  --      if token = strlit_t then                          -- strlit allowed
  --         if type_checks_done or else uniTypesOK( identifiers( test_id ).kind, uni_string_t ) then
  --            case_id := token;
  --            getNextToken;
  --         end if;
  --      elsif token = charlit_t then                      -- charlit allowed
  --         if type_checks_done or else uniTypesOK( identifiers( test_id ).kind, uni_string_t ) then
  --            case_id := token;
  --            getNextToken;
  --         end if;
  --      elsif token = number_t then                       -- num lit allowed
  --         if type_checks_done or else uniTypesOk( identifiers( test_id ).kind, uni_numeric_t ) then
  --            case_id := token;
  --            getNextToken;
  --         end if;
  --      else                                             -- constant allowed
  --         ParseIdentifier( case_id );                         -- get the case
  --         if identifiers( case_id ).usage /= constantUsage    -- is constant
  --            and identifiers( case_id ).class /= enumClass then -- or enum?
  --            err( "variable not allowed as a case" );         -- error if not
  --         elsif type_checks_done or else baseTypesOK( identifiers( test_id ).kind,
  --               identifiers( case_id ).kind ) then            -- types good?
  --            null;
  --         end if;
  --      end if;
  --      if not error_found then                         -- OK? check case
  --         b1 := b1 or                                  -- against test var
  --           Ada.Strings.Unbounded.trim( identifiers( test_id ).value.all, Ada.Strings.left ) =
  --           Ada.Strings.Unbounded.trim( identifiers( case_id ).value.all, Ada.Strings.left );
  --      end if;
  --      exit when error_found or token /= symbol_t or identifiers( token ).value.all /= "|";
  --      expect( symbol_t, "|" );                        -- expect alternate
  --   end loop;

  --   expect( symbol_t, "=>" );                             -- "=>"
  --   if b1 and not handled and not exit_block then         -- handled yet?
  --      ParseBlock( when_t );                           -- if not, handle
  --      handled := true;                                   -- and remember done
  --   else
  --      SkipBlock( when_t );                               -- else skip case
  --   end if;
  --end loop;

  -- others part

  --if token /= others_t then                                -- a little clearer
  --   err( "when others expected" );                        -- if pointing at
  --end if;                                                  -- end case
  --expect( others_t );                                      -- "others"
  --expect( symbol_t, "=>" );                                -- "=>"
  --if not handled and not exit_block then                   -- not handled yet?
  --   ParseBlock;                                        -- handle now
  --else                                                     -- else just
  --   SkipBlock;                                            -- skip
  --end if;

end ParseStandardCaseBlock;


-----------------------------------------------------------------------------
--  PARSE CASE BLOCK
--
-----------------------------------------------------------------------------

procedure ParseCaseBlock is
begin
  expect( case_t );

  if token = in_t then
     ParseCaseInBlock;
  else
     ParseStandardcaseBlock;
  end if;

  -- end case

  expect( end_t );                                         -- "end case"
  expect( case_t );
end ParseCaseBlock;


--procedure ParseCaseBlock is
--  test_id : identifier;
--  case_id : identifier;
--  handled : boolean := false;
--  b       : boolean := false;
--begin
--
--  -- case id is
--
--  expect( case_t );                                        -- "case"
--  ParseIdentifier( test_id );                              -- identifier to test
--  -- we allow const because parameters are consts in Bush 1.x.
--  --if class_ok( test_id, constClass, varClass ) then
--  if type_checks_done or else class_ok( test_id, varClass ) then
--     expect( is_t );                                       -- "is"
--  end if;
--
--  -- when const-id =>
--
--  if token /= when_t then                                 -- first when missing?
--     expect( when_t );                                    -- force error
--  end if;
--  while token = when_t loop
--     expect( when_t );                                    -- "when"
--     exit when error_found or token = others_t;
--     -- this should be ParseConstantIdentifier
--     b := false;                                          -- assume case fails
--     loop
--        if token = strlit_t then                          -- strlit allowed
--           if type_checks_done or else uniTypesOK( identifiers( test_id ).kind, uni_string_t ) then
--              case_id := token;
--              getNextToken;
--           end if;
--        elsif token = charlit_t then                      -- charlit allowed
--           if type_checks_done or else uniTypesOK( identifiers( test_id ).kind, uni_string_t ) then
--              case_id := token;
--              getNextToken;
--           end if;
--        elsif token = number_t then                       -- num lit allowed
--           if type_checks_done or else uniTypesOk( identifiers( test_id ).kind, uni_numeric_t ) then
--              case_id := token;
--              getNextToken;
--           end if;
--        else                                             -- constant allowed
--           ParseIdentifier( case_id );                         -- get the case
--           if identifiers( case_id ).usage /= constantUsage    -- is constant
--              and identifiers( case_id ).class /= enumClass then -- or enum?
--              err( "variable not allowed as a case" );         -- error if not
--           elsif type_checks_done or else baseTypesOK( identifiers( test_id ).kind,
--                 identifiers( case_id ).kind ) then            -- types good?
--              null;
--           end if;
--        end if;
--        if not error_found then                         -- OK? check case
--           b := b or                                    -- against test var
--             Ada.Strings.Unbounded.trim( identifiers( test_id ).value.all, Ada.Strings.left ) =
--             Ada.Strings.Unbounded.trim( identifiers( case_id ).value.all, Ada.Strings.left );
--        end if;
--        exit when error_found or token /= symbol_t or identifiers( token ).value.all /= "|";
--        expect( symbol_t, "|" );                        -- expect alternate
--     end loop;
--     expect( symbol_t, "=>" );                             -- "=>"
--     if b and not handled and not exit_block then          -- handled yet?
--        ParseBlock( when_t );                              -- if not, handle
--        handled := true;                                   -- and remember done
--     else
--        SkipBlock( when_t );                               -- else skip case
--     end if;
--  end loop;
--
--  -- others part
--
--  if token /= others_t then                                -- a little clearer
--     err( "when others expected" );                        -- if pointing at
--  end if;                                                  -- end case
--  expect( others_t );                                      -- "others"
--  expect( symbol_t, "=>" );                                -- "=>"
--  if not handled and not exit_block then                   -- not handled yet?
--     ParseBlock;                                           -- handle now
--  else                                                     -- else just
--     SkipBlock;                                            -- skip
--  end if;
--
--  -- end case
--
--  expect( end_t );                                         -- "end case"
--  expect( case_t );
--
--end ParseCaseBlock;


-----------------------------------------------------------------------------
--  STATIC CASE BLOCK
--
-- Conditionally execute code based on a static expression value.  This is
-- tied to parsePolicy: only policy block statements allowed in the code.
-----------------------------------------------------------------------------

procedure ParseStaticCaseBlock is
-- Syntax: case-block = "case" ident "is" "when" const-ident ["|"...] "=>" ...
-- "when others =>" ..."end case"
-- constants only
  test_ids : vector_identifier_lists.vector;
  test_id  : identifier;
  test_idx : count_type;
  test_len : count_type := 0;
  case_id  : identifier;
  handled  : boolean := false;
  b1       : boolean := false;
  b2       : boolean := false;
begin

  -- case id is

  expect( case_t );                                        -- "case"

 -- expect one or more test identifiers

  loop
      ParseStaticIdentifier( test_id );                        -- identifier to test
      if type_checks_done or else class_ok( test_id, varClass ) then
         if identifiers( test_id ).usage /= constantUsage then
            err( "constant expected" );
         else
            vector_identifier_lists.append( test_ids, test_id );
            test_len := test_len + 1;
         end if;
      end if;
  exit when token /= symbol_t and identifiers( token ).value.all /= ",";
      expectParameterComma;
  end loop;
  if onlyAda95 then
     if test_len > 1 then
        err( "multiple case identifiers are not allowed with " & optional_yellow( "pragma ada_95" ) );
     end if;
  end if;

  expect( is_t );                                       -- "is"

  if token /= when_t then                                 -- first when missing?
     expect( when_t );                                    -- force error
  end if;
  while token = when_t loop
     expect( when_t );                                    -- "when"
     exit when error_found or token = others_t;
     -- this should be ParseConstantIdentifier
     b2 := true;
     test_idx := 1;
     while test_idx <= test_len loop
        test_id := vector_identifier_lists.element( test_ids,positive( test_idx ) );
        b1 := false;                                      -- assume case fails
     loop
        if token = strlit_t then                          -- strlit allowed
           if type_checks_done or else uniTypesOK( identifiers( test_id ).kind, uni_string_t ) then
              case_id := token;
              getNextToken;
           end if;
        elsif token = charlit_t then                      -- charlit allowed
           if type_checks_done or else uniTypesOK( identifiers( test_id ).kind, uni_string_t ) then
              case_id := token;
              getNextToken;
           end if;
        elsif token = number_t then                       -- num lit allowed
           if type_checks_done or else uniTypesOk( identifiers( test_id ).kind, uni_numeric_t ) then
              case_id := token;
              getNextToken;
           end if;
        else                                             -- constant allowed
           ParseIdentifier( case_id );                         -- get the case
           if identifiers( case_id ).usage /= constantUsage    -- is constant
              and identifiers( case_id ).class /= enumClass then -- or enum?
              err( "variable not allowed as a case" );         -- error if not
           elsif type_checks_done or else baseTypesOK( identifiers( test_id ).kind,
                 identifiers( case_id ).kind ) then            -- types good?
              null;
           end if;
        end if;
        if not error_found then                         -- OK? check case
              b1 := b1 or                                  -- against test var
                Ada.Strings.Unbounded.trim( identifiers( test_id ).value.all, Ada.Strings.left ) =
                Ada.Strings.Unbounded.trim( identifiers( case_id ).value.all, Ada.Strings.left );
        end if;
        exit when error_found or token /= symbol_t or identifiers( token ).value.all /= "|";
        expect( symbol_t, "|" );                        -- expect alternate
     end loop;
        if not error_found then
           b2 := b2 and b1;
        end if;
        exit when error_found or token /= symbol_t or identifiers( token ).value.all /= ",";
        expectParameterComma;                                       -- expect alternate
        test_idx := test_idx + 1;
        if test_idx > test_len then
           err("too many cases compared to case identifier list" );
        end if;
     end loop;
     if test_idx < test_len then
        err("too few cases compared to case identifier list" );
     end if;
     expect( symbol_t, "=>" );                             -- "=>"
     if b2 and not handled and not exit_block then         -- handled yet?
        ParseStaticBlock( when_t );                        -- if not, handle
        handled := true;                                   -- and remember done
     else
        SkipStaticBlock( when_t );                         -- else skip case
     end if;
  end loop;

  -- others part

  if token /= others_t then                                -- a little clearer
     err( "when others expected" );                        -- if pointing at
  end if;                                                  -- end case
  expect( others_t );                                      -- "others"
  expect( symbol_t, "=>" );                                -- "=>"
  if not handled and not exit_block then                   -- not handled yet?
     ParseBlock;                                           -- handle now
  else                                                     -- else just
     SkipBlock;                                            -- skip
  end if;

  -- end case

  expect( end_t );                                         -- "end case"
  expect( case_t );

end ParseStaticCaseBlock;


--procedure ParseStaticCaseBlock is
---- Syntax: case-block = "case" ident "is" "when" const-ident ["|"...] "=>" ...
---- "when others =>" ..."end case"
---- constants only
--  test_id : identifier;
--  case_id : identifier;
--  handled : boolean := false;
--  b       : boolean := false;
--begin
--
--  -- case id is
--
--  expect( case_t );                                        -- "case"
--  ParseStaticIdentifier( test_id );                        -- identifier to test
--  -- we allow const because parameters are consts in Bush 1.x.
--  if type_checks_done or else class_ok( test_id, varClass ) then
--     if identifiers( test_id ).usage /= constantUsage then
--        err( "constant expected" );
--     end if;
--     expect( is_t );                                       -- "is"
--  end if;
--
--  -- when const-id =>
--
--  if token /= when_t then                                 -- first when missing?
--     expect( when_t );                                    -- force error
--  end if;
--  while token = when_t loop
--     expect( when_t );                                    -- "when"
--     exit when error_found or token = others_t;
--     -- this should be ParseConstantIdentifier
--     b := false;                                          -- assume case fails
--     loop
--        if token = strlit_t then                          -- strlit allowed
--           if type_checks_done or else baseTypesOK( identifiers( test_id ).kind, string_t ) then
--              case_id := token;
--              getNextToken;
--           end if;
--        elsif token = charlit_t then                      -- charlit allowed
--           if type_checks_done or else baseTypesOK( identifiers( test_id ).kind, character_t ) then
--              case_id := token;
--              getNextToken;
--           end if;
--        elsif token = number_t then                       -- num lit allowed
--           if type_checks_done or else uniTypesOk( identifiers( test_id ).kind, uni_numeric_t ) then
--              case_id := token;
--              getNextToken;
--           end if;
--        else                                             -- constant allowed
--           ParseIdentifier( case_id );                         -- get the case
--           if identifiers( case_id ).usage /= constantUsage    -- is constant
--              and identifiers( case_id ).class /= enumClass then -- or enum?
--              err( "variable not allowed as a case" );         -- error if not
--           elsif type_checks_done or else baseTypesOK( identifiers( test_id ).kind,
--                 identifiers( case_id ).kind ) then            -- types good?
--              null;
--           end if;
--        end if;
--        if not error_found then                         -- OK? check case
--           b := b or                                    -- against test var
--             identifiers( test_id ).value.all = identifiers( case_id ).value.all;
--        end if;
--        exit when error_found or token /= symbol_t or identifiers( token ).value.all /= "|";
--        expect( symbol_t, "|" );                        -- expect alternate
--     end loop;
--     expect( symbol_t, "=>" );                             -- "=>"
--     if b and not handled and not exit_block then          -- handled yet?
--        ParseStaticBlock( when_t );                        -- if not, handle
--        handled := true;                                   -- and remember done
--     else
--        SkipStaticBlock( when_t );                         -- else skip case
--     end if;
--  end loop;
--
--  -- others part
--
--  if token /= others_t then                                -- a little clearer
--     err( "when others expected" );                        -- if pointing at
--  end if;                                                  -- end case
--  expect( others_t );                                      -- "others"
--  expect( symbol_t, "=>" );                                -- "=>"
--  if not handled and not exit_block then                   -- not handled yet?
--     ParseBlock;                                           -- handle now
--  else                                                     -- else just
--     SkipBlock;                                            -- skip
--  end if;
--
--  -- end case
--
--  expect( end_t );                                         -- "end case"
--  expect( case_t );
--
--end ParseStaticCaseBlock;


-----------------------------------------------------------------------------
-- LOOPS
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE LOOP BLOCK
--
-- Handle a loop statement.
-- Syntax: loop-block = "loop" ... "end loop"
-----------------------------------------------------------------------------

procedure ParseLoopBlock is
  old_exit_block : constant boolean := exit_block;
begin

  pushBlock( newScope => false, newName => "loop loop" );  -- start new scope

  if syntax_check or exit_block then
     expect( loop_t );                                     -- "loop"
     ParseBlock;                                           -- check loop block
     goto loop_done;
  end if;

  loop
     expect( loop_t );                                     -- "loop"
     ParseBlock;                                           -- handle loop block
     exit when exit_block or error_found or token = eof_t;
     topOfBlock;                                           -- jump to top of block
  end loop;

<<loop_done>>
  pullBlock;                                               -- end of while scope
  if not syntax_check and not old_exit_block then          -- ignore exit when checking
     if exit_block and not done then                       -- exiting and not returning?
        if trace then
           Put_trace( "exited loop" );
        end if;
        exit_block := false;                               -- we handled exit_block
     end if;
  end if;

  expect( end_t );                                         -- "end loop"
  expect( loop_t );
end ParseLoopBlock;


-----------------------------------------------------------------------------
--  PARSE WHILE BLOCK
--
-- Handle a while loop statement.
-- Syntax: while-block = "while" bool-expr "loop" ... "end loop"
-----------------------------------------------------------------------------

procedure ParseWhileBlock is
  expr_val  : unbounded_string;
  expr_type : identifier;
  old_exit_block : constant boolean := exit_block;
begin
  pushBlock( newScope => false, newName => "while loop" ); -- start new scope

  if syntax_check or exit_block then
     expect( while_t );                                    -- "while"
     if token = while_t then                               -- this is from
        err( "redundant " & optional_yellow( "while" ) );    -- GNAT
     end if;
     ParseExpression( expr_val, expr_type );               -- expression
     if not type_checks_done and then not baseTypesOK( boolean_t, expr_type ) then       -- not boolean?
        err( "boolean expression expected" );
     end if;
     expect( loop_t );                                     --- "loop"
     ParseBlock;                                           -- check while block
     goto loop_done;
  end if;

  loop
     expect( while_t );                                    -- "while"
     ParseExpression( expr_val, expr_type );               -- expression
     -- The type of the while expression can only change during a while loop
     -- with a typeset at the command prompt.
     if not type_checks_done and then not baseTypesOK( boolean_t, expr_type ) then       -- not boolean?
        err( "boolean expression expected" );
        exit;
     elsif expr_val /= "1" or error_found or exit_block then -- skipping?
        expect( loop_t );                                  -- "loop"
        SkipBlock;                                         -- skip while block
        exit;                                              -- and quit
     end if;                                               -- otherwise do loop
     if trace then
        put_trace( "expression is true" );
     end if;
     expect( loop_t );                                     -- "loop"
     ParseBlock;                                           -- handle while block
     exit when exit_block or error_found or token = eof_t;
     topOfBlock;                                           -- jump to top of block
  end loop;

<<loop_done>>
  pullBlock;                                               -- end of while scope
  if not syntax_check and not old_exit_block then          -- ignore exit when checking
     if exit_block and not done then                          -- exiting and not returning?
        if trace then
           Put_trace( "exited while loop" );
        end if;
        exit_block := false;                                  -- we handled exit_block
     end if;
  end if;

  expect( end_t );                                         -- "end loop"
  expect( loop_t );
end ParseWhileBlock;


-----------------------------------------------------------------------------
--  PARSE FOR BLOCK
--
-- Handle a for loop statement.
-- Syntax: for-block = "for" local-var "in" expr ".." expr "loop" ... "end loop"
-----------------------------------------------------------------------------

procedure ParseForBlock is
  expr1_val  : unbounded_string;
  expr1_type : identifier;
  expr2_val  : unbounded_string;
  expr2_type : identifier;
  expr2_num  : long_float;
  for_var    : identifier;
  firstTime  : boolean := true;
  isReverse  : boolean := false;
  old_exit_block : constant boolean := exit_block;
  for_name   : unbounded_string;
begin

  pushBlock( newScope => true, newName => "for loop" );  -- start new scope
  -- well, not strictly a new scope, but we'll need to do this
  -- to implement automatic declaration of the index variable

  if syntax_check or exit_block then
     -- this is complicated enough it should be in it's own nested procedure
     expect( for_t );                                   -- "for"
     for_name := identifiers( token ).name;             -- save var name
     if token = number_t then
        err( optional_yellow( "identifier" ) & " expected, not a " &
             optional_yellow( "number" ) );
     elsif token = strlit_t then
        err( optional_yellow( "identifier" ) & " expected, not a " &
             optional_yellow( "string literal" ) );
     elsif token = backlit_t then
        err( optional_yellow( "identifier" ) & " expected, not a " &
             optional_yellow( "backquoted literal" ) );
     elsif token = charlit_t then
        err( optional_yellow( "identifier" ) & " expected, not a " &
             optional_yellow( "character literal" ) );
     elsif is_keyword( token ) and token /= eof_t then
        err( optional_yellow( "identifier" ) & " expected, not a " &
             optional_yellow( "keyword" ) );
     elsif token = symbol_t then
        err( optional_yellow( "identifier" ) & " expected, not a " &
             optional_yellow( "symbol" ) );
     elsif identifiers( token ).kind = new_t then          -- for var
        discardUnusedIdentifier( token );               -- brand new? toss it
     end if;                                            -- we'll declare it
     getNextToken;                                      -- declare after range
     expect( in_t );                                    -- "in"
     if token = reverse_t then                          -- "reverse"?
        isReverse := true;
        expect( reverse_t );
     end if;
     ParseExpression( expr1_val, expr1_type );          -- low range
     expect( symbol_t, ".." );                          -- ".."
     ParseExpression( expr2_val, expr2_type );          -- high range
     -- declare for var down here in case older var with same name
     -- used in for loop range (so that for k in k..k+1 is legit)
     declareIdent( for_var, for_name, uni_numeric_t);   -- declare for var
     -- the for index is not covered by unused identifiers because it
     -- might not be used within the loop...might just be a repeat loop
     if syntax_check and then not error_found then
        identifiers( for_var ).wasReferenced := true;
        --identifiers( for_var ).referencedByThread := getThreadName;
        identifiers( for_var ).wasWritten := true;
        identifiers( for_var ).wasFactor := true;
     end if;
     if type_checks_done or else baseTypesOK( expr1_type, expr2_type ) then      -- check types
        if getUniType( expr1_type ) = uni_numeric_t then
           null;
       elsif getUniType( expr1_type ) = root_enumerated_t then
           null;
       end if;
       if not error_found then
          if isReverse then
             identifiers( for_var ).kind := expr2_type;     -- this type
          else
             identifiers( for_var ).kind := expr1_type;     -- this type
           end if;
        end if;
     end if;
     expect( loop_t );
     ParseBlock;                                           -- check for block
     goto abort_loop;
  end if;

  loop
     expect( for_t );                                      -- "for"
     if firstTime then
        if identifiers( token ).kind = new_t then          -- for var
           for_var := token;                               -- brand new? ok
        else                                               -- else declare locally
           declareIdent( for_var,                          -- will be const below
              identifiers( token ).name,
              uni_numeric_t );
           -- This variable is written to by the for command itself.  So
           -- mark that for later identifier usage tests.
        end if;
        getNextToken;
        expect( in_t );                                    -- "in"
        if token = reverse_t then                          -- "reverse"?
           isReverse := true;
           expect( reverse_t );
        end if;
        ParseExpression( expr1_val, expr1_type );          -- low range
        expect( symbol_t, ".." );                          -- ".."
        ParseExpression( expr2_val, expr2_type );          -- high range
        if verboseOpt then
           put_trace( "in " & to_string( expr1_val ) & ".." & to_string( expr2_val ) );
        end if;

        --if error_found then                              -- errors?
        --    goto abort_loop;                             -- go no further
        --end if;
        -- Another strange case only possible at the command prompt and
        -- with typeset changing the data type.
        if type_checks_done or else baseTypesOK( expr1_type, expr2_type ) then      -- check types
           if getUniType( expr1_type ) = uni_numeric_t then
              null;
           elsif getUniType( expr1_type ) = root_enumerated_t then
              null;
           else
              err( "numeric or enumerated type expected" );
              -- should be err_previous but haven't exported it yet
           end if;
           if not error_found then
              if isReverse then
                 identifiers( for_var ).value.all := expr2_val; -- for var is
                 identifiers( for_var ).kind := expr2_type;     -- this type
                 identifiers( for_var ).class := varClass;      -- make const
                 identifiers( for_var ).usage := constantUsage;
                 if isExecutingCommand then
                    expr2_num := to_numeric( expr1_val );
                 end if;
              else
                 identifiers( for_var ).value.all := expr1_val; -- for var is
                 identifiers( for_var ).kind := expr1_type;     -- this type
                 identifiers( for_var ).class := varClass;      -- make const
                 identifiers( for_var ).usage := constantUsage;
                 if isExecutingCommand then
                    expr2_num := to_numeric( expr2_val );
                 end if;
              end if;
           end if;
        end if;
        expect( loop_t );                                  -- "loop"
        firstTime := false;                                -- don't do this again
     else
        -- don't interpret for line after first time
-- is this necessary any more?
        while token /= loop_t loop                         -- skip to
           getNextToken;                                 -- "loop"
        end loop;
        expect( loop_t );
        if isReverse then
           if isExecutingCommand then
              -- GCC Ada 7.4 says a conversion warning but it is wrong
              identifiers( for_var ).value.all := to_unbounded_string(
                  long_float( to_numeric( identifiers( for_var ).value.all ) - 1.0 ) );
           end if;
        else
           if isExecutingCommand then
              -- GCC Ada 7.4 says a conversion warning but it is wrong
              identifiers( for_var ).value.all := to_unbounded_string(
                  long_float( to_numeric( identifiers( for_var ).value.all ) + 1.0 ) );
           end if;
        end if;
     end if;
     if not isExecutingCommand then -- includes errors or exiting
        skipBlock;
        exit;
     elsif isReverse then
        if to_numeric( identifiers( for_var ).value.all ) < expr2_num then
           skipBlock;
           exit;
        end if;
     elsif to_numeric( identifiers( for_var ).value.all ) > expr2_num then
         skipBlock;
         exit;
     end if;
     if trace then
        put_trace(
            to_string( identifiers( for_var ).name ) & " := '" &
            to_string( identifiers( for_var ).value.all ) & "'" );
     end if;
     ParseBlock;                                           -- handle for block
     exit when exit_block or error_found or token = eof_t;
     topOfBlock;                                           -- jump to top of block
  end loop;

<<abort_loop>>
  pullBlock;                                               -- end of while scope
  if not syntax_check and not old_exit_block then          -- ignore exit when checking
     if exit_block and not done then                          -- exiting and not returning?
        if trace then
           Put_trace( "exited for loop" );
        end if;
        exit_block := false;                                  -- we handled exit_block
     end if;
  end if;

  expect( end_t );                                         -- "end loop"
  expect( loop_t );

end ParseForBlock;


-----------------------------------------------------------------------------
-- Other statements
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE DELAY
--
-- Handle a delay statement.
-- Syntax: delay expression
-- Source: Ada built-in
-----------------------------------------------------------------------------

procedure ParseDelay is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  expect( delay_t );
  ParseExpression( expr_val, expr_type );
  if type_checks_done or else baseTypesOK( expr_type, duration_t ) then
     if isExecutingCommand then
        begin
          delay duration( to_numeric( expr_val ) );
        exception when others =>
          err_exception_raised;
        end;
        if trace then
           put_trace( "duration := " & to_string( expr_val ) );
        end if;
     end if;
  end if;
end ParseDelay;


-----------------------------------------------------------------------------
--  PARSE TYPESET
--
-- Handle the typeset statement.
-- Syntax: typeset identifier is type
-- TODO: this should be converted to a build-in shell command
-----------------------------------------------------------------------------

procedure ParseTypeset is
  id     : identifier;
  typeid : identifier := eof_t;
  b      : boolean;
begin
   expect( typeset_t );
   if onlyAda95 then
      discardUnusedIdentifier( token );
      err( "typeset is not allowed with " & optional_yellow( "pragma ada_95" ) );
      return;
   elsif inputMode /= interactive and inputMode /= breakout then
      discardUnusedIdentifier( token );
      err( "typeset only allowed in an interactive session" );
      return;
   end if;
   if identifiers( token ).kind = new_t then
      ParseNewIdentifier( id );
   else
      ParseIdentifier( id );
   end if;
   if token = is_t then
      expect( is_t );
      if token = exception_t then
         err( "types cannot be changed to an exception" );
      else
         ParseIdentifier( typeid );
      end if;
   end if;
   if isExecutingCommand then
      if identifiers( id ).kind = exception_t then
         err( "exception types cannot be changed to another type" );
      elsif typeid = eof_t then
         identifiers( id ).kind := universal_t;
      elsif identifiers( id ).list then
         err( "typeset with array types not yet implemented" );
      elsif identifiers( typeid ).list then
         err( "typeset with array types not yet implemented" );
      elsif identifiers( id ).renamed_count > 0 then
         err_renaming( id );
      elsif identifiers( id ).renaming_of /= identifiers'first then
         err( "cannot change the type of a renaming" );
      else
         begin
            identifiers( id ).value.all := castToType(
               identifiers( id ).value.all, typeid );
            identifiers( id ).kind := typeid;
         exception when others =>
            err_exception_raised;
         end;
      end if;
   else
      b := deleteIdent( id );
   end if;
end ParseTypeset;


-----------------------------------------------------------------------------
--  PARSE VM
--
-- Parse virtual machine (vm) statement.
-- This command is not implemented.
-- Syntax: vm regtype, regnum
-----------------------------------------------------------------------------

procedure ParseVm is
  regtype_val  : unbounded_string;
  regtype_kind : identifier;
  regnum_val   : unbounded_string;
  regnum_kind  : identifier;
begin
  ParseExpression( regtype_val, regtype_kind );
  if baseTypesOK( regtype_kind, string_t ) then
     expectParameterComma;
     ParseExpression( regnum_val, regnum_kind );
     if baseTypesOK( regnum_kind, integer_t ) then
        null;
     end if;
  end if;
  builtins.vm( regtype_val, regnum_val );
end ParseVm;

procedure ParseProcedureBlock;
procedure ParseFunctionBlock;


-----------------------------------------------------------------------------
--  PARSE WITH
--
-- Handle an include file.
-- Syntax: with separate "file";
-----------------------------------------------------------------------------

procedure ParseWith is
begin
  -- is this true?
  --if inputMode = interactive or inputMode = breakout then
  --   err( "with can only be used in a script" );
  --end if;
  expect( with_t );
  expect( separate_t );
  if token = strlit_t then
     if syntax_check then
        if rshOpt then
           err( "subscripts are not allowed in a " & optional_yellow( "restricted shell" ) );
        else
           insertInclude( identifiers( token ).value.all );
        end if;
     end if;
  end if;
  expect( strlit_t );
  expectStatementSemicolon( contextNotes => "in with separate" );
  -- That was the end of the with separate statement.  However, remember that
  -- the subscript is embedded in the main script so we have to read the
  -- subscript header.  Only pragmas allowed before separate keyword.
  while token = pragma_t loop
      ParsePragma;
      expectStatementSemicolon( context => pragma_t );
  end loop;
  expect( separate_t );
  expectStatementSemicolon( context => separate_t );
end ParseWith;


-----------------------------------------------------------------------------
--  PARSE DECLARATIONS
--
-- Handle a set of declaration statements.
-- Syntax: declaration = "new-ident decl-part"
-----------------------------------------------------------------------------

procedure ParseDeclarations is
  var_id : identifier;
  save_syntax_check : boolean;
begin
  while token /= begin_t and token /= end_t and token /= eof_t loop
     if token = pragma_t then
        ParsePragma;
        expectStatementSemicolon( context => pragma_t );
     elsif token = type_t then
        ParseType;
        expectStatementSemicolon( context => type_t );
     elsif token = subtype_t then
        ParseSubtype;
        expectStatementSemicolon( context => subtype_t );
     elsif Token = with_t then
        -- When parsing a procedure declaration, we never want to run it.
        save_syntax_check := syntax_check;
        syntax_check := true;
        ParseWith;
        syntax_check := save_syntax_check;
     elsif Token = procedure_t then
        -- When parsing a procedure declaration, we never want to run it.
        save_syntax_check := syntax_check;
        syntax_check := true;
        ParseProcedureBlock;
        syntax_check := save_syntax_check;
     elsif Token = function_t then
        -- When parsing a function declaration, we never want to run it.
        save_syntax_check := syntax_check;
        syntax_check := true;
        ParseFunctionBlock;
        syntax_check := save_syntax_check;
     else
        ParseVariableIdentifier( var_id );
        ParseDeclarationPart( var_id, anon_arrays => true, exceptions => true ); -- var id may change...
        expectDeclarationSemicolon( context => var_id );
     end if;
  end loop;
end ParseDeclarations;


-----------------------------------------------------------------------------
--  PARSE WHEN CLAUSE
--
-- Handle a when clause, such as is attached to the end of another statement.
-- True is returned if the when clause is true
-- Syntax: ... when condition
-----------------------------------------------------------------------------

procedure ParseWhenClause( when_true : out boolean ) is
  expr_val    : unbounded_string;
  expr_type   : identifier;
begin
  expect( when_t );
  ParseExpression( expr_val, expr_type );
  if type_checks_done or else baseTypesOK( boolean_t, expr_type ) then
     if isExecutingCommand then
        when_true :=  expr_val = "1";
        if trace then
           if when_true then
              put_trace( "when condition is true" );
           else
              put_trace( "when condition is false" );
           end if;
        end if;
     else
        when_true := false;
     end if;
  else
     when_true := false;
  end if;
end ParseWhenClause;


-----------------------------------------------------------------------------
--  PARSE RAISE
--
-- Raise an exception outside of an exception block
-- Syntax: raise [when...] | raise e [with s] [when...]
-- TODO: inside a block
-----------------------------------------------------------------------------

procedure ParseRaise( has_when : out boolean ) is
  id : identifier;
  with_text : unbounded_string;
  withTextType : identifier;
  mustRaise : boolean := true;
  atSemicolon : aScannerState;
begin
  expect( raise_t );
  has_when := false;

  -- re-raise?  restore the exception occurrence and announce an error
  -- It is only valid in an exception handler (as flagged in the block).

  if token = symbol_t and identifiers( token ).value.all = to_unbounded_string( ";" ) then
     -- exceptions only exist at run-time
     if not syntax_check then
        if not inExceptionHandler then
           err( "re-raise is not in an exception handler" );
        else
           getBlockException( err_exception, err_message, last_status );
           -- Be careful to fix svalue pointer
           err_exception.value := err_exception.svalue'access;
        end if;
     end if;
     if token = when_t or token = if_t then
        ParseWhenClause( mustRaise );
     elsif syntax_check then
        -- if the raise has no when clause, check for unreachable statements
        -- but we have to read past the semicolon.
         markScanner( atSemicolon );
         getNextToken; -- skip semicolon
         -- eof_t because a raise might be the last line in a simple script
         if token /= end_t and token /= exception_t and token /= when_t and token /= else_t and token /= elsif_t and token /= eof_t then
           err( "the raise makes this unreachable code" );
         end if;
         resumeScanning( atSemicolon ); -- restore original position
     end if;
     if isExecutingCommand then
        if mustRaise then
           error_found := true;
        end if;
     end if;
  else

     -- Normal raise of an explicit exception

     if identifiers( token ).class /= exceptionClass and identifiers( token ).kind /= new_t then
        err( optional_yellow( to_string( identifiers( token ).name ) ) & " is a " & getIdentifierClassImage( identifiers( token ).class ) & " not an exception" );
     else
        ParseIdentifier( id );
        if token = with_t then
           if onlyAda95 then
              err( "with not allowed with " & optional_yellow( "pragma ada_95" ) );
           end if;
           expect( with_t );
           ParseExpression( with_text, withTextType );
           if type_checks_done or else uniTypesOK( withTextType, uni_string_t ) then
              null;
           end if;
           if token = use_t then
              err( optional_yellow( "use" ) & " may only be used in exception declaration" );
           end if;
        elsif token /= when_t and token /= symbol_t and identifiers( token ).value.all /= ";" then
           err( "when, with or ';' expected" );
        end if;
        if token = when_t then
           ParseWhenClause( mustRaise );
           has_when := true;
        end if;
     end if;
     if isExecutingCommand then
        if mustRaise then
           -- if no message, check for and use the default message
           if length( with_text ) = 0 then
              if length( identifiers( id ).value.all ) > 1 then
                 with_text := unbounded_slice( identifiers( id ).value.all, 2, length( identifiers( id ).value.all ) );
              end if;
           end if;
           -- Record the exception id
           -- Pull the declaration out of the symbol table.  Mark as not deleted.
           -- Redirect storage pointer to point to local storage, not to the
           -- symbol table svalue which could get disappear when a block is
           -- deallocated.
           err_exception := identifiers( id );
           err_exception.deleted := false;
           err_exception.value := err_exception.svalue'access;
           if length( with_text ) > 0 then
              raise_exception( "raised " &
                   optional_yellow( to_string( identifiers( id ).name ) ) &
                   ": " &
                   to_string( with_text )
              );
           else
              raise_exception( "raised " &
                   optional_yellow( to_string( identifiers( id ).name ) )
              );
           end if;
           -- set the exit status
           last_status := character'pos( element( identifiers( id ).value.all, 1 ) );
        end if;
     end if;
  end if;
end ParseRaise;


-----------------------------------------------------------------------------
--  SKIP BLOCK
--
-- Read to the end of a block of code, executing nothing.
-- Stop reading when termid1, termid2 or a common end of block keyword is
-- seen.
------------------------------------------------------------------------------

procedure SkipBlock( termid1, termid2 : identifier := keyword_t ) is
  old_error : boolean;
  old_skipping : boolean;
begin
  if token = end_t or token = eof_t or token = exception_t or token = termid1 or token = termid2 then
     err( "missing statement or command" );
  end if;
  if syntax_check then               -- if we're checking syntax
     ParseBlock( termid1, termid2 ); -- must process the block to look
     return;                         -- for syntax errors
  end if;
  --old_error := error_found;          -- save error code
  --error_found := true;               -- skip by setting error flag
  old_error := syntax_check;
  old_skipping := skipping_block;
  syntax_check := true;
  skipping_block := true;
  -- if an error happens in the block, we were skipping it anyway...
  while token /= end_t and token /= eof_t and token /= exception_t and token /= termid1 and token /= termid2 loop
      ParseBlockExecutablePartStatement; -- step through context
  end loop;
  --error_found := old_error;          -- ignore any error while skipping
  syntax_check := old_error;
  skipping_block := old_skipping;
end SkipBlock;


-----------------------------------------------------------------------------
--  PARSE BLOCK
--
-- execute a block of code
-- Syntax: block = "general-stmt [general-stmt...] termid1 | termid2"
-----------------------------------------------------------------------------

procedure ParseBlock( termid1, termid2 : identifier := keyword_t ) is
begin
  if token = end_t or token = eof_t or token = exception_t or token = termid1 or token = termid2 then
     err( "missing statement or command" );
  end if;
  while token /= end_t and token /= eof_t and token /= exception_t and token /= termid1 and token /= termid2 loop
      ParseBlockExecutablePartStatement;
  end loop;
end ParseBlock;


-----------------------------------------------------------------------------
--  PARSE BLOCK EXECUTABLE PART STATEMENT
--
-- Handle the executable part of a block.
-- I was asked to allow declarations by a user.  I was going to allow this
-- when pragma ada_95 is not used but thought it better to make it a
-- suppress until I reviewed impacts on loops and conditional statements.
-----------------------------------------------------------------------------

procedure ParseBlockExecutablePartStatement is
begin
   -- Probably a more efficient way to do this than checking
   -- every statement.
   if onlyAda95 or restriction_no_declarations then
     ParseExecutableStatement;
   else
     ParseGeneralStatement;
   end if;
end ParseBlockExecutablePartStatement;


-----------------------------------------------------------------------------
--  PARSE EXCEPTION BLOCK
--
-- Handle an exception block
-----------------------------------------------------------------------------

--procedure ParseExceptionBlock( termid1, termid2 : identifier := keyword_t ) is
procedure ParseExceptionBlock is
  -- Same as ParseBlock except raise is permitted
begin
  if token = end_t or token = eof_t or token = when_t then
     err( "missing statement or command" );
  end if;
  while token /= end_t and token /= eof_t and token /= exception_t and token /= when_t  loop
     ParseBlockExecutablePartStatement;
  end loop;
  if token = exception_t then
     err( "already in an exception handler" );
  end if;
end ParseExceptionBlock;


-----------------------------------------------------------------------------
--  SKIP EXCEPTION BLOCK
--
-- Same as SkipBlock except raise is permitted
-----------------------------------------------------------------------------

procedure SkipExceptionBlock is
  pragma warnings( off );
  null_declaration : declaration;
  pragma warnings( on );
  old_error : boolean;
  old_skipping : boolean;
begin
  if token = end_t or token = eof_t or token = when_t then
     err( "missing statement or command" );
  end if;
  if syntax_check then               -- if we're checking syntax
     ParseExceptionBlock;
     return;                         -- for syntax errors
  end if;
  old_error := syntax_check;
  old_skipping := skipping_block;
  syntax_check := true;
  skipping_block := true;
  -- if an error happens in the block, we were skipping it anyway...
  while token /= end_t and token /= eof_t and token /= exception_t and token /= when_t loop
     ParseBlockExecutablePartStatement; -- skip content
  end loop;
  syntax_check := old_error;
  skipping_block := old_skipping;
end SkipExceptionBlock;


-----------------------------------------------------------------------------
--  PARSE EXCEPTION HANDLER
--
-- syntax: exception when others => (block)
-- errorOnEntry - true if there was already an error when the block
-- this handler was attached to was started (thus, this handler does
-- not apply)
-----------------------------------------------------------------------------

procedure ParseExceptionHandler( errorOnEntry : boolean ) is
  found_exception     : boolean := false;
  formal_exception_id : identifier;
  handling_exceptions : boolean;
  handled_exception   : boolean := false;

  occurrence_exception : declaration;
  occurrence_message   : unbounded_string;
  occurrence_status    : aStatusCode;
  occurrence_full      : unbounded_string;
begin
  handling_exceptions := (error_found and not done and not syntax_check and not errorOnEntry);

  -- for trace purposes, turn off errors as soon as possible.  Also, save
  -- the information about the exception for re-raising.

  if handling_exceptions then
     if trace then
        put_trace( "exception handler running" );
     end if;
     error_found := false;
     -- save the exception id
     occurrence_exception := err_exception;
     -- because value is a pointer now we must be careful with the value
     occurrence_exception.value := occurrence_exception.svalue'unchecked_access;
     -- TODO: an exception may be progated out of the declaration scope, leaving
-- the position in the symbol table undefined
     occurrence_message := err_message;
     occurrence_status := last_status;
     occurrence_full := fullErrorMessage;
     startExceptionHandler;
  end if;

  -- parse the exception block and see if there's a matching case for this
  -- exception.  If there's an others clause, it matches if nothing else did

  expect( exception_t );
  loop
    expect( when_t );
    -- if we have a case that matches, match will not be eof_t
    if token /= others_t then
       if identifiers( token ).class = exceptionClass then
          ParseIdentifier( formal_exception_id );
          -- TODO: a propogated exception that propogates but a new exception overlooks it.  The
          -- exception name must be unique
          found_exception := identifiers( formal_exception_id ).name = occurrence_exception.name;
          if found_exception then
             if trace then
                put_trace( "exception handler applying case " & to_string( identifiers( formal_exception_id ).name ) );
             end if;
          end if;
       else
          err( optional_yellow( to_string( identifiers( token ).name ) ) & " is a " & getIdentifierClassImage( identifiers( token ).class ) & " not an exception" );
          exit;
       end if;
    else
       -- if we haven't found a case that matches, others always matches
       expect( others_t );
       if handling_exceptions then
          if not handled_exception then
             found_exception := true;
             if trace then
                put_trace( "exception handler applying default case for '" &
                   to_string( occurrence_exception.name ) & "'" );
             end if;
          end if;
       end if;
    end if;
    expect( symbol_t, "=>" );
    -- this case matches and we're handling exceptions? then handle it.
    -- otherwise, skip the exception handler block
    if handling_exceptions and not error_found then
       if found_exception then
          ParseExceptionBlock;
          found_exception := false;                          -- clear found flag
          handled_exception := true;                 -- we handled the exception
          if error_found and err_exception.deleted then     -- non-except error?
             if trace then
                put_trace( "an error occurred while handling the exception" );
             end if;
          elsif not error_found then                             -- no re-raise?
             err_exception.deleted := true;                      -- mark handled
             last_status := 0;                          -- and clear status code
                                                      -- exception already clear
             err_message := null_unbounded_string;                  -- clear any
             fullErrorMessage := null_unbounded_string;              -- messages
             if trace then
                put_trace( "cleared exception occurrence" );
             end if;
          end if;
       else
          SkipExceptionBlock;
       end if;
    else
       SkipExceptionBlock;
    end if;
    exit when token = eof_t or token = end_t;
  end loop;
  -- no handler found?  restore the error flag
  -- if another error happened in the handler, it takes precidence
  if handling_exceptions then                       -- not syntax check, etc.?
     if not handled_exception then                        -- no handler found?
        if error_found then                                 -- error occurred?
           null;                                       -- then can't propogate
        else                                                      -- otherwise
           if trace then                                            -- explain
              put_trace( "no appropriate handler was found" );
           end if;
           -- propogate the exception
           -- restore the exception unless a new exception or an error occurred.
           -- do the messages just to be safe.
           err_exception := occurrence_exception;
           -- because value is a pointer now we must be careful with the value
           copyValue( err_exception, occurrence_exception );
           err_message := occurrence_message;
           last_status := occurrence_status;
           fullErrorMessage := occurrence_full;
           error_found := true;
        end if;
     end if;
  end if;
end ParseExceptionHandler;


-----------------------------------------------------------------------------
--  PARSE EXCEPTION BLOCK
--
-- Handle a declare...begin..end block.
-----------------------------------------------------------------------------

procedure ParseDeclareBlock is
  old_error_found : constant boolean := error_found;
begin
  pushBlock( newScope => true, newName => "declare block" );
  expect( declare_t );
  ParseDeclarations;
  expect( begin_t );
  ParseBlock;
  if token = exception_t then
     ParseExceptionHandler( old_error_found );
  end if;
  expect( end_t );
  pullBlock;
end ParseDeclareBlock;


-----------------------------------------------------------------------------
--  PARSE BEGIN BLOCK
--
-- Handle a begin..end block.
-----------------------------------------------------------------------------

procedure ParseBeginBlock is
  old_error_found : constant boolean := error_found;
begin
  pushBlock( newScope => true, newName => "begin block" );
  expect( begin_t );
  ParseBlock;
  if token = exception_t then
     ParseExceptionHandler( old_error_found );
  end if;
  expect( end_t );
  pullBlock;
end ParseBeginBlock;


------------------------------------------------------------------------------
--  PARSE FORMAL PARAMETER PROPERTIES
--
-- Interpret a formal subprogram parameter but do not declare it.  Instead,
-- return its properties.
-- TODO: this approach won't work if we do a list of parameters.
------------------------------------------------------------------------------

procedure ParseFormalParameterProperties(
  formalParamId : out identifier;
  passingMode : out aParameterPassingMode;
  paramKind : out identifier;
  abstractKind : out identifier;
  subId : identifier;
  isFunction : boolean ) is
begin
  abstractKind := eof_t;

  -- Parse the new parameter name

  ParseNewIdentifier( formalParamId );
  expect( symbol_t, ":" );

  -- Parse the access mode

  if token = out_t then
     expect( out_t );
     passingMode := out_mode;
  elsif token = in_t then
     -- TODO: deny in (just use default)?  Or require in?
     passingMode := in_mode;
     expect( in_t );
     if token = out_t then
        expect( out_t );
        passingMode := in_out_mode;
        -- DEBUG
        -- err( "in out parameters not yet supported" );
     end if;
  elsif token = access_t then
     err( "access parameters not yet supported" );
  else
     passingMode := in_mode;
  end if;

  -- Check for anonymous array

  if token = array_t then
     err( "anonymous array parameters not yet supported" );
  end if;

  -- Parse the name of the type
  --
  -- If it's an abstract type, record it because the subproram must be
  -- abstract also.

  ParseIdentifier( paramKind );
  if identifiers( paramKind ).usage = abstractUsage then
     if abstractKind = eof_t then
        abstractKind := paramKind;
     end if;
  end if;

  ----------------------------------------------------------------------------
  -- Perform checks on the parameter
  ----------------------------------------------------------------------------

  -- Check type
  --
  -- in mode for aggregates not yet written.

   if passingMode = out_mode then
      if isFunction then
         err( "out mode parameters not allowed in functions" );
     end if;
   end if;
   if passingMode = in_out_mode then
      if isFunction and onlyAda95 then
         err( "in out mode parameters not allowed in functions with " &
             optional_yellow( "pragma ada_95" ) );
      end if;
   end if;
  if passingMode = in_mode then
     if identifiers( getBaseType( paramKind ) ).list then
         err( "array parameters not yet supported" );
     elsif identifiers( getBaseType( paramKind ) ).kind = root_record_t then
        err( "records not yet supported" );
     end if;
  end if;
  --elsif identifiers( getBaseType( type_token ) ).kind = root_record_t then
  if getBaseType( paramKind ) = command_t then
     err( "commands not yet supported" );
  end if;

  -- Check for default value

  if token = symbol_t and identifiers( token ).value.all = ":=" then
     err( "default values are not yet supported" );
  end if;

end ParseFormalParameterProperties;


-----------------------------------------------------------------------------
--  PARSE FORMAL PARAMETER
--
-----------------------------------------------------------------------------

procedure ParseFormalParameter(
   formal_param_id : out identifier; -- the parameter's id
   proc_id : identifier;             -- the procedure (or function) id
   param_no : in out integer;        -- the parameter number
   abstract_parameter : in out identifier; -- any abstract param (or eof)
   is_function : boolean := false ) is   -- is this a function parameter?
   -- TODO: declare it won't work
-- Syntax: name : mode type
-- Parameters are implemented using the same scheme as records
   b : boolean;
   type_token   : identifier;
   passingMode  : aParameterPassingMode;
begin
  param_no := param_no + 1;

  ParseFormalParameterProperties(
    formalParamId => formal_param_id,
    passingMode => passingMode,
    paramKind => type_token,
    abstractKind => abstract_parameter,
    subId => proc_id,
    isFunction => is_function );

  -- Create the parameter, associating it to the procedure/function

  if syntax_check then
     identifiers( formal_param_id ).wasReferenced := true;
     --identifiers( formal_param_id ).referencedByThread := getThreadName;
     identifiers( type_token ).wasApplied := true;
  end if;

  updateFormalParameter( formal_param_id, type_token, proc_id, param_no,
    passingMode );

  -- Blow away on error

  if error_found then
     b := deleteIdent( formal_param_id );
  end if;
end ParseFormalParameter;


-----------------------------------------------------------------------------
--  PARSE FORMAL PARAMETERS
--
-----------------------------------------------------------------------------

procedure ParseFormalParameters( proc_id : identifier; param_no : in out integer; abstract_parameter : in out identifier; is_function : boolean := false ) is
-- Syntax: parameter-declaration [; parameter-declaration]
-- Fields are implemented using records
   formal_param_id : identifier;
begin
  loop
    ParseFormalParameter( formal_param_id, proc_id, param_no, abstract_parameter, is_function );
    if error_found or token = eof_t or (token = symbol_t and identifiers( token ).value.all = ")" ) then
       exit;
    end if;
    expectDeclarationSemicolon( context => formal_param_id );
    -- TODO: this will show the formal param as sub.param, which might be
    -- confusing to users.
    -- the symbol table will overflow before field_no does
  end loop;
end ParseFormalParameters;


-----------------------------------------------------------------------------
--  PARSE FUNCTION RETURN PART PROPERTIES
--
-----------------------------------------------------------------------------

procedure ParseFunctionReturnPartProperties(
  resultKind : out identifier;
  abstractKind : out identifier;
  funcId : identifier) is
  -- Parse a function's return part but do not declare it.  This is necessary
  -- for forward declarations.  Return the properties to the caller who can
  -- then declare it.
begin
  abstractKind := eof_t;

  -- Consume the return

  expect( return_t );

  -- The name of the type

  ParseIdentifier( resultKind );
  if syntax_check then
     identifiers( resultKind ).wasApplied := true; -- type was used
     if identifiers( resultKind ).usage = abstractUsage then
        abstractKind := resultKind;
     end if;
  end if;

  -- Check type

  if identifiers( getBaseType( resultKind ) ).list then
     err( "array parameters not yet supported" );
  elsif identifiers( getBaseType( resultKind ) ).kind = root_record_t then
     err( "records not yet supported" );
  elsif getBaseType( resultKind ) = command_t then
     err( "commands not yet supported" );
  end if;
end ParseFunctionReturnPartProperties;

procedure ParseFunctionReturnPart( funcId : identifier; abstractReturn : in out identifier ) is
   -- Parse a function's return part.
   -- Syntax: return type
   resultId : identifier;
   b : boolean;
   resultKind    : identifier;
begin

  -- Parse the return part.  Get the properties.

  ParseFunctionReturnPartProperties(
    resultKind => resultKind,
    abstractKind => abstractReturn,
    funcId => funcId
  );

  identifiers( funcId ).kind := resultKind;

  -- Create the return result.  This is a hidden variable with a
  -- special name and parameter number zero.  Pretend it is read
  -- and was a factor (so as not to be treated as limtied) but
  -- don't mark it as written to as we want to detect when no
  -- return value is assigned.

  declareIdent( resultId, return_value_str, resultKind, varClass );
  if syntax_check then
     identifiers( resultId ).wasReferenced := true;
     -- This does not seem to work??  I had to hard-code the
     -- return_value_str in the scanner tests.
     identifiers( resultId ).wasFactor := true;
     --identifiers( formal_param_id ).referencedByThread := getThreadName;
  end if;
  updateFormalParameter( resultId, resultKind, funcId, 0, none );

  -- Blow away the result variable on an error

  if error_found then
     b := deleteIdent( resultId );
  end if;
end ParseFunctionReturnPart;


-----------------------------------------------------------------------------
--  DECLARE ACTUAL PARAMETERS
--
-- This function declare fake actual parameters for parsing the formal
-- definition of a procedure or function.  It doesn't create the parameters
-- used when a procedure or function is called (that's ParseActualParameters).
-----------------------------------------------------------------------------

procedure DeclareActualParameters( proc_id : identifier ) is
  actual_param_t : identifier;
  param_no : natural;
  startAt : identifier;
  recordBaseTypeId : identifier;
begin
  if not error_found then
     -- unlike arrays, user-defined functions and procedures do not have
     -- a total number of parameters stored in their value field

     -- functions have an extra, hidden actual parameter for the function
     -- result (parameter zero).

     if identifiers( proc_id ).class = userFuncClass then
         declareReturnResult(
            actual_param_t,
            proc_id );
         if syntax_check then
            identifiers( actual_param_t ).wasReferenced := true;
            --identifiers( actual_param_t ).referencedByThread := getThreadName;
            identifiers(
              identifiers( actual_param_t ).kind
              ).wasApplied := true; -- type was used
         end if;
     end if;

     startAt := identifiers_top-1;

     -- this declares the actual parameters but does not set the value
     -- or implement the passing mode.

     param_no := 1;
     loop
         declareUsableFormalParameter(
            actual_param_t,
            proc_id,
            param_no,
            null_unbounded_string,
            startAt );
         exit when actual_param_t = eof_t;
         -- because parameters cannot be limited, we pretend that they
         -- were used in an expression.
         identifiers( actual_param_t ).wasFactor := true;
         -- if a record, we need fields
         recordBaseTypeId := getBaseType( identifiers( actual_param_t ).kind );
         if identifiers( recordBaseTypeId ).kind = root_record_t then  -- record type?
            -- if identifiers( actual_param_t ).kind = root_record_t then  -- record type?
            declareRecordFields( actual_param_t, recordBaseTypeId );
         end if;
         -- search for the next one starting one later
         param_no := param_no + 1;
     end loop;
  end if;
end DeclareActualParameters;


-----------------------------------------------------------------------------
--  VERIFY SUBPROGRAM PARAMETERS
--
-- Given an incomplete specification, confirm that the
-- parameters given and the same as the earlier specification.
-----------------------------------------------------------------------------

procedure VerifySubprogramParameters( specId : identifier; is_function : boolean := false ) is
  specParamId : identifier;
  specParameterNumber : natural;
  specParamName : unbounded_string;

  bodyParamId : identifier;
  bodyPassingMode :aParameterPassingMode;
  bodyParamKind : identifier;
  bodyAbstractKind : identifier;

  b : boolean;
begin
  -- put_line( standard_error, "Verifying " & to_string( identifiers( proc_id ).name ) ); -- DEBUG
  -- put_token;
  specParamId := specId + 1;
  specParameterNumber := 1;

  -- if there are no parameters and this is a function, you will run into the function's return
  -- value, which does not count as a parameter...abort the parameter search if it exists.
  if identifiers( specParamId ).name = return_value_str then
     specParamId := identifiers_top; -- abort search
  end if;

  -- TODO: probably less brute-force ways to do this.
  loop
     while specParamId < identifiers_top loop
        if identifiers( specParamId ).field_of = specId then
           if integer'value( to_string( identifiers( specParamId ).value.all )) = specParameterNumber then
              exit;
           end if;
        end if;
        specParamId := identifier( integer( specParamId ) + 1 );
      end loop;

    exit when specParamId = identifiers_top;

    specParamName := identifiers( specParamId ).name;
    specParamName := delete( specParamName, 1, index( specParamName, "." ) );

    -- Now compare against what the new parameters
    --
    -- A procedure can have no parameters

    if token = is_t then
       err( "no parameters found but earlier specification had parameters (at " &
            to_string( identifiers( specId ).specFile) & ":" &
            identifiers( specId ).specAt'img & ")");
       exit  ;

    -- A function may have no parameters, just a return

    elsif is_function and then token = return_t then
       err( "no parameters found but earlier specification had parameters (at " &
            to_string( identifiers( specId ).specFile) & ":" &
            identifiers( specId ).specAt'img & ")");
       exit;

    -- Too few parameters, ran into closing parenthesis early

    elsif token = symbol_t and identifiers( token ).value.all = ")" then
       err( "missing parameter " & optional_yellow( to_string(specParamName) ) &
            " from earlier specification (at " &
            to_string( identifiers( specId ).specFile) & ":" &
            identifiers( specId ).specAt'img & ")");
       exit;
    else
       -- Parse the next parameter and compare with specification
       --
       -- Get the properties of the next parameter

       ParseFormalParameterProperties(
          formalParamId => bodyParamId,
          passingMode => bodyPassingMode,
          paramKind => bodyParamKind,
          abstractKind => bodyAbstractKind,
          subId => specId,
          isFunction => false
          );

       -- Parameter name: specification vs implementation

       if specParamName /= identifiers( bodyParamId ).name then
          err("parameter name " &
              optional_yellow( to_string( identifiers( bodyParamId ).name )) &
              " was " & optional_yellow( to_string( specParamName )) &
              " in the earlier specification (at " &
              to_string( identifiers( specId ).specFile) & ":" &
              identifiers( specId ).specAt'img & ")");
       end if;

       -- Parameter type: specification vs implementation

       if identifiers( specParamId ).kind /= bodyParamKind then
          err("parameter type " &
             optional_yellow( to_string( identifiers( bodyParamKind ).name ) ) &
             " was " & optional_yellow( to_string( identifiers( identifiers( specParamId ).kind ).name )) &
             " in the earlier specification (at " &
             to_string( identifiers( specId ).specFile) & ":" &
             identifiers( specId ).specAt'img & ")");
       end if;

       -- TODO: Check the qualifier (currently not possible)

       -- Parameter passing mode: specification vs implementation

       if identifiers( specParamId ).passingMode /= bodyPassingMode then
          err("parameter mode " &
              optional_yellow( to_string( bodyPassingMode ) ) &
              " was " & optional_yellow( to_string( identifiers( specParamId ).passingMode ) ) &
              " in the earlier specification (at " &
              to_string( identifiers( specId ).specFile) & ":" &
              identifiers( specId ).specAt'img & ")");
       end if;

       -- Discard the implementation parameter

       b := deleteIdent( bodyParamId );
     end if;

     -- consume a trailing semi-colon

     if token = symbol_t and identifiers( token ).value.all = ";" then
        expectDeclarationSemicolon( context => specParamId );
     end if;

    specParamId := identifier( integer( specParamId ) + 1 );
    specParameterNumber := specParameterNumber + 1;
  end loop;
end VerifySubprogramParameters;


-----------------------------------------------------------------------------
--  VERIFY SUBPROGRAM RETURN PART
--
-- Given an incomplete specification of a function, verify the
-- return part of the implementation matches it.
-----------------------------------------------------------------------------

procedure VerifySubprogramReturnPart( func_id : identifier ) is
  resultKind     : identifier;
  abstractReturn : identifier;
begin
  ParseFunctionReturnPartProperties(
      resultKind => resultKind,
      abstractKind => abstractReturn,
      funcId => func_id
   );
  if resultKind /= identifiers( func_id ).kind then
     err("function return type " &
         optional_yellow( to_string( identifiers( resultKind ).name ) ) &
         " was " & optional_yellow( to_string( identifiers( identifiers( func_id ).kind ).name )) &
                " in the earlier specification (at " &
                to_string( identifiers( func_id ).specFile) & ":" &
                identifiers( func_id ).specAt'img & ")");
  end if;
end VerifySubprogramReturnPart;


-----------------------------------------------------------------------------
--  PARSE SEPARATE PROC HEADER
--
-- Syntax: separate( parent ); procedure p [(param1...)] is
-- TODO: forward declaration handling has been written so minimal parameter
-- checking in the header should be corrected here.
-----------------------------------------------------------------------------

procedure ParseSeparateProcHeader( proc_id : identifier; procStart : out natural ) is
  separate_proc_id : identifier;
  parent_id        : identifier;
  b : boolean;
  pu : unbounded_string;
  i : integer;
  ch : character;
begin
   -- getFullParentUnitName( pu );
   -- separate
   expectDeclarationSemicolon( context => proc_id );
   expect( separate_t );
   expect( symbol_t, "(");
   ParseIdentifier( parent_id );
   pu := identifiers( parent_id ).name;
   -- NOTE: the identifier token returned has the prefix stripped!  This needs
   -- to be fixed so I cannot check to see if it was full path before stripping.
   i := length( pu );
   while i > 0 loop
     ch := element( pu, i );
     if ch = '.' then
        delete( pu, 1, i );
        exit;
     end if;
     i := i - 1;
   end loop;
   if identifiers( parent_id ).class /= userProcClass and identifiers( parent_id ).class /= userFuncClass and identifiers( parent_id ).class /= mainProgramClass then
         err( "parent unit should be a subprogram" );
   elsif identifiers( parent_id ).name /= pu then
         err( "expected parent unit " & optional_yellow( to_string( pu ) ) );
   end if;
   expect( symbol_t, ")");
   expectStatementSemicolon( separate_t );
   -- separate's procedure header
   procStart := firstPos;
   expect( procedure_t );
   -- could do ParseIdentifier since it should exist but want a more meaningful
   -- message error for a mismatch
   ParseProcedureIdentifier( separate_proc_id );
   if identifiers( separate_proc_id ).value.all = identifiers( proc_id ).value.all then
      -- names match?  OK, discard.  proc is stored under original ident
      b := deleteIdent( separate_proc_id );
   else
      err( optional_yellow( to_string( identifiers( separate_proc_id ).name ) ) & " is different from parent file's " & optional_yellow( to_string( identifiers( proc_id  ).name ) ) );
   end if;
   -- check for forward declarations not yet written so minimal checking here
   -- flush this out if i have time to walk the identifiers list if available
   if token = symbol_t and identifiers( token ).value.all = "(" then
      expect( symbol_t, "(" );
      while token /= symbol_t and identifiers( token ).value.all /= ")" and token /= eof_t loop
         getNextToken;
      end loop;
      expect( symbol_t, ")" );
   end if;
   expect( is_t );
end ParseSeparateProcHeader;


-----------------------------------------------------------------------------
--  PARSE PROCEDURE BLOCK
--
-- Syntax: procedure [abstract] p [(param1...)] OR procedure [abstract] p [(param1...)] is block
-- end p;
-- Handle procedure declarations, including forward declarations.
-- Note: DoUserDefinedProcedure executes a user-defined procedure created by
-- this routine.
-----------------------------------------------------------------------------

procedure ParseProcedureBlock is
  proc_id   : identifier;
  procStart : natural;
  procEnd   : natural;
  no_params   : integer := 0;  -- TODO: delete this
  old_error_found : constant boolean := error_found;
  abstract_parameter : identifier := eof_t;
  declarationFile : unbounded_string;
  declarationLine : natural;
begin
  procStart := firstPos;
  declarationFile := getSourceFileName;
  declarationLine := getLineNo;

  expect( procedure_t );
  ParseProcedureIdentifier( proc_id );

  -- Whether forward or not, handle the parameters.  If there's a
  -- forward specification, verify the new parameters match the old
  -- ones.  Otherwise, parse the parameters as normal.

  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     if identifiers( proc_id ).specAt /= noSpec then
        VerifySubprogramParameters( proc_id );
        if token /= symbol_t and identifiers( token ).value.all /= ")" then
           err( "too many parameters compared to earlier specification (at " &
                to_string( identifiers( proc_id ).specFile) & ":" &
                identifiers( proc_id ).specAt'img & ")");
        end if;
     else
     --no_params := 0;
        ParseFormalParameters( proc_id, no_params, abstract_parameter );
        --identifiers( proc_id ).value := to_unbounded_string( no_params );
     end if;
     expect( symbol_t, ")" );
  elsif identifiers( proc_id ).specAt /= noSpec then
     VerifySubprogramParameters( proc_id );
  end if;

  -- Is it a forward declaration?

  if token = symbol_t and identifiers( token ).value.all = ";" then
     if identifiers( proc_id ).specAt /= noSpec then
        err( "already declared specification for " & optional_yellow( to_string( identifiers( proc_id ).name ) ) & " (at " &
                to_string( identifiers( proc_id ).specFile) & ":" &
                identifiers( proc_id ).specAt'img & ")");
     end if;
     identifiers( proc_id ).class := userProcClass;
     identifiers( proc_id ).kind := procedure_t;
     identifiers( proc_id ).specFile := declarationFile;
     identifiers( proc_id ).specAt := declarationLine;
  else
     identifiers( proc_id ).class := userProcClass;
     identifiers( proc_id ).kind := procedure_t;
     identifiers( proc_id ).specFile := null_unbounded_string;
     identifiers( proc_id ).specAt := noSpec;
     pushBlock( newScope => true,
       newName => to_string (identifiers( proc_id ).name ) );
     DeclareActualParameters( proc_id );
     expect( is_t );
     if token = null_t then                               -- null abstract
        expect( null_t );
        expect( abstract_t );
        identifiers( proc_id ).usage := abstractUsage;
        if syntax_check then
           identifiers( proc_id ).wasReferenced := true;
           --identifiers( proc_id ).referencedByThread := getThreadName;
        end if;
        pullBlock;
     else
        if token = separate_t then
           if rshOpt then
              err( "subunits are not allowed in a " & optional_yellow( "restricted shell" ) );
           end if;
           expect( separate_t );
           -- "is separate" is effectively an include
           -- only insert include on a syntax check
           if syntax_check then
              insertInclude( identifiers( proc_id ).name & ".sp" );
           end if;
           ParseSeparateProcHeader( proc_id, procStart );
        elsif token = abstract_t then
           expect( abstract_t );
           identifiers( proc_id ).usage := abstractUsage;
           if syntax_check then
              identifiers( proc_id ).wasReferenced := true;
              --identifiers( proc_id ).referencedByThread := getThreadName;
           end if;
        elsif abstract_parameter /= eof_t then
           err( "procedure must be abstract because parameter type " &
              optional_yellow( to_string( identifiers( abstract_parameter ).name ) ) &
              " is abstract" );
        end if;
        ParseDeclarations;
        expect( begin_t );
        skipBlock;                                       -- never execute now
        if token = exception_t then
           ParseExceptionHandler( old_error_found );
        end if;
        pullBlock;
        expect( end_t );
        expect( proc_id );
        procEnd := lastPos+1; -- include EOL ASCII.NUL
        identifiers( proc_id ).value.all := to_unbounded_string( copyByteCodeLines( procStart, procEnd ) );
        -- fake initial indent of 1 for byte code (SOH)
        -- we don't know what the initial indent is (if any) since it may
        -- not be the first token on the line (though it usually is)
     end if;
  end if;
  expectDeclarationSemicolon( context => proc_id );
end ParseProcedureBlock;


-----------------------------------------------------------------------------
--  PARSE ACTUAL PARAMETERS
--
-- Syntax: (param : declaration [; declaration ... ] )
--
-- declareparams is true, then the parameters will be created (e.g. this is
--   false on a syntax check)
-- is_function is true when this is called to setup a function's parameters
--   (false is a procedure)
--
-- For a call proc( x, y, z ), this procedure parses x, y, z and
-- creates the equivalent parameters for running proc.  There are three
-- kind of parameters involved:
--
-- * x is an actual parameter, the parameter passed into proc
-- * proc.p1 is a formal parameter, the proc definition parameter
-- * p1 is the usuable identifier created from the template of proc.p1.
--   p1's value is filled in (on in-mode) or p1 renames x (in an out or
--   in out mode)
-----------------------------------------------------------------------------

procedure ParseActualParameters( proc_id : identifier;
  declareParams : boolean := true ) is

  ----------------------------------------------------------------------------
  -- Usable Parameter Helper Functions
  --
  -- Handles arrays, array elements and records.
  -- TODO: refactor to make this more general, place in world.adb?
  ----------------------------------------------------------------------------

  --  UPDATE RENAMED ARRAY ELEMENT PARAMETER
  --
  ----------------------------------------------------------------------------

  procedure UpdateRenamedArrayElementParameter( actualParamRef : renamingReference;
    usableParamId : identifier ) is
  begin
    if isExecutingCommand then                      -- no value change
       identifiers( usableParamId ).value :=
          identifiers( actualParamRef.id ).avalue(
          actualParamRef.index )'access;
    end if;
    exception when storage_error =>              -- prob freed mem
         err( gnat.source_info.source_location &
           ": internal error: storage_error exception raised" );
       when others =>
         err( gnat.source_info.source_location &
            ": internal error: exception raised" );
  end UpdateRenamedArrayElementParameter;

  --  UPDATE RENAMED FULL ARRAY PARAMETER
  --
  -- For a full array, mark it as an array and fix the value pointer
  ----------------------------------------------------------------------------

  procedure UpdateRenamedFullArrayParameter( actualParamRef : renamingReference;
    usableParamId : identifier ) is
  begin
    identifiers( usableParamId ).list := true;
    FixRenamedArray( actualParamRef, usableParamId );
  end UpdateRenamedFullArrayParameter;

  --  UPDATE RENAMED RECORD PARAMETER
  --
  -- Records are a complex case.  They must match the formal parmaeters but
  -- the values rename the actual parameters.
  ----------------------------------------------------------------------------

procedure UpdateRenamedRecordParameter( actualRecordRef : renamingReference;
   formalRecordParamId, usableRecordParamId : identifier ) is
   numFields : natural;
   fieldName     : unbounded_string;
   usableFieldId : identifier;
   dotPos        : natural;
   recordTypeFieldId : identifier;
begin
     -- TODO: use this above with formal parameters?

  -- Always a risk of an exception thrown here
  begin
    numFields := natural( to_numeric( identifiers( identifiers(
        actualRecordRef.id ).kind ).value.all ) );
  exception when storage_error =>
    numFields := 0;
    err( gnat.source_info.source_location &
         "internal error: storage_error: unable to determine the number of fields" );
  when constraint_error =>
    numFields := 0;
    err( gnat.source_info.source_location &
         "internal error: constraint_error: unable to determine the number of fields" );
  end;

  recordTypeFieldId := identifiers( formalRecordParamId ).kind + 1;

  for fieldNumber in 1..numFields loop

      -- brutal search...we can do better than...
      --  for recordTypeFieldId in reverse keywords_top..identifiers_top-1 loop
      --
      -- As an optimization, the fields are likely located immediately after
      -- the record itself is defined.  Also assumes they are stored
      -- sequentially.  In the future, records will be stored differently.

      while recordTypeFieldId < identifiers_top loop
        if identifiers( recordTypeFieldId ).field_of = identifiers( formalRecordParamId ).kind then
           if integer'value( to_string( identifiers( recordTypeFieldId ).value.all )) = fieldNumber then
               exit;
            end if;
         end if;
         recordTypeFieldId := identifier( integer( recordTypeFieldId ) + 1 );
      end loop;

     -- no more identifiers means we didn't find it.
     if recordTypeFieldId = identifiers_top then
        err( gnat.source_info.source_location &
           "internal error: record field not found" );
        exit;
     end if;

    -- TODO: should this be the base record type?  subtypes may break it
    fieldName := identifiers( recordTypeFieldId ).name;
    dotPos := length( fieldName );
    while dotPos > 1 loop
       exit when element( fieldName, dotPos ) = '.';
       dotPos := dotPos - 1;
    end loop;
    fieldName := delete( fieldName, 1, dotPos );
    fieldName := identifiers( usableRecordParamId ).name & "." & fieldName;
--put_line( "field name = " & to_string( fieldName ) );
    declareIdent( usableFieldId, fieldName, identifiers(
        recordTypeFieldId ).kind, varClass );
    -- fields have not been marked as children of the parent
    -- record.  However, to make sure the record is used, it
    -- is convenient to track the field.
    identifiers( usableFieldId ).field_of := usableRecordParamId;
    -- at least, for now, don't worry if record fields are
    -- declared but not accessed.  We'll just check the
    -- main record identifier.
    if syntax_check then
       identifiers( usableFieldId ).wasReferenced := true;
       --identifiers( usableFieldId ).referencedByThread := getThreadName;
       identifiers(
         identifiers( recordTypeFieldId ).kind
         ).wasApplied := true;
     end if;
     recordTypeFieldId := identifier( integer( recordTypeFieldId ) + 1 );
  end loop; -- for

  -- We have to do another search for the actual parameter fields and
  -- link them to the usable parameter fields
  FixRenamedRecordFields( actualRecordRef, usableRecordParamId );

end updateRenamedRecordParameter;

------------------------------------------------------------------------------
-- Parameter Modes
--
-- Create usable parameters based on the parameter mode
------------------------------------------------------------------------------

  --  PARSE USABLE IN MODE PARAMETER
  --
  -- For an in mode parameter, expect an expression
  -- Create a constant containing the value.
  ----------------------------------------------------------------------------

procedure parseUsableInModeParameter( formalParamId : identifier; paramName : unbounded_string ) is
   expr_value : unbounded_string;
   expr_type : identifier;
   usableParamId : identifier;
   typesOK : boolean;
begin
  ParseExpression( expr_value, expr_type );
  if type_checks_done then
     typesOK := true;
  else
     typesOK := baseTypesOK( identifiers( formalParamId ).kind, expr_type );
  end if;
  if typesOK and then declareParams then
     declareIdent(
         usableParamId,
         to_string( paramName ),
         identifiers( formalParamId ).kind
     );
     -- For an in-mode parameter, downgrade full usage to constant
     -- limited, abstract already cannot be assigned to.
     if identifiers( usableParamId ).usage = fullUsage then
        identifiers( usableParamId ).usage := constantUsage;
     end if;
     -- Originally, made a constant but constant is now a usage qualifier
     --declareStandardConstant( usableParamId,
     --   to_string( paramName ),
     --   identifiers( formalParamId ).kind,
     --   to_string( expr_value ) );
     if isExecutingCommand then
        if identifiers( formalParamId ).kind /= expr_type then
           DoContracts( identifiers( formalParamId ).kind, expr_value );
        end if;
        identifiers( usableParamId ).value.all := expr_value;
        if trace then
           put_trace(
              to_string( identifiers( usableParamId ).name ) & " := " &
              to_string( expr_value ) );
        end if;
     end if;
  end if;
end parseUsableInModeParameter;

  --  PARSE USABLE IN OUT MODE PARAMETER
  --
  ----------------------------------------------------------------------------

procedure parseUsableInoutModeParameter( formalParamId : identifier; paramName : unbounded_string ) is
   actual_param_ref : renamingReference;
   usableParamId : identifier;
begin
  -- the reference is the actual parameter, the thing being passed...
  ParseRenamingReference(
    actual_param_ref,
    identifiers( formalParamId ).kind
  );
 --put_line("HERE2"); -- DEBUG
 --put_line( "actual" & to_string( identifiers( actual_param_ref.id ).name) ); -- DEBUG
 --put_line( "     -" & identifiers( actual_param_ref.id ).usage'img ); -- DEBUG
 --put_line( "formal" & to_string( identifiers( formalParamId ).name) ); -- DEBUG
 --put_line( "     -" & identifiers( formalParamId ).usage'img ); -- DEBUG
  -- parameters may not be constants or enumerated items
  if identifiers( actual_param_ref.id ).usage = limitedUsage and
     identifiers( formalParamId ).usage /= limitedUsage then
        identifiers( formalParamId ).wasWritten := true;
        err(
           contextNotes => "While looking at type usage",
           subject => actual_param_ref.id,
           subjectType => identifiers( actual_param_ref.id ).kind,
           reason => "is limited and limited is required for",
           obstructor => formalParamId,
           obstructorType => identifiers( formalParamId ).kind
        );
  elsif identifiers( actual_param_ref.id ).usage = constantUsage then
     err( "a constant cannot be used as an in out or out mode parameter" );
  elsif identifiers( actual_param_ref.id ).class = enumClass then
     -- TODO: I could probably get this to work for out parameters but
     -- not yet implemented
     err( "enumerated items cannot be used as an in out or out mode parameter" );
  end if;
  if not error_found then
     -- whether declaring the params or not, during a syntax check, the actual
     -- parameter must be marked as "written" to avoid an error over not being
     -- a constant
     if syntax_check then
        if identifiers( actual_param_ref.id ).field_of /= eof_t then
           identifiers( identifiers( actual_param_ref.id ).field_of ).wasWritten := true;
        else
           identifiers( actual_param_ref.id ).wasWritten := true;
           identifiers( actual_param_ref.id ).wasFactor := true;
        end if;
     else
        -- when running, mark that the actual parameter was written for
        -- side-effect prevention.
        --checkDoubleGlobalWrite( actual_param_ref.id );
        -- As a kludge, the line count hasn't advanced yet from the expression
        -- so add one, otherwise it won't recognize that the write happend
        -- after the expression started.
        identifiers( actual_param_ref.id ).writtenOn := perfStats.lineCnt+1;
     end if;
  end if;

  if declareParams then

     -- Run-time side-effects tracking and test
     -- Test for two or more "threads" writing to one unprotected variable

     checkDoubleThreadWrite( actual_param_ref.id );

       -- the actual parameter will be the canonical identifier for
       -- the renaming
       declareIdent(
          usableParamId,
          to_string( paramName ),
          identifiers( formalParamId ).kind
       );
        -- TODO: actually, an update not a declaration.  But can it be
        -- combined with declareIdent?
        declareRenaming( usableParamId, actual_param_ref ); -- basic renaming
        -- Usuable parameter was "written"
        if syntax_check and then not error_found then
           identifiers( usableParamId ).wasWritten := true;
        end if;
        -- An array?  make it happen.
        if identifiers( actual_param_ref.id).list then        -- array/element?
           if actual_param_ref.hasIndex then                  -- element?
              updateRenamedArrayElementParameter( actual_param_ref, usableParamId );
           else
              updateRenamedFullArrayParameter( actual_param_ref, usableParamId );
           end if;
        elsif identifiers( getBaseType( actual_param_ref.kind ) ).kind = root_record_t then  -- record type?
-- TODO: a function for this
           UpdateRenamedRecordParameter( actual_param_ref, formalParamId,
   usableParamId );
        end if;
  end if; -- declareParams
end parseUsableInoutModeParameter;


  --  PARSE USABLE OUT MODE PARAMETER
  --
  -- For an out mode parameter, expect an identifier.
  -- declare a variable as a renaming for the
  -- identifier.  Technically the value is
  -- undefined.
  --
  -- For now, it's treated the same as an in out mode parameter
  ----------------------------------------------------------------------------

procedure parseUsableOutModeParameter( formalParamId : identifier; paramName : unbounded_string ) is
begin
   parseUsableInoutModeParameter( formalParamId, paramName );
end parseUsableOutModeParameter;

   parameterNumber : integer;
   paramName : unbounded_string;
   formalParamId : identifier;
   seenBracket : boolean := false;

begin

    -- In a syntax check, the formal parameters aren't created so there's
    -- no reason to look them up.  We're just reading through the parameters
    -- for the procedure call...

  -- Less brutal search than the old one:
  -- for i in 1..identifiers_top-1 loop
  --
  -- As an optimization, the params are likely located immediately after
  -- the proc itself is defined.  params are assumed to be stored sequentially
  -- In the future, params could be stored differently.
  --
  -- TODO: modify to handle namespaces

  formalParamId := proc_id + 1;
  parameterNumber := 1;
  -- put_line( "--- Searching from" ); -- DEBUG
  -- put_line( to_string( identifiers( proc_id-2 ).name ) ); -- DEBUG
  -- put_line( to_string( identifiers( proc_id-1 ).name ) ); -- DEBUG
  -- put_line( to_string( identifiers( proc_id ).name ) & " proc" ); -- DEBUG
  -- put_line( to_string( identifiers( proc_id+1 ).name ) ); -- DEBUG
  -- put_line( to_string( identifiers( proc_id+2 ).name ) ); -- DEBUG

  -- if there are no parameters and this is a function, you will run into the function's return
  -- value, which does not count as a parameter...abort the parameter search if it exists.
  if identifiers( formalParamId ).name = "return value" then
     formalParamId := identifiers_top; -- abort search
  end if;

  loop
      while formalParamId < identifiers_top loop
        if identifiers( formalParamId ).field_of = proc_id then
           if integer'value( to_string( identifiers( formalParamId ).value.all )) = parameterNumber then
              exit;
           end if;
        end if;
        formalParamId := identifier( integer( formalParamId ) + 1 );
      end loop;

    exit when formalParamId = identifiers_top;

    paramName := identifiers( formalParamId ).name;
    paramName := delete( paramName, 1, index( paramName, "." ) );

    -- On the first parameter, we expect the parameter to be in bracketed
    -- list.

    if not seenBracket then
       if token = symbol_t and identifiers( token ).value.all = "(" then
          expect( symbol_t, "(" );
          seenBracket := true;
       else
          err( "too few parameters" );
       end if;
    end if;

    -- Parse the parameter based on the passing mode (in, out, in out)

    case identifiers( formalParamId ).passingMode is

         when in_mode =>
              parseUsableInModeParameter( formalParamId, paramName );

         when out_mode =>
              parseUsableOutModeParameter( formalParamId, paramName );

         when in_out_mode =>
              parseUsableInoutModeParameter( formalParamId, paramName );

         when others =>
              err( gnat.source_info.source_location &
                   ": internal error: formal parameter" &
                   formalParamId'img &
                   "/" & to_string( identifiers( formalParamId ).name ) &
                   " has unsupported parameter passing mode" );
     end case;

     -- Not sure we need to exit on an error here - KB: 17/10/15
     --exit when error_found or identifiers( token ).value.all /= ","; -- more?
     exit when identifiers( token ).value.all /= ","; -- more?
     expectParameterComma;
     parameterNumber := parameterNumber + 1;
     formalParamId := identifier( integer( formalParamId ) + 1 );
  end loop;

  -- if the parameter is not found, we don't know if it misnamed
  -- or if there were too many given.
  -- This only applies if there were parameters.
  -- TODO: distinguish these error messages

  if formalParamId = identifiers_top and seenBracket then
     err( "parameter not found or too many parameters" );

  -- Check for too few formal parameters
  --
  -- If there are no actual parameters at all, this whole function
  -- doesn't run.  So this check does not help.

  elsif formalParamId < identifiers_top then
     parameterNumber := parameterNumber + 1;
     formalParamId := identifier( integer( formalParamId ) + 1 );
     while formalParamId < identifiers_top loop
        if identifiers( formalParamId ).field_of = proc_id then
           -- return value is end of function's parameters
           exit when identifiers( formalParamId ).name = "return value";
           if integer'value( to_string( identifiers( formalParamId ).value.all )) = parameterNumber then
              err( "too few parameters" );
              exit;
           end if;
        end if;
        formalParamId := identifier( integer( formalParamId ) + 1 );
     end loop;
  end if;

  -- If an open bracket, we need a closing bracket

  if seenBracket then
     expect( symbol_t, ")" );
  end if;
end ParseActualParameters;


-----------------------------------------------------------------------------
--  PARSE SEPARATE FUNC HEADER
--
-- Syntax: separate( parent ); function p [(param1...)] return type is
-- Note: forward declaration handling not yet written so minimal parameter
-- checking in the header.
-----------------------------------------------------------------------------

procedure ParseSeparateFuncHeader( func_id : identifier; funcStart : out natural ) is
  separate_func_id : identifier;
  type_token       : identifier;
  parent_id        : identifier;
  b : boolean;
  pu : unbounded_string;
  i : integer;
  ch : character;
begin
   -- separate
   expectDeclarationSemicolon( context => func_id );
   expect( separate_t );
   expect( symbol_t, "(");
   ParseIdentifier( parent_id );
   pu := identifiers( parent_id ).name;
   -- NOTE: the identifier token returned has the prefix stripped!  This needs
   -- to be fixed so I cannot check to see if it was full path before stripping.
   i := length( pu );
   while i > 0 loop
     ch := element( pu, i );
     if ch = '.' then
        delete( pu, 1, i );
        exit;
     end if;
     i := i - 1;
   end loop;
   if identifiers( parent_id ).class /= userProcClass and identifiers( parent_id ).class /= userFuncClass and identifiers( parent_id ).class /= mainProgramClass then
         err( "parent should be a subprogram" );
   elsif identifiers( parent_id ).name /= pu then
         err( "expected parent unit " & optional_yellow( to_string( pu ) ) );
   end if;
   expect( symbol_t, ")");
   expectDeclarationSemicolon( context => separate_t );
   -- separate's procedure header
   funcStart := firstPos;
   expect( function_t );
   -- could do ParseIdentifier since it should exist but want a more meaningful
   -- message error for a mismatch
   ParseProcedureIdentifier( separate_func_id );
   if identifiers( separate_func_id ).value.all = identifiers( func_id ).value.all then
      -- names match?  OK, discard.  proc is stored under original ident
      b := deleteIdent( separate_func_id );
   else
      err( optional_yellow( to_string( identifiers( separate_func_id ).name ) ) & " is different from parent file's " & optional_yellow( to_string( identifiers( func_id  ).name ) ) );
   end if;
   -- check for forward declarations not yet written so minimal checking here
   -- flush this out if i have time to walk the identifiers list if available
   if token = symbol_t and identifiers( token ).value.all = "(" then
      expect( symbol_t, "(" );
      while token /= symbol_t and identifiers( token ).value.all /= ")" and token /= eof_t loop
         getNextToken;
      end loop;
      expect( symbol_t, ")" );
   end if;
   expect( return_t );
   ParseIdentifier( type_token ); -- don't really care
   if identifiers( func_id ).kind /= type_token then
      err( optional_yellow( to_string( identifiers( type_token ).name ) ) & " is different from parent file's " & optional_yellow( to_string( identifiers( identifiers( func_id ).kind  ).name ) ) );
   end if;
   expect( is_t );
end ParseSeparateFuncHeader;


-----------------------------------------------------------------------------
--  PARSE FUNCTION BLOCK
--
-- Syntax: function [abstract] f OR function [abstract] p return t is block end p;
-- Handle procedure declarations, including forward declarations.
-- Note: DoUserDefinedFunction executes a user-defined function created by
-- this routine.
-----------------------------------------------------------------------------

procedure ParseFunctionBlock is
  func_id   : identifier;
  funcStart : natural;
  funcEnd   : natural;
  no_params   : integer := 0;
  old_error_found : constant boolean := error_found;
  abstract_parameter : identifier := eof_t;
  abstract_return    : identifier := eof_t;
  --b : boolean;
  declarationFile : unbounded_string;
  declarationLine : natural;
begin
  funcStart := firstPos;
  declarationFile := getSourceFileName;
  declarationLine := getLineNo;

  expect( function_t );

  -- If this is a completion of a forwarded declaration, func_id will
  -- bee a new id, not the original.

  ParseProcedureIdentifier( func_id );

  -- Whether forward or not, handle the parameters.  If there's a
  -- forward specification, verify the new parameters match the old
  -- ones.  Otherwise, parse the parameters as normal.


  if token = is_t then
     -- force error for this common mistake
     expect( return_t );
  elsif token = symbol_t and identifiers( token ).value.all = "(" then
     -- has parameters
     expect( symbol_t, "(" );
     if identifiers( func_id ).specAt /= noSpec then
        -- has a forward specification
        VerifySubprogramParameters( func_id, is_function => true );
        if token /= symbol_t and identifiers( token ).value.all /= ")" then
           err( "too many parameters compared to earlier specification (at " &
                to_string( identifiers( func_id ).specFile) & ":" &
                identifiers( func_id ).specAt'img & ")");
        end if;
        expect( symbol_t, ")" );
        -- we won't be able to see the specification until we delete
        -- the new one.
        --b := deleteIdent( func_id );
        --findIdent( identifiers( func_id ).name, func_id );
        VerifySubprogramReturnPart( func_id );
     else
        identifiers( func_id ).kind := new_t;
        -- normal function, no forward specification
        ParseFormalParameters( func_id, no_params, abstract_parameter,
          is_function => true );
        expect( symbol_t, ")" );
        ParseFunctionReturnPart( func_id, abstract_return );
     end if;
  elsif identifiers( func_id ).specAt /= noSpec then
     -- no parameters but has a forward specification
     VerifySubprogramParameters( func_id, is_function => true );
     -- we won't be able to see the specification until we delete
     -- the new one.
     --b := deleteIdent( func_id );
     --findIdent( identifiers( func_id ).name, func_id );
     VerifySubprogramReturnPart( func_id );
  else
     -- no parameters and no forward specification
     identifiers( func_id ).kind := new_t;
     ParseFunctionReturnPart( func_id, abstract_return );
  end if;


  -- Is it a forward declaration?
  --
  -- The function type must be set on a forward declaration because
  -- FunctionReturnPart is not run.

  if token = symbol_t and identifiers( token ).value.all = ";" then
     if identifiers( func_id ).specAt /= noSpec then
        err( "already declared specification for " & optional_yellow( to_string( identifiers( func_id ).name ) ) & " (at " &
                to_string( identifiers( func_id ).specFile) & ":" &
                identifiers( func_id ).specAt'img & ")");
     end if;
     identifiers( func_id ).class := userFuncClass;
     identifiers( func_id ).specFile := declarationFile;
     identifiers( func_id ).specAt := declarationLine;
  else
     identifiers( func_id ).class := userFuncClass;
     identifiers( func_id ).specFile := null_unbounded_string;
     identifiers( func_id ).specAt := noSpec;

     pushBlock( newScope => true,
       newName => to_string (identifiers( func_id ).name ) );
     DeclareActualParameters( func_id );
     expect( is_t );

     if token = null_t then                               -- null abstract
        expect( null_t );
        expect( abstract_t );
        identifiers( func_id ).usage := abstractUsage;
        if syntax_check then
           identifiers( func_id ).wasReferenced := true;
           --identifiers( func_id ).referencedByThread := getThreadName;
        end if;
        pullBlock;
     else
        if token = separate_t then
           if rshOpt then
              err( "subunits are not allowed in a " & optional_yellow( "restricted shell" ) );
           end if;
            expect( separate_t );
           -- "is separate" is effectively an include
           -- only insert include on a syntax check
           if syntax_check then
              insertInclude( identifiers( func_id ).name & ".sf" );
           end if;
           ParseSeparateFuncHeader( func_id, funcStart );
        elsif token = abstract_t then
           expect( abstract_t );
           identifiers( func_id ).usage := abstractUsage;
           if syntax_check then
              identifiers( func_id ).wasReferenced := true;
              --identifiers( func_id ).referencedByThread := getThreadName;
           end if;
        elsif abstract_parameter /= eof_t then
           err( "function must be abstract because parameter type " &
              optional_yellow( to_string( identifiers( abstract_parameter ).name ) ) &
              " is abstract" );
        elsif abstract_return /= eof_t then
           err( "function must be abstract because return type " &
              optional_yellow( to_string( identifiers( abstract_return ).name ) ) &
              " is abstract" );
        end if;
        ParseDeclarations;
        expect( begin_t );
        SkipBlock;                                       -- never execute
        if syntax_check then
           if not blockHasReturn then
              err( "function has no return value statement" );
           end if;
        end if;
        if token = exception_t then
           ParseExceptionHandler( old_error_found );
        end if;
        pullBlock;
        expect( end_t );
        expect( func_id );
        funcEnd := lastPos+1; -- include EOL ASCII.NUL
        identifiers( func_id ).value.all := to_unbounded_string( copyByteCodeLines( funcStart, funcEnd ) );
        -- fake initial indent of 1 for byte code (SOH)
        -- we don't know what the initial indent is (if any) since it may
        -- not be the first token on the line (though it usually is)
     end if;
  end if;
  expectDeclarationSemicolon( context => func_id );
end ParseFunctionBlock;


-----------------------------------------------------------------------------
--  DO USER DEFINED PROCEDURE
--
-- Execute a user-defined procedure.  Based on interpretScript.
-- procedure_name [(param1 [,param2...])]
-- Note: ParseProcedureBlock compiles / creates the user-defined procedure.
-- This routines runs the previously compiled procedure.
-----------------------------------------------------------------------------

procedure DoUserDefinedProcedure( s : unbounded_string ) is
  scriptState : aScriptState;
  results     : unbounded_string;
  proc_id     : identifier;

  -- chain contexts
  chain_count_id   : identifier := eof_t;
  last_in_chain_id : identifier := eof_t;
  in_chain     : boolean := false;
  has_context  : boolean := false;
  last_in_chain: boolean := false;
  contextName  : unbounded_string;
  old_error_found : constant boolean := error_found;
  old_exit_block : constant boolean := exit_block;
begin
  proc_id := token;
  if syntax_check then
     -- for declared but not used checking
     --When blocks are pulled, this will be checked.
     identifiers( proc_id ).wasReferenced := true;
     --identifiers( proc_id ).referencedByThread := getThreadName;
     if identifiers( proc_id ).usage = abstractUsage then
        err( optional_yellow( to_string( identifiers( proc_id ).name ) ) &
          " is abstract and cannot be run" );
     end if;
  end if;
  getNextToken;

  -- TODO: check for pre-existing chain context
  -- TODO: destroy chain context

  -- To check for a chain, the parameters must be read and the @ located
  -- (if it exists).  This must be done in syntax check mode since we
  -- don't want anything actually declared.  Then return to the start of
  -- the parameters and create a chain context block _prior_ to creating
  -- the procedure block.
  declare
    old_syntax_check : constant boolean := syntax_check;
    paramStart   : aScannerState;
  begin
    -- do we have a chain context?  Then we must be in a chain.
    contextName := identifiers( proc_id ).name & " chain";
    -- if we already have a context block, don't create another
    if blocks_top > 1 then
       has_context := ( getBlockName( blocks_top-1 ) = contextName );
       in_chain := has_context;
    end if;

    -- check for itself.  If it exists, we must be in a chain
    markScanner( paramStart );
    syntax_check := true;
    ParseActualParameters( proc_id, declareParams => false );
    if ( token = symbol_t or token = word_t ) and identifiers( token ).value.all = "@" then
       in_chain := true;
    end if;
    if has_context then
       if ( token = symbol_t or token = word_t ) and identifiers( token ).value.all = ";" then
          if trace then
             put_trace( "Last call in chain" );
          end if;
          last_in_chain := true;
       end if;
    end if;
    resumeScanning( paramStart );
    syntax_check := old_syntax_check;

    if in_chain then
       -- if we have a context?  then update the content of the context
       if has_context then
--put_line( "DEBUG: has context " );
          findIdent( chain_count_str, chain_count_id );
          if chain_count_id = eof_t then
             err( gnat.source_info.source_location &
                ": internal error: chain count not found" );
          else
             if isExecutingCommand then
                -- values only exist if not syntax check
                identifiers( chain_count_id ).value.all :=
                  to_unbounded_string(
                    to_numeric( identifiers( chain_count_id ).value.all ) + 1.0
                  );
                  findIdent( last_in_chain_str, last_in_chain_id );
                  if last_in_chain_id = eof_t then
                     err( gnat.source_info.source_location &
                        ": internal error: last in chain not found" );
                  end if;
                identifiers( last_in_chain_id ).value.all := to_bush_boolean( last_in_chain );
             end if;
--put_line( "DEBUG: chain count: " & to_string( identifiers( chain_count_id ).value )  );
--put_line( "DEBUG: last in cha: " & to_string( identifiers( last_in_chain_id ).value )  );
          end if;
       -- no context?  then we have to create a new context
       else
--put_line( "DEBUG: new context " );
          if trace then
             put_trace( "Creating chain context " & to_string( contextName ) );
          end if;
          pushBlock( newScope => true, newName => to_string( contextName ) );
          declareIdent( chain_count_id, chain_count_str, natural_t, varClass );
          declareIdent( last_in_chain_id, last_in_chain_str, boolean_t, varClass );
          if syntax_check then
             identifiers( chain_count_id ).wasReferenced := true;
             --identifiers( chain_count_id ).referencedByThread := getThreadName;
             identifiers( chain_count_id ).wasWritten := true;
             identifiers( chain_count_id ).wasFactor := true;
             identifiers( last_in_chain_id ).wasReferenced := true;
             --identifiers( last_in_chain_id ).referencedByThread := getThreadName;
             identifiers( last_in_chain_id ).wasWritten := true;
             identifiers( last_in_chain_id ).wasFactor := true;
          else
             if isExecutingCommand then
                -- values only exist if not syntax check
                identifiers( chain_count_id ).value.all := to_unbounded_string( " 1" );
                identifiers( last_in_chain_id ).value.all := to_bush_boolean( last_in_chain );
                -- TODO: we only want to export these if we are the current chain.
                -- Otherwise, they will always be exported even if the chain was further
                -- down the block stack
                --identifiers( chain_count_id ).export := true;
                --identifiers( last_in_chain_id ).export := true;
--put_line( "DEBUG: chain count: " & to_string( identifiers( chain_count_id ).value ) );
--put_line( "DEBUG: last in cha: " & to_string( identifiers( last_in_chain_id ).value ) );
             end if;
          end if;
       end if;
    end if;
    -- put_all_identifiers; -- DEBUG
  end;

  pushBlock( newScope => true,
     newName => to_string (identifiers( proc_id ).name ) );
  -- token will be @ here if in a chain but the final ; may or may not
  -- indicate a chain
  -- declareIdent( formal_param_id, to_unbounded_string( "chain count" ), type_token, varClass );
  -- if syntax_check then
  --    identifiers( formal_param_id ).wasReferenced := true;
  -- end if;
  if isExecutingCommand then
  -- Notice nothing gets executed during syntax check.  Any variables/parameters
  -- will have wasReferenced as false.
  -- TODO: perhaps using syntax_check inside PAP would fix this.
     --if token = symbol_t and identifiers( token ).value.all = "(" then
     ParseActualParameters( proc_id );
     --end if;
     parseNewCommands( scriptState, s );
     results := null_unbounded_string;        -- no results (yet)
     expect( procedure_t );
     if token = abstract_t then
        expect( abstract_t );
     end if;
     ParseIdentifier( proc_id );
     -- we already know the parameter syntax is good so skip to "is"
     while token /= is_t loop
        getNextToken;
     end loop;
     expect( is_t );
     ParseDeclarations;
     expect( begin_t );
     ParseBlock;
     if token = exception_t then
        ParseExceptionHandler( old_error_found );
     end if;
     -- Check to see if we're return-ing early
     -- TODO: Not pretty, but will work.  This should be improved.
     if exit_block and done_sub and not error_found and not syntax_check then
        done_sub := false;
        exit_block := old_exit_block;  -- TODO: is this right?
        done := false;
     end if;
     expect( end_t );
     expect( proc_id );
     expectDeclarationSemicolon( proc_id );
     if not done then                     -- not exiting?
         expect( eof_t );                  -- should be nothing else
     end if;
     restoreScript( scriptState );               -- restore original script
  elsif syntax_check or exit_block then
     -- at this point, we are still looking at call
     -- because nothing executes during a syntax check, we still need
     -- to parse the parameters to check for errors, but don't declare
     -- anything because wasReferenced will be false.
     ParseActualParameters( proc_id, declareParams => false );
  end if;
  pullBlock;

  if last_in_chain and has_context then
     if trace then
        put_trace( "Destroying chain context " & to_string( contextName ) );
     end if;
     pullBlock;
  end if;
end DoUserDefinedProcedure;


-----------------------------------------------------------------------------
--  DO USER DEFINED FUNCTION
--
-- Execute a user-defined function.  Based on interpretScript.  Return value
-- for function is result parameter.
-- function_name [(param1 [,param2...])]
-- Note: ParseFunctionBlock compiles / creates the user-defined function.
-- This routines runs the previously compiled function.
-----------------------------------------------------------------------------

procedure DoUserDefinedFunction( s : unbounded_string; result : out unbounded_string ) is
  scriptState : aScriptState;
  func_id     : identifier;
  return_id   : identifier;
  results     : unbounded_string;
  errorOnEntry : constant boolean := error_found;
  exitBlockOnEntry : constant boolean := exit_block;
begin
  -- Get the name of the function being called
  func_id := token;
  if syntax_check then
     -- for declared but not used checking
     --When blocks are pulled, this will be checked.
     identifiers( func_id ).wasReferenced := true;
     --identifiers( func_id ).referencedByThread := getThreadName;
     if identifiers( func_id ).usage = abstractUsage then
        err( optional_yellow( to_string( identifiers( func_id ).name ) ) &
          " is abstract and cannot be run" );
     end if;
  end if;
  getNextToken;
  -- Parameters will be in the new scope block
  pushBlock( newScope => true,
     newName => to_string (identifiers( func_id ).name ),
     newThread => identifiers( func_id ).name );
  -- Parameters?  Create storage space in the symbol table
  if isExecutingCommand then
     --if token = symbol_t and identifiers( token ).value.all = "(" then
     ParseActualParameters( func_id );
     --end if;
     -- Prepare to execute.  This should probably be a utility function.
     parseNewCommands( scriptState, s );
     results := null_unbounded_string;        -- no results (yet)
     expect( function_t );                    -- function
     if token = abstract_t then
        expect( abstract_t );
     end if;
     ParseIdentifier( func_id );              -- function name
     while token /= is_t loop                 -- skip header - syntax is good
        getNextToken;                         -- and params are declared
     end loop;
     expect( is_t );                          -- is
     ParseDeclarations;                       -- declaration block
     expect( begin_t );                       -- begin
     ParseBlock;                              -- executable block
     if token = exception_t then
        ParseExceptionHandler( errorOnEntry );
     end if;
     -- Check to see if we're return-ing early
     -- TODO: Not pretty, but will work.  This should be improved.
     if exit_block and done_sub and not error_found and not syntax_check then
        done_sub := false;
        exit_block := exitBlockOnEntry;
        done := false;
     end if;
     expect( end_t );                         -- end
     expect( func_id );                       -- function_name
     expectDeclarationSemicolon( context => func_id );
     if not done then                         -- not exiting?
         expect( eof_t );                     -- should be nothing else
     end if;
     -- return value is top-most variable called "return value"
     findIdent( to_unbounded_string( "return value" ), return_id );
     result := identifiers( return_id ).value.all;
     restoreScript( scriptState );            -- restore original script
  elsif syntax_check or exit_block then
     -- at this point, we are still looking at call
     -- because nothing executes during a syntax check, we still need
     -- to parse the parameters to check for errors, but don't declare
     -- anything because wasReferenced will be false.
     ParseActualParameters( func_id, declareParams => false );
  end if;
  pullBlock;                                  -- discard locals
end DoUserDefinedFunction;


-----------------------------------------------------------------------------
--  DO SHELL COMMAND
--
-- Syntax: command-word ( expr [,expr...] )
-- Syntax: command-word param-word [param-word...]
-----------------------------------------------------------------------------

procedure ParseShellCommand is
  cmdNameToken : identifier;
  cmdName    : unbounded_string;
  expr_val   : unbounded_string;
  expr_type  : identifier;
  ap         : argumentListPtr;          -- list of parameters to the cmd
  --paramCnt   : natural;                  -- number of parameters in ap
  firstParam : aScannerState;
  Success    : boolean;
  exportList : argumentListPtr;          -- exported C-string variables
  exportCnt  : natural := 0;             -- number of exported variables


  --  EXPORT VARIABLES
  --
  -- Search for all exported variables and export them to the
  -- environment so that the program we're running can see them.
  -- The search must start at the bottom of the symbol table so
  -- that, in the case of two exported variables with the same
  -- name, the most recent scope will supercede the older
  -- declaration.
  --  The variables exported are stored in exportList/exportCnt.
  -- Under UNIX/Linux, the application is responsible for storing
  -- the exported variables as C-strings.  The list must be cleared
  -- afterward.
  -- Note: this is not very efficient.
  ---------------------------------------------------------------------------

  procedure exportVariables is
    exportPos  : positive := 1;        -- position in exportList
    tempStr    : unbounded_string;
  begin
    -- count the number of exportable variables
    for id in 1..identifiers_top-1 loop
        if identifiers( id ).export and not identifiers( id ).deleted then
           exportCnt := exportCnt+1;
        end if;
    end loop;
    -- if there are exportable variables, export them and place them
    -- in exportList.
    if exportCnt > 0 then
       exportList := new argumentList( 1..positive( exportCnt ) );
       for id in 1..identifiers_top-1 loop
           if identifiers( id ).export and not identifiers( id ).deleted then
              -- a regular export means exporting the variable's value.  If
              -- it's a json export, then we have to convert the value to
              -- a json string (especially if it is a complex type)
              tempStr := identifiers( id ).value.all;
              if identifiers( id ).mapping = json then
                 if getUniType( identifiers( id ).kind ) = uni_string_t then
                    tempStr := DoStringToJson( tempStr );
                 elsif identifiers( id ).list then
                    DoArrayToJSON( tempStr, id );
                 elsif  identifiers( getBaseType( identifiers( id ).kind ) ).kind  = root_record_t then
                    DoRecordToJSON( tempStr, id );
                 elsif getUniType( identifiers( id ).kind ) = uni_numeric_t then
                    null; -- for numbers, JSON is as-is
                 else
                    err( "json export not yet written for this type" );
                 end if;
              end if;
              tempStr := identifiers( id ).name & "=" & tempStr;
              if trace then
                 put_trace( "exporting '" & to_string( tempStr ) & "'" );
              end if;
              exportList( exportPos ) := new string( 1..length( tempStr )+1 );
              exportList( exportPos ).all := to_string( tempStr ) & ASCII.NUL;
              if putenv( exportList( exportPos ).all ) /= 0 then
                 err( "unable to export " & optional_yellow( to_string( identifiers( id ).name) ) );
              end if;
              exportPos := exportPos + 1;
           end if;
       end loop;
    end if;
  end exportVariables;


  -- CLEAR EXPORTED VARIABLES
  --
  -- Clear the strings allocated in exportVariables
  -- should individual strings be deallocated?  If so, need to declare
  -- free?  Also, ap list?
  ---------------------------------------------------------------------------

  procedure clearExportedVariables is
    equalsPos : natural;
    result    : integer;
  begin
    if exportCnt > 0 then
       -- Remove exported items from environment O/S environment
       for i in exportList'range loop
           for j in exportList(i).all'range loop
               if exportList(i)(j) = '=' then
                  equalsPos := j;
                  exit;
               end if;
           end loop;
           C_reset_errno; -- freebsd bug: doesn't return result properly
           result := unsetenv( exportList( i )( 1..equalsPos-1 ) & ASCII.NUL );
           if result /= 0 and C_errno /= 0 then
              err( "unable to remove " &
                   optional_yellow( exportList( i )( 1..equalsPos-1 ) ) &
                   "from the O/S environment" );
           end if;
       end loop;
       -- Deallocate memory
       for i in exportList'range loop
           free( exportList( i ) );
       end loop;
       free( exportList ); -- deallocate memory
       exportCnt := 0;
    end if;
  end clearExportedVariables;


  --  CLEAR PARAM LIST
  --
  ---------------------------------------------------------------------------

  procedure clearParamList is
  begin
    for i in ap'range loop
        free( ap( i ) );
    end loop;
    free( ap );
  end clearParamList;

  --function isParenthesis return boolean is
  -- check for a paranthesis, skipping any white space in front.
  --begin
  --   skipWhiteSpace;
  --   return token = symbol_t and identifiers( token ).value.all = "(";
  --   -- return script( cmdpos ) = '(';
  --end isParenthesis;

  -- Word parsing and Parameter counting

  word         : unbounded_string;
  pattern      : unbounded_string;
  inBackground : boolean;
  --wordType     : aShellWordType;

  -- Pipeline parsing

  pipe2Next    : boolean := false;
  pipeFromLast : boolean := false;

  -- I/O Redirection parsing

  expectRedirectInFile        : boolean := false;     -- encountered <
  expectRedirectOutFile       : boolean := false;     -- encountered >
  expectRedirectAppendFile    : boolean := false;     -- encountered >>
  expectRedirectErrOutFile    : boolean := false;     -- encountered 2>
  expectRedirectErrAppendFile : boolean := false;     -- encountered 2>>

  redirectedInputFd           : aFileDescriptor := 0; -- input fd (if not 0)
  redirectedOutputFd          : aFileDescriptor := 0; -- output fd (if not 0)
  redirectedAppendFd          : aFileDescriptor := 0; -- output fd (if not 0)
  redirectedErrOutputFd       : aFileDescriptor := 0; -- err out fd (if not 0)
  redirectedErrAppendFd       : aFileDescriptor := 0; -- err out fd (if not 0)

  result                      : aFileDescriptor;
  closeResult                 : int;


  -- EXTERNAL COMMAND PARAMETER
  --
  ----------------------------------------------------------------------------

  procedure externalCommandParameters( ap : out argumentListPtr; list : in out bourneShellWordLists.List ) is
     len  : positive;
     theWord : anExpandedShellWord;
  begin
     if bourneShellWordLists.Length( list ) = 0 then
        ap := new argumentList( 1..0 );
        return;
     end if;
     len := positive( bourneShellWordLists.Length( list ) );
     ap := new argumentList( 1..len );
     for i in 1..len loop
         bourneShellWordLists.Find( list, long_integer( i ), theWord );
         ap( i ) := new string( 1..positive( length( theWord ) + 1 ) );
         ap( i ).all := to_string( theWord ) & ASCII.NUL;
     end loop;
  end externalCommandParameters;


  -- CHECK REDIRECT FILE
  --
  -- Check for a missing file for a redirection operator.  If a file
  -- was expected (according to the flags) but has not appeared, show
  -- an appropriate error message.
  ----------------------------------------------------------------------------

  procedure checkRedirectFile is
  begin
     if expectRedirectOutFile then
        err( "expected a file path for >" );
     elsif expectRedirectInFile then
        err( "expected a file path for <" );
     elsif expectRedirectAppendFile then
        err( "expected a file path for >>" );
     elsif expectRedirectErrOutFile then
        err( "expected a file path for 2>" );
     elsif expectRedirectErrAppendFile then
        err( "expected a file path for 2>>" );
     end if;
  end checkRedirectFile;

  wordList   : bourneShellWordLists.List;

  itselfNext : boolean := false;  -- true if a @ was encountered
  pipeStderr : boolean := false;  -- true stderr through pipeline
  needToRedirectErr2Out : boolean := false;  -- true if we're doing 2>&1


  --  PARSE SHELL REDIRECT TARGET
  --
  -- Read the next shell word argument, which should be the target for a
  -- redirection.  Expand the raw shell word if necessary.  Updates shellWord
  -- variable.
  ----------------------------------------------------------------------------
-- TODO: this could be a renaming

  procedure ParseShellRedirectTarget( shellWord : out anExpandedShellWord ) is
    rawWordValue : aRawShellWord;
  begin
    parseUniqueShellWord( shellWord );
  end ParseShellRedirectTarget;


  --  CHECK ADA 95 REDIRECTS
  ----------------------------------------------------------------------------

  procedure checkAda95Redirects is
  begin
     if onlyAda95 then
        err( "command line redirection not allowed with " &
             optional_yellow( "pragma ada_95" ) & ".  Use set_output/input/error instead" );
     end if;
  end checkAda95Redirects;


  --  PARSE SHELL OUTPUT REDIRECT
  ----------------------------------------------------------------------------

procedure ParseShellOutputRedirect is
  targetPath : anExpandedShellWord;
begin
  checkAda95Redirects;
  expect( shell_symbol_t, ">" );
  ParseShellRedirectTarget( targetPath );
  if redirectedAppendFD > 0 then
     err( "cannot redirect using both > and >>" );
  elsif rshOpt then
     err( "cannot redirect > in a " & optional_yellow( "restricted shell" ) );
  elsif pipe2Next then
     err( "> file should only be after the last pipeline command" );
  elsif isExecutingCommand then
<<retry1>> redirectedOutputFd := open( to_string( targetPath ) & ASCII.NUL,
              O_WRONLY+O_TRUNC+O_CREAT, 8#644# );
     -- Linux applies the umask to open()
     if redirectedOutputFd < 0 then
        if C_errno = EINTR then
           goto retry1;
        end if;
        err( "Unable to open > file: " & OSerror( C_errno ) );
     else
<<retry2>> result := dup2( redirectedOutputFd, stdout );
        if result < 0 then
           if C_errno = EINTR then
              goto retry2;
           end if;
           err( "unable to set output: " & OSerror( C_errno ) );
           closeResult := close( redirectedOutputFd );
           -- close EINTR is a diagnostic message.  Do not handle.
           redirectedOutputFd := 0;
        end if;
     end if;
  end if;
end ParseShellOutputRedirect;


  --  PARSE SHELL INPUT REDIRECT
  ----------------------------------------------------------------------------

procedure ParseShellInputRedirect is
  targetPath : anExpandedShellWord;
begin
  checkAda95Redirects;
  expect( shell_symbol_t, "<" );
  ParseShellRedirectTarget( targetPath );
  if pipeFromLast then
     err( "< file should only be after the first pipeline command" );
  elsif isExecutingCommand then
<<retry4>> redirectedInputFd := open( to_string( targetPath ) & ASCII.NUL, O_RDONLY, 8#644# );
     if redirectedInputFd < 0 then
        if C_errno = EINTR then
           goto retry4;
        end if;
        err( "Unable to open < file: " & OSerror( C_errno ) );
     else
<<retry5>> result := dup2( redirectedInputFd, stdin );
        if result < 0 then
           if C_errno = EINTR then
              goto retry5;
           end if;
           err( "unable to redirect input: " & OSerror( C_errno ) );
           closeResult := close( redirectedInputFd );
           -- close EINTR is a diagnostic message.  Do not handle.
           redirectedInputFd := 0;
        end if;
     end if;
  end if;
end ParseShellInputRedirect;


  --  PARSE SHELL OUTPUT APPEND REDIRECT
  ----------------------------------------------------------------------------

procedure ParseShellOutputAppendRedirect is
  targetPath : anExpandedShellWord;
begin
  checkAda95Redirects;
  expect( shell_symbol_t, ">>" );
  ParseShellRedirectTarget( targetPath );
  if redirectedOutputFD > 0 then
     err( "cannot redirect using both > and >>" );
  elsif pipe2Next then
     err( ">> file should only be after the last pipeline command" );
  elsif isExecutingCommand then
<<retry7>> redirectedAppendFd := open( to_string( targetPath ) & ASCII.NUL, O_WRONLY+O_APPEND, 8#644# );
     -- Linux applies the umask to open()
     if redirectedAppendFd < 0 then
        if C_errno = EINTR then
           goto retry7;
        end if;
        err( "Unable to open >> file: " & OSerror( C_errno ) );
     else
<<retry8>> result := dup2( redirectedAppendFd, stdout );
        if result < 0 then
           if C_errno = EINTR then
              goto retry8;
           end if;
           err( "unable to append output: " & OSerror( C_errno ) );
           closeResult := close( redirectedAppendFd );
           -- close EINTR is a diagnostic message.  Do not handle.
           redirectedAppendFd := 0;
        end if;
     end if;
  end if;
end ParseShellOutputAppendRedirect;


  --  PARSE SHELL ERR OUTPUT REDIRECT
  ----------------------------------------------------------------------------

procedure ParseShellErrOutputRedirect is
  targetPath : anExpandedShellWord;
begin
  checkAda95Redirects;
  expect( shell_symbol_t, "2>" );
  ParseShellRedirectTarget( targetPath );
  if redirectedErrAppendFD > 0 then
<<retry10>> result := dup2( currentStandardError, stderr );  -- restore stderr
     if result < 0 then                              -- check for error
        if C_errno = EINTR then
           goto retry10;
        end if;
        err( "unable to restore current error output: " & OSerror( C_errno ) );
     end if;
     closeResult := close( redirectedErrOutputFd );        -- done with file
     -- close EINTR is a diagnostic message.  Do not handle.
     redirectedErrOutputFD := 0;
     err( "cannot redirect using both 2> and 2>>" );
  elsif isExecutingCommand then
     -- Note: redirecting 2> to the same file twice in a pipeline
     -- is a race condition, but I don't know an easy way to
     -- guarantee a file isn't reused as multiple paths may lead
     -- to the same file.
<<retry12>> redirectedErrOutputFd := open( to_string( targetPath ) & ASCII.NUL,
               O_WRONLY+O_TRUNC+O_CREAT, 8#644# );
     -- Linux applies the umask to open()
     if redirectedErrOutputFd < 0 then
        if C_errno = EINTR then
           goto retry12;
        end if;
        err( "Unable to open 2> file: " & OSerror( C_errno ) );
     elsif rshOpt then
        err( "cannot redirect 2> in a " & optional_yellow( "restricted shell" ) );
     else
<<retry13>> result := dup2( redirectedErrOutputFd, stderr );
        if result < 0 then
           if C_errno = EINTR then
              goto retry13;
           end if;
           err( "unable to set error output: " & OSerror( C_errno ) );
           closeResult := close( redirectedErrOutputFd );
           -- close EINTR is a diagnostic message.  Do not handle.
           redirectedErrOutputFd := 0;
        end if;
     end if;
  end if;
end ParseShellErrOutputRedirect;


  --  PARSE SHELL ERR OUTPUT APPEND REDIRECT
  ----------------------------------------------------------------------------

procedure ParseShellErrOutputAppendRedirect is
  targetPath : anExpandedShellWord;
begin
  checkAda95Redirects;
  expect( shell_symbol_t, "2>>" );
  ParseShellRedirectTarget( targetPath );
  if redirectedErrOutputFD > 0 then
<<retry15>> result := dup2( currentStandardError, stderr );  -- restore stderr
      if result < 0 then                              -- check for error
         if C_errno = EINTR then
            goto retry15;
         end if;
         err( "unable to restore current error output: " & OSerror( C_errno ) );
      end if;
      closeResult := close( redirectedErrOutputFd );           -- done with file
      -- close EINTR is a diagnostic message.  Do not handle.
      redirectedErrOutputFD := 0;
      err( "cannot redirect using both 2> and 2>>" );
  elsif isExecutingCommand then
<<retry17>> redirectedErrAppendFd := open( to_string( targetPath ) & ASCII.NUL, O_WRONLY+O_APPEND, 8#644# );
      -- Linux applies the umask to open()
      if redirectedErrAppendFd < 0 then
         if C_errno = EINTR then
            goto retry17;
         end if;
         err( "Unable to open 2>> file: " & OSerror( C_errno ) );
      else
<<retry18>> result := dup2( redirectedErrAppendFd, stderr );
         if result < 0 then
            if C_errno = EINTR then
               goto retry18;
            end if;
            err( "unable to append error output: " & OSerror( C_errno ) );
            closeResult := close( redirectedErrAppendFd );
            -- close EINTR is a diagnostic message.  Do not handle.
            redirectedErrAppendFd := 0;
         end if;
      end if;
  end if;
end ParseShellErrOutputAppendRedirect;


  --  DO SHELL ERROR TO OUTPUT REDIRECT
  --
  -- This must be performed after we know if we're in a pipeline.
  ----------------------------------------------------------------------------

procedure DoShellErrorToOutputRedirect is
  targetPath : anExpandedShellWord;
begin
  checkAda95Redirects;
  needToRedirectErr2Out := false;
  if redirectedErrOutputFD > 0 then       -- no file for this one
<<retry20>> result := dup2( currentStandardError, stderr );  -- restore stderr
     if result < 0 then                              -- check for error
        if C_errno = EINTR then
           goto retry20;
        end if;
        err( "unable to restore current error output: " & OSerror( C_errno ) );
     end if;
     closeResult := close( redirectedErrOutputFd );          -- done with file
     -- close EINTR is a diagnostic message.  Do not handle.
     redirectedErrOutputFD := 0;
     err( "cannot redirect using two of 2>, 2>> and 2>&1" );

  elsif redirectedErrAppendFD > 0 then       -- no file for this one
<<retry22>> result := dup2( currentStandardError, stderr );  -- restore stderr
     if result < 0 then                               -- check for error
        if C_errno = EINTR then
           goto retry22;
        end if;
        err( "unable to restore current error output: " & OSerror( C_errno ) );
     end if;
     closeResult := close( redirectedErrAppendFd );   -- done with file
     -- close EINTR is a diagnostic message.  Do not handle.
     redirectedErrAppendFD := 0;
     err( "cannot redirect using two of 2>, 2>> and 2>&1" );
     -- KB: debugging
     --elsif pipe2Next then
     --   err( "2>&1 file should only be after the last pipeline command" );
  elsif isExecutingCommand then
     -- When redirecting standard error to standard output, how we
     -- do it depends on the context.  If we are in a pipeline,
     -- the jobs package must redirect both standard error and
     -- output to the pipe.  If we tried to redirect it here,
     -- the pipe hasn't been opened yet and there would nowhere to redirect
     -- to. If we are not in a pipeline, or are the last command,
     -- we redirect it here.
    if pipe2Next then
       pipeStderr := true; -- jobs package will handle it
    else
-- It looks wrong but, yes, active stderr to active stdout.
<<retry24>> redirectedErrOutputFd := dup2( stdout, stderr );
        if redirectedErrOutputFd < 0 then
           if C_errno = EINTR then
              goto retry24;
           end if;
           redirectedErrOutputFd := 0;
           err( "unable to set error output: " & OSerror( C_errno ) );
        end if;
     end if;
  end if;
end DoShellErrorToOutputRedirect;

  rawWordValue : aRawShellWord;
  shellWord  : anExpandedShellWord;
  -- TODO: refactor shellWord?

  haveAllParameters : boolean;

begin

  -- ParseGeneralStatement just did a resumeScanning.  The token should
  -- still be set to the value of the first word.

   -- Loop for all commands in a pipeline.

<<next_in_pipeline>>
  -- Reset parsing variables related to a single command

   -- shellWordList.Clear( wordList );                          -- discard params

  -- ParseGeneralStatement has rolled back the scanner after checking for :=.
  -- Reload the next token.

  -- getNextToken;

  -- Expand command variable (if any).  Otherwise, parse the first shell
  -- word.
  --
  -- Basically, we can't have a bareword expansion because of this: if the
  -- command is AdaScript syntax, the tokens need to be read from the script.
  -- It can't read tokens from the shell word list.  (e.g. if $cmd = "ls (",
  -- a $cmd bareword will put the paranthesis in the word list, not in the
  -- command line, after parsing.)
  --
  -- So, for now, I require the first shell word not to be a bareword with
  -- multiple subwords after expansion.  However, some of this could be
  -- improved in the future.

     cmdNameToken := token;                       -- avoid prob below w/discard

     -- For built-ins like cd, the command is the token name.  Otherwise, we'll
     -- have to treat the token as a shell word.
     if token >= env_t and token <= delete_t then
        cmdName := identifiers( token ).name;
        getNextToken;
     elsif identifiers( token ).kind /= new_t and then getBaseType( identifiers( token ).kind ) = command_t then
        cmdName := identifiers( token ).value.all;
        identifiers( token ).wasReferenced := true;
        getNextToken;
     else
        parseUniqueShellWord( shellWord );
        cmdName := unbounded_string( shellWord );
     end if;

     itself := cmdName;                                    -- this is new @
     pipeStderr := false;

  -- AdaScript Syntax: count the number of parameters, generate an argument
  -- list of the correct length, interpret the parameters "for real".

<<restart_with_itself>>

  inBackground := false;                                 -- assume fg command
  --paramCnt := 0;                                         -- params unknown

  if token = symbol_t and identifiers( token ).value.all = "(" then                                  -- parenthesis?
     -- getNextToken;                                    -- AdaScript syntax
     expect( symbol_t, "(" );                            -- skip paraenthesis
     markScanner( firstParam );                          -- save position
     while not error_found and token /= eof_t loop       -- count parameters
        ParseExpression( expr_val, expr_type );
        -- shellWordList.Queue( wordList, aShellWord'( normalWord, expr_val, expr_val ) );
        -- bourneShellWordLists.Queue( wordList, anExpandedShellWord( expr_val ) );
        addAdaScriptValue( wordList, expr_val );
        --paramCnt := paramCnt + 1;
        if Token = symbol_t and then identifiers( Token ).value.all = "," then
           getNextToken;
        else
           exit;
        end if;
     end loop;
     expect( symbol_t, ")" );
     if token = symbol_t and identifiers( token ).value.all = "|" then
        pipe2Next := true;
        getNextToken;
     end if;
     if pipe2Next and onlyAda95 then
        err( "pipelines are not allowed with " & optional_yellow( "pragma ada_95" ) );
     end if;

     -- Running in the background

     if token = symbol_t and identifiers(token).value.all = "&" then
        inbackground := true;
        expect( symbol_t, "&" );
        if pipe2Next then
           err( "no & - piped commands are automatically run in the background" );
        elsif pipeFromLast then
           err( "no & - final piped command always runs in the foreground" );
        end if;
        if token /= symbol_t or identifiers( token ).value.all /= ";" then
           err( "unexpected arguments after &" );
        end if;
     end if;

     -- Chaining with at-sign

     if token = symbol_t and identifiers(token).value.all = "@" then
        if onlyAda95 then
           err( "@ not allowed with " & optional_yellow( "pragma ada_95" ) );
        end if;
        itselfNext := true;
        expect( symbol_t, "@" );
     end if;

  else

    -- Bourne shell parameters

     -- discardUnusedIdentifier( token );
    -- Shell-style parameters.  Read a series of "words", counting the params.
    -- Generate an argument list of the correct length and repeat "for real".

     -- count loop
     -- markScanner( firstParam );
     word := null_unbounded_string;

     -- Shell words are not the only thing returned by the compiler, as there
     -- may be numbers, symbols, etc.  The compiler doesn't always know how
     -- to categorize tokens without the ability to look ahead.

  ---

  -- If there are no more shell words, then get the next token.
  -- If it's a semi-colon, then the we're done parsing the shell
  -- command.  Otherwise, if it's anything other than a shell
  -- symbol, then treat it as a shell word and expand it.

  -- First parameter

  haveAllParameters := false;
  while not haveAllParameters and not error_found and not done loop

    -- if bourneShellWordLists.IsEmpty(wordList) and not error_found and not done then
    if not error_found and not done then
       loop
--put_token; -- DEBUG
          declare
             token_value : unbounded_string renames identifiers( token ).value.all;
          begin
             if token = eof_t then
                expectStatementSemicolon( contextNotes => "in " & to_string( cmdName ) );
                haveAllParameters := true;
                exit;
             elsif token = symbol_t then
                -- if these exist, then the individual command is ended.
                if token_value = "|" then
                   if onlyAda95 then
                      err( "pipelines not allowed with " & optional_yellow( "pragma ada_95" ) );
                   end if;
                   pipe2next := true;
                   haveAllParameters := true;
                   getNextToken;
                   exit;
                elsif token_value = "@" then
                   if onlyAda95 then
                      err( "@ not allowed with " & optional_yellow( "pragma ada_95" ) );
                   end if;
                   itselfNext := true;
                   haveAllParameters := true;
                   getNextToken;
                   exit;
                elsif token_value = ";" then
                   haveAllParameters := true;
                   exit;
                elsif token_value = "&" then
                   --if not bourneShellWordLists.IsEmpty( wordList ) then
              -- if bourneShellWordLists.aListIndex( paramCnt ) /= bourneShellWordLists.Length( wordList ) then
                   --   err( "unexpected arguments after &" );
                   --end if;
                   inbackground := true;
                   if pipe2Next then
                      err( "no & - piped commands are automatically run in the background" );
                   elsif pipeFromLast then
                      err( "no & - final piped command always runs in the foreground" );
                   end if;
                   haveAllParameters := true;
                   expect( symbol_t, "&" );
                   if token /= symbol_t or identifiers( token ).value.all /= ";" then
                      err( "unexpected arguments after &" );
                   end if;
                   exit;
                 -- bourneShellWordLists.Clear( wordList, long_integer( paramCnt ) );
                 -- paramCnt := paramCnt-1;
               end if;

               --rawWordValue := aRawShellWord( token_value );
               parseShellWord( wordList );

             elsif token = shell_symbol_t then

               -- 2>&1 is not performed here because we need to know if we're
               -- in a pipeline first.

               if token_value = redirectOut_string then              -- > redirection
                  ParseShellOutputRedirect;
               elsif token_value = redirectIn_string then            -- < redirection
                  ParseShellInputRedirect;
               elsif token_value = redirectAppend_string then       -- >> redirection
                  ParseShellOutputAppendRedirect;
               elsif token_value = redirectErrOut_string then       -- 2> redirection
                  ParseShellErrOutputRedirect;
               elsif token_value = redirectErrAppend_string then   -- 2>> redirection
                 ParseShellErrOutputAppendRedirect;
               elsif token_value = redirectErr2Out_string then     -- 2>&1 redirection
                 expect( shell_symbol_t, "2>&1" );
                 needToRedirectErr2Out := true;
               end if;
           -- paramCnt := paramCnt+1;

          -- TODO: review this.
          -- Not a symbol or a shell symbol.
             else
            --rawWordValue := aRawShellWord( token_value );
               parseShellWord( wordList );
             end if;
          end; -- token_value
        --end if;
        end loop;
     end if;
   end loop;

-- Unlike the previous version, we're not processing the words first

  -- Ada 95 does not allow Bourne shell parameters after the command.

  if bourneShellWordLists.length( wordList ) > 0 then
     if onlyAda95 then
        err( "Bourne shell parameters not allowed with " &
             optional_yellow( "pragma ada_95" ) );
     end if;
   end if;

end if; -- AdaScript vs Bourne Shell

if needToRedirectErr2Out then
   DoShellErrorToOutputRedirect;
end if;

  -- End of Parameter Parsing

  -- At this point, only the command parameters should remain the word list.
  -- The input/output redirection are in place.  Declare the parameters and
  -- execute the command.

  if isExecutingCommand and not done then                   -- no problems?
     exportVariables;                                       -- make environment

     -- Create a list of C-strings for the parameters

-- TODO: wordList is only partial

     externalCommandParameters( ap, wordList );

     if boolean(rshOpt) and then Element( cmdName, 1 ) = '/' then -- rsh & cmd path
        err( "absolute paths to commands not allowed in " &
             optional_yellow( "restricted shells" ) );
     elsif not pipeFromLast and pipe2next then              -- first in pipeln?
        run_inpipe( cmdName, ap, Success,                   -- pipe output
           background => true,
           pipeStderr => pipeStderr );
     elsif pipeFromLast and not pipe2next then              -- last in pipeln?
        run_frompipe( cmdName, ap, Success,                 -- pipe input
           background => false );
        closePipeline;
        -- certain cmds (like "less") need to be cleaned up
        -- with wait4children.  Others are OK.  Why?
        -- KB: 18/05/23: because some commands expect child proceses and do
        -- a wait, others like "cut" and "less" do not.  SparForte must
        -- always wait4children to prevent zombies from being left behind
        -- by the commands of the pipeline.
         wait4children;                                     -- (child cleanup)
     elsif pipeFromLast and pipe2next then                  -- inside pipeline?
        run_bothpipe( cmdName, ap, Success,                 -- pipe in & out
           background => true,
           pipeStderr => pipeStderr );
     else                                                   -- no pipeline?
        run( cmdName, ap, Success,                          -- just run it
           background => inBackground );                    -- run the command
     end if;
     clearExportedVariables;                                -- clear environ
     discardUnusedIdentifier( cmdNameToken );               -- drop if not ident
  else                                                      -- cmd failure?

     -- If a pipeline command fails, then commands running in the
     -- background that accept user input will conflict with the
     -- command prompt.  We've got to wait until the final successful
     -- pipe command is finished before returning to the command prompt.
     --   For example, "cat | grep "h" < t.t" will fail because "<" must
     -- be on the first command.  However, cat will already be running
     -- in the background when the error occurs.  Spar will wait until
     -- ctrl-d is pressed, at which time the user is presented with the
     -- command prompt.  (This is the same behaviour as BASH.)
     --   Background commands do not require special handling.

     if pipeFromLast or pipe2next then                      -- in a pipeline?
        wait4LastJob;                                       -- (child cleanup)
     end if;

  end if;                                                   -- then discard it

  -- If there was command-line redirection, restore standard input/
  -- output to the original destinations.  The original files will
  -- be saved in currentStandardInput/Output.  The redirect flags
  -- should be set properly even if a parsing error occurred.

  if redirectedOutputFd > 0 then                            -- output redirect?
<<retry24b>> result := dup2( currentStandardOutput, stdout );       -- restore output
     if result < 0 then                                     -- check for error
        if C_errno = EINTR then
           goto retry24b;
        end if;
        err( "unable to restore current output: " & OSerror( C_errno ) );
     end if;
     closeResult := close( redirectedOutputFd );     -- done with file
     -- close EINTR is a diagnostic message.  Do not handle.
  elsif redirectedInputFd > 0 then                          -- input redirect?
<<retry25b>> result := dup2( currentStandardInput, stdout );-- restore input
     if result < 0 then                                     -- check for error
        if C_errno = EINTR then
           goto retry25b;
        end if;
        err( "unable to restore current input: " & OSerror( C_errno ) );
     end if;
     closeResult := close( redirectedInputFd );      -- done with file
     -- close EINTR is a diagnostic message.  Do not handle.
  elsif redirectedAppendFd > 0 then                         -- append redirect?
<<retry26b>> result := dup2( currentStandardOutput, stdout );-- restore output
     if result < 0 then                                     -- check for error
        if C_errno = EINTR then
           goto retry26b;
        end if;
        err( "unable to restore current output: " & OSerror( C_errno ) );
     end if;
     closeResult := close( redirectedAppendFd );     -- done with file
     -- close EINTR is a diagnostic message.  Do not handle.
  elsif redirectedErrOutputFd > 0 then                      -- errout redirect?
<<retry27b>> result := dup2( currentStandardError, stderr );-- restore stderr
     if result < 0 then                                     -- check for error
        if C_errno = EINTR then
           goto retry27b;
        end if;
        err( "unable to restore current error output: " & OSerror( C_errno ) );
     end if;
     -- If we redirected standard error to standard output, do not close
     -- standard error (fd 2).
     if redirectedErrOutputFd /= 2 then
        closeResult := close( redirectedErrOutputFd );  -- done with file
     end if;
     -- close EINTR is a diagnostic message.  Do not handle.
  elsif redirectedErrAppendFd > 0 then                      -- append redirect?
<<retry28b>>result := dup2( currentStandardError, stderr ); -- restore stderr
     if result < 0 then                                     -- check for error
        if C_errno = EINTR then
           goto retry28b;
        end if;
        err( "unable to restore current error output: " & OSerror( C_errno ) );
     end if;
     closeResult := close( redirectedErrAppendFd );  -- done with file
     -- close EINTR is a diagnostic message.  Do not handle.
  end if;

  -- restore the semi-colon we threw away at the beginning
  -- by this point, Token is eof_t, so we'll have to force it to a ';'
  -- since once Token is eof_t, it's always eof_t in the scanner

  if ap /= null then                                        -- parameter list?
     clearParamList;                                        -- discard it
     bourneShellWordLists.Clear( wordList );
  end if;

  -- Comand complete.  Look for next in pipeline (if any).

  pipeFromLast := pipe2Next;                                -- input from out
  if pipeFromLast and not error_found and not done then     -- OK so far?
     pipe2Next := false;                                    -- reset pipe flag
     if not error_found then                                -- found it?
        goto next_in_pipeline;                              -- next piped cmd
     end if;
  end if;

  -- Command ended with @?  Re-run with new parameters...

  if itselfNext then
     itselfNext := false;
     goto restart_with_itself;
  end if;

  bourneShellWordLists.clear( wordList );
end ParseShellCommand;


-----------------------------------------------------------------------------
--  COMPILE AND RUN
--
-- Compile and run the byte code.  Do not capture the output.
-- Set fragmement to false if the byte code is a complete script rather than
-- extracted from as a subscript.  You usually want to use
-- CompileRunAndCaptureOutput.  The only thing that uses this procedure
--  directly is the pragma debug because we don't want to capture the output.
-----------------------------------------------------------------------------

procedure CompileAndRun( commands : unbounded_string; firstLineNo : natural := 1; fragment : boolean := true ) is
  scriptState : aScriptState;
  --command     : unbounded_string := s;
  byteCode    : unbounded_string;
begin
-- TODO: set the line number from the enclosing script
  saveScript( scriptState );                            -- save current script
  compileCommand( commands, firstLineNo );              -- compile subscript
  byteCode := to_unbounded_string( script.all );        -- grab the byte code
  restoreScript( scriptState );                         -- restore original script
  if not error_found then                               -- no errors?
     if isExecutingCommand or Syntax_Check then            -- for real or check
        parseNewCommands( scriptState, byteCode, fragment ); -- setup byte code
        loop                                               -- run commands
           ParseGeneralStatement;                          -- general stmts
        exit when done or error_found or token = eof_t;    -- until done, error
        end loop;                                          --  or reached eof
        if not done then                                   -- not done?
           expect( eof_t );                                -- should be eof
       end if;
       restoreScript( scriptState );                 -- restore original script
     end if;
  end if;
end CompileAndRun;


-----------------------------------------------------------------------------
-- RUN AND CAPTURE OUTPUT
--
-- Run the byte code and return the results.  Set fragmement to false if the
-- byte code is a complete script rather than extracted from as a subscript.
-- You usually want to use CompileRunAndCaptureOutput.  The only thing that
-- uses this procedure directly is the prompt script because the byte code
-- is saved.
--
-- TODO: SHOULD BE REWRITTEN TO USE PIPES INSTEAD OF TEMP FILE
-- based on interpretScript
-----------------------------------------------------------------------------

procedure RunAndCaptureOutput( s : unbounded_string; results : out
  unbounded_string; fragment : boolean := true ) is
  scriptState : aScriptState;
  --command     : unbounded_string := s;
  oldStandardOutput : aFileDescriptor;
  oldCurrentOutput : aFileDescriptor;
  resultFile  : aFileDescriptor := -1;
  resultName  : unbounded_string;
  result      : aFileDescriptor;
  closeResult : int;
  unlinkResult : int;
  ch          : character := ASCII.NUL;
  chars       : size_t;
begin
  -- saveScript( scriptState );                  -- save current script
-- put_token; -- DEBUG
  results := null_unbounded_string;
  if isExecutingCommand then                               -- only for real
     makeTempFile( resultName );                           -- results filename
     resultFile := open( to_string( resultName ) & ASCII.NUL, -- open results
        O_WRONLY+O_TRUNC, 8#640# );                        -- for writing
     if resultFile < 0 then                                -- failed?
        err( "RunAndCaptureOutput: unable to open file: "&
           OSerror( C_errno ));
     elsif trace then                                      -- trace on?
        put_trace( "results will be captured from file descriptor" &
          resultFile'img );
     end if;
  end if;
  if isExecutingCommand or Syntax_Check then               -- for real or check
     parseNewCommands( scriptState, s, fragment );         -- install cmds
  end if;
  if isExecutingCommand and resultFile > 0 then            -- only for real
     oldStandardOutput := currentStandardOutput;           -- save old stdout
     result := dup2( resultFile, stdout );                 -- redirect stdout
     if result < 0 then                                    -- error?
        err( "unable to set output: " & OSerror( C_errno ) );
     elsif not error_found then                            -- no error?
        currentStandardOutput := resultFile;               -- track fd
     end if;

     -- SparForte's current output (fd 4) has changed
     oldCurrentOutput := dup( 4 );                         -- backup
     result := dup2( resultFile, 4 );                      -- redirect curout
     if result < 0 then                                    -- error?
        err( "unable to set output *current output): " & OSerror( C_errno ) );
     end if;

  end if;
  if isExecutingCommand or Syntax_Check then               -- for real or check
     loop                                                  -- run commands
        ParseGeneralStatement;                             -- general stmts
        exit when done or error_found or token = eof_t;    -- until done, error
      end loop;                                            --  or reached eof
      if not done then                                     -- not done?
         expect( eof_t );                                  -- should be eof
     end if;
  end if;
  -- Read the results.  Don't worry if a syntax check or not.  If we were
  -- redirecting for any reason, get the results and restore standard output
  -- if commands contain a pipeline, there may have been a fork
  -- If this is one of the pipeline commands, we'll be exiting
  -- so check to see that we are still executing something.
  if not done and resultFile > 0 then                   -- redirecting?

     -- Recover current_output

     result := dup2( oldCurrentOutput, 4 );                -- redirect curout
     if result < 0 then                                    -- error?
        err( "unable to restore output (current output): " & OSerror( C_errno ) );
     end if;
     closeResult := close( oldCurrentOutput );             -- free mem

     -- Recover standard_output

     result := dup2( oldStandardOutput, stdout );       -- to original
     if result < 0 then                                 -- error?
        err( "unable to restore stdout: " & OSerror( C_errno ) );
     else                                               -- no error?
        currentStandardOutput := oldStandardOutput;     -- track fd
     end if;
     closeResult := close( resultFile );           -- reopen results
     -- close EINTR is a diagnostic message.  Do not handle.
     resultFile := open( to_string(resultName) & ASCII.NUL, O_RDONLY,
         8#644# );
     if resultFile < 0 then                                -- error?
        err( "unable to open temp file for reading: " &
           OSError( C_errno ));
     else
        loop                                               -- for all results
<<reread>>
          readchar( chars, resultFile, ch, 1 );            -- slow (one char)
          if chars = 0 then                                -- read none?
             exit;                                         --   done
 -- KB: 2012/02/15: see spar_os-tty for an explaination of this kludge
          elsif chars not in 0..Interfaces.C.size_t'Last-1 then
             if C_errno = EAGAIN or C_errno = EINTR then   -- retry?
                goto reread;                               -- do so
             end if;                                       -- other error?
             err( "unable to read results: " & OSError( C_errno ) );
             exit;                                         --  and bail
          end if;
          results := results & ch;                         -- add to results
        end loop;
        closeResult := close( resultFile );            -- close and delete
        -- close EINTR is a diagnostic message.  Do not handle.
     end if;
     unlinkResult := unlink( to_string( resultName ) & ASCII.NUL );
     if unlinkResult < 0 then                              -- unable to delete?
        err( "unable to unlink temp file: " & OSError( C_errno ) );
     end if;
     if length( results ) > 0 then                         -- discard last EOL
        if element( results, length( results ) ) = ASCII.LF then
           delete( results, length( results ), length( results ) );
           if length( results ) > 0 then  -- MS-DOS
              if element( results, length( results ) ) = ASCII.CR then
                 delete( results, length( results ), length( results ) );
              end if;
           end if;
        end if;
     end if;
  -- elsif not syntax_check then                              --
  --    close( resultFile );
  end if;                                                  -- still executing
  -- If we saved the script state, restore it.
  if isValid( scriptState ) then
     restoreScript( scriptState );                            -- original script
  end if;
end RunAndCaptureOutput;


-----------------------------------------------------------------------------
-- COMPILE RUN AND CAPTURE OUTPUT
--
-- Compile commands, run the commands and return the results.  If first
-- line number is supplied, it will be used for the first line number of
-- the commands (as opposed to line 1).
-----------------------------------------------------------------------------

procedure CompileRunAndCaptureOutput( commands : unbounded_string; results : out
  unbounded_string; firstLineNo : natural := 1  ) is
  byteCode : unbounded_string;
  scriptState : aScriptState;
begin
  saveScript( scriptState );               -- save current script
  compileCommand( commands, firstLineNo );
  byteCode := to_unbounded_string( script.all );
  if not error_found then
     RunAndCaptureOutput( byteCode, results, fragment => false );
  end if;
  restoreScript( scriptState );            -- restore original script
end CompileRunAndCaptureOutput;


-----------------------------------------------------------------------------
--  PARSE STEP
--
-- debugger: step one instruction forward.  Do this by activating SIGINT
-----------------------------------------------------------------------------

procedure ParseStep is
begin
  expect( step_t );
  if inputMode /= breakout then
     err( "step can only be used when you break out of a script" );
  else
     done := true;
     breakoutContinue := true;
     stepFlag1 := true;
     put_trace( "stepping" );
  end if;
end ParseStep;


-----------------------------------------------------------------------------
--  PARSE RETURN
--
-- Syntax: return [function-result-expr]
-- Return from a subprogram or quit interactive session or resume from a
-- breakout.
-----------------------------------------------------------------------------

procedure ParseReturn is
  expr_val    : unbounded_string;
  expr_type   : identifier;
  return_id   : identifier;
  must_return : boolean;
  has_when    : boolean := false;
begin
  -- Return has a special meaning in interactive modes
  if inputMode = breakout then
     expect( return_t );
     expectReturnSemicolon;
     done := true;
     breakoutContinue := true;
     syntax_check := true;
     put_trace( "returning to script" );
  elsif inputMode = interactive then
     if isLoginShell then
        err( "warning: This is a login shell.  Use " &
             optional_yellow( "logout" ) & " to quit." );
     else
        expect( return_t );
        expectReturnSemicolon;
        if isExecutingCommand then
           DoQuit;
        end if;
     end if;
  else

     -- Return statement in a procedure or function

     expect( return_t );

     if token /= eof_t and token /= when_t and not (token = symbol_t and identifiers( token ).value.all = ";") and not (token = symbol_t and identifiers( token ).value.all = "|" ) then

        -- Handle a function return value
        --
        -- The return value gets assigned even if an optional when clause
        -- follows and indicates the return does not happen.

        if isExecutingCommand then
           -- return value only exists at run-time.  There are better ways to
           -- do this.
           findIdent( to_unbounded_string( "return value" ), return_id );
           if return_id = eof_t then
              err( "procedures cannot return a value" );
           else
           -- at this point, we don't know the function id.  Maybe we can
           -- check the block name and derrive it that way.  Until we do,
           -- no type checking on the function result!
              ParseExpression( expr_val, expr_type );
              identifiers( return_id ).value.all := expr_val;
              if trace then
                 put_trace( "returning """ & to_string( expr_val ) & """" );
              end if;
           end if;
        else
              -- for syntax checking, we need to walk the expression
              ParseExpression( expr_val, expr_type );
        end if;
     end if;

     -- Handle option when expression.

     if token = when_t or token = if_t then     -- if to give "expected when"
        has_when := true;
        ParseWhenClause( must_return );
     else
         must_return := true;
     end if;

     -- Check for semi-colon and

     expectReturnSemicolon;
     -- we don't know the subprogram name
     if syntax_check then
        -- this marks a function return has having been seen for checks on
        -- a function with no return.
        sawReturn;
        -- look for unreachable code only during syntax check
        -- does not apply if there's a when clause
        if not has_when then
           -- these are block ending tokens
           if token /= eof_t and token /= end_t and token /= elsif_t and
              token /= else_t and token /= when_t and token /= others_t and
              token /= exception_t then
                 err( "the return makes this unreachable code" );
           end if;
        end if;
     end if;

     -- Execute a return.  Trigger skipping block and exiting.

     if isExecutingCommand then
        if must_return then
           DoReturn;
        end if;
     end if;
  end if;
end ParseReturn;


-----------------------------------------------------------------------------
--  PARSE ASSIGNMENT
--
-- Basic variable assignment
-- Syntax: var := expression or array(index) := expr
-----------------------------------------------------------------------------

procedure ParseAssignment( autoDeclareAllowed : boolean := false ) is
  var_id     : identifier;
  var_kind   : identifier;
  expr_value : unbounded_string;
  right_type : identifier;
  index_value: unbounded_string;
  index_kind : identifier;
  -- array_id   : arrayID;
  arrayIndex : long_integer;
  targetExpression : line_count;
begin
  -- Get the variable to assign to.  If interactive, consider
  -- auto-declarations.
  if inputMode = interactive or inputMode = breakout or autoDeclareAllowed then
    if identifiers( token ).kind = new_t and not onlyAda95 and not restriction_no_auto_declarations then
       ParseNewIdentifier( var_id );
       if token = symbol_t and identifiers( token ).value.all = "(" then
          err( "cannot automatically declare new arrays" );
          discardUnusedIdentifier( var_id );
          var_id := eof_t;
       end if;
    else
       ParseIdentifier( var_id );
    end if;
  else
     ParseIdentifier( var_id );
  end if;

  -- Copy the type for convenience

  var_kind := identifiers( var_id ).kind;

  -- Setup itself

  itself := identifiers( var_id ).value.all;
  itself_type := var_kind;

  if type_checks_done or else class_ok( var_id, varClass ) then
      checkVarUsageQualifier( var_id );
  end if;

  -- Handle aggregate type assignment checks
  -- TODO: this will break when we can create derived record types

  if var_kind = root_record_t then
     err( "cannot assign to an entire record" );
  -- Array element
  elsif identifiers( var_id ).list then
     expect( symbol_t, "(" );
     ParseExpression( index_value, index_kind );
     if getUniType( index_kind ) = uni_string_t or identifiers( index_kind ).list then
        err( "scalar expression expected" );
     end if;

     expect( symbol_t, ")" );

     if isExecutingCommand then
        arrayIndex := long_integer( to_numeric( index_value ) );
        if identifiers( var_id ).avalue = null then
           err( gnat.source_info.source_location &
                ": internal error: target array storage unexpectedly null" );
        elsif identifiers( var_id ).avalue'first > arrayIndex then
           err( "array index " & to_string( trim( index_value, ada.strings.both ) ) & " not in" & identifiers( var_id ).avalue'first'img & " .." & identifiers( var_id ).avalue'last'img );
        elsif identifiers( var_id ).avalue'last < arrayIndex then
           err( "array index " &  to_string( trim( index_value, ada.strings.both ) ) & " not in" & identifiers( var_id ).avalue'first'img & " .." & identifiers( var_id ).avalue'last'img );
        end if;
     end if;
     var_kind := identifiers( var_kind ).kind; -- array of what?
  end if;

  -- Side-effect checking.  The time the variable being assigned to was used
  -- as a factor must be saved and recovered as we want to know if it was a
  -- factor in the previous nested expression context.
  --
  -- Example:
  --   x = y + f1() where f1(): y = 1
  --   x = y + f1() where f1(): y = y + 1
  --
  -- The first works fine.  The second has a problem.  y will be noted as a
  -- factor on the second expression, losing its association as a factor in
  -- the first expression.  It won't be detected as a side-effect.  This is
  -- because expressions can be nested.

  targetExpression := lastExpressionInstruction;

  ParseAssignPart( expr_value, right_type );

  lastExpressionInstruction := targetExpression;

  if inputMode = interactive or inputMode = breakout or autoDeclareAllowed then
     if identifiers( var_id ).kind = new_t and not onlyAda95 and not restriction_no_auto_declarations and not error_found then
        if index( identifiers( var_id ).name, "." ) /= 0 then
           err( "Identifier not declared.  Cannot auto-declare a record field" );
        else
           var_kind := right_type;
           identifiers( var_id ).kind := right_type;
           identifiers( var_id ).class := varClass;
           if inputMode = interactive or inputMode = breakout then
              put_trace( "Assuming " & to_string( identifiers( var_id ).name ) &
                 " is a new " & to_string( identifiers( right_type ).name ) &
                 " variable" );
           end if;
        end if;
      end if;
  end if;

  -- Type testing and casting

  -- try to assign an exception to a universal type.  We need to flag that as
  -- a special case
  if right_type = exception_t then
     err( "exceptions cannot be assigned" );
  elsif type_checks_done or else baseTypesOK( var_kind, right_type ) then
     if syntax_check then
        identifiers( var_kind ).wasCastTo := true;
     end if;
     if isExecutingCommand then
        expr_value := castToType( expr_value, var_kind );
     end if;
  end if;

  -- Before assignment, remember that this identifier was written to
  -- by the assignment for later identifier usage checks.  If this is
  -- the field of the record, mark the whole record as being written
  -- to.  Otherwise, mark just the identifier.
  -- Check for error in case var_id is eof_t.
  --
  -- A variable that is assigned a value cannot be limited, so
  -- treat it as if it was a factor.

  if syntax_check and then not error_found then
     if identifiers( var_id ).field_of /= eof_t then
        -- we don't track record fields, only the record
        identifiers( identifiers( var_id ).field_of ).wasWritten := true;
        identifiers( identifiers( var_id ).field_of ).wasFactor := true;
     else
        identifiers( var_id ).wasWritten := true;
        identifiers( var_id ).wasFactor := true;
     end if;
  end if;

  if isExecutingCommand then
     -- parse assignment: side-effect checking
     --
     -- Double write races (strict)

     --checkDoubleGlobalWrite( var_id );

     -- parse assignment: side-effect checking
     --
     -- Read in factor, then wrote

     checkExpressionFactorVolatilityOnWrite( var_id );

     -- parse assignment: expression side-effects
     -- writing to a variable that is already read?
     -- How do we do that?

     -- Expression Side-effects: record how many lines run by this point in time
     -- so we can determine later if this assignment occurred after the current
     -- expression started (if any).  lineCnt will be zero if not checking for
     -- side-effects.
     -- For a record field, mark the whole record.

     if identifiers( var_id ).field_of /= eof_t then
        identifiers( identifiers( var_id ).field_of ).writtenOn := perfStats.lineCnt;
     else
        identifiers( var_id ).writtenOn := perfStats.lineCnt;
     end if;

     -- parse assignment: side-effect checking
     --
     -- Double write races (relaxed)
     --
     -- Run-time side-effects tracking and test
     -- Test for two or more "threads" writing to one unprotected variable

     checkDoubleThreadWrite( var_id );

     -- Programming-by-contract

     if var_kind /= right_type then
        DoContracts( var_kind, expr_value );
     end if;

     if identifiers( var_id ).list then
        --mem_id := long_integer( to_numeric( identifiers( var_id ).value ) );
        --arrayIndex := long_integer( to_numeric( index_value ) );
        --if not inBounds( array_id, arrayIndex ) then
        --   err( "exception raised" );
        --else
           -- assignElement( array_id, arrayIndex, expr_value );
        begin
           -- KB: 16/10/02: these appear to be checked above
           --if identifiers( var_id ).avalue = null then
           --   err( gnat.source_info.source_location & ": internal error: target array storage unexpectedly null" );
           --elsif identifiers( var_id ).avalue'first > arrayIndex then -- DEBUG, this should never happen, checked above
           --   err( gnat.source_info.source_location & ": internal error: array index out of bounds " & identifiers( var_id ).avalue'first'img & " .. " & identifiers( var_id ).avalue'last'img );
           --elsif identifiers( var_id ).avalue'last < arrayIndex then
           --   err( gnat.source_info.source_location & ": internal error: array index out of bounds " & identifiers( var_id ).avalue'first'img & " .. " & identifiers( var_id ).avalue'last'img );
           --elsif not error_found then
           identifiers( var_id ).avalue( arrayIndex ) := expr_value; -- NEWARRAY
           --end if;
        exception when CONSTRAINT_ERROR =>
          err( "constraint_error : index out of range " & identifiers( var_id ).avalue'first'img & " .." & identifiers( var_id ).avalue'last'img );
        when STORAGE_ERROR =>
          err( gnat.source_info.source_location & ": internal error : storage error raised in ParseAssignment" );
        end;
        if trace then
           put_trace(
              to_string( identifiers( var_id ).name ) &
              "(" &
              to_string( index_value ) &
              ")" &
              " := """ &
              to_string( ToEscaped( expr_value ) ) &
                 """" );
        end if;
     else
        identifiers( var_id ).value.all := expr_value;
        if trace then
           -- builtins.env( ident ) would be better if a value is
           -- returned
           put_trace(
              to_string( identifiers( var_id ).name ) &
              " := """ &
              to_string( ToEscaped( expr_value ) ) &
              """" );
        end if;
     end if;
  end if;
  itself_type := new_t;
end ParseAssignment;


-----------------------------------------------------------------------------
--  PARSE VAR DECLARATION
--
-- Basic variable declaration
-- Syntax: var [,var2 ...] declaration_part
-- Array variables can only be declared one-at-a-time
-----------------------------------------------------------------------------

procedure ParseVarDeclaration is
  var_id  : identifier;
  var2_id : identifier;
  name    : unbounded_string;
  multi   : boolean := false;
  b       : boolean;
begin
   ParseVariableIdentifier( var_id );
   if token = symbol_t and identifiers( token ).value.all = "," then
      expectParameterComma;
      var2_id := token;
      pragma warnings( off ); -- hide infinite recursion warning
      ParseVarDeclaration;
      multi := true;
      pragma warnings( on );
      if error_found then
         discardUnusedIdentifier( var_id );
      else
         if identifiers( var2_id ).list then
            err( "multiple arrays cannot be declared in one declaration" );
            -- because only the array is assigned values with :=
            -- unless I want to copy all the array elements everytime.
            -- Also, can't overwrite array ident value field.
            b := deleteIdent( var2_id );
         else
            -- OK so far? copy declaration leftward through variable list

            name := identifiers( var_id ).name;
            -- Because var is now a pointer, we cannot simply assign
            -- identifiers to each other...the pointers will be wrong.
            -- identifiers( var_id ) := identifiers( var2_id );
            -- identifiers( var_id ).name := name;
            copyValue( var_id, var2_id );

            -- Record type?  Must create fields.

            if identifiers( getBaseType( identifiers( var_id ).kind ) ).kind = root_record_t then  -- record type?
-- put_line( "Recursing for " & identifiers( var_id ).name );
-- put_token;
               ParseRecordDeclaration( var_id, identifiers( var_id ).kind, canAssign => false );
               -- copy values for fields
               declare
                 numFields : natural;
                 source_id, target_id : identifier;
               begin
                 numFields := natural( to_numeric( identifiers( identifiers( var_id ).kind ).value.all ) );
-- put_line( "Copying " & numFields'img );
                 for i in 1..numFields loop
                     findField( var_id, i, source_id );
                     findField( var2_id, i, target_id );
                     identifiers( source_id ).value.all := identifiers( target_id ).value.all;
                 end loop;
               end;
           end if;

         end if;
      end if;
      return;
   end if;
   ParseDeclarationPart( var_id, anon_arrays => true, exceptions => true ); -- var id may change...won't effect next stmt
   if error_found then
      discardUnusedIdentifier( var_id );
   end if;
end ParseVarDeclaration;


-----------------------------------------------------------------------------
--  PARSE EXECUTABLE STATEMENT
--
-- This procedure is used when any executable statement is expected (such as
-- if, for, shell commands, procedure calls, etc.  It also handles Control-C,
-- itself, breakout stepping, return, exit, @ chaining and other common
-- functions.
--
-- This differs from ParseGeneralStatement in that declarations are not
-- allowed.
-----------------------------------------------------------------------------

procedure ParseExecutableStatement is
  -- Syntax: env-cmd | clear-cmd | ...
  cmdStart   : aScannerState;
  must_exit  : boolean;
  eof_flag   : boolean := false;
  term_id    : identifier;
  startToken : identifier;
  itself_question : boolean;
begin

  -- mark start of line (prior to breakout test which will change token
  -- to eof )

  startToken := Token;
  markScanner( cmdStart );

  -- interrupt handling

  if stepFlag1 then
     stepFlag1 := false;
     stepFlag2 := true;
  elsif stepFlag2 then
     stepFlag2 := false;
     wasSIGINT := true;
  end if;
  if wasSIGINT then                                      -- control-c?
     if inputMode = interactive or inputMode = breakout then -- interactive?
        wasSIGINT := false;                              -- just ignore
     elsif not breakoutOpt then                          -- no breakouts?
        wasSIGINT := false;                              -- clear flag
        DoQuit;                                          -- stop BUSH
     else                                                -- running script?
        for i in 1..identifiers_top-1 loop
            if identifiers( i ).inspect then
               Put_Identifier( i );
            end if;
        end loop;
        --BREAKDBG
        put_line( standard_error, get_script_execution_position(
           optional_inverse( "Break: return to continue, logout to quit" ) ) ); -- show stop posn
        error_found := true;
     end if;
  elsif wasSIGWINCH then                                 -- window change?
     findIdent( to_unbounded_string( "TERM" ), term_id );
     checkDisplay( identifiers( term_id ).value.all );       -- adjust size
     wasSIGWINCH := false;
  end if;

  -- Parse the general statement
  --
  -- built-in?

  itself := identifiers( token ).value.all;
  itself_type := token;
  itself_question := false;

--put( "PES: " ); -- DEBUG
--put_token; -- DEBUG

  if Token = command_t then
     err( "Bourne shell command command not implemented" );
     --getNextToken;
     --ParseShellCommand;
  elsif identifiers( token ).procCB /= null then  -- built-in proc w/cb?
     identifiers( token ).procCB.all;             -- call the callback
  elsif Token = typeset_t then
     ParseTypeSet;
  elsif Token = pragma_t then
     ParsePragma;
  elsif Token = type_t then
     err( "declarations not allowed in executable statements" );
     -- ParseType;
  elsif Token = null_t then
     getNextToken;
  elsif Token = subtype_t then
     err( "declarations not allowed in executable statements" );
     -- ParseSubtype;
  elsif Token = if_t then
     ParseIfBlock;
  elsif Token = case_t then
     ParseCaseBlock;
  elsif Token = while_t then
     ParseWhileBlock;
  elsif Token = for_t then
     ParseForBlock;
  elsif Token = loop_t then
     ParseLoopBlock;
  elsif Token = return_t then
     ParseReturn;
     return;
  elsif Token = step_t then
     ParseStep;
  elsif token = logout_t then
     if not isLoginShell and inputMode /= breakout then
        err( "warning: this is not a login shell: use " & optional_yellow( "return" ) &
             " to quit" );
     end if;
     getNextToken;
     expectStatementSemicolon( context => logout_t );
     if not error_found or inputMode = breakout then
        DoQuit;
     end if;
     return;
  elsif Token = create_t then
     ParseOpen( create => true );
  elsif Token = open_t then
     ParseOpen;
  elsif token = symbol_t and identifiers( token ).value.all = "?" then
     -- To implement "itself" with the "?" is difficult because
     -- "?" is a symbol and "@" is a symbol, so trying to look
     -- up the symbol value to determine if is "?" is not possible.
     -- So we flag this as a special case.
     itself_question := true;
     ParseQuestion;
  elsif Token = reset_t then
     ParseReset;
  elsif Token = delete_t then                     -- special case
     getNextToken;                                -- with possibilities
     if token = symbol_t and identifiers( token ).value.all = "(" then
        resumeScanning( cmdStart );
        ParseDelete;                              -- delete file
     else
        discardUnusedIdentifier( token );
        resumeScanning( cmdStart );
        ParseShellCommand;                        -- SQL delete
     end if;
  elsif Token = delay_t then
     ParseDelay;
  elsif token = pen_set_font_t then                -- Pen.Set_Font
     ParsePenSetFont;
  elsif token = pen_put_t then                     -- Pen.Put
     ParsePenPut;
  elsif Token = else_t then
     err( "else without if" );
  elsif Token = elsif_t then
     err( "elsif without if" );
  elsif Token = with_t then
     err( "with only allowed in declaration section or before main program" );
  elsif Token = use_t then
     err( "use not implemented" );
  elsif Token = task_t then
     err( "tasks not implemented" );
  elsif Token = protected_t then
     err( "protected types not implemented" );
  elsif Token = package_t then
     err( "packages not implemented" );
  elsif Token = raise_t then
     declare
        atSemicolon : aScannerState;
        has_when : boolean;
     begin
        ParseRaise( has_when );
        if syntax_check and not has_when then
           -- this is a bit slow but it's only during a syntax check
           markScanner( atSemicolon );
           getNextToken; -- skip semicolon
           -- eof_t because a raise might be the last line in a simple script
           if token /= end_t and token /= exception_t and token /= when_t and token /= else_t and token /= elsif_t and token /= eof_t then
             err( "the raise makes this unreachable code" );
          end if;
          resumeScanning( atSemicolon ); -- restore original position
        end if;
     end;
  elsif Token = exit_t then
     if blocks_top = block'first then           -- not complete. should check
         err( "no enclosing loop to exit" );    -- not just for no blocks
     end if;                                    -- but the block type isn't easily checked
     expect( exit_t );
     if token = when_t or token = if_t then     -- if to give "expected when"
        ParseWhenClause( must_exit );
     else
        -- check for unreachable code on a stand-alone exit
        if syntax_check then
           if token = symbol_t and identifiers( token ).value.all = ";" then
              -- we need to advance one to hilight the right token
              getNextToken;
              -- these are block ending tokens
              if token /= eof_t and token /= end_t and token /= elsif_t and
                 token /= else_t and token /= when_t and token /= others_t and
                 token /= exception_t then
                 err( "the exit makes this unreachable code" );
              end if;
              resumeScanning( cmdStart ); -- restore original position
              getNextToken;
           end if;
        end if;
        must_exit := true;
     end if;
     if isExecutingCommand and must_exit then
        exit_block := true;
        if trace then
           put_trace( "exiting" );
        end if;
     end if;
  elsif Token = declare_t then
     ParseDeclareBlock;
  elsif Token = begin_t then
     ParseBeginBlock;
  elsif token = word_t then
     resumeScanning( cmdStart );
     ParseShellCommand;
  elsif token = backlit_t then
     err( "unexpected backquote literal" );
  elsif token = procedure_t then
     err( "declare procedures in declaration sections" );
  elsif token = function_t then
     err( "declare functions in declaration sections" );
  elsif Token = eof_t then
     eof_flag := true;
     -- a script could be a single comment without a ;
  elsif Token = symbol_t and identifiers( token ).value.all = "@" then
     err( "unexpected @.  Itself can appear after a command or pragma (and no preceding semi-colon) or in an assignment expression" );
           getNextToken;
  elsif Token = symbol_t and identifiers( token ).value.all = ";" then
     err( "statement expected" );
  elsif not identifiers( Token ).deleted and identifiers( Token ).list then     -- array variable
     resumeScanning( cmdStart );           -- assume array assignment
     ParseAssignment;                      -- looks like a AdaScript command
     itself_type := new_t;                 -- except for token type...
  elsif not identifiers( Token ).deleted and identifiers( token ).class = userProcClass then
     DoUserDefinedProcedure( identifiers( token ).value.all );
  else

     -- we need to check the next token then back up
     -- should really change scanner to double symbol look ahead?

     getNextToken;

     -- declarations

     if Token = symbol_t and
        (to_string( identifiers( token ).value.all ) = ":" or
        to_string( identifiers( token ).value.all ) = ",") then
        resumeScanning( cmdStart );
        err( "variable declarations not allowed in executable statements" );
        --ParseVarDeclaration;
     else

        -- assignments
        --
        -- for =, will be treated as a command if we don't force an error
        -- here for missing :=, since it was probably intended as an assignment

        if Token = word_t and to_string( identifiers( token ).value.all ) = "=" then
           expect( symbol_t, ":=" );
        elsif Token = symbol_t and to_string( identifiers( token ).value.all ) = ":=" then
           resumeScanning( cmdStart );
           ParseAssignment;
           itself_type := new_t;

        -- Boolean true shortcut (boolean assertions)
        -- new_t check because a command will produce an varClass with no type
        elsif identifiers( startToken ).class = varClass and then identifiers( startToken ).kind /= new_t and then getBaseType( identifiers( startToken ).kind ) = boolean_t and then not identifiers( startToken ).deleted then
           if onlyAda95 then
              err( "use " & optional_yellow( ":= true " ) & " with " & optional_yellow( "pragma ada_95" ) );
           end if;
           if syntax_check and then not error_found then
              identifiers( startToken ).wasWritten := true;
              identifiers( startToken ).wasReferenced := true;
           end if;
           if isExecutingCommand then
              -- Run-time side-effects tracking and test
              -- Test for two or more "threads" writing to one unprotected variable
              checkExpressionFactorVolatilityOnWrite( startToken );
              checkDoubleThreadWrite( startToken );
              identifiers( startToken ).value.all := to_unbounded_string( "1" );
           end if;
           --if Token = symbol_t and to_string( identifiers( token ).value ) = ";" then
           --end if;
        else

           -- assume it's a shell command and run it
           -- current token is first "token" of parameter.  Blow it away
           -- if able (ie. "ls file", current token is file but we don't
           -- need that in our identifier list.)

           discardUnusedIdentifier( token );
           resumeScanning( cmdStart );
           ParseShellCommand;
        end if;
     end if;
  end if;

  if not eof_flag then
     -- itself?
     -- procedure with no parameters can be interpreted as shell words by the
     -- compiler because it doesn't know if something is a procedure or an
     -- external command at compile time.
     if ( token = symbol_t or token = word_t ) and identifiers( token ).value.all = "@" then
        if onlyAda95 then
           err( "@ is not allowed with " & optional_yellow( "pragma ada_95" ) );
           -- move to next token or inifinite loop if done = true
           getNextToken;
        elsif itself_type = new_t then
           err( "@ is not defined" );
           getNextToken;
        -- shell commands have no class so we can't do this (without
        -- changes, anyway...)
        --elsif class_ok( itself_type, procClass ) then -- lift this?
        else
           token := itself_type;
           -- question is a special case.  See ParseQuestion call above.
           if itself_question then
              itself_question := false;
              identifiers( token ).value.all := to_unbounded_string( "?" );
           end if;
           if identifiers( token ).class = varClass then
              -- not a procedure or keyword? restore value
              identifiers( token ).value.all := itself;
           end if;
        end if;
     else
        itself_type := new_t;
        expectStatementSemicolon( context => startToken );
     end if;
  end if;

  -- breakout handling
  --
  -- Breakout to a prompt if there was an error and --break is used.
  -- Don't break out if syntax checking or the error was caused while
  -- in the break out command prompt.

  if error_found and then boolean(breakoutOpt) then
     if not syntax_check and inputMode /= breakout then
     declare                                          -- we need to save
        saveMode    : constant anInputMode := inputMode; -- Spar's state
        scriptState : aScriptState;                   -- current script
     begin
        --BREAKDBG: 2
        put_line( standard_error, fullErrorMessage );
        wasSIGINT := false;                            -- clear sig flag
        saveScript( scriptState );                     -- save position
        error_found := false;                          -- not a real error
        script := null;                                -- no script to run
        inputMode := breakout;                         -- now interactive
        type_checks_done := false;                     -- enforce type checking
        interactiveSession;                            -- command prompt
        restoreScript( scriptState );                  -- restore original script
        if breakoutContinue then                       -- continuing execution?
           resumeScanning( cmdStart );                 -- start of command
           --BREAKDBG
           put_line( standard_error, get_script_execution_position(
              optional_inverse( "resuming here" ) ) ); -- redisplay line
           --err( optional_inverse( "resuming here" ) ); -- redisplay line
           done := false;                              --   clear logout flag
           error_found := false;                       -- not a real error
           exit_block := false;                        --   and don't exit
           syntax_check := false;
           breakoutContinue := false;                  --   we handled it
           -- Type checks would have been re-instated for the command prompt.
           -- Disable again now.
           type_checks_done := true;
        end if;
        inputMode := saveMode;                         -- restore Spar's
        resumeScanning( cmdStart );                    --   overwrite EOF token
     end;
     end if;
  end if;
exception when symbol_table_overflow =>
  err( optional_inverse( "too many identifiers (symbol table overflow)" ) );
  token := eof_t; -- this exception cannot be handled
  done := true;   -- abort
when block_table_overflow =>
  err( optional_inverse( "too many nested statements/blocks (block table overflow)" ) );
  token := eof_t; -- this exception cannot be handled
  done := true;
end ParseExecutableStatement;


-----------------------------------------------------------------------------
--  PARSE GENERAL STATEMENT
--
-- This procedure, for unstructured scripts, is used when any excutable or
-- declaration statement is expected.  (such as if, for, shell commands,
-- procedure calls, etc.  It also handles Control-C, itself, breakout
-- stepping, return, exit, @ chaining and other common functions.
--
-- This differs from ParseExecutableStatement in that declarations are
-- allowed.
-----------------------------------------------------------------------------

procedure ParseGeneralStatement is
  -- Syntax: env-cmd | clear-cmd | ...
  cmdStart   : aScannerState;
  must_exit  : boolean;
  eof_flag   : boolean := false;
  term_id    : identifier;
  startToken : identifier;
  itself_question : boolean;
begin

  -- mark start of line (prior to breakout test which will change token
  -- to eof )

  startToken := Token;
  markScanner( cmdStart );

  -- interrupt handling

  if stepFlag1 then
     stepFlag1 := false;
     stepFlag2 := true;
  elsif stepFlag2 then
     stepFlag2 := false;
     wasSIGINT := true;
  end if;
  if wasSIGINT then                                      -- control-c?
     if inputMode = interactive or inputMode = breakout then -- interactive?
        wasSIGINT := false;                              -- just ignore
     elsif not breakoutOpt then                          -- no breakouts?
        wasSIGINT := false;                              -- clear flag
        DoQuit;                                          -- stop BUSH
     else                                                -- running script?
        for i in 1..identifiers_top-1 loop
            if identifiers( i ).inspect then
               Put_Identifier( i );
            end if;
        end loop;
        --BREAKDBG
        put_line( standard_error, get_script_execution_position(
            optional_inverse( "Break: return to continue, logout to quit" ) ) ); -- show stop posn
        error_found := true;
        --err( optional_inverse( "Break: return to continue, logout to quit" ) ); -- show stop posn
     end if;
  elsif wasSIGWINCH then                                 -- window change?
     findIdent( to_unbounded_string( "TERM" ), term_id );
     checkDisplay( identifiers( term_id ).value.all );       -- adjust size
     wasSIGWINCH := false;
  end if;

  -- Parse the general statement
  --
  -- built-in?

  itself := identifiers( token ).value.all;
  itself_type := token;
  itself_question := false;

 --put( "PGS: " ); -- DEBUG
 --put_token; -- DEBUG

  if Token = command_t then
     err( "Bourne shell command command not implemented" );
     --getNextToken;
     --ParseShellCommand;
  elsif identifiers( token ).procCB /= null then  -- built-in proc w/cb?
     identifiers( token ).procCB.all;             -- call the callback
  elsif Token = typeset_t then
     ParseTypeSet;
  elsif Token = pragma_t then
     ParsePragma;
  elsif Token = type_t then
     ParseType;
  elsif Token = null_t then
     getNextToken;
  elsif Token = subtype_t then
     ParseSubtype;
  elsif Token = if_t then
     ParseIfBlock;
  elsif Token = case_t then
     ParseCaseBlock;
  elsif Token = while_t then
     ParseWhileBlock;
  elsif Token = for_t then
     ParseForBlock;
  elsif Token = loop_t then
     ParseLoopBlock;
  elsif Token = return_t then
     ParseReturn;
     return;
  elsif Token = step_t then
     ParseStep;
  elsif token = logout_t then
     --if not isLoginShell and inputMode /= interactive and inputMode /= breakout then
     -- ^--not as restrictive
     if not isLoginShell and inputMode /= breakout then
        err( "warning: this is not a login shell: use " & optional_yellow( "return" ) &
             " to quit" );
     end if;
     getNextToken;
     expectStatementSemicolon( context => logout_t );
     if not error_found or inputMode = breakout then
        DoQuit;
     end if;
     return;
  elsif Token = create_t then
     ParseOpen( create => true );
  elsif Token = open_t then
     ParseOpen;
  --elsif Token = close_t then
  --   ParseClose;
  --elsif Token = put_line_t then
  --   ParsePutLine;
  elsif token = symbol_t and identifiers( token ).value.all = "?" then
     -- To implement "itself" with the "?" is difficult because
     -- "?" is a symbol and "@" is a symbol, so trying to look
     -- up the symbol value to determine if is "?" is not possible.
     -- So we flag this as a special case.
     itself_question := true;
     ParseQuestion;
  elsif Token = reset_t then
     ParseReset;
  elsif Token = delete_t then                     -- special case
     getNextToken;                                -- with possibilities
     if token = symbol_t and identifiers( token ).value.all = "(" then
        resumeScanning( cmdStart );
        ParseDelete;                              -- delete file
     else
        discardUnusedIdentifier( token );
        resumeScanning( cmdStart );
        ParseShellCommand;                        -- SQL delete
     end if;
  elsif Token = delay_t then
     ParseDelay;
  elsif token = pen_set_font_t then                -- Pen.Set_Font
     ParsePenSetFont;
  elsif token = pen_put_t then                     -- Pen.Put
     ParsePenPut;
  elsif Token = else_t then
     err( "else without if" );
  elsif Token = elsif_t then
     err( "elsif without if" );
  elsif Token = with_t then
     err( "with only allowed in declaration section or before main program" );
  elsif Token = use_t then
     err( "use not implemented" );
  elsif Token = task_t then
     err( "tasks not implemented" );
  elsif Token = protected_t then
     err( "protected types not implemented" );
  elsif Token = package_t then
     err( "packages not implemented" );
  elsif Token = raise_t then
     declare
        atSemicolon : aScannerState;
        has_when : boolean;
     begin
        ParseRaise( has_when );
        if syntax_check and not has_when then
           -- this is a bit slow but it's only during a syntax check
           markScanner( atSemicolon );
           getNextToken; -- skip semicolon
           -- eof_t because a raise might be the last line in a simple script
           if token /= end_t and token /= exception_t and token /= when_t and token /= else_t and token /= elsif_t and token /= eof_t then
             err( "the raise makes this unreachable code" );
          end if;
          resumeScanning( atSemicolon ); -- restore original position
        end if;
     end;
  elsif Token = exit_t then
     if blocks_top = block'first then           -- not complete. should check
         err( "no enclosing loop to exit" );    -- not just for no blocks
     end if;                                    -- but the block type isn't easily checked
     expect( exit_t );
     if token = when_t or token = if_t then     -- if to give "expected when"
        ParseWhenClause( must_exit );
     else
        -- check for unreachable code on a stand-alone exit
        if syntax_check then
           if token = symbol_t and identifiers( token ).value.all = ";" then
              -- we need to advance one to hilight the right token
              getNextToken;
              -- these are block ending tokens
              if token /= eof_t and token /= end_t and token /= elsif_t and
                 token /= else_t and token /= when_t and token /= others_t and
                 token /= exception_t then
                 err( "the exit makes this unreachable code" );
              end if;
              resumeScanning( cmdStart ); -- restore original position
              getNextToken;
           end if;
        end if;
        must_exit := true;
     end if;
     if isExecutingCommand and must_exit then
        exit_block := true;
        if trace then
           put_trace( "exiting" );
        end if;
     end if;
  elsif Token = declare_t then
     ParseDeclareBlock;
  elsif Token = begin_t then
     ParseBeginBlock;
  elsif token = word_t then
     -- discardUnusedIdentifier( token );
     resumeScanning( cmdStart );
     ParseShellCommand;
  elsif token = backlit_t then
     err( "unexpected backquote literal" );
  elsif token = procedure_t then
     err( "declare procedures in declaration sections" );
  elsif token = function_t then
     err( "declare functions in declaration sections" );
  elsif Token = eof_t then
     eof_flag := true;
     -- a script could be a single comment without a ;
  elsif Token = symbol_t and identifiers( token ).value.all = "@" then
     err( "unexpected @.  Itself can appear after a command or pragma (and no preceding semi-colon) or in an assignment expression" );
           getNextToken;
  elsif Token = symbol_t and identifiers( token ).value.all = ";" then
     err( "statement expected" );
  elsif not identifiers( Token ).deleted and identifiers( Token ).list then     -- array variable
     resumeScanning( cmdStart );           -- assume array assignment
     ParseAssignment;                      -- looks like a AdaScript command
     itself_type := new_t;                 -- except for token type...
  elsif not identifiers( Token ).deleted and identifiers( token ).class = userProcClass then
     DoUserDefinedProcedure( identifiers( token ).value.all );
  else

     -- we need to check the next token then back up
     -- should really change scanner to double symbol look ahead?

     getNextToken;

     -- declarations

     if Token = symbol_t and
        (to_string( identifiers( token ).value.all ) = ":" or
        to_string( identifiers( token ).value.all ) = ",") then
        resumeScanning( cmdStart );
        ParseVarDeclaration;
     else

        -- assignments
        --
        -- for =, will be treated as a command if we don't force an error
        -- here for missing :=, since it was probably intended as an assignment
        -- For unstructured scripts, allow variables to be auto-declared.

        if Token = word_t and to_string( identifiers( token ).value.all ) = "=" then
           expect( symbol_t, ":=" );
        elsif Token = symbol_t and to_string( identifiers( token ).value.all ) = ":=" then
           resumeScanning( cmdStart );
           ParseAssignment( autoDeclareAllowed => (scriptType = unstructured) );
           itself_type := new_t;

        -- Boolean true shortcut (boolean assertions)
        -- new_t check because a command will produce an varClass with no type
        elsif identifiers( startToken ).class = varClass and then identifiers( startToken ).kind /= new_t and then getBaseType( identifiers( startToken ).kind ) = boolean_t and then not identifiers( startToken ).deleted then
           if onlyAda95 then
              err( "use " & optional_yellow( ":= true " ) & " with " & optional_yellow( "pragma ada_95" ) );
           end if;
           if syntax_check and then not error_found then
              identifiers( startToken ).wasWritten := true;
              identifiers( startToken ).wasReferenced := true;
           end if;
           if isExecutingCommand then
              -- Run-time side-effects tracking and test
              -- Test for two or more "threads" writing to one unprotected variable
              checkDoubleThreadWrite( startToken );
              identifiers( startToken ).value.all := to_unbounded_string( "1" );
           end if;
           --if Token = symbol_t and to_string( identifiers( token ).value ) = ";" then
           --end if;
        else

           -- assume it's a shell command and run it
           -- current token is first "token" of parameter.  Blow it away
           -- if able (ie. "ls file", current token is file but we don't
           -- need that in our identifier list.)

           discardUnusedIdentifier( token );
           resumeScanning( cmdStart );
           ParseShellCommand;
        end if;
     end if;
  end if;

  if not eof_flag then
     -- itself?
     -- procedure with no parameters can be interpreted as shell words by the
     -- compiler because it doesn't know if something is a procedure or an
     -- external command at compile time.
     if ( token = symbol_t or token = word_t ) and identifiers( token ).value.all = "@" then
        if onlyAda95 then
           err( "@ is not allowed with " & optional_yellow( "pragma ada_95" ) );
           -- move to next token or inifinite loop if done = true
           getNextToken;
        elsif itself_type = new_t then
           err( "@ is not defined" );
           getNextToken;
        -- shell commands have no class so we can't do this (without
        -- changes, anyway...)
        --elsif class_ok( itself_type, procClass ) then -- lift this?
        else
           token := itself_type;
           -- question is a special case.  See ParseQuestion call above.
           if itself_question then
              itself_question := false;
              identifiers( token ).value.all := to_unbounded_string( "?" );
           end if;
           if identifiers( token ).class = varClass then
              -- not a procedure or keyword? restore value
              identifiers( token ).value.all := itself;
           end if;
        end if;
     else
        itself_type := new_t;
        expectStatementSemicolon( context => startToken );
     end if;
  end if;

  -- breakout handling
  --
  -- Breakout to a prompt if there was an error and --break is used.
  -- Don't break out if syntax checking or the error was caused while
  -- in the break out command prompt.

  if error_found and then boolean(breakoutOpt) then
     if not syntax_check and inputMode /= breakout then
     declare                                          -- we need to save
        saveMode    : constant anInputMode := inputMode; -- Spar's state
        scriptState : aScriptState;                   -- current script
     begin
        --BREAKDBG: 2
        put_line( standard_error, fullErrorMessage );
        wasSIGINT := false;                            -- clear sig flag
        saveScript( scriptState );                     -- save position
        error_found := false;                          -- not a real error
        script := null;                                -- no script to run
        inputMode := breakout;                         -- now interactive
        -- Check: this type_checks_done line was missing
        type_checks_done := false;                     -- enforce type checking
        interactiveSession;                            -- command prompt
        restoreScript( scriptState );                  -- restore original script
        if breakoutContinue then                       -- continuing execution?
           resumeScanning( cmdStart );                 -- start of command
           --BREAKDBG
           --err( optional_inverse( "resuming here" ) ); -- redisplay line
           put_line( standard_error, get_script_execution_position(
              optional_inverse( "resuming here" ) ) ); -- redisplay line
           done := false;                              --   clear logout flag
           error_found := false;                       -- not a real error
           exit_block := false;                        --   and don't exit
           syntax_check := false;
           breakoutContinue := false;                  --   we handled it
           -- Type checks would have been re-instated for the command prompt.
           -- Disable again now.
           type_checks_done := true;
        end if;
        inputMode := saveMode;                         -- restore BUSH's
        resumeScanning( cmdStart );                    --   overwrite EOF token
     end;
     end if;
  end if;
exception when symbol_table_overflow =>
  err( optional_inverse( "too many identifiers (symbol table overflow)" ) );
  token := eof_t; -- this exception cannot be handled
  done := true;   -- abort
when block_table_overflow =>
  err( optional_inverse( "too many nested statements/blocks (block table overflow)" ) );
  token := eof_t; -- this exception cannot be handled
  done := true;
end ParseGeneralStatement;


------------------------------------------------------------------------------
-- PARSE POLICY
--
-- Parse a policy block.  This contains pragmas, static if, static case or
-- null.
-- Ada: N/A
-- Syntax: policy p is .. end c;
------------------------------------------------------------------------------

procedure parsePolicy is
  policy_id : identifier;
begin
  expect( policy_t );
  ParseNewIdentifier( policy_id );
  if identifiers( policy_id ).kind = new_t then
     identifiers( policy_id ).class := policyClass;
     identifiers( policy_id ).kind := policy_t;
  end if;
  expect( is_t );
  while (not error_found and not done) loop
     if token = pragma_t then
        ParsePragma;
        expectStatementSemicolon( context => pragma_t );
     elsif token = if_t then
        ParseStaticIfBlock;
        expectStatementSemicolon( context => if_t );
     elsif token = case_t then
        ParseStaticCaseBlock;
        expectStatementSemicolon( context => case_t );
     elsif token = null_t then
        expect( null_t );
        expectStatementSemicolon( context => null_t );
     else
        exit;
     end if;
  end loop;
  expect( end_t );
  expect( policy_id );
end parsePolicy;


------------------------------------------------------------------------------
-- PARSE CONFIG
--
-- Parse a configuration block.  The block contains declarations only.
-- Ada: N/A
-- Syntax: configuration c is .. end c;
------------------------------------------------------------------------------

procedure parseConfig is
  config_id : identifier;
begin
  expect( configuration_t );
  ParseNewIdentifier( config_id );
  if identifiers( config_id ).kind = new_t then
     identifiers( config_id ).class := configurationClass;
     identifiers( config_id ).kind := configuration_t;
  end if;
  expect( is_t );
  if not error_found and not done then -- this if is probably redundant
     ParseDeclarations;
  end if;
  expect( end_t );
  expect( config_id );
end parseConfig;


------------------------------------------------------------------------------
-- PARSE MAIN PROGRAM
--
-- Parse the main program of a formal, structured script.
------------------------------------------------------------------------------

procedure ParseMainProgram is
  program_id : identifier;
begin
  scriptType := structured;
  expect( procedure_t );
  ParseProgramName( program_id );
  pushBlock( newScope => true,
    newName => to_string (identifiers( program_id ).name ) );
  -- Note: pushBlock must be before "is" (single symbol look-ahead)
  expect( is_t );
  ParseDeclarations;
  expect( begin_t );
  ParseBlock;

  if token = exception_t then
     ParseExceptionHandler( false );
  end if;
  -- If we are processing a web template, we can't pull the block because it
  -- would destroy the global variables used by the template.
  if not hasTemplate then
     pullBlock;
  end if;
  expect( end_t );
  expect( program_id );
end ParseMainProgram;


------------------------------------------------------------------------------
-- PARSE
--
-- Initiate parsing a compiled set of AdaScript commands.  The commands should
-- have been compiled by interpretCommands or interpretScript.  This subprogram
-- doesn't compile byte code.  error_found will be true if the commands failed
-- because of errors.
------------------------------------------------------------------------------

procedure parse is
begin
  -- type_checks_done flag determines whether type checking is required.  It
  -- only needs to be done during syntax checking phase or if there is
  -- no syntax checking phase (such as an interactive prompt).  Once
  -- types are checked, it is not necessary to check them again.
  type_checks_done := not (inputMode = interactive) and not syntax_check;

  if not error_found then
     cmdpos := firstScriptCommandOffset;
     token := identifiers'first;                -- dummy, replaced by g_n_t
     getNextToken;                              -- load first token

     -- Expect some actual source code, at least token, for running.
     -- Normally, SparForte will not load a script that is empty.  However,
     -- eof can happen on edge cases like a script with newlines only.
     -- (A file with spaces will trigger a whitespace at end of line error
     -- during compilation and won't get this far.)

     if token = eof_t then
        err( "there were no commands to run" );
     elsif token = separate_t then
        err( "this is a " &
             optional_yellow( "separate file" ) &
             " not a runnable " &
             optional_yellow( "script" ) );
     end if;

     -- Begin by treating a script as unstructured.  We only know it's
     -- structured when a main program is encountered.

     scriptType := unstructured;

     -- Prior to a main program (or a simple script), a script may have
     -- pragmas, policies, configurations, withs, trace.  A procedure
     -- is a main program.  Otherwise, skip all this and drop down to the
     -- simple script handling.

     while (not error_found and not done) and (
       token = procedure_t or
       token = pragma_t or
       token = policy_t or
       token = with_t or
       token = configuration_t or
       token = trace_t ) loop
        if token = pragma_t then
           ParsePragma;
           expectStatementSemicolon( context => pragma_t );
        elsif token = policy_t then
           ParsePolicy;
           expectStatementSemicolon( context => policy_t );
        elsif token = configuration_t then
           ParseConfig;
           expectStatementSemicolon( context => configuration_t );
        elsif token = procedure_t then
           ParseMainProgram;
           expectStatementSemicolon( context => procedure_t );
           expect( eof_t );                        -- should be nothing else
           exit;
        elsif token = with_t then                  -- with before main pgm
           -- load the include file, parse the header
           ParseWith;
        elsif token = trace_t then
           ParseShellCommand;
           expectStatementSemicolon( context => trace_t );
        end if;
     end loop;

     -- If we're not done, then there's no main program and it's a simple
     -- script with general statements.
     --
     -- If there was a main program, we should be at EOF so this section
     -- should not run

     -- TODO: if there was a main program, none of this is done
     -- so I should clean this up to make that obvious.  This also allows
     -- simple scripts to follow the main program??
     --if not done or token = eof_t then
     if not done and token /= eof_t then
        loop
          ParseGeneralStatement;                   -- process the first statement
          exit when done or token = eof_t;         -- continue until done
        end loop;                                  --  or eof hit
        if not done then                           -- not exiting?
           expect( eof_t );                        -- should be nothing else
        end if;
        -- no blocks to pull, but we can still check requirements
        if syntax_check then
           checkIdentifiersForSimpleScripts;
           completeSoftwareModelRequirements;
        end if;
     end if;
  end if;
end parse;


------------------------------------------------------------------------------
-- PARSE NEW COMMANDS
--
-- Switch to a new set of commands.  This is used for user-defined procedures,
-- functions and back quoted commands.  It is up to the caller to restore the
-- scanner state.
------------------------------------------------------------------------------

procedure parseNewCommands( scriptState : out aScriptState; byteCode : unbounded_string; fragment : boolean := true ) is
begin
  saveScript( scriptState );                -- save current script
  if fragment then                          -- a fragment of byte code?
     replaceScriptWithFragment( byteCode ); -- install proc as script
  else                                      -- otherwise a complete script?
     replaceScript( byteCode );             -- install proc as script
  end if;
  --put_line( toEscaped( to_unbounded_string( script.all ) ) ); -- DEBUG
  inputMode := fromScriptFile;             -- running a script
  error_found := false;                    -- no error found
  exit_block := false;                     -- not exit-ing a block
  cmdpos := firstScriptCommandOffset;      -- start at first char
  token := identifiers'first;              -- dummy, replaced by g_n_t
  getNextToken;                            -- load first token
end parseNewCommands;


---------------------------------------------------------
-- END OF ADASCRIPT PARSER
---------------------------------------------------------

end parser.decl.as;
