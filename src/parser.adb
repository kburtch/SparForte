------------------------------------------------------------------------------
-- AdaScript Language Parser                                                --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------
pragma ada_2005;

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );
with system,
    ada.text_io,
    ada.command_line,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    ada.numerics.float_random,
    ada.calendar,
    gnat.regexp,
    gnat.directory_operations,
    gnat.source_info,
    cgi,
    spar_os.exec,
    string_util,
    user_io,
    user_io.getline,
    script_io,
    performance_monitoring,
    reports.test,
    builtins,
    jobs,
    signal_flags,
    compiler,
    scanner,
    scanner.calendar,
    scanner_res,
    scanner_restypes,
    parser_aux,
    parser.decl,
    parser.decl.as, -- circular dependency for parse general statement, etc.
    parser_params,
    parser_sidefx,
    parser_pragmas,
    parser_tio,
    parser_numerics,
    parser_cal,
    parser_pen,
    interpreter; -- circular relationship for breakout prompt
use ada.text_io,
    ada.command_line,
    ada.strings.unbounded,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    gnat.regexp,
    gnat.directory_operations,
    spar_os,
    spar_os.exec,
    user_io,
    script_io,
    string_util,
    performance_monitoring,
    reports.test,
    builtins,
    jobs,
    signal_flags,
    compiler,
    scanner,
    scanner.calendar,
    scanner_res,
    scanner_restypes,
    parser_aux,
    parser.decl,
    parser.decl.as, -- circular dependency for parse general statement, etc.
    parser_params,
    parser_sidefx,
    parser_pragmas,
    parser_tio,
    parser_numerics,
    parser_cal,
    parser_pen,
    interpreter; -- circular relationship for breakout prompt

package body parser is

-- some string literals converted to unbounded strings for efficiency

lowercase_l : constant unbounded_string := to_unbounded_string( "l" );
uppercase_o : constant unbounded_string := to_unbounded_string( "O" );

-- NON-MEANINGFUL WORDS
--
-- This is a list of vague, ambiguous words that don't make good variable or
-- function names.  Traditional words like "foobar" are deliberately not in
-- this list because they are often used in examples.  "result" is often
-- used in functions so is not included.

nonmeaningful_words : constant unbounded_string := to_unbounded_string( " blah amount asset assets const data func proc equals info input output parm param parms params stuff that thing things this whatever whatnot whatsoever value values variable variables " );

-- CONFUSING PROGRAM WORDS
--
-- These are words that, if used as the name of a program, will result in
-- confusion.  This means names that are also Linux/UNIX commands.  "Test"
-- is especially bad since typing "test" (the Linux command) results in
-- no output, making it look like the program didn't run.

confusingprogram_words : constant unbounded_string := to_unbounded_string( " eval exec read test " );

---------------------------------------------------------
-- START OF ADASCRIPT PARSER
---------------------------------------------------------

procedure ParseBasicShellWord( shell_word : out unbounded_string ) is
  -- Check token for a shell word.  Even though this is called "parse",
  -- don't to getNextToken as the shell word hasn't been expanded yet
  -- and errors have yet to be reported against the token.
begin
  shell_word := null_unbounded_string;
  if identifiers( token ).kind = command_t then   -- handle a command type
      shell_word := identifiers( token ).value.all;
      if syntax_check then
         identifiers( token ).wasReferenced := true;
         --identifiers( token ).referencedByThread := getThreadName;
      end if;
  elsif token = symbol_t then
     shell_word := identifiers( token ).value.all;
  elsif token = word_t then
     -- KB: 19/10/01 - this was disallowed...but I don't remember why.
     --if head( identifiers( token ).value.all, 1 ) = "`" then
     --   err( optional_bold( "shell word" ) & " expected, not a " &
     --     optional_bold( "backquoted literal" ) );
     --else
     --   shell_word := identifiers( token ).value.all;
     --end if;
     shell_word := identifiers( token ).value.all;
  elsif token = cd_t then
     shell_word := identifiers( token ).name;
  elsif token = clear_t then
     shell_word := identifiers( token ).name;
  elsif token = env_t then
     shell_word := identifiers( token ).name;
  elsif token = help_t then
     shell_word := identifiers( token ).name;
  elsif token = history_t then
     shell_word := identifiers( token ).name;
  elsif token = jobs_t then
     shell_word := identifiers( token ).name;
  elsif token = pwd_t then
     shell_word := identifiers( token ).name;
  elsif token = trace_t then
     shell_word := identifiers( token ).name;
  elsif token = unset_t then
     shell_word := identifiers( token ).name;
  elsif token = wait_t then
     shell_word := identifiers( token ).name;
  elsif token = alter_t then
     shell_word := identifiers( token ).name;
  elsif token = delete_t then
     shell_word := identifiers( token ).name;
  elsif token = insert_t then
     shell_word := identifiers( token ).name;
  elsif token = select_t then
     shell_word := identifiers( token ).name;
  elsif token = update_t then
     shell_word := identifiers( token ).name;
  elsif identifiers( token ).kind = new_t then
     shell_word := identifiers( token ).name;
     discardUnusedIdentifier( token );
  elsif token = number_t then
     err( optional_bold( "shell word") & " expected, not a " &
          optional_bold( "number" ) );
  elsif token = strlit_t then
     err( optional_bold( "shell word" ) & " expected, not a " &
          optional_bold( "string literal" ) );
  elsif token = charlit_t then
     err( optional_bold( "shell word" ) & " expected, not a " &
          optional_bold( "character literal" ) );
  -- This should be impossible as a backlit would be scanned as a word token
  --elsif token = backlit_t then
  --   err( optional_bold( "shell word" ) & " expected, not a " &
  --        optional_bold( "backquoted literal" ) );
  elsif token = eof_t then
     err( optional_bold( "shell word" ) & " or semi-colon expected. Possibly hidden by an unescaped '--'?" );
  elsif is_keyword( token ) then
     err( optional_bold( "shell word" ) & " expected, not a " &
          optional_bold( "keyword" ) );
  elsif identifiers( token ).funcCB /= null then
     err( optional_bold( "shell word" ) & " expected, not a " &
           optional_bold( "built-in function" ) );
  elsif identifiers( token ).field_of /= eof_t then
     err( optional_bold( "shell word" ) & " expected, not a " &
          optional_bold( "field of a record type" ) );
  else
     err( "shell word expected" );
  end if;
  --if identifiers( token ).kind /= new_t then
    --else
      -- err( "shell word expected" );
    --end if;
end ParseBasicShellWord;

procedure ParseFieldIdentifier( record_id : identifier; id : out identifier ) is
  -- Expect a new identifier, or one declared in this scope, but
  -- if one from another scope it will need to be redeclared in
  -- this scope.  Use this for record fields that might possibly
  -- be already declared in a different scope.
  --   The problem is that a field variable has a name of "r.f" not "f" as it
  -- appears in the source code.  When testing for existence, we need to
  -- use the full name of the field variable.
  fieldName : unbounded_string;
  temp_id   : identifier;
begin
  id := eof_t; -- dummy
  if identifiers( token ).kind /= new_t then
     if token = number_t then
        err( optional_bold( "identifier") & " expected, not a " &
             optional_bold( "number" ) );
     elsif token = strlit_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "string literal" ) );
     elsif token = charlit_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "character literal" ) );
     elsif token = backlit_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "backquoted literal" ) );
     elsif token = word_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "(shell immediate) word" ) );
     elsif token = eof_t then
        err( optional_bold( "identifier" ) & " expected" );
     elsif is_keyword( token ) then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "keyword" ) );
     elsif token = symbol_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "symbol" ) );
     elsif identifiers( token ).field_of /= eof_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "field of a record type" ) );
     else
        -- an existing token name
        fieldName := identifiers( record_id ).name & "." & identifiers( token ).name;
        findIdent( fieldName, temp_id );
        if temp_id /= eof_t then
           err( "already declared " &
                optional_bold( to_string( fieldName ) ) );
        else                                                     -- declare it
           declareIdent( id, fieldName, new_t, varClass );
        end if;
     end if;
     getNextToken;
  else
     -- a new token
     fieldName := identifiers( record_id ).name & "." & identifiers( token ).name;
     findIdent( fieldName, temp_id );
     if temp_id /= eof_t then
        err( "already declared " &
             optional_bold( to_string( fieldName ) ) );
     else                                                     -- declare it
        discardUnusedIdentifier( token );
        declareIdent( id, fieldName, new_t, varClass );
     end if;
     getNextToken;
  end if;
exception when symbol_table_overflow =>
  err( optional_inverse( "too many identifiers (symbol table overflow)" ) );
  token := eof_t; -- this exception cannot be handled
  done := true;   -- abort
end ParseFieldIdentifier;

procedure ParseProcedureIdentifier( id : out identifier ) is
  -- Expect a new identifier, or one declared in this scope, but
  -- if one from another scope it will need to be redeclared in
  -- this scope.  Use this for procedure names that might possibly
  -- be declared "forward".
  -- Also used for record field variables where the variables may
  -- be declared in a different scope.
begin
  id := eof_t; -- dummy
  if identifiers( token ).kind /= new_t then
     if token = number_t then
        err( optional_bold( "identifier") & " expected, not a " &
             optional_bold( "number" ) );
     elsif token = strlit_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "string literal" ) );
     elsif token = charlit_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "character literal" ) );
     elsif token = backlit_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "backquoted literal" ) );
     elsif token = word_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "(shell immediate) word" ) );
     elsif token = eof_t then
        err( optional_bold( "identifier" ) & " expected" );
     elsif is_keyword( token ) then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "keyword" ) );
     elsif token = symbol_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "symbol" ) );
     elsif identifiers( token ).field_of /= eof_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "field of a record type" ) );
     -- if old, don't redeclare if it was a forward declaration
     elsif identifiers( token ).class = userProcClass then       -- a proc?
        if isLocal( token ) then                                 -- local?
           if length( identifiers( token ).value.all ) = 0 then      -- forward?
              id := token;                                       -- then it's
           else                                                  -- not fwd?
              err( "already declared " &
                   optional_bold( to_string( identifiers( token ).name ) ) );
           end if;                                               -- not local?
        else                                                     -- declare it
           declareIdent( id, identifiers( token ).name, identifiers( token ).kind,
           identifiers( token ).class);
        end if;                                                  -- otherwise
     elsif isLocal( token ) then
        err( "already declared " &
             optional_bold( to_string( identifiers( token ).name ) ) );
     else
        -- create a new one in this scope
        declareIdent( id, identifiers( token ).name, identifiers( token ).kind,
        identifiers( token ).class);
     end if;
     getNextToken;
  else
     id := token;

     -- Procedure / Function style checks

     if length( identifiers(id).name ) < 3 then
        err( optional_bold( "style issue: " & to_string( identifiers(id).name ) ) & ", a procedure/function name, should contain 3 or more characters" );
     end if;

     getNextToken;

  end if;
exception when symbol_table_overflow =>
  err( optional_inverse( "too many identifiers (symbol table overflow)" ) );
  token := eof_t; -- this exception cannot be handled
  done := true;   -- abort
end ParseProcedureIdentifier;

procedure ParseNewIdentifier( id : out identifier ) is
  -- expect a token that is a new, not previously declared identifier
  -- or one previously declared in a different scope that must be re-declared
  -- in this scope
begin
  id := eof_t; -- dummy
  if identifiers( token ).kind /= new_t then
     if token = number_t then
        err( optional_bold( "identifier") & " expected, not a " &
             optional_bold( "number" ) );
     elsif token = strlit_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "string literal" ) );
     elsif token = backlit_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "backquoted literal" ) );
     elsif token = charlit_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "character literal" ) );
     elsif token = word_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "(shell immediate) word" ) );
     elsif token = eof_t then
        err( optional_bold( "identifier" ) & " expected" );
     elsif is_keyword( token ) and token /= eof_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "keyword" ) );
     elsif token = symbol_t then
        err( optional_bold( "identifier" ) & " expected, not a " &
             optional_bold( "symbol" ) );
     elsif isLocal( token ) then
        err( "already declared " &
             optional_bold( to_string( identifiers( token ).name ) ) );
     elsif element( identifiers( token ).name,
         length( identifiers( token ).name ) ) = '_' then
            err( "trailing underscores not allowed in identifiers" );
     else
        -- create a new one in this scope
        declareIdent( id, identifiers( token ).name, new_t, varClass );
     end if;
     getNextToken;
  else
     id := token;
     declare
        nameAsLower : unbounded_string := " " & toLower( identifiers(id).name ) & " ";
     begin
        -- if in a script, prohibit "l" and "O" as identifier names
        if inputMode /= interactive and inputMode /= breakout then
           if identifiers( id ).name = lowercase_l then
              err( "style issue: name lowercase " & optional_bold( "l" ) & " can be confused with the number one" );
           elsif identifiers( id ).name = uppercase_o then
              err( "style issue: name uppercase " & optional_bold( "O" ) & " can be confused with the number zero" );
           end if;
        end if;
        if index( nonmeaningful_words, to_string( nameAsLower ) ) > 0 then
           err( "style issue:  name " & optional_bold( to_string( identifiers(id).name ) ) & " may not be descriptive or meaningful" );
        elsif index( reserved_words, to_string( nameAsLower ) ) > 0 then
            err( "style issue: name " & optional_bold( to_string( identifiers(id).name ) ) & " is similar to a reserved keyword" );
        end if;
     end;
     getNextToken;
  end if;
exception when symbol_table_overflow =>
  err( optional_inverse( "too many identifiers (symbol table overflow)" ) );
  token := eof_t; -- this exception cannot be handled
  done := true;   -- abort
end ParseNewIdentifier;

procedure ParseIdentifier( id : out identifier ) is
  -- expect a  previously declared identifier
  recId : identifier;
begin
  id := eof_t; -- assume failure
  if token = number_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "number" ) );
  elsif token = strlit_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "string literal" ) );
  elsif token = backlit_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "backquoted literal" ) );
  elsif token = charlit_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "character literal" ) );
  elsif token = word_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "(shell immediate) word" ) );
  elsif is_keyword( token ) and token /= eof_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "keyword" ) );
  elsif token = symbol_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "symbol" ) );
  elsif identifiers( token ).kind = new_t or identifiers( token ).deleted then
     -- if we're skipping a block, it doesn't matter if the identifier is
     -- declared, but it does if we're executing a block or checking syntax
     if isExecutingCommand or syntax_check then
        for i in identifiers'first..identifiers_top-1 loop
            if i /= token and not identifiers(i).deleted then
               if typoOf( identifiers(i).name, identifiers(token).name ) then
                  discardUnusedIdentifier( token );
                  err( optional_bold( to_string( identifiers(token).name ) ) &
                  " is a possible typo of " &
                  optional_bold( to_string( identifiers(i).name ) ) );
                  exit;
               end if;
           end if;
       end loop;
       if not error_found then
          -- token will be eof_t if error has already occurred
          discardUnusedIdentifier( token );
          -- help for common mistakes
          -- php/shell - checking for echo/print doesn't work since these
          -- are Linux commands anyway and will be found.  Code removed.
          err( optional_bold( to_string( identifiers( token ).name ) ) & " not declared" );
       end if;
     end if;
     -- this only appears if err in typo loop didn't occur
     --if not error_found then
     --   discardUnusedIdentifier( token );
     --end if;
  else
     if syntax_check and then not error_found then
           -- for a record field, mark the record itself as used.
           -- record fields are ignored in the unused identifier checks.
           -- also do this for writing since it is hard to determine if
           -- all record fields have been written to.
           --
           -- rather than eof, precaution against unexpected values
           recId := identifiers( token ).field_of;
           -- TODO: this could be more efficient
           if recId in reserved_top..identifiers'last then
              identifiers( recId ).wasReferenced := true;
              --identifiers( recId ).referencedByThread := getThreadName;
           else
              -- mark the value as used because it was referred to
              identifiers( token ).wasReferenced := true;
              --identifiers( token ).referencedByThread := getThreadName;
           end if;
     end if;
     id := token;
  end if;
  getNextToken;
exception when symbol_table_overflow =>
  err( optional_inverse( "too many identifiers (symbol table overflow)" ) );
  token := eof_t; -- this exception cannot be handled
  done := true;   -- abort
end ParseIdentifier;

procedure ParseStaticIdentifier( id : out identifier ) is
  -- expect a previously declared static identifier
begin
  id := eof_t; -- assume failure
  if token = number_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "number" ) );
  elsif token = strlit_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "string literal" ) );
  elsif token = backlit_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "backquoted literal" ) );
  elsif token = charlit_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "character literal" ) );
  elsif token = word_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "(shell immediate) word" ) );
  elsif is_keyword( token ) and token /= eof_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "keyword" ) );
  elsif token = symbol_t then
     err( optional_bold( "identifier" ) & " expected, not a " &
          optional_bold( "symbol" ) );
  elsif identifiers( token ).kind = new_t or identifiers( token ).deleted then
     -- if we're skipping a block, it doesn't matter if the identifier is
     -- declared, but it does if we're executing a block or checking syntax
     if isExecutingCommand or syntax_check then
        for i in identifiers'first..identifiers_top-1 loop
            if i /= token and not identifiers(i).deleted then
               if typoOf( identifiers(i).name, identifiers(token).name ) then
                  discardUnusedIdentifier( token );
                  err( optional_bold( to_string( identifiers(token).name ) ) &
                  " is a possible typo of " &
                  optional_bold( to_string( identifiers(i).name ) ) );
                  exit;
               end if;
           end if;
       end loop;
       if not error_found then
          -- token will be eof_t if error has already occurred
          discardUnusedIdentifier( token );
          err( optional_bold( to_string( identifiers( token ).name ) ) & " not declared or is not static" );
       end if;
     end if;
     -- this only appears if err in typo loop didn't occur
     --if not error_found then
     --   discardUnusedIdentifier( token );
     --end if;
  else
     if syntax_check then
           -- for declared but not used checking, assign a value of "REF" to
           -- the value (during syntax check only because value is otherwise
           -- unused).  When blocks are pulled, this will be checked.
           identifiers( token ).wasReferenced := true;
           --identifiers( token ).referencedByThread := getThreadName;
     end if;
     id := token;
  end if;
  getNextToken;
exception when symbol_table_overflow =>
  err( optional_inverse( "too many identifiers (symbol table overflow)" ) );
  token := eof_t; -- this exception cannot be handled
  done := true;   -- abort
end ParseStaticIdentifier;

procedure ParseProgramName( program_id : out identifier ) is
begin
  ParseNewIdentifier( program_id );
  -- style check: no dangerous program names
  if syntax_check then
     if index( confusingprogram_words, to_string( " " & identifiers( program_id ).name & " " ) ) > 0 then
        err( "style issue: " & optional_bold( to_string( identifiers( program_id ).name ) ) & " is a built-in command in some shells" );
     end if;
  end if;
  identifiers( program_id ).kind := identifiers'first;
  identifiers( program_id ).class := mainProgramClass;
  if syntax_check then
     identifiers( program_id ).wasReferenced := true;
     --identifiers( program_id ).referencedByThread := getThreadName;
  end if;
end ParseProgramName;


-- DO CONTRACTS
--
-- Check for and execute a type's affirm clauses.  Use recursion to move to
-- the distant ancenstor type, then return, applying the clauses, ending
-- with the clause on the type itself.  If no clause exists, do nothing.
--
-- kind_id is the data type.
-- expr_val is the value to be tested for that type

procedure DoContracts( kind_id : identifier; expr_val : in out unbounded_string ) is
   type_value_id : identifier;

   -- DO CONTRACT1
   --
   -- The inner recursive procedure.

   procedure DoContract1( kind_id : identifier; expr_val : in out unbounded_string ) is
      scriptState : aScriptState;
   begin
      if identifiers( kind_id ).kind /= variable_t then    -- not uni?
         -- Cannot do DoContract1 because type identifier will change
         -- So switched it to DoContracts
         DoContracts( identifiers( kind_id ).kind, expr_val ); -- parents first
      end if;
      if identifiers( kind_id ).contract /= "" then        -- a contract?
         if trace then                                     -- trace message
            put_trace( to_string( identifiers( kind_id ).name ) & " affirm clause" );
         end if;
         parseNewCommands( scriptState,
           identifiers( kind_id ).contract,
           fragment => true );                           -- setup byte code
         ParseAffirmBlock;
         expectSemicolon;
         if not done then                                  -- not done?
            expect( eof_t );                               -- should be eof
         end if;
         restoreScript( scriptState );            -- restore original script
      end if;
   end DoContract1;

   oldRshOpt : commandLineOption := rshOpt;
begin

   -- Create a new block, declaring the data type variable
   -- We don't need to assign the value until we know we're executing.

   pushBlock( newScope => true, newName => affirm_clause_str, newThread
     => identifiers( kind_id ).name & " affirm" );
   declareIdent( type_value_id, identifiers( kind_id ).name, kind_id );

   -- for now, treat as a restricted shell to reduce the risk of side-effects.
   rshOpt := true;

   if isExecutingCommand then
--put_line( "Starting Contract for value " & to_string( expr_val ) ); -- DEBUG
--put_line( "Kind_id = " & kind_id'img ); -- DEBUG
--put_line( to_string( identifiers( type_value_id ).name ) & " = " & type_value_id'img ); -- DEBUG

      -- Disallow undefined or end_of_file tokens as a precaution.
      -- Otherwise, assign the value to the variable and apply contracts.

      if kind_id /= new_t and kind_id /= eof_t then
         identifiers( type_value_id ).value.all := expr_val;
         DoContract1( kind_id, expr_val );
      end if;

      -- If the validation function altered the data, return the new value
      -- for the data.
      -- We can't relay on wasWritten since it only applies to syntax checks.

--put_trace( "end of contract, value is " & to_string( toEscaped( expr_val ) ) ); -- DEBUG
--put_trace( "type_value is " & to_string( identifiers( type_value_id ).value.all ) ); -- DEBUG
--put_trace( "type_value written " & identifiers( type_value_id ).wasWritten'img ); -- DEBUG
      --if identifiers( type_value_id ).wasWritten then

      -- Copying a value is not so easy for an array
      expr_val := identifiers( type_value_id ).value.all;
      if trace then                                     -- trace message
         put_trace( "value after affirm clause: " & to_string( toEscaped( expr_val ) ) );
      end if;
      --end if;
   end if;

   -- Tear down affirm clause block

   rshOpt := oldRshOpt;
   pullBlock;
end DoContracts;


-----------------------------------------------------------------------------
-- Expressions
-----------------------------------------------------------------------------

-- These don't seem to help much.
--pragma inline( ParsePowerTermOperator );
--pragma inline( ParseTermOperator );
--pragma inline( ParseSimpleExpressionOperator );
--pragma inline( ParseRelationalOperator );
--pragma inline( ParseExpressionOperator );

procedure ParseFactor( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: factor = (expr) | "strlit" | numeric-lit | identifier | built-in fn
  -- if the identifier is volatile, reload the value from the environment
  castType  : identifier;
  array_id  : identifier;
  -- array_id2 : arrayID;
  arrayIndex: long_integer;
  type aUniOp is ( noOp, doPlus, doMinus, doNot );
  uniOp : aUniOp := noOp;
  t : identifier;
  codeFragment : unbounded_string;

  procedure ParseFactorIdentifier is
  begin
    ParseIdentifier( t );
    if identifiers( t ).volatile = checked then    -- volatile user identifier
       err( to_string( identifiers( t ).name ) & " is " & optional_bold( "volatile" ) &
          " and not allowed in expressions because it may cause side-effects" );
       --refreshVolatile( t );
       --f := identifiers( t ).value.all;
       --kind := identifiers( t ).kind;
    end if;
    if identifiers( t ).class = subClass or             -- type cast
       identifiers( t ).class = typeClass then
       -- this will change when arrays can have derived types.
       if identifiers( getBaseType( t ) ).list then
          err( optional_bold( to_string( identifiers( t ).name ) ) & " is an array type" );
       end if;                               -- represent array types
       castType := t;                        -- in expressiosn (yet)
       expect( symbol_t, "(" );
       ParseExpression( f, kind );
       expect( symbol_t, ")" );
       if type_checks_done or else uniTypesOk( castType, kind ) then
          kind := castType;
          if isExecutingCommand then
             f := castToType( f, kind );
             DoContracts( kind, f );
          end if;
       end if;
    elsif identifiers( t ).usage = limitedUsage then
       err( "limited variables cannot be used in an expression (or you may have spelled a subprogram name incorrectly)" );
       kind := eof_t;
    elsif identifiers( getBaseType( t ) ).list then        -- array(index)?
       array_id := t;                            -- array_id=array variable
       expect( symbol_t, "(" );                  -- parse index part
       ParseExpression( f, kind );               -- kind is the index type
       if getUniType( kind ) = uni_string_t or   -- index must be scalar
          getUniType( kind ) = root_record_t or
          identifiers( getBaseType( kind ) ).list then
          err( "array index must be a scalar type" );
       end if;                                   -- variables are not
       if isExecutingCommand then                -- declared in syntax chk
          -- parse factor identifier: arrays
          -- expression side-effect prevention
          checkExpressionFactorVolatility( t );
          arrayIndex := long_integer(to_numeric(f));  -- convert to number
          --array_id2 := arrayID( to_numeric(      -- array_id2=reference
          --   identifiers( array_id ).value ) );  -- to the array table
          --if indexTypeOK( array_id2, kind ) then -- check and access array
          --    if inBounds( array_id2, arrayIndex ) then
          --       f := arrayElement( array_id2, arrayIndex );
          --    end if;
          --end if;
          -- TODO: make a utility function for doing all this.
          -- TODO: probably needs a better error message
          if type_checks_done or else baseTypesOK( identifiers( array_id ).genKind, kind ) then
             if arrayIndex not in identifiers( array_id ).avalue'range then -- DEBUG
                err( "array index " &  to_string( trim( f, ada.strings.both ) ) & " not in" & identifiers( array_id ).avalue'first'img & " .." & identifiers( array_id ).avalue'last'img );
             end if;
          end if;
          if not error_found then
             begin
               f := identifiers( array_id ).avalue( arrayIndex ); -- NEWARRAY
             exception when CONSTRAINT_ERROR =>
               err( gnat.source_info.source_location &
                ": internal error: constraint_error : index out of range " &
                identifiers( array_id ).avalue'first'img & " .." &
                identifiers( array_id ).avalue'last'img );
             when STORAGE_ERROR =>
               err( gnat.source_info.source_location &
                ": internal error : storage error raised in ParseFactor" );
             end;
          end if;
          pushExpressionId( array_id );
       elsif syntax_check then
          identifiers( array_id ).wasFactor := true;
       end if;
       expect( symbol_t, ")" );                  -- element type is k's k
       kind := identifiers( identifiers( array_id ).kind ).kind;
    -- regular variable with an array index?
    else
       if identifiers( t ).field_of /= eof_t then
         if identifiers( identifiers( t ).field_of ).usage = limitedUsage then
            err( "limited record variables cannot be used in an expression" );
         end if;
       end if;
       if token = symbol_t and then identifiers( token ).value.all = "(" then
         err( optional_bold( to_string( identifiers( t ).name ) ) &
             " has an array index but is not an array" );
       end if;
       -- parse factor identifier: scalar or record
       -- expression side-effect prevention
       if not syntax_check then
          checkExpressionFactorVolatility( t );
          if t /= eof_t then
             if identifiers( t ).field_of /= eof_t then
                pushExpressionId( identifiers( t ).field_of );
             else
                pushExpressionId( t );
             end if;
          end if;
       end if;
       f := identifiers( t ).value.all;
       kind := identifiers( t ).kind;
       -- Mark as used as a factor.  if it is a record field, mark the whole
       -- record as used as a factor for limit type testing purposes.
       if syntax_check then
          if t /= eof_t then
             identifiers( t ).wasFactor := true;
             if identifiers( t ).field_of /= eof_t then
                identifiers( identifiers( t ).field_of ).wasFactor := true;
             end if;
          end if;
       end if;
    end if;
  end parseFactorIdentifier;
  pragma inline( parseFactorIdentifier );

begin
  if Token = symbol_t and identifiers( Token ).value.all = "+" then
     uniOp := doPlus;
     getNextToken;
  elsif Token = symbol_t and identifiers( Token ).value.all = "-" then
     uniOp := doMinus;
     getNextToken;
  elsif Token = not_t then
     uniOp := doNot;
     getNextToken;
  end if;
  if Token = symbol_t and then identifiers( Token ).value.all = "(" then
     expect( symbol_t, "(" );
     ParseExpression( f, kind );
     expect( symbol_t, ")" );
  -- to speed things up, these wide if statements break up tokens into
  -- categories.  If the token isn't in the category, skip the rest.
  elsif token < reserved_top then
     if Token = symbol_t and then identifiers( Token ).value.all = "$?" then
        f := to_unbounded_string( last_status'img );
        kind := uni_numeric_t;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "$$" then
        f := to_unbounded_string( aPID'image( getpid ) );
        kind := uni_numeric_t;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "$!" then
        f := to_unbounded_string( aPID'image( lastChild ) );
        kind := uni_numeric_t;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "$#" then
        if onlyAda95 then
           err( "$# not allowed with " & optional_bold( "pragma ada_95" ) &
           " -- use command_line package" );
        end if;
        if isExecutingCommand then
           f := to_unbounded_string( integer'image( Argument_Count-optionOffset) );
        end if;
        kind := uni_numeric_t;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all >= "$1" and then
        identifiers( Token ).value.all <= "$9" then
        -- this could be done a little tighter (ie length check)
        if onlyAda95 then
           err( "$1..$9 not allowed with " & optional_bold( "pragma ada_95" ) &
           " -- use command_line package" );
        end if;
        kind := uni_string_t;
        if isExecutingCommand then
           begin
              f := to_unbounded_string(
                 Argument(
                   integer'value(
                   "" & Element( identifiers( Token ).value.all, 2 ) )+optionOffset ) );
           exception when program_error =>
              err( "program_error exception raised" );
              kind := eof_t;
           when others =>
              err( "no such argument" );
              kind := eof_t;
           end;
        end if;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "$0" then
        if onlyAda95 then
           err( "$0 not allowed with " & optional_bold( "pragma ada_95" ) &
           " -- use command_line package" );
        end if;
        if isExecutingCommand then
           f := to_unbounded_string( Ada.Command_Line.Command_Name );
        end if;
        kind := uni_string_t;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "@" then
        if onlyAda95 then
           err( "@ is not allowed with " & optional_bold( "pragma ada_95" ) );
           f := null_unbounded_string;
           kind := eof_t;
        elsif itself_type = new_t then
           err( "@ is not defined" );
           f := null_unbounded_string;
           kind := eof_t;
        elsif identifiers( itself_type ).class = procClass then
           err( "@ is not a variable" );
        else
           f := itself;
           kind := itself_type;
        end if;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "%" then
        if onlyAda95 then
           err( "% is not allowed with " & optional_bold( "pragma ada_95" ) );
           f := null_unbounded_string;
           kind := eof_t;
        elsif syntax_check then             -- % depends on run-time
           f := to_unbounded_string( "0" ); -- so just use a dummy
           kind := universal_t;             -- typeless value
        else
           if last_output_type = eof_t then
              err( "there has been no output assigned to %" );
           else
              f := last_output;
           end if;
           kind := last_output_type;
        end if;
        getNextToken;
     elsif token = number_t then                           -- numeric literal
        f := identifiers( token ).value.all;
        kind := identifiers( token ).kind;
        getNextToken;
     elsif token = charlit_t then                          -- character literal
        --if length( identifiers( charlit_t ).value ) > 1 then
        --   err( "character literal more than 1 character" );
        --end if;
        f := identifiers( token ).value.all;
        kind := identifiers( token ).kind;
        getNextToken;
     elsif token = strlit_t then                           -- string literal
        f := identifiers( token ).value.all;
        kind := identifiers( token ).kind;
        getNextToken;
     elsif token = backlit_t then           -- `cmds`
        kind := identifiers( token ).kind;
        -- If the backquoted commands don't end with a semi-colon, add one.
        -- There is a chance that the semi-colon could be hidden by a
        -- comment symbol (--).
        codeFragment := identifiers( token ).value.all;
        if tail( codeFragment, 1 ) = " " or
           tail( codeFragment, 1 ) = "" & ASCII.HT then
           err( "trailing whitespace" );
        elsif tail( codeFragment, 1 ) /= ";" then
           codeFragment := codeFragment & ";";
        end if;
        CompileRunAndCaptureOutput( codeFragment, f, getLineNo );
        getNextToken;
     elsif token = abs_t then                             -- abs function
        ParseNumericsAbs( f );
        kind := uni_numeric_t;
     else
        f := null_unbounded_string;                -- (always return something)
        kind := eof_t;
        err( "operand expected" );
     end if;
     -- Another board category, is the token a pre-defined idenifier?
  elsif token < predefined_top then
     if identifiers( token ).funcCB /= null then         -- a built-in function?
        identifiers( token ).funcCB.all( f, kind );        -- run it
     elsif token = is_open_t then                         -- is_open function
        ParseIsOpen( t );
        if isExecutingCommand then
           f := identifiers( t ).value.all;
        end if;
        kind := boolean_t;
     elsif token = source_info_symbol_table_size_t then   -- Symbol_Table_Sz
        getNextToken;
        if onlyAda95 then
           err( "symbol_table_size is not allowed with " &
              optional_bold( "pragma ada_95" ) );
           f := null_unbounded_string;
           kind := eof_t;
        else
          f := delete( to_unbounded_string( identifier'image( identifiers_top-1 )), 1, 1 );
          kind := natural_t;
        end if;
     elsif token = source_info_file_t then                -- source_info.file
        f := basename( getSourceFileName );
        kind := string_t;
        getNextToken;
     elsif token = source_info_line_t then                -- source_info.line
        f := to_unbounded_string( getLineNo'img );
        kind := positive_t;
        getNextToken;
     elsif token = source_info_src_loc_t then      -- source_info.source_loc.
        f := to_unbounded_string( getLineNo'img );
        f := basename( getSourceFileName ) & ":" & f;
        kind := string_t;
        getNextToken;
     elsif token = source_info_enc_ent_t then      -- source_info.enclosing.
        if blocks_top > block'First then
           f := getBlockName( block'First );
        else
           f := to_unbounded_string( "script" );
        end if;
        kind := string_t;
        getNextToken;
     elsif identifiers( token ).procCB /= null then         -- a built-in procedure?
        err( optional_bold( to_string( identifiers( token ).name ) ) &
           " is a built-in procedure not a function" );
     else
        -- System package constants, etc.
        ParseFactorIdentifier;
     end if;
  elsif identifiers( token ).class = userFuncClass then  -- a user function?
     declare
       funcToken : identifier := token;
     begin
       DoUserDefinedFunction( identifiers( funcToken ).value.all, f );
       kind := identifiers( funcToken ).kind;
     end;
  elsif identifiers( token ).kind = keyword_t then      -- no keywords
     f := null_unbounded_string;                        -- (always return something)
     kind := universal_t;
     err( "variable, value or expression expected" );
  else                                                  -- a user ident?
     ParseFactorIdentifier;
  end if;
  case uniOp is
  when noOp => null;
  when doPlus =>
       if type_checks_done or else baseTypesOK( kind, uni_numeric_t ) then
          null;
       end if;
  when doMinus =>
       begin
          if type_checks_done or else baseTypesOK( kind, uni_numeric_t ) then
             if isExecutingCommand then
                f := to_unbounded_string( -to_numeric( f ) );
             end if;
          end if;
       exception when others =>
          err_exception_raised;
       end;
  when doNot =>
       begin
          if type_checks_done or else baseTypesOK( kind, boolean_t ) then
             if isExecutingCommand then
                if to_numeric( f ) = 1.0 then
                   f := to_unbounded_string( "0" );
                else
                   f := to_unbounded_string( "1" );
                end if;
             end if;
          end if;
       exception when others =>
          err_exception_raised;
       end;
  when others =>
      f := null_unbounded_string;                -- (always return something)
      kind := eof_t;
      err( "internal error: unexpected uniary operation error" );
  end case;
end ParseFactor;

procedure ParsePowerTermOperator( op : out unbounded_string ) is
-- Syntax: termop = "**"
begin
  -- token value is checked by parseTerm, but not token name
  if Token /= symbol_t then
     err( "operator expected");
  -- This is checked by parseTerm
  --elsif identifiers( Token ).value.all /= "**" then
  --   err( "** operator expected");
  else
     op := identifiers( token ).value.all;
  end if;
  getNextToken;
end ParsePowerTermOperator;

procedure ParsePowerTerm( term : out unbounded_string; term_type : out identifier ) is
  -- Syntax: term = "factor powerterm-op factor"
  factor1  : unbounded_string;
  factor2  : unbounded_string;
  kind1    : identifier;
  kind2    : identifier;
  operator : unbounded_string;
  operation: identifier;
begin
  ParseFactor( factor1, kind1 );
  term := factor1;
  term_type := kind1;
  while identifiers( Token ).value.all = "**" loop
     ParsePowerTermOperator( operator );
     ParseFactor( factor2, kind2 );
     if type_checks_done or else baseTypesOK( kind1, kind2 ) then
        term_type := getBaseType( kind1 );
        operation := getUniType( kind1 );
        if operation = universal_t then
           operation := getUniType( kind2 );
        end if;
        if operation = universal_t then
           operation := uni_string_t;
        end if;
        if operation = uni_numeric_t then
           if operator = "**" then
              begin
                 if isExecutingCommand then
                    term := to_unbounded_string(
                         to_numeric( term ) **
                         natural( to_numeric( factor2 ) ) );
                 end if;
              exception when program_error =>
                 err( "program_error exception raised" );
                 term := null_unbounded_string;
              when others =>
                 err_exception_raised;
                 term := null_unbounded_string;
              end;
          else
              err( "interal error: unknown power operator" );
          end if;
        else
           err( "operation ** not defined for these types" );
        end if;
     end if;
  end loop;
end ParsePowerTerm;

procedure ParseTermOperator( op : out unbounded_string ) is
  -- Syntax: termop = '*' | '/' | '&'
begin
  -- token value is checked by parseTerm, but not token name
  if Token = mod_t or Token = rem_t then
     op := identifiers( token ).name;
  elsif Token /= symbol_t then
     err( "operator expected");
  elsif identifiers( Token ).value.all /= "*" and identifiers( Token ).value.all /= "/" and identifiers( Token ).value.all /= "&" then
     err( "term operator expected");
  else
     op := identifiers( token ).value.all;
  end if;
  getNextToken;
end ParseTermOperator;

procedure ParseTerm( term : out unbounded_string; term_type : out identifier ) is
  -- Syntax: term = "powerterm term-op powerterm"
  pterm1   : unbounded_string;
  pterm2   : unbounded_string;
  kind1    : identifier;
  kind2    : identifier;
  operator : unbounded_string;
  operation: identifier;
begin
  ParsePowerTerm( pterm1, kind1 );
  term := pterm1;
  term_type := kind1;
  while identifiers( Token ).value.all = "*" or
        identifiers( Token ).value.all = "/" or
        identifiers( Token ).value.all = "&" or
        Token = mod_t or Token = rem_t loop
     ParseTermOperator( operator );
     ParsePowerTerm( pterm2, kind2 );
     term_type := getBaseType( kind2 );
     operation := getUniType( kind2 );
     if operation = universal_t then
        operation := getUniType( kind1 );
     end if;
     if operation = universal_t then
        operation := uni_string_t;
     end if;
     if operation = uni_numeric_t then
        if type_checks_done or else baseTypesOK( kind1, kind2 ) then
             if operator = "*" then
                begin
                  if isExecutingCommand then
                     term := castToType(
                        to_numeric( term ) *
                        to_numeric( pterm2 ),
                     term_type );
                  end if;
                 exception when program_error =>
                    err( "program_error exception raised" );
                    term := null_unbounded_string;
                 when others =>
                    err_exception_raised;
                    term := null_unbounded_string;
                 end;
             elsif operator = "/" then
                declare
                  t : long_float;
                  p : long_float;
                  z : long_float := 0.0;
                begin
                  if isExecutingCommand then
                     -- GCC Ada 4.7.1 doesn't catch divide by zero (returns
                     -- infinity for the following division:
                     -- term := castToType(
                     --   to_numeric( term ) /
                     --   to_numeric( pterm2 ),
                     --term_type );
                     -- As a kludge, we'll break up this function call into
                     -- its parts and explicitly test for division by zero.
                     -- this could likely be improved.
                     t := to_numeric( term );
                     p := to_numeric( pterm2 );
                     if p = 0.0 then
                        err( "division by zero" );
                     else
                        z := t / p;
                        term := castToType( z, term_type );
                     end if;
                  end if;
                exception when program_error =>
                   err( "program_error exception raised" );
                   term := null_unbounded_string;
                when others =>
                   err_exception_raised;
                   term := null_unbounded_string;
                end;
             elsif operator = "mod" then
                begin
                  if isExecutingCommand then
                     term := castToType(
                        --long_long_integer'image(
                        long_float(
                        long_long_integer( to_numeric( term ) ) mod
                        long_long_integer( to_numeric( pterm2 ) ) ),
                     term_type );
                  end if;
                exception when program_error =>
                   err( "program_error exception raised" );
                   term := null_unbounded_string;
                when others =>
                   err_exception_raised;
                   term := null_unbounded_string;
                end;
             elsif operator = "rem" then
                begin
                  if isExecutingCommand then
                     term := castToType(
                        --long_long_integer'image(
                        long_float(
                        long_long_integer( to_numeric( term ) ) rem
                        long_long_integer( to_numeric( pterm2 ) ) ),
                     term_type );
                  end if;
                exception when program_error =>
                   err( "program_error exception raised" );
                   term := null_unbounded_string;
                when others =>
                   err_exception_raised;
                   term := null_unbounded_string;
                end;
             else
                err( gnat.source_info.source_location &
                   ": Internal error: unable to handle term operator" );
             end if;
           end if;
        elsif operation = uni_string_t then
           declare
             base1 : identifier := getBaseType( kind1 );
             base2 : identifier := getBaseType( kind2 );
             uni1  : identifier := getUniType( kind1 );
             uni2  : identifier := getUniType( kind2 );
           begin
              if operator = "&" then
                 if base1 = character_t and base2 = character_t then
                    kind1 := string_t;
                    kind2 := string_t;
                    term_type := string_t;
                 elsif base1 = character_t and uni2 = uni_string_t then
                    kind1 := kind2;
                 elsif uni1 = uni_string_t and base2 = character_t then
                    kind2 := kind1;
                    term_type := kind1;
                 end if;
                 if type_checks_done or else baseTypesOK( kind1, kind2 ) then
                    if isExecutingCommand then
                       term := term & pterm2;
                    end if;
                 end if;
              elsif operator = "*" then
                 if type_checks_done or else baseTypesOK( kind1, natural_t ) then
                    if type_checks_done or else baseTypesOK( kind2, uni_string_t ) then
                       if isExecutingCommand then
                          term := natural( to_numeric( term ) ) * pterm2;
                       end if;
                    end if;
                 end if;
              else
                 err( "operation not defined for string types" );
              end if;
           exception when program_error =>
              err( "program_error exception raised" );
              term := null_unbounded_string;
           when others =>
              err_exception_raised;
              term := null_unbounded_string;
           end;
        else
           if operator = "*" then
              err( "operation * not defined for these types" );
           elsif operator = "/" then
              err( "operation / not defined for these types" );
           elsif operator = "&" then
              err( "operation & not defined for these types" );
           end if;
        end if;
  end loop;
end ParseTerm;

procedure ParseSimpleExpressionOperator( op : out unbounded_string ) is
  -- Syntax: simple-expr-op = '+' | '-'
begin
  -- token value is checked by parseTerm, but not token name
  if Token /= symbol_t then
     err( "operator expected");
  elsif identifiers( Token ).value.all /= "+" and identifiers( Token ).value.all /= "-" then
     err( "simple expression operator expected");
  end if;
  op := identifiers( token ).value.all;
  getNextToken;
end ParseSimpleExpressionOperator;

procedure ParseSimpleExpression( se : out unbounded_string; expr_type : out identifier ) is
  -- Syntax: term = "term expr-op term"
  term1    : unbounded_string;
  term2    : unbounded_string;
  kind1    : identifier;
  kind2    : identifier;
  operator : unbounded_string;
  operation: identifier;
  typesOK  : boolean := false;
begin
  ParseTerm( term1, kind1 );
  se := term1;
  expr_type := kind1;
  while identifiers( Token ).value.all = "+" or identifiers( Token ).value.all = "-" loop
     ParseSimpleExpressionOperator( operator );
     ParseTerm( term2, kind2 );
     -- Special exception for + and -...allow time arithmetic
     if (kind1 = cal_time_t) and then (getBaseType(kind2) = duration_t or kind2 = uni_numeric_t or kind2 = universal_t) then
        typesOK := true;
        expr_type := cal_time_t;
        operation := cal_time_t;
     elsif (kind2 = cal_time_t) and then (getBaseType(kind1) = duration_t or kind1 = uni_numeric_t or kind2 = universal_t) then
        typesOK := true;
        expr_type := cal_time_t;
        operation := variable_t; -- representing time on right
     elsif (kind1 = cal_time_t) and then (getBaseType(kind2) = cal_time_t) then
        if operator = "+" then
           typesOK := true;
           expr_type := cal_time_t;
        else
           typesOK := true;
           expr_type := duration_t;
        end if;
        operation := root_record_t; -- representing time on both side
     end if;
     -- Otherwise check the types normally
     if not typesOK then
        typesOK := type_checks_done or else baseTypesOK( kind1, kind2 );
        expr_type := getBaseType( kind1 );
        operation := getUniType( kind1 );
     end if;
     if typesOk then
        if operation = universal_t then
           operation := getUniType( kind2 );
        end if;
        if operation = universal_t then
           operation := uni_string_t;
        end if;
        if operation = uni_numeric_t then
             if operator = "+" then
                begin
                  if isExecutingCommand then
                     se := castToType(
                        to_numeric( se ) +
                        to_numeric( term2 ),
                     expr_type );
                  end if;
                exception when program_error =>
                   err( "program_error exception raised" );
                   se := null_unbounded_string;
                when others =>
                   err_exception_raised;
                   se := null_unbounded_string;
                end;
             elsif operator = "-" then
                begin
                  if isExecutingCommand then
                     se := castToType(
                        to_numeric( se ) -
                        to_numeric( term2 ),
                     expr_type );
                  end if;
                exception when program_error =>
                   err( "program_error exception raised" );
                   se := null_unbounded_string;
                when others =>
                   err_exception_raised;
                   se := null_unbounded_string;
                end;
             end if;
        elsif operation = cal_time_t then -- time +/- duration
           if operator = "+" then
              if isExecutingCommand then
                 declare
                    c : scanner.calendar.time := scanner.calendar.time( to_numeric( se ) );
                 begin
                    c := c + duration( to_numeric( term2 ) );
                    se := to_unbounded_string( long_long_integer'image( long_long_integer( c ) ) );
                 end;
              end if;
           elsif operator = "-" then
              if isExecutingCommand then
                 declare
                    c : scanner.calendar.time := scanner.calendar.time( to_numeric( se ) );
                 begin
                    c := c - duration( to_numeric( term2 ) );
                    se := to_unbounded_string( long_long_integer'image( long_long_integer( c ) ) );
                 end;
              end if;
           end if;
        elsif operation = variable_t then -- duration + time
           if operator = "+" then
              if isExecutingCommand then
                 declare
                    c : scanner.calendar.time := scanner.calendar.time( to_numeric( term2 ) );
                 begin
                    c := duration( to_numeric( se ) ) + c;
                    se := to_unbounded_string( long_long_integer'image( long_long_integer( c ) ) );
                 end;
              end if;
           elsif operator = "-" then
              err( "operation - not defined for these types" );
           end if;
        elsif operation = root_record_t then -- adding times
           if operator = "+" then
              err( "operation + not defined for these types" );
           else
              if isExecutingCommand then
                 declare
                    c : scanner.calendar.time := scanner.calendar.time( to_numeric( se ) );
                    c2: duration;
                 begin
                    c2 := c - scanner.calendar.time( to_numeric( term2 ) );
                    se := to_unbounded_string( duration'image( c2 ) );
                 exception when time_error =>
                    err( "duration value too large or small" );
                 when constraint_error =>
                    err( "constraint error" );
                 end;
              end if;
           end if;
        else
             if operator = "+" then
                err( "operation + not defined for these types" );
             elsif operator = "-" then
                err( "operation - not defined for these types" );
             end if;
        end if;
     end if;
  end loop;
  --put_line( "Simple Expression value = " & to_string( se ) );
end ParseSimpleExpression;

procedure ParseRelationalOperator( op : out unbounded_string ) is
  -- Syntax: rel-op = >, >=, etc.
begin
  -- token value is checked by parseTerm, but not token name
  if Token /= symbol_t and Token /= in_t and Token /= not_t then
     err( "operator expected");
  elsif identifiers( Token ).value.all /= ">=" and
        identifiers( Token ).value.all /= ">" and
        identifiers( Token ).value.all /= "<" and
        identifiers( Token ).value.all /= "<=" and
        identifiers( Token ).value.all /= "=" and
        identifiers( Token ).value.all /= "/=" and
        Token /= in_t and Token /= not_t then
     err( "relational operator expected");
  end if;
  if Token = in_t then
     op := identifiers( token ).name;
  elsif Token = not_t then
     op := to_unbounded_string( "not in" );
     getNextToken;
     if Token /= in_t then
        err( "relational operator expected");
     end if;
  else
     op := identifiers( token ).value.all;
  end if;
  getNextToken;
end ParseRelationalOperator;

procedure ParseRelation( re : out unbounded_string; rel_type : out identifier ) is
  -- Syntax: relation = "simple-expr" =|>|<|... "simple-expr"
  se1      : unbounded_string;
  se2      : unbounded_string;
  se3      : unbounded_string;
  kind1    : identifier;
  kind2    : identifier;
  kind3    : identifier;
  operator : unbounded_string;
  operation: identifier;
  b        : boolean;
begin
  ParseSimpleExpression( se1, kind1 );
  re := se1;
  rel_type := kind1;
  if identifiers( Token ).value.all = ">=" or
        identifiers( Token ).value.all = ">" or
        identifiers( Token ).value.all = "<" or
        identifiers( Token ).value.all = "<=" or
        identifiers( Token ).value.all = "=" or
        identifiers( Token ).value.all = "/=" or
        Token = in_t or Token = not_t then
     rel_type := boolean_t; -- always
     ParseRelationalOperator( operator );
     if operator = "in" or operator = "not in" then
        ParseFactor( se2, kind2 );
        if type_checks_done or else baseTypesOK( kind1, kind2 ) then -- redundant below but
           expect( symbol_t, ".." );        -- keeps error messages nice
           ParseFactor( se3, kind3 );       -- should probably restructure
           if type_checks_done or else baseTypesOK( kind2, kind3 ) then
              null;
           end if;
        end if;
     else
        ParseSimpleExpression( se2, kind2 );
     end if;
     if type_checks_done or else baseTypesOK( kind1, kind2 ) then
        operation := getUniType( kind1 );
        if operation = universal_t then
           operation := getUniType( kind2 );
        end if;
        if operation = universal_t then
           operation := uni_string_t;
        end if;
        if operation = uni_numeric_t or else operation = root_enumerated_t or else operation = cal_time_t then
             begin
               if operator = ">=" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) >= to_numeric( se2 );
                  end if;
               elsif operator = ">" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) > to_numeric( se2 );
                  end if;
               elsif operator = "<" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) < to_numeric( se2 );
                  end if;
               elsif operator = "<=" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) <= to_numeric( se2 );
                  end if;
               elsif operator = "=" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) = to_numeric( se2 );
                  end if;
               elsif operator = "/=" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) /= to_numeric( se2 );
                  end if;
               elsif operator = "in" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) in to_numeric( se2 )..to_numeric( se3 );
                  end if;
               elsif operator = "not in" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) not in to_numeric( se2 )..to_numeric( se3 );
                  end if;
               else
                  err( gnat.source_info.source_location &
                    ": Internal error: couldn't handle relational operator" );
               end if;
               if b then
                  re := to_unbounded_string( "1" );
               else
                  re := to_unbounded_string( "0" );
               end if;
             exception when others =>
               err_exception_raised;
             end;
        elsif operation = uni_string_t then
             if operator = ">=" then
                if isExecutingCommand then
                   b := se1 >= se2;
                end if;
             elsif operator = ">" then
                if isExecutingCommand then
                   b := se1 > se2;
                end if;
             elsif operator = "<" then
                if isExecutingCommand then
                   b := se1 < se2;
                end if;
             elsif operator = "<=" then
                if isExecutingCommand then
                   b := se1 <= se2;
                end if;
             elsif operator = "=" then
                if isExecutingCommand then
                   b := se1 = se2;
                end if;
             elsif operator = "/=" then
                if isExecutingCommand then
                   b := se1 /= se2;
                end if;
             elsif operator = "in" then
                if isExecutingCommand then
                   if length( se1 ) /= 1 or
                      length( se2 ) /= 1 or
                      length( se3 ) /= 1 then
                      err( "scalar type required for range" );
                   else
                      declare
                        c1 : character := element( se1, 1 );
                        c2 : character := element( se2, 1 );
                        c3 : character := element( se3, 1 );
                      begin
                        b := c1 in c2..c3;
                      exception when others =>
                        err_exception_raised;
                      end;
                   end if;
                end if;
             elsif operator = "not in" then
                if isExecutingCommand then
                   if length( se1 ) /= 1 or
                      length( se2 ) /= 1 or
                      length( se3 ) /= 1 then
                      err( "scalar type required for range" );
                   else
                      declare
                        c1 : character := element( se1, 1 );
                        c2 : character := element( se2, 1 );
                        c3 : character := element( se3, 1 );
                      begin
                        b := c1 not in c2..c3;
                      exception when others =>
                        err_exception_raised;
                      end;
                   end if;
                end if;
             else
                err( gnat.source_info.source_location &
                  ": Internal error: couldn't handle relational operator" );
             end if;
             if b then
                re := to_unbounded_string( "1" );
             else
                re := to_unbounded_string( "0" );
             end if;
        else
             err( "relational operation not defined for these types" );
        end if;
     end if;
  end if;
end ParseRelation;

procedure ParseExpressionOperator( op : out identifier ) is
  -- Syntax: expr-op = "and" | "or" | "xor"
begin
  if Token /= and_t and
     Token /= or_t and
     Token /= xor_t then
     err( "boolean operator expected");
  end if;
  op := Token;
  getNextToken;
end ParseExpressionOperator;

procedure ParseExpression( ex : out unbounded_string; expr_type : out identifier ) is
  -- Syntax: expr = "relation" and|or|xor "relation"
  re1      : unbounded_string;
  re2      : unbounded_string;
  kind1    : identifier;
  kind2    : identifier;
  operator : identifier;
  last_op  : identifier := eof_t;
  b        : boolean;
  type bitwise_number is mod 2**64;
  oldExpressionInstruction : line_count := lastExpressionInstruction;
  oldFirstExpressionInstruction : line_count := firstExpressionInstruction;
begin
  -- expression side-effects.  Remember how many lines have run prior to this
  -- expression to determine if variables in the expression were altered
  -- later than this line.  Remember that expressions can be nested.
  -- If not checking side-effects, this will be zer0.
  lastExpressionInstruction := perfStats.lineCnt;
  if firstExpressionInstruction = noExpressionInstruction then
     firstExpressionInstruction := perfStats.lineCnt;
  end if;
  --put_line( "ParseExpression: LEI =" & lastExpressionInstruction'img );
  ParseRelation( re1, kind1 );
  ex := re1;
  expr_type := kind1;
  while Token = and_t or Token = or_t or Token = xor_t loop
     ParseExpressionOperator( operator );
     if onlyAda95 and then last_op /= eof_t and then last_op /= operator then
        err( "mixed boolean operators in expression not allowed with " &
              optional_bold( "pragam ada_95" ) & " - use parantheses" );
     end if;
     ParseRelation( re2, kind2 );
     if type_checks_done or else baseTypesOK( kind1, kind2 ) then
        if getUniType( kind1 ) = uni_numeric_t then
           expr_type := getBaseType( kind1 );
           if operator = and_t then
              begin
                if isExecutingCommand then
                   re1 := to_unbounded_string(
                      long_float(
                      bitwise_number( to_numeric( re1 ) ) and
                      bitwise_number( to_numeric( re2 ) ) ) );
                end if;
              exception when program_error =>
                 err( "program_error exception raised" );
                 re1 := null_unbounded_string;
              when ada.strings.index_error =>
                 err( "variable was not intialized" );
                 re1 := null_unbounded_string;
              when others =>
                 err_exception_raised;
                 re1 := null_unbounded_string;
              end;
           elsif operator = or_t then
              begin
                if isExecutingCommand then
                   re1 := to_unbounded_string(
                      long_float(
                      bitwise_number( to_numeric( re1 ) ) or
                      bitwise_number( to_numeric( re2 ) ) ) );
                end if;
              exception when program_error =>
                 err( "program_error exception raised" );
                 re1 := null_unbounded_string;
              when ada.strings.index_error =>
                 err( "variable was not intialized" );
                 re1 := null_unbounded_string;
              when others =>
                 err_exception_raised;
                 re1 := null_unbounded_string;
              end;
           elsif operator = xor_t then
              begin
                if isExecutingCommand then
                   re1 := to_unbounded_string(
                      long_float(
                      bitwise_number( to_numeric( re1 ) ) xor
                      bitwise_number( to_numeric( re2 ) ) ) );
                end if;
              exception when program_error =>
                 err( "program_error exception raised" );
                 re1 := null_unbounded_string;
              when ada.strings.index_error =>
                 err( "variable was not intialized" );
                 re1 := null_unbounded_string;
              when others =>
                 err_exception_raised;
                 re1 := null_unbounded_string;
              end;
           end if;
        elsif getBaseType( kind1 ) = boolean_t then
           expr_type := getBaseType( kind1 );
           if operator = and_t then
              if isExecutingCommand then
                 b := re1 = "1" and re2 = "1";
              end if;
           elsif operator= or_t then
              if isExecutingCommand then
                 b := re1 = "1" or re2 = "1";
              end if;
           elsif operator = xor_t then
              if isExecutingCommand then
                 b := re1 = "1" xor re2 = "1";
              end if;
           else
              err( gnat.source_info.source_location &
                ": Internal error: unable to handle boolean operator" );
           end if;
           if b then
              re1 := to_unbounded_string( "1" );
           else
              re1 := to_unbounded_string( "0" );
           end if;
        else
           err( "boolean or number expected" );
        end if;
     end if;
     last_op := operator;
  end loop;
  ex := re1;
  -- Must pull before resetting...
  pullExpressionIds;
  -- expression side-effects: we're now whatever the previous expression
  -- instruction was.
  lastExpressionInstruction := oldExpressionInstruction;
  firstExpressionInstruction := oldFirstExpressionInstruction;
  --put_line( "Expression value = " & to_string( ex ) );
end ParseExpression;

-----------------------------------------------------------------------------
-- Static Expressions
-----------------------------------------------------------------------------

procedure ParseStaticFactor( f : out unbounded_string; kind : out identifier ) is
  -- Syntax: factor = (expr) | "strlit" | numeric-lit | identifier | built-in fn
  -- if the identifier is volatile, reload the value from the environment
  castType  : identifier;
  array_id  : identifier;
  -- array_id2 : arrayID;
  arrayIndex: long_integer;
  type aUniOp is ( noOp, doPlus, doMinus, doNot );
  uniOp : aUniOp := noOp;
  t : identifier;
  codeFragment : unbounded_string;
begin
  if Token = symbol_t and identifiers( Token ).value.all = "+" then
     uniOp := doPlus;
     getNextToken;
  elsif Token = symbol_t and identifiers( Token ).value.all = "-" then
     uniOp := doMinus;
     getNextToken;
  elsif Token = not_t then
     uniOp := doNot;
     getNextToken;
  end if;
  if Token = symbol_t and then identifiers( Token ).value.all = "(" then
     expect( symbol_t, "(" );
     ParseStaticExpression( f, kind );
     expect( symbol_t, ")" );
  else
     if Token = symbol_t and then identifiers( Token ).value.all = "$?" then
        f := to_unbounded_string( last_status'img );
        kind := uni_numeric_t;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "$$" then
        f := to_unbounded_string( aPID'image( getpid ) );
        kind := uni_numeric_t;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "$!" then
        f := to_unbounded_string( aPID'image( lastChild ) );
        kind := uni_numeric_t;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "$#" then
        if onlyAda95 then
           err( "$# not allowed with " & optional_bold( "pragma ada_95" ) &
           " -- use command_line package" );
        end if;
        if isExecutingCommand then
           f := to_unbounded_string( integer'image( Argument_Count-optionOffset) );
        end if;
        kind := uni_numeric_t;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all >= "$1" and then
        identifiers( Token ).value.all <= "$9" then
        -- this could be done a little tighter (ie length check)
        if onlyAda95 then
           err( "$1..$9 not allowed with " & optional_bold( "pragma ada_95" ) &
           " -- use command_line package" );
        end if;
        kind := uni_string_t;
        if isExecutingCommand then
           begin
              f := to_unbounded_string(
                 Argument(
                   integer'value(
                   "" & Element( identifiers( Token ).value.all, 2 ) )+optionOffset ) );
           exception when program_error =>
              err( "program_error exception raised" );
              kind := eof_t;
           when others =>
              err( "there are only" & integer'image( Argument_Count-optionOffset) & " arguments" );
              kind := eof_t;
           end;
        end if;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "$0" then
        if onlyAda95 then
           err( "$0 not allowed with " & optional_bold( "pragma ada_95" ) &
           " -- use command_line package" );
        end if;
        if isExecutingCommand then
           f := to_unbounded_string( Ada.Command_Line.Command_Name );
        end if;
        kind := uni_string_t;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "@" then
        if onlyAda95 then
           err( "@ is not allowed with " & optional_bold( "pragma ada_95" ) );
           f := null_unbounded_string;
           kind := eof_t;
        elsif itself_type = new_t then
           err( "@ is not defined" );
           f := null_unbounded_string;
           kind := eof_t;
        elsif identifiers( itself_type ).class = procClass then
           err( "@ is not a variable" );
        else
           f := itself;
           kind := itself_type;
        end if;
        getNextToken;
     elsif Token = symbol_t and then identifiers( Token ).value.all = "%" then
        if onlyAda95 then
           err( "% is not allowed with " & optional_bold( "pragma ada_95" ) );
           f := null_unbounded_string;
           kind := eof_t;
        elsif syntax_check then             -- % depends on run-time
           f := to_unbounded_string( "0" ); -- so just use a dummy
           kind := universal_t;             -- typeless value
        else
           if last_output_type = eof_t then
              err( "there has been no output assigned to %" );
           else
              f := last_output;
           end if;
           kind := last_output_type;
        end if;
        getNextToken;
     elsif token = number_t then                           -- numeric literal
        f := identifiers( token ).value.all;
        kind := identifiers( token ).kind;
        getNextToken;
     elsif token = charlit_t then                          -- character literal
        --if length( identifiers( charlit_t ).value ) > 1 then
        --   err( "character literal more than 1 character" );
        --end if;
        f := identifiers( token ).value.all;
        kind := identifiers( token ).kind;
        getNextToken;
     elsif token = strlit_t then                           -- string literal
        f := identifiers( token ).value.all;
        kind := identifiers( token ).kind;
        getNextToken;
     elsif token = backlit_t then           -- `cmds`
        kind := identifiers( token ).kind;
        -- If the backquoted commands don't end with a semi-colon, add one.
        -- There is a chance that the semi-colon could be hidden by a
        -- comment symbol (--).
        codeFragment := identifiers( token ).value.all;
        if tail( codeFragment, 1 ) /= ";" then
           codeFragment := codeFragment & ";";
        end if;
        CompileRunAndCaptureOutput( codeFragment, f, getLineNo );
        getNextToken;
     -- Static expressions must not run built-in functions because they will
     -- accept regular expressions
     elsif identifiers( token ).funcCB /= null then         -- a callback?
        err( "static expressions cannot call functions" );
        -- identifiers( token ).funcCB.all( f, kind );        -- run it
     elsif token = is_open_t then                         -- is_open function
        err( "static expressions cannot call functions" );
        --ParseIsOpen( t );
        --f := identifiers( t ).value.all;
        kind := boolean_t;
     elsif token = abs_t then                             -- abs function
        err( "static expressions cannot call functions" );
        --ParseNumericsAbs( f );
        kind := uni_numeric_t;
     elsif token = source_info_symbol_table_size_t then   -- Symbol_Table_Sz
        getNextToken;
        if onlyAda95 then
           err( "symbol_table_size is not allowed with pragma ada_95" );
           f := null_unbounded_string;
           kind := eof_t;
        else
          f := delete( to_unbounded_string( identifier'image( identifiers_top-1 )), 1, 1 );
          kind := natural_t;
        end if;
     elsif token = source_info_file_t then                -- source_info.file
        f := basename( getSourceFileName );
        kind := string_t;
        getNextToken;
     elsif token = source_info_line_t then                -- source_info.line
        f := to_unbounded_string( getLineNo'img );
        kind := positive_t;
        getNextToken;
     elsif token = source_info_src_loc_t then      -- source_info.source_loc.
        f := to_unbounded_string( getLineNo'img );
        f := basename( getSourceFileName ) & ":" & f;
        kind := string_t;
        getNextToken;
     elsif token = source_info_enc_ent_t then      -- source_info.enclosing.
        if blocks_top > block'First then
           f := getBlockName( block'First );
        else
           f := to_unbounded_string( "script" );
        end if;
        kind := string_t;
        getNextToken;
     -- Static expressions must not run user functions because they will
     -- accept regular expressions
      elsif identifiers( token ).class = userFuncClass then
        err( "static expressions cannot call functions" );
     --    declare
     --      funcToken : identifier := token;
     --    begin
     --      DoUserDefinedFunction( identifiers( funcToken ).value.all, f );
     --      kind := identifiers( funcToken ).kind;
     --    end;
     elsif identifiers( token ).kind = keyword_t then      -- no keywords
        f := null_unbounded_string;                        -- (always return something)
        kind := universal_t;
        err( "variable, value or expression expected" );
     else                                                  -- some kind of user ident?
        ParseStaticIdentifier( t );
        if identifiers( t ).volatile /= none then  -- volatile user identifier
           refreshVolatile( t );
           f := identifiers( t ).value.all;
           kind := identifiers( t ).kind;
        elsif identifiers( t ).class = subClass or             -- type cast
           identifiers( t ).class = typeClass then
           if getUniType( kind ) = uni_string_t or   -- index must be scalar
              getUniType( kind ) = root_record_t or
              identifiers( getBaseType( kind ) ).list then
              err( "array index must be a scalar type" );
           end if;
           castType := t;                        -- in expressiosn (yet)
           expect( symbol_t, "(" );
           ParseExpression( f, kind );
           expect( symbol_t, ")" );
           if type_checks_done or else uniTypesOk( castType, kind ) then
              kind := castType;
              if isExecutingCommand then
                 --f := castToType( to_numeric( f ), kind );
                 f := castToType( f, kind );
              end if;
           end if;
        elsif identifiers( getBaseType( t ) ).list then        -- array(index)?
           array_id := t;                            -- array_id=array variable
           expect( symbol_t, "(" );                  -- parse index part
           ParseExpression( f, kind );               -- kind is the index type
           if getUniType( kind ) = uni_string_t or   -- index must be scalar
              identifiers( getBaseType( kind ) ).list then
              err( "array index must be a scalar type" );
           end if;                                   -- variables are not
           if isExecutingCommand then                -- declared in syntax chk
              arrayIndex := long_integer(to_numeric(f));  -- convert to number
              --array_id2 := arrayID( to_numeric(      -- array_id2=reference
              --   identifiers( array_id ).value ) );  -- to the array table
              --if indexTypeOK( array_id2, kind ) then -- check and access array
              --    if inBounds( array_id2, arrayIndex ) then
              --       f := arrayElement( array_id2, arrayIndex );
              --    end if;
              --end if;
              -- TODO: make a utility function for doing all this.
              -- TODO: probably needs a better error message
              if type_checks_done or else baseTypesOK( identifiers( array_id ).genKind, kind ) then
                 if arrayIndex not in identifiers( array_id ).avalue'range then -- DEBUG
                    err( "array index " &  to_string( trim( f, ada.strings.both ) ) & " not in" & identifiers( array_id ).avalue'first'img & " .." & identifiers( array_id ).avalue'last'img );
                 end if;
              end if;
              if not error_found then
                 begin
                   f := identifiers( array_id ).avalue( arrayIndex ); -- NEWARRAY
                 exception when CONSTRAINT_ERROR =>
                   err( gnat.source_info.source_location &
                     ": internal error: constraint_error : index out of range " &
                     identifiers( array_id ).avalue'first'img & " .. " & identifiers( array_id ).avalue'last'img );
                 when STORAGE_ERROR =>
                   err( gnat.source_info.source_location &
                     ": internal error : storage error raised in ParseStaticFactor" );
                 end;
              end if;
           end if;
           expect( symbol_t, ")" );                  -- element type is k's k
           kind := identifiers( identifiers( array_id ).kind ).kind;
        -- regular variable with an array index?
        else
          if token = symbol_t and identifiers( token ).value.all = "(" then
             err( optional_bold( to_string( identifiers( t ).name ) ) &
                 " has an array index but is not an array" );
           end if;
           f := identifiers( t ).value.all;
           kind := identifiers( t ).kind;
        end if;
     end if;
  end if;
  case uniOp is
  when noOp => null;
  when doPlus =>
       if type_checks_done or else baseTypesOK( kind, uni_numeric_t ) then
          null;
       end if;
  when doMinus =>
       begin
          if type_checks_done or else baseTypesOK( kind, uni_numeric_t ) then
             if isExecutingCommand then
                f := to_unbounded_string( -to_numeric( f ) );
             end if;
          end if;
       exception when others =>
          err_exception_raised;
       end;
  when doNot =>
       begin
          if type_checks_done or else baseTypesOK( kind, boolean_t ) then
             if isExecutingCommand then
                if to_numeric( f ) = 1.0 then
                   f := to_unbounded_string( "0" );
                else
                   f := to_unbounded_string( "1" );
                end if;
             end if;
          end if;
       exception when others =>
          err_exception_raised;
       end;
  when others =>
      err( "unexpected uniary operation error" );
  end case;
end ParseStaticFactor;

procedure ParseStaticPowerTermOperator( op : out unbounded_string ) is
-- Syntax: termop = "**"
begin
  -- token value is checked by parseTerm, but not token name
  if Token /= symbol_t then
     err( "operator expected");
  -- This is checked by parseTerm
  --elsif identifiers( Token ).value.all /= "**" then
  --   err( "** operator expected");
  else
     op := identifiers( token ).value.all;
  end if;
  getNextToken;
end ParseStaticPowerTermOperator;

procedure ParseStaticPowerTerm( term : out unbounded_string; term_type : out identifier ) is
  -- Syntax: term = "factor powerterm-op factor"
  factor1  : unbounded_string;
  factor2  : unbounded_string;
  kind1    : identifier;
  kind2    : identifier;
  operator : unbounded_string;
  operation: identifier;
begin
  ParseStaticFactor( factor1, kind1 );
  term := factor1;
  term_type := kind1;
  while identifiers( Token ).value.all = "**" loop
     ParseStaticPowerTermOperator( operator );
     ParseStaticFactor( factor2, kind2 );
     if type_checks_done or else baseTypesOK( kind1, kind2 ) then
        term_type := getBaseType( kind1 );
        operation := getUniType( kind1 );
        if operation = universal_t then
           operation := getUniType( kind2 );
        end if;
        if operation = universal_t then
           operation := uni_string_t;
        end if;
        if operation = uni_numeric_t then
           if operator = "**" then
              begin
                 if isExecutingCommand then
                    term := to_unbounded_string(
                         to_numeric( term ) **
                         natural( to_numeric( factor2 ) ) );
                 end if;
              exception when program_error =>
                 err( "program_error exception raised" );
                 term := null_unbounded_string;
              when others =>
                 err_exception_raised;
                 term := null_unbounded_string;
              end;
          else
              err( "interal error: unknown power operator" );
          end if;
        else
           err( "operation ** not defined for these types" );
        end if;
     end if;
  end loop;
end ParseStaticPowerTerm;

procedure ParseStaticTermOperator( op : out unbounded_string ) is
  -- Syntax: termop = '*' | '/' | '&'
begin
  -- token value is checked by parseTerm, but not token name
  if Token = mod_t or Token = rem_t then
     op := identifiers( token ).name;
  elsif Token /= symbol_t then
     err( "operator expected");
  elsif identifiers( Token ).value.all /= "*" and identifiers( Token ).value.all /= "/" and identifiers( Token ).value.all /= "&" then
     err( "term operator expected");
  else
     op := identifiers( token ).value.all;
  end if;
  getNextToken;
end ParseStaticTermOperator;

procedure ParseStaticTerm( term : out unbounded_string; term_type : out identifier ) is
  -- Syntax: term = "powerterm term-op powerterm"
  pterm1   : unbounded_string;
  pterm2   : unbounded_string;
  kind1    : identifier;
  kind2    : identifier;
  operator : unbounded_string;
  operation: identifier;
begin
  ParseStaticPowerTerm( pterm1, kind1 );
  term := pterm1;
  term_type := kind1;
  while identifiers( Token ).value.all = "*" or
        identifiers( Token ).value.all = "/" or
        identifiers( Token ).value.all = "&" or
        Token = mod_t or Token = rem_t loop
     ParseStaticTermOperator( operator );
     ParseStaticPowerTerm( pterm2, kind2 );
     term_type := getBaseType( kind2 );
     operation := getUniType( kind2 );
     if operation = universal_t then
        operation := getUniType( kind1 );
     end if;
     if operation = universal_t then
        operation := uni_string_t;
     end if;
     if operation = uni_numeric_t then
        if type_checks_done or else baseTypesOK( kind1, kind2 ) then
             if operator = "*" then
                begin
                  if isExecutingCommand then
                     term := castToType(
                        to_numeric( term ) *
                        to_numeric( pterm2 ),
                     term_type );
                  end if;
                 exception when program_error =>
                    err( "program_error exception raised" );
                    term := null_unbounded_string;
                 when others =>
                    err_exception_raised;
                    term := null_unbounded_string;
                 end;
             elsif operator = "/" then
                declare
                  t : long_float;
                  p : long_float;
                  z : long_float := 0.0;
                begin
                  if isExecutingCommand then
                     -- GCC Ada 4.7.1 doesn't catch divide by zero (returns
                     -- infinity for the following division:
                     -- term := castToType(
                     --   to_numeric( term ) /
                     --   to_numeric( pterm2 ),
                     --term_type );
                     -- As a kludge, we'll break up this function call into
                     -- its parts and explicitly test for division by zero.
                     -- this could likely be improved.
                     t := to_numeric( term );
                     p := to_numeric( pterm2 );
                     if p = 0.0 then
                        err( "division by zero" );
                     else
                        z := t / p;
                        term := castToType( z, term_type );
                     end if;
                  end if;
                exception when program_error =>
                   err( "program_error exception raised" );
                   term := null_unbounded_string;
                when others =>
                   err_exception_raised;
                   term := null_unbounded_string;
                end;
             elsif operator = "mod" then
                begin
                  if isExecutingCommand then
                     term := castToType(
                        --long_long_integer'image(
                        long_float(
                        long_long_integer( to_numeric( term ) ) mod
                        long_long_integer( to_numeric( pterm2 ) ) ),
                     term_type );
                  end if;
                exception when program_error =>
                   err( "program_error exception raised" );
                   term := null_unbounded_string;
                when others =>
                   err_exception_raised;
                   term := null_unbounded_string;
                end;
             elsif operator = "rem" then
                begin
                  if isExecutingCommand then
                     term := castToType(
                        --long_long_integer'image(
                        long_float(
                        long_long_integer( to_numeric( term ) ) rem
                        long_long_integer( to_numeric( pterm2 ) ) ),
                     term_type );
                  end if;
                exception when program_error =>
                   err( "program_error exception raised" );
                   term := null_unbounded_string;
                when others =>
                   err_exception_raised;
                   term := null_unbounded_string;
                end;
             else
                err( gnat.source_info.source_location &
                  ": Internal error: unable to handle term operator" );
             end if;
           end if;
        elsif operation = uni_string_t then
           declare
             base1 : identifier := getBaseType( kind1 );
             base2 : identifier := getBaseType( kind2 );
             uni1  : identifier := getUniType( kind1 );
             uni2  : identifier := getUniType( kind2 );
           begin
              if operator = "&" then
                 if base1 = character_t and base2 = character_t then
                    kind1 := string_t;
                    kind2 := string_t;
                    term_type := string_t;
                 elsif base1 = character_t and uni2 = uni_string_t then
                    kind1 := kind2;
                 elsif uni1 = uni_string_t and base2 = character_t then
                    kind2 := kind1;
                    term_type := kind1;
                 end if;
                 if type_checks_done or else baseTypesOK( kind1, kind2 ) then
                    if isExecutingCommand then
                       term := term & pterm2;
                    end if;
                 end if;
              elsif operator = "*" then
                 if type_checks_done or else baseTypesOK( kind1, natural_t ) then
                    if type_checks_done or else baseTypesOK( kind2, uni_string_t ) then
                       if isExecutingCommand then
                          term := natural( to_numeric( term ) ) * pterm2;
                       end if;
                    end if;
                 end if;
              else
                 err( "operation not defined for string types" );
              end if;
           exception when program_error =>
              err( "program_error exception raised" );
              term := null_unbounded_string;
           when others =>
              err_exception_raised;
              term := null_unbounded_string;
           end;
        else
           if operator = "*" then
              err( "operation * not defined for these types" );
           elsif operator = "/" then
              err( "operation / not defined for these types" );
           elsif operator = "&" then
              err( "operation & not defined for these types" );
           end if;
        end if;
  end loop;
end ParseStaticTerm;

procedure ParseStaticSimpleExpressionOperator( op : out unbounded_string ) is
  -- Syntax: simple-expr-op = '+' | '-'
begin
  -- token value is checked by parseTerm, but not token name
  if Token /= symbol_t then
     err( "operator expected");
  elsif identifiers( Token ).value.all /= "+" and identifiers( Token ).value.all /= "-" then
     err( "simple expression operator expected");
  end if;
  op := identifiers( token ).value.all;
  getNextToken;
end ParseStaticSimpleExpressionOperator;

procedure ParseStaticSimpleExpression( se : out unbounded_string; expr_type : out identifier ) is
  -- Syntax: term = "term expr-op term"
  term1    : unbounded_string;
  term2    : unbounded_string;
  kind1    : identifier;
  kind2    : identifier;
  operator : unbounded_string;
  operation: identifier;
  typesOK  : boolean := false;
begin
  ParseStaticTerm( term1, kind1 );
  se := term1;
  expr_type := kind1;
  while identifiers( Token ).value.all = "+" or identifiers( Token ).value.all = "-" loop
     ParseStaticSimpleExpressionOperator( operator );
     ParseStaticTerm( term2, kind2 );
     -- Special exception for + and -...allow time arithmetic
     if (kind1 = cal_time_t) and then (getBaseType(kind2) = duration_t or kind2 = uni_numeric_t or kind2 = universal_t) then
        typesOK := true;
        expr_type := cal_time_t;
        operation := cal_time_t;
     elsif (kind2 = cal_time_t) and then (getBaseType(kind1) = duration_t or kind1 = uni_numeric_t or kind2 = universal_t) then
        typesOK := true;
        expr_type := cal_time_t;
        operation := variable_t;
     elsif (kind1 = cal_time_t) and then (getBaseType(kind2) = cal_time_t) then
        if operator = "+" then
           typesOK := true;
           expr_type := cal_time_t;
        else
           typesOK := true;
           expr_type := duration_t;
        end if;
        operation := root_record_t; -- representing time on both side
     end if;
     -- Otherwise check the types normally
     if not typesOK then
        typesOK := type_checks_done or else baseTypesOK( kind1, kind2 );
        expr_type := getBaseType( kind1 );
        operation := getUniType( kind1 );
     end if;
     if typesOk then
        if operation = universal_t then
           operation := getUniType( kind2 );
        end if;
        if operation = universal_t then
           operation := uni_string_t;
        end if;
        if operation = uni_numeric_t then
             if operator = "+" then
                begin
                  if isExecutingCommand then
                     se := castToType(
                        to_numeric( se ) +
                        to_numeric( term2 ),
                     expr_type );
                  end if;
                exception when program_error =>
                   err( "program_error exception raised" );
                   se := null_unbounded_string;
                when others =>
                   err_exception_raised;
                   se := null_unbounded_string;
                end;
             elsif operator = "-" then
                begin
                  if isExecutingCommand then
                     se := castToType(
                        to_numeric( se ) -
                        to_numeric( term2 ),
                     expr_type );
                  end if;
                exception when program_error =>
                   err( "program_error exception raised" );
                   se := null_unbounded_string;
                when others =>
                   err_exception_raised;
                   se := null_unbounded_string;
                end;
             end if;
        elsif operation = cal_time_t then -- time +/- duration
           if operator = "+" then
              if isExecutingCommand then
                 declare
                    c : scanner.calendar.time := scanner.calendar.time( to_numeric( se ) );
                 begin
                    c := c + duration( to_numeric( term2 ) );
                    se := to_unbounded_string( long_long_integer'image( long_long_integer( c ) ) );
                 end;
              end if;
           elsif operator = "-" then
              if isExecutingCommand then
                 declare
                    c : scanner.calendar.time := scanner.calendar.time( to_numeric( se ) );
                 begin
                    c := c - duration( to_numeric( term2 ) );
                    se := to_unbounded_string( long_long_integer'image( long_long_integer( c ) ) );
                 end;
              end if;
           end if;
        elsif operation = variable_t then -- duration + time
           if operator = "+" then
              if isExecutingCommand then
                 declare
                    c : scanner.calendar.time := scanner.calendar.time( to_numeric( term2 ) );
                 begin
                    c := duration( to_numeric( se ) ) + c;
                    se := to_unbounded_string( long_long_integer'image( long_long_integer( c ) ) );
                 end;
              end if;
           elsif operator = "-" then
              err( "operation - not defined for these types" );
           end if;
        elsif operation = root_record_t then -- adding times
           if operator = "+" then
              err( "operation + not defined for these types" );
           else
              if isExecutingCommand then
                 declare
                    c : scanner.calendar.time := scanner.calendar.time( to_numeric( se ) );
                    c2: duration;
                 begin
                    c2 := c - scanner.calendar.time( to_numeric( term2 ) );
                    se := to_unbounded_string( duration'image( c2 ) );
                 exception when time_error =>
                    err( "duration value too large or small" );
                 when constraint_error =>
                    err( "constraint error" );
                 end;
              end if;
           end if;
        else
             if operator = "+" then
                err( "operation + not defined for these types" );
             elsif operator = "-" then
                err( "operation - not defined for these types" );
             end if;
        end if;
     end if;
  end loop;
  --put_line( "Simple Expression value = " & to_string( se ) );
end ParseStaticSimpleExpression;

procedure ParseStaticRelationalOperator( op : out unbounded_string ) is
  -- Syntax: rel-op = >, >=, etc.
begin
  -- token value is checked by parseTerm, but not token name
  if Token /= symbol_t and Token /= in_t and Token /= not_t then
     err( "operator expected");
  elsif identifiers( Token ).value.all /= ">=" and
        identifiers( Token ).value.all /= ">" and
        identifiers( Token ).value.all /= "<" and
        identifiers( Token ).value.all /= "<=" and
        identifiers( Token ).value.all /= "=" and
        identifiers( Token ).value.all /= "/=" and
        Token /= in_t and Token /= not_t then
     err( "relational operator expected");
  end if;
  if Token = in_t then
     op := identifiers( token ).name;
  elsif Token = not_t then
     op := to_unbounded_string( "not in" );
     getNextToken;
     if Token /= in_t then
        err( "relational operator expected");
     end if;
  else
     op := identifiers( token ).value.all;
  end if;
  getNextToken;
end ParseStaticRelationalOperator;

procedure ParseStaticRelation( re : out unbounded_string; rel_type : out identifier ) is
  -- Syntax: relation = "simple-expr" =|>|<|... "simple-expr"
  se1      : unbounded_string;
  se2      : unbounded_string;
  se3      : unbounded_string;
  kind1    : identifier;
  kind2    : identifier;
  kind3    : identifier;
  operator : unbounded_string;
  operation: identifier;
  b        : boolean;
begin
  ParseStaticSimpleExpression( se1, kind1 );
  re := se1;
  rel_type := kind1;
  if identifiers( Token ).value.all = ">=" or
        identifiers( Token ).value.all = ">" or
        identifiers( Token ).value.all = "<" or
        identifiers( Token ).value.all = "<=" or
        identifiers( Token ).value.all = "=" or
        identifiers( Token ).value.all = "/=" or
        Token = in_t or Token = not_t then
     rel_type := boolean_t; -- always
     ParseStaticRelationalOperator( operator );
     if operator = "in" or operator = "not in" then
        ParseStaticFactor( se2, kind2 );
        if type_checks_done or else baseTypesOK( kind1, kind2 ) then -- redundant below but
           expect( symbol_t, ".." );        -- keeps error messages nice
           ParseStaticFactor( se3, kind3 );       -- should probably restructure
           if type_checks_done or else baseTypesOK( kind2, kind3 ) then
              null;
           end if;
        end if;
     else
        ParseStaticSimpleExpression( se2, kind2 );
     end if;
     if type_checks_done or else baseTypesOK( kind1, kind2 ) then
        operation := getUniType( kind1 );
        if operation = universal_t then
           operation := getUniType( kind2 );
        end if;
        if operation = universal_t then
           operation := uni_string_t;
        end if;
        if operation = uni_numeric_t or else operation = root_enumerated_t or else operation = cal_time_t then
             begin
               if operator = ">=" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) >= to_numeric( se2 );
                  end if;
               elsif operator = ">" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) > to_numeric( se2 );
                  end if;
               elsif operator = "<" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) < to_numeric( se2 );
                  end if;
               elsif operator = "<=" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) <= to_numeric( se2 );
                  end if;
               elsif operator = "=" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) = to_numeric( se2 );
                  end if;
               elsif operator = "/=" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) /= to_numeric( se2 );
                  end if;
               elsif operator = "in" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) in to_numeric( se2 )..to_numeric( se3 );
                  end if;
               elsif operator = "not in" then
                  if isExecutingCommand then
                     b := to_numeric( se1 ) not in to_numeric( se2 )..to_numeric( se3 );
                  end if;
               else
                  err( gnat.source_info.source_location &
                    ": Internal error: couldn't handle relational operator" );
               end if;
               if b then
                  re := to_unbounded_string( "1" );
               else
                  re := to_unbounded_string( "0" );
               end if;
             exception when others =>
               err_exception_raised;
             end;
        elsif operation = uni_string_t then
             if operator = ">=" then
                if isExecutingCommand then
                   b := se1 >= se2;
                end if;
             elsif operator = ">" then
                if isExecutingCommand then
                   b := se1 > se2;
                end if;
             elsif operator = "<" then
                if isExecutingCommand then
                   b := se1 < se2;
                end if;
             elsif operator = "<=" then
                if isExecutingCommand then
                   b := se1 <= se2;
                end if;
             elsif operator = "=" then
                if isExecutingCommand then
                   b := se1 = se2;
                end if;
             elsif operator = "/=" then
                if isExecutingCommand then
                   b := se1 /= se2;
                end if;
             elsif operator = "in" then
                if isExecutingCommand then
                   if length( se1 ) /= 1 or
                      length( se2 ) /= 1 or
                      length( se3 ) /= 1 then
                      err( "scalar type required for range" );
                   else
                      declare
                        c1 : character := element( se1, 1 );
                        c2 : character := element( se2, 1 );
                        c3 : character := element( se3, 1 );
                      begin
                        b := c1 in c2..c3;
                      exception when others =>
                        err_exception_raised;
                      end;
                   end if;
                end if;
             elsif operator = "not in" then
                if isExecutingCommand then
                   if length( se1 ) /= 1 or
                      length( se2 ) /= 1 or
                      length( se3 ) /= 1 then
                      err( "scalar type required for range" );
                   else
                      declare
                        c1 : character := element( se1, 1 );
                        c2 : character := element( se2, 1 );
                        c3 : character := element( se3, 1 );
                      begin
                        b := c1 not in c2..c3;
                      exception when others =>
                        err_exception_raised;
                      end;
                   end if;
                end if;
             else
                err( gnat.source_info.source_location &
                   ": Internal error: couldn't handle relational operator" );
             end if;
             if b then
                re := to_unbounded_string( "1" );
             else
                re := to_unbounded_string( "0" );
             end if;
        else
             err( "relational operation not defined for these types" );
        end if;
     end if;
  end if;
end ParseStaticRelation;

procedure ParseStaticExpressionOperator( op : out identifier ) is
  -- Syntax: expr-op = "and" | "or" | "xor"
begin
  if Token /= and_t and
     Token /= or_t and
     Token /= xor_t then
     err( "boolean operator expected");
  end if;
  op := Token;
  getNextToken;
end ParseStaticExpressionOperator;

procedure ParseStaticExpression( ex : out unbounded_string; expr_type : out identifier ) is
  -- Syntax: expr = "relation" and|or|xor "relation"
  re1      : unbounded_string;
  re2      : unbounded_string;
  kind1    : identifier;
  kind2    : identifier;
  operator : identifier;
  last_op  : identifier := eof_t;
  b        : boolean;
  type bitwise_number is mod 2**64;
begin
  ParseStaticRelation( re1, kind1 );
  ex := re1;
  expr_type := kind1;
  while Token = and_t or Token = or_t or Token = xor_t loop
     ParseStaticExpressionOperator( operator );
     if onlyAda95 and then last_op /= eof_t and then last_op /= operator then
        err( "mixed boolean operators in expression not allowed with " &
              optional_bold( "pragam ada_95" ) & " - use parantheses" );
     end if;
     ParseStaticRelation( re2, kind2 );
     if type_checks_done or else baseTypesOK( kind1, kind2 ) then
        if getUniType( kind1 ) = uni_numeric_t then
           expr_type := getBaseType( kind1 );
           if operator = and_t then
              begin
                if isExecutingCommand then
                   re1 := to_unbounded_string(
                      long_float(
                      bitwise_number( to_numeric( re1 ) ) and
                      bitwise_number( to_numeric( re2 ) ) ) );
                end if;
              exception when program_error =>
                 err( "program_error exception raised" );
                 re1 := null_unbounded_string;
              when ada.strings.index_error =>
                 err( "variable was not intialized" );
                 re1 := null_unbounded_string;
              when others =>
                 err_exception_raised;
                 re1 := null_unbounded_string;
              end;
           elsif operator = or_t then
              begin
                if isExecutingCommand then
                   re1 := to_unbounded_string(
                      long_float(
                      bitwise_number( to_numeric( re1 ) ) or
                      bitwise_number( to_numeric( re2 ) ) ) );
                end if;
              exception when program_error =>
                 err( "program_error exception raised" );
                 re1 := null_unbounded_string;
              when ada.strings.index_error =>
                 err( "variable was not intialized" );
                 re1 := null_unbounded_string;
              when others =>
                 err_exception_raised;
                 re1 := null_unbounded_string;
              end;
           elsif operator = xor_t then
              begin
                if isExecutingCommand then
                   re1 := to_unbounded_string(
                      long_float(
                      bitwise_number( to_numeric( re1 ) ) xor
                      bitwise_number( to_numeric( re2 ) ) ) );
                end if;
              exception when program_error =>
                 err( "program_error exception raised" );
                 re1 := null_unbounded_string;
              when ada.strings.index_error =>
                 err( "variable was not intialized" );
                 re1 := null_unbounded_string;
              when others =>
                 err_exception_raised;
                 re1 := null_unbounded_string;
              end;
           end if;
        elsif getBaseType( kind1 ) = boolean_t then
           expr_type := getBaseType( kind1 );
           if operator = and_t then
              if isExecutingCommand then
                 b := re1 = "1" and re2 = "1";
              end if;
           elsif operator= or_t then
              if isExecutingCommand then
                 b := re1 = "1" or re2 = "1";
              end if;
           elsif operator = xor_t then
              if isExecutingCommand then
                 b := re1 = "1" xor re2 = "1";
              end if;
           else
              err( gnat.source_info.source_location &
                ": Internal error: unable to handle boolean operator" );
           end if;
           if b then
              re1 := to_unbounded_string( "1" );
           else
              re1 := to_unbounded_string( "0" );
           end if;
        else
           err( "boolean or number expected" );
        end if;
     end if;
     last_op := operator;
  end loop;
  ex := re1;
  --put_line( "Expression value = " & to_string( ex ) );
end ParseStaticExpression;



--  START PARSER
--
-- Startup this package, performing any set up tasks.  In this case, none.
------------------------------------------------------------------------------

procedure startParser is
begin
  -- expression side-effect detection: no expressions have run yet.
  lastExpressionInstruction := noExpressionInstruction;
  clearActiveExpressionIds;
end startParser;


--  SHUTDOWN PARSER
--
-- Shut down this package, performing any cleanup tasks.  In this case, none.
------------------------------------------------------------------------------

procedure shutdownParser is
begin
  clearActiveExpressionIds;
end shutdownParser;

end parser;
