------------------------------------------------------------------------------
-- AdaScript Language Parser - Pragmas                                      --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2011 Free Software Foundation              --
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

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );
with system,
    ada.text_io.editing,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    ada.numerics.float_random,
    ada.calendar,
    gnat.regexp,
    gnat.directory_operations,
    cgi,
    bush_os.exec,
    pegasock.memcache,
    world,
    string_util,
    user_io,
    script_io,
    scanner,
    builtins,
    signal_flags,
    jobs, -- for clearCommandHash
    parser_aux,
    parser,
    parser_teams;
use ada.text_io,
    ada.text_io.editing,
    ada.command_line,
    ada.command_line.environment,
    ada.strings.unbounded,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    gnat.regexp,
    gnat.directory_operations,
    bush_os,
    bush_os.exec,
    pegasock.memcache,
    user_io,
    script_io,
    world,
    string_util,
    scanner,
    builtins,
    signal_flags,
    jobs,
    parser_aux,
    parser,
    parser_teams;

package body parser_pragmas is

-- Types of Pragmas

type aPragmaKind is (
     ada_95,
     advise,
     asserting,
     annotate,
     blocked,
     clarify,
     debug,
     debug_on,
     depreciated,
     dispute,
     export,
     export_json,
     gcc_errors,
     import,
     import_json,
     inspection,
     inspect_var,
     license,
     noCommandHash,
     peek,
     promptChange,
     propose,
     refactor,
     register_memcache_server,
     restriction,
     restriction_annotations,
     restriction_auto,
     restriction_unused,
     restriction_external,
     restriction_memcache,
     restriction_mysql,
     restriction_postgresql,
     restriction_todos,
     session_export_script,
     session_import_script,
     software_model,
     suppress,
     suppress_word_quoting,
     suppress_low_priority_todos,
     suppress_all_todos,
     template,
     test,
     test_result,
     todo,
     unchecked_import,
     unchecked_import_json,
     uninspect_var,
     unrestricted_template,
     volatile,
     unknown_pragma
   );

--  PARSE PRAGMA KIND
--
-- Check the current token for the kind of pragma and advance the scanner.
-----------------------------------------------------------------------------

function parsePragmaKind return aPragmaKind is
  name : string := to_string( identifiers( token ).name );
  pragmaKind : aPragmaKind := unknown_pragma;
begin
   -- just an error message...if ( with no name
   if token = symbol_t and identifiers( symbol_t ).value = to_unbounded_string( "(" ) then
      err( "pragma name missing" );
  elsif name = "ada_95" then
     pragmaKind := ada_95;
  elsif name = "advise" then
     pragmaKind :=  advise;
  elsif name = "assert" then
     pragmaKind :=  asserting;
  elsif name = "debug" then
     pragmaKind :=  debug;
  elsif name = "annotate" then
     pragmaKind :=  annotate;
  elsif name = "blocked" then
     pragmaKind :=  blocked;
  elsif name = "clarify" then
     pragmaKind :=  clarify;
  elsif name = "deprecated" then
     pragmaKind :=  depreciated;
  elsif name = "depreciated" then
     pragmaKind :=  depreciated;
  elsif name = "dispute" then
     pragmaKind :=  dispute;
  elsif name = "export" then
     pragmaKind := export;
  elsif name = "export_json" then
     pragmaKind := export_json;
  elsif name = "gcc_errors" then
     pragmaKind := gcc_errors;
  elsif name = "import" then
     pragmaKind := import;
  elsif name = "import_json" then
     pragmaKind := import_json;
  elsif name = "inspect" then
     pragmaKind := inspect_var;
  elsif name = "inspection_point" then
     pragmaKind := inspection;
  elsif name = "license" then
     pragmaKind := license;
  elsif name = "inspection_peek" then
     pragmaKind := peek;
  elsif name = "no_command_hash" then
     pragmaKind := noCommandHash;
  elsif name = "prompt_script" then
     pragmaKind := promptChange;
  elsif name = "propose" then
     pragmaKind := propose;
  elsif name = "refactor" then
     pragmaKind := refactor;
  elsif name = "register_memcache_server" then
     pragmaKind := register_memcache_server;
  elsif name = "restriction" then
     pragmaKind := restriction;
  elsif name = "restrictions" then
     discardUnusedIdentifier( token );
     err( "pragma restriction not restrictions" );
  elsif name = "session_export_script" then
     pragmaKind := session_export_script;
  elsif name = "session_import_script" then
     pragmaKind := session_import_script;
  elsif name = "software_model" then
     pragmaKind := software_model;
  elsif name = "suppress" then
     pragmaKind := suppress;
  elsif name = "template" then
     pragmaKind := template;
  elsif name = "test" then
     pragmaKind :=  test;
  elsif name = "test_result" then
     pragmaKind :=  test_result;
  elsif name = "todo" then
     pragmaKind :=  todo;
  elsif name = "unchecked_import" then
     pragmaKind := unchecked_import;
  elsif name = "unchecked_import_json" then
     pragmaKind := unchecked_import_json;
  elsif name = "uninspect" then
     pragmaKind := uninspect_var;
  elsif name = "unrestricted_template" then
     pragmaKind := unrestricted_template;
  elsif name = "volatile" then
     pragmaKind := volatile;
  else
     discardUnusedIdentifier( token );
     err( "unknown pragma" );
  end if;
  discardUnusedIdentifier( token );
  -- don't declare a new identifier for a pragma
  getNextToken;
  return pragmaKind;
end parsePragmaKind;


--  PARSE ANNOTATE KIND
--
-----------------------------------------------------------------------------

procedure ParseAnnotateKind is
  name : string := to_string( identifiers( token ).name );
begin
  annotationsFound := true;
  if token /= strlit_t then
     annotationTodoFound := name = "todo";
     if name /= "author" and
        name /= "created" and
        name /= "description" and
        name /= "errors" and
        name /= "modified" and
        name /= "param" and
        name /= "return" and
        name /= "see_also" and
        name /= "summary" and
        name /= "todo" and
        name /= "version" then
        err( "unknown annotation field type" );
     else
        discardUnusedIdentifier( token );
        getNextToken;
        expect( symbol_t, "," );
     end if;
  end if;
  expect( strlit_t );
end ParseAnnotateKind;

--  PARSE IMPORT KIND
--
-----------------------------------------------------------------------------

procedure ParseImportKind( var_id : out identifier; importKind: out unbounded_string ) is
  name_unbounded : unbounded_string := identifiers( token ).name;
  name : string := to_string( name_unbounded );
begin
  if name = "shell" then
     importKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  elsif name = "cgi" then
     importKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  elsif name = "local_memcache" then
     if restriction_no_memcache then
        err( "not allowed with " & bold( "pragma restriction( no_memcache )" ) );
     end if;
     importKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  elsif name = "memcache" then
     if restriction_no_memcache then
        err( "not allowed with " & bold( "pragma restriction( no_memcache )" ) );
     end if;
     importKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  elsif name = "session" then
     importKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  else
     importKind := null_unbounded_string;
     discardUnusedIdentifier( token );
     err( "only 'cgi', 'shell', 'local_memcache', 'memcache', 'session' convention supported" );
  end if;
end ParseImportKind;


--  PARSE EXPORT KIND
--
-----------------------------------------------------------------------------

procedure ParseExportKind( var_id : out identifier; exportKind: out unbounded_string ) is
  name_unbounded : unbounded_string := identifiers( token ).name;
  name : string := to_string( name_unbounded );
begin
  if name = "shell" then
     exportKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  elsif name = "local_memcache" then
     exportKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  elsif name = "memcache" then
     exportKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  elsif name = "session" then
     exportKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  else
     exportKind := null_unbounded_string;
     discardUnusedIdentifier( token );
     err( "only 'shell', 'local_memcache', 'memcache', 'session' convention supported" );
  end if;
end ParseExportKind;


--  PARSE LICENSE KIND
--
-----------------------------------------------------------------------------

procedure ParseLicenseKind( expr_val : out unbounded_string ) is
  name_unbounded : unbounded_string := identifiers( token ).name;
  name : string := to_string( name_unbounded );

  procedure ParseLicenseExtra is
  begin
     if token = symbol_t and identifiers( token ).value = "," then
        expect( symbol_t, "," );
        expr_val := expr_val & ": " & identifiers( token ).value;
        expect( strlit_t );
     end if;
  end ParseLicenseExtra;

begin
  if name = "unrestricted" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "gpl" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "gplv2" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "gplv3" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "agpl" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "bsd_original" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "bsd_revised" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "artistic" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "mit" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "apache" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "apache_2" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "freeware" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "shareware" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "public_domain" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "commercial" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  elsif name = "restricted" then
     expr_val := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     ParseLicenseExtra;
  else
     err( "unknown license " & bold( to_string( expr_val ) ) );
  end if;
end ParseLicenseKind;


--  PARSE SOFTWARE MODEL NAME
--
-----------------------------------------------------------------------------

procedure ParseSoftwareModelName( expr_val : out unbounded_string ) is
  name_unbounded : unbounded_string := identifiers( token ).name;
  name : string := to_string( name_unbounded );
begin
  expr_val := null_unbounded_string;
  if name = "application_desktop" then
     expr_val := name_unbounded;
  elsif name = "application_mobile" then
     expr_val := name_unbounded;
  elsif name = "application_realtime" then
     expr_val := name_unbounded;
  elsif name = "application_realtime_ravenscar" then
     expr_val := name_unbounded;
  elsif name = "daemon" then
     expr_val := name_unbounded;
  elsif name = "daemon_proxy" then
     expr_val := name_unbounded;
  elsif name = "http_framework" then
     expr_val := name_unbounded;
  elsif name = "http_service_external" then
     expr_val := name_unbounded;
  elsif name = "http_service_internal" then
     expr_val := name_unbounded;
  elsif name = "http_site_external" then
     expr_val := name_unbounded;
  elsif name = "http_site_internal" then
     expr_val := name_unbounded;
  elsif name = "http_proxy" then
     expr_val := name_unbounded;
  elsif name = "http_form" then
     expr_val := name_unbounded;
  elsif name = "package" then
     expr_val := name_unbounded;
  elsif name = "shell_batch" then
     expr_val := name_unbounded;
  elsif name = "shell_filter_script" then
     expr_val := name_unbounded;
  elsif name = "shell_report_script" then
     expr_val := name_unbounded;
  elsif name = "shell_script" then
     expr_val := name_unbounded;
  elsif name = "multimedia" then
     expr_val := name_unbounded;
  elsif name = "etl" then
     expr_val := name_unbounded;
  elsif name = "monitor" then
     expr_val := name_unbounded;
  elsif name = "driver" then
     expr_val := name_unbounded;
  end if;

  if length( expr_val ) > 0 then
     discardUnusedIdentifier( token );
     getNextToken;
  else
     err( "unknown software model " & bold( to_string( expr_val ) ) );
  end if;
end ParseSoftwareModelName;


--  PARSE PRAGMA STATEMENT
--
-- Syntax: ... kind [params]
-----------------------------------------------------------------------------

procedure ParsePragmaStatement( thePragmaKind : aPragmaKind ) is
  pragmaKind  : aPragmaKind := thePragmaKind; -- TODO: Hack
  expr_val    : unbounded_string;
  expr_val2   : unbounded_string;
  results     : unbounded_string;
  var_id      : identifier;
  exportType  : unbounded_string;
  importType  : unbounded_string;
  newValue    : unbounded_string;
begin

  -- Parse the pragma parameters (if any)

  if pragmaKind /= ada_95 and pragmaKind /= inspection and pragmaKind /=
     noCommandHash and pragmaKind /= peek and pragmaKind /= gcc_errors then
     if pragmaKind = debug and (token /= symbol_t or identifiers( token ).value /= "(") then
        pragmaKind := debug_on;
     else
        expect( symbol_t, "(" );
     end if;
  end if;

  case pragmaKind is
  when ada_95 =>                             -- pragma ada_95
     null;
  when advise =>                             -- pragma advise
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expect( symbol_t, "," );
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expect( symbol_t, "," );
           expect( strlit_t );
        end if;
     end if;
  when asserting =>                          -- pragma assert
     ParseExpression( expr_val, var_id );
  when annotate =>                           -- pragma annotate
     ParseAnnotateKind;
  when blocked =>                            -- pragma clarify
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expect( symbol_t, "," );
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expect( symbol_t, "," );
           expect( strlit_t );
        end if;
     end if;
  when clarify =>                            -- pragma clarify
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expect( symbol_t, "," );
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expect( symbol_t, "," );
           expect( strlit_t );
        end if;
     end if;
  when debug =>                              -- pragma debug
     expr_val := identifiers( token ).value;
     expect( backlit_t );
  when debug_on =>                              -- pragma debug (no param)
     null;
  when depreciated =>                           -- pragma depreciated
     expr_val := identifiers( token ).value;
     expect( strlit_t );
  when dispute =>                               -- pragma dispute
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expect( symbol_t, "," );
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expect( symbol_t, "," );
           expect( strlit_t );
        end if;
     end if;
  when export | export_json =>                  -- pragma export/json
     ParseExportKind( var_id, exportType );
  when import | unchecked_import | import_json | unchecked_import_json =>
     -- pragma unchecked/import/json
     ParseImportKind( var_id, importType );
  when gcc_errors =>                         -- pragma gcc_errors
     null;
  when inspection =>                         -- pragma inspection point
     null;
  when peek =>                               -- pragma inspection peek
     null;
  when noCommandHash =>                      -- pragma no_command_hash
     null;
  when promptChange =>                       -- pragma prompt_script
     expr_val := identifiers( token ).value;
     expect( backlit_t );
  when propose =>                           -- pragma refactor
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expect( symbol_t, "," );
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expect( symbol_t, "," );
           expect( strlit_t );
        end if;
     end if;
  when refactor =>                           -- pragma refactor
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expect( symbol_t, "," );
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expect( symbol_t, "," );
           expect( strlit_t );
        end if;
     end if;
  when register_memcache_server =>           -- pragma register_memcache_server
     expr_val := identifiers( token ).value;
     expect( strlit_t );
     expect( symbol_t, "," );
     expr_val2 := identifiers( token ).value;
     expect( number_t );
  when restriction =>                        -- pragma restriction
     if identifiers( token ).name = "no_auto_declarations" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_auto;
     elsif identifiers( token ).name = "no_annotate_todos" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_todos;
     elsif identifiers( token ).name = "annotations_not_optional" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_annotations;
     elsif identifiers( token ).name = "no_external_commands" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_external;
     elsif identifiers( token ).name = "no_memcache" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_memcache;
     elsif identifiers( token ).name = "no_mysql_database" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_mysql;
     elsif identifiers( token ).name = "no_postgresql_database" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_postgresql;
     elsif identifiers( token ).name = "no_unused_identifiers" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_unused;
     else
        discardUnusedIdentifier( token );
        err( "unknown restriction" );
        return;
     end if;
  when inspect_var =>                        -- pragma inspect
     ParseIdentifier( var_id );
  when license =>                            -- pragma license
     ParseLicenseKind( expr_val );
  when software_model =>                     -- pragma software_model
     ParseSoftwareModelName( expr_val );
  when session_export_script =>              -- pragma session_export_script
     expr_val := identifiers( token ).value;
     expect( backlit_t );
  when session_import_script =>              -- pragma session_import_script
     expr_val := identifiers( token ).value;
     expect( backlit_t );
  when suppress =>                           -- pragma restriction
     if identifiers( token ).name = "word_quoting" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := suppress_word_quoting;
     elsif identifiers( token ).name = "low_priority_todos_for_release" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := suppress_low_priority_todos;
     elsif identifiers( token ).name = "all_todos_for_release" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := suppress_all_todos;
     else
        discardUnusedIdentifier( token );
        err( "unknown error type" );
        return;
     end if;
  when template | unrestricted_template =>   -- pragma (unrestricted) template
     if rshOpt then
        err( "templates are not allowed in a restricted shell" );
     else
        expr_val := identifiers( token ).name;
        discardUnusedIdentifier( token );
        getNextToken;
        if token = symbol_t and identifiers( token ).value = "," then
           expect( symbol_t, "," );
           expect( strlit_t );
           var_id := strlit_t;
        else
           var_id := eof_t;
        end if;
     end if;
     -- Mark this script as having a template to disable unused variable checks
     -- We need the type of template so interpret it now.
     hasTemplate := true;
     templateHeader.templateType := noTemplate;
     templateHeader.status := 200;
     -- http://www.webmaster-toolkit.com/mime-types.shtml
     if expr_val = "html" then
        templateHeader.templateType := htmlTemplate;
     elsif expr_val = "css" then
        templateHeader.templateType := cssTemplate;
     elsif expr_val = "js" then
        templateHeader.templateType := jsTemplate;
     elsif expr_val = "json" then
        templateHeader.templateType := jsonTemplate;
     elsif expr_val = "text" then
        templateHeader.templateType := textTemplate;
     elsif expr_val = "wml" then
        templateHeader.templateType := wmlTemplate;
     elsif expr_val = "xml" then
        templateHeader.templateType := xmlTemplate; -- text/xml
     else
        err( "unknown template type" );
     end if;
  when test =>                               -- pragma test
     expr_val := identifiers( token ).value;
     expect( backlit_t );
  when test_result =>                        -- pragma test_result
     declare
       save_syntax : boolean;
     begin
        -- if we're not testing, we have to evaluate the expression but we
        -- don't want to execute it because there may be variables without
        -- values throwing exceptions when tests don't run.
        if not testOpt then
           save_syntax := syntax_check;
           syntax_check := true;
           ParseExpression( expr_val, var_id );
           syntax_check := save_syntax;
        else
           ParseExpression( expr_val, var_id );
        end if;
     end;
  when todo =>                               -- pragma to-do
     -- we don't need to save anything because this is an informational
     -- pragma.  The syntax is rather complicated.  This runs during
     -- the syntax check so we are limited to using literals (unless
     -- we create a new static expression feature).  Here, we're doing
     -- syntax checking.
     -- example: pragma to-do( me, "something", work_measure.story_points, 2, work_priority.level, 'l', "ticket" );
     declare
       unused_bool : boolean;
       work_estimate_unknown : boolean;
     begin
       ParseIdentifier( var_id );              -- the person
       unused_bool := baseTypesOK( identifiers( var_id ).kind, teams_member_t );
       expect( symbol_t, "," );
       --expr_val := identifiers( token ).value;
       expect( strlit_t );
       expect( symbol_t, "," );

       -- pragma to-do: the work estimate measure

       ParseIdentifier( var_id );
       unused_bool := baseTypesOK( identifiers( var_id ).kind, teams_work_measure_t );
       expect( symbol_t, "," );
       work_estimate_unknown := false;

       -- pragma to-do: the work estimate value

       if var_id = teams_work_measure_unknown_t then
          expect( number_t, " 0" );
          work_estimate_unknown := true;
       elsif var_id = teams_work_measure_size_t then
          if identifiers( token ).value /= "s" and
             identifiers( token ).value /= "m" and
             identifiers( token ).value /= "l" and
             identifiers( token ).value /= "xl" then
             err( "expected ""s"", ""m"", ""l"" or ""xl""" );
          end if;
         expect( strlit_t );
       elsif var_id = teams_work_measure_hours_t or
             var_id = teams_work_measure_fpoints_t or
             var_id = teams_work_measure_spoints_t or
             var_id = teams_work_measure_sloc_t then
         expect( number_t );
       else
          err( "internal error: don't know how to handle this type of work measure value" );
       end if;
       expect( symbol_t, "," );

       -- pragma to-do: the work priority measure

       ParseIdentifier( var_id );
       unused_bool := baseTypesOK( identifiers( var_id ).kind, teams_work_priority_t );
       expect( symbol_t, "," );

       -- pragma to-do: the work priority value

       if var_id = teams_work_priority_unknown_t then
          expect( number_t, " 0" );
       elsif var_id = teams_work_priority_completed_t then
          expect( number_t, " 0" );
       elsif var_id = teams_work_priority_level_t then
          if identifiers( token ).value /= "l" and
             identifiers( token ).value /= "m" and
             identifiers( token ).value /= "h" then
             err( "expected 'l', 'm' or 'h'" );
          end if;
          if not work_estimate_unknown and not allowAllTodosForRelease then
             if boolean( testOpt ) or boolean( maintenanceOpt ) then
                if allowLowPriorityTodosForRelease and identifiers( token ).value = "l" then
                   null;
                else
                   err( "priority todo task not yet completed" );
                end if;
             end if;
          end if;
          expect( charlit_t );
       elsif var_id = teams_work_priority_severity_t then
          if identifiers( token ).value < " 1" or
             identifiers( token ).value > " 5" then
             err( "expected 1..5" );
          end if;
          if not work_estimate_unknown and not allowAllTodosForRelease then
             if boolean( testOpt ) or boolean( maintenanceOpt ) then
                if allowLowPriorityTodosForRelease and identifiers( token ).value < " 2" then
                   null;
                else
                   err( "priority todo task not yet completed" );
                end if;
             end if;
          end if;
          expect( number_t );
       elsif var_id = teams_work_priority_risk_t then
          if not work_estimate_unknown and not allowAllTodosForRelease then
             if boolean( testOpt ) or boolean( maintenanceOpt ) then
                -- any financial risk
                if identifiers( token ).value /= " 0" then
                   err( "priority todo task not yet completed" );
                end if;
             end if;
          end if;
          expect( number_t );
       elsif var_id = teams_work_priority_cvss_t then
          declare
             v1 : long_float;
          begin
             v1 := to_numeric( identifiers( token ).value );
             if v1 < 0.0 or v1 > 10.0 then
                err( "expected 1..10" );
             end if;
             -- CVSS 2 says a score of 3.9 or lower is low risk
             if not work_estimate_unknown and not allowAllTodosForRelease then
                if boolean( testOpt ) or boolean( maintenanceOpt ) then
                   if allowLowPriorityTodosForRelease and v1 < 4.0 then
                      null;
                   elsif v1 > 0.0 then
                      err( "priority todo task not yet completed" );
                   end if;
                end if;
             end if;
          exception when others => null;
          end;
          expect( number_t );
       else
          err( "internal error: don't know how to handle this type of work priority value" );
       end if;
       -- optional ticket id
       if token = symbol_t and identifiers( token ).value = "," then
          expect( symbol_t, "," );
          expect( strlit_t );
       end if;
     end;
  when uninspect_var =>                      -- pragma uninspect
     ParseIdentifier( var_id );
  when volatile =>                           -- pragma volatile
     ParseIdentifier( var_id );
  when others =>
     err( "Internal error: can't handle pragma" );
  end case;

  if pragmaKind /= ada_95 and pragmaKind /= inspection and pragmaKind /=
     noCommandHash and pragmaKind /= debug_on and pragmaKind /= peek and
     pragmaKind /= gcc_errors then
     expect( symbol_t, ")" );
  end if;

  -- Execute the pragma

  -- this pragma only affects syntax checking.  syntax checking doesn't
  -- happen at the command prompt.
  if syntax_check then
     if pragmaKind = restriction_unused then
        restriction_no_unused_identifiers := true;
     end if;
     if pragmaKind = suppress_word_quoting then
        world.suppress_word_quoting := true;
     end if;
     if pragmaKind = suppress_low_priority_todos then
        allowLowPriorityTodosForRelease := true;
     end if;
     if pragmaKind = suppress_all_todos then
        allowAllTodosForRelease := true;
     end if;
  end if;

  if isExecutingCommand then
     case pragmaKind is
     when ada_95 =>
        onlyAda95 := true;
     when advise =>
        null;
     when asserting =>
        if debugOpt or testOpt then
           if not syntax_check then   -- has no meaning during syntax check
              if baseTypesOk( boolean_t, var_id ) then
                 if expr_val = "0" then
                    err( "assertion failed" );
                 end if;
              end if;
           end if;
        end if;
     when annotate =>
        null;
     when blocked =>
        null;
     when clarify =>
        null;
     when debug =>
        if debugOpt then
           if not syntax_check then
              declare
                 savershOpt : commandLineOption := rshOpt;
                 lineNo      : natural;
              begin
                 lineNo := getLineNo;
                 rshOpt := true;            -- force restricted shell mode
                 CompileAndRun( commands => expr_val, firstLineNo => lineNo, fragment => false );
                 rshOpt := savershOpt;
              end;
           end if;
        end if;
     when debug_on =>
        debugOpt := true;
     when depreciated =>
        -- later, this should create a list of depreciation message
        -- for now, only the entire script is depreciated
        depreciatedMsg := "This script made obsolete by " & expr_val & '.';
     when dispute =>
        null;
     when export | export_json  =>
        if pragmaKind = export_json then
           identifiers( var_id ).mapping := json;
           if identifiers( var_id ).class = userProcClass then
              err( "procedures cannot be exported" );
           elsif identifiers( var_id ).class = userFuncClass then
              err( "functions cannot be exported" );
           elsif identifiers( var_id ).export then
              err( "variable is already exported" );
           end if;
        else
           identifiers( var_id ).mapping := none; -- should not be necessary
           if identifiers( var_id ).class = userProcClass then
              err( "procedures cannot be exported" );
           elsif identifiers( var_id ).class = userFuncClass then
              err( "functions cannot be exported" );
           elsif identifiers( var_id ).list then
              err( "arrays cannot be exported without export_json or arrays.to_json" );
           elsif identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_record_t then
              err( "records cannot be exported without export_json or records.to_json" );
           elsif identifiers( var_id ).export then
              err( "variable is already exported" );
           elsif not uniTypesOK( identifiers( var_id ).kind, uni_string_t ) then
              err( "only string variables exported" );
           end if;
        end if;
        if not error_found then
           identifiers( var_id ).export := true;
           if exportType = "local_memcache" then
              identifiers( var_id ).method := local_memcache;
              checkAndInitializeLocalMemcacheCluster;
           elsif exportType = "memcache" then
              identifiers( var_id ).method := memcache;
              checkAndInitializeLocalMemcacheCluster;
           elsif exportType = "cgi" then
              -- this should have been caught earlier
              err( "cgi variables cannot be exported" );
           elsif exportType = "session" then
              identifiers( var_id ).method := session;
           elsif exportType = "shell" then
              identifiers( var_id ).method := shell;
           else
              err( "unexpected export method" );
           end if;
        end if;
     when gcc_errors =>
        gccOpt := true;
     when import | import_json =>
        -- Check for a reasonable identifier type
        if pragmaKind = import_json then
           identifiers( var_id ).mapping := json;
           if identifiers( var_id ).class = userProcClass then
              err( "procedures cannot be imported" );
           elsif identifiers( var_id ).class = userFuncClass then
              err( "functions cannot be imported" );
           elsif identifiers( var_id ).import then
              err( "variable is already imported" );
           end if;
        else
           identifiers( var_id ).mapping := none;
           if identifiers( var_id ).class = userProcClass then
              err( "procedures cannot be imported" );
           elsif identifiers( var_id ).class = userFuncClass then
              err( "functions cannot be imported" );
           elsif identifiers( var_id ).list then
              err( "arrays cannot be imported without import_json or arrays.to_json" );
           elsif identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_record_t then
              err( "records cannot be imported without import_json or records.to_json" );
           elsif identifiers( var_id ).import then
              err( "variable is already imported" );
           elsif not uniTypesOK( identifiers( var_id ).kind, uni_string_t ) then
              err( "only string variables exported" );
           end if;
        end if;
        -- All clear? Get the value
        if not error_found then
           identifiers( var_id ).import := true;
           if importType = "local_memcache" then
              identifiers( var_id ).method := local_memcache;
              checkAndInitializeLocalMemcacheCluster;
              begin
                 Get( localMemcacheCluster,
                      identifiers( var_id ).name,
                      newValue );
                 if length( newValue ) = 0 then
                    err( "unable to find variable " &
                         to_string( identifiers( var_id ).name ) &
                         " in the local memcache" );
                    identifiers( var_id ).import := false;
                 end if;
              exception when others =>
                 err( "exception raised" );
              end;
           elsif importType = "memcache" then
              identifiers( var_id ).method := memcache;
              checkAndInitializeLocalMemcacheCluster;
              begin
                 Get( distributedMemcacheCluster,
                      identifiers( var_id ).name,
                      newValue );
                 if length( newValue ) = 0 then
                    err( "unable to find variable " &
                         to_string( identifiers( var_id ).name ) &
                         " in memcached" );
                    identifiers( var_id ).import := false;
                    -- just for pragma import, mark as not imported if there
                    -- was an error (otherwise on the command prompt the
                    -- user will not be able to re-import)
                 end if;
              exception when others =>
                 err( "exception raised" );
              end;
           --elsif processingTemplate and importType = "cgi" then
           elsif importType = "cgi" then
              identifiers( var_id ).method := http_cgi;
              declare
                 found : boolean := false;
              begin
                 for i in 1..cgi.key_count( to_string( identifiers( var_id ).name ) ) loop
                     newValue := newValue & to_unbounded_string( cgi.value(
                         to_string( identifiers( var_id ).name ), 1, true ) );
                     found := true;
                  end loop;
                  if found then
              -- apply mapping, if any.  assume these are all set correctly
                     if identifiers( var_id ).mapping = json then                       -- json
                        if getUniType( identifiers( var_id ).kind ) = uni_string_t then -- string
                           DoJsonToString( identifiers( var_id ).value, newValue );
                        elsif identifiers( var_id ).list then                           -- array
                           DoJsonToArray( var_id, newValue );
                        elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_record_t then -- record
                           DoJsonToRecord( var_id, newValue );
                        elsif getUniType( identifiers( var_id ).kind ) = uni_numeric_t then -- number
                           DoJsonToNumber( newValue, identifiers( var_id ).value );
                        elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_enumerated_t then -- enum
                           DoJsonToNumber( newValue, identifiers( var_id ).value );
                           -- identifiers( var_id ).value := newValue;
                        else
                           err( "internal error: unexpected import translation type" );
                        end if;
                     else                                                           -- no mapping
                        identifiers( var_id ).value := newValue;
                     end if;
                  else
                    err( "unable to find variable " &
                         to_string( identifiers( var_id ).name ) &
                         " in the cgi variables" );
                  end if;
              end;
           elsif importType = "session" then
              if length( sessionImportScript ) = 0 then
                 err( "session import script not defined" );
              else
                 declare
                   temp1_t    : identifier;
                   temp2_t    : identifier;
                   importValue: unbounded_string;
                   --b          : boolean;
                 begin
                   -- TODO: full prefix is a good idea but should be done with
                   -- a separate pragma for consistency.
                   -- GetFullParentUnitName( prefix );
                   --declareStandardConstant( temp1_t,
                   --   "session_variable_name", string_t,
                   --   to_string( identifiers( var_id ).name ) );
                   --declareIdent( temp2_t,
                   --   "session_variable_value", string_t );
                   --if not error_found then
                   findIdent( sessions_session_variable_name_str, temp1_t );
                   findIdent( sessions_session_variable_value_str, temp2_t );
                   if temp1_t /= eof_t then
                      identifiers( temp1_t ).value := identifiers( var_id ).name;
                   end if;
                      CompileAndRun( sessionImportScript, 1, false );
                      importValue := identifiers( temp2_t ).value;
                   --   b := deleteIdent( temp2_t );
                   --   b := deleteIdent( temp1_t );
                      case identifiers( var_id ).mapping is
                      when json =>
                         if getUniType( identifiers( var_id ).kind ) = uni_string_t then -- string
                            DoJsonToString( identifiers( var_id ).value, importValue );
                         elsif identifiers( var_id ).list then                           -- array
                            DoJsonToArray( var_id, importValue );
                         elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_record_t then -- record
                            DoJsonToRecord( var_id, importValue );
                         elsif getUniType( identifiers( var_id ).kind ) = uni_numeric_t then -- number
                            DoJsonToNumber( importValue, identifiers( var_id ).value );
                         elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_enumerated_t then -- enum
                            DoJsonToNumber( importValue, identifiers( var_id ).value );
                         else
                            err( "internal error: unexpected import translation type" );
                         end if;
                      when none =>
                         identifiers( var_id ).value := importValue;
                      when others =>
                         err( "internal error: unexpected mapping type" );
                      end case;
                   --end if;
                 end;
              end if;
           else
--               identifiers( var_id ).method := shell;
                refreshVolatile( var_id );
           -- show a trace of what's imported.  If JSON, show JSON as it
           -- could be multiple values
           if trace then
               put_trace(
                  to_string( identifiers( var_id ).name ) & " := """ &
                  to_string( ToEscaped( newValue ) ) &
                  """" );
           end if;
           end if;
        end if;
     when inspection =>
        if breakoutOpt then
           wasSIGINT := true;                            -- pretend ctrl-c
        end if;
     when license =>
        if licenseSet then
           err( "license already set" );
        else
          declare
            id : identifier;
          begin
            findIdent( to_unbounded_string( "System.Script_License" ), id );
            if id /= eof_t then
               identifiers( id ).value := expr_val;
               licenseSet := true;
            end if;
          exception when others =>
            err( "exception raised" );
          end;
        end if;
     when noCommandHash =>
        clearCommandHash;
        no_command_hash := true;
     when peek =>
        for i in 1..identifiers_top-1 loop
            if identifiers( i ).inspect then
               Put_Identifier( i );
            end if;
        end loop;
        -- put_line( getStackTrace );
     when propose =>
        null;
     when refactor =>
        null;
     when register_memcache_server =>
         checkAndInitializeDistributedMemcacheCluster;
         begin
            RegisterServer( distributedMemcacheCluster,
               expr_val,
               natural'value( ' ' & to_string( expr_val2 ) ) );
         exception when name_error =>
            err( "server already registered or too many servers registered" );
         when others =>
            err( "exception raised" );
         end;
     when restriction_auto =>
        restriction_no_auto_declarations := true;
     when restriction_annotations =>
        restriction_annotations_not_optional := true;
     when restriction_unused =>
        null; -- only applies in syntax check
     when restriction_external =>
        restriction_no_external_commands := true;
     when restriction_memcache =>
        restriction_no_memcache := true;
     when restriction_mysql =>
        restriction_no_mysql_database := true;
     when restriction_postgresql =>
        restriction_no_postgresql_database := true;
     when restriction_todos =>
        restriction_no_annotate_todos := true;
     when promptChange =>
        promptScript := expr_val;
     when session_export_script =>
        if length( sessionExportScript ) = 0 then
           sessionExportScript := expr_val;
        else
           err( "session_export_script is already defined" );
        end if;
     when session_import_script =>
        if length( sessionImportScript ) = 0 then
           sessionImportScript := expr_val;
        else
           err( "session_import_script is already defined" );
        end if;
     when software_model =>
        if softwareModelSet then
           err( "software model already set" );
        else
          declare
            id : identifier;
          begin
            findIdent( to_unbounded_string( "System.Script_Software_Model" ), id );
            if id /= eof_t then
               identifiers( id ).value := expr_val;
               softwareModelSet := true;
            end if;
          exception when others =>
            err( "exception raised" );
          end;
        end if;
     when suppress_word_quoting =>
        null; -- only applies in syntax check
     when suppress_low_priority_todos =>
        null; -- only applies in syntax check
     when suppress_all_todos =>
        null; -- only applies in syntax check
     when template | unrestricted_template =>
        if processingTemplate then
           err( "template already used" );
        elsif inputMode = interactive or inputMode = breakout then
           err( "template is not allowed in an interactive session" );
        end if;
        if var_id = eof_t  then
           templatePath := basename( scriptFilePath );
           if length( templatePath ) > 3 and then tail( templatePath, 3 ) = ".sp" then
              Delete( templatePath, length(templatePath)-2, length( templatePath ) );
           elsif length( templatePath ) > 5 and then tail( templatePath, 5 ) = ".bush" then
              Delete( templatePath, length(templatePath)-4, length( templatePath ) );
           elsif length( templatePath ) > 4 and then tail( templatePath, 4 ) = ".cgi" then
              Delete( templatePath, length(templatePath)-3, length( templatePath ) );
           end if;
           templatePath := templatePath & ".tmpl";
        else
           templatePath := identifiers( strlit_t ).value;
        end if;
        processingTemplate := true;
        if pragmaKind = unrestricted_template then
           unrestrictedTemplate := true;
        end if;
        putTemplateHeader( templateHeader );
     when test =>
        if testOpt then
           if not syntax_check then
              declare
                 savershOpt : commandLineOption := rshOpt;
              begin
                 --rshOpt := true;            -- force restricted shell mode
                 CompileRunAndCaptureOutput( expr_val, results );
                 --rshOpt := savershOpt;
                 put( results );
              end;
           end if;
        end if;
     when test_result =>
        if testOpt then
           if not syntax_check then   -- has no meaning during syntax check
              if baseTypesOk( boolean_t, var_id ) then
                 if expr_val = "0" then
                    put( standard_error, scriptFilePath );
                    put( standard_error, ":" );
                    put( standard_error, getLineNo'img );
                    put( standard_error, ": " );
                    if gccOpt then
                       put_line( standard_error, "test failed" );
                    else
                       put_line( standard_error, to_string( getCommandLine ) );
                       put_line( standard_error, "^ test failed" );
                    end if;
                 end if;
              end if;
           end if;
        end if;
     when todo =>
        null;
     when unchecked_import | unchecked_import_json =>
        -- Check for a reasonable identifier type
        if pragmaKind = unchecked_import_json then
           identifiers( var_id ).mapping := json;
           if identifiers( var_id ).class = userProcClass then
              err( "procedures cannot be imported" );
           elsif identifiers( var_id ).class = userFuncClass then
              err( "functions cannot be imported" );
           elsif identifiers( var_id ).import then
              err( "variable is already imported" );
           end if;
        else
           identifiers( var_id ).mapping := none;
           if identifiers( var_id ).class = userProcClass then
              err( "procedures cannot be imported" );
           elsif identifiers( var_id ).class = userFuncClass then
              err( "functions cannot be imported" );
           elsif identifiers( var_id ).list then
              err( "arrays cannot be imported without import_json or arrays.to_json" );
           elsif identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_record_t then
              err( "records cannot be imported without import_json or records.to_json" );
           elsif identifiers( var_id ).import then
              err( "variable is already imported" );
           elsif not uniTypesOK( identifiers( var_id ).kind, uni_string_t ) then
              err( "only string variables exported" );
           end if;
        end if;
        -- All clear? Get the value
        if not error_found then
           -- set import flag to false.  we will use this to determine if
           -- something was found.
           identifiers( var_id ).import := false;
           if importType = "local_memcache" then
              identifiers( var_id ).method := local_memcache;
              checkAndInitializeLocalMemcacheCluster;
              -- According to the SparForte manual, only update the
              -- variable if imported value is found
              declare
                 temp : unbounded_string;
              begin
                 Get( localMemcacheCluster,
                      identifiers( var_id ).name,
                      temp );
                 if length( temp ) > 0 then
                    identifiers( var_id ).import := true;
                    newValue := temp;
                 end if;
              exception when others =>
                 err( "exception raised" );
              end;
           elsif importType = "memcache" then
              identifiers( var_id ).method := memcache;
              checkAndInitializeLocalMemcacheCluster;
              -- According to the SparForte manual, only update the
              -- variable if imported value is found
              declare
                 temp : unbounded_string;
              begin
                 Get( distributedMemcacheCluster,
                      identifiers( var_id ).name,
                      temp );
                 if length( temp ) > 0 then
                    identifiers( var_id ).import := true;
                    newValue := temp;
                 end if;
              exception when others =>
                 err( "exception raised" );
              end;
           --elsif processingTemplate and importType = "cgi" then
           elsif importType = "cgi" then
              identifiers( var_id ).import := true;
              identifiers( var_id ).method := http_cgi;
              declare
                 found : boolean := false;
              begin
                 for i in 1..cgi.key_count( to_string( identifiers( var_id ).name ) ) loop
                     newValue := newValue & to_unbounded_string( cgi.value(
                         to_string( identifiers( var_id ).name ), 1, true ) );
                     found := true;
                  end loop;
                  if found then
              -- apply mapping, if any.  assume these are all set correctly
                     if identifiers( var_id ).mapping = json then                       -- json
                        if getUniType( identifiers( var_id ).kind ) = uni_string_t then -- string
                           DoJsonToString( identifiers( var_id ).value, newValue );
                        elsif identifiers( var_id ).list then                           -- array
                           DoJsonToArray( var_id, newValue );
                        elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_record_t then -- record
                           DoJsonToRecord( var_id, newValue );
                        elsif getUniType( identifiers( var_id ).kind ) = uni_numeric_t then -- number
                           DoJsonToNumber( newValue, identifiers( var_id ).value );
                        elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_enumerated_t then -- enum
                           DoJsonToNumber( newValue, identifiers(var_id ).value );
                        else
                           err( "internal error: unexpected import translation type" );
                        end if;
                     else                                                           -- no mapping
                        identifiers( var_id ).value := newValue;
                     end if;
                  end if;
              end;
           elsif importType = "session" then
              identifiers( var_id ).import := true;
              identifiers( var_id ).method := session;
              err( "session import not yet implemented" );
           --elsif not processingTemplate and importType = "cgi" then
           --    err( "import type cgi must be used in a template" );
           else
              if inEnvironment( var_id ) then
                 identifiers( var_id ).import := true;
                 identifiers( var_id ).method := shell;
                   refreshVolatile( var_id );
              end if;
           end if;
           -- show a trace of what's imported.  If JSON, show JSON as it
           -- could be multiple values
           if trace then
               put_trace(
                  to_string( identifiers( var_id ).name ) & " := """ &
                  to_string( ToEscaped( newValue ) ) &
                  """" );
           end if;
           -- whether imported or not, set it to true because it may be used
           -- later with volatile to try to find a value.
           identifiers( var_id ).import := true;
        end if;

     when inspect_var =>
        identifiers( var_id ).inspect := true;
     when uninspect_var =>
        identifiers( var_id ).inspect := false;
     when volatile =>
        identifiers( var_id ).volatile := true;
     when others =>
        err( "Internal error: unable to execute pragma" );
     end case;
  end if;
end ParsePragmaStatement;


--  PARSE PRAGMA
--
-- Syntax: pragma kind pragma-params [@ pragma-params]..; |
-- pragma is kind pragma-params [@ pragma-params]...; ... end pragma
-----------------------------------------------------------------------------

procedure ParsePragma is
  pragmaKind : aPragmaKind;
begin
  expect( pragma_t );
  if token = is_t then
     if onlyAda95 then
        err( "pragma block is not allowed with " & optional_bold( "pragma ada_95" ) );
     end if;
     -- a pragma block
     expect( is_t );
     -- empty block?
     if token = end_t then
        err( "pragma name missing" );
     elsif token = pragma_t then
        err( "single pragma in a pragma block" );
     end if;
     -- examine the name of the pragma and return a pragma kind matching the
     -- name
     pragmaKind := parsePragmaKind;
     while token /= eof_t and token /= end_t loop
        -- an error check
        ParsePragmaStatement( pragmaKind );
        if token = symbol_t and identifiers( symbol_t ).value = to_unbounded_string( "@" ) then
           if onlyAda95 then
              err( "@ is not allowed with " & optional_bold( "pragma ada_95" ) );
           end if;
           expect( symbol_t, "@" );
        elsif token = symbol_t and identifiers( symbol_t ).value = to_unbounded_string( ";" ) then
           expect( symbol_t, ";" );
           if token = pragma_t then
              err( "single pragma in a pragma block" );
           end if;
           if token /= end_t then
              pragmaKind := parsePragmaKind;
           end if;
        else
           -- bit of a more descriptive error
           err( "'@' or ';' expected" );
        end if;
     end loop;
     expect( end_t );
     expect( pragma_t );
  else
     -- A single pragma
     pragmaKind := parsePragmaKind;
     loop
        ParsePragmaStatement( pragmaKind );
        -- bit of a more descriptive error
        if token = symbol_t and identifiers( symbol_t ).value = to_unbounded_string( "(" ) then
           err( "'@' or ';' expected" );
        end if;
        exit when done or token = eof_t or (token = symbol_t and identifiers( symbol_t ).value /= to_unbounded_string( "@" ) );
        if onlyAda95 then
           err( "@ is not allowed with " & optional_bold( "pragma ada_95" ) );
        end if;
        expect( symbol_t, "@" );
     end loop;
  end if;
end ParsePragma;

end parser_pragmas;
