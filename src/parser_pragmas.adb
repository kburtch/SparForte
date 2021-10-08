------------------------------------------------------------------------------
-- AdaScript Language Parser - Pragmas                                      --
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
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );
with  ada.text_io,
    ada.strings.unbounded.text_io,
    ada.calendar,
    gnat.source_info,
    cgi,
    pegasock.memcache,
    world,
    reports.test,
    pegasoft.strings,
    pegasoft.user_io,
    pegasoft.script_io,
    compiler,
    scanner,
    scanner.communications,
    signal_flags,
    jobs, -- for clearCommandHash
    parser.decl.as,
    parser_teams;
use ada.text_io,
    ada.command_line,
    ada.command_line.environment,
    ada.strings.unbounded,
    ada.strings.unbounded.text_io,
    ada.calendar,
    pegasock.memcache,
    pegasoft.user_io,
    pegasoft.script_io,
    world,
    reports,
    reports.test,
    pegasoft.strings,
    compiler,
    scanner,
    scanner.communications,
    signal_flags,
    jobs,
    parser,
    parser.decl.as,
    parser_teams;

package body parser_pragmas is

-- Types of Pragmas

type aPragmaKind is (
     ada_95,
     advise,
     affinity,
     asserting,
     assumption,
     assumption_applied,
     assumption_factor,
     assumption_used,
     assumption_written,
     annotate,
     blocked,
     clarify,
     colour_messages,
     constraint,
     debug,
     debug_on,
     declare_affinity,
     declare_constraint,
     depreciated,
     dispute,
     error,
     export,
     export_json,
     gcc_errors,
     import,
     import_json,
     inspection,
     inspect_var,
     license,
     manual_test,
     manual_test_result,
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
     restriction_declarations,
     restriction_external,
     restriction_memcache,
     restriction_mysql,
     restriction_postgresql,
     restriction_todos,
     restriction_volatiles,
     session_export_script,
     session_import_script,
     software_model,
     suppress,
     suppress_word_quoting,
     suppress_low_priority_todos,
     suppress_all_todos,
     suppress_no_empty_command_subs,
     template,
     test,
     test_report,
     test_result,
     todo,
     unchecked_import,
     unchecked_import_json,
     unchecked_volatile,
     uninspect_var,
     unrestricted_template,
     volatile,
     unknown_pragma
   );

--testReport : xmlTestReport;
--testReport : textTestReport;

-- TODO: this is ugly

usingTextTestReport : boolean := false; -- TODO: if we must do this, use enum.
myTextTestReport : textTestReport;
myXmlTestReport  : xmlTestReport;
reportPath       : unbounded_string;


-----------------------------------------------------------------------------
--  PARSE PRAGMA KIND
--
-- Check the current token for the kind of pragma and advance the scanner.
-----------------------------------------------------------------------------

function parsePragmaKind return aPragmaKind is
  name : constant string := to_string( identifiers( token ).name );
  pragmaKind : aPragmaKind := unknown_pragma;
begin
   -- just an error message...if ( with no name
   if token = symbol_t and identifiers( symbol_t ).value.all = to_unbounded_string( "(" ) then
      err( "pragma name missing" );
  elsif name = "ada_95" then
     pragmaKind := ada_95;
  elsif name = "advise" then
     pragmaKind :=  advise;
  elsif name = "affinity" then
     pragmaKind :=  affinity;
  elsif name = "assert" then
     pragmaKind :=  asserting;
  elsif name = "assumption" then
     pragmaKind :=  assumption;
  elsif name = "debug" then
     pragmaKind :=  debug;
  elsif name = "constraint" then
     pragmaKind :=  constraint;
  elsif name = "declare_affinity" then
     pragmaKind :=  declare_affinity;
  elsif name = "declare_constraint" then
     pragmaKind :=  declare_constraint;
  elsif name = "annotate" then
     pragmaKind :=  annotate;
  elsif name = "blocked" then
     pragmaKind :=  blocked;
  elsif name = "clarify" then
     pragmaKind :=  clarify;
  elsif name = "color_messages" then
     pragmaKind := colour_messages;
  elsif name = "colour_messages" then
     pragmaKind := colour_messages;
  elsif name = "deprecated" then
     pragmaKind :=  depreciated;
  elsif name = "depreciated" then
     pragmaKind :=  depreciated;
  elsif name = "dispute" then
     pragmaKind :=  dispute;
  elsif name = "error" then
     pragmaKind := error;
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
  elsif name = "manual_test" then
     pragmaKind :=  manual_test;
  elsif name = "manual_test_result" then
     pragmaKind :=  manual_test_result;
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
  elsif name = "test_report" then
     pragmaKind :=  test_report;
  elsif name = "test_result" then
     pragmaKind :=  test_result;
  elsif name = "todo" then
     pragmaKind :=  todo;
  elsif name = "unchecked_import" then
     pragmaKind := unchecked_import;
  elsif name = "unchecked_import_json" then
     pragmaKind := unchecked_import_json;
  elsif name = "unchecked_volatile" then
     pragmaKind := unchecked_volatile;
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


-----------------------------------------------------------------------------
--  PARSE ANNOTATE KIND
--
-- Check the current token for the kind of annotation and advance the scanner.
-----------------------------------------------------------------------------

procedure ParseAnnotateKind is
  name : constant string := to_string( identifiers( token ).name );
  exprVal  : unbounded_string;
  exprType : identifier;
begin
  annotationsFound := true;
  if token /= strlit_t and token /= charlit_t and not identifiers( token ).static then
     annotationTodoFound := name = "todo";
     -- param and return are singular but they are plural in the report class.
     -- they probably should be plural the the annotation but it wasn't
     -- important enough to fix.
     if name /= "account" and
        name /= "author" and
        name /= "created" and
        name /= "category" and
        name /= "contents" and
        name /= "description" and
        name /= "errors" and
        name /= "examples" and
        name /= "exceptions" and
        name /= "footer" and
        name /= "icon" and
        name /= "modified" and
        name /= "param" and
        name /= "rationale" and
        name /= "return" and
        name /= "screenshot" and
        name /= "see_also" and
        name /= "summary" and
        name /= "todo" and
        name /= "version" then
        err( "unknown annotation field type" );
     else
        ParsePragmaIdentifier;
        expectPragmaComma;
     end if;
  end if;
  -- A team.member variable for the author doesn't work because variables
  -- aren't declared when help runs.
  ParseStaticExpression( exprVal, exprType );
  baseTypesOK( exprType, uni_string_t );
end ParseAnnotateKind;


-----------------------------------------------------------------------------
--  PARSE ASSUMPTION KIND
--
-- Assumptions are workarounds...
----------------------------------------------------------------------------

procedure ParseAssumptionKind( var_id : out identifier; assumeKind: out aPragmaKind ) is
  name_unbounded : constant unbounded_string := identifiers( token ).name;
  name : constant string := to_string( name_unbounded );
begin
  if name = "used" then
     assumeKind := assumption_used;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  elsif name = "written" then
     assumeKind := assumption_written;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  elsif name = "applied" then
     assumeKind := assumption_applied;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  elsif name = "factor" then
     assumeKind := assumption_factor;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  else
     assumeKind := assumption;
     discardUnusedIdentifier( token );
     err( "only 'applied', 'factor', 'used', 'written' assumptions supported" );
  end if;
end ParseAssumptionKind;


-----------------------------------------------------------------------------
--  PARSE IMPORT KIND
--
-----------------------------------------------------------------------------

procedure ParseImportKind( var_id : out identifier; importKind: out unbounded_string ) is
  name_unbounded : constant unbounded_string := identifiers( token ).name;
  name : constant string := to_string( name_unbounded );
begin
  if name = "shell" then
     importKind := name_unbounded;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  elsif name = "cgi" then
     importKind := name_unbounded;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  elsif name = "local_memcache" then
     if restriction_no_memcache then
        err( "cannot be used " & bold( "pragma restriction( no_memcache )" ) );
     end if;
     importKind := name_unbounded;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  elsif name = "memcache" then
     if restriction_no_memcache then
        err( "cannot be used " & bold( "pragma restriction( no_memcache )" ) );
     end if;
     importKind := name_unbounded;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  elsif name = "session" then
     importKind := name_unbounded;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  else
     importKind := null_unbounded_string;
     discardUnusedIdentifier( token );
     err( "only 'cgi', 'shell', 'local_memcache', 'memcache', 'session' convention supported" );
  end if;
end ParseImportKind;


-----------------------------------------------------------------------------
--  PARSE EXPORT KIND
--
-----------------------------------------------------------------------------

procedure ParseExportKind( var_id : out identifier; exportKind: out unbounded_string ) is
  name_unbounded : constant unbounded_string := identifiers( token ).name;
  name : constant string := to_string( name_unbounded );
begin
  if name = "shell" then
     exportKind := name_unbounded;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  elsif name = "local_memcache" then
     exportKind := name_unbounded;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  elsif name = "memcache" then
     exportKind := name_unbounded;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  elsif name = "session" then
     exportKind := name_unbounded;
     ParsePragmaIdentifier;
     expectPragmaComma;
     ParseIdentifier( var_id );
  else
     exportKind := null_unbounded_string;
     discardUnusedIdentifier( token );
     err( "only 'shell', 'local_memcache', 'memcache', 'session' convention supported" );
  end if;
end ParseExportKind;


-----------------------------------------------------------------------------
--  PARSE LICENSE KIND
--
-- Check the current token for the kind of license and advance the scanner.
-----------------------------------------------------------------------------

procedure ParseLicenseKind( expr_val : out unbounded_string ) is
  name_unbounded : unbounded_string;

  procedure ParseLicenseExtra is
  begin
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        expr_val := expr_val & ": " & identifiers( token ).value.all;
        expect( strlit_t );
     end if;
  end ParseLicenseExtra;

begin
  ParsePragmaIdentifier( name_unbounded );
  expr_val := null_unbounded_string;
  declare
     name : constant string := to_string( name_unbounded );
  begin
     if name = "unrestricted" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "gpl" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "gplv2" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "gplv3" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "agpl" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "bsd_original" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "bsd_revised" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "artistic" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "mit" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "apache" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "apache_2" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "freeware" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "shareware" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "public_domain" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "commercial" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     elsif name = "restricted" then
        expr_val := name_unbounded;
        ParseLicenseExtra;
     else
        err( "unknown license " & bold( name ) );
     end if;
   end;
end ParseLicenseKind;


-----------------------------------------------------------------------------
--  PARSE SOFTWARE MODEL NAME
--
-- Check the current token for the kind of model and advance the scanner.
-----------------------------------------------------------------------------

procedure ParseSoftwareModelName( expr_val : out unbounded_string ) is
  name_unbounded : unbounded_string;
begin
  -- special case: package is a keyword and will cause an error with
  -- PragmaIdentifier.
  if token = package_t then
     name_unbounded := identifiers( token ).name;
     getNextToken;
  else
     ParsePragmaIdentifier( name_unbounded );
  end if;
  declare
     name : constant string := to_string( name_unbounded );
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
     elsif name = "nonstandard" then
        expr_val := name_unbounded;
     end if;
     if length( expr_val ) = 0 then
        err( "unknown software model " & bold( name ) );
     end if;
  end;
end ParseSoftwareModelName;


-----------------------------------------------------------------------------
--  PARSE WORK ESTIMATE
--
-- Syntax: measure, value
-- Where measure can be size, hours, feature points, story points or sloc
-- Used by pragma todo and pragma manual_test
-----------------------------------------------------------------------------

work_estimate_unknown : boolean;

procedure ParseWorkEstimate( work_estimate_unknown : out boolean ) is
   unused_bool : boolean;
   var_id : identifier;
begin
  -- the work estimate measure

  ParseIdentifier( var_id );
  unused_bool := baseTypesOK( identifiers( var_id ).kind, teams_work_measure_t );
  expectPragmaComma;
  work_estimate_unknown := false;

  -- the work estimate value

  if var_id = teams_work_measure_unknown_t then
     expect( number_t, " 0" );
     work_estimate_unknown := true;
  elsif var_id = teams_work_measure_size_t then
     if identifiers( token ).value.all /= "s" and
        identifiers( token ).value.all /= "m" and
        identifiers( token ).value.all /= "l" and
        identifiers( token ).value.all /= "xl" then
        err( "expected ""s"", ""m"", ""l"" or ""xl""" );
     end if;
    expect( strlit_t );
  elsif var_id = teams_work_measure_hours_t or
        var_id = teams_work_measure_fpoints_t or
        var_id = teams_work_measure_spoints_t or
        var_id = teams_work_measure_sloc_t then
    expect( number_t );
  else
     err( gnat.source_info.source_location & ": internal error: don't know how to handle this type of work measure value" );
  end if;
  expectPragmaComma;
end ParseWorkEstimate;


-----------------------------------------------------------------------------
--  PARSE WORK PRIORITY
--
-- Syntax: measure, value
-- Where measure can be unknown, completed, level, severity, risk, cvss
-- Used by pragma todo and pragma manual_test
-- is_todo is true if this is pragma todo, which triggers addition checks.
-----------------------------------------------------------------------------

procedure ParseWorkPriority( work_estimate_unknown : boolean; is_todo : boolean := true ) is
   unused_bool : boolean;
   var_id : identifier;
begin
  -- the work priority measure

  ParseIdentifier( var_id );
  unused_bool := baseTypesOK( identifiers( var_id ).kind, teams_work_priority_t );
  expectPragmaComma;

  -- the work priority value

  if var_id = teams_work_priority_unknown_t then
     expect( number_t, " 0" );
  elsif var_id = teams_work_priority_completed_t then
     expect( number_t, " 0" );
  elsif var_id = teams_work_priority_level_t then
     if identifiers( token ).value.all /= "l" and
        identifiers( token ).value.all /= "m" and
        identifiers( token ).value.all /= "h" then
        err( "expected 'l', 'm' or 'h'" );
     end if;
     if is_todo then
        if not work_estimate_unknown and not allowAllTodosForRelease then
           if boolean( testOpt ) or boolean( maintenanceOpt ) then
              if allowLowPriorityTodosForRelease and identifiers( token ).value.all = "l" then
                 null;
              else
                 err( "priority todo task not yet completed" );
              end if;
           end if;
        end if;
     end if;
     expect( charlit_t );
  elsif var_id = teams_work_priority_severity_t then
     if identifiers( token ).value.all < " 1" or
        identifiers( token ).value.all > " 5" then
        err( "expected 1..5" );
     end if;
     if is_todo then
        if not work_estimate_unknown and not allowAllTodosForRelease then
           if boolean( testOpt ) or boolean( maintenanceOpt ) then
              if allowLowPriorityTodosForRelease and identifiers( token ).value.all < " 2" then
                 null;
              else
                 err( "priority todo task not yet completed" );
              end if;
           end if;
        end if;
     end if;
     expect( number_t );
  elsif var_id = teams_work_priority_risk_t then
     if is_todo then
        if not work_estimate_unknown and not allowAllTodosForRelease then
           if boolean( testOpt ) or boolean( maintenanceOpt ) then
              -- any financial risk
              if identifiers( token ).value.all /= " 0" then
                 err( "priority todo task not yet completed" );
              end if;
           end if;
        end if;
     end if;
     expect( number_t );
  elsif var_id = teams_work_priority_cvss_t then
     declare
        v1 : long_float;
     begin
        v1 := to_numeric( identifiers( token ).value.all );
        if v1 < 0.0 or v1 > 10.0 then
           err( "expected 1..10" );
        end if;
        if is_todo then
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
        end if;
     exception when others => null;
     end;
     expect( number_t );
  else
     err( gnat.source_info.source_location & ": internal error: don't know how to handle this type of work priority value" );
  end if;
end ParseWorkPriority;


-----------------------------------------------------------------------------
--  RUN TEST CASE
--
-- Run a test case and report the result.
-----------------------------------------------------------------------------

procedure run_test_case( testScript, testCaseName : unbounded_string; manual_test : boolean := false ) is
   --savershOpt : commandLineOption := rshOpt;
   save_error_found : constant boolean := error_found;
   isTesting_old : constant boolean := isTesting;
   results     : unbounded_string;
begin

   -- If this is the first test, we need to initialize JUnit support

   if not isJunitStarted then
      begin
        if usingTextTestReport then
           startJunit( myTextTestReport );
        else
           startJunit( myXmlTestReport, reportPath );
        end if;
      exception when others =>
        err( "exception while creating test result file" );
      end;
   end if;

   -- If this is a new test case, close off the previous test case.

   begin
     if isJunitTestCaseStarted then
        if usingTextTestReport then
           endJunitTestCase( myTextTestReport );
        else
           endJunitTestCase( myXmlTestReport );
        end if;
     end if;
     if usingTextTestReport then
        checkForNewTestSuite( myTextTestReport );
     else
        checkForNewTestSuite( myXmlTestReport );
     end if;
     if usingTextTestReport then
        startJunitTestCase( myTextTestReport, testCaseName, testCaseName );
     else
        startJunitTestCase( myXmlTestReport,  testCaseName, testCaseName );
     end if;
   exception when others =>
      err( "exception while writing to test result file" );
   end;

   -- Evaluate the test result by evaluating the expression.
   -- Output the result of the test.  If an error occurred,
   -- log it as such for the test case.  Then recover the
   -- original error flag as an error during a test should
   -- not abort the test.
   --
   -- A manual test case is a description of what the testing person
   -- would do and has no automated test to run.
   --
   -- While running a test in a restricted shell would help protect
   -- against state change during testing, it turns out to be too
   -- restrictive.

   if not manual_test then
      begin
        isTesting := true;
        CompileRunAndCaptureOutput( testScript, results );
        isTesting := isTesting_old;
        put( results );
        if error_found then
           if usingTextTestReport then
              testCaseError( myTextTestReport );
           else
              testCaseError( myXmlTestReport );
           end if;
        end if;
        -- Script must continue to run even if an error occurred in the
        -- test subscript.
        error_found := save_error_found;
      exception when others =>
        if usingTextTestReport then
           testCaseError( myTextTestReport );
        else
           testCaseError( myXmlTestReport );
        end if;
        error_found := save_error_found;
      end;
   end if; -- automated test
end run_test_case;


-----------------------------------------------------------------------------
--  RECORD TEST RESULT
--
-- Record the test result
-----------------------------------------------------------------------------

procedure record_test_result( result_status : boolean; test_message : unbounded_string ) is
begin
  if not isJunitStarted then
     err( optional_yellow( "pragma test" ) & " must be used before pragma test_result" );
  elsif result_status then
     if usingTextTestReport then
        testCaseFailure( myTextTestReport, test_message );
    else
        testCaseFailure( myXmlTestReport, test_message );
    end if;
  else
    if usingTextTestReport then
       testCaseSuccess( myTextTestReport );
    else
       testCaseSuccess( myXmlTestReport );
    end if;
  end if;
end record_test_result;


-----------------------------------------------------------------------------
--  ENFORCE CONSTRAINT
--
-- Apply a constraint, checking the mode and the weight.
-----------------------------------------------------------------------------

procedure enforceConstraint( constraint, name : unbounded_string;
  weight : float ) is
  dc     : aDesignConstraint;
  dcPos  : DesignConstraintLists.aListIndex := 0;
  edc    : anEnforcedDesignConstraint;
  edcPos : EnforcedDesignConstraintLists.aListIndex := 0;
  fullUnitName : unbounded_string;
  isUnique : boolean;
begin

  -- Weight must be non-zero

  if weight < 0.0 then
     err( "weight " & optional_yellow( weight'img ) & " is less than zero" );
     return;
  end if;
  if name = "" then
      err( "constraint name should not be an empty string" );
      return;
  end if;

  -- Lookup the definition of the constraint

  dc.mode := undefined;
  dc.constraint := constraint;
  dc.name := name;
  dc.limit := 0.0;
  DesignConstraintLists.Find( designConstraintList, dc, foundAt => dcPos  );
  if dcPos = 0 then
     err( "constraint " &
          optional_yellow( to_string( constraint ) ) &
          ", " &
          optional_yellow( to_string( name ) ) &
          " is not declared" );
    return;
  end if;
  DesignConstraintLists.Find( designConstraintList, dcPos, dc  );

 -- Look up the last use of the constraint.  Note this Find procedure
 -- will looking up the category, as only one entry per category is
 -- allowed.

  --edc.mode := dc.mode;
  edc.mode := undefined;
  edc.enforcedFile := getSourceFileName;
  edc.constraint := constraint;
  edc.name := name;
  edc.weight := 0.0;

  if dc.mode = subprogram then
-- TODO: effect of nesting units
     GetFullParentUnitName( fullUnitName, isUnique  );
     edc.enforcedUnit := fullUnitName;
     EnforcedLocalDesignConstraintLists.Find( enforcedLocalDesignConstraintList, edc, foundAt => edcPos  );
     if edcPos > 0 then
        EnforcedLocalDesignConstraintLists.Find( enforcedLocalDesignConstraintList, edcPos, edc );
    end if;
  else
    EnforcedDesignConstraintLists.Find( enforcedDesignConstraintList, edc, foundAt => edcPos  );
    if edcPos > 0 then
       EnforcedDesignConstraintLists.Find( enforcedDesignConstraintList, edcPos, edc );
    end if;
  end if;

  --if edcPos = 0 then
  --      put_line(
  --                edc.mode'img & "/" &
  --                to_string( edc.enforcedFile ) & "/" &
  --                to_string( edc.constraint ) & "/" &
  --                to_string( edc.name ) & "/" &
  --                to_string( edc.enforcedUnit ) &
  --                " not found" );
  --else
  --      put_line(
  --                edc.mode'img & "/" &
  --                to_string( edc.enforcedFile ) & "/" &
  --                to_string( edc.constraint ) & "/" &
  --                to_string( edc.name ) & "/" &
  --                to_string( edc.enforcedUnit ) &
  --                " found" );
--
 -- end if;

  -- If this constraint is enforced already...

  if edcPos > 0 then

     case edc.mode is

     -- For a unique constraint, if the category exists, it is a failure
     -- to create a second one for that category.
     -- Otherwise, apply the constraint.

     when unique =>

        -- We don't accumulate weight because it's an automatic error to
        -- have two unique constraints.

        err( "unique constraint " &
              optional_yellow( to_string( edc.constraint ) ) &
              " value " &
              optional_yellow( to_string( name ) ) &
              " conflicts with " &
              optional_yellow( to_string( edc.name ) ) &
              " (at " &
                to_string( edc.enforcedFile ) & ":" &
              edc.enforcedAt'img & ")" );

    -- For an exclusive constraint, if the category exists but has a different
    -- name, it is a failure.  The identical category and name is permitted.
    -- Otherwise, do nothing because the constraint is repeated.

    when file =>

        -- Accumulate weight

        edc.weight := edc.weight + weight;

        --if edc.constraint = constraint and edc.name /= name then
        if edc.name /= name then
           err( "file constraint " &
                optional_yellow( to_string( edc.constraint ) ) &
                " value " &
                optional_yellow( to_string( name ) ) &
                " conflicts with " &
                optional_yellow( to_string( edc.name ) ) &
                " (at " &
                  to_string( edc.enforcedFile ) & ":" &
                edc.enforcedAt'img & ")" );
        --elsif edc.constraint = constraint and edc.name = name and
        elsif edc.weight > dc.limit then
           err( "file constraint " &
                optional_yellow( to_string( edc.constraint ) ) &
                " accumulated weight" &
                optional_yellow( edc.weight'img ) &
                " exceeds the limit of" &
                optional_yellow( dc.limit'img ) );
        elsif edc.weight /= weight then
           EnforcedDesignConstraintLists.Replace( enforcedDesignConstraintList, edcPos, edc );
        end if;

    when subprogram =>

     -- put_line( to_string( edc.enforcedUnit ) & " vs " & to_string( fullUnitName ) );
     -- put_line( to_string( edc.constraint )   & " vs " & to_string( constraint ) );
     -- put_line( to_string( edc.name )         & " vs " & to_string( name ) );

        -- Accumulate weight

        edc.weight := edc.weight + weight;

        if edc.name /= name then
           err( -- optional_yellow( to_string( edc.enforcedUnit ) ) &
                -- " local constraint " &
                "local constraint " &
                optional_yellow( to_string( edc.constraint ) ) &
                " value " &
                optional_yellow( to_string( name ) ) &
                " conflicts with " &
                optional_yellow( to_string( edc.name ) ) &
                " (at " &
                  to_string( edc.enforcedFile ) & ":" &
                edc.enforcedAt'img & ")" );
        elsif edc.weight > dc.limit then
           err( "local constraint " &
                optional_yellow( to_string( edc.constraint ) ) &
                " accumulated weight" &
                optional_yellow( edc.weight'img ) &
                " exceeds the limit of" &
                optional_yellow( dc.limit'img ) );
        elsif edc.weight /= weight then
           EnforcedLocalDesignConstraintLists.Replace( enforcedLocalDesignConstraintList, edcPos, edc );
        end if;

    when others =>
       err( Gnat.Source_Info.Source_Location & "internal_error: unexpected constraint mode" );
    end case;

  else

     -- If the constraint category is unused, it is safe to apply the
     -- constraint.

     edc.mode := dc.mode;
     edc.enforcedAt := getLineNo;
     edc.enforcedFile := getSourceFileName;
     edc.enforcedUnit := null_unbounded_string;
     edc.constraint := constraint;
     edc.name := name;
     edc.weight := weight;

     case edc.mode is
     when unique =>
        -- Even though unique constraints can only be used once, they can still
        -- have a weight too high on their first use.
        if edc.weight > dc.limit then
           err( "unique constraint " &
                optional_yellow( to_string( edc.constraint ) ) &
                " weight" &
                optional_yellow( weight'img ) &
                " exceeds limit of" &
                optional_yellow( dc.limit'img ) );
        else
           EnforcedDesignConstraintLists.Queue( enforcedDesignConstraintList, edc );
        end if;
     when file =>
        if edc.weight > dc.limit then
           err( "file constraint " &
                optional_yellow( to_string( edc.constraint ) ) &
                " weight" &
                optional_yellow( weight'img ) &
                " exceeds limit of" &
                optional_yellow( dc.limit'img ) );
        else
           EnforcedDesignConstraintLists.Queue( enforcedDesignConstraintList, edc );
        end if;
     when subprogram =>
        if edc.weight > dc.limit then
           err( "file constraint " &
                optional_yellow( to_string( edc.constraint ) ) &
                " weight" &
                optional_yellow( weight'img ) &
                " exceeds limit of" &
                optional_yellow( dc.limit'img ) );
        else
           edc.enforcedUnit := fullUnitName;
           EnforcedLocalDesignConstraintLists.Queue( enforcedLocalDesignConstraintList, edc );
        end if;
    when others =>
       err( Gnat.Source_Info.Source_Location & "internal_error: unexpected constraint mode" );
    end case;

    -- if dc.mode = local then
    --    -- put_line(
    --    --           edc.mode'img & "/" &
    --    --           to_string( edc.enforcedFile ) & "/" &
    --    --           to_string( edc.constraint ) & "/" &
    --    --           to_string( edc.name ) & "/" &
    --    --           to_string( edc.enforcedUnit ) &
    --    --           " local save" );
    --    edc.enforcedUnit := fullUnitName;
    --    EnforcedLocalDesignConstraintLists.Queue( enforcedLocalDesignConstraintList, edc );
    -- else
    --    edc.enforcedUnit := null_unbounded_string;
    --    EnforcedDesignConstraintLists.Queue( enforcedDesignConstraintList, edc );
    -- end if;
  end if;
end enforceConstraint;


-----------------------------------------------------------------------------
--  DECLARE CONSTRAINT
--
-- Define the constraints to be used with pragma constraint.
-----------------------------------------------------------------------------

procedure declareConstraint( mode : designConstraintModes; constraint, name : unbounded_string; limit : float ) is
  dc : aDesignConstraint;
  pos : DesignConstraintLists.aListIndex := 0;
begin
  if limit < 0.0 then
     err( "limit " & optional_yellow( limit'img ) & " is less than zero" );
  else
     dc.mode := mode;
     dc.constraint := constraint;
     dc.name := name;
     dc.limit := limit;
     DesignConstraintLists.Find( designConstraintList, dc, foundAt => pos  );
     if pos > 0 then
        err( "constraint " &
             optional_yellow( to_string( constraint ) ) &
             ", " &
             optional_yellow( to_string( name ) ) &
             " is already declared" );
     else
        DesignConstraintLists.Queue( designConstraintList, dc );
     end if;
  end if;
end declareConstraint;


-----------------------------------------------------------------------------
--  ENFORCE AFFINITY
--
-- Apply an affinity, checking the mode and the weight.
-----------------------------------------------------------------------------

procedure enforceAffinity( affinity : unbounded_string;
  weight : float ) is
  da     : aDesignAffinity;
  daPos  : DesignAffinityLists.aListIndex := 0;
  eda    : anEnforcedDesignAffinity;
  edaPos : EnforcedDesignAffinityLists.aListIndex := 0;
  fullUnitName : unbounded_string;
  isUnique : boolean;
begin

  -- Weight must be non-zero

  if weight < 0.0 then
     err( "weight " & optional_yellow( weight'img ) & " is less than zero" );
     return;
  end if;

  -- Lookup the definition of the affinity

  da.mode := undefined;
  da.affinity := affinity;
  da.limit := 0.0;
  DesignAffinityLists.Find( designAffinityList, da, foundAt => daPos  );
  if daPos = 0 then
     err( "affinity " &
          optional_yellow( to_string( affinity ) ) &
          " is not declared" );
    return;
  end if;
  DesignAffinityLists.Find( designAffinityList, daPos, da  );

 -- Look up the last use of the affinity.  Note this Find procedure
 -- will looking up the category, as only one entry per category is
 -- allowed.

  eda.mode := undefined;
  eda.enforcedFile := getSourceFileName;
  eda.affinity := affinity;
  eda.weight := 0.0;

  if da.mode = subprogram then
-- TODO: effect of nesting units
     GetFullParentUnitName( fullUnitName, isUnique  );
     eda.enforcedUnit := fullUnitName;
     EnforcedLocalDesignAffinityLists.Find( enforcedLocalDesignAffinityList, eda, foundAt => edaPos  );
     if edaPos > 0 then
        EnforcedLocalDesignAffinityLists.Find( enforcedLocalDesignAffinityList, edaPos, eda );
    end if;
  else
    EnforcedDesignAffinityLists.Find( enforcedDesignAffinityList, eda, foundAt => edaPos  );
    if edaPos > 0 then
       EnforcedDesignAffinityLists.Find( enforcedDesignAffinityList, edaPos, eda );
    end if;
  end if;


  -- If this affinity is enforced already...

  if edaPos > 0 then

     case eda.mode is

    -- For an inclusive affinity, if the category exists but has a different
    -- name, it is a failure.  The identical category and name is permitted.
    -- Otherwise, do nothing because the constraint is repeated.

    when file =>

        -- Accumulate weight

        eda.weight := eda.weight + weight;

        if eda.enforcedFile /= getSourceFileName then
           err( "file affinity " &
                optional_yellow( to_string( eda.affinity ) ) &
                " is enforced in at least two files (at " &
                  to_string( eda.enforcedFile ) & ":" &
                eda.enforcedAt'img & ")" );
        elsif eda.weight > da.limit then
           err( "file affinity " &
                optional_yellow( to_string( eda.affinity ) ) &
                " accumulated weight" &
                optional_yellow( eda.weight'img ) &
                " exceeds the limit of" &
                optional_yellow( da.limit'img ) );
        elsif eda.weight /= weight then
           EnforcedDesignAffinityLists.Replace( enforcedDesignAffinityList, edaPos, eda );
        end if;

    when subprogram =>

        -- Accumulate weight

        eda.weight := eda.weight + weight;

        if eda.enforcedUnit /= fullUnitName then
           err( "subprogram affinity " &
                optional_yellow( to_string( eda.affinity ) ) &
                " is enforced in at least two subprograms (at " &
                  to_string( eda.enforcedFile ) & ":" &
                eda.enforcedAt'img & ")" );
        elsif eda.weight > da.limit then
           err( "local affinity " &
                optional_yellow( to_string( eda.affinity ) ) &
                " accumulated weight" &
                optional_yellow( eda.weight'img ) &
                " exceeds the limit of" &
                optional_yellow( da.limit'img ) );
        elsif eda.weight /= weight then
           EnforcedLocalDesignAffinityLists.Replace( enforcedLocalDesignAffinityList, edaPos, eda );
        end if;

    when others =>
       err( Gnat.Source_Info.Source_Location & "internal_error: unexpected affinity mode" );
    end case;

  else

     -- If the constraint category is unused, it is safe to apply the
     -- affinity.

     eda.mode := da.mode;
     eda.enforcedAt := getLineNo;
     eda.enforcedFile := getSourceFileName;
     eda.enforcedUnit := null_unbounded_string;
     eda.affinity := affinity;
     eda.weight := weight;

     case eda.mode is
     when file =>
        if eda.weight > da.limit then
           err( "file affinity " &
                optional_yellow( to_string( eda.affinity ) ) &
                " weight" &
                optional_yellow( weight'img ) &
                " exceeds limit of" &
                optional_yellow( da.limit'img ) );
        else
           EnforcedDesignAffinityLists.Queue( enforcedDesignAffinityList, eda );
        end if;
     when subprogram =>
        if eda.weight > da.limit then
           err( "subprogram affinity " &
                optional_yellow( to_string( eda.affinity ) ) &
                " weight" &
                optional_yellow( weight'img ) &
                " exceeds limit of" &
                optional_yellow( da.limit'img ) );
        else
           eda.enforcedUnit := fullUnitName;
           EnforcedLocalDesignAffinityLists.Queue( enforcedLocalDesignAffinityList, eda );
        end if;
    when others =>
       err( Gnat.Source_Info.Source_Location & "internal_error: unexpected affinity mode" );
    end case;

  end if;
end enforceAffinity;


-----------------------------------------------------------------------------
--  DECLARE AFFINITY
--
-- Define the affinities to be used with pragma constraint.
-----------------------------------------------------------------------------

procedure declareAffinity( mode : designAffinityModes; affinity : unbounded_string; limit : float ) is
  da  : aDesignAffinity;
  pos : DesignAffinityLists.aListIndex := 0;
begin
  if limit < 0.0 then
     err( "limit " & optional_yellow( limit'img ) & " is less than zero" );
  else
     da.mode := mode;
     da.affinity := affinity;
     da.limit := limit;
     DesignAffinityLists.Find( designAffinityList, da, foundAt => pos  );
     if pos > 0 then
        err( "affinity " &
             optional_yellow( to_string( affinity ) ) &
             " is already declared" );
     else
        DesignAffinityLists.Queue( designAffinityList, da );
     end if;
  end if;
end declareAffinity;


-----------------------------------------------------------------------------
--  PARSE PRAGMA STATEMENT
--
-- Syntax: ... kind [params]
-----------------------------------------------------------------------------

procedure ParsePragmaStatement( thePragmaKind : aPragmaKind ) is
  pragmaKind  : aPragmaKind := thePragmaKind; -- TODO: Hack
  expr_val    : unbounded_string;
  expr_val2   : unbounded_string;
  expr_val3   : unbounded_string;
  expr_type   : identifier;
  --results     : unbounded_string;
  var_id      : identifier;
  --var_id2     : identifier;
  exportType  : unbounded_string;
  importType  : unbounded_string;
  newValue    : unbounded_string;
  test_result_status : boolean := false;
  test_message : unbounded_string;
  testCaseName : unbounded_string;
  constraintMode : designconstraintmodes;
  affinityMode : designaffinitymodes;
begin

  -- Parse the pragma parameters (if any)

  if pragmaKind /= ada_95 and pragmaKind /= inspection and pragmaKind /=
     noCommandHash and pragmaKind /= peek and pragmaKind /= gcc_errors and
     pragmaKind /= colour_messages then
     if pragmaKind = debug and (token /= symbol_t or identifiers( token ).value.all /= "(") then
        pragmaKind := debug_on;
     else
        expectPragmaParameterOpen( pragmaKind'img );
        --expect( symbol_t, "(" );
     end if;
  end if;

  case pragmaKind is
  when ada_95 =>                             -- pragma ada_95
     null;
  when advise =>                             -- pragma advise
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expectPragmaComma;
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expectPragmaComma;
           ParseStaticExpression( expr_val, var_id );
           baseTypesOK( var_id, uni_string_t );
        end if;
     end if;
  when affinity =>                           -- pragma affinity
     ParseDesignPragmaAffinityIdentifier( expr_val );
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseStaticExpression( expr_val3, var_id );
        baseTypesOK( var_id, float_t );
     else
        expr_val3 := to_unbounded_string( "0.0" );
     end if;
  when asserting =>                          -- pragma assert
     ParseExpression( expr_val, var_id );
  when annotate =>                           -- pragma annotate
     ParseAnnotateKind;
  when assumption =>                         -- pragma assumption
     ParseAssumptionKind( var_id, pragmaKind );
  when blocked =>                            -- pragma clarify
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expectPragmaComma;
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expectPragmaComma;
           ParseStaticExpression( expr_val, var_id );
           baseTypesOK( var_id, uni_string_t );
        end if;
     end if;
  when clarify =>                            -- pragma clarify
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expectPragmaComma;
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expectPragmaComma;
           ParseStaticExpression( expr_val, var_id );
           baseTypesOK( var_id, uni_string_t );
        end if;
     end if;
  when colour_messages =>
     null;
  when constraint =>                         -- pragma constraint
     ParseDesignPragmaConstraintIdentifier( expr_val );
     expectPragmaComma;
     if token = strlit_t then
        expr_val2 := identifiers( token ).value.all;
        getNextToken;
     else
        ParsePragmaIdentifier ( expr_val2 );
     end if;
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseStaticExpression( expr_val3, var_id );
        baseTypesOK( var_id, float_t );
     else
        expr_val3 := to_unbounded_string( "0.0" );
     end if;
  when debug =>                              -- pragma debug
     expr_val := identifiers( token ).value.all;
     expect( backlit_t );
  when debug_on =>                              -- pragma debug (no param)
     null;
  when declare_affinity =>                      -- pragma declare_affinity
     ParseDesignPragmaAffinityModeIdentifier( expr_val );
     affinityMode := toAffinityMode( expr_val );
     expectPragmaComma;
     ParseDesignPragmaAffinityIdentifier( expr_val );
     -- expect( symbol_t, "," );
     -- if token = strlit_t then
     --    if identifiers( token ).value.all = "" then
     --       err( "affinity name should not be an empty string" );
     --    else
     --       expr_val2 := identifiers( token ).value.all;
     --    end if;
     --    getNextToken;
     -- else
     --    ParsePragmaIdentifier( expr_val2 );
     -- end if;
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseStaticExpression( expr_val3, var_id );
        baseTypesOK( var_id, float_t );
     else
        expr_val3 := to_unbounded_string( "0.0" );
     end if;
  when declare_constraint =>                    -- pragma declare_constraint
     ParseDesignPragmaModeIdentifier( expr_val );
     constraintMode := toConstraintMode( expr_val );
     expectPragmaComma;
     ParseDesignPragmaConstraintIdentifier( expr_val );
     expectPragmaComma;
     if token = strlit_t then
        if identifiers( token ).value.all = "" then
           err( "constraint name should not be an empty string" );
        else
           expr_val2 := identifiers( token ).value.all;
        end if;
        getNextToken;
     else
        ParsePragmaIdentifier( expr_val2 );
     end if;
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseStaticExpression( expr_val3, var_id );
        baseTypesOK( var_id, float_t );
     else
        expr_val3 := to_unbounded_string( "0.0" );
     end if;
  when depreciated =>                           -- pragma depreciated
     expr_val := identifiers( token ).value.all;
     ParseStaticExpression( expr_val, var_id );
     baseTypesOK( var_id, uni_string_t );
  when dispute =>                               -- pragma dispute
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expectPragmaComma;
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expectPragmaComma;
           ParseStaticExpression( expr_val, var_id );
           baseTypesOK( var_id, uni_string_t );
        end if;
     end if;
  when error =>                                 -- pragma error
     ParseStaticExpression( expr_val, var_id );
     baseTypesOK( var_id, uni_string_t );
  when export | export_json =>                  -- pragma export/json
     ParseExportKind( var_id, exportType );
  when import | unchecked_import | import_json | unchecked_import_json =>
     -- pragma unchecked/import/json
     ParseImportKind( var_id, importType );
  when gcc_errors =>                         -- pragma gcc_errors
     null;
  when inspection =>                         -- pragma inspection point
     -- GCC Ada 7.4 falsely says conversion is not needed
     pragma warnings( off );
     if inputMode /= breakout and boolean(maintenanceOpt or testOpt) then
     pragma warnings( on );
         err( "inspection_point is not allowed in testing or maintenance phase mode unless at the breakout prompt" );
     end if;
  when manual_test =>                        -- pragma manual_test
     ParseIdentifier( var_id );                -- test owner
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expectPragmaComma;
        ParseStaticExpression( testCaseName, var_id );  -- test name/subject
        baseTypesOK( var_id, uni_string_t );
        expectPragmaComma;
        ParseStaticExpression( expr_val, var_id );  -- test objective
        baseTypesOK( var_id, uni_string_t );
        expectPragmaComma;
        ParseStaticExpression( expr_val, var_id );  -- test description
        baseTypesOK( var_id, uni_string_t );
        expectPragmaComma;
        ParseStaticExpression( expr_val, var_id );  -- test environment
        baseTypesOK( var_id, uni_string_t );
        expectPragmaComma;
        ParseStaticExpression( expr_val, var_id );  -- test category
        baseTypesOK( var_id, uni_string_t );
        expectPragmaComma;
        ParseStaticExpression( expr_val, var_id );  -- preconditions
        baseTypesOK( var_id, uni_string_t );
        expectPragmaComma;
        ParseStaticExpression( expr_val, var_id );  -- steps/expected results
        baseTypesOK( var_id, uni_string_t );
        expectPragmaComma;
        ParseStaticExpression( expr_val, var_id );  -- postconditions/cleanup
        baseTypesOK( var_id, uni_string_t );
        expectPragmaComma;
        ParseWorkEstimate( work_estimate_unknown ); -- work estimate
        ParseWorkPriority( work_estimate_unknown, is_todo => false ); -- work priority
        expectPragmaComma;
        ParseStaticExpression( expr_val, var_id );  -- work ticket / user story
        baseTypesOK( var_id, uni_string_t );
     else
        err( "team.member expected" );
     end if;
  when manual_test_result =>                 -- pragma manual_test_result
      ParseIdentifier( var_id );                -- tester
      if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expectPragmaComma;
           ParseStaticExpression( expr_val, var_id );  -- date
           baseTypesOK( var_id, uni_string_t );
           test_message := "Date: " & expr_val;
           expectPragmaComma;
           ParseStaticExpression( expr_val, var_id );  -- notes
           baseTypesOK( var_id, uni_string_t );
           test_message := test_message & "; Notes: " & expr_val;
           expectPragmaComma;
           ParseStaticExpression( expr_val, var_id );  -- screenshots
           baseTypesOK( var_id, uni_string_t );
           test_message := test_message & "; Screenshots: " & expr_val;
           expectPragmaComma;
           -- TODO: status is false when test succeeded.
           if token = true_t then
              test_result_status := false;
              expect( true_t );
           elsif token = false_t then
              test_result_status := true;
              expect( false_t );
           else
              err( "true or false expected for the test status" );
           end if;
          if token = symbol_t and identifiers( token ).value.all = "," then
             getNextToken;
             ParseStaticExpression( expr_val, var_id );  -- defect id
             baseTypesOK( var_id, uni_string_t );
             test_message := test_message & "; Ticket: " & expr_val;
          end if;
     else
        err( "team.member expected" );
     end if;
  when peek =>                               -- pragma inspection peek
     -- GCC Ada 7.4 falsely says conversion is not needed
     pragma warnings( off );
     if inputMode /= breakout and boolean(maintenanceOpt or testOpt) then
     pragma warnings( on );
         err( "inspection_peek cannot be used with testing or maintenance phase mode unless at the breakout prompt" );
     end if;
  when noCommandHash =>                      -- pragma no_command_hash
     null;
  when promptChange =>                       -- pragma prompt_script
     if rshOpt then                          -- security precaution
        err( "prompt scripts cannot be used in a restricted shell" );
     else
        expr_val := identifiers( token ).value.all;
        if expr_val /= null_unbounded_string then
           if tail( expr_val, 1 ) /= ";" then
              expr_val := expr_val & ";";
           end if;
        end if;
        expect( backlit_t );
     end if;
  when propose =>                           -- pragma refactor
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expectPragmaComma;
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expectPragmaComma;
           ParseStaticExpression( expr_val, var_id );
           baseTypesOK( var_id, uni_string_t );
        end if;
     end if;
  when refactor =>                           -- pragma refactor
     ParseIdentifier( var_id );
     if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
        expectPragmaComma;
        ParseIdentifier( var_id );
        if baseTypesOK( identifiers( var_id ).kind, teams_member_t ) then
           expectPragmaComma;
           ParseStaticExpression( expr_val, var_id );
           baseTypesOK( var_id, uni_string_t );
        end if;
     end if;
  when register_memcache_server =>           -- pragma register_memcache_server
     expr_val := identifiers( token ).value.all;
     expect( strlit_t );
     expectPragmaComma;
     expr_val2 := identifiers( token ).value.all;
     expect( number_t );
  when restriction =>                        -- pragma restriction
     ParsePragmaIdentifier( expr_val );
     if expr_val = "no_auto_declarations" then
        pragmaKind := restriction_auto;
     elsif expr_val = "no_annotate_todos" then
        pragmaKind := restriction_todos;
     elsif expr_val = "annotations_not_optional" then
        pragmaKind := restriction_annotations;
     elsif expr_val = "no_external_commands" then
        pragmaKind := restriction_external;
     elsif expr_val = "no_memcache" then
        pragmaKind := restriction_memcache;
     elsif expr_val = "no_mysql_database" then
        pragmaKind := restriction_mysql;
     elsif expr_val = "no_postgresql_database" then
        pragmaKind := restriction_postgresql;
     elsif expr_val = "no_unused_identifiers" then
        pragmaKind := restriction_unused;
     elsif expr_val = "no_volatiles" then
        pragmaKind := restriction_volatiles;
     elsif expr_val = "no_declarations_in_executable_statements" then
        pragmaKind := restriction_declarations;
     else
        err( "unknown restriction" );
        return;
     end if;
  when inspect_var =>                        -- pragma inspect
     -- GCC Ada 7.4 falsely says conversion is not needed
     pragma warnings( off );
     if inputMode /= breakout and boolean(maintenanceOpt or testOpt) then
     pragma warnings( on );
         err( "inspect cannot be used in testing or maintenance phase mode unless at the breakout prompt" );
     end if;
     ParseIdentifier( var_id );
  when license =>                            -- pragma license
     ParseLicenseKind( expr_val );
  when software_model =>                     -- pragma software_model
     ParseSoftwareModelName( expr_val );
  when session_export_script =>              -- pragma session_export_script
     if rshOpt then                          -- security precaution
        err( "session scripts cannot be defined in a " & optional_yellow( "restricted shell" ) );
     end if;
     expr_val := identifiers( token ).value.all;
     expect( backlit_t );
  when session_import_script =>              -- pragma session_import_script
     if rshOpt then                          -- security precaution
        err( "session scripts cannot be defined in a " & optional_yellow( "restricted shell" ) );
     end if;
     expr_val := identifiers( token ).value.all;
     expect( backlit_t );
  when suppress =>                           -- pragma restriction
     ParsePragmaIdentifier( expr_val );
     if expr_val = "word_quoting" then
        pragmaKind := suppress_word_quoting;
     elsif expr_val = "low_priority_todos_for_release" then
        pragmaKind := suppress_low_priority_todos;
     elsif expr_val = "all_todos_for_release" then
        pragmaKind := suppress_all_todos;
     elsif expr_val = "no_empty_command_substitutions" then
        pragmaKind := suppress_no_empty_command_subs;
     else
        err( "unknown error type" );
        return;
     end if;
  when template | unrestricted_template =>   -- pragma (unrestricted) template
     if rshOpt then
        err( "templates cannot be used in a restricted shell" );
     else
        ParsePragmaIdentifier( expr_val );
        if token = symbol_t and identifiers( token ).value.all = "," then
           getNextToken;
           expect( strlit_t );
           var_id := strlit_t;
        else
           var_id := eof_t;
        end if;
     end if;
     -- Mark this script as having a template to disable unused variable checks
     -- We need the type of template so interpret it now.
     if not skipping_block then
        hasTemplate := true;
        templateHeader.templateType := noTemplate;
        templateHeader.status := 200;
        -- if no type specified, assume HTML.
        if var_id = eof_t then
           templateHeader.templateType := htmlTemplate;
        -- http://www.webmaster-toolkit.com/mime-types.shtml
        elsif expr_val = "html" then
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
     end if;
  when test =>                               -- pragma test
     expr_val := identifiers( token ).value.all;
     expect( backlit_t );
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        expect( strlit_t );
        expr_val2 := identifiers( strlit_t ).value.all;
     else
        expr_val2 := null_unbounded_string;
     end if;
  when test_report =>                        -- pragma test_report
     expr_val := identifiers( token ).name;
     discardUnusedIdentifier( token );
     if expr_val = "text" then
        getNextToken;
     elsif expr_val = "xml" then
        getNextToken;
     else
        err( "unknown test report type '" & to_string( expr_val ) & "'" );
     end if;
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseStaticExpression( reportPath, expr_type );
        baseTypesOK( expr_type, uni_string_t );
        if expr_val = "text" and length( reportPath ) > 0 then
           err( "text reports currently cannot be written to a file" );
        elsif expr_val = "xml" and length( reportPath ) = 0 then
           err( "file path is empty" );
        end if;
     else
        reportPath := null_unbounded_string;
     end if;
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
           test_result_status := expr_val = "0";
        end if;
        -- Optional description string
        --if token = symbol_t and identifiers( token ).value.all = "," then
        --   expect( symbol_t, "," );
        --   expr_val2 := identifiers( token ).value.all;
        --   expect( strlit_t );
        --else
        --   expr_val2 := null_unbounded_string;
        --end if;
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
     begin
       ParseIdentifier( var_id );              -- the person
       unused_bool := baseTypesOK( identifiers( var_id ).kind, teams_member_t );
       expectPragmaComma;
       ParseStaticExpression( expr_val, var_id );
       baseTypesOK( var_id, uni_string_t );
       expectPragmaComma;

       ParseWorkEstimate( work_estimate_unknown );
       ParseWorkPriority( work_estimate_unknown );

       -- optional ticket id
       if token = symbol_t and identifiers( token ).value.all = "," then
          getNextToken;
          expect( strlit_t );
       end if;
     end;
  when uninspect_var =>                      -- pragma uninspect
     ParseIdentifier( var_id );
  when volatile =>                           -- pragma volatile
     if restriction_no_volatiles then
        err( "pragma restriction( no_volatiles ) does not allow pragma volatile" );
     end if;
     ParseIdentifier( var_id );
     expr_val := null_unbounded_string;
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseStaticExpression( expr_val, expr_type );
        baseTypesOK( expr_type, duration_t );
     end if;
  when unchecked_volatile =>                 -- pragma volatile
     if restriction_no_volatiles then
        err( "pragma restriction( no_volatiles ) does not allow pragma unchecked_volatile" );
     end if;
     ParseIdentifier( var_id );
     expr_val := null_unbounded_string;
     if token = symbol_t and identifiers( token ).value.all = "," then
        getNextToken;
        ParseStaticExpression( expr_val, expr_type );
        baseTypesOK( expr_type, duration_t );
     end if;
  when others =>
     err( gnat.source_info.source_location & ": Internal error: can't handle pragma" );
  end case;

  if pragmaKind /= ada_95 and pragmaKind /= inspection and pragmaKind /=
     noCommandHash and pragmaKind /= debug_on and pragmaKind /= peek and
     pragmaKind /= gcc_errors and pragmaKind /= colour_messages then
     expectPragmaParameterClose( pragmaKind'img );
     --expect( symbol_t, ")" );
  end if;

  -- Execute the pragma

  -- Some pragmas have an effect at syntax checking or both syntax
  -- checking and run-time.  Syntax checking doesn't happen at the
  -- command prompt.

  --if syntax_check then
  if interpreterPhase = checking then
     if pragmaKind = ada_95 then
        onlyAda95 := true;
     elsif pragmaKind = affinity then
        if not maintenanceOpt then
           if isExecutingStaticCommand then
              declare
                limit : float;
              begin
                limit := float( to_numeric( expr_val3 ) );
                EnforceAffinity( expr_val, limit );
              end;
           end if;
        end if;
     elsif pragmaKind = assumption_used then
        identifiers( var_id ).wasReferenced := true;
        --identifiers( var_id ).referencedByThread := getThreadName;
     elsif pragmaKind = assumption_written then
        if identifiers( var_id ).field_of /= eof_t and
           -- KLUDGE: should never be zero...should be eof_t
           identifiers( var_id ).field_of /= 0 then
           identifiers( identifiers( var_id ).field_of ).wasWritten := true;
        else
           identifiers( var_id ).wasWritten := true;
        end if;
     elsif pragmaKind = assumption_applied then
        if identifiers( var_id ).class /= typeClass and
           identifiers( var_id ).class /= subClass then
           err( "concrete type or subtype expected" );
        else
           identifiers( var_id ).wasApplied := true;
        end if;
     elsif pragmaKind = assumption_factor then
        if identifiers( var_id ).class /= varClass then
           err( "variable expected" );
        else
           identifiers( var_id ).wasFactor := true;
        end if;
     elsif pragmaKind = colour_messages then
           colourOpt := true;
     elsif pragmaKind = constraint then
        if not maintenanceOpt then
           if isExecutingStaticCommand then
              declare
                weight : float;
              begin
                weight := float( to_numeric( expr_val3 ) );
                EnforceConstraint( expr_val, expr_val2, weight );
              end;
           end if;
        end if;
     elsif pragmaKind = declare_affinity then
        if not maintenanceOpt then
           if isExecutingStaticCommand then
              declare
                limit : float := 0.0;
              begin
                if expr_val3 /= null_unbounded_string then
                   limit := float( to_numeric( expr_val3 ) );
                end if;
                DeclareAffinity( affinityMode, expr_val, limit );
              end;
           end if;
        end if;
     elsif pragmaKind = declare_constraint then
        if not maintenanceOpt then
           if isExecutingStaticCommand then
              declare
                limit : float := 0.0;
              begin
                if expr_val3 /= null_unbounded_string then
                   limit := float( to_numeric( expr_val3 ) );
                end if;
                DeclareConstraint( constraintMode, expr_val, expr_val2, limit );
              end;
           end if;
        end if;
     elsif pragmaKind = gcc_errors then
        gccOpt := true;
     elsif pragmaKind = restriction_unused then
        restriction_no_unused_identifiers := true;
     elsif pragmaKind = restriction_declarations then
        restriction_no_declarations := true;
     elsif pragmaKind = suppress_word_quoting then
        world.suppress_word_quoting := true;
     elsif pragmaKind = suppress_low_priority_todos then
        allowLowPriorityTodosForRelease := true;
     elsif pragmaKind = suppress_all_todos then
        allowAllTodosForRelease := true;
     elsif pragmaKind = suppress_all_todos then
        allowAllTodosForRelease := true;
     elsif pragmaKind = suppress_no_empty_command_subs then
        world.suppress_no_empty_command_subs := true;
     -- Pragma volatile is checked at syntax check time because volatiles
     -- must be exempt from limited testing.  They will also be applied
     -- at run-time.
     elsif pragmaKind = volatile then
        identifiers( var_id ).volatile := checked;
        if expr_val /= "" then
           begin
             identifiers( var_id ).volatileTTL := duration( to_numeric( expr_val ) );
             if identifiers( var_id ).volatileTTL <= 0.0 then
                err( "volatile TTL is less than zero" );
             elsif identifiers( var_id ).volatileTTL > 0.0 then
                identifiers( var_id ).volatileExpire := clock;
             end if;
           exception when others =>
             err("bad value for volatile TTL" );
           end;
        end if;
     elsif pragmaKind = unchecked_volatile then
        identifiers( var_id ).volatile := unchecked;
        if expr_val /= "" then
           begin
             identifiers( var_id ).volatileTTL := duration( to_numeric( expr_val ) );
             if identifiers( var_id ).volatileTTL <= 0.0 then
                err( "volatile TTL is less than zero" );
             elsif identifiers( var_id ).volatileTTL > 0.0 then
                identifiers( var_id ).volatileExpire := clock;
             end if;
           exception when others =>
             err("bad value for volatile TTL" );
           end;
        end if;
     end if;
  end if;

  if isExecutingCommand then
     case pragmaKind is
     when ada_95 =>
        onlyAda95 := true;
     when advise =>
        null;
     when affinity =>                               -- pragma constraint
        if inputMode = interactive or inputMode = breakout then
            err( "pragma affinity cannot be used in an interactive session" );
        end if;
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
     when assumption|assumption_used|assumption_written|assumption_applied|assumption_factor =>
        null;
     when blocked =>
        null;
     when clarify =>
        null;
     when colour_messages =>
        -- this executes when the profile loads because there is no syntax
        -- check phase for that.
        colourOpt := true;
     when constraint =>                            -- pragma constraint
        if inputMode = interactive or inputMode = breakout then
            err( "pragma constraint cannot be used in an interactive session" );
        end if;
     when debug =>
        if debugOpt then
           if not syntax_check then
              declare
                 savershOpt : constant commandLineOption := rshOpt;
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
     when declare_affinity =>                      -- pragma declare_affinity
        if inputMode = interactive or inputMode = breakout then
            err( "pragma declare_affinity cannot be used in an interactive session" );
        end if;
     when declare_constraint =>                    -- pragma declare_constraint
        if inputMode = interactive or inputMode = breakout then
            err( "pragma declare_constraint cannot be used in an interactive session" );
        end if;
     when depreciated =>
        -- TODO: this should create a list of depreciation message
        -- for now, only the entire script is depreciated
        -- It is an error in the design phase to run a deprecated script.
        depreciatedMsg := "This script made obsolete by " & expr_val & '.';
        if designOpt then
           err( to_string( depreciatedMsg ) );
           depreciatedMsg := null_unbounded_string;
        end if;
     when dispute =>
        null;
     when error =>
        err( "error: " & to_string( expr_val ) );
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
        -- this executes when the profile loads because there is no syntax
        -- check phase for that.
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
           -- KLUDGE: there are so many standard types...
           elsif getBaseType( identifiers( var_id ).kind ) <= complex_imaginary_t then
              err( "security issue: import variable " & optional_yellow( to_string( identifiers( var_id ).name ) ) & " should be a derived type not a predefined type (or a subtype of one) like " & optional_yellow( to_string( identifiers( identifiers( var_id ).kind ).name ) ) );
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
           -- Force users to import to use a derived type as a way of
           -- tracking imported untrusted data.
           elsif getBaseType( identifiers( var_id ).kind ) = string_t or
                 getBaseType( identifiers( var_id ).kind ) = unbounded_string_t then
              err( "security issue: import variable " & optional_yellow( to_string( identifiers( var_id ).name ) ) & " should be a derived type not a predefined type (or a subtype of one) like " & optional_yellow( to_string( identifiers( identifiers( var_id ).kind ).name ) ) );
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
                 err_exception_raised;
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
                 err_exception_raised;
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
                           DoJsonToString( identifiers( var_id ).value.all, newValue );
                        elsif identifiers( var_id ).list then                           -- array
                           DoJsonToArray( var_id, newValue );
                        elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_record_t then -- record
                           DoJsonToRecord( var_id, newValue );
                        elsif getUniType( identifiers( var_id ).kind ) = uni_numeric_t then -- number
                           DoJsonToNumber( newValue, identifiers( var_id ).value.all );
                        elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_enumerated_t then -- enum
                           DoJsonToNumber( newValue, identifiers( var_id ).value.all );
                           -- identifiers( var_id ).value := newValue;
                        else
                           err( gnat.source_info.source_location & ": internal error: unexpected import translation type" );
                        end if;
                     else                                                           -- no mapping
                        identifiers( var_id ).value.all := newValue;
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
                      identifiers( temp1_t ).value.all := identifiers( var_id ).name;
                   end if;
                      CompileAndRun( sessionImportScript, 1, false );
                      importValue := identifiers( temp2_t ).value.all;
                   --   b := deleteIdent( temp2_t );
                   --   b := deleteIdent( temp1_t );
                      case identifiers( var_id ).mapping is
                      when json =>
                         if getUniType( identifiers( var_id ).kind ) = uni_string_t then -- string
                            DoJsonToString( identifiers( var_id ).value.all, importValue );
                         elsif identifiers( var_id ).list then                           -- array
                            DoJsonToArray( var_id, importValue );
                         elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_record_t then -- record
                            DoJsonToRecord( var_id, importValue );
                         elsif getUniType( identifiers( var_id ).kind ) = uni_numeric_t then -- number
                            DoJsonToNumber( importValue, identifiers( var_id ).value.all );
                         elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_enumerated_t then -- enum
                            DoJsonToNumber( importValue, identifiers( var_id ).value.all );
                         else
                            err( gnat.source_info.source_location & ": internal error: unexpected import translation type" );
                         end if;
                      when none =>
                         identifiers( var_id ).value.all := importValue;
                      when others =>
                         err( gnat.source_info.source_location & ": internal error: unexpected mapping type" );
                      end case;
                   --end if;
                 end;
              end if;
           elsif importType = "shell" then
              identifiers( var_id ).method := shell;
              refreshVolatile( var_id );
              -- show a trace of what's imported.  If JSON, show JSON as it
              -- could be multiple values
              if trace then
                  put_trace(
                     to_string( identifiers( var_id ).name ) & " := """ &
                     to_string( ToEscaped( identifiers( var_id ).value.all ) ) &
                     """" );
              end if;
           else
              err( gnat.source_info.source_location & ": internal error: unexpected import method '" & to_string( importType ) & "'" );
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
               identifiers( id ).value.all := expr_val;
               licenseSet := true;
            end if;
          exception when others =>
            err_exception_raised;
          end;
        end if;
     when manual_test =>
        if testOpt then
           -- not clear what doing a test in an interactive session means,
           -- and the parser isn't designed to produce a test report in
           -- this situation.
           if inputMode = interactive or inputMode = breakout then
              err( "manual_test cannot be used in an interactive session" );
           elsif not syntax_check then
              -- for a manual test case, there's nothing to run
              run_test_case( null_unbounded_string, testCaseName, manual_test => true );
           end if;
        end if;
     when manual_test_result =>
        if testOpt then
           -- not clear what doing a test in an interactive session means,
           -- and the parser isn't designed to produce a test report in
           -- this situation.
           if inputMode = interactive or inputMode = breakout then
              err( "manual_test_result cannot be used in an interactive session" );
           elsif not syntax_check then
              record_test_result( test_result_status, test_message );
           end if;
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
            err_exception_raised;
         end;
     when restriction_auto =>
        restriction_no_auto_declarations := true;
     when restriction_annotations =>
        restriction_annotations_not_optional := true;
     when restriction_unused =>
        null; -- only applies to script in syntax check
     when restriction_declarations =>
        restriction_no_declarations := true;
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
     when restriction_volatiles =>
        restriction_no_volatiles := true;
     when promptChange =>
        if not error_found then
           promptScript := expr_val;
        end if;
     when session_export_script =>
        if not error_found then
           if length( sessionExportScript ) = 0 then
              sessionExportScript := expr_val;
           else
              err( "session_export_script is already defined" );
           end if;
        end if;
     when session_import_script =>
        if not error_found then
           if length( sessionImportScript ) = 0 then
              sessionImportScript := expr_val;
           else
              err( "session_import_script is already defined" );
           end if;
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
                identifiers( id ).value.all := expr_val;
                softwareModelSet := true;
             end if;
           exception when others =>
             err_exception_raised;
           end;
        end if;
     when suppress_word_quoting =>
        world.suppress_word_quoting := true;
     when suppress_low_priority_todos =>
        null; -- only applies to script in syntax check
     when suppress_all_todos =>
        null; -- only applies to script in syntax check
     when suppress_no_empty_command_subs =>
        world.suppress_no_empty_command_subs := true;
     when template | unrestricted_template =>
        if processingTemplate then
           err( "template already used" );
        elsif inputMode = interactive or inputMode = breakout then
           err( "template cannot be used in an interactive session" );
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
           templatePath := identifiers( strlit_t ).value.all;
        end if;
        processingTemplate := true;
        if pragmaKind = unrestricted_template then
           unrestrictedTemplate := true;
        end if;
        --putTemplateHeader( templateHeader );
     when test =>
        if testOpt then
           -- not clear what doing a test in an interactive session means,
           -- and the parser isn't designed to produce a test report in
           -- this situation.
           if inputMode = interactive or inputMode = breakout then
              err( "test cannot be used in an interactive session" );
           elsif not syntax_check then
               run_test_case( expr_val, expr_val2, manual_test => false );
           end if;
        end if;
     when test_report =>
        -- TODO: should be a syntax-time tests, not a run-time test.
        if isJunitStarted then
           err( "test report has already been started" );
        elsif expr_val = "text" then
           usingTextTestReport := true;
        elsif expr_val = "xml" then
           usingTextTestReport := false;
        else
           err( gnat.source_info.source_location & ": internal error: unexpected test report type '" & to_string( expr_val ) & "'" );
        end if;
     when test_result =>
        if testOpt then
           -- not clear what doing a test in an interactive session means,
           -- and the parser isn't designed to produce a test report in
           -- this situation.
           if inputMode = interactive or inputMode = breakout then
              err( "test_result cannot be used in an interactive session" );
           elsif not syntax_check then
              if baseTypesOk( boolean_t, var_id ) then
                 record_test_result( test_result_status, null_unbounded_string );
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
                 err_exception_raised;
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
                 err_exception_raised;
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
                           DoJsonToString( identifiers( var_id ).value.all, newValue );
                        elsif identifiers( var_id ).list then                           -- array
                           DoJsonToArray( var_id, newValue );
                        elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_record_t then -- record
                           DoJsonToRecord( var_id, newValue );
                        elsif getUniType( identifiers( var_id ).kind ) = uni_numeric_t then -- number
                           DoJsonToNumber( newValue, identifiers( var_id ).value.all );
                        elsif  identifiers( getBaseType( identifiers( var_id ).kind ) ).kind  = root_enumerated_t then -- enum
                           DoJsonToNumber( newValue, identifiers(var_id ).value.all );
                        else
                           err( gnat.source_info.source_location & ": internal error: unexpected import translation type" );
                        end if;
                     else                                                           -- no mapping
                        identifiers( var_id ).value.all := newValue;
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
        identifiers( var_id ).volatile := checked;
        if expr_val /= "" then
           begin
             identifiers( var_id ).volatileTTL := duration( to_numeric( expr_val ) );
             if identifiers( var_id ).volatileTTL <= 0.0 then
                err( "volatile TTL is less than zero" );
             elsif identifiers( var_id ).volatileTTL > 0.0 then
                identifiers( var_id ).volatileExpire := clock;
             end if;
           exception when others =>
             err("bad value for volatile TTL" );
           end;
        end if;
     when unchecked_volatile =>
        identifiers( var_id ).volatile := unchecked;
        if expr_val /= "" then
           begin
             identifiers( var_id ).volatileTTL := duration( to_numeric( expr_val ) );
             if identifiers( var_id ).volatileTTL <= 0.0 then
                err( "volatile TTL is less than zero" );
             elsif identifiers( var_id ).volatileTTL > 0.0 then
                identifiers( var_id ).volatileExpire := clock;
             end if;
           exception when others =>
             err("bad value for volatile TTL" );
           end;
        end if;
     when others =>
        err( gnat.source_info.source_location & ": Internal error: unable to execute pragma" );
     end case;
  end if;
end ParsePragmaStatement;


-----------------------------------------------------------------------------
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
        err( "pragma block cannot be used with " & optional_yellow( "pragma ada_95" ) );
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
     while token /= eof_t and token /= end_t and not error_found loop
        -- an error check
        ParsePragmaStatement( pragmaKind );
        if token = symbol_t and identifiers( symbol_t ).value.all = to_unbounded_string( "@" ) then
           if onlyAda95 then
              err( "@ cannot be used with " & optional_yellow( "pragma ada_95" ) );
           end if;
           expect( symbol_t, "@" );
        elsif token = symbol_t and identifiers( symbol_t ).value.all = to_unbounded_string( ";" ) then
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
        if token = symbol_t and identifiers( symbol_t ).value.all = to_unbounded_string( "(" ) then
           err( "'@' or ';' expected" );
        end if;
        exit when done or error_found or token = eof_t or (token = symbol_t and identifiers( symbol_t ).value.all /= to_unbounded_string( "@" ) );
        if onlyAda95 then
           err( "@cannot be used with " & optional_yellow( "pragma ada_95" ) );
        end if;
        expect( symbol_t, "@" );
     end loop;
  end if;
end ParsePragma;

procedure startupPragmas is
begin
  null;
end startupPragmas;

procedure shutdownPragmas is
begin
  if isJunitStarted then
     if isJunitTestCaseStarted then
        if usingTextTestReport then
           endJunitTestCase( myTextTestReport );
        else
           endJunitTestCase( myXmlTestReport );
        end if;
     end if;
     if isJunitTestSuiteStarted then
        if usingTextTestReport then
           endJUnitTestSuite( myTextTestReport );
        else
           endJUnitTestSuite( myXmlTestReport );
        end if;
     end if;
     if usingTextTestReport then
        endJunit( myTextTestReport );
     else
        endJunit( myXmlTestReport );
     end if;
  end if;
end shutdownPragmas;

end parser_pragmas;
