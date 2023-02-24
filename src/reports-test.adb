------------------------------------------------------------------------------
-- Built-in Help Command Reports                                            --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
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
with Ada.Characters.Latin_1,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded.Text_IO,
     Ada.Calendar.Arithmetic,
     pegasoft.strings,
     world,
     compiler; -- Circular dependency...
use  Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded.Text_IO,
     Ada.Calendar.Arithmetic,
     pegasoft.strings,
     world,
     compiler;

package body reports.test is

  -- JUNIT Utilities

  Quotation : constant character := Ada.Characters.Latin_1.Quotation;

  -- ENCODE MESSAGE
  --
  -- Encode special XML/HTML characters.

  function encode_message( msg : unbounded_string ) return string is
    encoded : unbounded_string;
    ch : character;
  begin
    for i in 1..length( msg ) loop
        ch := element( msg, i );
        if ch = Quotation then
           encoded := encoded & "&quot;";
        elsif ch = '&' then
           encoded := encoded & "&amp;";
        elsif ch = '<' then
           encoded := encoded & "&lt;";
        elsif ch = '>' then
           encoded := encoded & "&gt;";
        else
           encoded := encoded & ch;
        end if;
    end loop;
    return to_string( encoded );
  end encode_message;

  -- ENCODE CLASS
  --
  -- If filenames are used for classes, anything with a "." will be treated
  -- as two classes.  So strip off the filename suffix if it exists.

  function encode_class( class : unbounded_string ) return string is
    encoded : unbounded_string;
  begin
    if tail( class, 3 ) = ".sp" then
       encoded := delete( class, length( class )-2, length( class ) );
    elsif tail( class, 5 ) = ".bush" then
       encoded := delete( class, length( class )-4, length( class ) );
    else
       encoded := class;
    end if;
    return to_string( encoded );
  end encode_class;

  isStarted : boolean := false;
  isTestCaseStarted : boolean := false;
  isTestSuiteStarted : boolean := false;
  lastSourceFile : natural := 0;

  -- For now, at least, there is only one test report.  So these are
  -- globals.

  -- TODO: result file is actually created in report root class.  Modify
  -- to use it (though it is currently only temporary file).
  test_result_file : file_type;
  test_case_file : file_type;

  js  : junitState;
  jts : junitTestSuite;
  jtc : junitTestCase;

  function isJunitTestCaseStarted return boolean is
  begin
    return isTestCaseStarted;
  end isJunitTestCaseStarted;

  function isJunitTestSuiteStarted return boolean is
  begin
    return isTestSuiteStarted;
  end isJunitTestSuiteStarted;

  function isJunitStarted return boolean is
  begin
    return isStarted;
  end isJunitStarted;

  ----------------------------------------------------------------------------
  --
  -- TEXT REPORTS
  --
  ----------------------------------------------------------------------------

  procedure startJUnitTestCase( report: in out textTestReport ;
    name, description : unbounded_string := null_unbounded_string ) is
    cmdline    : messageStrings;
    firstpos   : natural;
    lastpos    : natural;
    lineno     : natural;
    fileno     : natural;
    sfr        : aSourceFile;
  begin
    jtc.isOpen := true;
    jtc.class := to_unbounded_string( "undefined" );
    jtc.startTime := ada.calendar.clock;
    jtc.testNo := jtc.testNo + 1;
    getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
    sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
    if length( name ) = 0 then
      jtc.name := sfr.name & "_" & trim( to_unbounded_string( jtc.testno'img ), both );
    else
      jtc.name := sfr.name & "_" & name;
    end if;
    -- lazy assurance that no spaces are in the test name
    for i in 1..length( jtc.name ) loop
      if Element( jtc.name, i ) = ' ' then
         Replace_Element( jtc.name, i, '_' );
      end if;
    end loop;
    jtc.file := sfr.name;
    jtc.description := description;
    jtc.line := to_unbounded_string( lineno'img );
    jtc.assertionCnt := 0;
    jtc.errorCnt := 0;
    jtc.failureCnt := 0;
    errorList.Clear( jtc.errorMsgs );
    failureList.Clear( jtc.failureMsgs );
    isTestCaseStarted := true;
  end startJUnitTestCase;

  procedure endJunitTestCase( report : in out textTestReport ) is
    cl : contentList.list;
    elapsedDays : Day_Count;
    elapsedSeconds : duration;
    elapsedLeapSeconds : Leap_Seconds_Count;
    msg : unbounded_string;
  begin
    js.totalTests := js.totalTests + 1;
    jts.assertionCnt := jts.assertionCnt + jtc.assertionCnt;
    jts.failureCnt := jts.failureCnt + jtc.failureCnt;
    jts.errorCnt := jts.errorCnt + jtc.errorCnt;
    Difference( ada.calendar.clock, jtc.startTime, elapsedDays, elapsedSeconds, elapsedLeapSeconds );
    if jtc.failureCnt > 0 or jtc.errorCnt > 0 then
       --contentList.Queue( cl, jtc.name );
       contentList.queue( cl, "Description: " & jtc.description );
       contentList.Queue( cl, "Class: " & jtc.class );
       contentList.Queue( cl, "File: " & jtc.file );
       contentList.Queue( cl, "Line: " & jtc.line );
       contentList.Queue( cl, to_unbounded_string( "Test_Result's: " & jtc.assertionCnt'img ) );
       contentList.Queue( cl, to_unbounded_string( "Failures: " & jtc.failureCnt'img ) );
       contentList.Queue( cl, to_unbounded_string( "Errors: " & jtc.errorCnt'img ) );
     contentList.Queue( cl, to_unbounded_string( "Time: " & elapsedSeconds'img ) );
    --put( test_case_file, "  <testcase name=" & Quotation & to_string( jtc.name ) & Quotation & " " );
    --put( test_case_file, "class=" & Quotation & to_string( jtc.class ) & Quotation & " " );
    --put( test_case_file, "file=" & Quotation & to_string( jtc.file ) & Quotation & " " );
    --put( test_case_file, "line=" & Quotation & to_string( trim( jtc.line, both ) ) & Quotation & " " );
    --put( test_case_file, "assertions=" & Quotation & trim( jtc.assertionCnt'img, both ) & Quotation & " " );  -- TODO: trim
    --put( test_case_file, "failures=" & Quotation & trim( jtc.failureCnt'img, both ) & Quotation & " " );
    --put( test_case_file, "errors=" & Quotation & trim( jtc.errorCnt'img, both ) & Quotation & " " );
    --put( test_case_file, "time=" & Quotation & trim( elapsedSeconds'img, both ) & Quotation & ">" );
    --new_line( test_case_file );
       while not failureList.isEmpty( jtc.failureMsgs ) loop
          failureList.Pull( jtc.failureMsgs, msg );
          contentList.Queue( cl, "Failure: " & msg );
       end loop;
       while not errorList.isEmpty( jtc.errorMsgs ) loop
          errorList.Pull( jtc.errorMsgs, msg );
          contentList.Queue( cl, "Error: " & msg );
       end loop;
       renderBulletList( report, cl, "Test " & to_string( jtc.name ) );
    end if;
    jtc.isOpen := false;
    isTestCaseStarted := false;
  end endJunitTestCase;

  -- TEST CASE ERROR
  --
  -- Record an exception or some other unexpected problem with the testing.

  procedure testCaseError( report : in out textTestReport ) is
  begin
    jtc.errorCnt := jtc.errorCnt + 1;
    wasTestErrorOrFailure := true;
    errorList.Queue( jtc.errorMsgs, toEscaped( fullErrorMessage.gccMessage ) );
  end testCaseError;

  -- TEST CASE FAILURE
  --
  -- Record a test case failure.

  procedure testCaseFailure( report : in out textTestReport; test_message : unbounded_string := null_unbounded_string ) is
    cmdline    : messageStrings;
    firstpos   : natural;
    lastpos    : natural;
    lineno     : natural;
    fileno     : natural;
    -- sfr        : aSourceFile;
  begin
     -- if an error occurred in the test, ignore the success or failure
    if jtc.errorCnt = 0 then
       wasTestErrorOrFailure := true;
       jtc.assertionCnt := jtc.assertionCnt + 1;
       jtc.failureCnt := jtc.failureCnt + 1;
       getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
       -- sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
       if test_message /= null_unbounded_string then
          failureList.Queue( jtc.failureMsgs, toEscaped( test_message ) );
       else
          -- TODO: if the test was multi-line, this will only show
          -- the final line.
          failureList.Queue( jtc.failureMsgs, toEscaped( cmdLine.gccMessage ) );
       end if;
    elsif jtc.errorCnt = 1 then
       failureList.Clear( jtc.failureMsgs );
    end if;
  end testCaseFailure;

  -- TEST CASE SUCCESS
  --
  -- Record a test case success.

  procedure testCaseSuccess( report : in out textTestReport ) is
  begin
     -- if an error occurred in the test, ignore the success or failure
    if jtc.errorCnt = 0 then
       jtc.assertionCnt := jtc.assertionCnt + 1;
       --jtc.successCnt := jtc.successCnt + 1;
    end if;
  end testCaseSuccess;

  -- more here


  --  START JUNIT TEST SUITE
  --
  --
  ----------------------------------------------------------------------------

  procedure startJUnitTestSuite( report: in out textTestReport;
    name : unbounded_string := null_unbounded_string;
    test_name : unbounded_string := null_unbounded_string ) is
    cmdline    : messageStrings;
    firstpos   : natural;
    lastpos    : natural;
    lineno     : natural;
    fileno     : natural;
    sfr        : aSourceFile;
  begin
    jts.isOpen := true;
    getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
    sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );

    -- if no name is provided, the test name prefix is the source file.
    if length( name ) = 0 then
      jts.name := jts.name & "_" & trim( to_unbounded_string( lineno'img ), both );
    else
      jts.name := name;
    end if;

    -- TODO: path, but this is a name
    jts.path := sfr.name;
    jts.startTime := ada.calendar.clock;
    create( test_case_file, out_file );
    lastSourceFile := fileNo;
    isTestSuiteStarted := true;
  end startJUnitTestSuite;


  --  END JUNIT TEST SUITE
  --
  --
  ----------------------------------------------------------------------------

  procedure endJunitTestSuite( report : in out textTestReport ) is
    cl : contentList.List;
    elapsedDays : Day_Count;
    elapsedSeconds : duration;
    elapsedLeapSeconds : Leap_Seconds_Count;
  begin
    Difference( ada.calendar.clock, jts.startTime, elapsedDays, elapsedSeconds, elapsedLeapSeconds );
    contentList.Queue( cl, "File: " & jts.path );
    contentList.Queue( cl, "Tests: " & to_unbounded_string( jts.testCnt'img ) );
    contentList.Queue( cl, "Test_Result's: " & to_unbounded_string( jts.assertionCnt'img ) );
    contentList.Queue( cl, "Failures: " & to_unbounded_string( jts.failureCnt'img ) );
    contentList.Queue( cl, "Errors: " & to_unbounded_string( jts.errorCnt'img ) );
    contentList.Queue( cl, "Time: " & to_unbounded_string( elapsedSeconds'img ) );
    renderBulletList( report, cl, "Suite Summary for " & to_string( jts.name ) );
    jts.isOpen := false;
    isTestCaseStarted := false;
  end endJunitTestSuite;


  --  CHECK FOR NEW TEST SUITE
  --
  --
  ----------------------------------------------------------------------------

  procedure checkForNewTestSuite( report : in out textTestReport;
    test_name : unbounded_string := null_unbounded_string ) is
    cmdline    : messageStrings;
    firstpos   : natural;
    lastpos    : natural;
    lineno     : natural;
    fileno     : natural;
    sfr        : aSourceFile;
  begin
    getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
    if lastSourceFile /= fileNo then
       if lastSourceFile /= 0 then -- or if not isJunitTestSuiteStarted then
          endJunitTestSuite( report );
       end if;
       sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
       startJUnitTestSuite( report, sfr.name, test_name );
    end if;
  end checkForNewTestSuite;

  procedure startJunit( report : in out textTestReport ) is
  begin
    start( report );
    isStarted := true;
    lastSourceFile := 0;
    wasTestErrorOrFailure := false;
  end startJunit;

  procedure endJunit( report : in out textTestReport ) is
  begin
    finish( report );
    if wasTestErrorOrFailure then
       last_status := 1;
    end if;
  end endJunit;

  ----------------------------------------------------------------------------
  --
  -- JUNIT XML REPORTS
  --
  ----------------------------------------------------------------------------

  procedure startJunit( report: in out xmlTestReport; path : unbounded_string ) is
  begin
    isStarted := true;
    lastSourceFile := 0;
    wasTestErrorOrFailure := false;
    if length( path ) = 0 then
       create( test_result_file, out_file, "sparforte_test.xml" );
    else
       create( test_result_file, out_file, to_string( path ) );
    end if;
    put_line( test_result_file, "<?xml version=" & Quotation & "1.0" &
      Quotation & " encoding="  & Quotation &
      "UTF-8"  & Quotation & "?>" );
    put_line( test_result_file, "<testsuites>" );
  end startJunit;

  procedure startJUnitTestCase( report: in out xmlTestReport;
     name, description : unbounded_string := null_unbounded_string ) is
    cmdline    : messageStrings;
    firstpos   : natural;
    lastpos    : natural;
    lineno     : natural;
    fileno     : natural;
    sfr        : aSourceFile;
  begin
    jtc.isOpen := true;
    jtc.class := to_unbounded_string( "undefined" );
    jtc.startTime := ada.calendar.clock;
    jtc.testNo := jtc.testNo + 1;
    getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
    sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
    if length( name ) = 0 then
      jtc.name := sfr.name & "_" & trim( to_unbounded_string( jtc.testno'img ), both );
    else
      jtc.name := sfr.name & "_" & name;
    end if;
    -- lazy assurance that no spaces are in the test name
    for i in 1..length( jtc.name ) loop
      if Element( jtc.name, i ) = ' ' then
         Replace_Element( jtc.name, i, '_' );
      end if;
    end loop;
    jtc.description := description;
    jtc.file := sfr.name;
    jtc.line := to_unbounded_string( lineno'img );
    jtc.assertionCnt := 0;
    jtc.errorCnt := 0;
    jtc.failureCnt := 0;
    errorList.Clear( jtc.errorMsgs );
    failureList.Clear( jtc.failureMsgs );
    isTestCaseStarted := true;
  end startJUnitTestCase;

  procedure endJunitTestCase( report: in out xmlTestReport ) is
    elapsedDays : Day_Count;
    elapsedSeconds : duration;
    elapsedLeapSeconds : Leap_Seconds_Count;
    msg : unbounded_string;
  begin
    js.totalTests := js.totalTests + 1;
    jts.assertionCnt := jts.assertionCnt + jtc.assertionCnt;
    jts.failureCnt := jts.failureCnt + jtc.failureCnt;
    jts.errorCnt := jts.errorCnt + jtc.errorCnt;
    Difference( ada.calendar.clock, jtc.startTime, elapsedDays, elapsedSeconds, elapsedLeapSeconds );
    put( test_case_file, "  <testcase name=" & Quotation & to_string( jtc.name ) & Quotation & " " );
    put( test_case_file, "class=" & Quotation & to_string( jtc.class ) & Quotation & " " );
    put( test_case_file, "file=" & Quotation & to_string( jtc.file ) & Quotation & " " );
    put( test_case_file, "line=" & Quotation & to_string( trim( jtc.line, both ) ) & Quotation & " " );
    put( test_case_file, "assertions=" & Quotation & trim( jtc.assertionCnt'img, both ) & Quotation & " " );
    put( test_case_file, "failures=" & Quotation & trim( jtc.failureCnt'img, both ) & Quotation & " " );
    put( test_case_file, "errors=" & Quotation & trim( jtc.errorCnt'img, both ) & Quotation & " " );
    put( test_case_file, "time=" & Quotation & trim( elapsedSeconds'img, both ) & Quotation & ">" );
    new_line( test_case_file );
    -- TODO: should be cleaned up
    while not failureList.isEmpty( jtc.failureMsgs ) loop
       failureList.Pull( jtc.failureMsgs, msg );
       if jtc.description = "" then
          put_line( test_case_file, "    <failure message=" & Quotation & encode_message( msg ) &
            Quotation & ">" & jtc.file & ":" & trim( jtc.line, both ) & "</failure>" );
       else
          put_line( test_case_file, "    <failure message=" & Quotation & encode_message( msg ) &
            Quotation & ">" & jtc.file & ":" & trim( jtc.line, both ) &
            ":" & encode_message( jtc.description ) & "</failure>" );
       end if;
    end loop;
    -- TODO: should be cleaned up
    while not errorList.isEmpty( jtc.errorMsgs ) loop
       errorList.Pull( jtc.errorMsgs, msg );
       if jtc.description = "" then
          put_line( test_case_file, "    <error message=" & Quotation & encode_message( msg ) &
            Quotation & ">" & jtc.file & ":" & trim( jtc.line, both ) & "</error>" );
       else
          put_line( test_case_file, "    <error message=" & Quotation & encode_message( msg ) &
            Quotation & ">" & jtc.file & ":" & trim( jtc.line, both ) &
            ":" & encode_message( jtc.description ) & "</error>" );
       end if;
    end loop;
    -- TODO: nothing actually skipped yet.
    if jtc.skipped then
       put( test_case_file, "<skipped />" );
    end if;
    put_line( test_case_file, "  </testcase>" );
    jtc.isOpen := false;
    isTestCaseStarted := false;
  end endJunitTestCase;

  procedure testCaseError ( report: in out xmlTestReport ) is
  begin
    jtc.errorCnt := jtc.errorCnt + 1;
    wasTestErrorOrFailure := true;
    errorList.Queue( jtc.errorMsgs, toEscaped( fullErrorMessage.gccMessage ) );
  end testCaseError;

  procedure testCaseFailure( report: in out xmlTestReport; test_message : unbounded_string := null_unbounded_string ) is
    cmdline    : messageStrings;
    firstpos   : natural;
    lastpos    : natural;
    lineno     : natural;
    fileno     : natural;
    -- sfr        : aSourceFile;
  begin
     -- if an error occurred in the test, ignore the success or failure
    if jtc.errorCnt = 0 then
       wasTestErrorOrFailure := true;
       jtc.assertionCnt := jtc.assertionCnt + 1;
       jtc.failureCnt := jtc.failureCnt + 1;
       getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
       -- sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
       if test_message /= null_unbounded_string then
          failureList.Queue( jtc.failureMsgs, test_message );
       else
          -- TODO: if the test was multi-line, this will only show
          -- the final line.
          failureList.Queue( jtc.failureMsgs, toEscaped( cmdLine.gccMessage ) );
       end if;
    elsif jtc.errorCnt = 1 then
       failureList.Clear( jtc.failureMsgs );
    end if;
  end testCaseFailure;

  procedure testCaseSuccess ( report: in out xmlTestReport ) is
  begin
     -- if an error occurred in the test, ignore the success or failure
    if jtc.errorCnt = 0 then
       jtc.assertionCnt := jtc.assertionCnt + 1;
       --jtc.successCnt := jtc.successCnt + 1;
    end if;
  end testCaseSuccess;

  procedure startJUnitTestSuite( report: in out xmlTestReport; name : unbounded_string := null_unbounded_string ) is
    cmdline    : messageStrings;
    firstpos   : natural;
    lastpos    : natural;
    lineno     : natural;
    fileno     : natural;
    sfr        : aSourceFile;
  begin
    jts.isOpen := true;
    --jts.path := path;
    getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
    sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
    if length( name ) = 0 then
      raise SPARFORTE_ERROR;
    end if;
    jts.name := name;
    jts.path := sfr.name; -- TODO: path, but this is a name
    jts.startTime := ada.calendar.clock;
    create( test_case_file, out_file );
    lastSourceFile := fileNo;
    isTestSuiteStarted := true;
  end startJUnitTestSuite;

  procedure endJunitTestSuite( report: in out xmlTestReport ) is
    elapsedDays : Day_Count;
    elapsedSeconds : duration;
    elapsedLeapSeconds : Leap_Seconds_Count;
    s : unbounded_string;
  begin
    Difference( ada.calendar.clock, jts.startTime, elapsedDays, elapsedSeconds, elapsedLeapSeconds );
    put( test_result_file, "<testsuite name=" & Quotation & encode_class( jts.name ) & Quotation & " " );
    put( test_result_file, "file=" & Quotation & to_string( jts.path ) & Quotation & " " );
    put( test_result_file, "tests=" & Quotation & trim( jts.testCnt'img, both ) & Quotation & " " ); -- TODO: trim
    put( test_result_file, "assertions=" & Quotation & trim( jts.assertionCnt'img, both ) & Quotation & " " );
    put( test_result_file, "failures=" & Quotation & trim( jts.failureCnt'img, both ) & Quotation & " " );
    put( test_result_file, "errors=" & Quotation & trim( jts.errorCnt'img, both ) & Quotation & " " );
    put( test_result_file, "time=" & Quotation & trim( elapsedSeconds'img, both ) & Quotation & ">" );
    new_line( test_result_file );
    -- insert test cases
    if is_open( test_case_file ) then
       reset( test_case_file, in_file );
       while not end_of_file( test_case_file ) loop
         s := get_line( test_case_file );
         put_line( test_result_file, s );
       end loop;
       delete( test_case_file );
    end if;
    put_line( test_result_file, "</testsuite>" );
    jts.isOpen := false;
    isTestCaseStarted := false;
  end endJunitTestSuite;

  procedure checkForNewTestSuite( report: in out xmlTestReport ) is
    cmdline    : messageStrings;
    firstpos   : natural;
    lastpos    : natural;
    lineno     : natural;
    fileno     : natural;
    sfr        : aSourceFile;
  begin
    getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
    if lastSourceFile /= fileNo then
       if lastSourceFile /= 0 then -- or if not isJunitTestSuiteStarted then
          endJunitTestSuite( report );
       end if;
       sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
       startJUnitTestSuite( report, sfr.name );
    end if;
  end checkForNewTestSuite;

  procedure endJunit( report : in out xmlTestReport ) is
  begin
    put_line( test_result_file, "</testsuites>" );
    close( test_result_file );
  end endJunit;

end reports.test;
