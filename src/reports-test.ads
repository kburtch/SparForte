------------------------------------------------------------------------------
-- Automated Test Reports                                                   --
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

with ada.calendar,
     pegasoft.gen_list;

package reports.test is

  type test_natural is new natural;
  type assertion_natural is new natural;
  type error_natural is new natural;
  type failure_natural is new natural;

  package errorList is new pegasoft.gen_list( unbounded_string, "=", ">=" );
  package failureList is new pegasoft.gen_list( unbounded_string, "=", ">=" );

  wasTestErrorOrFailure : boolean := false;

  type junitState is record
    inTestCase  : boolean := false;
    inTestSuite : boolean := false;
    totalTests  : test_natural := 0;
  end record;

  type junitTestCase is record
    name         : unbounded_string;
    description  : unbounded_string;
    class        : unbounded_string;
    file         : unbounded_string;
    line         : unbounded_string;
    assertionCnt : assertion_natural := 0;
    failureCnt   : failure_natural := 0;
    errorCnt     : error_natural := 0;
    startTime    : ada.calendar.time;
    errorMsgs    : errorList.list;
    failureMsgs  : failureList.list;
    skipped      : boolean := false;
    isOpen       : boolean := false;
    testNo       : natural := 0;
  end record;

  type junitTestSuite is record
    name         : unbounded_string;
    path         : unbounded_string;
    startTime    : ada.calendar.time;
    testCnt      : test_natural := 1;
    assertionCnt : assertion_natural := 0;
    failureCnt   : failure_natural := 0;
    errorCnt     : error_natural := 0;
    isOpen       : boolean := false;
  end record;

  ----------------------------------------------------------------------------
  --
  -- TESTING REPORTS
  --
  ----------------------------------------------------------------------------
  -- TODO: different test report formats

  -- TODO: xmlReport not yet written, so use text report

  function isJunitStarted return boolean;

  function isJunitTestCaseStarted return boolean;

  function isJunitTestSuiteStarted return boolean;

  ----------------------------------------------------------------------------
  --
  -- TEXT REPORTS
  --
  -- rootReport -> textReport -> textTestReport
  --
  ----------------------------------------------------------------------------

  type textTestReport is new textReport with null record;

  procedure startJUnitTestCase( report: in out textTestReport ;
    name, description : unbounded_string := null_unbounded_string );

  procedure endJunitTestCase( report : in out textTestReport );

  -- TEST CASE ERROR
  --
  -- Record an exception or some other unexpected problem with the testing.

  procedure testCaseError( report : in out textTestReport );

  -- TEST CASE FAILURE
  --
  -- Record a test case failure.

  procedure testCaseFailure( report : in out textTestReport; test_message : unbounded_string := null_unbounded_string );

  -- TEST CASE SUCCESS
  --
  -- Record a test case success.

  procedure testCaseSuccess( report : in out textTestReport );

  -- more here

  procedure startJUnitTestSuite( report: in out textTestReport;
     name : unbounded_string := null_unbounded_string;
     test_name : unbounded_string := null_unbounded_string );
  procedure endJunitTestSuite( report : in out textTestReport );
  procedure checkForNewTestSuite( report : in out textTestReport;
     test_name : unbounded_string := null_unbounded_string );

  procedure startJunit( report : in out textTestReport );
  procedure endJunit( report : in out textTestReport );

  ----------------------------------------------------------------------------
  --
  -- JUNIT XML REPORTS
  --
  -- rootReport -> textReport* -> xmlTestReport
  --
  ----------------------------------------------------------------------------

  type xmlTestReport is new textReport with null record;

  procedure startJUnitTestCase( report: in out xmlTestReport ;
     name, description : unbounded_string := null_unbounded_string );
  procedure endJunitTestCase( report : in out xmlTestReport );

    -- TEST CASE ERROR
  --
  -- Record an exception or some other unexpected problem with the testing.

  procedure testCaseError( report : in out xmlTestReport );

  -- TEST CASE FAILURE
  --
  -- Record a test case failure.

  procedure testCaseFailure( report : in out xmlTestReport; test_message : unbounded_string := null_unbounded_string );

  -- TEST CASE SUCCESS
  --
  -- Record a test case success.

  procedure testCaseSuccess( report : in out xmlTestReport );

  procedure startJUnitTestSuite( report: in out xmlTestReport;
      name : unbounded_string := null_unbounded_string );
  procedure endJunitTestSuite( report : in out xmlTestReport );
  procedure checkForNewTestSuite( report : in out xmlTestReport );

  procedure startJunit( report : in out xmlTestReport; path : unbounded_string );
  procedure endJunit( report : in out xmlTestReport );

end reports.test;

-- vim: ft=spar
