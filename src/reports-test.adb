------------------------------------------------------------------------------
-- Built-in Help Command Reports                                            --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2017 Free Software Foundation              --
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
     Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package body reports.test is

  -- JUNIT Utilities

  Quotation : character := Ada.Characters.Latin_1.Quotation;

  type test_natural is new natural;
  type assertion_natural is new natural;
  type error_natural is new natural;
  type failure_natural is new natural;

  type junitState is record
    inTestCase  : boolean := false;
    inTestSuite : boolean := false;
    totalTests  : test_natural := 0;
  end record;

  type junitTestCase is record
    name         : unbounded_string;
    class        : unbounded_string;
    file         : unbounded_string;
    line         : unbounded_string;
    assertionCnt : assertion_natural := 1;
    failureCnt   : failure_natural := 0;
    errorCnt     : error_natural := 0;
    startTime    : ada.calendar.time;
    failureMsg   : unbounded_string;
    errorMsg     : unbounded_string;
    skipped      : boolean := false;
    isOpen       : boolean := false;
  end record;

  type junitTestSuite is record
    name         : unbounded_string;
    path         : unbounded_string;
    startTime    : ada.calendar.time;
    assertionCnt : assertion_natural := 1;
    failureCnt   : failure_natural := 0;
    errorCnt     : error_natural := 0;
    isOpen       : boolean := false;
  end record;

  procedure startJunit is
  begin
    put_line( "<?xml version=" & ASCII.Quotation & "1.0" &
      ASCII.Quotation & " encoding="  & ASCII.Quotation & 
      "UTF-8"  & ASCII.Quotation & "?>" );
    put_line( "<testsuites>" );
  end startJunit;

  procedure startJUnitTestCase( jtc : in out junitTestCase; name, class, file : unbounded_string ) is
  begin
    jtc.isOpen := true;
    jtc.name := name;
    jtc.class := class;
    jtc.startTime := ada.calendar.clock;
  end startJUnitTestCase;

  procedure endJunitTestCase( jtc : in out junitTestCase ) is
    elapsed : duraton;
  begin
    js.testCnt := js.testCnt + 1;
    jts.assertionCnt := jts.assertionCnt + jtc.assertionCnt;
    jts.failureCnt := jts.failureCnt + jtc.failureCnt;
    jts.errorCnt := jts.errorCnt + jtc.errorCnt;
    elapsed := ada.calendar.clock - jtc.startTime;
    put( "  <testcase name=" & Quotation & jtc.name & Quotation & " " );
    put( "class=" & Quotation & jtc.class & Quotation & " " );
    put( "file=" & Quotation & jtc.file & Quotation & " " );
    put( "line=" & Quotation & jtc.line'img & Quotation & " " );
    put( "assertions=" & Quotation & jtc.assertionCnt'img & Quotation & " " );  -- TODO: trim
    put( "failures=" & Quotation & jtc.failureCnt'img & Quotation & " " );
    put( "errors=" & Quotation & jtc.errorCnt'img & Quotation & " " );
    put( "time=" & Quotation & elapsed & Quotation & ">"
    new_line;
    if length( jtc.failureMsg ) > 0 then
       put( "    <failure message=" & Quotation & "test failure" & 
         Quotation & ">" & jtc.failureMsg & "</failure>" );
    end if;
    if length( jtc.errorMsg ) > 0 then
       put( "    <error message=" & Quotation & "test error" & 
         Quotation & ">" & jtc.errorMsg & "</error>" );
    end if;
    if jtc.skipped then
       put( "<skipped />" );
       end if;
    put_line( "  </testcase>" );
    jtc.isOpen := false;
  end endJunitTestCase;

  procedure startJUnitTestSuite( jts : in out junitTestSuite; name, path : unbounded_string ) is
  begin
    jts.isOpen := true;
    jts.name := name;
    jts.path := path;
    jts.startTime := ada.calendar.clock;
  end startJUnitTestCase;

  procedure endJunitTestSuite( jts : in out junitTestSuite ) is
    elapsed : duraton;
  begin
    elapsed := ada.calendar.clock - jts.startTime;
    put( "<testsuite name=" & Quotation & jts.name & Quotation & " " );
    put( "file=" & Quotation & jts.path & Quotation & " " );
    put( "tests=" & Quotation & $jts.testCnt'img & Quotation & " " ); -- TODO: trim
    put( "assertions=" & Quotation & jts.assertionCnt'img & Quotation & " " );
    put( "failures=" & Quotation & jts.failureCnt'img & Quotation & " " );
    put( "errors=" & Quotation & jts.errorCnt'img & Quotation & " " );
    put( "time=" & Quotation & elapsed'img & Quotation & ">"
    new_line;
    -- TODO: test cases go here
    put_line( "</testsuite>" );
    jts.isOpen := false;
  end endJunitTestSuite;

  procedure endJunit is
  begin
    put_line( "</testsuites>" );
  end endJunit;

end reports.test;