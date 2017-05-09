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

with ada.text_io,
     ada.strings.unbounded;

use ada.text_io,
    ada.strings.unbounded;

with gen_list;

package reports.help is

  ----------------------------------------------------------------------------
  --  HELP ENTRY
  --
  -- One help "page".  Help list is a collection of help entries.
  ----------------------------------------------------------------------------

  type aHelpEntry is record
       topic         : unbounded_string;
       summary       : unbounded_string;
       description   : unbounded_string;
       footer        : unbounded_string;
       bugs          : contentList.List;
       bugsWidth     : positive := 1;
       category      : unbounded_string;
       content       : contentList.List;
       contentWidth  : positive := 1;
       exceptions    : contentList.List;
       exceptionsWidth : positive := 1;
       implementationNotes : contentList.List;
       implementationWidth : positive := 1;
       seeAlso       : unbounded_string;
       author        : unbounded_string;
       modified      : unbounded_string;
       returns       : unbounded_string;
       errors        : contentList.List;
       errorsWidth   : positive := 1;
       examples      : contentList.List;
       examplesWidth : positive := 1;
       params        : contentList.List;
       paramsWidth   : positive := 1;
       todos         : contentList.List;
       todosWidth    : positive := 1;
       version       : unbounded_string;
       -- TODO: nesting
  end record;

  function ">="( left, right : aHelpEntry ) return boolean;

  package helpList is new gen_list( aHelpEntry, "=", ">=" );

  procedure startHelp( e : in out aHelpEntry; topic : string );

  procedure endHelp( e : in out aHelpEntry );

  procedure author( e : in out aHelpEntry; s : string );
  procedure authorKen( e : in out aHelpEntry );

  procedure bugs( e : in out aHelpEntry; s : string );

  procedure category( e : in out aHelpEntry; s : string );

  procedure content( e : in out aHelpEntry; s1, s2, s3, s4  : string := "" );
  procedure categoryBuiltin( e : in out aHelpEntry );
  procedure categoryPackage( e : in out aHelpEntry );
  procedure categoryKeyword( e : in out aHelpEntry );

  procedure description( e : in out aHelpEntry; s : string );

  procedure errors( e : in out aHelpEntry; s : string );

  procedure examples( e : in out aHelpEntry; s : string );

  procedure exceptions( e : in out aHelpEntry; s : string );

  procedure footer( e : in out aHelpEntry; s : string );

  procedure implementationNotes( e : in out aHelpEntry; s : string );

  procedure modified( e : in out aHelpEntry; s : string );
  procedure modifiedKen( e : in out aHelpEntry );

  procedure params( e : in out aHelpEntry; s : string );

  procedure returns( e : in out aHelpEntry; s : string );

  procedure section( e : in out aHelpEntry; s : string );

  procedure seeAlso( e : in out aHelpEntry; s : string );
  procedure seeAlsoShellCmds( e : in out aHelpEntry );
  procedure seeAlsoFlowControl( e : in out aHelpEntry );

  procedure summary( e : in out aHelpEntry; s : string );

  procedure todos( e : in out aHelpEntry; s : string );

  procedure version( e : in out aHelpEntry; s : string );

  ----------------------------------------------------------------------------
  --
  -- LONG HELP REPORT
  --
  -- rootReport -> textReport -> longHelpReport
  --
  -- Detailed text-based help report.
  ----------------------------------------------------------------------------

  type longHelpReport is new textReport with null record;

  procedure render( r : in out longHelpReport; e : in out aHelpEntry );

  type longHtmlHelpReport is new htmlReport with null record;

  procedure render( r : in out longHtmlHelpReport; e : in out aHelpEntry );

  type longManPageHelpReport is new manPageReport with null record;

  procedure render( r : in out longManPageHelpReport; e : in out aHelpEntry );

  ----------------------------------------------------------------------------
  --
  -- SHORT HELP REPORT
  --
  -- rootReport -> textReport -> shortHelpReport
  --
  -- Abbreviated text-based help report.
  ----------------------------------------------------------------------------

  type shortHelpReport is new textReport with null record;

  ----------------------------------------------------------------------------

  type machineReadableHelpReport is new textReport with null record;

  ----------------------------------------------------------------------------

  type manHelpReport is new manPageReport with null record;

  ----------------------------------------------------------------------------

  type htmlHelpReport is new htmlReport with null record;

end reports.help;

