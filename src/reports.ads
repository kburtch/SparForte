------------------------------------------------------------------------------
-- Report Utilities                                                         --
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

with unchecked_deallocation,
     ada.text_io,
     ada.strings.unbounded;

use ada.text_io,
    ada.strings.unbounded;

with gen_list;

package reports is

  -- REPORTS
  --
  -- This package is a collection of render functions which format data used
  -- in different types of SparForte reports.  Formats include human-readable
  -- text, HTML, markdown and others.
  --
  -- The child packages have the layout and content for the specific reports.
  -- Those reports use these classes here.
  --
  -- Every report must begin with start and end with finish.
  ----------------------------------------------------------------------------

  -- CONTENT LIST
  --
  -- A linked list of strings used in reports.
  ----------------------------------------------------------------------------

  function insensitiveGreaterThan( left, right : unbounded_string ) return boolean;

  package contentList is new gen_list( unbounded_string, "=", insensitiveGreaterThan );

  -- ROOT REPORT
  --
  -- All report formats are extended from this class.  This class has two
  -- procedures, one to start creating the report and one to output it.
  ----------------------------------------------------------------------------

  type rootReport is abstract tagged limited record
      -- nestingLevel : natural := 1;
      outputFile   : file_type;   -- temp spool file holding report
      screenWidth  : natural;     -- size of the display
  end record;
  type aRootReportPtr is access all rootReport'class;
  procedure free is new unchecked_deallocation( rootReport'class, aRootReportPtr );


  -- START
  --
  -- Start a new report.
  ----------------------------------------------------------------------------

  procedure start( r : in out rootReport'class );

  -- FINISH
  --
  -- Complete the report and show the finished report on standard output.
  ----------------------------------------------------------------------------

  procedure finish( r : in out rootReport'class );

  ----------------------------------------------------------------------------
  --
  -- Report Formats
  --
  ----------------------------------------------------------------------------

  type textReport is abstract new rootReport with private;
  -- human-readable text report (e.g. built-in help)

  type htmlReport is abstract new rootReport with null record;
  -- HTML fragment suitable for inserting into web pages

  type manPageReport is abstract new rootReport with null record;
  -- groff format like man command pages

  type markdownReport is abstract new rootReport with null record;
  -- Markdown format (as used by GitHub)

  type xmlReport is abstract new rootReport with null record;
  -- XML (not yet written)

private

  type textReport is new rootReport with record
     lineWidth    : natural := 0;
  end record;

  -- The rendering procedures available to specific reports in child packages.

  procedure renderRequiredText( r : in out textReport'class; s : unbounded_string );
  procedure renderText( r : in out textReport'class; name : string; s : unbounded_string );
  procedure renderDescription( r : in out textReport'class; indent : positive; s : unbounded_string );
  procedure renderTable( r : in out textReport'class; l : in out contentList.List;
    name : string; columnWidth : positive );
  procedure renderPackageContent( r : in out textReport'class; l : in out contentList.List;
    columnWidth : positive; indentWidth : natural := 0 );
  procedure renderBulletList( r : in out textReport'class; l : in out contentList.List; name : string );

  procedure renderText( r : in out htmlReport'class; name : string; s : unbounded_string );
  procedure renderDescription( r : in out htmlReport'class; indent : positive; s : unbounded_string );
  procedure renderTable( r : in out htmlReport'class; l : in out contentList.List;
    name : string; columnWidth : positive );
  procedure renderBulletList( r : in out htmlReport'class; l : in out contentList.List; name : string );

  procedure renderText( r : in out manPageReport'class; name : string; s : unbounded_string );
  procedure renderDescription( r : in out manPageReport'class; indent : positive; s : unbounded_string );
  procedure renderTable( r : in out manPageReport'class; l : in out contentList.List;
    name : string; columnWidth : positive );
  procedure renderBulletList( r : in out manPageReport'class; l : in out contentList.List; name : string );

  procedure renderText( r : in out markdownReport'class; name : string; s : unbounded_string );
  procedure renderDescription( r : in out markdownReport'class; indent : positive; s : unbounded_string );
  procedure renderTable( r : in out markdownReport'class; l : in out contentList.List; name : string; columnWidth : positive );
  procedure renderBulletList( r : in out markdownReport'class; name : string; l : in out contentList.List );

end reports;

