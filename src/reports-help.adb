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
with ada.strings.unbounded.text_io,
     ada.calendar,
     ada.text_io,
     cgi,
     pegasoft.strings,
     pegasoft.user_io,
     world;
use  ada.strings,
     ada.strings.unbounded.text_io,
     ada.calendar,
     ada.text_io,
     cgi,
     pegasoft.strings,
     pegasoft.user_io,
     world;

package body reports.help is

  ----------------------------------------------------------------------------
  --  HELP ENTRY
  --
  -- One help "page".  Help list is a collection of help entries.
  ----------------------------------------------------------------------------

  function ">="( left, right : aHelpEntry ) return boolean is
  begin
     return ToUpper(left.topic) >= ToUpper( right.topic );
  end ">=";

  procedure clearHelp( e : in out aHelpEntry ) is
  begin
    e.author := null_unbounded_string;
    contentList.Clear( e.accounts );
    contentList.Clear( e.bugs );
    contentList.Clear( e.content );
    contentList.Clear( e.sectionContent );
    e.inSection := false;
    e.description := null_unbounded_string;
    contentList.Clear( e.errors );
    contentList.Clear( e.examples );
    contentList.Clear( e.exceptions );
    e.createdOn := null_unbounded_string;
    e.footer := null_unbounded_string;
    contentList.Clear( e.rationale );
    e.modified := null_unbounded_string;
    contentList.Clear( e.params );
    e.returns := null_unbounded_string;
    e.seeAlso := null_unbounded_string;
    e.summary:= null_unbounded_string;
    contentList.Clear( e.todos );
    e.topic := null_unbounded_string;

    e.contentWidth    := 1;
    e.bugsWidth       := 1;
    e.errorsWidth     := 1;
    e.examplesWidth   := 1;
    e.rationaleWidth  := 1;
    e.paramsWidth     := 1;
    e.exceptionsWidth := 1;
    e.todosWidth      := 1;
    e.accountsWidth   := 1;
    e.empty := true;
  end clearHelp;

  procedure startHelp( e : in out aHelpEntry; topic : string ) is
  begin
    clearHelp( e );
    e.topic := to_unbounded_string( topic );
    e.empty := false;
  end startHelp;

  procedure addSection( e : in out aHelpEntry ) is
    s : unbounded_string;
  begin
    while not contentList.isEmpty( e.sectionContent ) loop
      contentList.Pull( e.sectionContent, s );
      contentList.Queue( e.content, s );
    end loop;
    e.inSection := false;
  end addSection;

  procedure endHelp( e : in out aHelpEntry ) is
  begin
    if e.inSection then
      addSection( e );
    end if;
  end endHelp;

  function isEmpty( e : aHelpEntry ) return boolean is
  begin
    return e.empty;
  end isEmpty;

  procedure author( e : in out aHelpEntry; s : string ) is
  begin
    e.author := to_unbounded_string( s );
  end author;

  procedure authorKen( e : in out aHelpEntry ) is
  begin
    author( e, "ken@pegasoft.ca" );
  end authorKen;

  procedure modified( e : in out aHelpEntry; s : string ) is
  begin
    e.modified := to_unbounded_string( s );
  end modified;

  procedure description( e : in out aHelpEntry; s : string ) is
  begin
    if length( e.description ) > 0 then
       e.description := e.description & " ";
    end if;
    e.description := e.description & to_unbounded_string( s );
  end description;

  procedure footer( e : in out aHelpEntry; s : string ) is
  begin
    e.footer := to_unbounded_string( s );
  end footer;

  procedure bugs( e : in out aHelpEntry; s : string ) is
  begin
     if s'length > 0 then
        contentList.Queue( e.bugs, to_unbounded_string( s ) );
        if s'length > e.bugsWidth then
           e.bugsWidth := positive( s'length );
        end if;
     end if;
  end bugs;

  procedure category( e : in out aHelpEntry; s : string ) is
  begin
    e.category := to_unbounded_string( s );
  end category;

  procedure categoryBuiltin( e : in out aHelpEntry ) is
  begin
    category( e, "built-in shell command" );
  end categoryBuiltin;

  procedure categoryFunction( e : in out aHelpEntry ) is
  begin
    category( e, "built-in function" );
  end categoryFunction;

  procedure categoryKeyword( e : in out aHelpEntry ) is
  begin
    category( e, "keyword" );
  end categoryKeyword;

  procedure categoryPackage( e : in out aHelpEntry ) is
  begin
    category( e, "built-in package" );
  end categoryPackage;

  procedure categoryProcedure( e : in out aHelpEntry ) is
  begin
    category( e, "built-in procedure" );
  end categoryProcedure;

  procedure content( e : in out aHelpEntry; s1, s2, s3, s4  : string := "" ) is

    procedure addContent( e : in out aHelpEntry; s : string ) is
    begin
      if s'length > 0 then
         contentList.Insert( e.content, to_unbounded_string( s ) );
         if s'length > e.contentWidth then
            e.contentWidth := positive( s'length );
         end if;
      end if;
    end addContent;

    procedure addSectionContent( e : in out aHelpEntry; s : string ) is
    begin
      if s'length > 0 then
         contentList.Insert( e.sectionContent, to_unbounded_string( s ) );
         if s'length > e.contentWidth then
            e.contentWidth := positive( s'length );
         end if;
      end if;
    end addSectionContent;

  begin
     if e.inSection then
       if s1'length > 0 then
          addSectionContent( e, s1 );
       end if;
       if s2'length > 0 then
          addSectionContent( e, s2 );
       end if;
       if s3'length > 0 then
          addSectionContent( e, s3 );
       end if;
       if s4'length > 0 then
          addSectionContent( e, s4 );
       end if;
     else
       if s1'length > 0 then
          addContent( e, s1 );
       end if;
       if s2'length > 0 then
          addContent( e, s2 );
       end if;
       if s3'length > 0 then
          addContent( e, s3 );
       end if;
       if s4'length > 0 then
          addContent( e, s4 );
       end if;
    end if;
  end content;

  procedure createdOn( e : in out aHelpEntry; s : string ) is
  begin
    e.createdOn := to_unbounded_string( s );
  end createdOn;

  procedure section( e : in out aHelpEntry; s : string ) is
  begin
    if e.inSection then
      addSection( e );
    end if;
    e.inSection := true;
    contentList.Queue( e.content, null_unbounded_string );
    contentList.Queue( e.content, to_unbounded_string( s ) );
  end section;

  procedure errors( e : in out aHelpEntry; s : string ) is
  begin
     if s'length > 0 then
        contentList.Queue( e.errors, to_unbounded_string( s ) );
        if s'length > e.errorsWidth then
           e.errorsWidth := positive( s'length );
        end if;
     end if;
  end errors;

  procedure examples( e : in out aHelpEntry; s : string ) is
  begin
     if s'length > 0 then
        contentList.Queue( e.examples, to_unbounded_string( s ) );
        if s'length > e.examplesWidth then
           e.examplesWidth := positive( s'length );
        end if;
     end if;
  end examples;

  procedure rationale( e : in out aHelpEntry; s : string ) is
  begin
     if s'length > 0 then
        contentList.Queue( e.rationale, to_unbounded_string( s ) );
        if s'length > e.rationaleWidth then
           e.rationaleWidth := positive( s'length );
        end if;
     end if;
  end rationale;

  procedure params( e : in out aHelpEntry; s : string ) is
  begin
     if s'length > 0 then
        contentList.Queue( e.params, to_unbounded_string( s ) );
        if s'length > e.paramsWidth then
           e.paramsWidth := positive( s'length );
        end if;
     end if;
  end params;

  procedure returns( e : in out aHelpEntry; s : string ) is
  begin
    e.returns := to_unbounded_string( s );
  end returns;

  procedure seeAlso( e : in out aHelpEntry; s : string ) is
  begin
    e.seeAlso := to_unbounded_string( s );
  end seeAlso;

  procedure seeAlsoShellCmds( e : in out aHelpEntry ) is
  begin
    seeAlso( e, "doc/ref_shellcmds.html" );
  end seeAlsoShellCmds;

  procedure seeAlsoFlowControl( e : in out aHelpEntry ) is
  begin
    seeAlso( e, "doc/ref_flow.html" );
  end seeAlsoFlowControl;

  procedure exceptions( e : in out aHelpEntry; s : string ) is
  begin
     if s'length > 0 then
        contentList.Queue( e.exceptions, to_unbounded_string( s ) );
        if s'length > e.exceptionsWidth then
           e.exceptionsWidth := positive( s'length );
        end if;
     end if;
  end exceptions;

  procedure summary( e : in out aHelpEntry; s : string ) is
  begin
    e.summary := to_unbounded_string( s );
  end summary;

  procedure todos( e : in out aHelpEntry; s : string ) is
  begin
     if s'length > 0 then
        contentList.Queue( e.todos, to_unbounded_string( s ) );
        if s'length > e.todosWidth then
           e.todosWidth := positive( s'length );
        end if;
     end if;
  end todos;

  procedure accounts( e : in out aHelpEntry; s : string ) is
  begin
     if s'length > 0 then
        contentList.Queue( e.accounts, to_unbounded_string( s ) );
        if s'length > e.accountsWidth then
           e.accountsWidth := positive( s'length );
        end if;
     end if;
  end accounts;

  procedure releaseVersion( e : in out aHelpEntry; s : string ) is
  begin
    e.version := to_unbounded_string( s );
  end releaseVersion;

  procedure icon( e : in out aHelpEntry; s : string ) is
  begin
    e.iconPath := to_unbounded_string( s );
  end icon;

  procedure screenshot( e : in out aHelpEntry; s : string ) is
  begin
    e.screenshotPath := to_unbounded_string( s );
  end screenshot;

  ----------------------------------------------------------------------------
  --
  -- LONG HELP REPORT
  --
  -- rootReport -> textReport -> longHelpReport
  --
  -- Detailed text-based help report.
  ----------------------------------------------------------------------------

  procedure render( r : in out longHelpReport; e : in out aHelpEntry ) is
  begin
     -- display the help topic and the header if there is one
     put( r.outputfile, optional_green( to_string( e.topic ), boolean( gccOpt ), boolean( colourOpt ) ) );
     if length( e.summary ) > 0 then
        put( r.outputfile, " " & utf_diamond & " " & to_string( e.summary ) );
     end if;
     if length( e.category ) > 0 then
        put( r.outputfile, " " & utf_diamond & " " & adorn_red( to_string( e.category ) ) );
     end if;
     new_line( r.outputfile );

     if length( e.iconPath ) > 0 then
        renderText( r, "Icon", e.iconPath );
     end if;
     if length( e.screenshotPath ) > 0 then
        renderText( r, "Screenshot", e.screenshotPath );
     end if;

     renderDescription( r, 2, e.description );

     -- parameters
     renderBulletList( r, e.params, "Parameter List" );

     renderText( r, "Returns", e.returns );

     renderPackageContent( r, e.content, e.contentWidth, 2 );

     -- examples
     renderBulletList( r, e.examples, "Examples" );

     -- exceptions
     renderBulletList( r, e.exceptions, "Exceptions" );

     renderBulletList( r, e.errors, "Errors" );

     renderText( r, "Author", e.author );
     renderText( r, "Created", e.createdOn );
     renderText( r, "Modified", e.modified );
     renderText( r, "Version", e.version );
     --if contentList.length( e.todos ) > 0 then -- TODO: needed?
     renderBulletList( r, e.todos, "To Do" );
     --end if;
     renderBulletList( r, e.rationale, "Rationale" );
     renderBulletList( r, e.bugs, "Bugs" );
     renderBulletList( r, e.accounts, "Accounts" );
     renderText( r, "See Also", e.seeAlso );

     -- display footer
     renderText( r, "", e.footer );
     clearHelp( e );
  end render;

  ----------------------------------------------------------------------------
  --
  -- LONG HTML HELP REPORT
  --
  -- rootReport -> htmlReport -> longHtmlHelpReport
  --
  -- Detailed text-based help report.
  ----------------------------------------------------------------------------
  -- TODO: html encoding

  procedure render( r : in out longHtmlHelpReport; e : in out aHelpEntry ) is
  begin
     -- display the help topic and the header if there is one
     put( r.outputfile, "<h2>" & html_encode( to_string( e.topic ) )  );
     if length( e.category ) > 0 then
        put( r.outputfile, " (" & html_encode( to_string( e.category ) ) & ")" );
     end if;
     put_line( r.outputfile, "</h2>" );
     if length( e.summary ) > 0 then
        put( r.outputfile, "<p>" & html_encode( to_string( e.summary ) ) & "</p>" );
     end if;
     new_line( r.outputfile );
     if length( e.iconPath ) > 0 then
        renderText( r, "Icon", e.iconPath );
     end if;
     if length( e.screenshotPath ) > 0 then
        renderText( r, "Screenshot", e.screenshotPath );
     end if;

     renderDescription( r, 2, e.description );

     -- parameters
     renderBulletList( r, e.params, "Parameter List" );

     renderText( r, "Returns", e.returns );

     -- TODO: renderPackageContent does not exist yet for HTML
     --renderPackageContent( r, e.content, e.contentWidth, 2 );
     renderPackageContent( r, e.content, "Content" );

     -- examples
     renderBulletList( r, e.examples, "Examples" );

     -- exceptions
     renderBulletList( r, e.exceptions, "Exceptions" );

     renderBulletList( r, e.errors, "Errors" );

     renderText( r, "Author", e.author );
     renderText( r, "Created", e.createdOn );
     renderText( r, "Modified", e.modified );
     renderText( r, "Version", e.version );
     renderBulletList( r, e.todos, "To Do" );
     renderBulletList( r, e.rationale, "Rationale" );
     renderBulletList( r, e.bugs, "Bugs" );
     renderBulletList( r, e.accounts, "Accounts" );
     renderText( r, "See Also", e.seeAlso );

     -- display footer
     renderText( r, "", e.footer );
     clearHelp( e );
  end render;


  ----------------------------------------------------------------------------
  --
  -- LONG MAN HELP REPORT
  --
  -- rootReport -> manReport -> longManHelpReport
  --
  -- Detailed groff help report.
  ----------------------------------------------------------------------------

  procedure render( r : in out longManPageHelpReport; e : in out aHelpEntry ) is
    genDate : unbounded_string;
  begin
    genDate := to_unbounded_string( integer'image( day( ada.calendar.clock ) ) );
    delete( genDate, 1, 1 );
    genDate := integer'image( month( ada.calendar.clock ) )& "-" & genDate;
    delete( genDate, 1, 1 );
    genDate := integer'image( ada.calendar.year( clock ) ) & "-" & genDate;
    delete( genDate, 1, 1 );

     put( r.outputfile, "./" & ASCII.Quotation & "man page " );
     put_line( r.outputfile, to_string( e.topic ) & ".9" );
     -- display the help topic and the header if there is one
     put( r.outputfile, ".TH " & ASCII.Quotation & to_string( e.topic ) &
        ASCII.Quotation & " 9 " );
     -- TODO: the date
     put( r.outputfile, ASCII.Quotation & genDate & ASCII.Quotation );
     -- TODO: company name
     put( r.outputfile, " " );
     put( r.outputfile, ASCII.Quotation & "Company" & ASCII.Quotation );
     put( r.outputfile, " " );
     put_line( r.outputfile, ASCII.Quotation & "Manual" & ASCII.Quotation );

     if length( e.summary ) > 0 then
        -- TODO: render required text
        put_line( r.outputfile, e.summary );
     end if;
     if length( e.category ) > 0 then
        -- TODO: render required text
        put_line( r.outputfile, ".SH CATEGORY" );
        put_line( r.outputfile,  e.category );
     end if;

     if length( e.iconPath ) > 0 then
        renderText( r, "Icon", e.iconPath );
     end if;
     if length( e.screenshotPath ) > 0 then
        renderText( r, "Screenshot", e.screenshotPath );
     end if;

     renderDescription( r, 2, e.description );

     -- parameters
     renderBulletList( r, e.params, "Parameter List" );

     renderText( r, "Returns", e.returns );

     renderPackageContent( r, e.content, "Content" );
     -- renderBulletList( r, e.content,"Content" );
--     renderPackageContent( r, e.content, "Content" );

     -- examples
     renderBulletList( r, e.examples, "Examples" );

     -- exceptions
     renderBulletList( r, e.exceptions, "Exceptions" );

     renderBulletList( r, e.errors, "Errors" );

     renderText( r, "Author", e.author );
     renderText( r, "Created", e.createdOn );
     renderText( r, "Modified", e.modified );
     renderText( r, "Version", e.version );
     --if contentList.length( e.todos ) > 0 then
     --   new_line( r.outputfile );
     renderBulletList( r, e.todos, "To Do" );
     --end if;
     renderBulletList( r, e.rationale, "Rationale" );
     renderBulletList( r, e.bugs, "Bugs" );
     renderBulletList( r, e.accounts, "Accounts" );
     renderText( r, "See Also", e.seeAlso );

     -- display footer
     renderText( r, "", e.footer );
     clearHelp( e );
  end render;

  ----------------------------------------------------------------------------
  --
  -- SHORT HELP REPORT
  --
  -- rootReport -> textReport -> shortHelpReport
  --
  -- Abbreviated text-based help report.
  ----------------------------------------------------------------------------
  -- TODO: not done

end reports.help;

