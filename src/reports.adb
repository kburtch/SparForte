------------------------------------------------------------------------------
-- Report Utilities                                                         --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2020 Free Software Foundation              --
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
     spar_os.tty,
     string_util,
     user_io;
use  ada.strings.unbounded.text_io,
     ada.calendar,
     spar_os.tty,
     string_util,
     user_io;

package body reports is

  -- THIS IS A STUB to avoid bringing in user_io dependencies
  --function optional_bold( s : string ) return unbounded_string is
  --begin
    --return "*" & to_unbounded_string( s ) & "*";
  --end optional_bold;

  -- Sort case-insensitive and ignoring any "... := " that function synopses have

  function insensitiveGreaterThan( left, right : unbounded_string ) return boolean is
    l, r : unbounded_string;
    p : natural;
  begin
    l := ToUpper( left );
    p := index( l, " := " );
    if p > 0 then
      delete( l, 1, p+3 );
    end if;

    r := ToUpper( right );
    p := index( r, " := " );
    if p > 0 then
      delete( r, 1, p+3 );
    end if;

     return l >= r;
  end insensitiveGreaterThan;

  ----------------------------------------------------------------------------
  --
  -- ROOT REPORT
  --
  -- Classes for rendering different reports in different formats.
  --
  ----------------------------------------------------------------------------

  -- START
  --
  -- Start a new report.
  ----------------------------------------------------------------------------

  procedure start( r : in out rootReport'class ) is
    s : unbounded_string;
    ch : character;
  begin
    -- get the terminal width
    s := term( cols );
    -- strip any EOL or leading space characters
    while length( s ) > 0 loop
       ch := element(s, length(s) );
       exit when ch in '0'..'9';
       delete( s, length(s), length(s) );
    end loop;
    while length( s ) > 0 loop
       ch := element(s, 1 );
       exit when ch in '0'..'9';
       delete( s, 1, 1 );
    end loop;
    -- Convert to an integer.  If none, probably not a terminal.
    if length( s ) = 0 then
       r.screenWidth := 65536;
    else
      r.screenWidth := integer'value( to_string( s ) );
      -- constrain to reasonable limits
      if r.screenWidth < 20 then
         r.screenWidth := 20;
      elsif r.screenWidth > 160 then
         r.screenWidth := 160;
      end if;
    end if;
    -- create the temporary spool file
    create( r.outputfile, out_file );
  end start;

  -- FINISH
  --
  -- Complete the report and show the finished report on standard output.
  ----------------------------------------------------------------------------

  procedure finish( r : in out rootReport'class ) is
    s : unbounded_string;
  begin
    reset( r.outputfile, in_file );
    while not end_of_file( r.outputFile ) loop
       s := get_line( r.outputfile );
       put_line( to_string( s ) );
    end loop;
    delete( r.outputfile );
  end finish;

  ----------------------------------------------------------------------------
  --
  -- PLAIN TEXT REPORTS
  --
  ----------------------------------------------------------------------------

  --  RENDER REQUIRED TEXT
  --
  -- Draw left-justified text on the screen.  Honour initial indentation, if
  -- any.
  -- TODO: doesn't handle unprintable characters
  ----------------------------------------------------------------------------

  procedure renderRequiredText( r : in out textReport'class; s : unbounded_string ) is
    partialString : unbounded_string := s;
    initialIndent : natural;
    initialPos : positive;
    pos : positive;
    ch  : character;
  begin
    initialIndent := r.lineWidth;
    while length( partialString ) > 0 loop
       -- delete leading spaces
       while length( partialString ) > 0 loop
          exit when element( partialString, 1 ) /= ' ';
          delete( partialString, 1, 1 );
       end loop;
       exit when length( partialString ) = 0;
       -- determine maximum string length
       initialPos := r.screenWidth - initialIndent;
       if initialPos > length( partialString ) then
          -- it all fits
          initialPos := length( partialString );
          pos := initialPos;
       else
          -- back up from max length to first printable character
          pos := initialPos;
          while pos > 1 loop
             ch := element( partialString, pos );
          exit when ch = ' ';
             pos := pos - 1;
          end loop;
          -- only one word?
          -- back up from max length to first printable character
          if pos = 1 then
             pos := length( partialString );
          else
             while pos > 1 loop
                ch := element( partialString, pos );
             exit when ch /= ' ';
                pos := pos - 1;
             end loop;
          end if;
       end if;
       -- display substring and delete it
       put_line( r.outputfile, slice( partialString, 1, pos ) );
       delete( partialString, 1, pos );
       -- maintain indent, if any, and if more text to do
       if length( s ) > 0 then
          if initialIndent > 1 then
             put( r.outputfile, to_string( initialIndent * " " ) );
          end if;
       end if;
    end loop;
    r.lineWidth := 0;
  end renderRequiredText;


  --  RENDER TEXT
  --
  -- Draw left-justified text on the screen.  Honour initial indentation, if
  -- any. If there is a name for the text, draw the name.  If there is no
  -- text, draw nothing.
  ----------------------------------------------------------------------------

  procedure renderText( r : in out textReport'class; name : string; s : unbounded_string ) is
  begin
     if length( s ) > 0 then
        new_line( r.outputfile );
        if name'length > 0 then
           renderRequiredText( r,  optional_bold( name ) & ": " & s );
        else
           renderRequiredText( r, s );
        end if;
     end if;
  end renderText;


  --  RENDER DESCRIPTION
  --
  -- Draw left-justified text at the given indent.  If there is no text,
  -- draw nothing.
  ----------------------------------------------------------------------------

  procedure renderDescription( r : in out textReport'class; indent : positive; s : unbounded_string ) is
  begin
     if length( s ) > 0 then
        new_line( r.outputfile );
        put( r.outputfile, indent * " " );
        r.lineWidth := r.lineWidth + indent;
        renderRequiredText( r, s );
     end if;
  end renderDescription;


  --  RENDER TABLE
  --
  -- Draw a columned table.  If there are no items, draw nothing.
  ----------------------------------------------------------------------------

  procedure renderTable( r : in out textReport'class; l : in out contentList.List; name : string; columnWidth : positive ) is
    newLineWidth : natural;
    s : unbounded_string;
  begin
   if not contentList.isEmpty( l ) then
      put_line( r.outputfile, optional_bold( name ) & ": " );
      while not contentList.isEmpty( l ) loop
         contentList.Pull( l, s );
         newLineWidth := r.lineWidth + columnWidth + 1;
         if newLineWidth > r.screenWidth then
            r.lineWidth := 0;
            new_line( r.outputfile );
         elsif r.lineWidth > 0 then
            put( r.outputfile, to_string( integer( columnWidth-natural( length(s) ) + 1 ) * " " ) );
         end if;
         put( r.outputfile, to_string( s ) );
         r.lineWidth := r.lineWidth + columnWidth + 1;
      end loop;
      r.lineWidth := 0;
      new_line( r.outputfile );
   end if;
  end renderTable;

  --  RENDER PACKAGE CONTENT
  --
  -- Draw a columned table.  If there are no items, draw nothing.
  ----------------------------------------------------------------------------

  procedure renderPackageContent( r : in out textReport'class; l : in out contentList.List; columnWidth : positive; indentWidth : natural := 0 ) is
    newLineWidth : natural;
    nextTab : natural;
    s : unbounded_string;
    firstNewLine : boolean := true;
  begin
    if not contentList.isEmpty( l ) then
      new_line( r.outputfile );
      nextTab := indentWidth;
      r.lineWidth := indentWidth;
      while not contentList.isEmpty( l ) loop
         contentList.Pull( l, s );

         -- a blank line denotes a new section
         if length( s ) = 0 then
           new_line( r.outputfile );
           -- suppress first newline or we'll get a double-space
           if firstNewLine then
             firstnewLine := false;
           else
             new_line( r.outputfile );
           end if;
            if not contentList.isEmpty( l ) then
               contentList.Pull( l, s );
               put_line( r.outputfile, to_string( s ) );
               new_line( r.outputfile );
            end if;
            r.lineWidth := 0;
            nextTab := indentWidth;

          -- add a new item to the line with the same column width
          -- if the column width is exceeded, start a new line
         else
            newLineWidth := r.lineWidth + columnWidth + 1;
            if newLineWidth > r.screenWidth then
               r.lineWidth := 0;
               nextTab := indentWidth;
               new_line( r.outputfile );
            --elsif r.lineWidth > 0 then
            end if;
            if nextTab > 0 then
               put( r.outputfile, nextTab * " " );
            end if;
            put( r.outputfile, to_string( s ) );
            r.lineWidth := r.lineWidth + columnWidth + 1;
            nextTab := columnWidth-natural( length(s) ) + 1;
         end if;
      end loop;
      r.lineWidth := 0;
      new_line( r.outputfile );
    end if;
  end renderPackageContent;

  --  RENDER BULLET LIST
  --
  -- Draw a bullet list as text.  If there is only one item, show the item
  -- without a bullet list (to save screen space).  If there are no items,
  -- draw nothing.
  ----------------------------------------------------------------------------
  -- TODO: parameters are reversed

  procedure renderBulletList( r : in out textReport'class; l : in out contentList.List; name : string ) is
    --lineWidth : natural := 0;
    --newLineWidth : natural;
    s : unbounded_string;
  begin
   if not contentList.isEmpty( l ) then
      new_line( r.outputfile );
      put( r.outputfile, optional_bold( name ) & ": " );
      if contentList.length( l ) = 1 then
         contentList.Pull( l, s );
         put_line( r.outputfile, to_string( s ) );
      else
         new_line( r.outputfile );
         while not contentList.isEmpty( l ) loop
            contentList.Pull( l, s );
            put_line( r.outputfile, " * " & to_string( s ) );
         end loop;
      end if;
   end if;
   r.lineWidth := 0;
  end renderBulletList;


  ----------------------------------------------------------------------------
  --
  -- HTML REPORTS
  --
  ----------------------------------------------------------------------------


  procedure renderText( r : in out htmlReport'class; name : string; s : unbounded_string ) is
  begin
    if length( s ) > 0 then
       put_line( r.outputfile, "<h3>" & name & "</h3>" );
       put_line( r.outputfile, "<p>" & to_string( s ) & "</p>" );
    end if;
  end renderText;


  procedure renderDescription( r : in out htmlReport'class; indent : positive; s : unbounded_string ) is
  begin
    -- TOOD: indent
    put_line( r.outputfile, "<p style=" & ASCII.Quotation &
       "padding: 0px 20px 0px 20px" &
       ASCII.Quotation & ">" & to_string( s ) & "</p>" );
  end renderDescription;

  procedure renderTable( r : in out htmlReport'class; l : in out contentList.List; name : string; columnWidth : positive ) is
    s : unbounded_string;
  begin
    if not contentList.isEmpty( l ) then
       put_line( r.outputfile, "<h3>" & name & "</h3>" );
       if contentList.length( l ) = 1 then
          contentList.Pull( l, s );
          put_line( r.outputfile, "<p>" & s & "</p>" );
       else
          put_line( r.outputfile, "<table>" );
          -- TODO: handle columns in table
          while not contentList.isEmpty( l ) loop
             contentList.Pull( l, s );
             put_line( r.outputfile, "<tr>" );
             put_line( r.outputfile, "<td>" & s & "</td>" );
             put_line( r.outputfile, "</tr>" );
          end loop;
          put_line( r.outputfile, "</table>" );
       end if;
    end if;
  end renderTable;


  procedure renderPackageContent( r : in out htmlReport'class; l : in out contentList.List; name : string ) is
    s : unbounded_string;
    num_per_row : natural := 3;
    row_count : positive := 1;
    max_length : natural := 0;

    new_subsection : boolean := false;
    table_open : boolean := false;

    procedure table_tag is
    begin
       if table_open then
          put_line( r.outputfile, "</tr></table>" );
       end if;
       put_line( r.outputfile, "<table style=" & ASCII.Quotation &
           "margin: 0px 10px 0px 20px" &
           ASCII.Quotation & "><tr>" );
       table_open := true;
    end table_tag;

    procedure td_tag( s : unbounded_string ) is
    begin
      put_line( r.outputfile, "<td style=" & ASCII.Quotation &
                "padding: 10px" &
                ASCII.Quotation & ">" & s & "</td>" );
    end td_tag;

    procedure h3_tag( s : unbounded_string ) is
    begin
       if table_open then
          put_line( r.outputfile, "</tr></table>" );
          table_open := false;
       end if;
      put_line( r.outputfile, "<h3>" & s & "</h3>" );
    end h3_tag;

  begin
    if not contentList.isEmpty( l ) then

       -- There's no way to really know how something will look in a web
       -- browser window, as people can zoom in and out, etc.  We'll try
       -- for reasonable defaults for my laptop.

       for list_pos in 1..contentList.Length( l ) loop
          contentList.Find( l, list_pos, s );
          if length( s ) > max_length then
             max_length := length( s );
          end if;
       end loop;
       num_per_row := 120 / max_length;
       if num_per_row < 0 then
          num_per_row := 1;
       end if;

       while not contentList.isEmpty( l ) loop
          contentList.Pull( l, s );
          if new_subsection then
             h3_tag( s );
             new_subsection := false;
             row_count := 1;
          -- A blank line indicates the end of a section
          elsif length( s ) = 0 then
             new_subsection := true;
          elsif row_count = 1 then
             if not table_open then
                table_tag;
             end if;
             td_tag( s );
             row_count := row_count + 1;
          elsif row_count > num_per_row then
             row_count := 1;
             put_line( r.outputfile, "</tr><tr>" );
             td_tag( s );
          else
             td_tag( s );
             row_count := row_count + 1;
          end if;
       end loop;
       if table_open then
          put_line( r.outputfile, "</tr></table>" );
       end if;
    end if;
  end renderPackageContent;


  procedure renderBulletList( r : in out htmlReport'class; l : in out contentList.List; name : string ) is
    s : unbounded_string;
  begin
    if not contentList.isEmpty( l ) then
       put_line( r.outputfile, "<h3>" & name & "</h3>" );
       if contentList.length( l ) = 1 then
          contentList.Pull( l, s );
          put_line( r.outputfile, "<p>" & s & "</p>" );
       else
          put_line( r.outputfile, "<ul>" );
          while not contentList.isEmpty( l ) loop
             contentList.Pull( l, s );
             put_line( r.outputfile, "<li>" & s & "</li>" );
          end loop;
          put_line( r.outputfile, "</ul>" );
       end if;
    end if;
  end renderBulletList;


  ----------------------------------------------------------------------------
  --
  -- MAN PAGE REPORTS
  --
  ----------------------------------------------------------------------------

  procedure renderText( r : in out manPageReport'class; name : string; s : unbounded_string ) is
  begin
    if length( s ) > 0 then
       if name'length > 0 then
          put_line( r.outputfile, ".SH " &
               to_string( ToUpper( to_unbounded_string( name ) ) ) );
       else
          put_line( r.outputfile, ".SH ADDITIONAL NOTES" );
       end if;
       put_line( r.outputfile, to_string( s ) );
    end if;
  end renderText;
  -- TODO: probably need a renderFooter

  procedure renderDescription( r : in out manPageReport'class; indent : positive; s : unbounded_string ) is
  begin
    put_line( r.outputfile, ".SH DESCRIPTION" );
    put_line( r.outputfile, s );
  end renderDescription;

  procedure renderTable( r : in out manPageReport'class; l : in out contentList.List; name : string; columnWidth : positive ) is
    s : unbounded_string;
  begin
    if not contentList.isEmpty( l ) then
       put_line( r.outputfile, ".SH " &
          to_string(  ToUpper( to_unbounded_string( name ) ) ) );
       while not contentList.isEmpty( l ) loop
          contentList.Pull( l, s );
          put_line( r.outputfile, s );
       end loop;
    end if;
  end renderTable;


  procedure renderBulletList( r : in out manPageReport'class; l : in out contentList.List; name : string ) is
    s : unbounded_string;
  begin
    if not contentList.isEmpty( l ) then
       put_line( r.outputfile, ".SH " &
          to_string( ToUpper( to_unbounded_string( name ) ) ) );
       while not contentList.isEmpty( l ) loop
          contentList.Pull( l, s );
          put_line( r.outputfile, to_string( s ) );
       end loop;
    end if;
  end renderBulletList;


  procedure renderPackageContent( r : in out manPageReport'class; l : in out contentList.List; name : string ) is
    s : unbounded_string;
    new_subsection : boolean := false;
  begin
    if not contentList.isEmpty( l ) then

       while not contentList.isEmpty( l ) loop
          contentList.Pull( l, s );
          if new_subsection then
             put_line( r.outputfile, s );
             put_line( r.outputfile, ".PP" );
             new_subsection := false;
          -- A blank line indicates the end of a section
          elsif length( s ) = 0 then
             new_subsection := true;
             put_line( r.outputfile, ".PP" );
          else
             put_line( r.outputfile, ".IP \[bu] 2" );
             put_line( r.outputfile, s );
          end if;
       end loop;

       put_line( r.outputfile, ".PP" );
    end if;
  end renderPackageContent;


  ----------------------------------------------------------------------------
  --
  -- MARKDOWN REPORTS
  --
  ----------------------------------------------------------------------------


  --  RENDER TEXT (Markdown)
  --
  -- Draw left-justified text on the screen.  Honour initial indentation, if
  -- any. If there is a name for the text, draw the name.  If there is no
  -- text, draw nothing.
  ----------------------------------------------------------------------------

  procedure renderText( r : in out markdownReport'class; name : string; s : unbounded_string ) is
  begin
    if length( s ) > 0 then
       if name'length > 0 then
          -- headings in markdown are underlined
          put_line( r.outputfile, name );
          put_line( r.outputfile, name'length * "-" );
       end if;
       put_line( r.outputfile, to_string( s ) );
    end if;
  end renderText;

  --  RENDER DESCRIPTION (Markdown)
  --
  -- Draw left-justified text at the given indent.  If there is no text,
  -- draw nothing.
  ----------------------------------------------------------------------------

  procedure renderDescription( r : in out markdownReport'class; indent : positive; s : unbounded_string ) is
  begin
    put( r.outputfile, indent * " " );
    put_line( r.outputfile, s );
    -- TODO: not sure if this is right
  end renderDescription;

  --  RENDER TABLE (Markdown)
  --
  -- Draw a columned table.  If there are no items, draw nothing.  In
  -- Markdown, this is an HTML table.
  ----------------------------------------------------------------------------
  -- TODO: columns

  procedure renderTable( r : in out markdownReport'class; l : in out contentList.List; name : string; columnWidth : positive ) is
    s : unbounded_string;
  begin
    if not contentList.isEmpty( l ) then
       put_line( r.outputfile, name );
       put_line( r.outputfile, name'length * "-" );
       if contentList.length( l ) = 1 then
          contentList.Pull( l, s );
          put_line( r.outputfile, "<p>" & s & "</p>" );
       else
          put_line( r.outputfile, "<table>" );
          -- TODO: handle columns in table
          while not contentList.isEmpty( l ) loop
             contentList.Pull( l, s );
             put_line( r.outputfile, "<tr>" );
             put_line( r.outputfile, "<td>" & s & "</td>" );
             put_line( r.outputfile, "</tr>" );
          end loop;
          put_line( r.outputfile, "</table>" );
       end if;
    end if;
  end renderTable;

  --  RENDER BULLET LIST
  --
  -- Draw a bullet list as text.  If there is only one item, show the item
  -- without a bullet list (to save screen space).  If there are no items,
  -- draw nothing.  In Markdown, bullets are leading asterisks.
  ----------------------------------------------------------------------------
  -- TODO: parameters are reversed

  procedure renderBulletList( r : in out markdownReport'class; name : string; l : in out contentList.List ) is
    s : unbounded_string;
  begin
    if not contentList.isEmpty( l ) then
       put_line( r.outputfile, name );
       put_line( r.outputfile, length( s ) * "-" );
       if contentList.length( l ) = 1 then
          contentList.Pull( l, s );
          put_line( r.outputfile, s );
       else
          while not contentList.isEmpty( l ) loop
             contentList.Pull( l, s );
             put_line( r.outputfile, "* " & s );
          end loop;
       end if;
    end if;
  end renderBulletList;

  ----------------------------------------------------------------------------
  --
  -- XML REPORTS
  --
  ----------------------------------------------------------------------------
  -- TODO: none yet

end reports;

