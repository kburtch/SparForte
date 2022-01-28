------------------------------------------------------------------------------
-- Built-in Shell Commands (Help)                                           --
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

with interfaces.c,
     ada.characters.Latin_1,
     ada.text_io,
     ada.strings.unbounded.text_io,
     ada.strings.fixed,
     ada.calendar,
     cgi,
     spar_os,
     pegasoft.strings,
     world,
     pegasoft.user_io,
     pegasoft.script_io,
     reports.help,
     jobs,
     compiler,
     scanner.communications,
     parser_db,
     parser_mysql,
     parser_teams,
     parser;  -- for pragma annotate
use  interfaces.c,
     ada.text_io,
     ada.strings.unbounded.text_io,
     ada.strings.fixed,
     ada.calendar,
     cgi,
     spar_os,
     pegasoft.strings,
     world,
     jobs,
     pegasoft.user_io,
     pegasoft.script_io,
     reports,
     reports.help,
     compiler,
     scanner.communications,
     parser_db,
     parser_mysql,
     parser_teams,
     parser;  -- for pragma annotate

package body builtins.help is

-- for help command
  HTMLOutput    : boolean := false;
  MANOutput     : boolean := false;
  LicenseOutput : boolean := false;
  TodoOutput    : boolean := false;
  CollabOutput  : boolean := false;

  -- Script Help

  e : aHelpEntry;
  r : aRootReportPtr;

-- Scan and script and provide help

     procedure DoScriptHelp( helpTopic : unbounded_string ) is
       scriptState : aScriptState;
       firstLine   : aliased unbounded_string;
       exprVal     : unbounded_string;
       info        : unbounded_string;
       genDate     : unbounded_string;
       -- not really in another script but we'll be safe...
       last_tag    : unbounded_string;
       closeResult : int;

       -- to-do's
       todoTotal                : natural := 0;
       workMeasureCntUnknown    : natural := 0;
       workMeasureCntHours      : natural := 0;
       workMeasureCntFpoints    : natural := 0;
       workMeasureCntSpoints    : natural := 0;
       workMeasureCntSloc       : natural := 0;
       workMeasureCntSizeS      : natural := 0;
       workMeasureCntSizeM      : natural := 0;
       workMeasureCntSizeL      : natural := 0;
       workMeasureCntSizeXL     : natural := 0;
       workPriorityCntUnknown   : natural := 0;
       workPriorityCntLevelL    : natural := 0;
       workPriorityCntLevelM    : natural := 0;
       workPriorityCntLevelH    : natural := 0;
       workPriorityCntSeverity1 : natural := 0;
       workPriorityCntSeverity2 : natural := 0;
       workPriorityCntSeverity3 : natural := 0;
       workPriorityCntSeverity4 : natural := 0;
       workPriorityCntSeverity5 : natural := 0;
       workPriorityCntRisk      : natural := 0;
       workPriorityCntCVSSMinor : natural := 0;
       workPriorityCntCVSSMajor : natural := 0;
       workPriorityCntCVSSCrit  : natural := 0;
       workPriorityCompleted    : natural := 0;
       measure : identifier;
       units   : unbounded_string;

       -- repeatedly used strings, converted to unbounded_strings

       advise_str   : constant unbounded_string := to_unbounded_string( "advise" );
       license_str  : constant unbounded_string := to_unbounded_string( "license" );
       todo_str     : constant unbounded_string := to_unbounded_string( "todo" );
       blocked_str  : constant unbounded_string := to_unbounded_string( "blocked" );
       clarify_str  : constant unbounded_string := to_unbounded_string( "clarify" );
       dispute_str  : constant unbounded_string := to_unbounded_string( "dispute" );
       propose_str  : constant unbounded_string := to_unbounded_string( "propose" );
       annotate_str : constant unbounded_string := to_unbounded_string( "annotate" );
       refactor_str : constant unbounded_string := to_unbounded_string( "refactor" );

       accounts_str    : constant unbounded_string := to_unbounded_string( "accounts" );
       author_str      : constant unbounded_string := to_unbounded_string( "author" );
       bugs_str        : constant unbounded_string := to_unbounded_string( "bugs" );
       created_str     : constant unbounded_string := to_unbounded_string( "created" );
       category_str    : constant unbounded_string := to_unbounded_string( "category" );
       content_str     : constant unbounded_string := to_unbounded_string( "content" );
       description_str : constant unbounded_string := to_unbounded_string( "description" );
       errors_str      : constant unbounded_string := to_unbounded_string( "errors" );
       examples_str    : constant unbounded_string := to_unbounded_string( "examples" );
       exceptions_str  : constant unbounded_string := to_unbounded_string( "exceptions" );
       footer_str      : constant unbounded_string := to_unbounded_string( "footer" );
       icon_str        : constant unbounded_string := to_unbounded_string( "icon" );
       modified_str    : constant unbounded_string := to_unbounded_string( "modified" );
       param_str       : constant unbounded_string := to_unbounded_string( "param" );
       return_str      : constant unbounded_string := to_unbounded_string( "return" );
       rationale_str   : constant unbounded_string := to_unbounded_string( "rationale" );
       screenshot_str  : constant unbounded_string := to_unbounded_string( "screenshot" );
       see_also_str    : constant unbounded_string := to_unbounded_string( "see_also" );
       summary_str     : constant unbounded_string := to_unbounded_string( "summary" );
       todos_str       : constant unbounded_string := to_unbounded_string( "todo" );
       version_str     : constant unbounded_string := to_unbounded_string( "version" );

  l : helpList.List;
       --authorId     : identifier := eof_t;

       function ParsePragmaKindAsHelp return unbounded_string is
          pragmaKind : unbounded_string;
       begin
          pragmaKind := identifiers( token ).name;
          discardUnusedIdentifier( token );
          getNextToken;
          return pragmaKind;
       end ParsePragmaKindAsHelp;

       procedure ParsePragmaStatementAsHelp( pragmaKind : unbounded_string ) is
         exprVal  : unbounded_string;
         exprType : identifier;
         annotationKind : unbounded_string;
       begin
         if pragmaKind = license_str then
            if token = symbol_t and identifiers( token ).value.all = "(" then
               getNextToken;
               info := identifiers( token ).name;
               discardUnusedIdentifier( token );
               getNextToken;
               if token = symbol_t and identifiers( token ).value.all = "," then
                  getNextToken;
                  info := info & ": " & identifiers( token ).value.all;
                  expect( strlit_t );
               end if;
               expect( symbol_t, ")" );
               if LicenseOutput then
                  put_line( to_string( info ) );
               end if;
            end if;
         elsif pragmaKind = todo_str then
            if token = symbol_t and identifiers( token ).value.all = "(" then
               todoTotal := todoTotal + 1;
               getNextToken;
               info := identifiers( token ).name; -- name
               getNextToken;
               expectPragmaComma;
               ParseStaticExpression( exprVal, exprType );
               info := info & "," & ToCSV( exprVal ); -- message
               expectPragmaComma;
               info := info & "," & identifiers( token ).name; -- measure
               measure := token;
               getNextToken;
               expectPragmaComma;
               info := info & "," & identifiers( token ).value.all; -- unit
               units := identifiers( token ).value.all;
               -- calculate work by measure
               if measure = teams_work_measure_unknown_t then
                  workMeasureCntUnknown := workMeasureCntUnknown + 1;
               elsif measure = teams_work_measure_hours_t then
                  workMeasureCntHours   := workMeasureCntHours + natural( to_numeric( units ) );
               elsif measure = teams_work_measure_fpoints_t then
                  workMeasureCntFpoints := workMeasureCntFpoints + natural( to_numeric( units ) );
               elsif measure = teams_work_measure_spoints_t then
                  workMeasureCntSpoints := workMeasureCntSpoints + natural( to_numeric( units ) );
               elsif measure = teams_work_measure_sloc_t then
                  workMeasureCntSloc    := workMeasureCntSloc + natural( to_numeric( units ) );
               elsif measure = teams_work_measure_size_t and units = "s" then
                  workMeasureCntSizeS   := workMeasureCntSizeS + 1;
               elsif measure = teams_work_measure_size_t and units = "m" then
                  workMeasureCntSizeM   := workMeasureCntSizeM + 1;
               elsif measure = teams_work_measure_size_t and units = "l" then
                  workMeasureCntSizeL   := workMeasureCntSizeL + 1;
               elsif measure = teams_work_measure_size_t and units = "xl" then
                  workMeasureCntSizeXL   := workMeasureCntSizeXL + 1;
               else
                  null; -- DEBUG
               end if;
               getNextToken;
               expectPragmaComma;
               info := info & "," & identifiers( token ).name; -- priority
               measure := token;
               getNextToken;
               expectPragmaComma;
               info := info & "," & identifiers( token ).value.all; -- unit
               units := identifiers( token ).value.all;
               if measure = teams_work_priority_unknown_t then
                  workPriorityCntUnknown := workPriorityCntUnknown + 1;
               elsif measure = teams_work_priority_level_t and units = "l" then
                  workPriorityCntLevelL := workPriorityCntLevelL + 1;
               elsif measure = teams_work_priority_level_t and units = "m" then
                  workPriorityCntLevelM := workPriorityCntLevelM + 1;
               elsif measure = teams_work_priority_level_t and units = "h" then
                  workPriorityCntLevelH := workPriorityCntLevelH + 1;
               elsif measure = teams_work_priority_severity_t and units = " 1" then
                  workPriorityCntSeverity1 := workPriorityCntSeverity1 + 1;
               elsif measure = teams_work_priority_severity_t and units = " 2" then
                  workPriorityCntSeverity2 := workPriorityCntSeverity2 + 1;
               elsif measure = teams_work_priority_severity_t and units = " 3" then
                  workPriorityCntSeverity3 := workPriorityCntSeverity3 + 1;
               elsif measure = teams_work_priority_severity_t and units = " 4" then
                  workPriorityCntSeverity4 := workPriorityCntSeverity4 + 1;
               elsif measure = teams_work_priority_severity_t and units = " 5" then
                  workPriorityCntSeverity5 := workPriorityCntSeverity5 + 1;
               elsif measure = teams_work_priority_risk_t then
                  workPriorityCntRisk := workPriorityCntRisk + natural( to_numeric( units ) );
               elsif measure = teams_work_priority_completed_t then
                  workPriorityCompleted := workPriorityCompleted + 1;
               elsif measure = teams_work_priority_cvss_t then
                  declare
                    u : long_float;
                  begin
                    u := to_numeric( units );
                    if u < 4.0 then
                      workPriorityCntCVSSMinor := workPriorityCntCVSSMinor + 1;
                    elsif u < 7.0 then
                      workPriorityCntCVSSMajor := workPriorityCntCVSSMajor + 1;
                    else
                      workPriorityCntCVSSCrit  := workPriorityCntCVSSCrit  + 1;
                    end if;
                  end;
               else
                  null; -- DEBUG
               end if;
               getNextToken;
               -- ticket is optional
               if token = symbol_t and identifiers( token ).value.all = "," then
                  getNextToken;
                  info := info & "," & ToCSV( identifiers( token ).value.all );
                  getNextToken;
               end if;
               expect( symbol_t, ")" );
               if TodoOutput then
                  put_line( to_string( info ) );
               end if;
            end if;
         elsif pragmaKind = advise_str or
               pragmaKind = blocked_str or
               pragmaKind = clarify_str or
               pragmaKind = dispute_str or
               pragmaKind = propose_str or
               pragmaKind = refactor_str then
            if token = symbol_t and identifiers( token ).value.all = "(" then
               getNextToken;
               info := pragmaKind & "," & identifiers( token ).name; -- name
               getNextToken;
               expectPragmaComma;
               info := info & "," & identifiers( token ).name; -- name
               getNextToken;
               expectPragmaComma;
               ParseStaticExpression( exprVal, exprType );
               info := info & "," & ToCSV( exprVal ); -- message
               expect( symbol_t, ")" );
               if CollabOutput then
                  put_line( to_string( info ) );
               end if;
            end if;
         elsif pragmaKind = annotate_str then
            if token = symbol_t and identifiers( token ).value.all = "(" then
               getNextToken;
               annotationKind := identifiers( token ).name;
               discardUnusedIdentifier( token );
               getNextToken;
               expectPragmaComma;
               ParseStaticExpression( exprVal, exprType );
               if annotationKind = accounts_str then
                  accounts( e, to_string(exprVal)  );
               elsif annotationKind = author_str then
                  author( e, to_string(exprVal)  );
                  -- handle a teams.member variable for an author
                  -- declarations don't happen so this doesn't work.
                  --if token /= strlit_t then
                  --   if identifiers( token ).class = VarClass then
                  --      if getBaseType( identifiers( token ).kind ) = teams_member_t then
                  --         ParseIdentifier( authorId );
                  --      end if;
                  --   end if;
                  --end if;
               elsif annotationKind = bugs_str then
                  bugs( e, to_string(exprVal)  );
               elsif annotationKind = created_str then
                  createdOn( e, to_string(exprVal)  );
               elsif annotationKind = category_str then
                  category( e, to_string(exprVal)  );
               elsif annotationKind = content_str then
                  content( e, to_string(exprVal)  );
               elsif annotationKind = description_str then
                  description( e, to_string(exprVal)  );
               elsif annotationKind = errors_str then
                  errors( e, to_string(exprVal)  );
               elsif annotationKind = examples_str then
                  examples( e, to_string(exprVal)  );
               elsif annotationKind = exceptions_str then
                  exceptions( e, to_string(exprVal)  );
               elsif annotationKind = footer_str then
                  footer( e, to_string(exprVal)  );
               elsif annotationKind = icon_str then
                  icon( e, to_string(exprVal)  );
               elsif annotationKind = modified_str then
                  modified( e, to_string(exprVal)  );
               elsif annotationKind = param_str then
                  params( e, to_string(exprVal)  );
               elsif annotationKind = return_str then
                  returns( e, to_string(exprVal)  );
               elsif annotationKind = rationale_str then
                  rationale( e, to_string(exprVal)  );
               elsif annotationKind = screenshot_str then
                  screenshot( e, to_string(exprVal)  );
               elsif annotationKind = see_also_str then
                  seeAlso( e, to_string(exprVal)  );
               elsif annotationKind = summary_str then
                  summary( e, to_string(exprVal)  );
               elsif annotationKind = todos_str then
                  todos( e, to_string(exprVal)  );
               elsif annotationKind = version_str then
                  releaseVersion( e, to_string(exprVal)  );
               else
                  info := null_unbounded_string;
               end if;
               expect( symbol_t, ")" ); -- getNextToken;
            end if;
         else
            -- Any other pragma skip to the ; or @ at the end
            loop
              exit when token = symbol_t and identifiers( token ).value.all = to_unbounded_string( ";" );
              exit when token = symbol_t and identifiers( token ).value.all = to_unbounded_string( "@" );
              exit when error_found or done or token = eof_t;
              getNextToken;
            end loop;
-- getNextToken;
         end if;
       end ParsePragmaStatementAsHelp;

       procedure ParsePragmaAsHelp is
         pragmaKind : unbounded_string;
       begin
         expect( pragma_t );
         if token = is_t then
            -- a pragma block
            expect( is_t );
            -- examine the name of the pragma and return a pragma kind matching the
            -- name
            pragmaKind := parsePragmaKindAsHelp;
            while token /= eof_t and token /= end_t loop
               -- an error check
               ParsePragmaStatementAsHelp( pragmaKind );
               if token = symbol_t and identifiers( symbol_t ).value.all = to_unbounded_string( "@" ) then
                  expect( symbol_t, "@" );
               elsif token = symbol_t and identifiers( symbol_t ).value.all = to_unbounded_string( ";" ) then
                  expect( symbol_t, ";" );
                  if token /= end_t then
                     pragmaKind := parsePragmaKindAsHelp;
                  end if;
               else
                  err( "'@' or ';' expected" );
               end if;
            end loop;
            expect( end_t );
            expect( pragma_t );
         else
            -- A single pragma
            pragmaKind := parsePragmaKindAsHelp;
            loop
               ParsePragmaStatementAsHelp( pragmaKind );
               exit when done or token = eof_t or (token = symbol_t and identifiers( symbol_t ).value.all /= to_unbounded_string( "@" ) );
               expect( symbol_t, "@" );
            end loop;
         end if;
       end ParsePragmaAsHelp;

     begin
       saveScript( scriptState );
       if isExecutingCommand or Syntax_Check then
          scriptFilePath := helpTopic;                     -- script name
<<retry1>> scriptFile := open( to_string( scriptFilePath ) & ASCII.NUL, 0, 660 ); -- open script
          if scriptFile < 1 then                           -- error?
             if C_errno = EINTR then
                goto retry1;
             end if;
<<retry2>>   scriptFile := open( to_string( scriptFilePath ) & ".sp" & ASCII.NUL, 0, 660 );
             if scriptFile > 0 then
                if C_errno = EINTR then
                   goto retry2;
                end if;
                scriptFilePath := scriptFilePath & ".sp"  ;
             end if;
          end if;
          if scriptFile < 1 then                           -- error?
<<retry3>>   scriptFile := open( to_string( scriptFilePath ) & ".bush" & ASCII.NUL, 0, 660 );
             if scriptFile > 0 then
                if C_errno = EINTR then
                   goto retry3;
                end if;
                scriptFilePath := scriptFilePath & ".bush"  ;
             end if;
          end if;
          if scriptFile > 0 then                           -- good?
             error_found := false;                         -- no error found
             exit_block := false;                          -- not exit-ing a block
             if not LineRead( firstLine'access ) then        -- read first line
                err( "help command is unable to read first line of script" );
             end if;
             if script = null then
                if verboseOpt then
                   Put_Trace( "Compiling Byte Code" );
                end if;
                compileScript( firstline );
            end if;
          else
           -- try manual entries
            Put_Line( "Not a SparForte command or script...trying manual entries" );
            delay 2.0;
            declare
              status : integer;
            begin
              status := linux_system( "man "& to_string( helpTopic ) & ASCII.NUL );
            end;
          end if;
       end if;
       if scriptFile > 0 then                      -- file open
          genDate := to_unbounded_string( integer'image( day( ada.calendar.clock ) ) );
          delete( genDate, 1, 1 );
          genDate := integer'image( month( ada.calendar.clock ) )& "-" & genDate;
          delete( genDate, 1, 1 );
          genDate := integer'image( ada.calendar.year( clock ) ) & "-" & genDate;
          delete( genDate, 1, 1 );
          if HTMLoutput then
             put_line( "<p><u><b>File</b>: " & to_string( scriptFilePath ) & "</u></p><p>" );
          elsif MANOutput then
             put_line( "./" & ASCII.Quotation & "man page " & to_string( scriptFilePath ) & ".9" );
             put_line( ".TH " & ASCII.Quotation & to_string( scriptFilePath ) & ASCII.Quotation &
                 " 9 " &
                 ASCII.Quotation & to_string( genDate ) & ASCII.Quotation & " " &
                 ASCII.Quotation & "Company" & ASCII.Quotation & " " &
                 ASCII.Quotation & "Manual" & ASCII.Quotation );
          elsif LicenseOutput then
             null;
          else
             null;
             --Put_Line( "Help for script " & bold( to_string( scriptFilePath ) ) & ":" );
             --New_Line;
          end if;
          -- lineno := 1;                             -- prepare to read it
          inputMode := fromScriptFile;             -- running a script
          error_found := false;                    -- no error found
          exit_block := false;                     -- not exit-ing a block
          cmdpos := firstScriptCommandOffset;
          token := identifiers'first;                -- dummy, replaced by g_n_t

         if HTMLoutput then
           r := new longHtmlHelpReport;
         elsif ManOutput then
           r := new longManPageHelpReport;
         else
           r := new longHelpReport;
         end if;
        start( r.all );
        startHelp( e, to_string( scriptFilePath ) );
        category( e, "SparForte script" );

          -- search the script for pragmas, interpreting the results
          -- as necessary for the help command

          while (not error_found and not done and token /= eof_t) loop
             getNextToken;                            -- load first token
             if token = pragma_t then
                ParsePragmaAsHelp;
             end if;
          end loop;

         endHelp(e );

-- TODO: generated
          --if HTMLoutput then
          --   put_line( "</p><p><i>Generated " & to_string( genDate ) & "</i><br></p>" );
          if TodoOutput then
            -- produce summary
             if todoTotal > 0 then
                new_line;
                put_line( "Amount of Work" );
                new_line;
                if workMeasureCntUnknown > 0 then
                   put_line( "Unknown:" & workMeasureCntUnknown'img );
                end if;
                if workMeasureCntHours > 0 then
                   put_line( "Hours:" & workMeasureCntHours'img );
                end if;
                if workMeasureCntFpoints > 0 then
                   put_line( "Function Points:" & workMeasureCntFpoints'img );
                end if;
                if workMeasureCntSpoints > 0 then
                   put_line( "Story Points:" & workMeasureCntSpoints'img );
                end if;
                if workMeasureCntSloc > 0 then
                   put_line( "Lines-of-Code:" & workMeasureCntSloc'img );
                end if;
                if workMeasureCntSizeS > 0 then
                   put_line( "Small:" & workMeasureCntSizeS'img );
                end if;
                if workMeasureCntSizeM > 0 then
                   put_line( "Medium:" & workMeasureCntSizeM'img );
                end if;
                if workMeasureCntSizeL > 0 then
                   put_line( "Large:" & workMeasureCntSizeL'img );
                end if;
                if workMeasureCntSizeXL > 0 then
                   put_line( "Extra Large:" & workMeasureCntSizeXL'img );
                end if;

                new_line;
                put_line( "Priorities of Work" );
                new_line;
                if workPriorityCntUnknown > 0 then
                   put_line( "Unknown:" & workPriorityCntUnknown'img );
                end if;
                if workPriorityCompleted > 0 then
                   put_line( "Completed:" & workPriorityCompleted'img );
                end if;
                if workPriorityCntLevelL > 0 then
                   put_line( "Low:" & workPriorityCntLevelL'img );
                end if;
                if workPriorityCntLevelM > 0 then
                   put_line( "Medium:" & workPriorityCntLevelM'img );
                end if;
                if workPriorityCntLevelH > 0 then
                   put_line( "High:" & workPriorityCntLevelH'img );
                end if;
                if workPriorityCntSeverity1 > 0 then
                   put_line( "Severity 1:" & workPriorityCntSeverity1'img );
                end if;
                if workPriorityCntSeverity2 > 0 then
                   put_line( "Severity 2:" & workPriorityCntSeverity2'img );
                end if;
                if workPriorityCntSeverity3 > 0 then
                   put_line( "Severity 3:" & workPriorityCntSeverity3'img );
                end if;
                if workPriorityCntSeverity4 > 0 then
                   put_line( "Severity 4:" & workPriorityCntSeverity4'img );
                end if;
                if workPriorityCntSeverity5 > 0 then
                   put_line( "Severity 5:" & workPriorityCntSeverity5'img );
                end if;
                if workPriorityCntRisk > 0 then
                   put_line( "Risk:" & workPriorityCntRisk'img );
                end if;
                if workPriorityCntCVSSMinor > 0 then
                   put_line( "CVSS Minor:" & workPriorityCntCVSSMinor'img );
                end if;
                if workPriorityCntCVSSMajor > 0 then
                   put_line( "CVSS Major:" & workPriorityCntCVSSMajor'img );
                end if;
                if workPriorityCntCVSSCrit > 0 then
                   put_line( "CVSS Critical:" & workPriorityCntCVSSCrit'img );
                end if;

                new_line;
                put_line( "Number of To-Do Items:" & todoTotal'img );
             else
                put_line( "No todo's found" );
             end if;
          elsif not LicenseOutput and not CollabOutput then
             if not isEmpty( e ) then
                -- TODO: this need to be fixed with a class interface or something
                if HTMLoutput then
                   render( longHtmlHelpReport( r.all ), e );
                elsif ManOutput then
                   render( longManPageHelpReport( r.all ), e );
                else
                   render( longHelpReport( r.all ), e );
                end if;
                finish( r.all );
             end if;
             free( r );
          end if;
        end if;
<<retryclose>> closeResult := close( scriptFile );
        if closeResult < 0 then
           if C_errno = EINTR then
              goto retryclose;
           end if;
        end if;
        restoreScript( scriptState );
        discardUnusedIdentifier( token );
     end DoScriptHelp;

-----------------------------------------------------------------------------
--   HELP (POSIX SHELL COMMAND)
-- Syntax: help = help [ ident ]
-- Source: SparForte built-in
-----------------------------------------------------------------------------
-- Help is broken up into sub-procedures due to its large size.

procedure helpMain is
  e : aHelpEntry;
  r : aRootReportPtr;
  l : helpList.List;
begin
   --r := new longHelpReport;
   if HTMLoutput then
     r := new longHtmlHelpReport;
   elsif ManOutput then
     r := new longManPageHelpReport;
   else
     r := new longHelpReport;
   end if;

   start( r.all );
   startHelp( e, "help" );
   summary( e, "SparForte command prompt help" );
   category( e, "reference" );
   description( e,
     "The help command gives short advice on keywords, packages, " &
     "scripts, operating system commands and other topics.  Enter 'help' and " &
     "a topic to get advice.  For example, 'help arrays' briefly explains " &
     "the arrays package. These are the internal topics:" );
   content( e, "affirm" );
   content( e, "arrays" );
   content( e, "calendar" );
   content( e, "case" );
   content( e, "cd" );
   content( e, "cgi" );
   content( e, "chains" );
   content( e, "clear" );
   content( e, "close" );
   content( e, "command_line" );
   content( e, "configure" );
   content( e, "create" );
   content( e, "db" );
   content( e, "dbm" );
   content( e, "declare" );
   content( e, "delay" );
   content( e, "delete" );
   content( e, "directory_operations" );
   content( e, "doc_format" );
   content( e, "doubly_linked_lists" );
   content( e, "dynamic_hash_tables" );
   content( e, "end_of_file" );
   content( e, "end_of_line" );
   content( e, "btree_io" );
   content( e, "enums" );
   content( e, "env" );
   content( e, "exceptions" );
   content( e, "exit" );
   content( e, "expressions" );
   content( e, "files" );
   content( e, "for" );
   content( e, "function" );
   content( e, "get" );
   content( e, "get_immediate" );
   content( e, "get_line" );
   content( e, "glossary" );
   content( e, "gnat.cgi" );
   content( e, "gnat.crc32" );
   content( e, "hash_io" );
   content( e, "hashed_maps" );
   content( e, "help" );
   content( e, "history" );
   content( e, "if" );
   content( e, "inkey" );
   content( e, "is_open" );
   content( e, "jobs" );
   content( e, "keys" );
   content( e, "l10n" );
   content( e, "line" );
   content( e, "logs" );
   content( e, "loop" );
   content( e, "memcache" );
   content( e, "mode" );
   content( e, "mysql" );
   content( e, "mysqlm" );
   content( e, "name" );
   content( e, "new_line" );
   content( e, "null" );
   content( e, "numerics" );
   content( e, "open" );
   content( e, "os" );
   content( e, "pen" );
   content( e, "pen.gl" );
   content( e, "policy" );
   content( e, "pragma" );
   content( e, "procedure" );
   content( e, "put" );
   content( e, "put_line" );
   content( e, "raise" );
   content( e, "reset" );
   content( e, "records" );
   content( e, "return" );
   content( e, "set_input" );
   content( e, "shell" );
   content( e, "skip_line" );
   content( e, "sound" );
   content( e, "source_info" );
   content( e, "stats" );
   content( e, "step" );
   content( e, "strings" );
   content( e, "strings.match" );
   content( e, "strings.perl_match" );
   content( e, "subtype" );
   content( e, "System" );
   content( e, "teams" );
   content( e, "templates" );
   content( e, "trace" );
   content( e, "type" );
   content( e, "types" );
   content( e, "typeset" );
   content( e, "units" );
   content( e, "umask" );
   content( e, "unset" );
   content( e, "variables" );
   content( e, "vectors" );
   content( e, "wait" );
   content( e, "while" );
   content( e, "with" );
   content( e, ASCII.Quotation & "?" & ASCII.Quotation );
   footer( e, "For full details, see the SparForte documentation.  To leave " &
     "SparForte, enter 'return' (or 'logout' if this is your login session)." );
   endHelp( e );

  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
   finish( r.all );
end helpMain;

procedure helpBtreeIO is
  e : aHelpEntry;
  r : aRootReportPtr;
begin
     startHelp( e, "btree_io" );
     summary( e, "btree_io package" );
     authorKen( e );
     categoryPackage( e );
     createdOn( e, "v2.0");
     authorKen( e );
     description( e, "A collection of common routines using Berkeley DB B-tree files." );
     content( e, "add( f, k, v )","get_previous( f, c, k, v )" );
     content( e, "append( f, k, v )","b := has_element( f, k )" );
     content( e, "clear( f )","increment( f, k [,n] )" );
     content( e, "close( f )","b := is_open( f )" );
     content( e, "close_cursor( f, c )","e := last_error( f )" );
     content( e, "create( f, p, kl, vl )","open( f, p, kl, vl )" );
     content( e, "decrement( f, k [,n] )","open_cursor( f, c )" );
     content( e, "delete( f )","prepend( f, k, v )" );
     content( e, "flush( f )","raise_exceptions( f, b )" );
     content( e, "get( f, k, v )","remove( f, k )" );
     content( e, "get_first( f, c, k, v )","replace( f, k, v )" );
     content( e, "get_last( f, c, k, v )","set( f, k, v )" );
     content( e, "get_next( f, c, k, v )","truncate( f )" );
     content( e, "recover( f, p )", "b := will_raise( f )" );
     seeAlso( e, "doc/pkg_btree_io.html" );
     endHelp( e );
   if HTMLoutput then
     r := new longHtmlHelpReport;
   elsif ManOutput then
     r := new longManPageHelpReport;
   else
     r := new longHelpReport;
   end if;
   start( r.all );
  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
   finish( r.all );
end helpBtreeIO;

procedure helpDb is
  e : aHelpEntry;
  r : aRootReportPtr;
begin
     startHelp( e, "db" );
     summary( e, "db (PostgreSQL) package" );
     authorKen( e );
     categoryPackage( e );
     authorKen( e );
     description( e, "A collection of common routines using a PostgreSQL database.  " &
        "Based on the APQ project.  See the dbm package for multiple connections." );
     content( e, "db.connect( d [, u, w ][, h][, p] )" );
     content( e, "append( s [,a] )","prepare( s [, a] )" );
     content( e, "append_line( s )","append_quoted( s )" );
     content( e, "execute","execute_checked( [ s ] )" );
     content( e, "disconnect","b := is_connected" );
     content( e, "reset","s := error_message" );
     content( e, "s := notice_message","s := in_abort_state" );
     content( e, "s := options","set_rollback_on_finalize( b )" );
     content( e, "open_db_trace( f [,m] )","b := will_rollback_on_finalize" );
     content( e, "close_db_trace","set_trace( b )" );
     content( e, "b := is_trace","clear" );
     content( e, "raise_exceptions( [ b ] )","report_errors( b )" );
     content( e, "begin_work","rollback_work" );
     content( e, "commit_work","rewind" );
     content( e, "fetch [ (i) ]","b := end_of_query" );
     content( e, "t := tuple","n := tuples" );
     content( e, "n := columns","s := column_name( c )" );
     content( e, "i := column_index( s )","b := is_null( c )" );
     content( e, "s := value( c )","d := engine_of" );
     content( e, "show","list" );
     content( e, "schema( t )","users" );
     content( e, "databases" );
     seeAlso( e, "doc/pkg_db.html" );
     endHelp( e );
   if HTMLoutput then
     r := new longHtmlHelpReport;
   elsif ManOutput then
     r := new longManPageHelpReport;
   else
     r := new longHelpReport;
   end if;
   start( r.all );
  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
   finish( r.all );
end helpDb;

procedure helpDbm is
  e : aHelpEntry;
  r : aRootReportPtr;
begin
     startHelp( e, "dbm" );
     summary( e, "dbm (PostgreSQL) package" );
     authorKen( e );
     createdOn( e, "v2.0");
     categoryPackage( e );
     authorKen( e );
     description( e, "A collection of common routines using PostgreSQL databases.  " &
         "Based on the APQ project.  Supports multiple connections." );
     content( e, "connect( c, d [, u, w ][, h][, p] )" );
     content( e, "append( q, s [, a] )","prepare( q, s [, a] )" );
     content( e, "append_line( q, s )","append_quoted( q, c, s )" );
     content( e, "execute( q, c )","execute_checked( q, c [, s ] )" );
     content( e, "disconnect( c )","is_connected( c )" );
     content( e, "reset( c )","s := error_message( c )" );
     content( e, "databases( c )","b := in_abort_state( c )" );
     content( e, "s := options( c )","set_rollback_on_finalize( c, b )" );
     content( e, "open_db_trace( c, f [, m] )","b := will_rollback_on_finalize( c )" );
     content( e, "close_db_trace( c )","set_trace( c, b )" );
     content( e, "b := is_trace( c )","clear( q )" );
     content( e, "raise_exceptions( q [, b ] )","report_errors( q, b )" );
     content( e, "begin_work( q, c )","rollback_work( q, c )" );
     content( e, "commit_work( q, c )","rewind( q )" );
     content( e, "fetch( q [, i] )","b := end_of_query( q )" );
     content( e, "n := tuple( q, t )","n := tuples( q )" );
     content( e, "n := columns( q )","s := column_name( q, i )" );
     content( e, "i := column_index( q, s )","b := is_null( q, i )" );
     content( e, "s := value( q, i )","b := engine_of( c )" );
     content( e, "show( q, c )","list( c )" );
     content( e, "schema( c, t )","users( c )" );
     content( e, "new_connection( c )","new_query( q )" );
     content( e, "fetch_values( q, c, r )","s := notice_message( c )" );
     content( e, "append_for_insert( q, c, r )","append_for_update( q, c, r )" );
     seeAlso( e, "doc/pkg_dbm.html" );
     endHelp( e );
   if HTMLoutput then
     r := new longHtmlHelpReport;
   elsif ManOutput then
     r := new longManPageHelpReport;
   else
     r := new longHelpReport;
   end if;
   start( r.all );
  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
   finish( r.all );
end helpDbm;

procedure helpHashIO is
  e : aHelpEntry;
  r : aRootReportPtr;
begin
     startHelp( e, "hash_io" );
     summary( e, "hash_io package" );
     authorKen( e );
     categoryPackage( e );
     createdOn( e, "v2.0");
     authorKen( e );
     description( e, "A collection of common routines using Berkeley DB Hash files." );
     content( e, "add( f, k, v )","get_previous( f, c, k, v )" );
     content( e, "append( f, k, v )","b := has_element( f, k )" );
     content( e, "clear( f )","increment( f, k [,n] )" );
     content( e, "close( f )","b := is_open( f )" );
     content( e, "close_cursor( f, c )","e := last_error( f )" );
     content( e, "create( f, p, kl, vl )","open( f, p, kl, vl )" );
     content( e, "decrement( f, k [,n] )","open_cursor( f, c )" );
     content( e, "delete( f )","prepend( f, k, v )" );
     content( e, "flush( f )","raise_exceptions( f, b )" );
     content( e, "get( f, k, v )","remove( f, k )" );
     content( e, "get_first( f, c, k, v )","replace( f, k, v )" );
     content( e, "get_last( f, c, k, v )","set( f, k, v )" );
     content( e, "get_next( f, c, k, v )","truncate( f )" );
     content( e, "recover( f, p )", "b := will_raise( f )" );
     seeAlso( e, "doc/pkg_hash_io.html" );
     endHelp( e );
   if HTMLoutput then
     r := new longHtmlHelpReport;
   elsif ManOutput then
     r := new longManPageHelpReport;
   else
     r := new longHelpReport;
   end if;
   start( r.all );
  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
   finish( r.all );
end helpHashIO;

procedure helpMySQL is
  e : aHelpEntry;
  r : aRootReportPtr;
begin
     startHelp( e, "mysql" );
     summary( e, "mysql package" );
     authorKen( e );
     categoryPackage( e );
     authorKen( e );
     description( e, "A collection of common routines using a MySQL database.  " &
        "Based on the APQ project.  See the mysqlm package for multiple connections." );
     content( e, "connect( d [, u, w ][, h][, p] )" );
     content( e, "append( s [, a] )","prepare( s [, a] )" );
     content( e, "append_line( s )","append_quoted( s )" );
     content( e, "execute","execute_checked( [ s ] )" );
     content( e, "disconnect","is_connected" );
     content( e, "reset","s := error_message" );
     content( e, "databases","b := in_abort_state" );
     content( e, "s := options","set_rollback_on_finalize( b )" );
     content( e, "open_db_trace( f [,m] )","b := will_rollback_on_finalize" );
     content( e, "close_db_trace","set_trace( b )" );
     content( e, "b := is_trace","clear" );
     content( e, "raise_exceptions( [ b ] )","report_errors( b )" );
     content( e, "begin_work","rollback_work" );
     content( e, "commit_work","rewind" );
     content( e, "fetch [ (i) ]","b := end_of_query" );
     content( e, "n := tuple( t )","n := tuples" );
     content( e, "n := columns","s := column_name( c )" );
     content( e, "i := column_index( s )","b := is_null( c )" );
     content( e, "s := value( c )","b := engine_of " );
     content( e, "show","list" );
     content( e, "schema( t )","users" );
     seeAlso( e, "doc/pkg_mysql.html" );
     endHelp( e );
   if HTMLoutput then
     r := new longHtmlHelpReport;
   elsif ManOutput then
     r := new longManPageHelpReport;
   else
     r := new longHelpReport;
   end if;
   start( r.all );
  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
   finish( r.all );
end helpMySQL;

procedure helpMySQLM is
  e : aHelpEntry;
  r : aRootReportPtr;
begin
     startHelp( e, "mysqlm" );
     summary( e, "mysqlm package" );
     authorKen( e );
     createdOn( e, "v2.0");
     categoryPackage( e );
     authorKen( e );
     description( e, "A collection of common routines using MySQL databases.  " &
        "Based on the APQ project.  Supports multiple connections." );
     content( e, "connect( c, d [, u, w ][, h][, p] )" );
     content( e, "append( q, s [, a] )","prepare( q, s [, a] )" );
     content( e, "append_line( q, s )","append_quoted( q, c, s )" );
     content( e, "execute( q, c )","execute_checked( q, c [, s ] )" );
     content( e, "disconnect( c )","is_connected( c )" );
     content( e, "reset( c )","s := error_message( c )" );
     content( e, "databases( c )","b := in_abort_state( c )" );
     content( e, "s := options( c )","set_rollback_on_finalize( c, b )" );
     content( e, "open_db_trace( c, f [, m] )","b := will_rollback_on_finalize( c )" );
     content( e, "close_db_trace( c )","set_trace( c, b )" );
     content( e, "b := is_trace( c )","clear( q )" );
     content( e, "raise_exceptions( q [, b ] )","report_errors( q, b )" );
     content( e, "begin_work( q, c )","rollback_work( q, c )" );
     content( e, "commit_work( q, c )","rewind( q )" );
     content( e, "fetch( q [, i] )","b := end_of_query( q )" );
     content( e, "n := tuple( q, t )","n := tuples( q )" );
     content( e, "n := columns( q )","s := column_name( q, i )" );
     content( e, "i := column_index( q, s )","b := is_null( q, i )" );
     content( e, "s := value( q, i )","b := engine_of( c ) " );
     content( e, "show( q, c )","list( c )" );
     content( e, "schema( c, t )","users( c )" );
     content( e, "new_connection( c )","new_query( q )" );
     content( e, "fetch_values( q, c, r )" );
     content( e, "append_for_insert( q, c, r )","append_for_update( q, c, r )" );
     seeAlso( e, "doc/pkg_mysqlm.html" );
     endHelp( e );
   if HTMLoutput then
     r := new longHtmlHelpReport;
   elsif ManOutput then
     r := new longManPageHelpReport;
   else
     r := new longHelpReport;
   end if;
   start( r.all );
  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
   finish( r.all );
end helpMySQLM;

procedure helpUnits is
  e : aHelpEntry;
  r : aRootReportPtr;
begin
     startHelp( e, "units" );
     summary( e, "units package" );
     authorKen( e );
     categoryPackage( e );
     authorKen( e );
     description( e, "A collection of common routines for conversion of units of measurement." );
     content( e, "r := acres2hectares( f )","r := bytes2mb( f )","r := c2f( f )" );
     content( e, "r := c2k( f )","r := cm2inches( f )","r := cucm2floz( f )" );
     content( e, "r := cucm2usfloz( f )","r := f2c( f )","r := feet2cm( f )" );
     content( e, "r := floz2usfloz( f )","r := floz2cucm( f )","r := floz2ml( f )" );
     content( e, "r := g2troz( f )","r := gal2l( f )","r := grams2oz( f )" );
     content( e, "r := hectares2acres( f )","r := inches2mm( f )","r := k2c( f )" );
     content( e, "r := kg2lb( f )","r := km2miles( f )","r := l2gal( f )" );
     content( e, "r := l2quarts( f )","r := l2usdryoz( f )","r := l2usliqoz( f )" );
     content( e, "r := lb2kg( f )","r := ly2pc( f )","r := m2yards( f )" );
     content( e, "r := mb2bytes( f )","r := miles2km( f )","r := ml2floz( f )" );
     content( e, "r := ml2usfloz( f )","r := mm2inches( f )","r := pc2ly( f )" );
     content( e, "r := pints2l( f )","r := oz2grams( f )","r := sqcm2sqin( f )" );
     content( e, "r := sqft2sqm( f )","r := sqin2sqcm( f )","r := sqkm2sqmiles( f )" );
     content( e, "r := sqm2sqft( f )","r := sqm2sqyd( f )","r := sqmiles2sqkm( f )" );
     content( e, "r := sqyd2sqm( f )","r := tonnes2tons( f )","r := tons2tonnes( f )" );
     content( e, "r := troz2g( f )","r := usdryoz2l( f )","r := usliqoz2l( f )" );
     content( e, "r := usfloz2cucm( f )","r := usfloz2floz( f )","r := usfloz2ml( f )" );
     content( e, "r := yards2m( f )" );
     seeAlso( e, "doc/pkg_units.html" );
     endHelp( e );
   if HTMLoutput then
     r := new longHtmlHelpReport;
   elsif ManOutput then
     r := new longManPageHelpReport;
   else
     r := new longHelpReport;
   end if;
   start( r.all );
  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
   finish( r.all );
end helpUnits;

procedure helpNumerics is
  e : aHelpEntry;
  r : aRootReportPtr;
begin
     startHelp( e, "numerics" );
     summary( e, "numerics package" );
     authorKen( e );
     categoryPackage( e );
     authorKen( e );
     description( e, "A collection of constants and common routines using numbers and mathematics." );
     section( e, "Constants" );
     content( e, "e","log2_e","log10_e","ln10" );
     content( e, "ln2","pi","pi_by_2","pi_by_4" );
     content( e, "pi_under_1","pi_under_2","sqrt_pi_under_2","sqrt_2" );
     content( e, "sqrt_2_under_1" );
     section( e, "General" );
     content( e, "f := abs( e )","f := ceiling( e )","f := copy_sign( x, y )" );
     content( e, "f := even( i )","f := exp( x )","f := exponent( e )" );
     content( e, "f := floor( e )","f := fnv_hash_of( s, l )","f := fraction( e )" );
     content( e, "f := hash_of( s, l )","f := leading_part( x, y )","f := log( e [,b] )" );
     content( e, "f := machine( e )","f := max( x, y )","r := md5( s )" );
     content( e, "f := min( x, y )","f := odd( x )","f := murmur_hash_of( s, l )" );
     content( e, "p := pos( c )","f := random","f := remainder( x, y )" );
     content( e, "r := rnd( p )","i := rotate_left( e, b )","i := rotate_right( e, b )" );
     content( e, "f := rounding( e )","f := scaling( x, y )","f := sdbm_hash_of( s, l )" );
     content( e, "f := serial","d := sha1_digest_of( s )","d := sha224_digest_of( s )" );
     content( e, "d := sha256_digest_of( s )","d := sha512_digest_of( s )","i := shift_left( e, b )" );
     content( e, "i := shift_right( e, b )","i := shift_right_arithmetic( x, b )" );
     content( e, "f := sqrt( e )","f := sturges( l, h, t )","f := truncation( e )" );
     content( e, "f := unbiased_rounding( e )","f := value( s )" );
     section( e, "Trigonometry" );
     content( e, "f := arccos( x [,cycle] )","f := arccosh( e )","f := arccot( x, y [,cycle] )" );
     content( e, "f := arccoth( e )","f := arcsin( e [,cycle] )","f := arcsinh( e )" );
     content( e, "f := arctan( x, y [,cycle] )","f := arctanh( e )","f := cos( e [,cycle] )" );
     content( e, "f := cosh( e )","f := cot( e [,cycle] )","f := coth( e )" );
     content( e, "f := sin( e [,cycle] )","f := sinh( e )","f := tan( e [,cycle] )" );
     content( e, "f := tanh( e )" );
     section( e, "Complex Numbers" );
     content( e, "re( complex )","im( complex )","modulus( complex )" );
--      content( e, "set_re( complex, r )","set_im( complex, i )","argument( complex )" );
     footer( e, "abs( x ) is not 'numerics.abs( x )' because it's a language construct" );
     seeAlso( e, "doc/pkg_numerics.html" );
     endHelp( e );
   if HTMLoutput then
     r := new longHtmlHelpReport;
   elsif ManOutput then
     r := new longManPageHelpReport;
   else
     r := new longHelpReport;
   end if;
   start( r.all );
  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
   finish( r.all );
end helpNumerics;

procedure helpPragma is
  e : aHelpEntry;
  r : aRootReportPtr;
begin
     startHelp( e, "pragma" );
     summary( e, "pragma name [( params...)]" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "Pragmas (or interpreter directives) provide SparForte with advice " &
                  "for running programs.  They can also " &
                  "supply additional information for people not covered by " &
                  "the language itself.  Pragmas are used for " &
                  "design and architecture, documentation, project management, team " &
                  "communication, debugging, configuration and other things.");
  content( e, "pragma ada_95 - enforce Ada 95 restrictions" );
  content( e, "pragma advise( from, to, message ) - request team advice/assistance" );
  content( e, "pragma affinity( affinity [, weight] ) - enforce a design affinity" );
  content( e, "pragma annotate( [type,] " & Ada.Characters.Latin_1.Quotation & "text" & Ada.Characters.Latin_1.Quotation & " ) - embed a comment for help command" );
  content( e, "pragma assert( condition ) - with --debug/--test, terminate program on condition fail" );
  content( e, "pragma assumption( applied, type ) - assume a type or subtype was used to make a variable" );
  content( e, "pragma assumption( factor, var ) - assume a variable was used in an expression" );
  content( e, "pragma assumption( used, var ) - assume a variable was read" );
  content( e, "pragma assumption( written, var ) - assume a variable was written" );
  content( e, "pragma blocked( from, message ) - announce programmer progress blocked" );
  content( e, "pragma clarify( from, to, message ) - request programmer clarification" );
  content( e, "pragma colour_messages | color_messages - same as --colour/--color" );
  content( e, "pragma constraint( constraint, value [, weight] ) - enforce a design constraint" );
  content( e, "pragma debug( `commands` ) - with --debug, execute debug commands" );
  content( e, "pragma declare_affinity( mode, affinity [, limit] ) - create a design affinity" );
  content( e, "pragma declare_constraint( mode, constraint, value [,limit] ) - create a design constraint" );
  content( e, "pragma depreciated/deprecated( " & Ada.Characters.Latin_1.Quotation & "newscript" & Ada.Characters.Latin_1.Quotation &" ) - report script as obsolete by newscript" );
  content( e, "pragma dispute( from, to, message ) - request program review" );
  content( e, "pragma export( shell | local_memcache | memcache | session , var ) - export a variable" );
  content( e, "pragma export_json( shell | local_memcache | memcache | session , var )" );
  content( e, "pragma gcc_errors - same as --gcc-errors" );
  content( e, "pragma import( shell | cgi | local_memcache | memcache | session, var ) - import a var" );
  content( e, "pragma import_json( shell | cgi | local_memcache | memcache | session, var )" );
  content( e, "pragma inspect( var ) - perform 'env var' on --break breakout" );
  content( e, "pragma inspection_peek - like a inspection_point but no breakout" );
  content( e, "pragma inspection_point - break to command prompt if --break is used" );
  content( e, "pragma license( license_name [, extra] ) - specify a software license" );
  content( e, "pragma manual_test( owner, name, objective, description, category, preconditions, steps, postconditions, units, work estimate, units, work priority, ticket) - manual test case" );
  content( e, "pragma manual_test_result( tester, date, notes, screenshots, status [, ticket] - manual test result" );
  content( e, "pragma no_command_hash - do not store command pathnames in the hash table" );
  content( e, "pragma prompt_script( `commands` ) - commands to draw command prompt" );
  content( e, "pragma propose( from, to, message ) - suggest a change to a program" );
  content( e, "pragma refactor( from, to, message ) - request programmer optimize program" );
  content( e, "pragma restriction( no_annotate_todos ) - must not have annotate/todo" );
  content( e, "pragma restriction( annotations_not_optional ) - must have pragma annotate" );
  content( e, "pragma restriction( no_auto_declarations ) - no auto command line declarations" );
  content( e, "pragma restriction( no_declarations_in_executable_statements) - declarations must be in declaration part of blocks" );
  content( e, "pragma restriction( no_external_commands ) - disable operating system commands" );
  content( e, "pragma restriction( no_memcache ) - disable connections to memcache" );
  content( e, "pragma restriction( no_mysql_database ) - disable connections to mysql" );
  content( e, "pragma restriction( no_postgresql_database ) - disable connections to postgresql" );
  content( e, "pragma restriction( no_unused_identifiers ) - stricter unused tests" );
  content( e, "pragma restriction( no_volatiles ) - volatile identifiers not allowed" );
  content( e, "pragma session_export_script( `commands` ) - commands to export session variables" );
  content( e, "pragma session_import_script( `commands` ) - commands to import session variables" );
  content( e, "pragma software_model( model_name ) - specify the category of the program" );
  content( e, "pragma suppress( word_quoting ) - allow shell 'barewords'" );
  content( e, "pragma suppress( all_priority_todos_for_release ) - all todo's allowed late in SDLC" );
  content( e, "pragma suppress( low_priority_todos_for_release ) - low priority todo's allowed late in SDLC" );
  content( e, "pragma suppress( suppress_no_empty_command_substitutions ) - allow empty strings in `..` and $(..)" );
  content( e, "pragma template( css|html|js|json|text|wml|xml [, path] ) - script is acting as a template processor" );
  content( e, "pragma test( condition [, " & ASCII.Quotation & "description" & ASCII.Quotation & "] ) - with --test, execute test commands" );
  content( e, "pragma test_report( text|xml [, " & ASCII.Quotation & "filepath" & ASCII.Quotation & "] ) - type and location of test report" );
  content( e, "pragma test_result( condition ) - with --test, display warning on condition failure" );
  content( e, "pragma todo( to, message, work, units, priority, units ) - task assignment/estimation" );
  content( e, "pragma unchecked_import( shell | cgi | local_memcache | memcache | session, var ) - import without checking for existence" );
  content( e, "pragma unchecked_volatile( var [,ttl] ) - like volatile, but not an error to use in expressions" );
  content( e, "pragma uninspect( var ) - undo pragma inspect" );
  content( e, "pragma unrestricted_template( css|html|js|json|text|wml|xml [, path] ) - run template without a restricted shell" );
  content( e, "pragma volatile( var [,ttl] ) - load value from environment on every access" );
     seeAlso( e, "doc/ref_pragmas.html" );
     endHelp( e );
   if HTMLoutput then
     r := new longHtmlHelpReport;
   elsif ManOutput then
     r := new longManPageHelpReport;
   else
     r := new longHelpReport;
   end if;
   start( r.all );
  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
   finish( r.all );
end helpPragma;

procedure help( ap : argumentListPtr ) is
  helpTopic : unbounded_string;
  e : aHelpEntry;
  r : aRootReportPtr;
begin
  --r := new longHelpReport;
  -- TODO: an enum type
  HTMLOutput    := false;
  MANOutput     := false;
  LicenseOutput := false;
  TodoOutput    := false;
  CollabOutput  := false;
  if ap'length = 0 then
     helpTopic := null_unbounded_string;
  else
     helpTopic := to_unbounded_string( ap( 1 ).all );
     delete( helpTopic, length( helpTopic ), length( helpTopic ) );
     if helpTopic = "-h" then      -- script help (html)
        HTMLOutput := true;
        if ap'length = 1 then
           helpTopic := null_unbounded_string;
        else
           helpTopic := to_unbounded_string( ap( 2 ).all );
           delete( helpTopic, length( helpTopic ), length( helpTopic ) );
        end if;
     elsif helpTopic = "-m" then      -- script help (man page)
        MANOutput := true;
        if ap'length = 1 then
           helpTopic := null_unbounded_string;
        else
           helpTopic := to_unbounded_string( ap( 2 ).all );
           delete( helpTopic, length( helpTopic ), length( helpTopic ) );
        end if;
     elsif helpTopic = "-l" then      -- script licenses
        LicenseOutput := true;
        if ap'length = 1 then
           helpTopic := null_unbounded_string;
        else
           helpTopic := to_unbounded_string( ap( 2 ).all );
           delete( helpTopic, length( helpTopic ), length( helpTopic ) );
        end if;
     elsif helpTopic = "-t" then      -- to-do's
        TodoOutput := true;
        if ap'length = 1 then
           helpTopic := null_unbounded_string;
        else
           helpTopic := to_unbounded_string( ap( 2 ).all );
           delete( helpTopic, length( helpTopic ), length( helpTopic ) );
        end if;
     elsif helpTopic = "-c" then       -- collaboration
        CollabOutput := true;
        if ap'length = 1 then
           helpTopic := null_unbounded_string;
        else
           helpTopic := to_unbounded_string( ap( 2 ).all );
           delete( helpTopic, length( helpTopic ), length( helpTopic ) );
        end if;
     end if;
  end if;

  -- The type of report depends on the switch chosen

  if HTMLoutput then
    r := new longHtmlHelpReport;
  elsif ManOutput then
    r := new longManPageHelpReport;
  else
    r := new longHelpReport;
  end if;

  if length( helpTopic ) = 0 then
     helpMain;
     return;
  end if;
  if helpTopic = "affirm" then
     startHelp( e, "affirm" );
     summary( e, "affirm clause" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "An affirm is block attached to a type (or a subtype)" &
       " that will test or alter a value being typecast to the type." &
       "  It is sometimes called a 'contract'.  Within the block, the type" &
       " name refers to the value being typecast.  If the parent of the type" &
       " has an affirm clause, it will also run.  If the value is altered," &
       " recursion will occur." );
     examples( e, "affirm / raise validation_error when customer_id not in 100000..999999; / end affirm;" );
     examples( e, "affirm / if mod_256 not in 0..255 then / mod_256 := @ mod 256; / end if; / end affirm;" );
     content( e, ( "affirm ... end affirm;" ) );
     seeAlso( e, "doc/pkg_typedecl.html" );
     endHelp( e );
  elsif helpTopic = "arrays" then
     startHelp( e, "arrays" );
     summary( e, "arrays package" );
     authorKen( e );
     categoryPackage( e );
     authorKen( e );
     description( e, "A collection of common routines using arrays." );
     content( e, "first( a )","last( a )","length( a )");
     content( e, "bubble_sort( a )","bubble_sort_descending( a )" );
     content( e, "heap_sort( a )","heap_sort_descending( a )" );
     content( e, "shuffle( a )","flip( a )" );
     content( e, "shift_left( a )","shift_right( a )" );
     content( e, "rotate_left( a )","rotate_right( a )" );
     content( e, "to_array( a, s )","to_json( s, a )" );
     seeAlso( e, "doc/pkg_arrays.html" );
     endHelp( e );
  elsif helpTopic = "btree_io" then
     helpBTreeIO;
  elsif helpTopic = "calendar" then
     discardUnusedIdentifier( token );
     startHelp( e, "calendar" );
     summary( e, "calendar package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using dates and times." );
     content( e, "t := clock","y := year( t )","m := month( t )" );
     content( e, "d := day( t )","s := seconds( t )","split( t, y, m, d, s )" );
     content( e, "t := time_of( y,m,d,s )","i := day_of_week( t )","t := to_time( j )" );
     seeAlso( e, "doc/pkg_calendar.html" );
     endHelp( e );
  elsif helpTopic = "case" then
     startHelp( e, "case" );
     summary( e, "case var [, var ...] is...end case" );
     authorKen( e );
     categoryKeyword( e );
     description( e,
        "Test multiple constants or variables, executing the commands for " &
        "the condition that matches.  If there are multiple variables, " &
        "each case in the list must match.   If no conditions match, the " &
        "others case will run." );
     content( e, "case var [, var ...] is when literal|const[|...][, ...] => ...when others => ...end case" );
     content( e, "case in var [, var ...] out var [,var...] is when literal|const|<>[|...][, ...] => ...when others => ...end case" );
     seeAlsoFlowControl( e );
     endHelp( e );
  elsif helpTopic = "cd" then
     startHelp( e, "cd" );
     summary( e, "change directory" );
     authorKen( e );
     categoryBuiltin( e );
     description( e,
      "Change the working directory.  Supports AdaScript parameters. " &
      "One parameter is required.  An empty path will change to the user's " &
      "home directory.  A minus sign will revert to the previous directory." );
     content( e, "cd -|" & ASCII.Quotation & ASCII.Quotation & "|dirname" );
     params( e, "path - the path to change to (or null string, minus)" );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "cgi" then
     discardUnusedIdentifier( token );
     startHelp( e, "cgi" );
     summary( e, "cgi package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines for web programming and cookies.  " &
         "Based on the ADACGI project.  See also gnat.cgi.");
     content( e, "parsing_errors","input_received","is_index" );
     content( e, "cgi_method","value( k, i, b )","key_exists( k, i )" );
     content( e, "key_count( k )","argument_count","key( p )" );
     content( e, "key_value_exists( k, v )","put_cgi_header( s )","put_html_head( t, m )" );
     content( e, "put_html_heading( s, p )","put_html_tail","put_error_message( s )" );
     content( e, "my_url","put_variables","line_count" );
     content( e, "line_count_of_value","line( v )","value_of_line( k, p )" );
     content( e, "url_decode","url_encode( s )","html_encode( s )" );
     content( e, "set_cookie( k,v,e,p,d,s )","cookie_value( p )","cookie_count" );
     content( e, "s := key_value( p )" );
     seeAlso( e, "doc/pkg_cgi.html" );
     endHelp( e );
  elsif helpTopic = "chains" then
     discardUnusedIdentifier( token );
     startHelp( e, "chains" );
     summary( e, "chain package" );
     authorKen( e );
     createdOn( e, "v1.5");
     categoryPackage( e );
     description( e, "A collection of common routines using @ chains.  " &
                  "A chain is a command, pragma or procedure run multiple times " &
                  "in a row but with different parameters.  "  &
                   "The chains package currently only works with procedure chains.");
     content( e, "p := chains.chain_count","e := chains.chain_context " );
     content( e, "b := chains.in_chain" );
     seeAlso( e, "doc/pkg_chains.html" );
     endHelp( e );
 elsif helpTopic = "clear" then
     startHelp( e, "clear" );
     summary( e, "clear" );
     authorKen( e );
     categoryBuiltin( e );
     description( e,
      "Clear the screen. " &
      "Reset the display device and clear the screen, placing the cursor in " &
      "the top-left corner of the display." );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "close" then
     startHelp( e, "close" );
     summary( e, "close (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "Closes an open file." );
     params( e, "file - a text_io file" );
     content( e, "close( file )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
--  elsif helpTopic = "command" then
--     Put_Line( "command - run a Linux command (instead of a built-in command)" );
--     Put_Line( "  " & bold( "command" ) & " cmd" );
  elsif helpTopic = "command_line" then
     discardUnusedIdentifier( token );
     startHelp( e, "command_line" );
     summary( e, "command_line package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using command line arguments, " &
       "environment variables, and the exit status.  " &
        "Command line arguments can also be accessed through the following Bourne " &
        "shell shortcuts: $# (the number of arguments), $1..$9 (the first nine " &
         "arguments), and $0 (the command path).  See also the os package.");
     content( e, "s := argument( p )" );
     content( e, "n := argument_count" );
     content( e, "s := command_name");
     content( e, "n := environment.environment_count" );
     content( e, "s := environment.environment_value( p )" );
     content( e, "set_exit_status( n )");
     seeAlso( e, "doc/pkg_cmdline.html" );
     endHelp( e );
  elsif helpTopic = "configure" then
     startHelp( e, "configure" );
     summary( e, "configuration block" );
     categoryKeyword( e );
     description( e, "Begin a new configuration block.  It contains declarations by system administrators.  Configuration blocks can automatically load from a global file (/etc/sparforte_config), which is done after a global policy file." );
     content( e, ( "configuration name is ... end name;" ) );
     seeAlso( e, "doc/ref_subprograms.html" );
     endHelp( e );
     authorKen( e );
  elsif helpTopic = "create" then
     startHelp( e, "create" );
     summary( e, "create (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "Open a new file, or overwrite an existing one." );
     params( e, "file - a text_io file" );
     params( e, "type - out_file (default) or append_file" );
     params( e, "path - path to the file (default, a temporary file)" );
     content( e, "create( file [, type] [, path ] ] )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "db" then
     helpDb;
  elsif helpTopic = "dbm" then
     helpDbm;
  elsif helpTopic = "declare" then
     startHelp( e, "declare" );
     summary( e, "declare/begin block" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "Begin a new unnamed block of source code within a procedure or function." );
     content( e, ( "[declare declarations] begin executable-commands exception handlers end;" ) );
     seeAlso( e, "doc/ref_subprograms.html" );
     endHelp( e );
  elsif helpTopic = "delay" then
     startHelp( e, "delay" );
     summary( e, "delay statement" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "Wait (or sleep) for the specified time." );
     params( e, "secs - the number of seconds" );
     content( e, "delay secs" );
     seeAlso( e, "doc/ref_other.html" );
     endHelp( e );
  elsif helpTopic = "delete" then
     startHelp( e, "delete" );
     summary( e, "delete (text_io package/SQL)" );
     authorKen( e );
     category( e, "keyword and built-in procedure" );
     description( e, "As a text.io procedure, delete closes and removes a file.  " &
       "As an SQL command, delete removes rows from a database table." );
     params( e, "file - a text_io file" );
     content( e, "delete( file )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "directory_operations" then
     startHelp( e, "directory_operations" );
     summary( e, "directory_operations package" );
     authorKen( e );
     modified( e, "Marek Kuziel" );
     categoryPackage( e );
     description( e, "A collection of common routines using directories and file paths." );
     content( e, "c := dir_separator","change_dir( p )","remove_dir( p [, r] )" );
     content( e, "p := get_current_dir","s := dir_name( p )","s := base_name( p [, f] )" );
     content( e, "s := file_extension( p )","s := file_name( p )","s := format_pathname( p [,t] )" );
     content( e, "s := expand_path( p [,t] )","make_dir( p )","close( d )" );
     content( e, "b := is_open( d )","open( d, p )","read( d, s )" );
     content( e, "s := absolute_dir_name( d )" );
     seeAlso( e, "doc/pkg_dirops.html" );
     endHelp( e );
  elsif helpTopic = "doc_format" then
     startHelp( e, "Description of the documentation format" );
     -- summary( e, "Summary: A short description, often in Backus-Naur form" );
     -- releaseVersion( e, "Any version number or release phase" );
     -- author( e, "The person who wrote this and/or contact info" );
     -- createdOn( e, "When it was first created" );
     -- modified( e, "When it was last modified" );
     -- category( e, "Category: A grouping name or tag this is placed under" );
     -- params( e, "The arguments, options or parameters" );
     -- exceptions( e, "Any exceptions that may be raised, if any" );
     -- returns( e, "Return values or status codes returned" );
     -- description( e, "Description: A detailed description" );
     -- content( e, "Content: What this contains, such as functions in packages.  May have subheadings" );
     -- examples( e, "Examples of use" );
     -- rationale( e, "Implementation Notes. The reasons for the design and its limits");
     -- bugs( e, "Any known bugs" );
     -- todos( e, "Any known unfinished work" );
     -- seeAlso( e, "Where to find more details or related documentation" );
     -- icon( e, "A pictogram for this item" );
     -- screenshot( e, "An image of a window or computer screen for this item" );

     author( e, "author - who wrote the script. (This cannot be a team.member variable" );
     bugs( e, "bugs - known bugs.  It may be used multiple times." );
     createdOn( e, "created - when the script was first released" );
     category( e, "category - a user string to organize scripts into groups" );
     content( e, "content - listing of subprograms or other item contained inside.  " &
                 "Use once for each item.  Use an empty string followed by a second annotation " &
                 "with a subheading to show a subheading."  );
     description( e, "description - a detailed description of what the script does.  " &
                  "It may be used multiple times to build a longer description." );
     errors( e, "errors - errors returned from the script.  It may be used multiple times." );
     examples( e, "examples - examples of use.  It may be used multiple times." );
     exceptions( e, "exceptions - exceptions used in the script.  It may be used multiple times." );
     footer( e, "footer - a footer for the annotations." );
     icon( e, "icon - the location of an icon for the script" );
     modified( e, "modified - when the script was last modified" );
     params( e, "param - description of a parameter to the script.  It may be used multiple times." );
     rationale( e, "rationale - the reasons behind a design decision.  It may be used multiple times." );
     returns( e, "return - return value" );
     screenshot( e, "screenshot - the location of a screenshot for the script" );
     seeAlso( e, "see_also - a reference to another document or script" );
     summary( e, "summary - a short description of what the script does" );
     todos( e, "todo - unfinished work.  It may be used multiple times." );
     releaseVersion( e, "version - the version of the script" );
     endHelp( e );
  elsif helpTopic = "end_of_file" then
     startHelp( e, "end_of_file" );
     summary( e, "end_of_file (text_io package)" );
     authorKen( e );
     categoryFunction( e );
     description( e, "True if a file has no more data in a file." );
     params( e, "file - a text_io file" );
     content( e, "end_of_file( file )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "end_of_line" then
     startHelp( e, "end_of_line" );
     summary( e, "end_of_line (text_io package)" );
     authorKen( e );
     categoryFunction( e );
     description( e, "True if an in_file file has reached the end of a line with get" );
     params( e, "file - a text_io file" );
     content( e, "end_of_line( file )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "doubly_linked_lists" then
     startHelp( e, "doubly_linked_lists" );
     summary( e, "doubly_linked_lists package" );
     authorKen( e );
     createdOn( e, "v1.5");
     categoryPackage( e );
     description( e, "A collection of common routines using bi-directional lists." );
     content( e, "append( l, e )","s := assemble( l [,d [,f]] )" );
     content( e, "assign( l1, l2 )","clear( l )" );
     content( e, "b := contains( l, e )","delete( l, c [,n] )" );
     content( e, "delete_first( l [,n] )","delete_last( l [,n] )" );
     content( e, "disassemble( s, l [,d [,f] ] )","e := element( c )" );
     content( e, "find( l, e, c )","first( l, c )" );
     content( e, "e := first_element( l )","flip( l )" );
     content( e, "b := has_element( c )" );
     content( e, "insert_before( l, c [, n] ) | ( l, c, e [, n] )" );
     content( e, "insert_before_and_mark( l, c, c2 [, n] ) | ( l, c, e, c2 [, n] )" );
     content( e, "b := is_empty( l )" );
     content( e, "e := last_element( l )","n := length( l )" );
     content( e, "move( l1, l2 )","next( c )" );
     content( e, "parcel( s, w, l )","prepend( l, e )","previous( c )" );
     content( e, "replace_element( l, c, e )","reverse_elements( l )" );
     content( e, "reverse_find( l, e, c )" );
     content( e, "splice( l1, c, l2 [,c2] ) | ( l1, c, c2 )" );
     content( e, "swap( l, c1, c2 )","swap_links( l, c1, c2 )" );
     seeAlso( e, "doc/pkg_doubly.html" );
     endHelp( e );
  elsif helpTopic = "dynamic_hash_tables" then
     startHelp( e, "dynamic_hash_tables" );
     summary( e, "dynamic_hash_tables package" );
     authorKen( e );
     createdOn( e, "v1.5");
     categoryPackage( e );
     description( e, "A collection of common routines using in-memory, dynamically growing hash tables." );
     content( e, "add( t, k, v )","append( t, k, v )" );
     content( e, "decrement( t, k [,n] )","v := get( t, k )" );
     content( e, "get_first( t, v, f )","get_next( t, v, f )" );
     content( e, "b := has_element( t, k )","increment( t, k [,n] )" );
     content( e, "prepend( t, k, v )","remove( t, k )" );
     content( e, "replace( t, k, v )","reset( t )" );
     content( e, "set( t, k, v )" );
     seeAlso( e, "doc/pkg_dht.html" );
     endHelp( e );
  elsif helpTopic = "enums" then
     startHelp( e, "enums" );
     summary( e, "enums package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using enumerated types." );
     content( e, "enums.first(t)","enums.last(t)","enums.pred(e2)");
     content( e, "enums.succ(e2)");
     seeAlso( e, "doc/pkg_enums.html" );
     endHelp( e );
  elsif helpTopic = "env" then
     startHelp( e, "env" );
     summary( e, "env [var]" );
     authorKen( e );
     categoryBuiltin( e );
     description( e,
       "Display a list of all declared identifiers, their values and their " &
       "properties, or the properties of a particular identifier.  Supports " &
       "AdaScript parameters." );
     params( e, "var - the identifier to display" );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "exceptions" then
     startHelp( e, "exceptions" );
     summary( e, "exceptions package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using exceptions." );
     content( e, "s := exceptions.exception_name","s := exceptions.exception_info" );
     content( e, "n := exceptions.exception_status_code" );
     seeAlso( e, "doc/pkg_exceptions.html" );
     endHelp( e );
  elsif helpTopic = "exit" then
     startHelp( e, "exit" );
     summary( e, "exit statement" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "Break out of a loop.  There is an optional exit condition." );
     content( e, "exit [when condition]" );
     seeAlso( e, "doc/ref_flow.html" );
     endHelp( e );
  elsif helpTopic = "files" then
     startHelp( e, "files" );
     summary( e, "files package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using files." );
     content( e, "b := basename( p )","b := dirname( p )","b := exists( p )" );
     content( e, "b := is_absolute_path( p )","b := is_directory( p )","b := is_executable( p )" );
     content( e, "b := is_executable_file( p )","b := is_regular_file( p )","b := is_readable( p )" );
     content( e, "b := is_readable_file( p )","b := is_waiting_file( p )","b := is_writable( p )  " );
     content( e, "b := is_writable_file( p )","t := last_accessed( p )","t := last_changed( p )" );
     content( e, "t := last_modified( p )","l := size( p )" );
     seeAlso( e, "doc/pkg_files.html" );
     endHelp( e );
  elsif helpTopic = "for" then
     startHelp( e, "for statement" );
     summary( e, "for" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "The for loop increments its index variable by 1 until it iterates through " &
                  "the specified range. The range can either be numeric or enumerated." );
     content( e, "for var in [reverse] first..last loop...end loop" );
     seeAlso( e, "doc/ref_flow.html" );
     endHelp( e );
  elsif helpTopic = "function" then
     startHelp( e, "function" );
     summary( e, "function statement" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "A user-defined function is a subprogram that returns " &
                  "a value so that it can be used in an expression.  Parameter " &
                "modes can be in, out, or in out.  A function specification " &
                "is a function without a body.  It must be completed later " &
                "with a full defintion." );
     content( e, "function f return type is [abstract] ...begin...end f" );
     content( e, "function f( p : [mode] type [; p2...] ) return type is [abstract] ...begin...end f" );
     content( e, "function f...return type is null abstract" );
     content( e, "function f...return type is separate" );
     content( e, "function f...return type;" );
     seeAlso( e, "doc/ref_subprograms.html" );
     endHelp( e );
  elsif helpTopic = "get" then
     startHelp( e, "get" );
     summary( e, "get (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "Read a character from a file." );
     params( e, "file - the text_io file.  The default is standard_input." );
     params( e, "ch - the character read." );
     content( e, "get ([file,] ch)" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "get_immediate" then
     startHelp( e, "get_immediate" );
     summary( e, "get_immediate (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "Read a character from a current_input without displaying on current_output." );
     params( e, "ch - the character read" );
     params( e, "b - true if non-blocking (default true)" );
     content( e, "get_immediate (ch [, b])" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "get_line" then
     startHelp( e, "get_line" );
     summary( e, "get_line (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     params( e, "file - the text_io file (default standard_input)" );
     description( e, "Read a line of text from a file." );
     content( e, "var := get_line [ (file) ]" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "glossary" then
     startHelp( e, "glossary" );
     summary( e, "glossary" );
     authorKen( e );
     category( e, "reference" );
     content( e, "Access type - a 'handle' or extended pointer to a storage location." );
     content( e, "Actual Parameter - the name of the parameter to a subprogram when it is being called. That is, 'put_line( foo );' foo is the actual parameter." );
     content( e, "Aggregate - a type with more than one value, like an array or record." );
     content( e, "Array - a composite type whose components are all of the same type, where components are selected with indices." );
     content( e, "Declaration - a language construct that associates a name with a storage location." );
     content( e, "Discrete Type - an integer or enumerated type (including characters)." );
     content( e, "Discriminant - a parameter of a composite type, like an index to an array." );
     content( e, "Elementary Type - a type without components." );
     content( e, "Expansion - changing the text of a line using a Bourne $ substitution." );
     content( e, "Expression - a calculation using operators, functions, etc. that can be assigned or used as a parameter." );
     content( e, "Fixed Point Type - a number with a fixed number of decimal places, often used for currency." );
     content( e, "Formal Parameter - the name of the parameter to a subprogram as used inside of a subprogram. That is, the parameter name used when the function is declared." );
     content( e, "Limited Type - a type for which assignment is not allowed." );
     content( e, "Literal - something you type representing a specific value, not a variable. The number 57 is a numeric literal. The string 'hello world' is a string literal." );
     content( e, "Package - a 'library' or 'module'. A program unit containing a collection of related items." );
     content( e, "Pragma - an interpreter directive" );
     content( e, "Record Type - a composite types with zero or more named components of various types, where components are selected by name." );
     content( e, "Real Type - a fixed point or floating point number." );
     content( e, "Scalar Type - a discrete or real type. In AdaScript, a string is also a scalar type." );
     content( e, "Tagged Type - a 'class'" );
     content( e, "Task - a 'thread'" );
     seeAlso( e, "doc/ref_adascript.html" );
     endHelp( e );
  elsif helpTopic = "gnat.crc32" then
     startHelp( e, "gnat.crc32" );
     summary( e, "gnat.crc32 package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines for cyclic redundancy checks." );
     content( e, "gnat.crc32.initialize( c )" );
     content( e, "gnat.crc32.update(c, s )" );
     content( e, "i := gnat.crc32.get_value( c )" );
     seeAlso( e, "doc/pkg_gnat_crc32.html" );
     endHelp( e );
  elsif helpTopic = "gnat.cgi" then
     startHelp( e, "gnat.crc32" );
     summary( e, "gnat.crc32 package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines for web programming and cookies.  " &
                  "See also cgi.");
     content( e, "gnat.cgi.put_header [(f) | (h [,f])]" );
     content( e, "b := gnat.cgi.ok" );
     content( e, "m := gnat.cgi.method" );
     content( e, "s := gnat.cgi.metavariable( k [,r] )" );
     content( e, "b := gnat.cgi.metavariable.exists( k )" );
     content( e, "s := gnat.cgi.url" );
     content( e, "n := gnat.cgi.argument_count" );
     content( e, "s := gnat.cgi.value( k [,r] | p )" );
     content( e, "b := gnat.cgi.key_exists( k )" );
     content( e, "s := gnat.cgi.key( p )" );
     content( e, "gnat.cgi.cookie.put_header [ (f) | (h [,f]) ]" );
     content( e, "b := gnat.cgi.cookie_ok" );
     content( e, "n := gnat.cgi.cookie_count" );
     content( e, "s := gnat.cgi.cookie_value( k [,r] )" );
     content( e, "b := gnat.cgi.cookie_exists( k )" );
     content( e, "s := gnat.cgi.cookie_key( p )" );
     content( e, "gnat.cgi.cookie.set( k [,v [,c [,d [,m [,p [,s] ] ] ] ] ] )" );
     content( e, "s := gnat.cgi.debug.text_output" );
     content( e, "s := gnat.cgi.debug.html_output" );
     seeAlso( e, "doc/pkg_gnat_cgi.html" );
     endHelp( e );
  elsif helpTopic = "help" then
     startHelp( e, "help" );
     summary( e, "help  [-c|-h|-m|-l|-t]" );
     authorKen( e );
     categoryBuiltin( e );
     description( e,
      "Show short advice on various topics.  Help can also " &
      "show documentation in a script or run reports. When showing " &
      "script docs, -h will show the annotations in HTML, -m as a man " &
      "page and, with no options, SparForte will show the annotations as " &
      "plain text. With -l, SparForte will show the script license as set " &
      "with pragma license. With -c, the teamwork pragmas will be shown in " &
      "CSV format. With -t, the todo pragmas will be shown in CSV format. " &
      "and with a work summary.  If no topic is known, help will try the " &
      "operating system documentation.  Supports AdaScript parameters.");
     params( e, "-c - show team collaboration report" );
     params( e, "-h - HTML output" );
     params( e, "-l - show script license report" );
     params( e, "-m - UNIX manual page output" );
     params( e, "-t - show todo pragmas and summarize" );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "hash_io" then
     helpHashIo;
  elsif helpTopic = "hashed_maps" then
     startHelp( e, "hashed_maps" );
     summary( e, "hashed maps package" );
     authorKen( e );
     categoryPackage( e );
     createdOn( e, "v2.4.2");
     description( e, "A collection of common routines for working with key-value tables (such as arrays in PHP, PERL hashes or Python dictionaries)." );
     content( e, "clear( m )", "is_empty( m )", "c := capacity( m ) ");
     content( e, "reserve_capacity( m, c )", "c := length( m )", "insert( m, k, e )" );
     content( e, "b := equal( m1, m2 )", "include( m, k, e )" );
     content( e, "replace( m, k, e )", "exclude( m, k, e )", "delete( m, k, e | c )" );
     content( e, "b := contains( m, k )", "e := element( m, k ) | ( c )" );
     content( e, "append( m, k, e )", "prepend( m, k, e )", "increment( m, k [, n] )" );
     content( e, "decrement( m, k [, n] )", "e := extract( m, k )" );
     content( e, "assign( t, s )",  "move( t, s )", "replace_element( m, c, e )" );
     content( e, "first( m, c )",  "next( c )", "k := key( c )" );
     content( e, "find( m, k, c )", "insert( m, k, c, b )", "insert( m, k, e, c, b )" );
     content( e, "b := has_element( c )" );
     seeAlso( e, "doc/pkg_hashed_maps.html" );
     endHelp( e );
  elsif helpTopic = "history" then
     startHelp( e, "history command" );
     summary( e, "history [amount|-c]" );
     authorKen( e );
     categoryBuiltin( e );
     description( e,
       "show the command history (up to n lines). If the number of lines is " &
       "not specified, show all the command history.  Use -c to erase the " &
       "command history.  Supports AdaScript parameters." );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "keys" then
     discardUnusedIdentifier( token );
     startHelp( e, "keys" );
     summary( e, "keyboard keys" );
     category( e, "reference" );
     authorKen( e );
     description( e, "A summary of the keyboard editing keys." );
     section( e, "Emacs Mode" );
     content( e, "control-b - move backwards" );
     content( e, "control-f - move forwards" );
     content( e, "control-p - move up" );
     content( e, "control-n - move down" );
     content( e, "control-x - erase line" );
     content( e, "control-a - move to start" );
     content( e, "control-e - move to end" );
     content( e, "control-r - search history" );
     content( e, "control-] - character search" );
     content( e, "tab       - complete filename" );
     section( e, "Vi Mode" );
     content( e, "J - move backwards" );
     content( e, "K - move forwards" );
     content( e, "I - move up" );
     content( e, "M - move down" );
     content( e, "^ - move to start" );
     content( e, "$ - move to end" );
     content( e, "ESC ESC - complete filename" );
     content( e, "ESC - enter/exit vi mode" );
     seeAlso( e, "doc/ref_cmdline.html" );
     endHelp( e );
  elsif helpTopic = "if" then
     startHelp( e, "if" );
     summary( e, "if statement" );
     description( e, "if statements are used for conditional branching." );
     authorKen( e );
     categoryKeyword( e );
     content( e, "if expression then ... [elsif expression then...] [else ...] end if" );
     seeAlso( e, "doc/ref_flow.html" );
     endHelp( e );
  elsif helpTopic = "inkey" then
     startHelp( e, "inkey" );
     summary( e, "inkey (text_io package)" );
     authorKen( e );
     categoryFunction( e );
     description( e, "Read a character from current_input without echoing to current_output." );
     content( e, "c := inkey" );
     authorKen( e );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "is_open" then
     startHelp( e, "is_open" );
     summary( e, "is_open (text_io package)" );
     authorKen( e );
     categoryFunction( e );
     description( e, "Return true if file is open." );
     params( e, "file - a text_io file" );
     content( e, "is_open( file )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "jobs" then
     startHelp( e, "jobs" );
     summary( e, "jobs" );
     categoryBuiltin( e );
     authorKen( e );
     description( e,
       "Give the status of commands currently running in the background" );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "l10n" then
     startHelp( e, "l10n" );
     summary( e, "localization package" );
     authorKen( e );
     categoryPackage( e );
     createdOn( e, "v2.4.2");
     description( e, "A collection of common routines for different languages and countries. " &
     " This is a binding to the GNU locale functions." );
     content( e, "s := codeset", "s := d_t_fmt", "s := d_fmt" );
     content( e, "s := t_fmt", "s := day( p )", "s := abday( p )" );
     content( e, "s := mon( p )", "s := abmon( p )", "s := decimal_point" );
     content( e, "s := thousands_sep", "s := yesexpr", "s := noexpr" );
     content( e, "s := currency_symbol", "s := amstr", "s := pmstr" );
     content( e, "s := t_fmt_ampm", "s := negative_sign", "s := positive_sign" );
     content( e, "s := int_curr_symbol","s := getlocale( c )", "setlocale( c, s )" );
     content( e, "n := grouping", "n := frac_digits","n := int_frac_digits" );
     content( e, "n := p_sep_by_space", "n := p_cs_precedes","n := p_sign_posn" );
     content( e, "n := n_sep_by_space", "n := n_cs_precedes","n := n_sign_posn" );
     content( e, "s := mon_decimal_point", "s := mon_thousands_sep","n := mon_grouping" );
     content( e, "s := era", "s := era_year", "s := era_d_t_fmt" );
     content( e, "s := era_d_fmt", "s := era_t_fmt" );
     authorKen( e );
     seeAlso( e, "doc/pkg_l10n.html" );
     endHelp( e );
  elsif helpTopic = "line" then
     startHelp( e, "line" );
     summary( e, "line (text_io package)" );
     categoryFunction( e );
     authorKen( e );
     description( e, "Return the number of read/written lines." );
     params( e, "file - a text_io file" );
     content( e, "line( file )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "logout" then
     startHelp( e, "logout" );
     summary( e, "logout" );
     authorKen( e );
     categoryBuiltin( e );
     description( e,
        "Stop an interactive, login session and leave the SparForte shell. " &
        "You have a login session if SparForte is your login shell or " &
        "if you start SparForte with the --login option. When in the " &
        "debugger (breakout mode), logout will abandon the debugger " &
        "session and leave the SparForte shell.  (Use the return command " &
        "to leave a non-login session.)" );
     errors( e, "If the session is not a login session, an error is displayed" );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "lock_files" then
     discardUnusedIdentifier( token );
     startHelp( e, "lock_files" );
     summary( e, "lock_files package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using lock files." );
     content( e, "lock_files.lock_file( dir, file [,wait [,retries] )" );
     content( e, "lock_files.lock_file( file [,wait [,retries] )" );
     content( e, "lock_files.unlock_file( dir, file )" );
     content( e, "lock_files.unlock_file( file )" );
     seeAlso( e, "doc/pkg_lock_files.html" );
     endHelp( e );
  elsif helpTopic = "logs" then
     discardUnusedIdentifier( token );
     startHelp( e, "logs" );
     summary( e, "logs package" );
     authorKen( e );
     categoryPackage( e );
     createdOn( e, "v2.2");
     description( e, "A collection of common routines for writing error logs.  " &
                  "Log messages are in increasing in severity: info, " &
                  "warning and error.  The ok message indicates a successful " &
                  "completion of an action." );
                  --"completion of an action.  A chain of message procedures " &
                  --"will make a single log entry." );
     content( e, "logs.close" );
     content( e, "logs.error( m )" );
     content( e, "logs.info( m )" );
     content( e, "b := logs.is_open" );
     content( e, "b := logs.is_rotating" );
     content( e, "logs.level_begin( l )" );
     content( e, "logs.level_end( l )" );
     content( e, "m := logs.mode" );
     content( e, "logs.ok( m )" );
     content( e, "logs.open( path, mode [, width] )" );
     content( e, "logs.rotate_begin" );
     content( e, "logs.rotate_end" );
     content( e, "logs.warning( m )" );
     seeAlso( e, "doc/pkg_logs.html" );
     endHelp( e );
  elsif helpTopic = "loop" then
     startHelp( e, "loop" );
     summary( e, "loop statement" );
     description( e, "A 'loop' loop is a general purpose loop. It can only be exited with 'exit'. " );
     authorKen( e );
     categoryKeyword( e );
     content( e, "loop ...end loop" );
     seeAlso( e, "doc/ref_flow.html" );
     endHelp( e );
  elsif helpTopic = "new_line" then
     startHelp( e, "new_line" );
     summary( e, "new_line (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "Begin a new line of text." );
     params( e, "file - a text_io file.  The default is current_output" );
     content( e, "new_line [(file)]" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "strings.match" then
     startHelp( e, "strings.match" );
     summary( e, "strings.match (strings package)" );
     authorKen( e );
     categoryFunction( e );
     description( e, "Pattern matching with UNIX V7 regular expressions and PERL extensions." );
     section( e, "Syntax" );
     content( e, "bool := match( expression, string )" );
     section( e, "Matching Symbols" );
     content( e, "^ - at beginning" );
     content( e, ". - any character" );
     content( e, "$ - at end" );
     content( e, "? - zero or one character" );
     content( e, "[s] - any in set s" );
     content( e, "+ - one or more characters" );
     content( e, "[^s] - any not in set s" );
     content( e, "* - zero or more characters" );
     content( e, "\ - escape character" );
     content( e, "(e) - nested expression" );
     content( e, "| - alternative" );
     seeAlso( e, "doc/pkg_strings.html" );
     footer( e, "Regular expressions are described in ""man 5 regexp""" );
     endHelp( e );
  elsif helpTopic = "strings.perl_match" then
     startHelp( e, "strings.perl_match" );
     summary( e, "strings.perl_match (strings package)" );
     authorKen( e );
     categoryFunction( e );
     description( e, "Pattern matching with PERL Compatible Regular Expressions (PCRE)." );
     section( e, "Syntax" );
     content( e, "bool := perl_match( expression, string )" );
     section( e, "Character Types" );
     content( e, ".      any character except newline;" );
     content( e, "\C     one data unit, even in UTF mode (best avoided)" );
     content( e, "\d     a decimal digit" );
     content( e, "\D     a character that is not a decimal digit" );
     content( e, "\h     a horizontal white space character" );
     content( e, "\H     a character that is not a horizontal white space character" );
     content( e, "\N     a character that is not a newline" );
     content( e, "\p{xx} a character with the xx property" );
     content( e, "\P{xx} a character without the xx property" );
     content( e, "\R     a newline sequence" );
     content( e, "\s     a white space character" );
     content( e, "\S     a character that is not a white space character" );
     content( e, "\v     a vertical white space character" );
     content( e, "\V     a character that is not a vertical white space character" );
     content( e, "\w     a 'word' character" );
     content( e, "\W     a 'non-word' character" );
     content( e, "\X     a Unicode extended grapheme cluster" );
     section( e, "Quantifiers" );
     content( e, "?       0 or 1, greedy" );
     content( e, "?+      0 or 1, possessive" );
     content( e, "??      0 or 1, lazy" );
     content( e, "*       0 or more, greedy" );
     content( e, "*+      0 or more, possessive" );
     content( e, "*?      0 or more, lazy" );
     content( e, "+       1 or more, greedy" );
     content( e, "++      1 or more, possessive" );
     content( e, "+?      1 or more, lazy" );
     content( e, "{n}     exactly n" );
     content( e, "{n,m}   at least n, no more than m, greedy" );
     content( e, "{n,m}+  at least n, no more than m, possessive" );
     content( e, "{n,m}?  at least n, no more than m, lazy" );
     content( e, "{n,}    n or more, greedy" );
     content( e, "{n,}+   n or more, possessive" );
     content( e, "{n,}?   n or more, lazy" );
     section( e, "Anchors" );
     content( e, "\b      word boundary" );
     content( e, "\B      not a word boundary" );
     content( e, "^       start of subject" );
     content( e, "\A      start of subject" );
     content( e, "$       end of subject" );
     content( e, "\Z      end of subject" );
     content( e, "\z      end of subject" );
     content( e, "\G      first matching position in subject" );
     footer( e, "This is not complete.  See the PCRE documentation for more." );
     seeAlso( e, "doc/pkg_strings.html, https://pcre.org" );
     endHelp( e );
  elsif helpTopic = "mysql" then
     helpMySQL;
  elsif helpTopic = "mysqlm" then
     helpMySQLM;
  elsif helpTopic = "memcache" then
     startHelp( e, "memcache" );
     summary( e, "memcache and memcache.highread packages" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using memcached, " &
                  "a distributed in-memory cache." );
     section( e, "memcache" );
     content( e, "add( cl, k, v )","prepend( cl, k, v )" );
     content( e, "append( cl, k, v )","register_server( cl, h, p )" );
     content( e, "clear_servers( cl )","replace( cl, k, v )" );
     content( e, "delete( cl, k )","set( cl, k, v )" );
     content( e, "flush( cl )","set_cluster_name( cl, s )" );
     content( e, "is_valid_memcache_key( k )","set_cluster_type( cl, e )" );
     content( e, "v := get( cl, k )","s := stats( cl )" );
     content( e, "cl := new_cluster","s := version( cl )" );
     section( e, "memcache.highread" );
     content( e, "add( cl, k, v )","register_alpha_server( cl, h, p )" );
     content( e, "append( cl, k, v )","register_beta_server( cl, h, p )" );
     content( e, "clear_servers( cl )","replace( cl, k, v )" );
     content( e, "delete( cl, k )","set( cl, k, v )" );
     content( e, "flush( cl )","set_cluster_name( cl, s )" );
     content( e, "v := get( cl, k )","set_cluster_type( cl, e )" );
     content( e, "cl := new_cluster","s := stats( cl )" );
     content( e, "prepend( cl, k, v )","s := version( cl )" );
     seeAlso( e, "doc/pkg_memcache.html and doc/pkg_memcache_highread.html" );
     endHelp( e );
  elsif helpTopic = "mode" then
     startHelp( e, "mode" );
     summary( e, "mode (text_io package)" );
     categoryFunction( e );
     authorKen( e );
     description( e, "Return the file mode (in_file, out_file, append_file)" );
     params( e, "file - a text_io file" );
     content( e, "mode( file )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "name" then
     startHelp( e, "name" );
     summary( e, "name (text_io package)" );
     categoryFunction( e );
     authorKen( e );
     description( e, "Return the path of an open file" );
     params( e, "file - a text_io file" );
     content( e, "name( file )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "null" then
     startHelp( e, "null" );
     summary( e, "null statement" );
     description( e, "The null statement doesn't do anything. It acts " &
        "as a placeholder in contexts where statements or commands " &
        "are required. There are no arguments." );
     categoryKeyword( e );
     authorKen( e );
     content( e, "null;" );
     seeAlso( e, "doc/ref_other.html" );
     endHelp( e );
  elsif helpTopic = "os" then
     startHelp( e, "os" );
     summary( e, "os package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common bindings for the operating system." );
     content( e, "s := error_string( i )" );
     content( e, "n := last_child" );
     content( e, "n := pid" );
     content( e, "i := status" );
     content( e, "system( s )" );
     footer( e, "The Bourne shell style $? operand is the same as os.status" );
     seeAlso( e, "doc/pkg_os.html" );
     endHelp( e );
  elsif helpTopic = "open" then
     startHelp( e, "open" );
     summary( e, "open (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "Open an existing file." );
     params( e, "file - a text_io file" );
     params( e, "type - in_file, out_file or append_file" );
     params( e, "path - path to the file" );
     content( e, "open( file, type, path )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "procedure" then
     startHelp( e, "procedure" );
     summary( e, "procedure statement" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "A user-defined procedure is a subprogram that does not return " &
                  "a value for use in an expression.  Parameter " &
                "modes can be in, out, or in out.  A procedure specification " &
                "is a procedure without a body.  It must be completed later " &
                "with a full definition.");
     content( e, "procedure p is [abstract] ...begin...end p" );
     content( e, "procedure p( p : [mode] type [; p2...] )is [abstract] ...begin...end p" );
     content( e, "procedure p...is null abstract" );
     content( e, "procedure p...is separate" );
     content( e, "procedure p;" );
     seeAlso( e, "doc/ref_subprograms.html" );
     bugs( e, "For known bugs, see the HTML manual" );
     endHelp( e );
  elsif helpTopic = "pragma" then
     helpPragma;
  elsif helpTopic = "put" then
     startHelp( e, "put" );
     summary( e, "put (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "Write a value to a file and do not start a new line.  If the value is " &
        "numeric, use the optional picture string to format the number." );
     params( e, "file - the text_io file (default current_output)" );
     params( e, "expression - the value to write" );
     params( e, "picture - the format string for a number" );
     section( e, "Syntax" );
     content( e, "put( [file,] expression [,picture] )" );
     section( e, "Picture Symbols" );
     content( e, "'+' - the number will be printed with a leading + or -" );
     content( e, "'-' - a negative numbers will be printed with a leading -" );
     content( e, "'<' and '>' - a negative number will be printed with (..)" );
     content( e, "CR - a negative number will be printed with a leading " & Ada.Characters.Latin_1.Quotation & "CR" & Ada.Characters.Latin_1.Quotation & " (credit)" );
     content( e, "DB - a negative number will be printed with a leading "& Ada.Characters.Latin_1.Quotation & "DB" & Ada.Characters.Latin_1.Quotation & " (debit)" );
     content( e, "'$' - the currency symbol will be printed, or a floating dollar sign if multiple instances" );
     content( e, "'.' - marks the actual position for a decimal point" );
     content( e, "'V' - marks the assumed position for a decimal point" );
     content( e, "'9' - space for a number with leading zeros" );
     content( e, "'#' - same as '$', except only the leading character is shown" );
     content( e, "'Z' - space for a numbers with leading blanks" );
     content( e, "'_', 'B', '0', '/'- inserted. 'B' is a blank" );
     content( e, "'*' - space for a number with leading asterisks" );
     seeAlso( e, "doc/pkg_text_io.html and doc/ref_numberformat.html" );
     endHelp( e );
  elsif helpTopic = "put_line" then
     startHelp( e, "put_line" );
     summary( e, "put_line (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "Write a value to a file and start a new line" );
     content( e, "put_line ( [file,] expression )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "pwd" then
     startHelp( e, "pwd" );
     summary( e, "pwd" );
     authorKen( e );
     categoryBuiltin( e );
     description( e,
        "Show the path of the present (current) working directory" );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "records" then
     startHelp( e, "records" );
     summary( e, "records package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using records" );
     content( e, "to_record( r, s )","to_json( s, r )" );
     seeAlso( e, "doc/pkg_records.html" );
     endHelp( e );
  elsif helpTopic = "raise" then
     startHelp( e, "raise" );
     summary( e, "raise statement" );
     categoryKeyword( e );
     authorKen( e );
     description( e, "Raise (throw) an exception.  There is an optional raise " &
       "condition which will raise if the condition is true." );
     content( e, "raise [when condition]  - re-raise an exception in an exception handler" );
     content( e, "raise e [when condition]- raise exception e" );
     content( e, "raise e with s [when condition]- raise exception e with new message s" );
     seeAlso( e, "doc/ref_subprograms.html" );
     endHelp( e );
  elsif helpTopic = "reset" then
     startHelp( e, "reset" );
     summary( e, "reset (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "Reopen an open file, possibly changing the file mode." );
     params( e, "file - a text_io file" );
     params( e, "mode - in_file, out_file or append_file" );
     content( e, "reset( file [,mode])" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "return" then
     startHelp( e, "return" );
     summary( e, "return statement" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "Leave a procedure, function or main program.  If a " &
        "function, return a value that can be used in an expression.  There " &
        "is an optional return condition which will return if the condition " &
        "is true.  With " &
        "an interactive prompt, quits a shell.  In breakout mode, resumes " &
        "execution of the script." );
     content( e, "return [expression] [when condition]" );
     seeAlso( e, "doc/ref_subprograms.html" );
     endHelp( e );
  elsif helpTopic = "typeset" then
     startHelp( e, "typeset" );
     summary( e, "typeset var is type" );
     categoryBuiltin( e );
     authorKen( e );
     description( e,
        "Change the type of a variable, declaring it if necssary. It will " &
        "attempt to typecast the value of the variable if the variable " &
        "exists." );
     errors( e, "An exception is raised if the variable cannot be typecast" );
     errors( e, "An exception is raised if pragma ada_95 is enforced" );
     errors( e, "An exception is raised if not used in an interactive session" );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "set_input" or helpTopic = "set_output" or helpTopic = "set_error" then
     startHelp( e, "set_input" );
     summary( e, "set_input (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "set_input redirects the current_input to a file.  "  &
                  "set_output redirects the current_output to a file.  " &
                "set_error redirects the current_error to a file.  " &
      "The default files are standard_input, standard_output and standard_error." );
     params( e, "file - a text_io file" );
     content( e, "set_input( file )" );
     content( e, "set_output( file )" );
     content( e, "set_error( file )" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "shell" then
     startHelp( e, "shell" );
     summary( e, "Bourne shell feature syntax" );
     category( e, "reference" );
     description( e, "Bourne shell features." );
     section( e, "Variable Substitution" );
     content( e, "$X - replace with value of X" );
     content( e, "${X} - replace with value of X" );
     content( e, "${#X} - replace with length of X" );
     content( e, "${X:-Y} - replace with X.  Use Y if X is empty" );
     content( e, "${X:+Y} - replace with X.  Use Y if X is not empty" );
     content( e, "${X:?Y} - replace with X or show error if X is not empty.  Y is optional error message" );
     content( e, "${X:P} or ${X:P:L} - substring from position P in X length L.  First index is 0." );
     content( e, "${X/S/R} - search for S in X replace with R.  Then replace with the result." );
     content( e, "$(X^) - replace with proper case of X." );
     content( e, "$(X^^) - replace with uppercase of X." );
     content( e, "$(X,,) - replace with lowercase of X." );
     content( e, "$(X) - replace with output of commands X.  Not recursive." );
     section( e, "Positional Parameters and Special Variables" );
     content( e, "$0 or ${0} - the script path" );
     content( e, "$1..$9 or ${1}..$(9} - the first 9 parameters" );
     content( e, "$# or ${#} - the number of parameters" );
     content( e, "$? or ${?} - the error status of the last command" );
     content( e, "$$ or ${$} - the current process id" );
     content( e, "$! or ${!} - the process id of the last background command" );
     content( e, "$* or ${*} - all arguments with word splitting" );
     content( e, ASCII.Quotation & "$@" & ASCII.Quotation & " - all arguments" );
     section( e, "Quoting" );
     content( e, ASCII.Quotation & "X" & ASCII.Quotation & " - weak quoting" );
     content( e, "'X' - strong quoting" );
     content( e, "`X` - replace with output of commands X" );
     authorKen( e );
     seeAlso( e, "doc/ref_shellargs.html" );
     endHelp( e );
  elsif helpTopic = "skip_line" then
     startHelp( e, "skip_line" );
     summary( e, "skip_line (text_io package)" );
     authorKen( e );
     categoryProcedure( e );
     description( e, "Read a line from a file and discard it." );
     params( e, "file - a text_io file.  The default is current_input" );
     content( e, "skip_line [(file)]" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  elsif helpTopic = "sound" then
     startHelp( e, "sound" );
     summary( e, "sound package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using sound." );
     content( e, "play( ""path"" [,pri] )" );
     content( e, "playcd [( ""path"" )]" );
     content( e, "stopcd" );
     seeAlso( e, "doc/pkg_sound.html" );
     endHelp( e );
  elsif helpTopic = "source_info" then
     discardUnusedIdentifier( token );
     startHelp( e, "source_info" );
     summary( e, "source_info package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines for the script execution state." );
     Put_Line( "source_info (package) - information on the current script" );
     content( e, "s := enclosing_entity" );
     content( e, "s := file" );
     content( e, "p := line" );
     content( e, "n := script_size" );
     content( e, "s := source_location" );
     content( e, "n := symbol_table_size" );
     seeAlso( e, "doc/pkg_sinfo.html" );
     endHelp( e );
  elsif helpTopic = "stats" then
     discardUnusedIdentifier( token );
     startHelp( e, "stats" );
     summary( e, "stats package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using statistics." );
     content( e, "r := average( a )","r := max( a )","r := min( a )" );
     content( e, "r := standard_deviation( a )","r := sum( a )","r := variance( a )" );
     seeAlso( e, "doc/pkg_stats.html" );
     endHelp( e );
  elsif helpTopic = "subtype" then
     startHelp( e, "subtype" );
     summary( e, "subtype statement" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "The subtype statement will create a type that is " &
                  "compatible with the original, as if it was a renaming of the original type. " );
     content( e, "subtype newtype is [abstract|limited] oldtype [affirm...end affirm]" );
     seeAlso( e, "doc/ref_typedecl.html" );
     endHelp( e );
  elsif helpTopic = "System" then
     discardUnusedIdentifier( token );
     startHelp( e, "System" );
     summary( e, "System package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common constants describing the " &
                  "computer hardware and execution environment." );
     content( e, "System.System_Name","System.Fine_Delta" );
     content( e, "System.Max_Int","System.Tick" );
     content( e, "System.Min_Int","System.Storage_Unit" );
     content( e, "System.Max_Binary_Modulus","System.Word_Size" );
     content( e, "System.Max_Nonbinary_Modulus","System.Memory_Size" );
     content( e, "System.Max_Base_Digits","System.Default_Bit_Order" );
     content( e, "System.Max_Mantissa","System.Login_Shell" );
     content( e, "System.Restricted_Shell","System.Script_License" );
     content( e, "System.Script_Software_Model","System.System_Version" );
     content( e, "System.Design_Phase", "System.Maintenance_Phase" );
     content( e, "System.Testing_Phase", "System.Development_Phase" );
     seeAlso( e, "doc/pkg_system.html" );
     endHelp( e );
  elsif helpTopic = "teams" then
     startHelp( e, "teams" );
     summary( e, "teams package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common types describing development teams." );
     content( e, "teams.member" );
     content( e, "teams.work_measure" );
     content( e, "teams.work_priority" );
     seeAlso( e, "doc/pkg_teams.html" );
     endHelp( e );
  elsif helpTopic = "templates" then
     startHelp( e, "templates" );
     summary( e, "templates package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using templates." );
     content( e, "b := has_put_template_header" );
     content( e, "put_template_header" );
     content( e, "set_http_location( n )" );
     content( e, "set_http_status( i )" );
     seeAlso( e, "doc/pkg_templates.html" );
     endHelp( e );
  elsif helpTopic = "trace" then
     startHelp( e, "trace" );
     summary( e, "trace [true|false]" );
     authorKen( e );
     categoryBuiltin( e );
     description( e,
        "Show which lines are read as the script runs. The lines are " &
        "displayed, along with additional information in parentheses, " &
        "showing how SparForte interprets the commands. The line number " &
        "is displayed in square brackets after the line. trace by " &
        "itself gives the current trace status (true or false).  false " &
        "turns off tracing.  Supports AdaScript parameters." );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "type" then
     startHelp( e, "type" );
     summary( e, "type statement" );
     categoryKeyword( e );
     authorKen( e );
     description( e, "The type statement will create a new type that is " &
                  "incompatible with the original type.  It is also " &
                  "used to create array, record and enumerated types. " &
                "See also subtypes.");
     content( e, "type newtype is new [abstract|limited] oldtype [affirm...end affirm]" );
     content( e, "type newtype is ( enum1 [,enum2...] [affirm...end affirm] )" );
     content( e, "type newtype is [abstract|limited] record field1 : type1 [;field2...] end record|newtype" );
     content( e, "type newtype is [abstract|limited] array( low..high) of item_type [:= array( item,...)]" );
     seeAlso( e, "doc/ref_typedecl.html" );
     endHelp( e );
  elsif helpTopic = "types" then
     startHelp( e, "types" );
     summary( e, "standard types" );
     category( e, "reference" );
     authorKen( e );
     description( e, "These are the fundamental (and text_io package) types:" );
     content( e, "boolean","integer","natural","short_short_integer     " );
     content( e, "character","long_float","positive","socket_type" );
     content( e, "duration","long_integer","short_float","string" );
     content( e, "file_type","long_long_float","short_integer","universal_numeric" );
     content( e, "file_mode","long_long_integer","short_short_float","universal_string" );
     content( e, "float","unbounded_string","universal_typeless" );
     content( e, "complex","json_string" );
     seeAlso( e, "doc/ref_types.html" );
     endHelp( e );
  elsif helpTopic = "units" then
     helpUnits;
  elsif helpTopic = "umask" then
     startHelp( e, "umask" );
     summary( e, "umask [mask]" );
     categoryBuiltin( e );
     authorKen( e );
     description( e,
         "Set the operating system file mode creation mask." &
         " With no mask, it writes the mask value to current output." &
         " Unlike traditional Bourne shells, the value is decimal." &
         " Supports AdaScript parameters." );
     params( e, "mask - the mode bits to turn off, in range 0..511" );
     errors( e, "An exception is raised if used in a restricted shell" );
     errors( e, "An exception is raised the value is not range 0..511" );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "unset" then
     startHelp( e, "unset" );
     summary( e, "unset ident" );
     categoryBuiltin( e );
     authorKen( e );
     description( e,
         "Delete a variable, data type or other identifier.  Keywords " &
         "cannot be unset.  Supports AdaScript parameters." );
     params( e, "ident - the identifier to delete" );
     errors( e, "An exception is raised if pragma ada_95 is enforced" );
     errors( e, "An exception is raised if not used in an interactive session" );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "vectors" then
     startHelp( e, "vectors" );
     summary( e, "vectors package" );
     authorKen( e );
     categoryPackage( e );
     createdOn( e, "v2.4.2");
     description( e, "A collection of common routines for working with dynamic arrays." );
     content( e, "capacity( v )", "reserve_capacity( v, n )", "set_length( v, n )" );
     content( e, "c := length( v )", "b := is_empty( v )", "to_vector( v, e, n )" );
     content( e, "append_elements( v, e [,c] )", "b := equal( v1, v2 )", "i := first_index( v )" );
     content( e, "assign( v2, v1 )", "i := last_index( v )", "b := contains( c, e )" );
     content( e, "e := first_element( v )", "e := last_element( v )", "reverse_elements( v )" );
     content( e, "flip( v )", "prepend_elements( v, e, [,c ] )", "append_vector( v1, v2 )" );
     content( e, "first( v, c )", "next( c )", "last( v, c )" );
     content( e, "clear( v )", "previous( c )", "delete( i [, c] )" );
     content( e, "insert( v, i, e [, n] )", "insert_vector( v, c, v2 )" );
     content( e, "insert_before( v, c, e [, n] )", "insert_vector_and_mark( v, c, v2, c2 )" );
     content( e, "insert_before_and_mark( v, c, e, c2 [, n] )" );
     content( e, "insert_space( v, i [,n] )", "insert_space( v, c, c2, [, n] )" );
     content( e, "swap( v, i1, i2 | v, c1, c2 )", "find( v, e, c1, c2 )", "reverse_find( v, e, c1, c2 )" );
     content( e, "find_index( v, e, i1, i2 )", "reverse_find_index( v, e, i1, i2 )" );
     content( e, "element( c ) | ( v, i )", "delete_first( v [,n] )", "delete_last( v [,n] )"  );
     content( e, "append( v, i, s )", "prepend( v, i, s )", "increment( v, i [, n] )" );
     content( e, "decrement( v, i [, n] )", "move( v2, v1 )" );
     seeAlso( e, "doc/pkg_vectors.html" );
     endHelp( e );
  elsif helpTopic = "wait" then
     startHelp( e, "wait" );
     summary( e, "wait" );
     categoryBuiltin( e );
     authorKen( e );
     description( e,
        "Stop execution and wait for all background commands to finish and " &
        "return the exit status of the last command." );
     seeAlsoShellCmds( e );
     endHelp( e );
  elsif helpTopic = "while" then
     startHelp( e, "while" );
     summary( e, "while statement" );
     description( e, "Loop as long as an expression remains true." );
     categoryKeyword( e );
     authorKen( e );
     content( e, "while expression loop ...end loop" );
     seeAlso( e, "doc/ref_flow.html" );
     endHelp( e );
  elsif helpTopic = "expressions" then
     discardUnusedIdentifier( token );
     startHelp( e, "expressions" );
     summary( e, "expressions operators and operands" );
     authorKen( e );
     category( e, "reference" );
     description( e, "These are the expression operators and operands:" );
     section( e, "Uniary Operators" );
     content( e, "+ - uniary plus" );
     content( e, "- - uniary minus" );
     content( e, "not - uniary not" );
     section( e, "Exponents" );
     content( e, "** - exponentiation" );
     section( e, "Mathematics" );
     content( e, "* - multiplication","and - bitwise and" );
     content( e, "/ - division","or  - bitwise or" );
     content( e, "& - string concatenation","xor - bitwise xor" );
     content( e, "+ - addition","- - subtraction" );
     section( e, "Relations" );
     content( e, "= - equals", "/= - not equals", "> - greater than" );
     content( e, ">= - greater than or equals", "< - less than" );
     content( e, "<= - less than or equals", "in - in a range" );
     content( e, "not in - not in a range" );
     section( e, "Boolean Operators" );
     content( e, "and - boolean and", "or - boolean or", "xor - boolean exclusive or" );
     section( e, "Operands" );
     content( e, "@ - reflexive operand (itself)","% - last output" );
     seeAlso( e, "doc/ref_assign.html and doc/ref_specials.html" );
     endHelp( e );
  elsif helpTopic = "numerics" then
     discardUnusedIdentifier( token );
     helpNumerics;
  elsif helpTopic = "pen" then
     startHelp( e, "pen" );
     summary( e, "pen package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using 2-D drawing." );
     content( e, "set_rect(r, l, t, r, b)","b := is_empty_rect( r )","offset_rect( r, dx, dy )" );
     content( e, "inset_rect( r, dx, dy )","intersect_rect( r,r1,r2)","b := inside_rect( ir, or )");
     content( e, "b := in_rect( x, y, r )");
     content( e, "frame_ellipse( id, r )","frame_rect( id, r )","fill_ellipse( id, r )");
     content( e, "paint_rect( id, r )","fill_rect(id,rct,r,g,b)","fill_rect(id,r,cn)");
     content( e, "paint_ellipse( id, r )");
     content( e, "line_to( id, x, y )","line( id, dx, dy )","hline( id, x1, x2, y )");
     content( e, "vline( id, x, y1, y2 )","move_to( id, x, y )","move( id, dx, dy )");
     content( e, "clear","clear( r, g, b)","clear( cn )");
     content( e, "get_pen_mode( id )","get_pen_brush( id )","set_pen_ink(id,r,g,b)    " );
     content( e, "set_pen_ink(id,cn)","set_pen_mode( id, m)","set_pen_pattern( id,pid) " );
     content( e, "set_pen_brush( id,brush )","set_font( c, f, p )*","put( c, s )*");
     content( e, "p := greyscale( r,g,b)","blend(r1,g1,b1,r2,g2,b2,r,g,b)","fade(r1,g1,b1,p,r,g,b)");
     content( e, "new_canvas(h,v,c,id)","new_screen_canvas(h,v,c,id)","new_window_canvas(h,v,c,id)");
     content( e, "new_canvas(p,id)","new_gl_screen_canvas(h,v,c,id)","save_canvas(p,id)");
     content( e, "close_canvas( id )","new_gl_window_canvas(h,v,c,id)","wait_to_reveal( id )" );
     content( e, "reveal( id )","reveal_now( id )");
     seeAlso( e, "doc/pkg_pen.html" );
     footer( e, "* - not finished" );
     endHelp( e );
  elsif helpTopic = "pen.gl" then
     startHelp( e, "pen" );
     summary( e, "pen package (OpenGL Functions)" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using 3-D drawing.  See also pen." );
     content( e, "This section is under construction." );
     seeAlso( e, "doc/pkg_pengl.html" );
     footer( e, "* - not finished" );
     endHelp( e );
  elsif helpTopic = "policy" then
     startHelp( e, "policy" );
     summary( e, "policy block" );
     categoryKeyword( e );
     description( e, "Begin a new architecture block.  It may only contain pragmas and static if or case statements.  Policy blocks can automatically load from a global file (/etc/sparforte_policy), which is done before a global configuration file." );
     content( e, ( "policy name is ... end name;" ) );
     seeAlso( e, "doc/ref_subprograms.html" );
     endHelp( e );
     authorKen( e );
  elsif helpTopic = "step" then
     startHelp( e, "step" );
     summary( e, "step" );
     categoryBuiltin( e );
     authorKen( e );
     description( e, "On --break breakout mode, run one instruction and stop." );
     seeAlso( e, "doc/tutorial_9.html" );
     endHelp( e );
  elsif helpTopic = "strings" then
     startHelp( e, "strings" );
     summary( e, "strings package" );
     authorKen( e );
     categoryPackage( e );
     description( e, "A collection of common routines using strings." );
     content( e, "n := count( s, p )","r := csv_field( s, c [, d [, q]] )","r := csv_replace( s, f, t, [, d [, q]] )" );
     content( e, "r := delete( s, l, h )","c := element( s, p )","r := field( s, c [, d] )" );
     content( e, "b := glob( e, s )","r := head( s, c [, p] )" );
     content( e, "r := image( n )","n := index( s, p [, d] )","n := index_non_blank( s [,d] )" );
     content( e, "r := insert( s, b, n )","r := is_alphanumeric( s )","r := is_basic( s )" );
     content( e, "r := is_control( s )","r := is_digit( s )","r := is_fixed( s )" );
     content( e, "r := is_graphic( s )","r := is_hexadecimal_digit(s)","r := is_letter( s )" );
     content( e, "r := is_lower( s )","r := is_slashed_date( s )","r := is_special( s )" );
     content( e, "b := is_typo_of( s1, s2 )","r := is_upper( s )","n := length( s )" );
     content( e, "r := lookup( s, k [, d] )","b := match( e, s )","r := mktemp( p )" );
     content( e, "r := overwrite( s, p, n )","replace( s, f, t [, d] )","r := replace_slice( s, l, h, b )" );
     content( e, "set_unbounded_string( u, s )","r := slice( s, l, h )","split( s, l, r , p )" );
     content( e, "r := tail( s, c [, p] )","r := to_base64( s )","r := to_basic( s )" );
     content( e, "r := to_escaped( s )","r := to_json( s )","r := to_lower( s )" );
     content( e, "r := to_proper( s )","r := to_string( s )","r := to_upper( s )" );
     content( e, "u := to_unbounded_string( s )","r := trim( s [, e] )","r := unbounded_slice(s, l, h)" );
     content( e, "s := to_hexadecimal_digits( n )", "c := val( n )","n := levenshtein( s1, s2 )" );
     content( e, "r := soundex( s )", "r := replace_all( s, n, t [, c] )", "b := starts_with( s, t [, c] )" );
     content( e, "b := ends_with( s, t [,c] )", "n := word_count( s )", "i := compare( s, t [, c [, l] ] )" );
     content( e, "n := index_set( s, t [, f [, m [, d] ] ] )" );
     discardUnusedIdentifier( token ); -- TODO: should this always be done automatically?
     seeAlso( e, "doc/pkg_strings.html" );
     endHelp( e );
  elsif helpTopic = "variables" then
     discardUnusedIdentifier( token );
     startHelp( e, "variables" );
     summary( e, "variables declarations" );
     authorKen( e );
     category( e, "reference" );
     description( e, "Variables are declared with a data type and an " &
       "optional usage qualifier (such as constant).  A constant " &
       "specification is a constant without a value.  A specification " &
       "must be completed later with a full definition." );
     content( e, "var [,var2...] : [constant|limited] type [:= expression]" );
     content( e, "var : [constant] type renames var2[(element)]" );
     content( e, "var : [constant] type copies var2[(element)]" );
     content( e, "array_var :  [limited] array( low..high) of item-type [ := array | (item,...) ]" );
     content( e, "array_var :  array_type [ := array | (item,...) ]" );
     content( e, "var : constant type;" );
     seeAlso( e, "doc/ref_typedecl.html, doc/ref_enum.html, doc/ref_arrays.html and doc/ref_records.html" );
     endHelp( e );
  elsif helpTopic = "with" then
     startHelp( e, "with" );
     summary( e, "with separate" );
     authorKen( e );
     categoryKeyword( e );
     description( e, "A separate declaration file is the SparForte " &
        "equivalent of an include file. It is an set of declaration " &
        "statements stored in a separate file. The file can include " &
        "variables, constants, types, functions, procedures, pragmas, " &
        "etc. A declaration file is used to share common declarations " &
        "across many scripts." );
     params( e, "path - the path to the include file" );
     content( e, "with separate " & Ada.Characters.Latin_1.Quotation &
        "path" & Ada.Characters.Latin_1.Quotation );
     seeAlso( e, "doc/ref_subprograms.html" );
     endHelp( e );
  elsif helpTopic = "?" then
     startHelp( e, "?" );
     summary( e, "question mark (text_io package)" );
     authorKen( e );
     categoryBuiltin( e );
     description( e, "Write a value to current output in human-readable " &
        "format and start a new line." );
     params( e, "expression - the value to display" );
     content( e, "? expression" );
     seeAlso( e, "doc/pkg_text_io.html" );
     endHelp( e );
  else
     DoScriptHelp( helpTopic );
  end if;
  if not isEmpty( e ) then
     start( r.all );
  -- TODO: this need to be fixed with a class interface or something
   if HTMLoutput then
     render( longHtmlHelpReport( r.all ), e );
   elsif ManOutput then
     render( longManPageHelpReport( r.all ), e );
   else
     render( longHelpReport( r.all ), e );
   end if;
     finish( r.all );
  end if;
  free( r );
-- getNextToken;
end help;

end builtins.help;

