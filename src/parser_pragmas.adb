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
    bush_os.sound,
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
    parser;
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
    bush_os.sound,
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
    parser;

package body parser_pragmas is

type aPragmaKind is ( ada_95, asserting, annotate, debug, debug_on,
     depreciated, export, gcc_errors, import, inspection, inspect_var,
     license, noCommandHash, peek, promptChange, register_memcache_server,
     restriction, restriction_auto, restriction_external,
     restriction_mysql, restriction_postgresql, template,
     unchecked_import, uninspect_var, unrestricted_template, volatile );

--  PARSE PRAGMA KIND
--
-- Check the current token for the kind of pragma and advance the scanner.
-----------------------------------------------------------------------------

function parsePragmaKind return aPragmaKind is
  name : string := to_string( identifiers( token ).name );
  pragmaKind : aPragmaKind;
begin
  if name = "ada_95" then
     pragmaKind := ada_95;
  elsif name = "assert" then
     pragmaKind :=  asserting;
  elsif name = "debug" then
     pragmaKind :=  debug;
  elsif name = "annotate" then
     pragmaKind :=  annotate;
  elsif name = "deprecated" then
     pragmaKind :=  depreciated;
  elsif name = "depreciated" then
     pragmaKind :=  depreciated;
  elsif name = "export" then
     pragmaKind := export;
  elsif name = "gcc_errors" then
     pragmaKind := gcc_errors;
  elsif name = "import" then
     pragmaKind := import;
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
  elsif name = "register_memcache_server" then
     pragmaKind := register_memcache_server;
  elsif name = "restriction" then
     pragmaKind := restriction;
  elsif name = "restrictions" then
     discardUnusedIdentifier( token );
     err( "pragma restriction not restrictions" );
  elsif name = "template" then
     pragmaKind := template;
  elsif name = "unchecked_import" then
     pragmaKind := unchecked_import;
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
  if token /= strlit_t then
     if name /= "author" and
        name /= "created" and
        name /= "description" and
        name /= "errors" and
        name /= "modified" and
        name /= "param" and
        name /= "return" and
        name /= "see also" and
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
     importKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  elsif name = "memcache" then
     importKind := name_unbounded;
     discardUnusedIdentifier( token );
     getNextToken;
     expect( symbol_t, "," );
     ParseIdentifier( var_id );
  else
     importKind := null_unbounded_string;
     discardUnusedIdentifier( token );
     err( "only 'shell' or 'cgi' convention supported" );
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
  else
     exportKind := null_unbounded_string;
     discardUnusedIdentifier( token );
     err( "only 'shell', 'local_memcache', 'memcache' convention supported" );
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


--  PARSE PRAGMA
--
-- Syntax: pragma kind [params]
-----------------------------------------------------------------------------

procedure ParsePragma is
  pragmaKind  : aPragmaKind;
  expr_val    : unbounded_string;
  expr_val2   : unbounded_string;
  results     : unbounded_string;
  var_id      : identifier;
  exportType  : unbounded_string;
  importType  : unbounded_string;
begin
  expect( pragma_t );

  -- examine the name of the pragma and return a pragma kind matching the name

  pragmaKind := parsePragmaKind;
-- DEBUG returns

  -- Parse the pragma parameters (if any)

  if pragmaKind /= ada_95 and pragmaKind /= inspection and pragmaKind /=
     noCommandHash and pragmaKind /= peek then
     if pragmaKind = debug and (token /= symbol_t or identifiers( token ).value /= "(") then
        pragmaKind := debug_on;
     else
        expect( symbol_t, "(" );
     end if;
  end if;

  case pragmaKind is
  when ada_95 =>                             -- pragma ada_95
     null;
  when asserting =>                          -- pragma assert
     ParseExpression( expr_val, var_id );
  when annotate =>                           -- pragma annotate
     ParseAnnotateKind;
  when debug =>                              -- pragma debug
     expr_val := identifiers( token ).value;
     expect( backlit_t );
  when debug_on =>                              -- pragma debug (no param)
     null;
  when depreciated =>                           -- pragma depreciated
     expr_val := identifiers( token ).value;
     expect( strlit_t );
  when export =>                                -- pragma export
     ParseExportKind( var_id, exportType );
  when import | unchecked_import =>             -- pragma unchecked/import
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
     elsif identifiers( token ).name = "no_external_commands" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_external;
     elsif identifiers( token ).name = "no_mysql_database" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_mysql;
     elsif identifiers( token ).name = "no_postgresql_database" then
        discardUnusedIdentifier( token );
        getNextToken;
        pragmaKind := restriction_postgresql;
     else
        discardUnusedIdentifier( token );
        err( "unknown restriction" );
        return;
     end if;
  when inspect_var =>                        -- pragma inspect
     ParseIdentifier( var_id );
  when license =>                            -- pragma license
     ParseLicenseKind( expr_val );
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
        else
           var_id := eof_t;
        end if;
     end if;
     gccOpt := true;
  when uninspect_var =>                      -- pragma uninspect
     ParseIdentifier( var_id );
  when volatile =>                           -- pragma volatile
     ParseIdentifier( var_id );
  when others =>
     err( "Internal error: can't handle pragma" );
  end case;

  if pragmaKind /= ada_95 and pragmaKind /= inspection and pragmaKind /=
     noCommandHash and pragmaKind /= debug_on and pragmaKind /= peek then
     expect( symbol_t, ")" );
  end if;

  -- Execute the pragma

  if isExecutingCommand then
     case pragmaKind is
     when ada_95 =>
        onlyAda95 := true;
     when asserting =>
        if debugOpt then
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
     when debug =>
        if debugOpt then
           if not syntax_check then
              declare
                 savershOpt : commandLineOption := rshOpt;
              begin
                 rshOpt := true;            -- force restricted shell mode
                 CompileRunAndCaptureOutput( expr_val, results );
                 rshOpt := savershOpt;
                 put( results );
              end;
           end if;
        end if;
     when debug_on =>
        debugOpt := true;
     when depreciated =>
        -- later, this should create a list of depreciation message
        -- for now, only the entire script is depreciated
        depreciatedMsg := "This script made obsolete by " & expr_val & '.';
     when export =>
        if identifiers( var_id ).class = userProcClass then
           err( "procedures cannot be exported" );
        elsif identifiers( var_id ).class = userFuncClass then
           err( "functions cannot be exported" );
        elsif identifiers( var_id ).list then
           err( "arrays cannot be exported" );
        elsif identifiers( var_id ).export then
           err( "variable is already exported" );
        elsif uniTypesOK( identifiers( var_id ).kind, uni_string_t ) then
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
           else
              identifiers( var_id ).method := shell;
           end if;
        end if;
     when gcc_errors =>
        gccOpt := true;
     when import =>
        if identifiers( var_id ).class = userProcClass then
           err( "procedures cannot be imported" );
        elsif identifiers( var_id ).class = userFuncClass then
           err( "functions cannot be imported" );
        elsif identifiers( var_id ).list then
           err( "arrays cannot be imported" );
        elsif identifiers( var_id ).import then
           err( "variable is already imported" );
        elsif uniTypesOK( identifiers( var_id ).kind, uni_string_t ) then
           identifiers( var_id ).import := true;
           if importType = "local_memcache" then
              identifiers( var_id ).method := local_memcache;
              checkAndInitializeLocalMemcacheCluster;
              begin
                 Get( localMemcacheCluster,
                      identifiers( var_id ).name,
                      identifiers( var_id ).value );
                 if length( identifiers( var_id ).value ) = 0 then
                    err( "unable to find variable " &
                         to_string( identifiers( var_id ).name ) &
                         " in the local memcache" );
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
                      identifiers( var_id ).value );
                 if length( identifiers( var_id ).value ) = 0 then
                    err( "unable to find variable " &
                         to_string( identifiers( var_id ).name ) &
                         " in the memcache" );
                 end if;
              exception when others =>
                 err( "exception raised" );
              end;
           elsif processingTemplate and importType = "cgi" then
              identifiers( var_id ).method := http_cgi;
              identifiers( var_id ).value := null_unbounded_string;
              for i in 1..cgi.key_count( to_string( identifiers( var_id ).name ) ) loop
                  identifiers( var_id ).value := identifiers( var_id ).value &
                     to_unbounded_string( cgi.value( to_string( identifiers( var_id ).value ), 1, true ) );
               end loop;
           elsif not processingTemplate and importType = "cgi" then
               err( "import type cgi must be used in a template" );
           else
              identifiers( var_id ).method := shell;
              refreshVolatile( var_id );
           end if;
           if trace then
               put_trace(
                  to_string( identifiers( var_id ).name ) & " := """ &
                  to_string( ToEscaped( identifiers( var_id ).value ) ) &
                  """" );
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
     when restriction_external =>
        restriction_no_external_commands := true;
     when restriction_mysql =>
        restriction_no_mysql_database := true;
     when restriction_postgresql =>
        restriction_no_postgresql_database := true;
     when promptChange =>
        promptScript := expr_val;
     when template | unrestricted_template =>
        templateType := noTemplate;
        if expr_val = "html" then
           templateType := htmlTemplate;
        elsif expr_val = "text" then
           templateType := textTemplate;
        else
           err( "unknown template type" );
        end if;
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
        -- Always output CGI header as soon as possible to avoid a HTTP/500
        -- error.  Give the web server a minimal header to return to the
        -- browser in case something goes wrong later that would prevent
        -- the header from being sent.
        if templateType = htmlTemplate then
           cgi.put_cgi_header( "Content-type: text/html" );
        elsif templateType = textTemplate then
           cgi.put_cgi_header( "Content-type: text/plain" );
        end if;
     when unchecked_import =>
        if identifiers( var_id ).class = userProcClass then
           err( "procedures cannot be imported" );
        elsif identifiers( var_id ).class = userFuncClass then
           err( "functions cannot be imported" );
        elsif identifiers( var_id ).list then
           err( "arrays cannot be imported" );
        elsif identifiers( var_id ).import then
           err( "variable is already imported" );
        elsif uniTypesOK( identifiers( var_id ).kind, uni_string_t ) then
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
                    identifiers( var_id ).value := temp;
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
                    identifiers( var_id ).value := temp;
                 end if;
              exception when others =>
                 err( "exception raised" );
              end;
           elsif processingTemplate and importType = "cgi" then
              if inEnvironment( var_id ) then
                 identifiers( var_id ).import := true;
                 identifiers( var_id ).method := http_cgi;
                 identifiers( var_id ).value := null_unbounded_string;
                 for i in 1..cgi.key_count( to_string( identifiers( var_id ).name ) ) loop
                     identifiers( var_id ).value := identifiers( var_id ).value &
                        to_unbounded_string( cgi.value( to_string( identifiers( var_id ).value ), 1, false) );
                  end loop;
              end if;
           elsif importType = "cgi" then
              err( "import type cgi must be used in a template" );
           else
              if inEnvironment( var_id ) then
                 identifiers( var_id ).import := true;
                 identifiers( var_id ).method := shell;
                 refreshVolatile( var_id );
              end if;
           end if;
           if trace then
              put_trace(
                 to_string( identifiers( var_id ).name ) & " := """ &
                 to_string( ToEscaped( identifiers( var_id ).value ) ) &
                 """" );
           end if;
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
end ParsePragma;

end parser_pragmas;
