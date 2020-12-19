------------------------------------------------------------------------------
-- Lexical Scanner (the thing that reads your source code)                  --
-- Also, the semantic stuff.                                                --
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
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------
pragma ada_2005;

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );

with system,
    ada.text_io,
    ada.strings.fixed,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    gnat.source_info,
    spar_os.tty,
    pegasock.memcache,
    user_io,
    script_io,
    string_util,
    software_models,
    performance_monitoring,
    scanner_res,
    --TODO: I need to fix this circular dependency between the scanner and
    -- parser.  That is, move start/shutdown to separate packages.
    parser.decl.as,
    parser_pragmas,
    parser_os,
    parser_arrays,
    parser_enums,
    parser_records,
    parser_files,
    parser_lock,
    parser_cmd,
    parser_cgi,
    parser_units,
    parser_cal,
    parser_db,
    parser_dbm,
    parser_mysql,
    parser_mysqlm,
    parser_numerics,
    parser_strings,
    parser_stats,
    parser_tio,
    parser_pen,
    parser_sound,
    parser_dirops,
    parser_memcache,
    parser_gnat_crc,
    parser_gnat_cgi,
    parser_exceptions,
    parser_chains,
    parser_containers,
    parser_vectors,
    parser_doubly,
    parser_dht,
    parser_teams,
    parser_sessions,
    parser_bdb,
    parser_btree_io,
    parser_hash_io,
    parser_templates,
    parser_tinyserve,
    parser_logs;
use ada.text_io,
    ada.command_line,
    ada.command_line.environment,
    ada.strings.fixed,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    spar_os,
    spar_os.tty,
    pegasock.memcache,
    user_io,
    script_io,
    string_util,
    software_models,
    performance_monitoring,
    scanner_res,
    parser,
    parser.decl.as,
    parser_pragmas,
    parser_os,
    parser_arrays,
    parser_enums,
    parser_records,
    parser_files,
    parser_lock,
    parser_cmd,
    parser_cgi,
    parser_units,
    parser_cal,
    parser_db,
    parser_dbm,
    parser_mysql,
    parser_mysqlm,
    parser_numerics,
    parser_strings,
    parser_stats,
    parser_tio,
    parser_pen,
    parser_sound,
    parser_dirops,
    parser_memcache,
    parser_gnat_crc,
    parser_gnat_cgi,
    parser_exceptions,
    parser_chains,
    parser_containers,
    parser_vectors,
    parser_doubly,
    parser_dht,
    parser_teams,
    parser_sessions,
    parser_bdb,
    parser_btree_io,
    parser_hash_io,
    parser_templates,
    parser_tinyserve,
    parser_logs;

package body scanner is

-----------------------------------------------------------------------------
--  PUT TOKEN
--
-- For debugging, show the current token, its value, type and properities.
-----------------------------------------------------------------------------

procedure Put_Token is
begin
  -- show name
  Put( "'" & toEscaped( identifiers( token ).name ) & "'" );
  -- show parser status
  if done then
     Put( " [DONE]" );
  end if;
  if done_sub then
     Put( " [SUB]" );
  end if;
  if error_found then
     Put( " [ERR]" );
  end if;
  if exit_block then
     Put( " [EXITBLK]" );
  end if;
  if syntax_check then
     Put( " [SYN]" );
  end if;
  New_Line;
  -- show details
  Put_Line( "    Symbol Table pos =" & token'img );
  Put_Line( "    Instruction counter first/current/last =" &
    firstPos'img & "/" & cmdpos'img & "/" & lastPos'img );
  if not identifiers( token ).kind'valid then
     Put_Line( "    Token type = OUT-OF-RANGE VALUE" & identifiers( token ).kind'img );
  else
    begin
       Put_Line( "    Token type = " & identifiers( identifiers( token ).kind ).name );
    exception
    when constraint_error =>
      put_line( standard_error, "put_token: constraint_error raised on token type" );
     end;
  end if;
  if identifiers( token ).value = null then
     Put_Line( "    Token value = null" );
  else
     Put_Line( "    Token value = '" & ToEscaped( identifiers( token ).value.all ) & "'" );
  end if;
  Put( "    Token properties = " );
  if identifiers( token ).import then
     Put( "import " );
  end if;
  if identifiers( token ).export then
     Put( "export " );
  end if;
  if identifiers( token ).inspect then
     Put( "inspected " );
  end if;
  if identifiers( token ).list then
     Put( "array" );
  end if;
  if identifiers( token ).field_of /= eof_t and identifiers( token ).class = subClass then
     Put( "field of record " );
     begin
       put( identifiers( identifiers( token ).field_of ).name );
     exception when others =>
       put( "unknown" );
     end;
  end if;
  if identifiers( token ).field_of /= eof_t and identifiers( token ).usage = constantUsage then
     Put( "formal parameter of " );
     begin
       put( identifiers( identifiers( token ).field_of ).name );
     exception when others =>
       put( "unknown" );
     end;
  end if;
  if identifiers (token ).renaming_of /= identifiers'first then
     put( "renaming of" & to_string( identifiers( identifiers( token ).renaming_of ).name ) );
  end if;
  if identifiers (token ).renamed_count /= 0 then
     put( "renamed" & identifiers( token ).renamed_count'img & " times" );
  end if;
  if is_keyword( token ) then
     Put( "(reserved keyword) " );
  end if;
  New_Line;
exception
  when constraint_error =>
    put_line( standard_error, "put_token: constraint_error raised" );
  when storage_error =>
    put_line( standard_error, "put_token: storage_error raised" );
end Put_Token;


-----------------------------------------------------------------------------
--  PUT IDENTIFIER ATTRIBUTES
--
-- Show an identifier's attributes (that is, it's type).  Shared by
-- put_identifier and put_all_identifiers.
-----------------------------------------------------------------------------

procedure put_identifier_attributes( id : identifier ) is
  ident : declaration renames identifiers( id );              -- the identifier
  kind  : declaration renames identifiers( ident.kind );      -- and its type
begin
  -- if a renaming, nothing more...
  -- TODO: depends on what kind of renaming because params can be renamed
  if ident.renaming_of /= identifier'first then
     put( "renaming of " &
        to_string( identifiers( ident.renaming_of ).name ) );
     return;
  end if;

     if ident.import then
        put( "imported " );
     end if;
     if ident.export then
        put( "exported " );
     end if;
     if ident.volatile = checked then
        put( "volatile " );
     elsif ident.volatile = unchecked then
        put( "unchecked volatile " );
     end if;
     if ident.inspect then
        put( "inspected " );
     end if;
     if ident.resource then
        put( "resource " );
     end if;

     -- if imported or exported, show the method

     if ident.import or ident.export then
        case ident.method is
        when http_cgi       => put( "HTTP CGI " );
        when local_memcache => put( "local memcache " );
        when memcache       => put( "memcache " );
        when shell          => put( "shell environment " );
        when session        => put( "session " );
        when others         => put( "unknown " );
        end case;
        if ident.mapping = json then
           put( "json " );
        end if;
     end if;

     -- Parameters and Passing Mode

     if ident.passingMode = in_mode then
           put( "in mode " );
     elsif ident.passingMode = out_mode then
           put( "out mode " );
     elsif ident.passingMode = in_out_mode then
           put( "in out mode " );
     end if;

     -- Show the class (type, constant, etc.)

     case ident.class is
     when subClass =>
        if ident.field_of = eof_t then
           put( "subtype of " );
        end if;
        -- abstract/limited are the type itself, not its parent
        case ident.usage is
        when abstractUsage =>
           put( "abstract " );
        when limitedUsage =>
           put( "limited " );
        when constantUsage =>
           put( "constant " );
        when fullUsage =>
           null;
        when others =>
           err( gnat.source_info.source_location & ": internal error: unexpected usage qualifier " & ident.usage'img );
        end case;
     when typeClass =>
        if not ident.list then
           put( "new type of " );
        end if;
        -- abstract/limited are the type itself, not its parent
        case ident.usage is
        when abstractUsage =>
           put( "abstract " );
        when limitedUsage =>
           put( "limited " );
        when constantUsage =>
           put( "constant " );
        when fullUsage =>
           null;
        when others =>
           err( gnat.source_info.source_location & ": internal error: unexpected usage qualifier " & ident.usage'img );
        end case;
     when funcClass =>
        put( "built-in " );
        if ident.usage = abstractUsage then
           put( "abstract " );
        end if;
        put( "function " );
     when procClass =>
        put( "built-in " );
        if ident.usage = abstractUsage then
           put( "abstract " );
        end if;
        put( "procedure " );
     when userProcClass =>
        if ident.usage = abstractUsage then
           put( "abstract " );
        end if;
        put( "procedure " );
     when userFuncClass =>
        if ident.usage = abstractUsage then
           put( "abstract " );
        end if;
        put( "function return " );
     when mainProgramClass =>
        put( "main program " );
     when exceptionClass =>
        put( "exception " );
     when namespaceClass =>
        put( "namespace " );
     when enumClass =>
        put( "enumerated item of the type " );
     when policyClass =>
        put( "policy block " );
     when configurationClass =>
        put( "configuration block " );
     when genericTypeClass =>
        put( "generic type using " );
     when others =>
        case ident.usage is
        when abstractUsage =>
           put( "abstract " );
        when limitedUsage =>
           put( "limited " );
        when constantUsage =>
           put( "constant " );
        when fullUsage =>
           null;
        when others =>
           err( gnat.source_info.source_location & ": internal error: unexpected usage qualifier " & ident.usage'img );
        end case;
        put( "identifier of the type " );
     end case;

     -- Usage qualifier

     case kind.usage is
     when limitedUsage =>
        put( "limited " );
     when constantUsage =>
        put( "constant " );
     when fullUsage =>
        null;
     when others =>
        -- bastract should not occur
        err( gnat.source_info.source_location & ": internal error: unexpected usage qualifier " & ident.usage'img );
     end case;

     -- Show the type
     -- Failsafe: shouldn't be keyword, but just in case.

     if ident.kind = keyword_t then
        if ident.class /= funcClass and
           ident.class /= procClass and
           ident.class /= userProcClass and
           ident.class /= userFuncClass and
           ident.class /= mainProgramClass and
           ident.class /= namespaceClass then
           put( "keyword" );
        end if;
     elsif ident.class = exceptionClass then
        null;
     elsif ident.class = namespaceClass then
        null;
     elsif ident.class = policyClass then
        null;
     elsif ident.class = genericTypeClass then
        null;
     else
        if kind.name = "an anonymous array" then
           -- special handling since they're not easily visible
           put( "anonymous array" );
           begin
             -- put( firstBound( arrayID( to_numeric( kind.value ) ) )'img );
             put( identifiers( getBaseType( ident.kind ) ).firstBound'img );
           exception when others =>
             put( " unknown" );
           end;
           put( " .." );
           begin
             -- put( lastBound( arrayID( to_numeric( kind.value ) ) )'img );
             put( identifiers( getBaseType( ident.kind ) ).lastBound'img );
           exception when others =>
             put( " unknown" );
           end;
           put( " of " );
           if ident.kind = eof_t then
              put( " unknown" );
           else
              put( identifiers( identifiers( ident.kind ).kind ).name );
           end if;

        elsif ident.field_of /= eof_t and ident.class = subClass then
           put( "field of record type " );
           put( identifiers( ident.field_of ).name );
           put( " of type " );
           put( kind.name );
        elsif ident.field_of /= eof_t and ident.usage = constantUsage then
           put( "formal parameter of " );
           put( identifiers( ident.field_of ).name );
           put( " of type " );
           put( kind.name );
        elsif ident.list and (ident.class = typeClass or ident.class = subClass) then
           put( "array" );
           declare
              base : identifier := id;
           begin
              -- with an array, the kind is the element type.  Subclasses are renamings.
              -- bounds are attached to the base type only.
              if ident.class = subClass then
                 base := getBaseType( id );
              end if;
             --put( firstBound( arrayID( to_numeric( ident.value ) ) )'img );
              put( identifiers( base ).firstBound'img );
           exception when others =>
             put( " unknown" );
           end;
           put( " .." );
           declare
              base : identifier := id;
           begin
              -- with an array, the kind is the element type.  Subclasses are renamings.
              -- bounds are attached to the base type only.
              if ident.class = subClass then
                 base := getBaseType( id );
              end if;
             --put( lastBound( arrayID( to_numeric( ident.value ) ) )'img );
              put( identifiers( base ).lastBound'img );
           exception when others =>
             put( " unknown" );
           end;
           put( " of " );
           if ident.kind = eof_t then
              put( " unknown" );
           else
              put( kind.name );
           end if;
        elsif ident.kind = root_record_t then -- base record type
           put( "record with " & ident.value.all & " fields" );
        elsif ident.kind = procedure_t then -- procedure's "kind" is procedure
           null;
        else
           if ident.kind = eof_t then
              put( " unknown" );
           else
              put( kind.name );
              -- If it's a generic type, show the instantiated types
              declare
                 baseType : identifier;
              begin
                 -- Don't look up the base type on a universal or a keyword.
                 if ident.class = subClass then
                    baseType := getBaseType( ident.kind );
                 else
                    basetype := ident.kind;
                 end if;
                 if identifiers( baseType ).class = genericTypeClass then
                    put( "(" );
                    put( to_string( identifiers( ident.genKind ).name ) );
                    if ident.genKind2 /= eof_t then
                       put( ", " );
                       put( to_string( identifiers( ident.genKind2 ).name ) );
                    end if;
                    put( ")" );
                 end if;
              end;
           end if;
        end if;
        --if identifiers( ident.kind ).kind = root_enumerated_t then
        --   put( " enumerated item" );
        --end if;
     end if;
end put_identifier_attributes;


-----------------------------------------------------------------------------
--  PUT IDENTIFIER
--
-- Show an identifier's name, value, attributes and type on standard output.
-- Used by env command and for debugging.
-----------------------------------------------------------------------------

procedure Put_Identifier( id : identifier ) is
  ident : declaration renames identifiers( id );              -- the identifier
begin
  if ident.deleted then
     put_line( "This identifier has been deleted" );
  else

     -- Is it a reserved keyword?  then nothing else to report

     if id <= reserved_top-1 then
        put_line( " ( AdaScript reserved word )" );
        return;
     end if;

     -- Enumerated?  this is prettier than "new type of root_enumerated"
     if ident.kind = root_enumerated_t then
        put_line( " ( enumerated type )" );
        return;
     elsif ident.kind = variable_t then
        put_line( " ( private type )" );
        return;
     end if;

     -- Show the value of the variable

      if ident.kind /= keyword_t then
        put( ident.name );
        if ident.renaming_of /= identifier'first then
            null;
        elsif not ident.list and ident.kind /= root_record_t and ident.class /= exceptionClass then
            put( " := " );
            if ident.class = userProcClass then
                -- this appears first because getBaseType will fail on a
                -- procedure
                put( '"' );
                put( ToEscaped( ident.value.all ) );
                put( '"' );
                -- (should really used root type to determine quoting)
             elsif identifiers( getBaseType( ident.kind ) ).kind = root_record_t then
                put( "(" );
                declare
                   field_id  : identifier;
                   numFields : natural;
                begin
                   numFields := natural( to_numeric( identifiers( getBaseType( ident.kind ) ).value.all ) );
                   for i in 1..numFields loop
                       findField( id, i, field_id );
                       put( delete( identifiers( field_id ).name, 1, length( ident.name ) + 1 ) ); -- skip record name + '.'
                       put( " =>" );
                       put( ToEscaped( identifiers( field_id ).value.all ) );
                       if i /= numFields then
                          put( "," );
                       end if;
                   end loop;
                end;
                put( ")" );
            elsif ident.class = userFuncClass then
                put( '"' );
                put( ToEscaped( ident.value.all ) );
                put( '"' );
            elsif ident.kind = string_t then
                put( '"' );
                put( ToEscaped( ident.value.all ) );
                put( '"' );
            elsif ident.kind = character_t then
                put( "'" );
                put( ToEscaped( ident.value.all ) );
                put( "'" );
            elsif ident.class = enumClass then
                put( '"' );
                put( ToEscaped( ident.value.all ) );
                put( '"' );
            elsif getUniType( ident.kind ) = root_enumerated_t then
                for i in identifiers'first..identifiers_top-1 loop
                    if identifiers( i ).kind = ident.kind then
                       if identifiers( i ).class = enumClass then
                          if identifiers( i ).value = ident.value then
                             put( ToEscaped( identifiers( i ).name ) );
                             exit;
                          end if;
                       end if;
                    end if;
               end loop;
            elsif not ident.list then
                put( ToEscaped( ident.value.all ) );
            end if;
        end if;
     end if;

     -- Show the attributes

     put( "; -- " );
     put_identifier_attributes( id );
     new_line;

     -- if a renaming, use recusion

     if ident.renaming_of /= identifier'first then
        put_identifier( ident.renaming_of );
     end if;
  end if;
end Put_Identifier;


-----------------------------------------------------------------------------
--  PUT ALL IDENTIFIERS
--
-- Show all the identifiers in the symbol table.  Use a tabular layout.
-----------------------------------------------------------------------------

procedure put_all_identifiers is
   maxNameWidth  : constant natural := 20;
   maxValueWidth : constant natural := natural( displayInfo.col ) / 3;
   escapedValue  : unbounded_string;
   round         : natural;
   firstChar     : natural;
   lastChar      : natural;
begin
  -- Show all the information in a table
  for i in 1..identifiers_top-1 loop
      if not identifiers( i ).deleted and identifiers( i ).class /= namespaceClass then
         round := 1;
         firstChar := 0;
         lastChar := 0;
         if identifiers( i ).kind /= keyword_t and not identifiers( i ).list then
            escapedValue := toEscaped( identifiers( i ).value.all );
         else
            escapedValue := null_unbounded_string;
         end if;
         -- break up output into chunks of the column width
         loop
           if round = 1 then
              --put_line( "A" );
              --put_line( identifiers( i ).name );
              if length( identifiers( i ).name ) > maxNameWidth then
                 put( identifiers( i ).name );
              else
                 -- pad name to fit
                 put( head( identifiers( i ).name, maxNameWidth ) );
              end if;
           else
              -- additional rounds is blank space in name column
              put( head( " ", maxNameWidth ) );
           end if;
           put( " | " );
           if length( escapedValue ) > 0 then
              firstChar := (round-1)*maxValueWidth+1;
              lastChar  := round*maxValueWidth;
              if lastChar > length( escapedValue ) then
                 lastChar := length( escapedValue );
              end if;
              put( head( slice( escapedValue, firstChar, lastChar ), maxValueWidth ) );
           else
              put( to_unbounded_string( integer( maxValueWidth ) * " ") );
           end if;
           put( " | " );
           if round = 1 then
              put_identifier_attributes( i );
           end if;
           new_line;
           exit when lastChar = length( escapedValue );
           round := round+1;
         end loop;
      end if;
  end loop;
exception
   -- DEVICE ERROR is typically a broken pipe... e.g. env | head . If it
   -- occurs, just stop outputing and raise to the caller (e.g. env
   -- command).
   when DEVICE_ERROR => raise;
end put_all_identifiers;


-----------------------------------------------------------------------------
-- Error reporting
-----------------------------------------------------------------------------

templateErrorHeader : constant unbounded_string := to_unbounded_string( "SparForte says" );


-----------------------------------------------------------------------------
--  CONVERT TO HTML
--
-- Change an error message so that it is formatted as HTML
-----------------------------------------------------------------------------

function convertToHTML( oldString : unbounded_string ) return unbounded_string is
   s : unbounded_string := oldString;
   p : natural;
   timeout : natural;
begin

   -- replace end-of-lines / spaces
   -- do this first as the spans we may insert have spaces

   p := 1;
   while p <= length( s ) loop
      if element( s, p ) = ASCII.LF then
         delete( s, p, p );
         insert( s, p, "<br>" );
      elsif element( s, p ) = ' ' then
         delete( s, p, p );
         insert( s, p, "&nbsp;" );
      end if;
      p := p + 1;
   end loop;

   -- replace boldface on

   timeout := 0;
   loop
      p := index( s, to_string( term( bold ) ) );
   exit when p = 0 or timeout = 10;
      delete( s, p, p - 1 + length( term( bold ) ) );
      insert( s, p, "<span style=""font-weight:bold"">" );
      timeout := timeout + 1;
   end loop;

   -- replace inverse on

   timeout := 0;
   loop
      p := index( s, to_string( term( inverse ) ) );
   exit when p = 0 or timeout = 10;
      delete( s, p, p - 1 + length( term( inverse ) ) );
      insert( s, p, "<span style=""font-style:italic"">" );
      timeout := timeout + 1;
   end loop;

   -- replace boldface/inverse off

   timeout := 0;
   loop
       p := index( s, to_string( term( normal ) ) );
   exit when p = 0 or timeout = 10;
      delete( s, p, p - 1 + length( term( normal ) ) );
      insert( s, p, "</span>" );
      timeout := timeout + 1;
   end loop;

   return s;
end convertToHTML;


-----------------------------------------------------------------------------
--  CONVERT TO PLAIN TEXT
--
-- Change an error message so that it is formatted as plain text
-- for a web server log file.  By default, all line feeds are removed.
-----------------------------------------------------------------------------

type plainTextOptions is ( no_lf, with_lf );

function convertToPlainText( oldString : unbounded_string; options : plainTextOptions := no_lf ) return unbounded_string is
  s : unbounded_string := oldString;
  p : natural;
  timeout : natural;
begin

  -- remove any end-of-lines to ensure message is on one line

  if options = no_lf then
     p := 1;
     while p <= length( s ) loop
        if element( s, p ) = ASCII.LF then
           delete( s, p, p );
           insert( s, p, " " );
        end if;
        p := p + 1;
     end loop;
  end if;

  -- remove boldface on

  timeout := 0;
  loop
     p := index( s, to_string( term( bold ) ) );
  exit when p = 0 or timeout = 10;
     delete( s, p, p - 1 + length( term( bold ) ) );
     timeout := timeout + 1;
  end loop;

  -- remove inverse on

   timeout := 0;
   loop
      p := index( s, to_string( term( inverse ) ) );
   exit when p = 0 or timeout = 10;
      delete( s, p, p - 1 + length( term( inverse ) ) );
      timeout := timeout + 1;
   end loop;

   -- remove boldface/inverse off

   timeout := 0;
   loop
      p := index( s, to_string( term( normal ) ) );
   exit when p = 0 or timeout = 10;
      delete( s, p, p - 1 + length( term( normal ) ) );
      timeout := timeout + 1;
   end loop;

   return s;
 end convertToPlainText;


-----------------------------------------------------------------------------
--  GET SCRIPT POSITION MESSAGE
--
-----------------------------------------------------------------------------

function get_script_execution_position( msg : string ) return unbounded_string is
  cmdline    : unbounded_string;
  firstpos   : natural;
  lastpos    : natural;
  lineStr    : unbounded_string;
  firstposStr : unbounded_string;
  lineno     : natural;
  fileno     : natural;
  outLine    : unbounded_string;
  gccOutLine : unbounded_string;
  sfr        : aSourceFile;
  needGccVersion : boolean := false;
  fullErrorMessage : unbounded_string;
begin
  -- Only create the abbreviated GCC-style error message if we need it
  --
  -- In the case of templates, we need both the Gcc version and the non-Gcc
  -- version of the error message.  In a CGI script that isn't a template,
  -- regular errors are reported back.

  needGccVersion := boolean( gccOpt ) or hasTemplate;

  -- Decode a copy of the command line to show the error.  Also returns
  -- the current token position and the line number.

  if script /= null then
     getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
  else
     -- can't use optional_inverse here because the text will be
     -- escaped later
     cmdLine := to_unbounded_string( "<No executable line to show> in <no script loaded>" );
  end if;

  -- Clear any old error messages from both the screen error and the
  -- template error (if one exists)

  fullErrorMessage := null_unbounded_string;

  -- If in a script (that is, a non-interactive input mode) then
  -- show the location and traceback.  Otherwise, if we're just at
  -- the command prompt, don't bother with the location/traceback.

  if inputMode /= interactive and inputMode /= breakout then

  -- Get the location information.  If gcc option, strip the leading
  -- blanks form the location information.  Use outLine to generate a full
  -- line of text because individual Put's are shown as individual lines
  -- in Apache's error logs for templates...a real mess.
  --
  -- The basic GCC message will be recorded in a separate "out line"
  -- as we may need both message formats for a web template.

     if script /= null then

        if needGccVersion then                            -- gcc style?
           lineStr := to_unbounded_string( lineno'img );  -- remove leading
           if length( lineStr ) > 0 then                  -- space (if any)
              if element( lineStr, 1 ) = ' ' then
                 delete( lineStr, 1, 1 );
              end if;
           end if;
           firstposStr := to_unbounded_string( firstpos'img );
           if length( firstposStr ) > 0 then              -- here, too
              if element( firstposStr, 1 ) = ' ' then
                 delete( firstposStr, 1, 1 );
              end if;
           end if;
           sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
           gccOutLine := sfr.name
             & ":" & lineStr
             & ":" & firstposStr
             & ":";                                       -- no traceback
           gccOutLine := gccOutLine & ' ';                -- token start
           gccOutLine := gccOutLine & msg;
        end if;

        -- For the regular format, show the location and traceback

        sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
        outLine := sfr.name                               -- location
           & ":" & lineno'img
           & ":" & firstpos'img
           & ": ";

        -- TODO: we're using UNIX eof's but should ideally be o/s
        -- independent

        if blocks_top > blocks'first then                 -- in a block?
           for i in reverse blocks'first..blocks_top-1 loop -- show the
               if i /= blocks_top-1 then                  -- simplified
                  outLine := outLine & " in ";            -- traceback
               end if;
               outLine := outLine & ToEscaped( blocks( i ).blockName );
           end loop;
           fullErrorMessage := outLine & ASCII.LF;
           outLine := null_unbounded_string;
        else                                              -- if no blocks
           outLine := outLine & "in script";              -- just say
           fullErrorMessage := outLine & ASCII.LF;        -- "in script"
           outLine := null_unbounded_string;
        end if;
     else
        -- no script?
        outLine := null_unbounded_string;
     end if; -- a script exists
  end if;

  -- For the normal version, we must follow the traceback with the
  -- message, error underline and show the error message.
  -- Output only full lines to avoid messy Apache error logs.
  --
  -- First, add the line the error occurred in

  fullErrorMessage := fullErrorMessage & toEscaped( cmdline );

  -- Draw the underline error pointer

  if script /= null then
     outLine := outLine & ada.strings.unbounded.to_string( (firstPos-1) * " " );      -- indent
     outLine := outLine & '^';                                  -- token start
     if lastpos > firstpos then                                 -- multi chars?
        outLine := outLine & ada.strings.unbounded.to_string( (lastpos-firstPos-1) * "-" );
        outLine := outLine & '^';                               -- token end
     end if;
     outLine := outLine & ' ';                                  -- token start
  end if;
  outLine := outLine & msg;

  -- Even for a template, if the user selected gccOpt specifically,
  -- use it.

  -- Pick which format the user wants for the full message.
  --
  -- TODO: we're using UNIX eof's but should ideally be o/s
  -- independent

  if gccOpt then
     fullErrorMessage := gccOutLine;
  else
     fullErrorMessage := fullErrorMessage & ASCII.LF & outLine;
  end if;

  -- If we are in any mode of the development cycle except maintenance
  -- mode, create an error message to display.  If we're in maintenance
  -- mode, create an error message only if debug is enabled.

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        fullErrorMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & convertToHTML( fullErrorMessage ) & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        fullErrorMessage := "/* " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) &  " */";
     when xmlTemplate =>
        fullErrorMessage := "<!-- " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) & " -->";
     when textTemplate =>
        fullErrorMessage := convertToPlainText( fullErrorMessage, with_lf );
     when noTemplate | jsonTemplate =>
        fullErrorMessage := convertToPlainText( fullErrorMessage );
     end case;
     -- In the case of the template, the error output must always
     -- be in gcc format (a single line) for the web server log.
     --
     -- This affects exception handling since HTML output for template
     -- will differ from error message in exceptions package.  Also,
     -- format this for Apache by stripping out the boldface or
     -- other effects.
     --
     fullErrorMessage := ConvertToPlainText( gccOutLine );
  end if;
  return fullErrorMessage;
end get_script_execution_position;


-----------------------------------------------------------------------------
--  ERR
--
-- Stop execution and record an compile-time or run-time error.  Format the
-- error according to the user's preferences and set the error_found flag.
--
-- Only display the first error/exception encounted.
-----------------------------------------------------------------------------

procedure err_shell( msg : string; wordOffset : natural ) is
  cmdline    : unbounded_string;
  firstpos   : natural;
  lastpos    : natural;
  lineStr    : unbounded_string;
  firstposStr : unbounded_string;
  lineno     : natural;
  fileno     : natural;
  outLine    : unbounded_string;
  gccOutLine : unbounded_string;
  sfr        : aSourceFile;
  needGccVersion : boolean := false;
begin

  -- Already displayed one error or script is complete?
  -- Don't record any more errors.

  if error_found or done then
     return;
  end if;

  -- Only create the abbreviated GCC-style error message if we need it
  --
  -- In the case of templates, we need both the Gcc version and the non-Gcc
  -- version of the error message.  In a CGI script that isn't a template,
  -- regular errors are reported back.

  needGccVersion := boolean( gccOpt ) or hasTemplate;

  -- Decode a copy of the command line to show the error.  Also returns
  -- the current token position and the line number.

  if script /= null then
     getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
  else
     -- can't use optional_inverse here because the text will be
     -- escaped later
     cmdLine := to_unbounded_string( "<No executable line to show> in <no script loaded>" );
  end if;

  -- Clear any old error messages from both the screen error and the
  -- template error (if one exists)

  fullErrorMessage := null_unbounded_string;
  fullTemplateErrorMessage := null_unbounded_string;

  -- If in a script (that is, a non-interactive input mode) then
  -- show the location and traceback.  Otherwise, if we're just at
  -- the command prompt, don't bother with the location/traceback.

  if inputMode /= interactive and inputMode /= breakout then

  -- Get the location information.  If gcc option, strip the leading
  -- blanks form the location information.  Use outLine to generate a full
  -- line of text because individual Put's are shown as individual lines
  -- in Apache's error logs for templates...a real mess.
  --
  -- The basic GCC message will be recorded in a separate "out line"
  -- as we may need both message formats for a web template.

     if script /= null then

        if needGccVersion then                            -- gcc style?
           lineStr := to_unbounded_string( lineno'img );  -- remove leading
           if length( lineStr ) > 0 then                  -- space (if any)
              if element( lineStr, 1 ) = ' ' then
                 delete( lineStr, 1, 1 );
              end if;
           end if;
           firstposStr := to_unbounded_string( firstpos'img );
           if length( firstposStr ) > 0 then              -- here, too
              if element( firstposStr, 1 ) = ' ' then
                 delete( firstposStr, 1, 1 );
              end if;
           end if;
           sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
           gccOutLine := sfr.name
             & ":" & lineStr
             & ":" & firstposStr
             & ":";                                       -- no traceback
           gccOutLine := gccOutLine & ' ';                -- token start
           gccOutLine := gccOutLine & msg;
        end if;

        -- For the regular format, show the location and traceback

        sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
        outLine := sfr.name                               -- location
           & ":" & lineno'img
           & ":" & firstpos'img
           & ": ";

        -- TODO: we're using UNIX eof's but should ideally be o/s
        -- independent

        if blocks_top > blocks'first then                 -- in a block?
           for i in reverse blocks'first..blocks_top-1 loop -- show the
               if i /= blocks_top-1 then                  -- simplified
                  outLine := outLine & " in ";            -- traceback
               end if;
               outLine := outLine & ToEscaped( blocks( i ).blockName );
           end loop;
           fullErrorMessage := outLine & ASCII.LF;
           outLine := null_unbounded_string;
        else                                              -- if no blocks
           outLine := outLine & "in script";              -- just say
           fullErrorMessage := outLine & ASCII.LF;        -- "in script"
           outLine := null_unbounded_string;
        end if;
     else
        -- no script?
        outLine := null_unbounded_string;
     end if; -- a script exists
  end if;

  -- For the normal version, we must follow the traceback with the
  -- message, error underline and show the error message.
  -- Output only full lines to avoid messy Apache error logs.
  --
  -- First, add the line the error occurred in

  fullErrorMessage := fullErrorMessage & toEscaped( cmdline );

  -- Draw the underline error pointer
  -- If it's a shell word with an offset into the word, just show a single
  -- caret.

  if script /= null then
     outLine := outLine & to_unbounded_string( (firstPos-1) * " " );      -- indent
     if wordOffset > 0 then
        outLine := outLine & to_unbounded_string( (wordOffset-1) * " " );
        outLine := outLine & '^';
     else
        outLine := outLine & '^';                               -- token start
        if lastpos > firstpos then                              -- multi chars?
           outLine := outLine & to_unbounded_string( (lastpos-firstPos-1) * "-" );
           outLine := outLine & '^';                            -- token end
        end if;
     end if;
     outLine := outLine & ' ';                                  -- token start
  end if;
  outLine := outLine & msg;

  -- Even for a template, if the user selected gccOpt specifically,
  -- use it.

  -- Pick which format the user wants for the full message.
  --
  -- TODO: we're using UNIX eof's but should ideally be o/s
  -- independent

  if gccOpt then
     fullErrorMessage := gccOutLine;
  else
     fullErrorMessage := fullErrorMessage & ASCII.LF & outLine;
  end if;

  -- If we are in any mode of the development cycle except maintenance
  -- mode, create an error message to display.  If we're in maintenance
  -- mode, create an error message only if debug is enabled.

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        fullTemplateErrorMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & convertToHTML( fullErrorMessage ) & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        fullTemplateErrorMessage := "/* " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) &  " */";
     when xmlTemplate =>
        fullTemplateErrorMessage := "<!-- " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) & " -->";
     when textTemplate =>
        fullTemplateErrorMessage := convertToPlainText( fullErrorMessage, with_lf );
     when noTemplate | jsonTemplate =>
        fullTemplateErrorMessage := convertToPlainText( fullErrorMessage );
     end case;
     -- In the case of the template, the error output must always
     -- be in gcc format (a single line) for the web server log.
     --
     -- This affects exception handling since HTML output for template
     -- will differ from error message in exceptions package.  Also,
     -- format this for Apache by stripping out the boldface or
     -- other effects.
     --
     -- TODO: document this
     fullErrorMessage := ConvertToPlainText( gccOutLine );
  end if;

  -- Show that this is an error, not an exception

  error_found := true;                                          -- flag error
  err_exception.name := null_unbounded_string;            -- not an exception
  last_status := 0;

  -- If trace mode is enabled, show this as the point in the execution
  -- where the error occurred.

  if traceOpt then
     put_trace( "error: " & msg );
  end if;
end err_shell;

-----------------------------------------------------------------------------
--  ERR
--
-- Stop execution and record an compile-time or run-time error.  Format the
-- error according to the user's preferences and set the error_found flag.
--
-- Only display the first error/exception encounted.
-----------------------------------------------------------------------------

procedure err( msg : string ) is
begin
   err_shell( msg, 0 );
end err;


-----------------------------------------------------------------------------
--  ERR EXCEPTION RAISED
--
-- General message when raising on when others =>
-----------------------------------------------------------------------------

procedure err_exception_raised is
begin
  err( "an unexpected exception was raised" );
end err_exception_raised;

-----------------------------------------------------------------------------
--  ERR RENAMING
--
-- Show an error message for something disallowed for renaming
-----------------------------------------------------------------------------

procedure err_renaming( ident : identifier ) is
  renaming_id : identifier;
begin
  for i in reverse ident+1..identifiers_top-1 loop
       if not identifiers(i).deleted then
           if identifiers(i).renaming_of = ident then
               renaming_id := i;
                exit;
            end if;
        end if;
  end loop;
  if identifiers( ident ).renamed_count = 1 then
     err( "renaming " &
          optional_bold( to_string( identifiers( renaming_id ).name ) ) &
          " still refers to " &
          optional_bold( to_string( identifiers( ident ).name ) ) );
  else
     err( "renaming " &
          optional_bold( to_string( identifiers( renaming_id ).name ) ) &
          " (and others) still refer to " &
          optional_bold( to_string( identifiers( ident ).name ) ) );
  end if;
end err_renaming;

-----------------------------------------------------------------------------
--  RAISE EXCEPTION
--
-- Like err, but for exceptions.  Stop execution and report a run-time
-- exception.  Set the error_found and exception-related global values.
-----------------------------------------------------------------------------

procedure raise_exception( msg : string ) is
  cmdline    : unbounded_string;
  firstpos   : natural;
  lastpos    : natural;
  lineStr    : unbounded_string;
  firstposStr : unbounded_string;
  lineno     : natural;
  fileno     : natural;
  outLine    : unbounded_string;
  gccOutLine : unbounded_string;
  sfr        : aSourceFile;
  needGccVersion : boolean := false;
begin

  -- Already displayed one error or script is complete?
  -- Don't display any more

  if error_found or done then
     return;
  end if;

  -- Only create the abbreviated GCC-style error message if we need it
  --
  -- In the case of templates, we need both the Gcc version and the non-Gcc
  -- version of the error message.  In a CGI script that isn't a template,
  -- regular errors are reported back.

  needGccVersion := boolean( gccOpt ) or hasTemplate;

  -- Decode a copy of the command line to show the error.  Also returns
  -- the current token position and the line number.

  getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );

  -- Clear any old error messages from both the screen error and the
  -- template error (if one exists)

  fullErrorMessage := null_unbounded_string;
  fullTemplateErrorMessage := null_unbounded_string;

  -- If in a script (that is, a non-interactive input mode) then
  -- show the location and traceback.  Otherwise, if we're just at
  -- the command prompt, don't bother with the location/traceback.

  if inputMode /= interactive and inputMode /= breakout then

  -- Get the location information.  If gcc option, strip the leading
  -- blanks form the location information.  Use outLine to generate a full
  -- line of text because individual Put's are shown as individual lines
  -- in Apache's error logs for templates...a real mess.
  --
  -- The basic GCC message will be recorded in a separate "out line"
  -- as we may need both message formats for a web template.

     if needGccVersion then                            -- gcc style?
        lineStr := to_unbounded_string( lineno'img );  -- remove leading
        if length( lineStr ) > 0 then                  -- space (if any)
           if element( lineStr, 1 ) = ' ' then
              delete( lineStr, 1, 1 );
           end if;
        end if;
        firstposStr := to_unbounded_string( firstpos'img );
        if length( firstposStr ) > 0 then              -- here, too
           if element( firstposStr, 1 ) = ' ' then
              delete( firstposStr, 1, 1 );
           end if;
        end if;
        sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
        gccOutLine := sfr.name
          & ":" & lineStr
          & ":" & firstposStr
          & ":";                  -- no traceback
        gccOutLine := gccOutLine & ' ';                -- token start
        gccOutLine := gccOutLine & msg;
     end if;

     -- For the regular format, show the location and traceback

     sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
     outLine := sfr.name               -- otherwise
        & ":" & lineno'img
        & ":" & firstpos'img
        & ": ";

     -- TODO: we're using UNIX eof's but should ideally be o/s
     -- independent

     if blocks_top > blocks'first then                     -- in a block?
        for i in reverse blocks'first..blocks_top-1 loop   -- show the
            if i /= blocks_top-1 then                      -- simplified
               outLine := outLine & " in ";                -- traceback
            end if;
            outLine := outLine & ToEscaped( blocks( i ).blockName );
        end loop;
        fullErrorMessage := outLine & ASCII.LF;
        outLine := null_unbounded_string;
     else                                                  -- otherwise
        outLine := outLine & "in script";                  -- just say
        fullErrorMessage := outLine & ASCII.LF;            -- "in script"
        outLine := null_unbounded_string;
     end if;
  end if;

  -- For the normal version, we must follow the traceback with the
  -- message, error underline and show the exception message.
  -- Output only full lines to avoid messy Apache error logs.
  --
  -- First, add the line the exception occurred in.  As a precaution,
  -- escape the command line.
  fullErrorMessage := fullErrorMessage & toEscaped( cmdline );

  -- Draw the underline error pointer

  outLine := outLine & ada.strings.unbounded.to_string( (firstPos-1) * " " );      -- indent
  outLine := outLine & '^';                                  -- token start
  if lastpos > firstpos then                                 -- multi chars?
     outLine := outLine & ada.strings.unbounded.to_string( (lastpos-firstPos-1) * "-" );
     outLine := outLine & '^';                               -- token end
  end if;
  outLine := outLine & ' ';                                  -- token start
  outLine := outLine & msg;
  -- DEBUG
  -- fullErrorMessage := fullErrorMessage & ASCII.LF & outLine;

  -- Even for a template, if the user selected gccOpt specifically,
  -- use it.

  -- Pick which format the user wants for the full message.
  --
  -- TODO: we're using UNIX eof's but should ideally be o/s
  -- independent

  if gccOpt then
     fullErrorMessage := gccOutLine;
  else
     fullErrorMessage := fullErrorMessage & ASCII.LF & outLine;
  end if;

  if hasTemplate and ( boolean( debugOpt or not maintenanceOpt ) ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        fullTemplateErrorMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #9F6000; background-color: #FEEFB3; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-weight:bold; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#9f6000; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">!</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #9F6000; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & convertToHTML( fullErrorMessage ) & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        fullTemplateErrorMessage := "/* " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) &  " */";
     when xmlTemplate =>
        fullTemplateErrorMessage := "<!-- " & templateErrorHeader & " " & convertToPlainText( fullErrorMessage ) & " -->";
     when noTemplate | textTemplate | jsonTemplate =>
        fullTemplateErrorMessage := convertToPlainText( fullErrorMessage );
     end case;
     -- In the case of the template, the error output must always
     -- be in gcc format (a single line) for the web server log.
     --
     -- This affects exception handling since HTML output for template
     -- will differ from error message in exceptions package.  Also,
     -- format this for Apache by stripping out the boldface or
     -- other effects.
     --
     -- TODO: document this
     fullErrorMessage := ConvertToPlainText( gccOutLine );

  end if;

  -- Show that this is an exception, not an error.  Do not erase
  -- err_exception.name.  The ParseRaise, etc. procedure will set
  -- err_exception.

  error_found := true;                                          -- flag error

  -- If trace mode is enabled, show this as the point in the execution
  -- where the error occurred.

  if traceOpt then
     put_trace( "exception: " & msg );
  end if;
end raise_exception;


-----------------------------------------------------------------------------
--  ERR TEST RESULT
--
-- Show a failed result from pragma test_result
-----------------------------------------------------------------------------

procedure err_test_result is
  cmdline    : unbounded_string;
  firstpos   : natural;
  lastpos    : natural;
  lineStr    : unbounded_string;
  firstposStr : unbounded_string;
  lineno     : natural;
  fileno     : natural;
  outLine    : unbounded_string;
  gccOutLine : unbounded_string;
  sfr        : aSourceFile;
  needGccVersion : boolean := false;
  msg        : constant unbounded_string := to_unbounded_string( "test failed" );
  ourFullErrorMessage : unbounded_string;
  ourFullTemplateErrorMessage : unbounded_string;
begin
  -- determine if gcc format is requested or required
  needGccVersion := boolean( gccOpt ) or hasTemplate;

  -- get the command line
  if script /= null then
     getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
  else
     raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location &
         ": internal error: script unexpectly empty";
  end if;

  if inputMode /= interactive and inputMode /= breakout then
        if needGccVersion then                            -- gcc style?
           lineStr := to_unbounded_string( lineno'img );  -- remove leading
           if length( lineStr ) > 0 then                  -- space (if any)
              if element( lineStr, 1 ) = ' ' then
                 delete( lineStr, 1, 1 );
              end if;
           end if;
           firstposStr := to_unbounded_string( firstpos'img );
           if length( firstposStr ) > 0 then              -- here, too
              if element( firstposStr, 1 ) = ' ' then
                 delete( firstposStr, 1, 1 );
              end if;
           end if;
           sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
           gccOutLine := sfr.name
             & ":" & lineStr
             & ":" & firstposStr
             & ":";                                       -- no traceback
           gccOutLine := gccOutLine & ' ';                -- token start
           gccOutLine := gccOutLine & msg;
        end if;


        sourceFilesList.Find( sourceFiles, SourceFilesList.aListIndex( fileno ), sfr );
        outLine := sfr.name                               -- location
           & ":" & lineno'img
           & ":" & firstpos'img
           & ": ";
        -- TODO: we're using UNIX eof's but should ideally be o/s
        -- independent
        if blocks_top > blocks'first then                 -- in a block?
           for i in reverse blocks'first..blocks_top-1 loop -- show the
               if i /= blocks_top-1 then                  -- simplified
                  outLine := outLine & " in ";            -- traceback
               end if;
               outLine := outLine & ToEscaped( blocks( i ).blockName );
           end loop;
           ourFullErrorMessage := outLine & ASCII.LF;
           outLine := null_unbounded_string;
        else                                              -- if no blocks
           outLine := outLine & "in script";              -- just say
           ourFullErrorMessage := outLine & ASCII.LF;        -- "in script"
           outLine := null_unbounded_string;
        end if;
  end if;

  ourFullErrorMessage := ourFullErrorMessage & toEscaped( cmdline );

  -- Draw the underline error pointer

  outLine := outLine & ada.strings.unbounded.to_string( (firstPos-1) * " " );      -- indent
  outLine := outLine & '^';                                  -- token start
  if lastpos > firstpos then                                 -- multi chars?
     outLine := outLine & ada.strings.unbounded.to_string( (lastpos-firstPos-1) * "-" );
     outLine := outLine & '^';                               -- token end
  end if;
  outLine := outLine & ' ';                                  -- token start
  outLine := outLine & msg;

  -- Even for a template, if the user selected gccOpt specifically,
  -- use it.

  -- Pick which format the user wants for the full message.
  --
  -- TODO: we're using UNIX eof's but should ideally be o/s
  -- independent

  if gccOpt then
     ourFullErrorMessage := gccOutLine;
  else
     ourFullErrorMessage := ourFullErrorMessage & ASCII.LF & outLine;
  end if;

  -- If we are in any mode of the development cycle except maintenance
  -- mode, create an error message to display.  If we're in maintenance
  -- mode, create an error message only if debug is enabled.

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        ourFullTemplateErrorMessage := "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & convertToHTML( ourFullErrorMessage ) & "</p>" &
           "</div>" &
           "</div>" &
           "<br />";
     when cssTemplate | jsTemplate =>
        ourFullTemplateErrorMessage := "/* " & templateErrorHeader & " " & convertToPlainText( ourFullErrorMessage ) &  " */";
     when xmlTemplate =>
        fullTemplateErrorMessage := "<!-- " & templateErrorHeader & " " & convertToPlainText( ourFullErrorMessage ) & " -->";
     when noTemplate | textTemplate | jsonTemplate =>
        ourFullTemplateErrorMessage := convertToPlainText( ourFullErrorMessage );
     end case;
     -- In the case of the template, the error output must always
     -- be in gcc format (a single line) for the web server log.
     --
     -- This affects exception handling since HTML output for template
     -- will differ from error message in exceptions package.  Also,
     -- format this for Apache by stripping out the boldface or
     -- other effects.
     --
     -- TODO: document this
     ourFullErrorMessage := ConvertToPlainText( gccOutLine );
  end if;

  -- Show that this is an error, not an exception

  -- this doesn't raise an exception or error
  -- err_exception.name := null_unbounded_string;            -- not an exception
  -- last_status := 0;

  -- It is redundanct to output the message when tracing since it is always
  -- output when it occurs anyway...it's not an error or exception.

  -- Show the test result message immediately

  put_line( standard_error, ourFullErrorMessage );
  -- may or may not have a template at this point, so check
  if hasTemplate then
     putTemplateHeader( templateHeader );
     put_line( ourFullTemplateErrorMessage );
  end if;

  -- Originally:
  --
  -- put( standard_error, scriptFilePath );
  -- put( standard_error, ":" );
  -- put( standard_error, getLineNo'img );
  -- put( standard_error, ": " );
  -- if gccOpt then
  --     put_line( standard_error, "test failed" );
  -- else
  --     put_line( standard_error, to_string( getCommandLine ) );
  --     put_line( standard_error, "^ test failed" );
  -- end if;
end err_test_result;


-----------------------------------------------------------------------------
--  WARN
--
-- Issue a warning.  This is done immediately, is not formatted by gcc-style
-- preference and is not stored.
-----------------------------------------------------------------------------

procedure warn( msg : string ) is
  location : unbounded_string;
  fullMsg  : unbounded_string;
begin
  location := scriptFilePath & ":" & getLineNo'img & ": ";
  fullMsg  := location & "warning--" & msg;

  put_line( standard_error, fullMsg );

  if hasTemplate and boolean( debugOpt or not maintenanceOpt ) then
     case templateHeader.templateType is
     when htmlTemplate | wmlTemplate =>
        put( "<div style=""border: 1px solid; margin: 10px 5px padding: 15px 10px 15px 50px; color: #00529B; background-color: #BDE5F8; width:100%; overflow:auto"">" &
           "<div style=""float:left;font: 32px Times New Roman,serif; font-style:italic; border-radius:50%; height:50px; width:50px; color: #FFFFFF; background-color:#00529B; text-align: center; vertical-align: middle; line-height: 50px; margin: 5px"">i</div>" &
           "<div style=""float:left;font: 12px Courier New,Courier,monospace; color: #00529B; background-color: transparent"">" &
           "<p style=""font: 14px Verdana,Arial,Helvetica,sans-serif; font-weight:bold"">" & templateErrorHeader & "</p>" &
           "<p>" & convertToHTML( fullMsg ) & "</p>" &
           "</div>" &
           "</div>" &
           "<br />" );
     when cssTemplate | jsTemplate =>
        put( "/* " & templateErrorHeader & " " & convertToPlainText( fullMsg ) &  " */" );
     when xmlTemplate =>
        put( "<!-- " & templateErrorHeader & " " & convertToPlainText( fullMsg ) & " -->" );
     when noTemplate | textTemplate | jsonTemplate =>
        put( convertToPlainText( fullMsg ) );
     end case;
  end if;
end warn;


-----------------------------------------------------------------------------
--  ERR PREVIOUS
--
-- Same as err below, but don't hilight the current token because the error
-- actually happened before it.  Just mark start of current token.
-----------------------------------------------------------------------------

procedure err_previous( msg : string ) is
  savepos : integer;
begin
  savepos := lastpos;     -- save current token's last character position
  lastpos := firstpos;    -- token length is one to produce a single '^'
  err( msg );             -- show message pointing at first character
  lastpos := savepos;     -- restore current token's last character position
end err_previous;


-----------------------------------------------------------------------------
-- Scope
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  RECORD SOFTWARE MODEL REQUIREMENTS
--
-- Checked the referenced identifier and update the requirements for the
-- selected software model.
-----------------------------------------------------------------------------

procedure recordSoftwareModelRequirements( id : identifier ) is
  model_id : identifier;
  model    : unbounded_string;
begin
-- TODO: ensure software model is set during syntax_check
  if softwareModelSet then
     -- TODO: this is slow...should cache it somehow
     findIdent( system_script_software_model_name, model_id );
     model := identifiers( model_id ).value.all;
  end if;
  if model = shell_script_model_name then
     if id = standard_error_t then
        null; -- TODO: record that standard_error was used (not yet written)
     end if;
  end if;
end recordSoftwareModelRequirements;


-----------------------------------------------------------------------------
--  CHECK IDENTIFIERS IN CURRENT BLOCK
--
-- check for unused variables and tally presence of software model req's
-- this is normally called automatically by pullBlock but is exposed
-- here for cases where pullBlock doesn't get run, such as simple scripts
-- with no blocks.  This should only be run during a syntax check.
-----------------------------------------------------------------------------

procedure checkIdentifiersInCurrentBlock is
begin
  --if blocks_top > 1 then
    -- put_line( "checkSoftwareModelRequirements: " & to_string( blocks( blocks_top-1 ).blockName ) ); -- DEBUG
  --else
     --put_line( "checkSoftwareModelRequirements: no block" ); -- DEBUG
  --end if;

  if not hasTemplate then
     for i in reverse blocks(blocks_top-1).identifiers_top..identifiers_top-1 loop
         if not identifiers( i ).deleted then
            -- put( " id:" ); put( i'img ); -- DEBUG
            -- put_line( " " & to_string( identifiers( i ).name ) ); -- DEBUG
            -- types that are not applied out to be abstract in design or
            -- test phase
            if identifiers( i ).class = typeClass or
               identifiers( i ).class = subClass then
               if boolean( designOpt ) or boolean( testOpt ) then
                  if not identifiers( i ).wasApplied then
                     if identifiers( i ).field_of = eof_t then -- not a field of a record
                        err( optional_bold( to_string( identifiers( i ).name ) ) &
                           " is a " & optional_bold( "concrete type" ) &
                           " but expected an " & optional_bold( "abstract type" ) &
                           ".  It is not used in declarations." );
                     end if;
                  end if;
               end if;
               -- Often subprogram are defined and not used (in libraries)
               --elsif identifiers( i ).class = userProcClass or
               --   identifiers( i ).class = userFuncClass then
               --   if boolean( designOpt ) or boolean( testOpt ) then
               --      if not identifiers( i ).wasReferenced then
               --         if not identifiers( i ).noVar then
               --            err( optional_bold( to_string( identifiers( i ).name ) ) &
               --               " is a " & optional_bold( "concrete subprogram" ) &
               --               " but expected an " & optional_bold( "abstract subprogram" ) &
               --               ".  It is never run." );
               --         end if;
               --      end if;
               --   end if;
            end if; -- abstract test

            -- Do not apply to record fields as some record fields may not be
            -- accessed.
            if not identifiers( i ).wasFactor and not identifiers( i ).wasWritten then
               if testOpt then
                  if not onlyAda95 then -- limited not available with pragma ada_95
                     if identifiers( i ).class = varClass then
                        if identifiers( i ).field_of /= eof_t then
                           if identifiers( i ).usage /= limitedUsage then
                              if identifiers( i ).name /= "return value" then
                                 err( optional_bold( to_string( identifiers( i ).name ) ) &
                                    " is a " & optional_bold( "not-limited variable" ) &
                                    " but expected a " & optional_bold( "limited" ) &
                                    ".  It (or its elements) are not used in expressions nor is assigned to." );
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if; -- limited test

            -- Test for variables that are never written to.  This is only done
            -- in testing phase mode as code under development may indeed have
            -- variables like this, and many things are unwritten in design phase.
            -- Don't apply to record fields.

            if identifiers( i ).wasReferenced and not identifiers( i ).wasWritten then
               if testOpt then
                  if identifiers( i ).class = varClass then
                     if identifiers( i ).field_of /= eof_t then
                        err( optional_bold( to_string( identifiers( i ).name ) ) &
                           " is a " & optional_bold( "variable" ) &
                           " but expected a " & optional_bold( "constant" ) &
                           " (or in mode parameter).  It (or its elements) are never written to." );
                     end if;
                  end if;
               end if;
            end if; -- constant

            -- Unimplemented subprogram or constant specifications
            -- This need to occur before the unused identifier test.

            if identifiers( i ).class = userProcClass or identifiers( i ).class = userFuncClass or
               identifiers( i ).class = varClass then
               if identifiers( i ).specAt /= noSpec then
                  err( optional_bold( to_string( identifiers( i ).name ) ) &
                       " has a specification but is not implemented (at " &
                       to_string( identifiers( i ).specFile) & ":" &
                       identifiers( i ).specAt'img & ");" );
               end if;
            end if;

            -- If something was used, apply any software model requirements (if
            -- any)
            if identifiers( i ).wasReferenced then
               --put( " REF'D: " ); put_identifier( i ); -- DEBUG
               -- TODO: Refactor out
               if softwareModelSet then
                  recordSoftwareModelRequirements( i );
               end if;
            -- Unused variables are always checked.  Skip record fields since there
            -- is no guarantee that all fields will be accessed.
            else
--put_line( standard_error, "HERE 1" ); -- DEBUG
--put( standard_error, " id:" ); put( identifier'image(i-2) ); -- DEBUG
--put_line( standard_error, " " & to_string( identifiers( i-2 ).name ) ); -- DEBUG
--put( standard_error, " id:" ); put( identifier'image(i-1) ); -- DEBUG
--put_line( standard_error, " " & to_string( identifiers( i-1 ).name ) ); -- DEBUG
--put( standard_error, " id:" ); put( i'img ); -- DEBUG
--put_line( standard_error, " " & to_string( identifiers( i ).name ) ); -- DEBUG
--           if identifiers( i ).field_of = eof_t then
              -- in design mode, only check types
               if designOpt then
                  if identifiers( i ).class = typeClass or
                     identifiers( i ).class = subClass or
                     identifiers( i ).class = genericTypeClass then
                     err( optional_bold( to_string( identifiers( i ).name ) ) & " is declared but never used" );
                  end if;
                  -- when testing or maintenance, check all identifiers, even
                  -- variables
               elsif testOpt or maintenanceOpt then
                  err( optional_bold( to_string( identifiers( i ).name ) ) & " is declared but never used" );
                  -- in development, only check variables
               elsif identifiers( i ).class = varClass then
                  err( optional_bold( to_string( identifiers( i ).name ) ) & " is declared but never used" );
               end if;
            end if;
         end if; -- not deleted

          -- TODO declaration line would be helpful if two identifiers have the same
          -- name.
      end loop;
  end if;
end checkIdentifiersInCurrentBlock;


-----------------------------------------------------------------------------
--  CHECK IDENTIFIERS FOR SIMPLE SCRIPTS
--
-- Check the identifiers in a simple script to see if they were used.  It
-- is also run after a well-formed script to confirm any identifiers declared
-- outside of the main program.
-- Variables are always checked.  Other identifiers are checked if SparForte
-- is in testing mode.  If a software model is set, check the used
-- identifiers against the software model and record requirments that were
-- met.  This should only be run during the syntax check.
--
-- This only runs if not processing a template because the variables may be
-- used in the template file.  TODO: can this both work?
-----------------------------------------------------------------------------

procedure checkIdentifiersForSimpleScripts is
begin
   --put_line( "checkSoftwareModelRequirementsForSimpleScripts: no block" ); -- DEBUG
  if not hasTemplate then
     for i in reverse predefined_top..identifiers_top-1 loop
         if not identifiers( i ).deleted then
--put( " id:" ); put( i'img ); -- DEBUG
--put_line( " " & to_string( identifiers( i ).name ) ); -- DEBUG
            if identifiers( i ).class = typeClass or
               identifiers( i ).class = subClass then
               if boolean( designOpt ) or boolean( testOpt ) then
                  if not identifiers( i ).wasApplied then
                     if identifiers( i ).field_of = eof_t then -- not a field of a record
                        err( optional_bold( to_string( identifiers( i ).name ) ) &
                           " is a " & optional_bold( "concrete type" ) &
                           " but expected an " & optional_bold( "abstract type" ) &
                           ".  It is not used in declarations." );
                     end if;
                  end if;
               end if;
            -- Often subprogram are defined and not used (in libraries)
            --elsif identifiers( i ).class = userProcClass or
            --   identifiers( i ).class = userFuncClass then
            --   if boolean( designOpt ) or boolean( testOpt ) then
            --      if not identifiers( i ).wasReferenced then
            --         if not identifiers( i ).noVar then
            --            err( optional_bold( to_string( identifiers( i ).name ) ) &
            --               " is a " & optional_bold( "concrete subprogram" ) &
            --               " but expected an " & optional_bold( "abstract subprogram" ) &
            --               ".  It is never run." );
            --         end if;
            --      end if;
            --   end if;
            end if;
--put_line( to_string( identifiers( i ).name ) & "being tested" ); -- DEBUGME
--put_line( identifiers( i ).wasFactor'img ); -- DEBUGME
            if not identifiers( i ).wasFactor and not identifiers( i ).wasWritten then
--put_line( to_string( identifiers( i ).name ) & " is not a factor" ); -- DEBUGME
               if testOpt then
--put_line( "test opt" ); -- DEBUGME
                  if identifiers( i ).class = varClass then
--put_line( "variable" ); -- DEBUGME
                     if not onlyAda95 then -- limited not available with pragma ada_95
--put_line( "not ada_95" ); -- DEBUGME
                        if identifiers( i ).usage /= limitedUsage then
                           if identifiers( i ).name /= "return value" then
                              err( optional_bold( to_string( identifiers( i ).name ) ) &
                                 " is a " & optional_bold( "not-limited variable" ) &
                                 " but expected a " & optional_bold( "limited" ) &
                                 ".  It (or its elements) are not used in expressions nor is assigned to." );
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;

            -- Test for variables that are never written to.  This is only done
            -- in testing phase mode as code under development may indeed have
            -- variables like this, and many things are unwritten in design phase.
            -- Don't apply to record fields.

            --if identifiers( i ).wasReferenced and not identifiers( i ).wasWritten then
            --   if testOpt then
            --      if identifiers( i ).class = varClass then
            --         if identifiers( i ).field_of /= eof_t then
            --            err( optional_bold( to_string( identifiers( i ).name ) ) &
            --               " is a " & optional_bold( "variable" ) &
            --               " but expected a " & optional_bold( "constant" ) &
            --               " (or in mode parameter).  It (or its elements) are never written to." );
            --         end if;
            --      end if;
            --   end if;
            --end if; -- constant

            -- Unimplemented subprogram and constant specifications
            -- This need to occur before the unused identifier test.
            -- In simple scripts, these would be in a declare block.

            if identifiers( i ).class = userProcClass or identifiers( i ).class = userFuncClass or
               identifiers( i ).class = varClass then
               if identifiers( i ).specAt /= noSpec then
                  err( optional_bold( to_string( identifiers( i ).name ) ) &
                       " has a specification but is not implemented (at " &
                       to_string( identifiers( i ).specFile) & ":" &
                       identifiers( i ).specAt'img & ");" );
               end if;
            end if;

            if identifiers( i ).wasReferenced then
--put( " REF'D: " ); put_identifier( i ); -- DEBUG
         -- TODO: Refactor out
               if softwareModelSet then
                  recordSoftwareModelRequirements( i );
               end if;
-- TODO: should this be dropped altogether?
            elsif boolean( testOpt ) or identifiers( i ).class = varClass then
              -- in design mode, only check types
--put_line( standard_error, "HERE 2" ); -- DEBUG
--           if identifiers( i ).field_of = eof_t then
--put_line( standard_error, "HERE 2 - not a field" ); -- DEBUG
--put( standard_error, " id:" ); put( i'img ); -- DEBUG
--put_line( standard_error, " " & to_string( identifiers( i ).name ) ); -- DEBUG
                 err( optional_bold( to_string( identifiers( i ).name ) ) & " is declared but never used" );
--           end if;
            end if;
         end if; -- not deleted
     end loop;
  end if;
end checkIdentifiersForSimpleScripts;


-----------------------------------------------------------------------------
--  COMPLETE SOFTWARE MODEL REQUIREMENTS
--
-- Check pre-defined identifiers for software requirements.  Then evaluate
-- if the requirements were met for the software model.  This should only be
-- run during the syntax check.
-----------------------------------------------------------------------------

procedure completeSoftwareModelRequirements is
begin
  for i in reverse reserved_top..predefined_top-1 loop
      if identifiers( i ).wasReferenced then
--put( " REF'D: " ); put_identifier( i ); -- DEBUG
         if softwareModelSet then
            recordSoftwareModelRequirements( i );
         end if;
      end if;
  end loop;
end completeSoftwareModelRequirements;

-----------------------------------------------------------------------------
--  PUSH BLOCK
--
-- Start a block statement or scope by pushing the current symbol table stack
-- information on the blocks stack.  It also records the current
-- scanner position so execution can return to the top of the block.
-- (Pulling the block later will restore the symbol table to its old
-- contents.)  If newScope is true, a new nested identifier scope
-- is also started.
-----------------------------------------------------------------------------

procedure pushBlock( newScope : boolean := false;
  newName : string := ""; newThread : aThreadName := noThread ) is
begin
  if blocks_top = block'last then                               -- no room?
     raise block_table_overflow with Gnat.Source_Info.Source_Location &
         ": too many nested blocks";
  else
     -- start new scope by recording current
     declare
        theBlock : blockDeclaration renames blocks( blocks_top );  -- new block
     begin
        theBlock.startpos := scriptLineStart;                      -- current line
        theBlock.identifiers_top := identifiers_top;               -- last ident
        theBlock.newScope := newScope;                             -- scope flag
        theBlock.blockName := To_Unbounded_String( newName );      -- name if any
        if newThread = noThread then
           if blocks_top = block'first then
              theBlock.threadName := mainThread;
           else
              -- inheret name from parent
              theBlock.threadName := blocks( blocks_top-1 ).threadName;
           end if;
        else
           theBlock.threadName := newThread;
        end if;
        markScanner( theBlock.state );                            -- scanner pos
     end;
     blocks_top := blocks_top + 1;                              -- push stack

  -- Tiny Hash Cache

  --resetTinyHashCache;

  end if;
end pushBlock;

-----------------------------------------------------------------------------
--  PULL BLOCK
--
-- Discards a block statement from the blocks stack, discarding all
-- declared identifiers.  (pullBlock treats all declarations in the
-- block as local.  This is not a problem due to the problem of skipping
-- over conditional code, but once that issue is solved I'll need to
-- come back to this one.)
-----------------------------------------------------------------------------

procedure pullBlock is
  b : boolean;
begin
  if blocks_top = block'first then                              -- no blocks?
     raise block_table_overflow with Gnat.Source_Info.Source_Location &
         ": too many nested blocks";
  else
     -- If it was a block with a new scope, check the identifiers and pull
     -- (discard) any resources prior to removing the block.
     if blocks( blocks_top-1 ).newScope then -- KB: 18/01/17
        if syntax_check and not error_found and not done then
          checkIdentifiersInCurrentBlock;
        end if;
        -- Resources are assigned to blocks_top (i.e. the next free block )
        -- so you must pull prior to decrementing.
        pullResourceBlock( blocks_top );                        -- do any r's
     end if;
     blocks_top := blocks_top - 1;                              -- pop stack
     --end if;
     -- delete the identifiers, exporting if necessary
     for i in reverse blocks( blocks_top ).identifiers_top .. identifiers_top-1 loop
         b := deleteIdent( i );
     end loop;
     identifiers_top := blocks( blocks_top ).identifiers_top;   -- pop decl's

  -- Tiny Hash Cache

  --resetTinyHashCache;

  end if;
end pullBlock;

-----------------------------------------------------------------------------
--  TOP OF BLOCK
--
-- Return to the top of the current block statement as marked by pushBlock.
-----------------------------------------------------------------------------

procedure topOfBlock is
  firstpos : natural;
  lastpos  : natural;
  lineno   : natural;
  fileno   : natural;
  cmdLine  : unbounded_string;
begin
   if blocks_top = blocks'first then                            -- in a block?
      err( gnat.source_info.source_location & ": internal error: topOfBlock: not in a block" );
   else
      resumeScanning( blocks( blocks_top-1 ).state );           -- move to top
      if inputMode /= interactive and inputMode /= breakout then -- in a script?
         scriptLineStart := blocks( blocks_top-1).startpos;     -- current line
         if trace and not exit_block and not error_found then   -- display
            put( standard_error, "=> " & '"' );                 -- line if
            getCommandLine( cmdline, firstpos, lastpos, lineno, fileno );
            put( standard_error, toEscaped( cmdline ) );
            put( standard_error, """ [" );
            put( standard_error, lineno'img );
            put_line( standard_error, "]" );
         end if;
      end if;
  end if;
end topOfBlock;

-----------------------------------------------------------------------------
--  GET FULL PARENT UNIT NAME
--
-- Return the full (dotted) name of the scope (i.e. what you would prefix
-- to identify a variable).  Be aware that some blocks like declare have a
-- space in the name.
-- Unique is true if the scope path doesn't contain an anonymous block
-- (that is, a declare block, which has no name) in the path.  This is
-- because two anonymous blocks in the same scope may have two variables
-- with the same name: they would produce the same path but would not be
-- the same variable.
-- At the time of writing, this was created but never used anywhere.
-----------------------------------------------------------------------------

procedure GetFullParentUnitName( fullUnitName : out unbounded_string; unique : out boolean ) is
begin
  unique := true;
  if blocks_top = blocks'first then
     fullUnitName := to_unbounded_string( "script" );
  end if;
  for b in blocks'first..blocks_top -1 loop
      if blocks( b ).newScope then
         if length( fullUnitName ) = 0 then
            fullUnitName := blocks( b ).blockName;
         else
            fullUnitName := fullUnitName & "." & blocks( b ).blockName;
         end if;
         if blocks( b ).blockName = "declare block" then
            unique := false;
         end if;
      end if;
  end loop;
end GetFullParentUnitName;

-----------------------------------------------------------------------------
--  IS LOCAL
--
-- True if the identifier is local to the current block's scope
-----------------------------------------------------------------------------

function isLocal( id : identifier ) return boolean is
  blockStart : identifier := 1; -- start from very first identifier
  nextBlock       : block;
  hasLocalBlock : boolean;
begin
  if blocks_top > blocks'first then                    -- in a nested block?
     nextBlock := blocks_top - 1;                      -- start with latest
     hasLocalBlock := true;                            -- assume we'll find one
     --while not blocks( nextBlock ).newScope            -- if not new scope
     while not blocks( nextBlock ).newScope            -- if not new scope
       or blocks( nextBlock ).blockName = "for loop" loop   -- for is a special case
        if nextBlock = 1 then                          -- hit bottom?
           hasLocalBlock := false;                     -- no local blocks
           exit;                                       -- and quit
        end if;
        nextBlock := nextBlock - 1;                    -- try previous block
     end loop;
     if hasLocalBlock then                              -- found local scope?
        blockStart := blocks( nextBlock ).identifiers_top; -- use it
     end if;
  end if;
--put_line( "isLocal: testing identifier " & identifiers( id ).name );
--put_line( "isLocal: blocks top is " & Blocks_top'img );
--put_line( "isLocal: local scope starts at block " & nextBlock'img );
--put_line( "isLocal: " & id'img & " >= " & blockStart'img );
   return id >= blockStart;
end isLocal;


-----------------------------------------------------------------------------
--  GET IDENTIFIER BLOCK
--
-- Return the block number for an identifier
-----------------------------------------------------------------------------

function getIdentifierBlock( id : identifier ) return block is
  -- the default block is block zero (blocks_top = 1 )
  theBlock : block := block'first;
begin
--put_line( "getidentBlock: " & to_string( identifiers( id ).name ) ); -- DEBUG
  for b in reverse blocks'first..blocks_top-1 loop     -- from top down
      if blocks( b ).newScope then                     -- new scope?
         if id >= blocks( b ).identifiers_top then     -- id in it?
            theBlock := b;                             -- return block
--put_line( "getidentBlock: " & theBlock'img & ' ' & to_string( blocks( b ).blockName ) ); -- DEBUG
            exit;
         end if;
      end if;
  end loop;
  return theBlock;
end getIdentifierBlock;


-----------------------------------------------------------------------------
--  GET THREAD NAME
--
-- Return the name of the given thread.  If in doubt, presume the main
-- program is the thread.
-----------------------------------------------------------------------------

function getThreadName return aThreadName is
begin
  if blocks_top = block'first then
     return mainThread;
  end if;
  return blocks( blocks_top-1 ).threadName;
end getThreadName;

function getThreadName( b : block ) return aThreadName is
begin
  if b >= blocks_top then
     return mainThread;
  end if;
  return blocks( b ).threadName;
end getThreadName;


-----------------------------------------------------------------------------
--  GET BLOCK NAME
--
-- return the name of the given block
-----------------------------------------------------------------------------

function getBlockName( b : block ) return unbounded_string is
-- return the name of the given block
begin
  if b >= blocks_top then
     return null_unbounded_string;
  end if;
  return blocks( b ).blockName;
end getBlockName;


-----------------------------------------------------------------------------
--  SAW RETURN
--
-- Mark return statement has having been seen in the most recent non-declare
-- scope block.  If no such block, does nothing.
-----------------------------------------------------------------------------

procedure sawReturn is
  b : block := blocks_top;
begin
  -- blocks_top is +1
  while b > block'first loop                           -- while blocks
     b := b - 1;                                       -- current block
     if blocks( b ).newScope and
        blocks( b ).blockName /= "declare block" and
        blocks( b ).blockName /= "begin block" then
        blocks( b ).hasReturn := true;
        exit;
     end if;
  end loop;
end sawReturn;


-----------------------------------------------------------------------------
--  BLOCK HAS RETURN
--
-- True if a block has been flagged as having a return
-----------------------------------------------------------------------------

function blockHasReturn return boolean is
  res : boolean := false;
begin
  if blocks_top > block'first then
     res := blocks( blocks_top-1 ).hasReturn;
  end if;
  return res;
end blockHasReturn;


-----------------------------------------------------------------------------
--  START EXCEPTION HANDLER
--
-- Mark exception handler block as running (if there is one)
-----------------------------------------------------------------------------

procedure startExceptionHandler is
begin
  if blocks_top > block'first then
     -- TODO: could be optimized with eof_t...
     blocks( blocks_top-1 ).inHandler := true;
     blocks( blocks_top-1 ).occurrence_exception := err_exception;
     -- TODO: an exception may be progated out of the declaration scope, leaving
     -- the position in the symbol table undefined
     blocks( blocks_top-1 ).occurrence_message := err_message;
     blocks( blocks_top-1 ).occurrence_status := last_status;
     blocks( blocks_top-1 ).occurrence_full := fullErrorMessage;
  end if;
end startExceptionHandler;


-----------------------------------------------------------------------------
--  IN EXCEPTION HANDLER
--
-- True if in exception handler (i.e. re-raise is valid).  If no block, then
-- no handler.
-----------------------------------------------------------------------------

function inExceptionHandler return boolean is
  res : boolean := false;
  b : block := blocks_top;
begin
  if blocks_top > block'first then                        -- if have a block
     b := b - 1;                                          -- current block
     loop                                                 -- while blocks
        if blocks( b ).newScope then                      -- a scoping block?
           res := blocks( b ).inHandler;                  -- whatever it is
           exit;
        end if;                                           -- else
     exit when b = blocks'first;
        b := b - 1;                                       -- keep looking
     end loop;
  end if;
  return res;
end inExceptionHandler;


-----------------------------------------------------------------------------
--  GET BLOCK EXCEPTION
--
-- Return the exception defined by startExceptionHandler
-----------------------------------------------------------------------------

procedure getBlockException(
  occurrence_exception : out declaration;
  occurrence_message   : out unbounded_string;
  occurrence_status    : out aStatusCode
) is
  b : block := blocks_top;
begin
  if blocks_top > block'first then                        -- if have a block
     b := b - 1;                                          -- current block
     loop                                                 -- while blocks
        if blocks( b ).newScope then                      -- a scoping block?
           occurrence_exception := blocks( b ).occurrence_exception;
           occurrence_message   := blocks( b ).occurrence_message;
           occurrence_status    := blocks( b ).occurrence_status;
           exit;
        end if;                                           -- else
     exit when b = block'first;
        b := b - 1;                                       -- keep looking
     end loop;
  else
     err( gnat.source_info.source_location & ": internal error: getting exception but not in an exception block" );
  end if;
end getBlockException;


-----------------------------------------------------------------------------
--  DUMP SYMBOL TABLE
--
-- Debugging routine to display the top of the symbol table.
-----------------------------------------------------------------------------

procedure dumpSymbolTable is
  count : natural := 0;
begin
  put_line( "-- Symbol Table Dump ---------------------------------------------" );
  for i in reverse 1..identifiers_top-1 loop
      put( "symbol" & i'img & ": " );
      Put_Identifier( i );
  exit when count = 10;
      count := count + 1;
  end loop;
end dumpSymbolTable;


-----------------------------------------------------------------------------
-- Scanner Housekeeping
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  SHUTDOWN SCANNER
--
-- Shutdown the scanner.  Run shutdown for the various SparForte packages.
-- Clear the symbol table and block (scope) table.
-----------------------------------------------------------------------------

procedure shutdownScanner is
begin
  ShutdownLogs;
  ShutdownTemplates;
  ShutdownTinyServe;
  ShutdownHashIO;
  ShutdownBTreeIO;
  ShutdownBDB;
  ShutdownSessions;
  ShutdownTeams;
  ShutdownDHT;
  ShutdownDoubly;
  ShutdownVectors;
  ShutdownContainers;
  ShutdownChains;
  ShutdownExceptions;
  ShutdownGnatCGI;
  ShutdownSound;
  ShutdownGnatCrc;
  ShutdownMemcache;
  ShutdownDirOps;
  ShutdownPen;
  ShutdownStats;
  ShutdownNumerics;
  ShutdownStrings;
  ShutdownCommandLine;
  ShutdownLockFiles;
  ShutdownTextIO;
  ShutdownCGI;
  ShutdownCalendar;
  ShutdownUnits;
  ShutdownFiles;
  ShutdownRecords;
  ShutdownEnums;
  ShutdownArrays;
  ShutdownMySQLM;
  ShutdownMySQL;
  ShutdownDBM;
  ShutdownDB;
  ShutdownSparOS;
  ShutdownPragmas;

  -- Clear design constraints

  EnforcedLocalDesignConstraintLists.Clear( enforcedLocalDesignConstraintList );
  EnforcedDesignConstraintLists.Clear( enforcedDesignConstraintList );
  DesignConstraintLists.Clear( designConstraintList );
  EnforcedLocalDesignAffinityLists.Clear( enforcedLocalDesignAffinityList );
  EnforcedDesignAffinityLists.Clear( enforcedDesignAffinityList );
  DesignAffinityLists.Clear( designAffinityList );

  -- Deallocate arrays and resources before the symbol and block tables
  -- is cleared

  -- pullArrayBlock( 1 );
  pullResourceBlock( 1 );

  -- Clear the block and identifier symbol table, just in case the
  -- scanner should be started again later.

  identifiers_top := identifiers'first;                       -- no keywords
  blocks_top := block'first;                                  -- no blocks
end shutdownScanner;


-----------------------------------------------------------------------------
--  DECLARE ASCII CHARACTERS
--
-- these are pulled out of reset scanner to avoid large function warnings
-- on Red Hat 6
-----------------------------------------------------------------------------

  procedure declareASCIICharacters is
  begin
     declareNamespace( "ASCII" );
     declareStandardConstant( "ASCII.NUL", character_t, "" & ASCII.NUL );
     declareStandardConstant( "ASCII.SOH", character_t, "" & ASCII.soh );
     declareStandardConstant( "ASCII.STX", character_t, "" & ASCII.stx );
     declareStandardConstant( "ASCII.ETX", character_t, "" & ASCII.etx );
     declareStandardConstant( "ASCII.EOT", character_t, "" & ASCII.eot );
     declareStandardConstant( "ASCII.ENQ", character_t, "" & ASCII.enq );
     declareStandardConstant( "ASCII.ACK", character_t, "" & ASCII.ack );
     declareStandardConstant( "ASCII.BEL", character_t, "" & ASCII.bel );
     declareStandardConstant( "ASCII.BS",  character_t, "" & ASCII.bs );
     declareStandardConstant( "ASCII.HT",  character_t, "" & ASCII.ht );
     declareStandardConstant( "ASCII.LF",  character_t, "" & ASCII.lf );
     declareStandardConstant( "ASCII.VT",  character_t, "" & ASCII.vt );
     declareStandardConstant( "ASCII.FF",  character_t, "" & ASCII.ff );
     declareStandardConstant( "ASCII.CR",  character_t, "" & ASCII.cr );
     declareStandardConstant( "ASCII.SO",  character_t, "" & ASCII.so );
     declareStandardConstant( "ASCII.SI",  character_t, "" & ASCII.si );
     declareStandardConstant( "ASCII.DLE", character_t, "" & ASCII.dle );
     declareStandardConstant( "ASCII.DC1", character_t, "" & ASCII.dc1 );
     declareStandardConstant( "ASCII.DC2", character_t, "" & ASCII.dc2 );
     declareStandardConstant( "ASCII.DC3", character_t, "" & ASCII.dc3 );
     declareStandardConstant( "ASCII.DC4", character_t, "" & ASCII.dc4 );
     declareStandardConstant( "ASCII.NAK", character_t, "" & ASCII.nak );
     declareStandardConstant( "ASCII.SYN", character_t, "" & ASCII.syn );
     declareStandardConstant( "ASCII.ETB", character_t, "" & ASCII.etb );
     declareStandardConstant( "ASCII.CAN", character_t, "" & ASCII.can );
     declareStandardConstant( "ASCII.EM",  character_t, "" & ASCII.em );
     declareStandardConstant( "ASCII.SUB", character_t, "" & ASCII.sub );
     declareStandardConstant( "ASCII.ESC", character_t, "" & ASCII.esc );
     declareStandardConstant( "ASCII.FS",  character_t, "" & ASCII.fs );
     declareStandardConstant( "ASCII.GS",  character_t, "" & ASCII.gs );
     declareStandardConstant( "ASCII.RS",  character_t, "" & ASCII.rs );
     declareStandardConstant( "ASCII.US",  character_t, "" & ASCII.us );
     declareStandardConstant( "ASCII.DEL", character_t, "" & ASCII.del );
     declareStandardConstant( "ASCII.Exclam",    character_t, "!" );
     declareStandardConstant( "ASCII.Quotation", character_t, """" );
     declareStandardConstant( "ASCII.Sharp",     character_t, "#" );
     declareStandardConstant( "ASCII.Dollar",    character_t, "$" );
     declareStandardConstant( "ASCII.Percent",   character_t, "%" );
     declareStandardConstant( "ASCII.Ampersand", character_t, "&" );
     declareStandardConstant( "ASCII.Colon",     character_t, ":" );
     declareStandardConstant( "ASCII.Semicolon", character_t, ";" );
     declareStandardConstant( "ASCII.Query",     character_t, "?" );
     declareStandardConstant( "ASCII.At_Sign",   character_t, "@" );
     declareStandardConstant( "ASCII.L_Bracket", character_t, "[" );
     declareStandardConstant( "ASCII.Back_Slash",character_t, "\" );
     declareStandardConstant( "ASCII.R_Bracket", character_t, "]" );
     declareStandardConstant( "ASCII.Circumflex",character_t, "^" );
     declareStandardConstant( "ASCII.Underline", character_t, "_" );
     declareStandardConstant( "ASCII.Grave",     character_t, "`" );
     declareStandardConstant( "ASCII.L_Brace",   character_t, "{" );
     declareStandardConstant( "ASCII.Bar",       character_t, "|" );
     declareStandardConstant( "ASCII.R_Brace",   character_t, "}" );
     declareStandardConstant( "ASCII.Tilde",     character_t, "~" );
     declareStandardConstant( "ASCII.LC_A",      character_t, "a" );
     declareStandardConstant( "ASCII.LC_B",      character_t, "b" );
     declareStandardConstant( "ASCII.LC_C",      character_t, "c" );
     declareStandardConstant( "ASCII.LC_D",      character_t, "d" );
     declareStandardConstant( "ASCII.LC_E",      character_t, "e" );
     declareStandardConstant( "ASCII.LC_F",      character_t, "f" );
     declareStandardConstant( "ASCII.LC_G",      character_t, "g" );
     declareStandardConstant( "ASCII.LC_H",      character_t, "h" );
     declareStandardConstant( "ASCII.LC_I",      character_t, "i" );
     declareStandardConstant( "ASCII.LC_J",      character_t, "j" );
     declareStandardConstant( "ASCII.LC_K",      character_t, "k" );
     declareStandardConstant( "ASCII.LC_L",      character_t, "l" );
     declareStandardConstant( "ASCII.LC_M",      character_t, "m" );
     declareStandardConstant( "ASCII.LC_N",      character_t, "n" );
     declareStandardConstant( "ASCII.LC_O",      character_t, "o" );
     declareStandardConstant( "ASCII.LC_P",      character_t, "p" );
     declareStandardConstant( "ASCII.LC_Q",      character_t, "q" );
     declareStandardConstant( "ASCII.LC_R",      character_t, "r" );
     declareStandardConstant( "ASCII.LC_S",      character_t, "s" );
     declareStandardConstant( "ASCII.LC_T",      character_t, "t" );
     declareStandardConstant( "ASCII.LC_U",      character_t, "u" );
     declareStandardConstant( "ASCII.LC_V",      character_t, "v" );
     declareStandardConstant( "ASCII.LC_W",      character_t, "w" );
     declareStandardConstant( "ASCII.LC_X",      character_t, "x" );
     declareStandardConstant( "ASCII.LC_Y",      character_t, "y" );
     declareStandardConstant( "ASCII.LC_Z",      character_t, "z" );
     declareStandardConstant( "ASCII.UC_A",      character_t, "A" );
     declareStandardConstant( "ASCII.UC_B",      character_t, "B" );
     declareStandardConstant( "ASCII.UC_C",      character_t, "C" );
     declareStandardConstant( "ASCII.UC_D",      character_t, "D" );
     declareStandardConstant( "ASCII.UC_E",      character_t, "E" );
     declareStandardConstant( "ASCII.UC_F",      character_t, "F" );
     declareStandardConstant( "ASCII.UC_G",      character_t, "G" );
     declareStandardConstant( "ASCII.UC_H",      character_t, "H" );
     declareStandardConstant( "ASCII.UC_I",      character_t, "I" );
     declareStandardConstant( "ASCII.UC_J",      character_t, "J" );
     declareStandardConstant( "ASCII.UC_K",      character_t, "K" );
     declareStandardConstant( "ASCII.UC_L",      character_t, "L" );
     declareStandardConstant( "ASCII.UC_M",      character_t, "M" );
     declareStandardConstant( "ASCII.UC_N",      character_t, "N" );
     declareStandardConstant( "ASCII.UC_O",      character_t, "O" );
     declareStandardConstant( "ASCII.UC_P",      character_t, "P" );
     declareStandardConstant( "ASCII.UC_Q",      character_t, "Q" );
     declareStandardConstant( "ASCII.UC_R",      character_t, "R" );
     declareStandardConstant( "ASCII.UC_S",      character_t, "S" );
     declareStandardConstant( "ASCII.UC_T",      character_t, "T" );
     declareStandardConstant( "ASCII.UC_U",      character_t, "U" );
     declareStandardConstant( "ASCII.UC_V",      character_t, "V" );
     declareStandardConstant( "ASCII.UC_W",      character_t, "W" );
     declareStandardConstant( "ASCII.UC_X",      character_t, "X" );
     declareStandardConstant( "ASCII.UC_Y",      character_t, "Y" );
     declareStandardConstant( "ASCII.UC_Z",      character_t, "Z" );
     declareNamespaceClosed( "ASCII" );
  end declareASCIICharacters;


-----------------------------------------------------------------------------
--  DECLARE LATIN1 CHARACTERS
--
-- these are pulled out of reset scanner to avoid large function warnings
-- on Red Hat 6
-----------------------------------------------------------------------------

  procedure declareLatin1Characters is
  begin
     declareNamespace( "Latin_1" );

     ------------------------
     -- Control Characters --
     ------------------------

     declareStandardConstant( "Latin_1.NUL", character_t, "" & Character'Val (0));
     declareStandardConstant( "Latin_1.SOH", character_t, "" & Character'Val (1));
     declareStandardConstant( "Latin_1.STX", character_t, "" & Character'Val (2));
     declareStandardConstant( "Latin_1.ETX", character_t, "" & Character'Val (3));
     declareStandardConstant( "Latin_1.EOT", character_t, "" & Character'Val (4));
     declareStandardConstant( "Latin_1.ENQ", character_t, "" & Character'Val (5));
     declareStandardConstant( "Latin_1.ACK", character_t, "" & Character'Val (6));
     declareStandardConstant( "Latin_1.BEL", character_t, "" & Character'Val (7));
     declareStandardConstant( "Latin_1.BS", character_t,  "" & Character'Val (8));
     declareStandardConstant( "Latin_1.HT", character_t,  "" & Character'Val (9));
     declareStandardConstant( "Latin_1.LF", character_t,  "" & Character'Val (10));
     declareStandardConstant( "Latin_1.VT", character_t,  "" & Character'Val (11));
     declareStandardConstant( "Latin_1.FF", character_t,  "" & Character'Val (12));
     declareStandardConstant( "Latin_1.CR", character_t, "" & Character'Val (13));
     declareStandardConstant( "Latin_1.SO", character_t, "" & Character'Val (14));
     declareStandardConstant( "Latin_1.SI", character_t, "" & Character'Val (15));

     declareStandardConstant( "Latin_1.DLE", character_t, "" & Character'Val (16));
     declareStandardConstant( "Latin_1.DC1", character_t, "" & Character'Val (17));
     declareStandardConstant( "Latin_1.DC2", character_t, "" & Character'Val (18));
     declareStandardConstant( "Latin_1.DC3", character_t, "" & Character'Val (19));
     declareStandardConstant( "Latin_1.DC4", character_t, "" & Character'Val (20));
     declareStandardConstant( "Latin_1.NAK", character_t, "" & Character'Val (21));
     declareStandardConstant( "Latin_1.SYN", character_t, "" & Character'Val (22));
     declareStandardConstant( "Latin_1.ETB", character_t, "" & Character'Val (23));
     declareStandardConstant( "Latin_1.CAN", character_t, "" & Character'Val (24));
     declareStandardConstant( "Latin_1.EM", character_t, "" & Character'Val (25));
     declareStandardConstant( "Latin_1.SUB", character_t, "" & Character'Val (26));
     declareStandardConstant( "Latin_1.ESC", character_t, "" & Character'Val (27));
     declareStandardConstant( "Latin_1.FS", character_t, "" & Character'Val (28));
     declareStandardConstant( "Latin_1.GS", character_t, "" & Character'Val (29));
     declareStandardConstant( "Latin_1.RS", character_t, "" & Character'Val (30));
     declareStandardConstant( "Latin_1.US", character_t, "" & Character'Val (31));

     --------------------------------
     -- ISO 646 Graphic Characters --
     --------------------------------

     declareStandardConstant( "Latin_1.Space", character_t, " ");
     declareStandardConstant( "Latin_1.Exclamation", character_t, "!");
     declareStandardConstant( "Latin_1.Quotation", character_t, """");
     declareStandardConstant( "Latin_1.Number_Sign", character_t, "#");
     declareStandardConstant( "Latin_1.Dollar_Sign", character_t, "$");
     declareStandardConstant( "Latin_1.Percent_Sign", character_t, "%");
     declareStandardConstant( "Latin_1.Ampersand", character_t, "&");
     declareStandardConstant( "Latin_1.Apostrophe", character_t, "'");
     declareStandardConstant( "Latin_1.Left_Parenthesis", character_t, "(");
     declareStandardConstant( "Latin_1.Right_Parenthesis", character_t, ")");
     declareStandardConstant( "Latin_1.Asterisk", character_t, "*");
     declareStandardConstant( "Latin_1.Plus_Sign", character_t, "+");
     declareStandardConstant( "Latin_1.Comma", character_t, ",");
     declareStandardConstant( "Latin_1.Hyphen", character_t, "-");
     declareStandardConstant( "Latin_1.Minus_Sign", character_t, "-");
     declareStandardConstant( "Latin_1.Full_Stop", character_t, ".");
     declareStandardConstant( "Latin_1.Solidus", character_t, "/");

     --  Decimal digits '0' though '9' are at positions 48 through 57

     declareStandardConstant( "Latin_1.Colon", character_t, ":");
     declareStandardConstant( "Latin_1.Semicolon", character_t, ";");
     declareStandardConstant( "Latin_1.Less_Than_Sign", character_t, "<");
     declareStandardConstant( "Latin_1.Equals_Sign", character_t, "=");
     declareStandardConstant( "Latin_1.Greater_Than_Sign", character_t, ">");
     declareStandardConstant( "Latin_1.Question", character_t, "?");

     declareStandardConstant( "Latin_1.Commercial_At", character_t, "@");

     --  Letters 'A' through 'Z' are at positions 65 through 90

     declareStandardConstant( "Latin_1.Left_Square_Bracket", character_t, "[");
     declareStandardConstant( "Latin_1.Reverse_Solidus", character_t, "\");
     declareStandardConstant( "Latin_1.Right_Square_Bracket", character_t, "]");
     declareStandardConstant( "Latin_1.Circumflex", character_t, "^");
     declareStandardConstant( "Latin_1.Low_Line", character_t, "_");

     declareStandardConstant( "Latin_1.Grave", character_t, "`");
     declareStandardConstant( "Latin_1.LC_A", character_t, "a");
     declareStandardConstant( "Latin_1.LC_B", character_t, "b");
     declareStandardConstant( "Latin_1.LC_C", character_t, "c");
     declareStandardConstant( "Latin_1.LC_D", character_t, "d");
     declareStandardConstant( "Latin_1.LC_E", character_t, "e");
     declareStandardConstant( "Latin_1.LC_F", character_t, "f");
     declareStandardConstant( "Latin_1.LC_G", character_t, "g");
     declareStandardConstant( "Latin_1.LC_H", character_t, "h");
     declareStandardConstant( "Latin_1.LC_I", character_t, "i");
     declareStandardConstant( "Latin_1.LC_J", character_t, "j");
     declareStandardConstant( "Latin_1.LC_K", character_t, "k");
     declareStandardConstant( "Latin_1.LC_L", character_t, "l");
     declareStandardConstant( "Latin_1.LC_M", character_t, "m");
     declareStandardConstant( "Latin_1.LC_N", character_t, "n");
     declareStandardConstant( "Latin_1.LC_O", character_t, "o");
     declareStandardConstant( "Latin_1.LC_P", character_t, "p");
     declareStandardConstant( "Latin_1.LC_Q", character_t, "q");
     declareStandardConstant( "Latin_1.LC_R", character_t, "r");
     declareStandardConstant( "Latin_1.LC_S", character_t, "s");
     declareStandardConstant( "Latin_1.LC_T", character_t, "t");
     declareStandardConstant( "Latin_1.LC_U", character_t, "u");
     declareStandardConstant( "Latin_1.LC_V", character_t, "v");
     declareStandardConstant( "Latin_1.LC_W", character_t, "w");
     declareStandardConstant( "Latin_1.LC_X", character_t, "x");
     declareStandardConstant( "Latin_1.LC_Y", character_t, "y");
     declareStandardConstant( "Latin_1.LC_Z", character_t, "z");
     declareStandardConstant( "Latin_1.Left_Curly_Bracket", character_t, "{");
     declareStandardConstant( "Latin_1.Vertical_Line", character_t, "|");
     declareStandardConstant( "Latin_1.Right_Curly_Bracket", character_t, "}");
     declareStandardConstant( "Latin_1.Tilde", character_t, "~");
     declareStandardConstant( "Latin_1.DEL", character_t, "" & Character'Val (127));

     ---------------------------------
     -- ISO 6429 Control Characters --
     ---------------------------------

     declareStandardConstant( "Latin_1.IS4", character_t, "" & Character'Val (28));
     declareStandardConstant( "Latin_1.IS3", character_t, "" & Character'Val (29));
     declareStandardConstant( "Latin_1.IS2", character_t, "" & Character'Val (30));
     declareStandardConstant( "Latin_1.IS1", character_t, "" & Character'Val (31));

     declareStandardConstant( "Latin_1.Reserved_128", character_t, "" & Character'Val (128));
     declareStandardConstant( "Latin_1.Reserved_129", character_t, "" & Character'Val (129));
     declareStandardConstant( "Latin_1.BPH", character_t, "" & Character'Val (130));
     declareStandardConstant( "Latin_1.NBH", character_t, "" & Character'Val (131));
     declareStandardConstant( "Latin_1.Reserved_132", character_t, "" & Character'Val (132));
     declareStandardConstant( "Latin_1.NEL", character_t, "" & Character'Val (133));
     declareStandardConstant( "Latin_1.SSA", character_t, "" & Character'Val (134));
     declareStandardConstant( "Latin_1.ESA", character_t, "" & Character'Val (135));
     declareStandardConstant( "Latin_1.HTS", character_t, "" & Character'Val (136));
     declareStandardConstant( "Latin_1.HTJ", character_t, "" & Character'Val (137));
     declareStandardConstant( "Latin_1.VTS", character_t, "" & Character'Val (138));
     declareStandardConstant( "Latin_1.PLD", character_t, "" & Character'Val (139));
     declareStandardConstant( "Latin_1.PLU", character_t, "" & Character'Val (140));
     declareStandardConstant( "Latin_1.RI", character_t, "" & Character'Val (141));
     declareStandardConstant( "Latin_1.SS2", character_t, "" & Character'Val (142));
     declareStandardConstant( "Latin_1.SS3", character_t, "" & Character'Val (143));

     declareStandardConstant( "Latin_1.DCS", character_t, "" & Character'Val (144));
     declareStandardConstant( "Latin_1.PU1", character_t, "" & Character'Val (145));
     declareStandardConstant( "Latin_1.PU2", character_t, "" & Character'Val (146));
     declareStandardConstant( "Latin_1.STS", character_t, "" & Character'Val (147));
     declareStandardConstant( "Latin_1.CCH", character_t, "" & Character'Val (148));
     declareStandardConstant( "Latin_1.MW", character_t, "" & Character'Val (149));
     declareStandardConstant( "Latin_1.SPA", character_t, "" & Character'Val (150));
     declareStandardConstant( "Latin_1.EPA", character_t, "" & Character'Val (151));

     declareStandardConstant( "Latin_1.SOS", character_t, "" & Character'Val (152));
     declareStandardConstant( "Latin_1.Reserved_153", character_t, "" & Character'Val (153));
     declareStandardConstant( "Latin_1.SCI", character_t, "" & Character'Val (154));
     declareStandardConstant( "Latin_1.CSI", character_t, "" & Character'Val (155));
     declareStandardConstant( "Latin_1.ST", character_t, "" & Character'Val (156));
     declareStandardConstant( "Latin_1.OSC", character_t, "" & Character'Val (157));
     declareStandardConstant( "Latin_1.PM", character_t, "" & Character'Val (158));
     declareStandardConstant( "Latin_1.APC", character_t, "" & Character'Val (159));

      ------------------------------
      -- Other Graphic Characters --
      ------------------------------

      --  Character positions 160 (16#A0#) .. 175 (16#AF#)

     declareStandardConstant( "Latin_1.No_Break_Space", character_t, "" & Character'Val (160));
     declareStandardConstant( "Latin_1.NBSP", character_t, "" & Character'Val (160));
     declareStandardConstant( "Latin_1.Inverted_Exclamation", character_t, "" & Character'Val (161));
     declareStandardConstant( "Latin_1.Cent_Sign", character_t, "" & Character'Val (162));
     declareStandardConstant( "Latin_1.Pound_Sign", character_t, "" & Character'Val (163));
     declareStandardConstant( "Latin_1.Currency_Sign", character_t, "" & Character'Val (164));
     declareStandardConstant( "Latin_1.Yen_Sign", character_t, "" & Character'Val (165));
     declareStandardConstant( "Latin_1.Broken_Bar", character_t, "" & Character'Val (166));
     declareStandardConstant( "Latin_1.Section_Sign", character_t, "" & Character'Val (167));
     declareStandardConstant( "Latin_1.Diaeresis", character_t, "" & Character'Val (168));
     declareStandardConstant( "Latin_1.Copyright_Sign", character_t, "" & Character'Val (169));
     declareStandardConstant( "Latin_1.Feminine_Ordinal_Indicator", character_t, "" & Character'Val (170));
     declareStandardConstant( "Latin_1.Left_Angle_Quotation", character_t, "" & Character'Val (171));
     declareStandardConstant( "Latin_1.Not_Sign", character_t, "" & Character'Val (172));
     declareStandardConstant( "Latin_1.Soft_Hyphen", character_t, "" & Character'Val (173));
     declareStandardConstant( "Latin_1.Registered_Trade_Mark_Sign", character_t, "" & Character'Val (174));
     declareStandardConstant( "Latin_1.Macron", character_t, "" & Character'Val (175));

      --  Character positions 176 (16#B0#) .. 191 (16#BF#)

     declareStandardConstant( "Latin_1.Degree_Sign", character_t, "" & Character'Val (176));
      declareStandardConstant( "Latin_1.Ring_Above", character_t, "" & "" & Character'Val (176));
     declareStandardConstant( "Latin_1.Plus_Minus_Sign", character_t, "" & Character'Val (177));
     declareStandardConstant( "Latin_1.Superscript_Two", character_t, "" & Character'Val (178));
     declareStandardConstant( "Latin_1.Superscript_Three", character_t, "" & Character'Val (179));
     declareStandardConstant( "Latin_1.Acute", character_t, "" & Character'Val (180));
     declareStandardConstant( "Latin_1.Micro_Sign", character_t, "" & Character'Val (181));
     declareStandardConstant( "Latin_1.Pilcrow_Sign", character_t, "" & Character'Val (182));
     declareStandardConstant( "Latin_1.Paragraph_Sign", character_t, "" & Character'Val (182));
     declareStandardConstant( "Latin_1.Middle_Dot", character_t, "" & Character'Val (183));
     declareStandardConstant( "Latin_1.Cedilla", character_t, "" & Character'Val (184));
     declareStandardConstant( "Latin_1.Superscript_One", character_t, "" & Character'Val (185));
     declareStandardConstant( "Latin_1.Masculine_Ordinal_Indicator", character_t, "" & Character'Val (186));
     declareStandardConstant( "Latin_1.Right_Angle_Quotation", character_t, "" & Character'Val (187));
     declareStandardConstant( "Latin_1.Fraction_One_Quarter", character_t, "" & Character'Val (188));
     declareStandardConstant( "Latin_1.Fraction_One_Half", character_t, "" & Character'Val (189));
     declareStandardConstant( "Latin_1.Fraction_Three_Quarters", character_t, "" & Character'Val (190));
     declareStandardConstant( "Latin_1.Inverted_Question", character_t, "" & Character'Val (191));

      --  Character positions 192 (16#C0#) .. 207 (16#CF#)

     declareStandardConstant( "Latin_1.UC_A_Grave", character_t, "" & Character'Val (192));
     declareStandardConstant( "Latin_1.UC_A_Acute", character_t, "" & Character'Val (193));
     declareStandardConstant( "Latin_1.UC_A_Circumflex", character_t, "" & Character'Val (194));
     declareStandardConstant( "Latin_1.UC_A_Tilde", character_t, "" & Character'Val (195));
     declareStandardConstant( "Latin_1.UC_A_Diaeresis", character_t, "" & Character'Val (196));
     declareStandardConstant( "Latin_1.UC_A_Ring", character_t, "" & Character'Val (197));
     declareStandardConstant( "Latin_1.UC_AE_Diphthong", character_t, "" & Character'Val (198));
     declareStandardConstant( "Latin_1.UC_C_Cedilla", character_t, "" & Character'Val (199));
     declareStandardConstant( "Latin_1.UC_E_Grave", character_t, "" & Character'Val (200));
     declareStandardConstant( "Latin_1.UC_E_Acute", character_t, "" & Character'Val (201));
     declareStandardConstant( "Latin_1.UC_E_Circumflex", character_t, "" & Character'Val (202));
     declareStandardConstant( "Latin_1.UC_E_Diaeresis", character_t, "" & Character'Val (203));
     declareStandardConstant( "Latin_1.UC_I_Grave", character_t, "" & Character'Val (204));
     declareStandardConstant( "Latin_1.UC_I_Acute", character_t, "" & Character'Val (205));
     declareStandardConstant( "Latin_1.UC_I_Circumflex", character_t, "" & Character'Val (206));
     declareStandardConstant( "Latin_1.UC_I_Diaeresis", character_t, "" & Character'Val (207));

      --  Character positions 208 (16#D0#) .. 223 (16#DF#)

     declareStandardConstant( "Latin_1.UC_Icelandic_Eth", character_t, "" & Character'Val (208));
     declareStandardConstant( "Latin_1.UC_N_Tilde", character_t, "" & Character'Val (209));
     declareStandardConstant( "Latin_1.UC_O_Grave", character_t, "" & Character'Val (210));
     declareStandardConstant( "Latin_1.UC_O_Acute", character_t, "" & Character'Val (211));
     declareStandardConstant( "Latin_1.UC_O_Circumflex", character_t, "" & Character'Val (212));
     declareStandardConstant( "Latin_1.UC_O_Tilde", character_t, "" & Character'Val (213));
     declareStandardConstant( "Latin_1.UC_O_Diaeresis", character_t, "" & Character'Val (214));
     declareStandardConstant( "Latin_1.Multiplication_Sign", character_t, "" & Character'Val (215));
     declareStandardConstant( "Latin_1.UC_O_Oblique_Stroke", character_t, "" & Character'Val (216));
     declareStandardConstant( "Latin_1.UC_U_Grave", character_t, "" & Character'Val (217));
     declareStandardConstant( "Latin_1.UC_U_Acute", character_t, "" & Character'Val (218));
     declareStandardConstant( "Latin_1.UC_U_Circumflex", character_t, "" & Character'Val (219));
     declareStandardConstant( "Latin_1.UC_U_Diaeresis", character_t, "" & Character'Val (220));
     declareStandardConstant( "Latin_1.UC_Y_Acute", character_t, "" & Character'Val (221));
     declareStandardConstant( "Latin_1.UC_Icelandic_Thorn", character_t, "" & Character'Val (222));
     declareStandardConstant( "Latin_1.LC_German_Sharp_S", character_t, "" & Character'Val (223));

      --  Character positions 224 (16#E0#) .. 239 (16#EF#)

     declareStandardConstant( "Latin_1.LC_A_Grave", character_t, "" & Character'Val (224));
     declareStandardConstant( "Latin_1.LC_A_Acute", character_t, "" & Character'Val (225));
     declareStandardConstant( "Latin_1.LC_A_Circumflex", character_t, "" & Character'Val (226));
     declareStandardConstant( "Latin_1.LC_A_Tilde", character_t, "" & Character'Val (227));
     declareStandardConstant( "Latin_1.LC_A_Diaeresis", character_t, "" & Character'Val (228));
     declareStandardConstant( "Latin_1.LC_A_Ring", character_t, "" & Character'Val (229));
     declareStandardConstant( "Latin_1.LC_AE_Diphthong", character_t, "" & Character'Val (230));
     declareStandardConstant( "Latin_1.LC_C_Cedilla", character_t, "" & Character'Val (231));
     declareStandardConstant( "Latin_1.LC_E_Grave", character_t, "" & Character'Val (232));
     declareStandardConstant( "Latin_1.LC_E_Acute", character_t, "" & Character'Val (233));
     declareStandardConstant( "Latin_1.LC_E_Circumflex", character_t, "" & Character'Val (234));
     declareStandardConstant( "Latin_1.LC_E_Diaeresis", character_t, "" & Character'Val (235));
     declareStandardConstant( "Latin_1.LC_I_Grave", character_t, "" & Character'Val (236));
     declareStandardConstant( "Latin_1.LC_I_Acute", character_t, "" & Character'Val (237));
     declareStandardConstant( "Latin_1.LC_I_Circumflex", character_t, "" & Character'Val (238));
     declareStandardConstant( "Latin_1.LC_I_Diaeresis", character_t, "" & Character'Val (239));

      --  Character positions 240 (16#F0#) .. 255 (16#FF)
     declareStandardConstant( "Latin_1.LC_Icelandic_Eth", character_t, "" & Character'Val (240));
     declareStandardConstant( "Latin_1.LC_N_Tilde", character_t, "" & Character'Val (241));
     declareStandardConstant( "Latin_1.LC_O_Grave", character_t, "" & Character'Val (242));
     declareStandardConstant( "Latin_1.LC_O_Acute", character_t, "" & Character'Val (243));
     declareStandardConstant( "Latin_1.LC_O_Circumflex", character_t, "" & Character'Val (244));
     declareStandardConstant( "Latin_1.LC_O_Tilde", character_t, "" & Character'Val (245));
     declareStandardConstant( "Latin_1.LC_O_Diaeresis", character_t, "" & Character'Val (246));
     declareStandardConstant( "Latin_1.Division_Sign", character_t, "" & Character'Val (247));
     declareStandardConstant( "Latin_1.LC_O_Oblique_Stroke", character_t, "" & Character'Val (248));
     declareStandardConstant( "Latin_1.LC_U_Grave", character_t, "" & Character'Val (249));
     declareStandardConstant( "Latin_1.LC_U_Acute", character_t, "" & Character'Val (250));
     declareStandardConstant( "Latin_1.LC_U_Circumflex", character_t, "" & Character'Val (251));
     declareStandardConstant( "Latin_1.LC_U_Diaeresis", character_t, "" & Character'Val (252));
     declareStandardConstant( "Latin_1.LC_Y_Acute", character_t, "" & Character'Val (253));
     declareStandardConstant( "Latin_1.LC_Icelandic_Thorn", character_t, "" & Character'Val (254));
     declareStandardConstant( "Latin_1.LC_Y_Diaeresis", character_t, "" & Character'Val (255));
     declareNamespaceClosed( "Latin_1" );
  end declareLatin1Characters;


-----------------------------------------------------------------------------
--  DECLARE STANDARD TYPES
--
-- these are pulled out of reset scanner to avoid large function warnings
-- on Red Hat 6
-----------------------------------------------------------------------------

procedure declareStandardTypes is
begin
  declareIdent( root_enumerated_t, "root enumerated", variable_t, typeClass );
  declareIdent( root_record_t, "root record", variable_t, typeClass );
  declareIdent( command_t, "command", variable_t, typeClass );
  declareIdent( file_type_t, "file_type", variable_t, typeClass );
  identifiers( file_type_t ).usage := limitedUsage;
  identifiers( identifiers_top-1 ).usage := limitedUsage;
  declareIdent( socket_type_t, "socket_type", variable_t, typeClass );
  identifiers( identifiers_top-1 ).usage := limitedUsage;
  declareIdent( universal_t, "universal_typeless", variable_t, typeClass );
  declareIdent( integer_t, "integer", uni_numeric_t, typeClass );
  --declareIdent( natural_t, "natural", uni_numeric_t, typeClass );
  --declareIdent( positive_t, "positive", uni_numeric_t, typeClass );
  declareIdent( natural_t, "natural", integer_t, subClass );
  declareIdent( positive_t, "positive", integer_t, subClass );
  declareIdent( short_short_integer_t, "short_short_integer", uni_numeric_t, typeClass );
  declareIdent( short_integer_t, "short_integer", uni_numeric_t, typeClass );
  declareIdent( long_integer_t, "long_integer", uni_numeric_t, typeClass );
  declareIdent( long_long_integer_t, "long_long_integer", uni_numeric_t, typeClass );
  declareIdent( character_t, "character", uni_string_t, typeClass );
  declareIdent( float_t, "float", uni_numeric_t, typeClass );
  declareIdent( short_float_t, "short_float", uni_numeric_t, typeClass );
  declareIdent( long_float_t, "long_float", uni_numeric_t, typeClass );
  declareIdent( boolean_t, "boolean", root_enumerated_t, typeClass );
  declareIdent( string_t, "string", uni_string_t, typeClass );
  declareIdent( duration_t, "duration", uni_numeric_t, typeClass );
  declareIdent( file_mode_t, "file_mode", root_enumerated_t, typeClass );
  declareIdent( unbounded_string_t, "unbounded_string", uni_string_t, typeClass );
  declareIdent( complex_t, "complex", root_record_t, typeClass );
  identifiers( complex_t ).value.all := to_unbounded_string( "2" );
  declareIdent( complex_real_t, "complex.re", long_float_t, subClass );
  identifiers( complex_real_t ).field_of := complex_t;
  identifiers( complex_real_t ).value.all := to_unbounded_string( "1" );
  declareIdent( complex_imaginary_t, "complex.im", long_float_t, subClass );
  identifiers( complex_imaginary_t ).field_of := complex_t;
  identifiers( complex_imaginary_t ).value.all := to_unbounded_string( "2" );
end declareStandardTypes;


-----------------------------------------------------------------------------
--  DECLARE STANDARD PACKAGE
--
-- these are pulled out of reset scanner to avoid large function warnings
-- on Red Hat 6
-----------------------------------------------------------------------------

procedure declareStandardPackage is
begin
  declareNamespace( "System" );
  declareStandardConstant( "System.System_Name", uni_string_t, "SYSTEM_NAME_SPARFORTE" );
  declareStandardConstant( "System.Min_Int", uni_numeric_t, to_string( to_unbounded_string( long_float( integerOutputType'first+0.9 ) ) ) );
  -- out minimum integer is the limit of a long_float's mantissa.  should
  -- probably check that system.min_int isn't smaller, but Gnat gives bogus
  -- result on long_float( max_int ) if mantissa isn't big enough.
  declareStandardConstant( "System.Max_Int", uni_numeric_t, to_string( to_unbounded_string( maxInteger ) ) );
  -- out maximum integer is the limit of a long_float's mantissa
  -- probably check that system.min_max isn't smaller
  declareStandardConstant( "System.Max_Binary_Modulus", uni_numeric_t,
    long_long_float'image( long_long_float( system.max_binary_modulus ) ) );
  declareStandardConstant( "System.Max_Nonbinary_Modulus", uni_numeric_t,
    long_long_float'image( long_long_float( system.max_nonbinary_modulus ) ) );
  declareStandardConstant( "System.Max_Base_Digits", uni_numeric_t, system.max_base_digits'img );
  declareStandardConstant( "System.Max_Digits", uni_numeric_t, system.max_digits'img );
  declareStandardConstant( "System.Max_Mantissa", uni_numeric_t, system.max_mantissa'img );
  declareStandardConstant( "System.Fine_Delta", uni_numeric_t, system.fine_delta'img );
  declareStandardConstant( "System.Tick", uni_numeric_t, system.tick'img );
  declareStandardConstant( "System.Storage_Unit", uni_numeric_t, system.storage_unit'img );
  declareStandardConstant( "System.Word_Size", uni_numeric_t, system.word_size'img );
  declareStandardConstant( "System.Memory_Size", uni_numeric_t, long_float'image( long_float( system.memory_size ) ) );
  -- NOTE: This was a universal integer but memory_size of 512 MB or larger gave an 'img error
  -- so it is now a long float.
  declareStandardConstant( "System.Default_Bit_Order", uni_string_t, system.default_bit_order'img );
  declareStandardConstant( "System.Login_Shell", boolean_t,  integer'image( boolean'pos(isLoginShell))(2) & "" );
  declareStandardConstant( "System.Restricted_Shell", boolean_t, integer'image( commandLineOption'pos(rshOpt))(2) & "" );
  declareStandardConstant( "System.Script_License", uni_string_t, "" );
  declareStandardConstant( "System.Script_Software_Model", uni_string_t, "" );
  declareStandardConstant( "System.System_Version", uni_string_t, world.version );
  declareStandardConstant( "System.Design_Phase", boolean_t, integer'image( commandLineOption'pos(designOpt))(2) & "" );
  declareStandardConstant( "System.Maintenance_Phase", boolean_t, integer'image( commandLineOption'pos(maintenanceOpt))(2) & "" );
  declareStandardConstant( "System.Testing_Phase", boolean_t, integer'image( commandLineOption'pos(testOpt))(2) & "" );
  declareStandardConstant( "System.Development_Phase", boolean_t, integer'image( commandLineOption'pos( not testOpt and not maintenanceOpt and not designOpt ) )(2) & "" );
  declareNamespaceClosed( "System" );
end declareStandardPackage;


-----------------------------------------------------------------------------
--  DECLARE DEFAULT DESIGN CONSTRAINTS
--
-- 
-----------------------------------------------------------------------------

procedure declareDefaultDesignConstraints is
  dc : aDesignConstraint;
begin

  -- Erase both the declared constraints and the enforced constraint lists

  EnforcedLocalDesignConstraintLists.Clear( enforcedLocalDesignConstraintList );
  EnforcedDesignConstraintLists.Clear( enforcedDesignConstraintList );
  DesignConstraintLists.Clear( designConstraintList );
  EnforcedDesignAffinityLists.Clear( enforcedDesignAffinityList );
  EnforcedDesignAffinityLists.Clear( enforcedDesignAffinityList );
  DesignAffinityLists.Clear( designAffinityList );

  -- Predefined constraints: physical

  dc.mode := file;
  dc.constraint := to_unbounded_string( "physical" );
  dc.name :=  to_unbounded_string( "cpu" );
  dc.limit := 0.0;
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.name :=  to_unbounded_string( "memory" );
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.name :=  to_unbounded_string( "network" );
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.name :=  to_unbounded_string( "storage" );
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.mode := unique;
  dc.name :=  to_unbounded_string( "uncertain" );
  DesignConstraintLists.Queue( designConstraintList, dc );

  -- Predefined constraints: optimization

  dc.mode := file;
  dc.constraint := to_unbounded_string( "optimization" );
  dc.name := to_unbounded_string( "complexity" );
  dc.limit := 0.0;
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.name := to_unbounded_string( "maintenance" );
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.name := to_unbounded_string( "performance" );
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.name := to_unbounded_string( "reliability" );
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.name := to_unbounded_string( "reusable" );
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.name := to_unbounded_string( "scalability" );
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.name := to_unbounded_string( "size" );
  DesignConstraintLists.Queue( designConstraintList, dc );
  dc.mode := unique;
  dc.name := to_unbounded_string( "uncertain" );
  DesignConstraintLists.Queue( designConstraintList, dc );

  -- Predefined constraints: knowledge

  dc.mode := unique;
  dc.constraint := to_unbounded_string( "knowledge" );
  dc.name := to_unbounded_string( "uncertain" );
  dc.limit := 0.0;
  DesignConstraintLists.Queue( designConstraintList, dc );
end declareDefaultDesignConstraints;


-----------------------------------------------------------------------------
--  RESET SCANNER
--
-- A less harsh version of shutdownScanner / startScanner.
-- Restart the scanner by resetting scanner variables to their startup
-- values and reload the standard identifiers.  (Although keywords
-- can't be unset by the user, standard identifiers can.  The user
-- can unset "false", for example.  We'll initialize all standard
-- identifiers here to make sure they exist.  However, this raises
-- issues for nested scripts if resetScanner is used for that
-- in the future purpose.)
-----------------------------------------------------------------------------

procedure resetScanner is

  ---------------------------------------------------------------------------
  --  IMPORT ENVIRONMENT (RESET SCANNER)
  --
  -- Declare all Environment Variables.  If --import_all is not used,
  -- still declare PATH, PWD, OLDPWD, HOME, TERM if they exist.
  ---------------------------------------------------------------------------

  procedure importEnvironment is
    path_key   : constant unbounded_string := to_unbounded_string( "PATH=" );
    pwd_key    : constant unbounded_string := to_unbounded_string( "PWD=" );
    oldpwd_key : constant unbounded_string := to_unbounded_string( "OLDPWD=" );
    home_key   : constant unbounded_string := to_unbounded_string( "HOME=" );
    term_key   : constant unbounded_string := to_unbounded_string( "TERM=" );
    shell_key  : constant unbounded_string := to_unbounded_string( "SHELL=" );
    library_key: constant unbounded_string := to_unbounded_string( "SPAR_LIBRARY_PATH=" );
    tab_key    : constant unbounded_string := to_unbounded_string( "TABSIZE=" );
    ev  : unbounded_string;                                     -- an env var
  begin
     for i in 1..environmentList.Length( initialEnvironment ) loop
        environmentList.Find( initialEnvironment, i, ev );
        if Head( ev, 5 ) = path_key then
           init_env_ident( to_string( ev ) );
        elsif Head( ev, 4 ) = pwd_key then
           init_env_ident( to_string( ev ) );
        elsif Head( ev, 7 ) = oldpwd_key then
           init_env_ident( to_string( ev ) );
        elsif Head( ev, 5 ) = home_key then
           init_env_ident( to_string( ev ) );
        elsif Head( ev, 5 ) = term_key then
           init_env_ident( to_string( ev ) );
           identifiers( identifiers_top-1).export := true;
        elsif Head( ev, 6 ) = shell_key then
           init_env_ident( to_string( ev ) );
        elsif Head( ev, 18 ) = library_key then
           init_env_ident( to_string( ev ) );
        elsif Head( ev, 8 ) = tab_key then
           init_env_ident( to_string( ev ) );
        elsif importOpt then
           init_env_ident( to_string( ev ) );
        end if;
     end loop;
  end importEnvironment;

  temp_id : identifier;                                         -- unused id

begin

  -- Restore the scanner to a startup state.  Discard all identifiers
  -- on the symbol table except the reserved keywords.

  identifiers_top := reserved_top;                            -- keep keywords
  blocks_top := block'first;                                  -- no blocks
  error_found := false;                                       -- no error
  err_exception.name := null_unbounded_string;                -- no exception
  onlyAda95 := false;                                         -- no Ada_95
  depreciatedMsg := Null_Unbounded_String;                    -- nothing dep.
  exit_block := false;                                        -- not exiting
  restriction_no_auto_declarations := false;                  -- auto decl OK
  syntax_check := false;                                      -- not checking
  last_status := 0;                                           -- status OK
  done := false;                                              -- not quitting
  trace := false;                                             -- not tracing
  cmdpos := 3;                                                -- first char

  -- side-effect handling
  --
  lastExpressionInstruction := noExpressionInstruction;
  firstExpressionInstruction := noExpressionInstruction;

  -- reset namespace

  currentNamespace    := to_unbounded_string( "UNDEFINED" );
  currentNamespaceId  := identifiers'first;
  -- The following is defined in the compiler when it starts.
  -- Return to the global namespace
  lastNamespaceId     := identifiers'first;

  -- Tiny Hash Cache

  --resetTinyHashCache;

  -- Predefined types

  declareStandardTypes;

  -- The user should never see this but we need to set these to known
  -- values for the scanner.

  itself := null_unbounded_string;
  itself_type := eof_t;
  last_output := null_unbounded_string;
  last_output_type := eof_t;

  -- Boolean enumerated

  declareStandardEnum( false_t, "false", boolean_t, "0" );
  declareStandardEnum( true_t, "true", boolean_t, "1" );

  declareIdent( json_string_t, "json_string", string_t, typeClass );

  -- Standard Package constants: ASCII
  -- this is broken out to reduce the size of this procedure.  On some
  -- machines, this gives a frame size warning otherwise.

  declareASCIICharacters;

  -- Latin_1
  -- this is broken out to reduce the size of this procedure.  On some
  -- machines, this gives a frame size warning otherwise.

  declareLatin1Characters;

  -- System Package constants

  declareStandardPackage;

-- most of the source_info must be filled in later by the parser

  declareStandardConstant( source_info_file_t, "source_info.file", uni_string_t, "" );
  declareStandardConstant( source_info_line_t, "source_info.line", positive_t, "1" );
  declareStandardConstant( source_info_src_loc_t, "source_info.source_location", uni_string_t, "1:" );
  declareStandardConstant( source_info_enc_ent_t, "source_info.enclosing_entity", uni_string_t, "" );
  declareStandardConstant( source_info_script_size_t, "source_info.script_size", natural_t, "0" ); -- will be filled in later when script is loaded
  declareFunction( source_info_symbol_table_size_t, "source_info.symbol_table_size" );

  -- startup built-in packages from other modules
  --

  StartupPragmas;
  StartupSparOS;
  StartupDB;
  StartupDBM;
  StartupMySQL;
  StartupMySQLM;
  StartupArrays;
  StartupEnums;
  StartupRecords;
  StartupFiles;
  StartupUnits;
  StartupCalendar;
  StartupCGI;
  StartupTextIO;
  StartupLockFiles;
  StartupCommandLine;
  StartupStrings;
  StartupNumerics;
  StartupStats;
  StartupPen;
  StartupDirOps;
  StartupMemcache;
  StartupGnatCrc;
  StartupSound;
  StartupGnatCGI;
  StartupExceptions;
  StartupChains;
  StartupContainers;
  StartupVectors;
  StartupDoubly;
  StartupDHT;
  StartupTeams;
  StartupSessions;
  StartupBDB;
  StartupBTreeIO;
  StartupHashIO;
  StartupTinyServe;
  StartupTemplates;
  StartupLogs;

  -- Declare all Environment Variables
  --
  -- If --import_all is not used, still declare PATH, PWD, OLDPWD, HOME,
  -- TERM, SHELL
  -- if they exist.

  importEnvironment;

  -- Declare any standard shell variables that are not
  -- yet declared.  PATH, PWD, OLDPWD, HOME are necessary for SparForte
  -- to function: if they weren't imported, declare them locally.

  findIdent( to_unbounded_string( "PATH" ), temp_id );        -- PATH
  if temp_id = eof_t then                                     -- still missing?
     declareIdent( temp_id, "PATH", uni_string_t );           -- declare it
  end if;
  if rshOpt then                                              -- restricted sh?
     identifiers( temp_id).usage := constantUsage;            -- PATH is a
  end if;                                                     -- constant
  findIdent( to_unbounded_string( "PWD" ), temp_id );         -- PWD
  if temp_id = eof_t then                                     -- missing?
     declareIdent( temp_id, "PWD", uni_string_t );            -- declare it
  end if;
  -- Always set the value.  PWD could be defined in the environment
  -- by something else
  declare
    -- lookup current working directory
    -- perhaps a spar_os.pwd package is in order to share
    -- this with scanner and builtins?
    buffer : string( 1..4096 );
  begin
    C_reset_errno;
    getcwd( buffer, buffer'length );
    if C_errno = 0 then
       identifiers( temp_id ).value.all := to_unbounded_string(
           buffer( 1..index( buffer, ASCII.NUL & "" ) - 1 ) ) ;
    end if;
  end;
  if rshOpt then                                              -- restricted sh?
     identifiers( temp_id).usage := constantUsage;            -- PWD is a
  end if;                                                     -- constant
  findIdent( to_unbounded_string( "OLDPWD" ), temp_id );      -- OLDPWD
  if temp_id = eof_t then                                     -- missing?
     declareIdent( temp_id, "OLDPWD", uni_string_t );         -- declare it
  end if;
  if rshOpt then                                              -- restricted sh?
     identifiers( temp_id).usage := constantUsage;            -- OLDPWD is a
  end if;                                                     -- constant
  findIdent( to_unbounded_string( "HOME" ), temp_id );        -- HOME
  if temp_id = eof_t then                                     -- missing?
     declareIdent( temp_id, "HOME", uni_string_t );           -- declare it
  end if;
  if rshOpt then                                              -- restricted sh?
     identifiers( temp_id).usage := constantUsage;            -- HOME is a
  end if;                                                     -- constant
  findIdent( to_unbounded_string( "SHELL" ), temp_id );       -- SHELL
  if temp_id = eof_t then                                     -- missing?
     declareIdent( temp_id, "SHELL", uni_string_t );          -- declare it
     identifiers( temp_id ).export := true;
     -- not exactly right, but works...
     if C_is_executable_file( "/usr/local/bin/spar" & ASCII.NUL ) then
        identifiers( temp_id ).value.all := to_unbounded_string( "/usr/local/bin/spar" );
     elsif C_is_executable_file( "/bin/spar" & ASCII.NUL ) then
        identifiers( temp_id ).value.all := to_unbounded_string( "/bin/spar" );
     end if;
  end if;
  if rshOpt then                                              -- restricted sh?
     identifiers( temp_id).usage := constantUsage;            -- SHELL is a
  end if;                                                     -- constant
  findIdent( to_unbounded_string( "TERM" ), temp_id );        -- TERM
  if temp_id = eof_t then                                     -- missing?
     declareIdent( temp_id, "TERM", uni_string_t );           -- declare it
     identifiers( temp_id ).export := true;                   -- default xterm
     identifiers( temp_id ).value.all := to_unbounded_string( "xterm" );
  else
     -- xterm emulation?  then change the window name during interactive
     -- sessions
     if head( identifiers( temp_id ).value.all, 5 ) = "xterm" then
        terminalWindowNaming := true;
     elsif identifiers( temp_id ).value.all = "linux" then       -- could be
        findIdent( to_unbounded_string( "DISPLAY" ), temp_id );  -- a console
        if identifiers( temp_id ).value.all = "DISPLAY" then     -- hope its x
           terminalWindowNaming := true;
        end if;
     end if;
  end if;
  findIdent( to_unbounded_string( "SPAR_LIBRARY_PATH" ), temp_id ); -- SPAR_LIBRARY_PATH defined?
  if temp_id /= eof_t then                                    -- missing?
    if rshOpt then                                            -- restricted sh?
       identifiers( temp_id).usage := constantUsage;          -- it's a constant
    end if;
  end if;
  findIdent( to_unbounded_string( "TABSIZE" ), temp_id );        -- TABSIZE
  if temp_id = eof_t then                                     -- missing?
     tabSize := 8;
  else
     begin
       tabSize := natural( to_numeric( identifiers( temp_id ).value.all ) );
     exception when others =>
       tabSize := 8;
     end;
  end if;

  -- Erase both the declared constraints and the enforced constraint lists

  EnforcedLocalDesignConstraintLists.Clear( enforcedLocalDesignConstraintList );
  EnforcedDesignConstraintLists.Clear( enforcedDesignConstraintList );
  DesignConstraintLists.Clear( designConstraintList );
  EnforcedDesignAffinityLists.Clear( enforcedDesignAffinityList );
  EnforcedDesignAffinityLists.Clear( enforcedDesignAffinityList );
  DesignAffinityLists.Clear( designAffinityList );
  declareDefaultDesignConstraints;

end resetScanner;


-----------------------------------------------------------------------------
--  START SCANNER
--
-- Set up symbol table, declaring all keywords, constants, and environment
-- variables.  This should be executed once when SparForte is started, or to
-- restart the scanner after it has been shut down.  Run resetScanner.
-----------------------------------------------------------------------------

procedure startScanner is

  ---------------------------------------------------------------------------
  --  SAVE INITIAL ENVIRONMENT (START SCANNER)
  --
  -- Save a copy of the environment at startup.
  ---------------------------------------------------------------------------

  procedure saveInitialEnvironment is
    ev  : unbounded_string;                                     -- an env var
  begin
     for i in 1..Environment_Count loop
        ev := to_unbounded_string( Environment_Value( i ) );
        environmentList.Queue( initialEnvironment, ev );
     end loop;
  end saveInitialEnvironment;

begin

  maxInteger := long_float( integerOutputType'last-0.9 );

  saveInitialEnvironment;
  -- save a copy of the O/S environment

  clearHistory;

  -- The keyword used to be defined here.  There are now defined in the
  -- byte code compiler's startCompiler.

  -- Initialize all scanner variables and declare all other
  -- standard identifiers.

  resetScanner;

  -- last predefind thing
  predefined_top := identifiers_top;

  -- No last output

  last_output_type := eof_t;
end startScanner;


-----------------------------------------------------------------------------
--
-- Types
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  INIT ENVIRONMENT
--
-- Return true if variable is in O/S environment
-----------------------------------------------------------------------------

function inEnvironment( id : identifier ) return boolean is
  -- Return true if variable is in O/S environment
  key : unbounded_string;                                     -- "var="
  ev  : unbounded_string;                                     -- an env var
  exists : boolean := false;
begin
  key := identifiers( id ).name & "=";                        -- look for this
  for i in 1..Environment_Count loop                          -- all env vars
      ev := to_unbounded_string( Environment_Value( i ) );    -- get next one
      if Head( ev, length( key ) ) = key then                 -- match?
         exists := true;                                      -- found
         exit;                                                -- we're done
      end if;
  end loop;
  return exists;
end inEnvironment;


-----------------------------------------------------------------------------
--  REFRESH VOLATILE
--
-- Look up a volatile variable and refresh it
-- (Volatile variables are only environment variables.)
-----------------------------------------------------------------------------

procedure refreshVolatile( id : identifier ) is
  key : unbounded_string;                                     -- "var="
  ev  : unbounded_string;                                     -- an env var
  refreshed : boolean := false;
  importedStringValue : unbounded_string;
begin
  if identifiers( id ).method = local_memcache then
     Get( localMemcacheCluster,
          identifiers( id ).name,
          importedStringValue );
  elsif identifiers( id ).method = memcache then
     Get( distributedMemcacheCluster,
          identifiers( id ).name,
          importedStringValue );
  elsif identifiers( id ).method = session then
     declare
        temp1_t    : identifier;
        temp2_t    : identifier;
        -- b          : boolean;
     begin
        -- TODO: full prefix is a good idea but should be done with
        -- a separate pragma for consistency.
        -- GetFullParentUnitName( prefix );
        --declareStandardConstant( temp1_t,
        --   "session_variable_name", string_t,
        --   to_string( identifiers( id ).name ) );
        --declareIdent( temp2_t,
        --  "session_variable_value", string_t );
        findIdent( sessions_session_variable_name_str, temp1_t );
        if temp1_t /= eof_t then
           identifiers( temp1_t ).value.all := identifiers( id ).name;
        end if;
        findIdent( sessions_session_variable_value_str, temp2_t );
        if temp2_t /= eof_t then
           -- temp2_t could be rendered a constant for the export script
           identifiers( temp2_t ).value.all := identifiers( id ).name;
        end if;
        if not error_found then
           CompileAndRun( sessionImportScript, 1, false );
           importedStringValue := identifiers( temp2_t ).value.all;
           -- b := deleteIdent( temp2_t );
           -- b := deleteIdent( temp1_t );
        end if;
     end;
  elsif identifiers( id ).method = shell then
     key := identifiers( id ).name & "=";                        -- look for this
     for i in 1..Environment_Count loop                          -- all env vars
         ev := to_unbounded_string( Environment_Value( i ) );    -- get next one
         if Head( ev, length( key ) ) = key then                 -- match?
            importedStringValue :=                               -- get value
                Tail( ev, length( ev ) - length( key ) );        -- and assign
            refreshed := true;
            exit;                                                -- we're done
         end if;
     end loop;
     -- fall back to initial environment
     if not refreshed then
        for i in 1..environmentList.Length( initialEnvironment ) loop
            environmentList.Find( initialEnvironment, i, ev );
            if Head( ev, length( key ) ) = key then                 -- match?
               importedStringValue :=                               -- get value
                   Tail( ev, length( ev ) - length( key ) );        -- and assign
               refreshed := true;
               exit;                                                -- we're done
            end if;
        end loop;
     end if;
     if not refreshed then
        err( "unable to find volatile " &
             optional_bold( to_string( identifiers( id ).name ) ) &
             "in the O/S environment" );
     end if;
  else
     -- KB: 20/03/18 - hack have no effect
     return;
  end if;
  -- apply mapping, if any.  assume these are all set correctly
  -- do not apply mapping if an error occurred as the value may be
  -- incorrect.
  if not error_found then
     if identifiers( id ).mapping = json then                       -- json
        if getUniType( identifiers( id ).kind ) = uni_string_t then -- string
           DoJsonToString( identifiers( id ).value.all, importedStringValue );
        elsif identifiers( id ).list then                           -- array
           DoJsonToArray( id, importedStringValue );
        elsif  identifiers( getBaseType( identifiers( id ).kind ) ).kind  = root_record_t then -- record
           DoJsonToRecord( id, importedStringValue );
        elsif getUniType( identifiers( id ).kind ) = uni_numeric_t then -- number
           DoJsonToNumber( importedStringValue, identifiers( id ).value.all );
        elsif  identifiers( getBaseType( identifiers( id ).kind ) ).kind  = root_enumerated_t then -- enum
           DoJsonToNumber( importedStringValue, identifiers( id ).value.all );
        --else
        --   err( "internal error: unexpected import translation type" );
        end if;
     else                                                           -- no mapping
        identifiers( id ).value.all := importedStringValue;
     end if;
  end if;
end refreshVolatile;


-----------------------------------------------------------------------------
--  GET UNI TYPE
--
-- Dereference identifier until we find the universal type that this type
-- is based on.  quit if a circular relationship is suspected.  An
-- error will be reported to the user and universal_typeless is
-- returned.
-----------------------------------------------------------------------------

function getUniType( original : identifier ) return identifier is
  temp_id : identifier;
  count   : natural := 0;
begin

  -- safety check: eof_t is often returned on an error in the parser

  if identifiers( original ).kind = eof_t then
        err( "type expected" );
        return universal_t;

  -- new identifiers declared by the scanner have no type yet

  elsif identifiers( original ).kind = new_t then
        return new_t;

  -- exceptions are unique

  elsif original = exception_t then
        return exception_t;

  -- generics are themselves

  elsif identifiers( original ).class = genericTypeClass then
        return original;

  -- safety check: keywords have no type

  elsif identifiers( original ).kind = keyword_t then
        err( "type expected, not the keyword " &
           optional_bold( to_string( identifiers( original ).name ) ) );
        return universal_t;
  end if;

  -- "Dereference" types/subtypes, moving up type hierarchy,
  -- until a type derived from variable_t is found.  This
  -- will be the universal type the type is derived from.
  -- If there are more than 100 dereferences, assume this
  -- is a circular relationship (this should only occur in
  -- an internal error in SparForte).

  temp_id := original;
  while identifiers( temp_id ).kind /= variable_t loop
     temp_id := identifiers( temp_id ).kind;
     count := count + 1;
     if count >= 100 then
        err( "circular type relationship" );
        exit;
     end if;
  end loop;
  return temp_id;

end getUniType;


-----------------------------------------------------------------------------
--  GET BASE TYPE
--
-- Dereference original type until we find the original, parent root type
-- (i.e. for types declared with "subtype", move up the type
-- hierarchy to the first parent that is not a subtype).  Quit
-- if a circular relationship is suspected.  On an error, an error
-- message is displayed and universal_typeless is returned.
-----------------------------------------------------------------------------

function getBaseType( original : identifier ) return identifier is
  temp_id : identifier;
  count   : natural := 0;
begin

  -- safety check: eof_t is often returned on an error in the parser

  if identifiers( original ).kind = eof_t then
        err( "type expected" );
        return universal_t;

  -- new identifiers declared by the scanner have no type yet

  elsif identifiers( original ).kind = new_t then
        return new_t;

  -- exceptions are unique

  elsif identifiers( original ).kind = exception_t then
        return exception_t;

  -- safety check: keywords have no type

  elsif identifiers( original ).kind = keyword_t then
        err( "type expected, not the keyword " &
           optional_bold( to_string( identifiers( original ).name ) ) );
        return universal_t;
  end if;

  -- "Dereference" subtypes, moving up type hierarchy,
  -- until a non-subtype (that is, the base type) is found.
  -- If there are more than 100 dereferences, assume this
  -- is a circular relationship (this should only occur in
  -- an internal error in SparForte).

  temp_id := original;
  while identifiers( temp_id ).class = subClass loop
     temp_id := identifiers( temp_id ).kind;
     count := count + 1;
     if count >= 100 then
        err( "circular type relationship" );
        exit;
     end if;
  end loop;
  return temp_id;

end getBaseType;


-----------------------------------------------------------------------------
--  CLASS OK
--
-- Check if identifier matches a certain class.  If the identifier is
-- of another class, display an error message and return false.
-- Exception is a special case because it is a keyword.
-----------------------------------------------------------------------------

function class_ok( id : identifier; class : anIdentifierClass ) return boolean is
begin
  if identifiers( id ).class /= class then
     if id = eof_t then
        err( gnat.source_info.source_location & ": internal error: eof given as the identifier to class_ok(1)" );
     elsif id = exception_t then
        err_previous( "an " & bold( "exception" ) &
           " is not a " &
           getIdentifierClassImage( class ) );
     elsif id < reserved_top then
        err_previous( "a " & bold( "keyword" ) &
           " is not a " &
           getIdentifierClassImage( class ) );
     else
        err_previous( bold( to_string( identifiers( id ).name ) ) &
           " is a " &
           getIdentifierClassImage( identifiers( id ).class ) &
           ", not a " &
           getIdentifierClassImage( class ) );
     end if;
     return false;
  end if;
  return true;

end class_ok;


-----------------------------------------------------------------------------
--  CLASS OK
--
-- Check if identifier matches one of two classes.  If the identifier is of
-- another class, display an error message and return false.
-- Exception is a special case because it is a keyword.
-----------------------------------------------------------------------------

function class_ok( id : identifier; c1,c2 : anIdentifierClass ) return boolean is
begin
  if identifiers( id ).class /= c1 and identifiers( id ).class /= c2 then
     if id = eof_t then
        err( gnat.source_info.source_location & ": internal error: eof given as the identifier to class_ok(2)" );
     elsif id = exception_t then
        err_previous( "an " & bold( "exception" ) &
           " is not a " &
           getIdentifierClassImage( c1 ) &
           " or a " &
           getIdentifierClassImage( c2 ) );
     elsif id < reserved_top then
        err_previous( "a " & bold( "keyword" ) &
           " is not a " &
           getIdentifierClassImage( c1 ) &
           " or a " &
           getIdentifierClassImage( c2 ) );
     else
        err_previous( bold( to_string( identifiers( id ).name ) ) &
           " is a " &
           getIdentifierClassImage( identifiers( id ).class ) &
           ", not a " &
           getIdentifierClassImage( c1 ) &
           " or a " &
           getIdentifierClassImage( c2 ) );
     end if;
     return false;
  end if;
  return true;

end class_ok;


-----------------------------------------------------------------------------
--  CLASS OK
--
-- Check if identifier matches one of two classes.  If the identifier is of
-- another class, display an error message and return false.
-- Exception is a special case because it is a keyword.
-----------------------------------------------------------------------------

function class_ok( id : identifier; c1,c2,c3 : anIdentifierClass ) return boolean is
begin
  if identifiers( id ).class /= c1 and identifiers( id ).class /= c2 and identifiers( id ).class /= c3 then
     if id = eof_t then
        err( gnat.source_info.source_location & ": internal error: eof given as the identifier to class_ok(3)" );
     elsif id = exception_t then
        err_previous( "an " & bold( "exception" ) &
           " is not a " &
           getIdentifierClassImage( c1 ) &
           ", " &
           getIdentifierClassImage( c2 ) &
           " or a " &
           getIdentifierClassImage( c3 ) );
     elsif id < reserved_top then
        err_previous( "a " & bold( "keyword" ) &
           " is not a " &
           getIdentifierClassImage( c1 ) &
           ", " &
           getIdentifierClassImage( c2 ) &
           " or a " &
           getIdentifierClassImage( c3 ) );
     else
        err_previous( bold( to_string( identifiers( id ).name ) ) &
           " is a " &
           getIdentifierClassImage( identifiers( id ).class ) &
           ", not a " &
           getIdentifierClassImage( c1 ) &
           ", " &
           getIdentifierClassImage( c2 ) &
           " or a " &
           getIdentifierClassImage( c3 ) );
     end if;
     return false;
  end if;
  return true;

end class_ok;


-----------------------------------------------------------------------------
--  UNI TYPES OK
--
-- Check that the two types are extended from a common universal type.
-- If the types differ, report an error message and return false.
-----------------------------------------------------------------------------

function uniTypesOk( leftType, rightType : identifier ) return boolean is
  effectiveLeftType : identifier;
  effectiveRightType : identifier;
  msg : unbounded_string;
begin

  -- Basic checks: we're expecting a type, subtype or array type.  Unversal
  -- typeless is always a match.  Exceptions are a special case as they
  -- are a keyword not a data type.

  if leftType = exception_t or rightType = exception_t then
     null;
  elsif not class_ok( leftType, typeClass, subClass, genericTypeClass ) then
     return false;
  elsif not class_ok( rightType, typeClass, subClass, genericTypeClass ) then
     return false;
  elsif leftType = variable_t or rightType = variable_t then
     return true;
  end if;

  -- Determine the root type for the variables.  If either is
  -- extended from universal typeless, it's a match.

  effectiveLeftType := getUniType( leftType );
  effectiveRightType := getUniType( rightType );
  if effectiveLeftType = universal_t or effectiveRightType = universal_t then
     return true;
  end if;

  -- If the types don't match, display an error message showing
  -- the type as well as the root type.

  if identifiers( leftType ).list and not identifiers( rightType ).list then
    msg := "type " & bold( to_string( identifiers( leftType ).name ) ) &
           "is an array but type " & to_unbounded_string(
           bold( to_string( identifiers( rightType ).name ) ) &
           " is not an array" );
  elsif not identifiers( leftType ).list and identifiers( rightType ).list then
    msg := "type " & bold( to_string( identifiers( leftType ).name ) ) &
           "is not an array but type " & to_unbounded_string(
           bold( to_string( identifiers( rightType ).name ) ) &
           " is an array" );
  elsif effectiveLeftType /= effectiveRightType then
    msg := to_unbounded_string( "type " & bold( to_string( identifiers(
      leftType ).name ) ) );
    if effectiveLeftType = root_enumerated_t then
       msg := msg & " (an enumerated type)";
    elsif identifiers( leftType ).kind /= variable_t then
       msg := msg & " ("
          & AorAN( identifiers( effectiveLeftType ).name )
          & ")";
    end if;
    msg := msg & " is inherently different from " &
        bold( to_string( AorAN( identifiers( rightType ).name ) ) );
    if effectiveRightType = root_enumerated_t then
       msg := msg & " (an enumerated type)";
    elsif identifiers( rightType ).kind /= variable_t then
       msg := msg & " ("
           & AorAN( identifiers( effectiveRightType ).name )
           & ")";
    end if;
    err_previous( to_string( msg ) );
    return false;
  end if;
  return true;

end uniTypesOk;


-----------------------------------------------------------------------------
--  BASE TYPES OK
--
-- Check that the two types are extended from a common base type.
-- If the types differ, report an error message and return false.
-----------------------------------------------------------------------------

function baseTypesOk( leftType, rightType : identifier ) return boolean is
  effectiveLeftType : identifier;
  effectiveRightType : identifier;
begin

  -- Basic checks: if the root types don't match, then the base types
  -- won't.  If either type is universal typeless, they automatically
  -- match.

  if not uniTypesOk( leftType, rightType ) then
     return false;
  end if;
  if leftType = universal_t or rightType = universal_t then
     return true;
  end if;
  effectiveLeftType := getBaseType( leftType );
  effectiveRightType := getBaseType( rightType );

  -- Universal type cases: Universal numeric or universal string will
  -- match depending on the root type of the second type.

  if effectiveLeftType = uni_numeric_t and then getUniType( rightType ) = uni_numeric_t then
     return true;
  end if;
  if effectiveLeftType = uni_string_t and then getUniType( rightType ) = uni_string_t then
     return true;
  end if;
  if effectiveRightType = uni_numeric_t and then getUniType( leftType ) = uni_numeric_t then
     return true;
  end if;
  if effectiveRightType = uni_string_t and then getUniType( leftType ) = uni_string_t then
     return true;
  end if;

  -- Otherwise, the types must be identical.

  if effectiveLeftType /= effectiveRightType then
     err_previous( "type " & bold( to_string( identifiers( leftType ).name) ) &
          " is not compatible with type " &
          bold( to_string( identifiers( rightType ).name ) ) );
     return false;
  end if;
  return true;

end baseTypesOk;

-- same, as a procedure

procedure baseTypesOk( leftType, rightType : identifier ) is
  discard : boolean;
begin
  discard := baseTypesOk( leftType, rightType );
end baseTypesOk;


-----------------------------------------------------------------------------
--  GEN TYPES OK
--
-- same as base types OK, but clearer error message for generic items
-- Check that the two types are extended from a common base type.
-- If the types differ, report an error message and return false.
-----------------------------------------------------------------------------
-- TODO: the getUniType message will still just be "type".  Refactor
-- getUniType and getBaseType to take an optional descriptive string
-- (e.g. "item") and make genTypesOk pass this.

function genTypesOk( leftType, rightType : identifier ) return boolean is
  effectiveLeftType : identifier;
  effectiveRightType : identifier;
begin

  -- If an error occurred, the identifiers passed to this test may be invalid.

  if error_found then
     return false;
  end if;

  -- Basic checks: if the root types don't match, then the base types
  -- won't.  If either type is universal typeless, they automatically
  -- match.

  if not uniTypesOk( leftType, rightType ) then
     return false;
  end if;
  if leftType = universal_t or rightType = universal_t then
     return true;
  end if;
  effectiveLeftType := getBaseType( leftType );
  effectiveRightType := getBaseType( rightType );

  -- Universal type cases: Universal numeric or universal string will
  -- match depending on the root type of the second type.

  if effectiveLeftType = uni_numeric_t and then getUniType( rightType ) = uni_numeric_t then
     return true;
  end if;
  if effectiveLeftType = uni_string_t and then getUniType( rightType ) = uni_string_t then
     return true;
  end if;
  if effectiveRightType = uni_numeric_t and then getUniType( leftType ) = uni_numeric_t then
     return true;
  end if;
  if effectiveRightType = uni_string_t and then getUniType( leftType ) = uni_string_t then
     return true;
  end if;

  -- Otherwise, the types must be identical.

  if effectiveLeftType /= effectiveRightType then
     err_previous( "item type " & bold( to_string( identifiers( leftType ).name) ) &
          " is not compatible with item type " &
          bold( to_string( identifiers( rightType ).name ) ) );
     return false;
  end if;
  return true;
end genTypesOk;

-- same as a procedure

procedure genTypesOk( leftType, rightType : identifier ) is
  discard : boolean;
begin
  discard := genTypesOk( leftType, rightType );
end genTypesOk;


-----------------------------------------------------------------------------
--  RENAMING TYPES OK
--
-- Basically the same as gen types OK, but different error message.
-----------------------------------------------------------------------------
-- TODO: the getUniType message will still just be "type".  Refactor
-- getUniType and getBaseType to take an optional descriptive string
-- (e.g. "item") and make genTypesOk pass this.
-- TODO: should I allow smaller integers to rename larger integer types?

function renamingTypesOk( renamingType, canonicalType : identifier ) return boolean is
  effectiveRenamingType : identifier;
  effectiveCanonicalType : identifier;
begin
  -- For our purposes, leftType is the renaming type.  Right
  -- Basic checks: if the root types don't match, then the base types
  -- won't.  If either type is universal typeless, they automatically
  -- match.

  if not uniTypesOk( renamingType, canonicalType ) then
     return false;
  end if;

  effectiveRenamingType := getBaseType( renamingType );
  effectiveCanonicalType := getBaseType( canonicalType );
-- put_line( to_string( identifiers( effectiveRenamingType ).name ) );
-- put_line( to_string( identifiers( effectiveCanonicalType ).name ) );

  -- The types must be identical.  For universal or integer types, unless
  -- both types are identicial, either type may end up with values it should
  -- not represent.

  if effectiveRenamingType /= effectiveCanonicalType then
     err_previous( "renaming or copying type " & bold( to_string( identifiers( renamingType ).name) ) &
          " is not equivalent to canonical identifier's type " &
          bold( to_string( identifiers( canonicalType ).name ) ) );
     return false;
  end if;
  return true;
end renamingTypesOk;


-----------------------------------------------------------------------------
--   CAST TO TYPE
--
-- If a value is an integer type (i.e. positive, natural or integer),
-- round the value.  Otherwise do not round the value.  Return the
-- result as a string value.  This version only handles numbers.
--
-- It is permissible to cast to an abstract type as you are not defining
-- a new variable (instance) of that type.
-----------------------------------------------------------------------------

function castToType( val : long_float; kind : identifier ) return unbounded_string is
  baseType : identifier;
  roundedVal : long_long_integer;
  str : unbounded_string;
begin
  -- what kind is it
  baseType := getBaseType( kind );
  --put_identifier( baseType ); -- DEBUG
  -- If it's an integer type, just round it
  if kind = short_short_integer_t or
     kind = short_integer_t or
     kind = integer_t or
     kind = long_integer_t or
     kind = long_long_integer_t then
     roundedVal := long_long_integer( val );
     str := to_unbounded_string( long_long_integer'image( roundedVal ) );
  -- If it's a natural type, round it and check for negative
  elsif kind = natural_t then
     roundedVal := long_long_integer( val );
     if roundedVal < 0 then
        err( "natural value is less than zero" );
     end if;
     str := to_unbounded_string( long_long_integer'image( roundedVal ) );
  -- If it's a positive type, round it and check for negative or zero
  elsif kind = positive_t then
     roundedVal := long_long_integer( val );
     if roundedVal <= 0 then
        err( "positive value is less than zero" );
     end if;
     str := to_unbounded_string( long_long_integer'image( roundedVal ) );
  -- If it's anything else, including universals, don't do anything
  -- except convert to a string
  else
     -- return unchanged
     str := to_unbounded_string( val'img );
  end if;
  return str;
end castToType;

function castToType( val : unbounded_string; kind : identifier ) return unbounded_string is
  baseType : identifier;
  roundedVal : long_long_integer;
  str : unbounded_string;
begin
  -- what kind is it
  baseType := getBaseType( kind );
  --put_identifier( baseType ); -- DEBUG
  -- If it's an integer type, just round it
  -- TODO: positive is subtype of integer.  We should really walk the tree
  -- to find out if any ancestor type is positive, natural.
  if kind = short_short_integer_t or
     kind = short_integer_t or
     kind = integer_t or
     kind = long_integer_t or
     kind = long_long_integer_t then
     begin
       roundedVal := long_long_integer( long_float'value( to_string( val ) ) );
     exception when constraint_error =>
       err( "a variable has no value or a value is out-of-range" );
     when others =>
       err_exception_raised;
     end;
     str := to_unbounded_string( long_long_integer'image( roundedVal ) );
  -- If it's a natural type, round it and check for negative
  --elsif baseType = natural_t then
  elsif kind = natural_t then
     begin
       roundedVal := long_long_integer( long_float'value( to_string( val ) ) );
     exception when constraint_error =>
       err( "a variable has no value or a value is out-of-range" );
     when others =>
       err_exception_raised;
     end;
     if roundedVal < 0 then
        err( "natural value is less than zero" );
     end if;
     begin
       str := to_unbounded_string( long_long_integer'image( roundedVal ) );
     exception when others =>
       err_exception_raised;
     end;
  -- If it's a positive type, round it and check for negative or zero
  --elsif baseType = positive_t then
  elsif kind = positive_t then
     begin
       roundedVal := long_long_integer( long_float'value( to_string( val ) ) );
     exception when constraint_error =>
       err( "a variable has no value or a value is out-of-range" );
     when others =>
       err_exception_raised;
     end;
     if roundedVal <= 0 then
        err( "positive value is less than zero" );
     end if;
     str := to_unbounded_string( long_long_integer'image( roundedVal ) );
  -- If it's anything else, including universals, don't do anything
  -- except convert to a string
  elsif baseType = character_t then
     if length( val ) /= 1 then
        err( "character value must be one character long" );
     end if;
     str := val;
  else
     -- return unchanged
     str := val;
  end if;
  return str;
end castToType;


-----------------------------------------------------------------------------
--   DELETE IDENT
--
-- Delete a keyword / identifier from the symbol table
-- This was originally in world but moved here because deleting now has a lot of
-- triggered actions that may be performed.
-----------------------------------------------------------------------------
-- TODO: Exporting should be refactored so it is not duplicated

function deleteIdent( id : identifier ) return boolean is
   tempStr : unbounded_string;

  ---------------------------------------------------------------------------
  --  CONVERT VALUE TO JSON (DELETE IDENT)
  --
  -- convert the value to json.  assumes you checked that it needs to be
  ---------------------------------------------------------------------------

  function ConvertValueToJson( id : identifier ) return unbounded_string is
  begin
    tempStr := identifiers( id ).value.all;
    if getUniType( identifiers( id ).kind ) = uni_string_t then
       tempStr := DoStringToJson( tempStr );
    elsif identifiers( id ).list then
       DoArrayToJSON( tempStr, id );
    elsif  identifiers( getBaseType( identifiers( id ).kind ) ).kind  = root_record_t then
       DoRecordToJSON( tempStr, id );
    elsif getUniType( identifiers( id ).kind ) = uni_numeric_t then
        null; -- for numbers, JSON is as-is
    else
        err( "json export not yet written for this type" );
    end if;
    return tempStr;
  end ConvertValueToJson;

  ---------------------------------------------------------------------------
  --  EXPORT VALUE (DELETE IDENT)
  --
  -- export a value before deleting.
  ---------------------------------------------------------------------------

  procedure ExportValue( id : identifier ) is
  begin
    case identifiers( id ).method is
    when local_memcache =>
        begin
           if identifiers( id ).mapping = json then
              Set( localMemcacheCluster,
                   identifiers( id ).name,
                   ConvertValueToJSon( id ) );
           else
              Set( localMemcacheCluster,
                   identifiers( id ).name,
                   identifiers( id ).value.all );
           end if;
        exception when others => null;
        end;
    when memcache =>
        begin
           if identifiers( id ).mapping = json then
              Set( distributedMemcacheCluster,
                   identifiers( id ).name,
                   ConvertValueToJSon( id ) );
           else
              Set( distributedMemcacheCluster,
                   identifiers( id ).name,
                   identifiers( id ).value.all );
           end if;
        exception when others => null;
        end;
    when session =>
        if length( sessionExportScript ) = 0 then
           err( "session export script not defined" );
        else
           declare
             old_rshOpt : constant commandLineOption := rshOpt;
             temp1_t : identifier;
             temp2_t : identifier;
             -- b : boolean;
           begin
             findIdent( sessions_session_variable_name_str, temp1_t );
             if temp1_t /= eof_t then
                identifiers( temp1_t ).value.all := identifiers( id ).name;
             end if;
             findIdent( sessions_session_variable_value_str, temp2_t );
             if temp2_t /= eof_t then
                -- temp2_t could be rendered a constant for the export script
                if identifiers( id ).mapping = json then
                  identifiers( temp2_t ).value.all := ConvertValueToJson( id );
                else
                  identifiers( temp2_t ).value.all := identifiers( id ).value.all;
                end if;
             --    declareStandardConstant( temp2_t, "session_variable_value", string_t, to_string( ConvertValueToJson( id ) ) );
             -- else
             --    declareStandardConstant( temp2_t, "session_variable_value", string_t, to_string( identifiers( id ).value ) );
             -- end if;
             end if;
             -- declareStandardConstant( temp1_t, "session_variable_name", string_t, to_string( identifiers( id ).name ) );
             -- if identifiers( id ).mapping = json then
             --    declareStandardConstant( temp2_t, "session_variable_value", string_t, to_string( ConvertValueToJson( id ) ) );
             -- else
             --    declareStandardConstant( temp2_t, "session_variable_value", string_t, to_string( identifiers( id ).value ) );
             -- end if;
             --
             -- Session scripts must run without restricted shell so values can
             -- be saved.
             rshOpt := false;
             CompileAndRun( sessionExportScript, 1, false );
             -- b := deleteIdent( temp2_t );  -- recursion, ignore result
             -- b := deleteIdent( temp1_t );  -- recursion, ignore result
             rshOpt := old_rshOpt;
           end;
        end if;
    when shell =>
        null; -- does not save on variable destruction
    when http_cgi =>
        null; -- does not save on variable destruction
    when others =>
        err( gnat.source_info.source_location & ": internal error: unexpected export mapping in ExportValue" );
    end case;
  end ExportValue;

begin
  if id >= identifiers_top then                                 -- id > top?
     return false;                                              -- delete fail
  elsif identifiers( id ).deleted then                          -- flagged?
     return false;                                              -- delete fail
  elsif identifiers( id ).resource and identifiers( id ).class = varClass then
     return false;                                              -- delete fail
        -- deleting a single resource is not allowed because the id is the index
        -- into the list of resources.
  elsif id = identifiers_top-1 then                             -- last id?
     -- If a renaming, decrement the renaming count of the target first.
     if identifiers( id ).renaming_of /= identifiers'first then
        -- the avalue is a pointer to the canonical avalue, so just
        -- remove it so declareIdent won't be confused and try to free it.
        identifiers( id ).avalue := null;
        declare
          derefed_id : constant identifier := identifiers( id ).renaming_of;
        begin
          if identifiers( derefed_id ).renamed_count > 0 then
             identifiers( derefed_id ).renamed_count :=
              identifiers( derefed_id ).renamed_count - 1;
          else
              err( gnat.source_info.source_location & ": internal error: dereferenced identifier's renamed_count " &
                 "unexpectedly zero  for "  & optional_bold( to_string( identifiers( derefed_id ).name ) ) );
          end if;
        end;
     end if;
     -- when destroying an identifier, even if it as references to some
     -- other variable, it is to be removed.
     if identifiers( id ).export then
        ExportValue( id );
     end if;
     -- TODO: destroying a variable with an anonymous array doesn't destroy
     -- the anonymous array type.  Since the user cannot name the type,
     -- it will linger until the block is destroyed.
     identifiers_top := identifiers_top - 1;                -- pull stack

  -- When a variable with the same name is encountered, it will be
  -- reinitialized with a Kind of new but these will remain unchanged.
  -- Reset these to defaults to avoid confusing SparForte...
  identifiers( id ).import := false;                            -- clear these
  identifiers( id ).export := false;
  identifiers( id ).method := none;
  identifiers( id ).mapping := none;
  identifiers( id ).list   := false;
  identifiers( id ).field_of  := eof_t;
  identifiers( id ).renaming_of := identifiers'first;
  identifiers( id ).volatile := none;
  identifiers( id ).usage  := fullUsage;
  identifiers( id ).inspect := false;
  identifiers( id ).class  := otherClass;
  identifiers( id ).renamed_count := 0;

  -- TODO: avalue not released on unset.  it is left to be released on reuse
     return true;                                               -- delete ok
  end if;                                                       -- else

  -- The following only occurs if a variable is not at the top of the
  -- identifier stack.

  -- If a renaming, decrement the renaming count of the target first.
  if identifiers( id ).renaming_of /= identifiers'first then
     -- the avalue is a pointer to the canonical avalue, so just
     -- remove it so declareIdent won't be confused and try to free it.
     identifiers( id ).avalue := null;
     declare
       derefed_id : constant identifier := identifiers( id ).renaming_of;
     begin
       if identifiers( derefed_id ).renamed_count > 0 then
          identifiers( derefed_id ).renamed_count :=
             identifiers( derefed_id ).renamed_count - 1;
       else
           err( gnat.source_info.source_location & ": internal error: dereferenced identifier's renamed_count " &
                "unexpectedly zero  for "  & optional_bold( to_string( identifiers( derefed_id ).name ) ) );
       end if;
     end;
  end if;

  -- If not the top-most identifier, then you can't just pull
  -- the top of the stack

  if identifiers( id ).export then
     ExportValue( id );
  end if;

  -- TODO: destroying a variable with an anonymous array doesn't destroy
  -- the anonymous array type.  Since the user cannot name the type,
  -- it will linger until the block is destroyed.
  identifiers( id ).deleted := true;                            -- flag it
  -- When a variable with the same name is encountered, it will be
  -- reinitialized with a Kind of new but these will remain unchanged.
  -- Reset these to defaults to avoid confusing SparForte...
  identifiers( id ).import := false;                            -- clear these
  identifiers( id ).export := false;
  identifiers( id ).method := none;
  identifiers( id ).mapping := none;
  identifiers( id ).list   := false;
  identifiers( id ).field_of  := eof_t;
  identifiers( id ).renaming_of := identifiers'first;
  identifiers( id ).volatile := none;
  identifiers( id ).usage := fullUsage;
  identifiers( id ).inspect := false;
  identifiers( id ).class  := otherClass;
  identifiers( id ).renamed_count := 0;
  -- TODO: avalue not released on unset.  it is left to be released on reuse
  return true;                                                  -- delete ok
end deleteIdent;


-----------------------------------------------------------------------------
--  DISCARD UNUSED IDENTIFIER
--
-- If an identifier has been not been assigned a type,
-- assume it's unused and discard it.
-----------------------------------------------------------------------------

procedure discardUnusedIdentifier( id : identifier ) is
  b : boolean;
begin
  if id /= eof_t then
     if identifiers( id ).kind = new_t or identifiers( id ).kind = eof_t then
        b := deleteIdent( id );
     end if;
  end if;
end discardUnusedIdentifier;


-----------------------------------------------------------------------------
--
--  JSON
--
-- This was originally in parser_aux but was moved here to make it more
-- accessible to the language.  I couldn't break it out into its own file
-- due to its dependencies.
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  IS JSON WHITESPACE
--
-- True if character is a JSON whitespace character.
-----------------------------------------------------------------------------

function isJSONWhitespace( ch : character ) return boolean is
begin
  return ch = ' ' or ch = ASCII.CR or ch = ASCII.LF or ch = ASCII.HT;
end isJSONWhitespace;
pragma inline( isJSONWhitespace );


-----------------------------------------------------------------------------
--  SKIP JSON WHITESPACE
--
-- Move the start index ahread until it is a position in the string that has
-- a non-whitespace character.
-- --------------------------------------------------------------------------

procedure SkipJSONWhitespace( jsonString : unbounded_string; start : in out positive ) is
  stringEnd : constant natural := length( jsonString );
  ch        : character;
begin
   while start <= stringEnd loop
     ch := element( jsonString, start );
     exit when not isJSONWhitespace( ch );
     start := start + 1;
   end loop;
end SkipJSONWhitespace;


-----------------------------------------------------------------------------
-- JSON EXPECT
--
-- Expect a character.  Report an error if it's not there.
-----------------------------------------------------------------------------

procedure JSONexpect(jsonString : unbounded_string; start : in out positive;
  expectedChar : character ) is
  ch : character;
begin
  SkipJSONWhitespace( jsonString, start );
  if start <= length( jsonString ) then
     ch := element( jsonString, start );
     if ch = expectedChar then
         start := start + 1;
     else
         err( expectedChar & " expected in JSON string at" & start'img );
     end if;
  else
     err( expectedChar & " expected in JSON string at" & start'img );
  end if;
end JSONexpect;


-----------------------------------------------------------------------------
--  PARSER JSON ITEM
--
-- Parse a JSON item and return the text.  Start at the character start
-- and if the item has nested elements, recurse and include them in
-- the result.  Does not decode the JSON string.  Nesting is not used, just
-- for debugging.
-----------------------------------------------------------------------------

procedure ParseJSONItem( jsonString : unbounded_string;
  result : in out unbounded_string;
  start  : in out positive;
  nesting : positive ) is

  ch          : character;
  j           : positive;
  inBackslash : boolean := false;
  item        : unbounded_string;
  stringEnd   : constant natural := length( jsonString );
begin
  -- Beyond end of string?  Just abort.
  if start > stringEnd then
     return;
  end if;

  -- Skip any white space
  j := start;
  skipJSONWhitespace( jsonString, j );
  if j > stringEnd then
     return;
  end if;
  ch := element( jsonString, j );
  item := item & ch;
  j := j + 1;

  -- We have characters to build into a string
--put_line( "Parsing on character " & ch & " nesting" & nesting'img ); -- DEBUG

  -- Deterime what we're doing by examining the first character
  -- Aggregate type will recurse

  if ch = '"' then
     loop
        exit when j > stringEnd-1;
        ch := element( jsonString, j );
        if not inBackslash then
           if ch = '"' then
              item := item & ch;
              j := j + 1;
              exit;
           end if;
        end if;
        if inBackslash then
           inBackslash := false;
        elsif ch = '\' then
           inBackslash := true;
        end if;
        item := item & ch;
        j := j + 1;
     end loop;

  -- JSON object: read the label and recurse.  repeat for all items
  elsif ch = '{' then
     skipJSONWhitespace( jsonString, j );
     while j <= stringEnd loop
        ch := element( jsonString, j );
        if ch = '}' then
           item := item & ch;
           j := j + 1;
           exit;
        end if;
        -- really should be string-only here
        ParseJSONItem( jsonString, item, j, nesting+1 );
        JSONexpect( jsonString, j, ':' );
        item := item & ":";
        ParseJSONItem( jsonString, item, j, nesting+1 );
        if j <= stringEnd then
           ch := element( jsonString, j );
           if ch = ',' then
              item := item & ',';
              j := j + 1;
           end if;
       end if;
     end loop;

  -- JSON array: repeat for all items
  elsif ch = '[' then
     skipJSONWhitespace( jsonString, j );
     while j <= stringEnd loop
        ch := element( jsonString, j );
        if ch = ']' then
           item := item & ch;
           j := j + 1;
           exit;
        end if;
        ParseJSONItem( jsonString, item, j, nesting+1 );
        if j <= stringEnd then
           ch := element( jsonString, j );
           if ch = ',' then
              item := item & ',';
              j := j + 1;
           end if;
        end if;
     end loop;

  -- JSON number/boolean/other: exit on white space or terminating character
  else
     while j <= stringEnd loop
        ch := element( jsonString, j );
        if not inBackslash and (ch = ',' or ch = ']' or ch = '"' or ch = '}'
          or isJSONWhitespace(  ch ) ) then
           exit;
        end if;
        if ch = '\' then
           inBackslash := not inBackslash;
        end if;
        item := item & ch;
        j := j + 1;
     end loop;
   end if;

   start := j;
   result := result & item;

end ParseJSONItem;

-- The wrapper that normally gets called.

procedure ParseJSONItem( jsonString : unbounded_string;
                         item : in out unbounded_string;
                         start : in out positive ) is
begin
  item := null_unbounded_string;
  ParseJSONItem( jsonString, item, start, 1 );
end ParseJSONItem;


-----------------------------------------------------------------------------
--  DO JSON TO STRING
--
-- Convert a JSON string and return the string  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
--
-----------------------------------------------------------------------------

procedure DoJsonToString( result : out unbounded_string; expr_val : unbounded_string ) is
  ch : character;
  i  : integer;
  ok : boolean := false;
begin
  result := null_unbounded_string;
  i := 1;
  SkipJSONWhitespace( expr_val, i );
  if i < length( expr_val ) then
     if element( expr_val, i ) = '"' then
        ok := true;
     end if;
  end if;
  if not ok then
     err( optional_bold( "JSON string value" ) & " expected in string """ & toSecureData( to_string( toEscaped( expr_val ) ) ) & """" );
     return;
  end if;
  i := i + 1;
  -- note : could be rewritten to discard quotes and do json unescaped
  loop
     exit when i > length(expr_val)-1;
     if element( expr_val, i ) = '\' then
        i := i + 1;
        ch := element( expr_val, i );
        -- Note : \u not implemented
        if ch = '"' then
           result := result & '"';
        elsif ch = '\' then
           result := result & '\';
        elsif ch = '/' then
           result := result & '/';
        elsif ch =  'b' then
           result := result & ASCII.BS;
        elsif ch = 'f' then
           result := result & ASCII.FF;
        elsif ch = 'n' then
           result := result & ASCII.LF;
        elsif ch = 'r' then
           result := result & ASCII.CR;
        elsif ch = 't' then
           result := result & ASCII.HT;
        end if;
     else
        result := result & element( expr_val, i );
     end if;
     i := i + 1;
  end loop;
end DoJsonToString;


-----------------------------------------------------------------------------
--  DO ARRAY TO JSON
--
-- Convert an array to a JSON string.  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
-----------------------------------------------------------------------------
-- TODO: target_ref should be an unbounded_string

procedure DoArrayToJson( result : out unbounded_string; source_var_id : identifier ) is
  source_first  : long_integer;
  source_last   : long_integer;
  source_len    : long_integer;
  item          : unbounded_string;
  -- sourceArrayId : arrayID;
  kind          : identifier;
  elementKind   : identifier;
  data          : unbounded_string;
begin
  -- look up the array information

     -- sourceArrayId := arrayID( to_numeric( identifiers( source_var_id ).value ) );
     -- source_first := firstBound( sourceArrayID );
     -- source_last  := lastBound( sourceArrayID );
     source_first := identifiers( source_var_id ).avalue'first;
     source_last  := identifiers( source_var_id ).avalue'last;
     source_len   := source_last - source_first + 1;
     kind := getUniType( identifiers( source_var_id ).kind );
     elementKind := getBaseType( identifiers( identifiers( source_var_id ).kind ).kind );

     -- In JSON, enumerateds (or, at least, booleans) are by the name,
     -- not the value.
     if elementKind = boolean_t then
        declare
           enum_val : integer;
        begin
           result := to_unbounded_string( "[" );
           for arrayElementPos in source_first..source_last loop
               -- data := arrayElement( sourceArrayId, arrayElementPos );
               data := identifiers( source_var_id ).avalue( arrayElementPos );
               enum_val := integer( to_numeric( data ) );
               if enum_val = 0 then
                  item := to_unbounded_string( "false" );
               elsif enum_val = 1 then
                  item := to_unbounded_string( "true" );
               else
                  err( gnat.source_info.source_location & ": internal error: unexpect boolean position" & enum_val'img );
               end if;
               if arrayElementPos /= source_last then
                  result := result & item & to_unbounded_string( "," );
               end if;
           end loop;
           result := result & item & to_unbounded_string( "]" );
        end;

     elsif kind = uni_string_t or kind = universal_t then
        result := to_unbounded_string( "[" );
        for arrayElementPos in source_first..source_last loop
           -- data := arrayElement( sourceArrayId, arrayElementPos );
           data := identifiers( source_var_id ).avalue( arrayElementPos );
           if elementKind = json_string_t then
              -- if it's a JSON string, just copy the data
              result := result & data;
           else
              item := to_unbounded_string( """" );
              item := item & toJSONEscaped( data );
              item := item & '"';
           end if;
           if arrayElementPos /= source_last then
              result := result & item & to_unbounded_string( "," );
           end if;
        end loop;
        result := result & item & to_unbounded_string( "]" );
     elsif kind = uni_numeric_t or kind = root_enumerated_t then

        result := to_unbounded_string( "[" );
        for arrayElementPos in source_first..source_last loop
           -- data := arrayElement( sourceArrayId, arrayElementPos );
           data := identifiers( source_var_id ).avalue( arrayElementPos );
           if element( data, 1 ) = ' ' then
              delete( data, 1, 1 );
           end if;
           if arrayElementPos /= source_last then
              result := result & data & to_unbounded_string( "," );
           end if;
        end loop;
        result := result & data & to_unbounded_string( "]" );
     else
        -- private types are unique types extending variable_t
        err( "private type elements cannot be encoded as JSON" );
     end if;
exception when CONSTRAINT_ERROR =>
  err( gnat.source_info.source_location & ": internal error: constraint_error" );
when STORAGE_ERROR =>
  err( gnat.source_info.source_location & ": internal error : storage error raised in ParseFactor" );
end DoArrayToJson;


-----------------------------------------------------------------------------
--  DO JSON TO ARRAY
--
-- Convert a JSON string and store in an array.  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
-----------------------------------------------------------------------------

procedure DoJsonToArray( target_var_id : identifier; source_val : unbounded_string ) is
  target_first  : long_integer;
  target_last   : long_integer;
  target_len    : long_integer;
  sourceLen     : long_integer;
  item          : unbounded_string;
  decoded_item  : unbounded_string;
  -- targetArrayId : arrayID;
  arrayElement  : long_integer;
  kind          : identifier;
  elementKind   : identifier;
  ch            : character;
  inBackslash   : boolean;
  inQuotes      : boolean;
begin
  -- look up the array information
  -- targetArrayId := arrayID( to_numeric( identifiers( target_var_id ).value ) );
  -- target_first := firstBound( targetArrayID );
  -- target_last  := lastBound( targetArrayID );
  target_first := identifiers( target_var_id ).avalue'first;
  target_last  := identifiers( target_var_id ).avalue'last;
  target_len   := target_last - target_first + 1;
  kind := getUniType( identifiers( target_var_id ).kind );
  elementKind := getBaseType( identifiers( identifiers( target_var_id ).kind ).kind );

     -- basic JSon validation.  Important to verify it isn't a record.
     declare
       i : integer := 1;
       ch: character;
     begin
       skipJSONWhitespace( source_val, i );
       if length( source_val ) > 0 then
          ch := element( source_val, i );
          if ch = '{' then
            err( "JSON array expected but found object" );
          elsif ch /= '[' then
             err( optional_bold( "JSON array expected" ) & " but found string """ & toSecureData( to_string( toEscaped( source_val ) ) ) & '"' );
          elsif element( source_val, length( source_val ) ) /= ']' then
             err( "expected trailing ]" );
          end if;
       end if;
     end;

     -- Count the number of items in the JSON string, handling escaping as
     -- required.  Source len will be the number of items.

     sourceLen := 0;

     declare
       i : integer := 1;
       discard : unbounded_string;
     begin
       while i <= length( source_val ) loop
         ch := element( source_val, i );
         if ch = '[' then
             exit;
         end if;
         i := i + 1;
       end loop;

       if i <= length( source_val ) then
          i := i + 1; -- skip [
          loop
            ParseJSONItem( source_val, discard, i );
            if i > length( source_val ) then
               exit;
            else
               sourceLen := sourceLen + 1;
               SkipJSONWhitespace( source_val, i );
               if i <= length( source_val ) then
                  ch := element( source_val, i );
                  if ch = ',' then
                     i := i + 1;
                  elsif ch = ']' then
                     exit;
                  end if;
               else
                  err( "JSON parse error on character" & i'img );
                  exit;
               end if;
            end if;
          end loop;
       else
          err( "JSON parse error on character" & i'img );
       end if;
     end;

     inBackslash := false;
     inQuotes := false;

    -- Check to see if the length matches that of the array we are assigning
    -- to.  Null array is a special case.
     if sourceLen = 0 and target_len = 0 then
        null;
     elsif sourceLen /= target_len then
       err( "array has" &
            target_len'img &
            " item(s) but JSON string has" &
            sourceLen'img );
     elsif kind = root_enumerated_t then

        -- In JSON, booleans are stored by the name,
        -- Other enums will be by ordinal position (more or less).

        if elementKind = boolean_t then
           -- for a boolean array, we will have to convert true or false
           -- as well as raise an error on illegal values
           arrayElement := target_first;
           for i in 2..length( source_val ) loop
               ch := element( source_val, i );
               if ch = ',' or ch = ']' then
                  if item = "false" then
                     -- assignElement( targetArrayId, arrayElement, to_unbounded_string( "0" ) );
                     identifiers( target_var_id ).avalue( arrayElement ) := to_unbounded_string( "0" );
                     arrayElement := arrayElement + 1;
                     item := null_unbounded_string;
                  elsif item = "true" then
                     -- assignElement( targetArrayId, arrayElement, to_unbounded_string( "1" ) );
                     identifiers( target_var_id ).avalue( arrayElement ) := to_unbounded_string( "1" );
                     arrayElement := arrayElement + 1;
                     item := null_unbounded_string;
                  else
                     err( optional_bold( toSecureData( to_string( item ) ) ) & " is neither JSON true nor false" );
                  end if;
               else
                  item := item & ch;
               end if;
           end loop;
        else
           -- for non-boolean array of enumerated types, use the ordinal
           -- position and treat it as an array of integers

           -- i don't actually record the maximum value for an enumerated type
           -- the only way to tell is to search the symbol table for a match.
           -- the enum item closest to the top of the symbol table should be
           -- the greatest ordinal position.

           declare
             maxEnum : integer;
             enumVal : integer;
           begin
              for i in reverse keywords_top..identifiers_top-1  loop
                  if identifiers( i ).kind = elementKind then
                     if identifiers( i ).class = enumClass then
                        maxEnum := integer( to_numeric( identifiers( i ).value.all ) );
                        exit;
                     end if;
                  end if;
              end loop;

              -- Assign the positions to the array, checking for out-of-range
              -- positions.
              arrayElement := target_first;
              for i in 2..length( source_val ) loop
                  ch := element( source_val, i );
                  if ch = ',' or ch = ']' then
                     enumVal := integer'value( ' ' & to_string( item ) );
                     if enumVal < 0 or enumVal > maxEnum then
                        err( "enumerated position " &
                             optional_bold( to_string( item ) ) &
                             " is out of range for " &
                             optional_bold( to_string( identifiers( elementKind ).name ) ) );
                     else
                        -- assignElement( targetArrayId, arrayElement, ' ' & item );
                        identifiers( target_var_id ).avalue( arrayElement ) := ' ' & item;
                        arrayElement := arrayElement + 1;
                        item := null_unbounded_string;
                     end if;
                  else
                     item := item & ch;
                  end if;
              end loop;
             end;
           end if;

     elsif kind = uni_string_t or kind = universal_t then

        -- some kind of string items

        arrayElement := target_first;
        -- for i in 2..length( source_val ) loop
        declare
          i : integer := 2;
        begin

          skipJSONWhitespace( source_val, i );
          while i <= length( source_val ) loop
            ch := element( source_val, i );

            if elementKind = json_string_t then
            -- if it's JSON string, read the string as-is and assign it to the
            -- array.
               ParseJSONItem( source_val, item, i );
               if i <= length( source_val ) then
                   skipJSONWhitespace( source_val, i );
                   ch := element( source_val, i );
                   if ch = ',' or ch = ']' then
                      i := i + 1;
                   else
                      err( "JSON parse error on character" & i'img );
                   end if;
               end if;

               -- assignElement( targetArrayId, arrayElement, item );
               identifiers( target_var_id ).avalue( arrayElement ) := item;
               arrayElement := arrayElement + 1;
               item := null_unbounded_string;
            else
               if i <= length( source_val ) then
                  ch := element( source_val, i );
                  if ch /= '"' then
                     err( "JSON string value expected" );
                  end if;
               end if;

               ParseJSONItem( source_val, item, i );

               decoded_item := null_unbounded_string;
               for j in 2..length( item )-1 loop
                   ch := element( item, j );
                   if inBackslash then
                      if ch = '"' then
                         decoded_item := decoded_item & '"';
                      elsif ch = '\' then
                         decoded_item := decoded_item & '\';
                      elsif ch = '/' then
                         decoded_item := decoded_item & '/';
                      elsif ch =  'b' then
                         decoded_item := decoded_item & ASCII.BS;
                      elsif ch = 'f' then
                         decoded_item := decoded_item & ASCII.FF;
                      elsif ch = 'n' then
                         decoded_item := decoded_item & ASCII.LF;
                      elsif ch = 'r' then
                         decoded_item := decoded_item & ASCII.CR;
                      elsif ch = 't' then
                         decoded_item := decoded_item & ASCII.HT;
                      end if;
                      inBackslash := false;
                   elsif ch = '\' then
                      inBackslash := true;
                   else
                      decoded_item := decoded_item & ch;
                   end if;
               end loop;

               if i <= length( source_val ) then
                   skipJSONWhitespace( source_val, i );
                   ch := element( source_val, i );
                   if ch = ',' or ch = ']' then
                      i := i + 1;
                   else
                      err( "JSON parse error on character" & i'img );
                   end if;
               end if;

               -- assignElement( targetArrayId, arrayElement, decoded_item );
               identifiers( target_var_id ).avalue( arrayElement ) := decoded_item;
               arrayElement := arrayElement + 1;
               item := null_unbounded_string;
               --else
               --   item := item & ch;
               --end if;
            end if; -- if not json_string
          end loop;
        end;

     elsif kind = uni_numeric_t then

        arrayElement := target_first;
        declare
          i : integer := 1;
          ok : boolean := false;
          ch : character;
        begin
          skipJSONWhitespace( source_val, i );
          if i <= length( source_val ) then
             JSONexpect( source_val, i, '[' );
             while i <= length( source_val ) loop
               skipJSONWhitespace( source_val, i );
               item := null_unbounded_string;
               ParseJSONItem( source_val, item, i );
               -- assume that it's OK if it starts with a numeric character
               -- TODO: better validation based on actual type
               ch := element( item, 1 );
               if ch /= '-' and ch /= ' ' and ch /= '+' then
                  item := ' ' & item;
               end if;
               -- try to see if it is a valid long float
               declare
                  lf : long_float;
               begin
                  lf := to_numeric( item );
                  ok := true;
               exception when others => null;
               end;
               if not ok then
                  err( optional_bold( "JSON number value" ) & " expected in string """ & toSecureData( to_string( toEscaped( item ) ) ) & """" );
               end if;
               -- assignElement( targetArrayId, arrayElement, item );
               identifiers( target_var_id ).avalue( arrayElement ) := item;
               arrayElement := arrayElement + 1;
               skipJSONWhitespace( source_val, i );
               if i <= length( source_val ) then
                   ch := element( source_val, i );
                   if ch = ',' or ch = ']' then
                      i := i + 1;
                   else
                      err( "JSON parse error on character" & i'img );
                   end if;
               else
                   err( "JSON parse error on character" & i'img );
                   exit;
               end if;
             end loop;
          else
             err( "JSON parse error on character" & i'img );
          end if;
        end;
     else
        -- private types are unique types extending variable_t
        err( "private type elements cannot be used to store JSON data" );
     end if;
exception when CONSTRAINT_ERROR =>
  err( gnat.source_info.source_location & ": internal error: constraint_error" );
when STORAGE_ERROR =>
  err( gnat.source_info.source_location & ": internal error : storage error raised in ParseFactor" );
end DoJsonToArray;


-----------------------------------------------------------------------------
--  DO RECORD TO JSON
--
-- Convert a record to a JSON string.  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
-----------------------------------------------------------------------------

procedure DoRecordToJson( result : out unbounded_string; source_var_id : identifier ) is
   firstField    : boolean := true;
   fieldName   : unbounded_string;
   jsonFieldName   : unbounded_string;
   dotPos      : natural;
   field_t     : identifier;
   uniFieldType : identifier;
   item        : unbounded_string;
begin
     result := to_unbounded_string( "{" );
     for i in 1..integer'value( to_string( identifiers( identifiers( source_var_id ).kind ).value.all ) ) loop
         for j in 1..identifiers_top-1 loop
             if identifiers( j ).field_of = identifiers( source_var_id ).kind then
                if integer'value( to_string( identifiers( j ).value.all )) = i then
                      fieldName := identifiers( j ).name;
                      dotPos := length( fieldName );
                      while dotPos > 1 loop
                         exit when element( fieldName, dotPos ) = '.';
                         dotPos := dotPos - 1;
                      end loop;
                      jsonFieldName := delete( fieldName, 1, dotPos );
                      fieldName := identifiers( source_var_id ).name & "." & jsonFieldName;
                      findIdent( fieldName, field_t );
                      if field_t = eof_t then
                         err( "unable to find record field " &
                            optional_bold( to_string( fieldName ) ) );
                      else
                         if firstField then
                            firstField := false;
                         else
                            result := result & ',';
                         end if;
                         result := result & '"' & jsonFieldName & '"' & ":";
                         -- json encode primitive types
                         uniFieldType := getUniType( identifiers( field_t ).kind );
                         if getBaseType( identifiers( field_t ).kind ) = boolean_t then
                            begin
                               if integer( to_numeric( identifiers( field_t ).value.all ) ) = 0 then
                                  result := result & "false";
                               else
                                  result := result & "true";
                               end if;
                            exception when others =>
                               err( "unable to parse boolean value " &
                                  ASCII.Quotation &
                                  toSecureData( to_string( identifiers( field_t ).value.all ) ) &
                                  ASCII.Quotation );
                            end;
                         elsif uniFieldType = uni_numeric_t then
-- trim?
                            result := result & identifiers( field_t ).value.all;
                         elsif uniFieldType = root_enumerated_t then
                            result := result & identifiers( field_t ).value.all;
                         elsif getBaseType( identifiers( field_t ).kind ) = json_string_t then
                            -- if it's a JSON string, just copy the data
                            result := result & identifiers( field_t ).value.all;
                         elsif uniFieldType = uni_string_t or uniFieldType = universal_t then
                            item := to_unbounded_string( """" );
                            item := item & ToJSONEscaped( identifiers( field_t ).value.all );
                            item := item & '"';
                            result := result & item;
                         else
                            -- private types are unique types extending variable_t
                            err( "private type fields like " &
                                optional_bold( to_string( fieldName ) ) &
                                " cannot be encoded as JSON" );
                         end if;
                      end if;
                end if;
             end if;
         end loop;
     end loop;
     result := result & "}";
end DoRecordToJson;


-----------------------------------------------------------------------------
--  DO JSON TO RECORD
--
-- Convert a JSON string and store in a record.  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
-----------------------------------------------------------------------------

procedure DoJsonToRecord( target_var_id : identifier; sourceVal : unbounded_string ) is
  k             : natural;
  item          : unbounded_string;
  decodedItemName  : unbounded_string;
  decodedItemValue  : unbounded_string;
  elementKind   : identifier;
  ch            : character;
  sourceLen     : long_integer;
  found         : boolean;
  searchName    : unbounded_string;
  jsonStringType : boolean;
begin

     k := 2; -- skip leading { in JSON
     item := sourceVal;

     -- basic JSon validation.  Important to verify it isn't an array.
     declare
       i : integer := 1;
       ch: character;
     begin
       skipJSONWhitespace( item, i );
       if length( item ) > 0 then
          ch := element( item, i );
          if ch = '[' then
            err( "JSON object expected but found array" );
          elsif ch /= '{' then
             err( optional_bold( "JSON object expected" ) & " but found string """ & toSecureData( to_string( toEscaped( item ) ) ) & '"' );
          elsif element( item, length( item ) ) /= '}' then
             err( "expected trailing }" );
          end if;
       end if;
     end;

     sourceLen := 0;
     declare
       i : integer := 1;
       discard : unbounded_string;
       stringEnd : constant natural := length( sourceVal );
     begin
       JSONexpect( sourceVal, i, '{' );
       if i <= length( sourceVal ) then
          loop
            ParseJSONItem( sourceVal, discard, i );
            JSONexpect( sourceVal, i, ':' );
            ParseJSONItem( sourceVal, discard, i );
            if i <= stringEnd then
               ch := element( sourceVal, i );
               sourceLen := sourceLen + 1;
               SkipJSONWhitespace( sourceVal, i );
               if i > length( sourceVal ) then
                  exit;
               else
                  ch := element( sourceVal, i );
                  if ch = ',' then
                     i := i + 1;
                  elsif ch = '}' then
                     exit;
                  end if;
               end if;
            else
               err( "JSON parse error on character" & i'img );
               exit;
            end if;
          end loop;
        else
          err( "JSON parse error on character" & i'img );
       end if;
     end;
--put_line( "new length = " & sourceLen'img ); -- DEBUG

    -- The number of items in the JSON string should equal the size of the
    -- record.
    if sourceLen = long_integer'value( to_string( identifiers( identifiers( target_var_id ).kind ).value.all ) ) then

       -- for each of the items in the JSON string

       declare
          i : integer := 1;
       begin
          JSONexpect( sourceVal, i, '{' );
          while i <= length( sourceVal ) loop

          -- there should be a label and a string

          ParseJSONItem( sourceVal, decodedItemName, i );
          JSONexpect( sourceVal, i, ':' );
          ParseJSONItem( sourceVal, decodedItemValue, i );

          -- removed the quotes from around the label

          if length( decodedItemName ) > 0 then
             decodedItemName := ToJSONUnescaped( decodedItemName );
          end if;
          if length( decodedItemName ) > 0 then
             delete( decodedItemName, 1, 1 );
          end if;
          if length( decodedItemName ) > 0 then
             delete( decodedItemName, length( decodedItemName ), length( decodedItemName ) );
          end if;

          -- if the value starts with a quote, it's a string type in JSON

          jsonStringType := false;
          if element( decodedItemValue, 1 ) = '"' then
             jsonStringType := true;
          end if;

          -- we have a label and a value.  the record field is stored in the
          -- symbol table as rec.field.  Prepend the record name and search
          -- the symbol table for the record field.  When found, cast the
          -- value and assign it.  Otherwise, if the field is not found, it
          -- is an error.
          found := false;
          searchName := identifiers( target_var_id ).name & "." & decodedItemName;
          for j in reverse 1..identifiers_top-1 loop
              if identifiers( j ).name = searchName then
                 found := true;
                 if not error_found then
                    -- for booleans, it's true or false, not value
                    elementKind := getBaseType( identifiers( j ).kind );
                    if elementKind = boolean_t then
                       if decodedItemValue = "true" then
                          identifiers( j ).value.all := to_unbounded_string( "1" );
                       elsif decodedItemValue = "false" then
                          identifiers( j ).value.all :=  to_unbounded_string( "0" );
                       else
                          err( optional_bold( to_string( toEscaped( decodedItemName ) ) ) & " has a value of " & optional_bold( toSecureData( to_string( toEscaped( decodedItemValue ) ) ) ) & " but expected JSON true or false" );
                       end if;

-- range check the valuse for enumerateds
                    elsif getUniType( elementKind ) = root_enumerated_t then
                      -- Enumerateds
                      -- i don't actually record the maximum value for an enumerated type
                      -- the only way to tell is to search the symbol table for a match.
                      -- Enums are numbers shouldn't need to have special characters decoded.

                      declare
                        maxEnum : integer;
                        enumVal : integer;
                      begin
                        for i in reverse keywords_top..identifiers_top-1  loop
                            if identifiers( i ).kind = elementKind then
                               if identifiers( i ).class = enumClass then
                                  maxEnum := integer( to_numeric( identifiers( i ).value.all ) );
                                  exit;
                               end if;
                            end if;
                        end loop;
                        enumVal := integer'value( ' ' & to_string( decodedItemValue ) );
                        if enumVal < 0 or enumVal > maxEnum then
                           err( "enumerated position " &
                                optional_bold( to_string( toEscaped( decodedItemValue ) ) ) &
                                " is out of range for " &
                                optional_bold( to_string( identifiers( elementKind ).name ) ) );
                        end if;
                        -- Space is required for findEnumImage.  Values are
                        -- stored with leading space.
                        identifiers( j ).value.all := ' ' & decodedItemValue;
                      end;
                    elsif getUniType( elementKind ) = uni_string_t or
                          getUniType( elementKind ) = universal_t then

                      -- Strings
                      -- JSON string is raw json...could be anything.  Otherwise, the string
                      -- needs to be un-escaped.
                      if elementKind /= json_string_t then
                         if not jsonStringType then
                            err( optional_bold( "JSON string value" ) & " expected for " & toSecureData( to_string( ToEscaped( searchName ) ) ) );
                         end if;
                         -- strip of quotes and un-escape any characters.
                         if length( decodedItemValue ) > 0 then
                            delete( decodedItemValue, 1, 1 );
                         end if;
                         if length( decodedItemValue ) > 0 then
                            delete( decodedItemValue, length( decodedItemValue ), length( decodedItemValue ) );
                         end if;
                         if length( decodedItemValue ) > 0 then
                            decodedItemValue := ToJSONUnescaped( decodedItemValue );
                         end if;
                      end if;
                      identifiers( j ).value.all := castToType( decodedItemValue,
                         identifiers( j ).kind );
                    elsif getUniType( elementKind ) = uni_numeric_t then
                      -- Numbers
                      -- Numbers shouldn't need to have special characters decoded.
                      if jsonStringType then
                         err( optional_bold( "JSON number value" ) & " expected in """ & toSecureData( to_string( toEscaped( searchName ) ) ) & """" );
                      end if;
                      identifiers( j ).value.all := castToType( decodedItemValue,
                         identifiers( j ).kind );
                    else
                       -- private types are unique types extending variable_t
                       err( "private type fields like " &
                             optional_bold( to_string( identifiers( j ).name ) ) &
                             " cannot store JSON data" );
                    end if;
                 end if;
                 exit; -- the searchName may occur more than once.  stop when found first match

              end if;
          end loop; -- j
          if not found then
             err( to_string( toEscaped( searchName ) ) & " not declared" );
          end if;

          if i <= length( sourceVal ) then
             ch := element( sourceVal, i );
             if ch = ',' then
                i := i + 1;
             elsif ch = '}' then
                exit;
             end if;
          end if;
       end loop; -- i
       end;
    else
       err( "record has" &
            to_string( identifiers( identifiers( target_var_id ).kind ).value.all ) &
            " field(s) but JSON string has" &
            sourceLen'img );
    end if;
end DoJsonToRecord;


-----------------------------------------------------------------------------
--  DO STRING TO JSON
--
-- Convert a string to JSON and return the string  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
-----------------------------------------------------------------------------

function DoStringToJson( val : unbounded_string ) return unbounded_string is
  result : unbounded_string;
begin
  result := to_unbounded_string( """" );
  result := result & ToJSONEscaped( val );
  result := result & '"';
  return result;
end DoStringToJson;


-----------------------------------------------------------------------------
--  DO NUMBER TO JSON
--
-- Numbers are "as is" but we still need to check for a valid JSON string.
-----------------------------------------------------------------------------

procedure DoJsonToNumber( jsonString : unbounded_string; expr_val : out unbounded_string ) is
  i  : positive := 1;
  ch : character;
  ok : boolean := false;
  tempStr : unbounded_string;
begin
  expr_val := null_unbounded_string;
  SkipJSONWhitespace(jsonString, i );
  if i <= length( jsonString ) then
     -- assume that it's OK if it starts with a numeric character
     -- we don't know the exact numeric type here, not without the identifier
     -- being assigned to
     ch := element( jsonString, i );
     if ch = '+' or ch = '-' or ( ch >= '0' and ch <= '9' ) then
        ch := element( jsonString, 1 );
        if ch /= '-' and ch /= ' ' and ch /= '+' then
           tempStr := ' ' & jsonString;
        else
           tempStr := jsonString;
        end if;
        -- try to see if it is a valid long float
        declare
          lf : long_float;
        begin
          lf := to_numeric( tempStr );
          ok := true;
        exception when others => null;
        end;
     end if;
  end if;
  if ok then
     expr_val := jsonString;
  else
     err( optional_bold( "JSON number value" ) & " expected in string """ & toSecureData( to_string( toEscaped( jsonString ) ) ) & """" );
  end if;
end DoJsonToNumber;


-----------------------------------------------------------------------------
--
-- Scanning
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  GET NEXT TOKEN
--
-- The main scanner procedure.  Interpret the input text and return a token
-- representing the next item on the line.  Declare new, unknown identifiers
-- in the identifier symbol table automatically (it's up to the caller to
-- remove them).  If the last token was an end-of-file token, continue
-- to return end-of-file tokens forever.
-----------------------------------------------------------------------------

gnt_commandLine : unbounded_string;

procedure getNextToken is
  id   : identifier;
  word : unbounded_string;
  ch   : character;
  -- ch is a character buffer to reduce array accesses.  Really,
  -- these should be optimized away by the compiler, but you'd
  -- be surprised by what a compiler won't optimize away...
  is_based_number : boolean; -- true if numeric literal has a base
  token_firstpos, token_lastpos, lineno, fileno : natural;
  sfr : aSourceFile;
  inBackslash : boolean;
  adj : integer;

begin

  -- Out of data?  Never any more data.

  if token = eof_t then
     return;
  end if;

  -- End of line?  Read next line.  Display it if tracing is on.

<<redo>>

   ch := script( cmdpos );

   while ch = ASCII.NUL loop
      if trace then
         if syntax_check or (not exit_block and not error_found) then
            cmdpos := cmdpos + 2; -- first character of next command
            put( standard_error, "=> " & '"' );
            getCommandLine( gnt_commandLine, token_firstpos, token_lastpos, lineno, fileno );
            put( standard_error, toEscaped( gnt_commandLine ) );
            put( standard_error, """ [" );
            if fileno > 1 then -- don't bother naming main file
               sourceFilesList.Find( sourceFiles, sourceFilesList.aListIndex( fileno ), sfr );
               put( standard_error, toEscaped( sfr.name ) );
               put( standard_error, ":" );
            end if;
            put( standard_error, lineno'img );
            put_line( standard_error, "]" );
            cmdpos := cmdpos - 2;
         end if;
      end if;

      -- The counting of line was original when "--perf" was used but is now
      -- used for the side-effect detection so it is always done.
      if not syntax_check and (not exit_block or not error_found) then
         begin
            perfStats.lineCnt := perfStats.lineCnt + 1;
         exception when constraint_error =>
            -- at 50,000 lines per second, this theoretically happens at
            -- 5.8 million years on my 64-bit Linux laptop computer.
            err( "performance stats: line count overflow" );
         end;
      end if;
      cmdpos := cmdpos+nextScriptCommandOffset; -- line header and indent marker
      ch := script( cmdpos );
   end loop;

  -- skip any white space before token

  if ch = ' ' or ch = ASCII.HT then
     cmdpos := cmdpos + 1;
     while script( cmdpos ) = ' ' or script( cmdpos ) = ASCII.HT loop
        cmdpos := cmdpos + 1;
     end loop;
     ch := script( cmdpos );
  end if;

  -- token begins here

  firstpos := cmdpos;                                         -- first char
  lastpos := cmdpos;                                          -- last default

  -- Immediate Words

-- Keep the following line for debugging
--put_line( "first char: " & toEscaped( to_unbounded_string( "" & ch ) ) ); -- DEBUG
  if  ch = eof_character then
      token := eof_t;
      return;

  elsif  ch = immediate_word_delimiter then

     -- Immediate word string literals (word_t)
     --
     -- The preprocessor will have placed delimiters around the word so we
     -- shouldn't have to check for a missing one.

     word := null_unbounded_string;
     cmdpos := cmdpos + 1;
     while script( cmdpos ) /= immediate_word_delimiter loop
        if ch = high_ascii_escape then  -- high_ascii escape?
           cmdpos := cmdpos + 1;
        end if;
        word := word & script( cmdpos );
        cmdpos := cmdpos + 1;
     end loop;
     lastpos := cmdpos - 1; -- last char
     cmdpos := cmdpos + 1; -- skip delim
     token := word_t;
     identifiers( word_t ).value.all := word;
     return;

  elsif  ch = immediate_symbol_delimiter then

     -- Immediate symbols (imm_symbol_delim_t)
     --
     -- The preprocessor will have placed delimiters around the symbols
     -- with complex characters that are not obvious sybmols, such as 2>&1
     -- (a leading number and ending with a number).  By using delimiters
     -- as bookends, we'll read these faster.

     cmdpos := cmdpos+1;                                      -- continue
     lastpos := cmdpos;                                       -- reading
     while script( lastpos ) /= immediate_symbol_delimiter loop -- until last
        lastpos := lastpos+1;
     end loop;
     lastpos := lastpos-1;
     identifiers( shell_symbol_t ).value.all := To_Unbounded_String( -- extract string
       script( cmdpos..lastpos ) );
     cmdpos := lastpos+2;                                     -- skip delim
     token := shell_symbol_t;                                 -- a shell symbol
     return;

  elsif  ch = immediate_sql_word_delimiter then

     -- Immediate SQL word string literals (sql_word_t)
     --
     -- This is the same as a word_t but a different token value is used so
     -- SparForte will not try to expand the pattern (we don't want "select *"
     -- to be replaced with select and a list of file names).
     --
     -- The preprocessor will have placed delimiters around the word so we
     -- shouldn't have to check for a missing one.

     word := null_unbounded_string;
     cmdpos := cmdpos + 1;
     while script( cmdpos ) /= immediate_sql_word_delimiter loop
        if ch = high_ascii_escape then  -- high_ascii escape?
           cmdpos := cmdpos + 1;
        end if;
        word := word & script( cmdpos );
        cmdpos := cmdpos + 1;
     end loop;
     lastpos := cmdpos - 1; -- last char
     cmdpos := cmdpos + 1; -- skip delim
     token := sql_word_t;
     identifiers( sql_word_t ).value.all := word;
     return;

  elsif (ch >= 'a' and ch <='z') or
        (ch >= 'A' and ch <='Z') or ch = high_ascii_escape then  -- identifier?
 --put_line( "GNT: " & ch ); -- DEBUG

  -- Identifiers or uncompressed keywords
  --
  -- (Note: Control characters and leading underscores filtered out
  -- previously in tokenize stage).  Previously unseen identifiers
  -- will be declared as type "new".

     if ch = high_ascii_escape then                           -- high ascii?
        word := null_unbounded_string;                        -- skip it
     else                                                     -- otherwise
        word := null_unbounded_string & ch;                   -- add to buffer
     end if;
     ch := script( lastpos );                                 -- continue
     while ch /= immediate_word_delimiter loop
         lastpos := lastpos+1;                                -- keep looking
         ch := script( lastpos );                             -- next char
         exit when ch = immediate_word_delimiter;             -- end found? bail
         if ch /= high_ascii_escape then                      -- skip high flag
            word := word & ch;                                -- otherwise
         end if;                                              -- add to buffer
     end loop;
     id := eof_t;                                             -- assume not
     lastpos := lastpos - 1;                                  -- before delim
     findIdent( word, id );

     --for i in reverse 1..identifiers_top-1 loop               -- search symbol
     --    if identifiers( i ).name = word then                 -- table
     --       id := i;
     --       exit;
     --    end if;
     --end loop;
     if id = eof_t then                                       -- not found?
-- todo: should probably be "otherClass" by default
        declareIdent( token, word, new_t );                   -- declare it
     else                                                     -- otherwise
        if identifiers( id ).deleted then                     -- was deleted?
           identifiers( id ).deleted := false;                -- redeclare
           identifiers( id ).kind := new_t;                   -- with type new
           identifiers( id ).renaming_of := identifiers'first; -- cautious
        end if;                                               -- either way
        token := id;                                          -- return id
     end if;                                                  -- skip delim
     cmdpos := lastpos + 2;                                   -- next from here
     return;

  -- Check for a compressed keyword (represented with high bit set)

  elsif ch > ASCII.DEL then                                   -- > ASCII 127
     -- token := identifier( character'pos( script( cmdpos ) ) - 128 );
     -- cmdpos := cmdpos + 1;
     toIdentifier( script( cmdpos ), script( cmdpos + 1 ), token , adj );
     cmdpos := cmdpos + adj;
     -- special VM tokens here
     --
     -- load_nr: load value from numeric register.  Treat it like a number
     -- literal because that's all that's currently loaded into VMNR.
     -- load_sr: load value from string register.  Treat it like a string
     -- literal because that's all that's currently loaded into VMSR.
     --if token = load_nr_t then
     --   token := number_t;
     --   identifiers( token ).value := VMNR(aVMNRNumber(character'pos(script(cmdpos))-1));
     --   cmdpos := cmdpos + 1;
     --elsif token = load_sr_t then
     --   token := strlit_t;
     --   identifiers( token ).value := VMSR(aVMSRNumber(character'pos(script(cmdpos))-1));
     --   cmdpos := cmdpos + 1;
     --end if;
     return;

  elsif is_digit( ch ) then                                   -- a digit?

     -- Numeric literals (number_t)

     is_based_number := false;
     lastpos := lastpos+1;                                    -- continue
     ch := script( lastpos );                                 -- reading
     while is_digit( ch ) or ch = '_' or ch = '.' or
        ch = '#' or (ch >= 'a' and ch <= 'f') or
        (ch >= 'A' and ch <= 'F') loop  -- until end
         if script( lastpos..lastpos+1 ) = ".." then -- ".."  -- of number
            exit;
         end if;
         if ch = '#' then
            is_based_number := true;
         end if;
         lastpos := lastpos+1;
         ch := script( lastpos );
         exit when ch = ASCII.NUL;
     end loop;
     lastpos := lastpos - 1;
     if script( lastpos ) = '.' then
        err( "no digits after decimal" );
        cmdpos := lastpos+1;                                 -- advance posn
        return;
     end if;
     if is_based_number then
        begin
           identifiers( number_t ).value.all := to_unbounded_string(
              natural'image( natural'value( ' ' & script( cmdpos..lastpos ) ) ) );
        exception when others =>
           err( "invalid based numeric literal" );
        end;
     else
        identifiers( number_t ).value.all := To_Unbounded_String( -- extract number
           ' ' & script( cmdpos..lastpos ) );
     end if;
     cmdpos := lastpos+1;                                 -- advance posn
     token := number_t;                                   -- numeric lit.
     return;

  elsif ch = ''' then                                         -- single quote?

     -- Character literal / single quote string literals (charlit_t)
     --
     -- (Note: Missing trailing quote handled in tokenizing stage.)

     cmdpos := cmdpos+1;                                      -- continue
     lastpos := cmdpos;                                       -- reading
     -- SPECIAL CASE: ''' (single quoted single quote)
     if script( lastpos ) = ''' then                     -- ''?
        if script( lastpos+1 ) = ''' then                -- '''?
           lastpos := lastpos+2;                         -- skip literal
           cmdpos := lastpos;                            -- start here next
           identifiers( charlit_t ).value.all := To_Unbounded_String( "'" );
                                                         -- extract string
           token := charlit_t;                           -- char literal
           return;                                       -- that's it
        end if;                                          -- fall through
     end if;
     -- NORMAL CASE
     if script( lastpos ) = high_ascii_escape then
        lastpos := lastpos + 1;
     end if;
     identifiers( charlit_t ).value.all := To_Unbounded_String( -- extract string
       "" & script( lastpos ) );
     lastpos := lastpos+1;
     cmdpos := lastpos+1;                                     -- skip last '
     token := charlit_t;                                      -- char literal
     return;

  elsif ch = '"' then                                         -- double quote?

     -- Double quote string literals (strlit_t)
     --
     -- (Note: Missing trailing quote handled in tokenizing stage.)

     cmdpos := cmdpos+1;                                      -- continue
     lastpos := cmdpos;                                       -- reading
     -- SPECIAL CASE: """" (double quoted double quote)
     if script( lastpos ) = '"' then                     -- ""?
        if script( lastpos+1 ) = '"' then                -- """?
           if script( lastpos+2 ) = '"' then             -- """"?
              lastpos := lastpos+3;                      -- skip literal
              cmdpos := lastpos;                         -- start here next
              identifiers( strlit_t ).value.all := To_Unbounded_String( """" );
                                                         -- extract string
              token := strlit_t;                         -- string literal
              return;                                    -- that's it
           end if;
        end if;                                          -- fall through
     end if;
     -- NORMAL CASE
     identifiers( strlit_t ).value.all := Null_Unbounded_String;
     while script( lastpos ) /= '"' loop                 -- until last "
        if script( lastpos ) = high_ascii_escape then
           lastpos := lastpos + 1;
        end if;
        identifiers( strlit_t ).value.all := identifiers( strlit_t ).value.all &
          script( lastpos );
        lastpos := lastpos + 1;
     end loop;
     cmdpos := lastpos+1;                                     -- skip last "
     token := strlit_t;                                       -- string literal
     return;

  elsif ch = '`' then                                         -- back quote?

     -- Backquoted strings (backlit_t)
     --
     -- (Note: Missing trailing quote handled in tokenizing stage.)
     -- TODO: this would run faster if the compiler but unique symbols
     -- so the backslashes would not have to be parsed here.

     cmdpos := cmdpos+1;                                      -- continue
     lastpos := cmdpos;                                       -- reading
     identifiers( backlit_t ).value.all := Null_Unbounded_String;
     inBackslash := false;
     while not (script( lastpos ) = '`' and not inBackslash) loop -- until last `
        -- allow anything to be backslashed. the shell parser will
        -- check if it's legitimate later.
        if inBackslash then
           inBackslash := false;
        elsif script( lastpos ) = '\' then
           inBackslash := true;
        end if;
        if script( lastpos ) = high_ascii_escape then
           lastpos := lastpos + 1;
        end if;
        identifiers( backlit_t ).value.all := identifiers( backlit_t ).value.all &
            script( lastpos );
        lastpos := lastpos + 1;
     end loop;
     -- lastpos := lastpos+1;
     cmdpos := lastpos+1;                                     -- skip last `
     token := backlit_t;                                      -- string literal
     return;

  else

   -- Anything else is a symbol token (symbol_t)

     case ch is
     when '$' =>                                              -- $$ $# $?
          cmdpos := cmdpos + 1;                               -- $0..$9
          if script( cmdpos ) = '$' then
             cmdpos := cmdpos + 1;
          elsif script( cmdpos ) = '#' then
             cmdpos := cmdpos + 1;
          elsif script( cmdpos ) = '?' then
             cmdpos := cmdpos + 1;
          elsif script( cmdpos ) = '!' then
             cmdpos := cmdpos + 1;
          elsif script( cmdpos ) >= '0' and script( cmdpos ) <= '9' then
             cmdpos := cmdpos + 1;
          end if;
     when '#' =>                                              -- # comments
          cmdpos := cmdpos + 1;
          while script( cmdpos ) /= ASCII.NUL loop
                cmdpos := cmdpos + 1;
          end loop;
          goto redo;
     when '-' =>                                              -- - / -- comment
          cmdpos := cmdpos + 1;
          if script( cmdpos ) = '-' then
             while script( cmdpos ) /= ASCII.NUL loop
                   cmdpos := cmdpos + 1;
             end loop;
             goto redo;
          end if;
     when '=' =>                                              -- = / =>
          cmdpos := cmdpos + 1;
          if script( cmdpos ) = '>' then
             cmdpos := cmdpos + 1;
          end if;
     when ':' =>                                              -- : / :=
          cmdpos := cmdpos + 1;
          if script( cmdpos ) = '=' then
             cmdpos := cmdpos + 1;
          end if;
     when '*' =>                                              -- * / **
          cmdpos := cmdpos + 1;
          if script( cmdpos ) = '*' then
             cmdpos := cmdpos + 1;
          end if;
     when '>' =>                                              -- > / >=
          cmdpos := cmdpos + 1;
          if script( cmdpos ) = '=' then
             cmdpos := cmdpos + 1;
          end if;
     when '<' =>                                              -- < / <=
          cmdpos := cmdpos + 1;
          if script( cmdpos ) = '=' then
             cmdpos := cmdpos + 1;
          end if;
     when '/' =>                                              -- '/' / /=
          cmdpos := cmdpos + 1;
          if script( cmdpos ) = '=' then
             cmdpos := cmdpos + 1;
          end if;
     when '.' =>                                              -- . / ..
          cmdpos := cmdpos + 1;
          if script( cmdpos ) = '.' then
             cmdpos := cmdpos + 1;
          end if;
     when '_' =>                                              -- _ test
          err( "Leading underscores not allowed in identifiers" );
          cmdpos := cmdpos+1;                                 -- advance posn
          return;
     when '!' =>                                              -- ! / != test
          if script( cmdpos+1 ) = '=' then
             err( "/= expected" );
          end if;
          cmdpos := cmdpos+2;                                 -- advance posn
          return;
     when others =>                                           -- return other
          cmdpos := cmdpos + 1;                               -- as a symbol
     end case;
     lastpos := cmdpos-1;                                     -- end of token
     identifiers( symbol_t ).value.all := To_Unbounded_String(
        script( firstpos..lastpos ) );
     token := symbol_t;                                       -- a symbol
     return;
  end if;
end getNextToken;


-----------------------------------------------------------------------------
--  EXPECT
--
-- Check for the specified identifier.  If the current token matches,
-- get the next token, otherwise show an error.
-----------------------------------------------------------------------------

procedure expect( expected_token : identifier ) is
begin
  if token /= expected_token then
     if expected_token = keyword_t then
        err( "keyword expected" );
     elsif expected_token = number_t then
        err( "number expected" );
     elsif expected_token = strlit_t then
        err( "string literal expected" );
     elsif expected_token = symbol_t then
        err( "symbol expected" );
     else
        err( to_string( identifiers( expected_token ).name ) & " expected" );
     end if;
  end if;
  getNextToken;
end expect;


-----------------------------------------------------------------------------
--  EXPECT
--
-- Check for the specified identifier and value.  If the current token
-- and its value matches, get the next token, otherwise show an error.
-----------------------------------------------------------------------------

procedure expect( expected_token : identifier; value : string ) is
begin
  if token /= expected_token then
     if expected_token = keyword_t then
        err( "keyword expected" );
     elsif expected_token = number_t then
        err( "number expected" );
     elsif expected_token = strlit_t then
        err( "string literal expected" );
     elsif expected_token = symbol_t then
        err( "symbol expected" );
     else
        err( to_string( identifiers( expected_token ).name ) & " expected" );
     end if;
  end if;
  if value /= to_string( identifiers( token ).value.all ) then
      err( "'" & value & "' expected" );
  end if;
  getNextToken;
end expect;


-----------------------------------------------------------------------------
--  EXPECT SEMICOLON
--
-- Because it happens a lot.
-----------------------------------------------------------------------------

procedure expectSemicolon is
begin
  if token = symbol_t and identifiers( token ).value.all = ":" then
     err( "':' should  be ';'" );
  else
     expect( symbol_t, ";" );
  end if;
end expectSemicolon;


-----------------------------------------------------------------------------
--  SKIP WHITESPACE
--
-- Move scanner position to the first non-white space character
-- (that is, spaces or tabs in the tokenized script).
-----------------------------------------------------------------------------

procedure skipWhiteSpace is
begin
  firstPos := cmdpos;
  while script( firstPos ) /= ASCII.NUL loop
    exit when script( firstPos ) /= ' ' or script( firstpos ) = ASCII.HT;
    firstPos := firstPos + 1;
  end loop;
  cmdPos := firstPos;
end skipWhiteSpace;


------------------------------------------------------
--
-- Saving/Restoring Position
--
------------------------------------------------------


-----------------------------------------------------------------------------
--  MARK SCANNER
--
-- Record the current state of the scanner, including the token
-- and the position in the current line.
-----------------------------------------------------------------------------

procedure markScanner( scannerState : out aScannerState ) is
begin
  scannerState.token   := token;
  scannerState.first   := firstpos;
  scannerState.cmdpos  := cmdpos;
  scannerState.last    := lastpos;
  scannerState.itself  := itself;
  scannerState.itself_type := itself_type;
  scannerState.last_output := last_output;
  scannerState.last_output_type := last_output_type;
  scannerState.err_exception := err_exception;
  -- because value is now a pointer
  scannerState.err_exception.value := scannerState.err_exception.svalue'unchecked_access;
  if token = symbol_t or token = strlit_t or token = charlit_t or token = number_t or token = word_t then
     scannerState.value := identifiers( token ).value.all;
  end if;
end markScanner;


-----------------------------------------------------------------------------
--  RESUME SCANNING
--
-- Restore the scanner to a previously recorded position, to continue
-- execution at that place.
-----------------------------------------------------------------------------

procedure resumeScanning( scannerState : aScannerState ) is
begin
  token    := scannerState.token;
  firstpos := scannerState.first;
  cmdpos   := scannerState.cmdpos;
  lastpos  := scannerState.last;
  itself   := scannerState.itself;
  itself_type := scannerState.itself_type;
  last_output := scannerState.last_output;
  last_output_type := scannerState.last_output_type;
  -- If an exception occurred, do not erase it from a previous state
  -- because value is now a pointer, make sure it points to the right
  -- stoarge.
  if length( err_exception.name ) = 0 then
     err_exception := scannerState.err_exception;
     err_exception.value := err_exception.svalue'access;
  end if;
  if token = symbol_t or token = strlit_t or token = charlit_t or token = number_t or token = word_t then
     identifiers( token ).value.all := scannerState.value;
  end if;
end resumeScanning;


-----------------------------------------------------------------------------
--  IS VALID
--
-- True if the script state contains a saved script state.
-----------------------------------------------------------------------------

function isValid( scriptState : aScriptState ) return boolean is
begin
  return scriptState.script /= null;
end isValid;


-----------------------------------------------------------------------------
--  SAVE SCRIPT
--
-- Save scanner state plus the current script so that a new
-- script can be executed.  The error flag, syntax check flag,
-- etc. are not saved.
-----------------------------------------------------------------------------

procedure saveScript( scriptState : out aScriptState ) is
begin
  if script = null then
     err( gnat.source_info.source_location & ": Internal error: saveScript has no script to save" );
  end if;
  markScanner( scriptState.scannerState );
  scriptState.script := script;
  scriptState.size := identifiers( source_info_script_size_t ).value.all;
  scriptState.inputMode := inputMode;
  script := null;
end saveScript;


-----------------------------------------------------------------------------
--  RESTORE SCRIPT
--
-- Restore a previously saved script, destroying the current one
-- (if any).  Execution will continue where it previously left
-- off.
-----------------------------------------------------------------------------

procedure restoreScript( scriptState : in out aScriptState ) is
begin
  if scriptState.script = null then
     err( gnat.source_info.source_location & ": Internal error: restoreScript has no script to restore" );
  end if;
  if script /= null then
     free( script );
  end if;
  script := scriptState.script;
  scriptState.script := null;
  inputMode := scriptState.inputMode;
  identifiers( source_info_script_size_t ).value.all := scriptState.size;
  resumeScanning( scriptState.scannerState );
end restoreScript;


-----------------------------------------------------------------------------
--
-- SCRIPT MANIPULATION / INCLUDE FILES
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  REPLACE SCRIPT WITH FRAGMENT
--
-- Switch the current byte code script with another byte code script string.
-- The byte code is assumed to be a piece of byte code and requires the
-- headers/trailers added to the byte code.
-----------------------------------------------------------------------------

procedure replaceScriptWithFragment( bytecode : unbounded_string ) is
  ci : compressionInfo;
begin
  if script /= null then                                      -- discard script
     free( script );
  end if;
  cmdpos := firstScriptCommandOffset; -- Reset cmdpos to beginning of script
  resetLineNo;
  beginByteCode( ci );
  ci.compressedScript := ci.compressedScript & bytecode;
  endByteCode( ci );
  --if verboseOpt then
  --   dumpByteCode( ci );
  --end if;
  script := new string( 1..length( ci.compressedScript ) );  -- alloc script
  script.all := to_string( ci.compressedScript );            -- and copy
end replaceScriptWithFragment;


-----------------------------------------------------------------------------
--  REPLACE SCRIPT
--
-- Switch the current byte code script with another byte code script string.
-----------------------------------------------------------------------------

procedure replaceScript( bytecode : unbounded_string ) is
  ci : compressionInfo;
begin
  if script /= null then                                      -- discard script
     free( script );
  end if;
  ci.compressedScript := ci.compressedScript & bytecode;
  --if verboseOpt then
  --   dumpByteCode( ci );
  --end if;
  script := new string( 1..length( ci.compressedScript ) );  -- alloc script
  script.all := to_string( ci.compressedScript );            -- and copy
end replaceScript;

-----------------------------------------------------------------------------
--  LOAD INCLUDE FILE
--
--
-----------------------------------------------------------------------------

procedure loadIncludeFile( includeName : unbounded_string; fileLocation : out SourceFilesList.aListIndex; includeText : out unbounded_string ) is
  workingPaths : unbounded_string;
  includeFileOpened : boolean;
  includeFileGood   : boolean;
  includeDirGood    : boolean;
  temp_id : identifier;
  libraryPrefix :unbounded_string;
  libraryPrefixNumber : natural;

  ---------------------------------------------------------------------------
  --  FIND INCLUDE FILE (LOAD INCLUDE FILE)
  --
  -- Find not working
  -- sourceFilesList.Find( sourceFiles, sfr, foundAt =>fileLocation );
  -- doesn't seem to be using equal()??
  ---------------------------------------------------------------------------
  -- TODO: investigate this

  function find_include_file( includeName : unbounded_string ) return SourceFilesList.aListIndex is
     sfr, temp_sfr : aSourceFile;
  begin
     sfr.pos  := lastPos;                                  -- pos of include
     sfr.name := includeName;                              -- name of file
     fileLocation := 0;                                     -- assume failure
     for i in 1..sourceFilesList.Length( sourceFiles ) loop -- in incl. files
         sourceFilesList.Find( sourceFiles, i, temp_sfr );  -- get one
         if equal( temp_sfr, sfr )  then                    -- already incl.?
            fileLocation := i;                              -- get the number
         end if;
     end loop;
     return fileLocation;
  end find_include_file;

  ---------------------------------------------------------------------------
  --  LOAD INCLUDE FILE (LOAD INCLUDE FILE)
  --
  ---------------------------------------------------------------------------

  function load_include_file( include_file : file_type; path : string ) return unbounded_string is
  begin
    if trace or verboseOpt = true then
       put_trace( "Including " & to_string( toEscaped( to_unbounded_string( path ) ) ) );
    end if;
    while not end_of_file( include_file ) loop
       includeText := includeText & ada.strings.unbounded.text_io.get_line( include_file ) & ASCII.LF;
    end loop;
    return includeText;
  end load_include_file;

begin

  includeText := null_unbounded_string;                    -- clear text
  fileLocation := find_include_file( includeName );

  if fileLocation /= 0 then                                -- already incl.?
     if trace or verboseOpt = true then
        put_trace( "file already included" );              -- note it
     end if;
  elsif sourceFilesList.Length( sourceFiles ) = 255 then   -- too many?
     -- 255 is one byte minus 0, which is reserved
     err( optional_inverse( "too many include files and subunits" ) );
  else                                                     -- new file

     includeFileGood := true;
     includeDirGood := true;

     -- Include Files by Absolute Paths

     if element( includeName, 1 ) = '/' then

        declare
          path : string := to_string( includeName );
          include_file : file_type;
          includeParentDir : unbounded_string;
        begin
          includeParentDir := dirname( to_unbounded_string( path ) );
          open( include_file, in_file, path );
          if C_is_secure_dir( to_string( includeParentDir ) & ASCII.NUL ) then
             if C_is_includable_file( path & ASCII.NUL ) then

                -- If the security checks pass, the file can be included.
                -- Read in the text.

                includeText :=  load_include_file( include_file, path );
                includeFileOpened := true;
             else
                includeFileGood := true;
             end if;
          else
             includeDirGood := false;
          end if;
          close( include_file );
        exception
            when STATUS_ERROR =>
              err( "cannot open include file" & optional_bold( to_string( toEscaped( includeName ) ) ) &
                 " - file may be locked" );
              return;
            when NAME_ERROR =>
                if traceOpt then
                   put_trace( "Cannot open " & to_string( toEscaped( libraryPrefix & includeName ) ) );
                end if;
            when MODE_ERROR =>
                err( "interal error: mode error on include file " & optional_bold( to_string( toEscaped( includeName ) ) ) );
                 return;
            when END_ERROR =>
              err( "interal error: end of file reached on include file " & optional_bold( to_string( toEscaped( includeName ) ) ) );
            return;
            when others =>
               err( "interal error: unexpected error reading " & optional_bold( to_string( toEscaped( includeName ) ) ) );
               return;
        end;

     else

        -- Include Files by Relative Paths
        --
        -- The working paths, or the paths to search, include any command line
        -- paths provided with -L, and any paths in the SPAR_LIBRARY_PATH
        -- environment variable.  If there are no working paths, then we still
        -- want to search the current directory.

        workingPaths := libraryPath;                          -- use -L paths
        if length( workingPaths ) = 0 then                    -- none?
           workingPaths := to_unbounded_string( "." );        -- use current
        end if;                                               -- check env var
        findIdent( to_unbounded_string( "SPAR_LIBRARY_PATH" ), temp_id );
        if temp_id /= eof_t then                              -- exists?
           if length( identifiers( temp_id ).value.all ) > 0 then  -- non-blank?
              workingPaths := workingPaths & ":" & identifiers( temp_id ).value.all;
           end if;
        end if;

        includeFileOpened := false;                           -- assume failure
        libraryPrefixNumber := 1;                             -- prefix one
        loop                                                  -- get next prefix
           libraryPrefix := stringField( workingPaths, ':', libraryPrefixNumber );
         exit when length( libraryPrefix ) = 0;               -- quit if none
           if element( libraryPrefix, length( libraryPrefix ) ) /= directory_delimiter then
              libraryPrefix := libraryPrefix & directory_delimiter;
           end if;

           declare
             path         : string     := to_string( libraryPrefix & includeName );
             include_file : file_type;
             includeParentDir : unbounded_string;
           begin
             open( include_file, in_file, path );

             -- If we get here, then the file was able to be opened.  If the
             -- file can be opened, it may still be insecure.  Or the parent
             -- directory may be insecure.

             includeParentDir := dirname( to_unbounded_string( path ) );
             if C_is_secure_dir( to_string( includeParentDir ) & ASCII.NUL ) then
                if C_is_includable_file( path & ASCII.NUL ) then

                   -- If the security checks pass, the file can be included.
                   -- Read in the text.  Remember we found it and stop the
                   -- search.

                   includeText :=  load_include_file( include_file, path );
                   includeFileOpened := true;
                   exit;
                else
                   includeFileGood := false;
                end if;
                close( include_file );
             else
                includeDirGood := false;
             end if;
           exception
               when STATUS_ERROR =>
                 err( "cannot open include file" & optional_bold( to_string( toEscaped( includeName ) ) ) &
                    " - file may be locked" );
                 return;
               when NAME_ERROR =>
                   if traceOpt then
                      put_trace( "Cannot open " & to_string( toEscaped( libraryPrefix & includeName ) ) );
                   end if;
               when MODE_ERROR =>
                   err( "interal error: mode error on include file " & optional_bold( to_string( toEscaped( includeName ) ) ) );
                    return;
               when END_ERROR =>
                 err( "interal error: end of file reached on include file " & optional_bold( to_string( toEscaped( includeName ) ) ) );
               return;
               when others =>
                  err( "interal error: unexpected error reading " & optional_bold( to_string( toEscaped( includeName ) ) ) );
                  return;
           end;
           libraryPrefixNumber := libraryPrefixNumber + 1;  -- next prefix
        end loop;
     end if;

     -- Either the file was found but is not unacceptable or the file was not found
     -- The security flags are only false if the security tests failed on an
     -- existing file.

     if not includeFileGood then
        err( "with separate file " & optional_bold( to_string( toEscaped( includeName ) ) ) &
             " is not readable, is world writable, is not a file or is empty" );
     elsif not includeDirGood then
        err( "with separate file " & optional_bold( to_string( toEscaped( includeName ) ) ) &
             " is in a directory that not readable, is world writable, or is not a directory" );
     elsif not includeFileOpened then
        err( "with separate file " & optional_bold( to_string( toEscaped( includeName ) ) ) &
             " doesn't exist or is not readable" );
        fileLocation := SourceFilesList.aListIndex'last;
     end if;
  end if;
end loadIncludeFile;


-----------------------------------------------------------------------------
--  INSERT INCLUDE
--
-----------------------------------------------------------------------------

procedure insertInclude( includeName : unbounded_string ) is
  sfr : aSourceFile;
  pre_script, post_script : unbounded_string;
  posAfterIncludePragma : natural;
  oldSourceFileNo : natural;
  fileLocation : SourceFilesList.aListIndex;
  new_script : unbounded_string;
  includeSemicolonPosition : aScannerState;
  includeText : unbounded_string;
begin
  if script = null then                                       -- no script?
     err( Gnat.Source_Info.Source_Location & "internal_error: no script" );
  else

     -- includes are based on a byte code position (the file name is not
     -- unique).  The file name is used for error reporting, to identify
     -- the included file.

     -- Not an error to include twice, just ignore.
     -- This is because it's not possible to determine if this is the initial
     -- syntax check or some other use of syntax mode (block skipping, for
     -- example)

     loadIncludeFile( includeName, fileLocation, includeText );
     if fileLocation > 0 then                                -- already exist?
        return;                                              -- don't include
     end if;                                                 -- or error

     sfr.pos  := lastPos;
     sfr.name := includeName;

     -- Split the script around the include point (i.e. line after the include)
     posAfterIncludePragma := lastPos + 1; -- after ';'
     while script(posAfterIncludePragma) /= ASCII.NUL loop
           posAfterIncludePragma := posAfterIncludePragma + 1; -- after ';'
     end loop;
     posAfterIncludePragma := posAfterIncludePragma + 1; -- first position in next line
     pre_script := to_unbounded_string( script(1..posAfterIncludePragma-1 ) );
     post_script := to_unbounded_string( script(posAfterIncludePragma..script'last ) );

     -- Set the new source file number
     oldSourceFileNo := sourceFileNo;
     sourceFilesList.Queue( sourceFiles, sfr );
     sourceFileNo := natural( sourceFilesList.Length( sourceFiles ) - 1 );

     -- save position, compile include file and insert the byte code
     -- record the size of the new script for source_info.script_size
     markScanner( includeSemicolonPosition );
     compileInclude( includeText );
     new_script := pre_script & to_unbounded_string( script.all ) & post_script;
     replaceScript( new_script );
     identifiers( source_info_script_size_t ).value.all := delete( to_unbounded_string( script.all'length'img), 1, 1 );
     resumeScanning( includeSemicolonPosition );
  end if;
end insertInclude;


-----------------------------------------------------------------------------
--  SET TEMPLATE NAME
--
-- Replace the name of the main program with the name of the template
-- file so that error messages have the template file name.
--
-- This should probably be something more elegant.
-----------------------------------------------------------------------------

procedure setTemplateName is
  sfr        : aSourceFile;
begin
  sourceFilesList.Find( sourceFiles, 1, sfr );
  sfr.name := templatePath;
  sourceFilesList.Replace( sourceFiles, 1, sfr );
end setTemplateName;

end scanner;
