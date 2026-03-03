------------------------------------------------------------------------------
-- Text_IO Package                                                          --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2026 Free Software Foundation              --
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
    ada.exceptions,
    ada.text_io,
    ada.strings.unbounded.text_io,
    gnat.source_info,
    pegasoft.strings,
    pegasoft.user_io,
    pegasoft.user_io.getline,
    signal_flags,
    message_strings,
    value_conversion,
    scanner.calendar,
    scanner.communications,
    parser_aux,
    parser_cal,
    parser,
    parser_params;
use interfaces.c,
    ada.exceptions,
    ada.text_io,
    ada.strings.unbounded.text_io,
    pegasoft,
    pegasoft.user_io,
    pegasoft.strings,
    signal_flags,
    message_strings,
    value_conversion,
    scanner,
    scanner.calendar,
    scanner.communications,
    parser_aux,
    parser_cal,
    parser,
    parser_params;

package body parser_tio is


-- ASSIGN RENAMED FILE
--
-- Reassign a renaming in the symbol table, based on the given reference.
-- This is used by Text_IO's set_input, set_output and set_error to select
-- the current file for input, output and error respectively.  new_id
-- should be current_input_t, current_output_t or current_error_t.
-- This does not setup aggregate renamings, which must be done by the
-- caller.
-- The canonical identifier will have its renaming count incremented.
-----------------------------------------------------------------------------

procedure assignRenamedFile( subprogramId : identifier; current_file_id : identifier; canonicalRef :
  reference ) is
begin

  -- Check for a valid file.  This is redundant since these are checked in the
  -- relevant parsers.

  if current_file_id /= current_input_t and current_file_id /= current_output_t and
     current_file_id /= current_error_t then
     err( contextNotes => pl( "At " & gnat.source_info.source_location &
            " while checking for the end of file" ),
          subject => subprogramId,
          reason => +"had an internal error because",
          obstructorNotes => em( "the file was not current_input, current_output or current_error" )
      );
      return;
  end if;

  -- First, unassign the current renaming.  When this is first run by the
  -- interpreter, there won't be an existing naming so this step can be
  -- skpped.  Reduce the renaming counter on the old canonical variable.
  -- This is based on deleteIdent().

  if identifiers( current_file_id ).renaming_of /= identifiers'first then
     declare
        oldCanonicalId : constant identifier := identifiers( current_file_id ).renaming_of;
     begin
        if identifiers( oldCanonicalId ).renamed_count > 0 then
           identifiers( oldCanonicalId ).renamed_count :=
           identifiers( oldCanonicalId ).renamed_count - 1;
        else
           err(
                contextNotes => pl( "At " & gnat.source_info.source_location &
                    " while unassigning the current file" ),
                subject => current_file_id,
                reason => pl( "had an internal error because the dereferenced identifier's renamed_count "  &
                     "is unexpectedly zero for" ),
                obstructor => oldCanonicalId
           );
        end if;
      end;
    end if;

    -- Second, create a new renaming to the new file_type variable.
    -- This is based on declareRenaming().

    -- These are the relevant declaration fields that need to be redirected
    -- in a renaming of an existing variable.  The store field is handled
    -- in the next step.

    identifiers( current_file_id ).kind := identifiers( canonicalRef.id ).kind;
    identifiers( current_file_id ).class := identifiers( canonicalRef.id ).class;
    identifiers( current_file_id ).static   := identifiers( canonicalRef.id ).static;
    identifiers( current_file_id ).list     := identifiers( canonicalRef.id ).list;
    identifiers( current_file_id ).field_of := identifiers( canonicalRef.id ).field_of;
    identifiers( current_file_id ).inspect  := identifiers( canonicalRef.id ).inspect;

    identifiers( current_file_id ).genKind := identifiers( canonicalRef.id ).genKind;
    identifiers( current_file_id ).genKind2 := identifiers( canonicalRef.id ).genKind2;
    identifiers( current_file_id ).firstBound := identifiers( canonicalRef.id ).firstBound;
    identifiers( current_file_id ).lastBound := identifiers( canonicalRef.id ).lastBound;
    identifiers( current_file_id ).sstorage := identifiers( canonicalRef.id ).sstorage;
    identifiers( current_file_id ).astorage := identifiers( canonicalRef.id ).astorage;
    identifiers( current_file_id ).renaming_of := canonicalRef.id;

    -- Third, assign the store pointer to the file_type value.
    -- The canonical reference itself may be another renaming.  We must follow
    -- the chain to the root canonical variable and point the store field to
    -- its value.  There is a timeout to prevent infinite loops.

     declare
        deref_id : identifier := canonicalRef.id;
        cnt : natural := 1;
     begin
        while identifiers( deref_id ).renaming_of /= identifiers'first loop
           deref_id := identifiers( deref_id ).renaming_of;
           cnt := cnt + 1;
           if cnt > 1000 then
              raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location &
                 ": internal error: infinite renaming loop";
           end if;
        end loop;
        identifiers( current_file_id ).store := identifiers( deref_id ).store;
     end;

     -- if the renaming is an array element, the type is the type of the
     -- array element, not the array type itself.  The element type should
     -- just be the "kind".

     if identifiers( canonicalRef.id ).list and canonicalRef.hasIndex then
        identifiers( current_file_id ).list := false;
     end if;

     -- Fourth, adjust the renaming counter on the canonical reference to
     -- indicate another renaming has been created.

     identifiers( canonicalRef.id ).renamed_count :=
        identifiers( canonicalRef.id ).renamed_count + 1;
end assignRenamedFile;

procedure ParseOpenFile( subprogram_id : identifier; return_ref : out reference ) is
  -- Reads a file record for a file that should be open
  -- standard output, standard error or a user file
  -- the file must be closed (ie value of variable is null)
  theFileRec : storage;
  ref : reference;
begin
  ref.id := eof_t;
  return_ref.id := eof_t; -- assume failure

  ParseInOutParameter( subprogram_id, ref, eof_t );
  if getUniType( ref.kind ) /= file_type_t then
     err( +"file_type expected" );
  end if;

  -- Verify that the identifier is a file.  Unless during a syntax check,
  -- verify that the file is also open (files are not opened during a
  -- syntax check).

  if not syntax_check then
     --theFileRec := identifiers( ref.id ).store.all;
     getParameterValue( ref, theFileRec );
     if length( theFileRec.value ) = 0 then
          err( context => subprogram_id,
             subject => ref.id,
             subjectType => ref.kind,
             reason => +"is not open",
             obstructorNotes => nullMessageStrings
          );
     elsif not error_found then
        return_ref := ref;
     end if;
  end if;
end ParseOpenFile;

procedure ParseOpenSocket( subprogram_id : identifier; return_ref : out reference ) is
  -- Reads a file record for a socket that should be open
  -- standard output, standard error or a user file
  -- the file must be closed (ie value of variable is null)
  theFileRec : storage;
  ref : reference;
begin
  ref.id := eof_t;
  return_ref.id := eof_t; -- assume failure

  ParseInOutParameter( subprogram_id, ref, eof_t );
  if getUniType( ref.kind ) /= socket_type_t then
     err( +"file_type or socket_type variable expected" );
  end if;

  -- Verify that the identifier is a socket.  Unless during a syntax check,
  -- verify that the socket is also open (sockets are not opened during a
  -- syntax check).

  if not syntax_check then
     getParameterValue( ref, theFileRec );
     if length( theFileRec.value ) = 0 then
        err( +"file not open" );
     elsif not error_found then
        return_ref := ref;
     end if;
  end if;
end ParseOpenSocket;

procedure ParseOpenFileOrSocket( subprogram_id : identifier; return_ref : out reference; kind : out identifier ) is
  -- Reads a file record for a file or socket that should be open
  -- standard output, standard error or a user file
  -- the file must be closed (ie value of variable is null)
  theFileRec : storage;
  ref : reference;
begin
  ref.id := eof_t;
  ref.kind := eof_t;
  return_ref.id := eof_t; -- assume failure
  return_ref.kind := eof_t;

  ParseInOutParameter( subprogram_id, ref, eof_t );
  if ref.kind /= file_type_t and ref.kind /= socket_type_t then
     err( +"file_type or socket_type variable expected" );
  end if;
  kind := ref.kind;

  -- Verify that the identifier is a file or socket.  Unless during a
  -- syntax check, verify that the file/socket is also open (they are
  -- not opened during a syntax check).

  if not syntax_check then
     getParameterValue( ref, theFileRec );
     if length( theFileRec.value ) = 0 then
        err( +"file not open" );
     elsif not error_found then
        return_ref := ref;
     end if;
  end if;
end ParseOpenFileOrSocket;

procedure ParseClosedFile( r : out reference ) is
  -- user file, must be closed (ie. value of variable not null)
  theFileRec : storage;
  ref : reference;
begin
  ref.id := eof_t; -- assume failure

  -- do not allow "current" or "standard" files to be closed.

  if token = standard_output_t then
     err( +"file already open" );
  elsif token = standard_error_t then
     err( +"file already open" );
  elsif token = current_input_t then
     err( +"file already open" );
  elsif token = current_output_t then
     err( +"file already open" );
  elsif token = current_error_t then
     err( +"file already open" );
  else
     ParseOutParameter( ref, file_type_t );
     -- GCC Ada 4.7.2 raises STORAGE_ERROR here under certain circumstances
     -- if a put_line doesn't appear.

  -- Verify that the identifier is a file.  Unless during a syntax check,
  -- verify that the file is also open (files are not opened during a
  -- syntax check).

     if getUniType( ref.kind ) /= file_type_t then
        err( +"expected file_type variable" );
     elsif not syntax_check then
        getParameterValue( ref, theFileRec );
        if length( theFileRec.value ) > 0 then
           err( +"file already open" );
        elsif not error_found then
           r := ref;
        end if;
     end if;
  end if;
end ParseClosedFile;

procedure ParseClosedSocket( r : out reference ) is
  -- user file, must be closed (ie. value of variable not null)
  --id : identifier;
  theSocketRec : storage;
  ref : reference;
begin
  ref.id := eof_t; -- assume failure

  -- do not allow "current" or "standard" files to be closed.

  if token = standard_output_t then
     err( +"file already open" );
  elsif token = standard_error_t then
     err( +"file already open" );
  elsif token = current_input_t then
     err( +"file already open" );
  elsif token = current_output_t then
     err( +"file already open" );
  elsif token = current_error_t then
     err( +"file already open" );
  else
     ParseOutParameter( ref, socket_type_t );

  -- Verify that the identifier is a socket.  Unless during a syntax check,
  -- verify that the socket is also open (sockets are not opened during a
  -- syntax check).

     if getUniType( identifiers( ref.id ).kind ) /= socket_type_t then
        err( +"expected socket_type variable" );
     elsif not syntax_check then
        getParameterValue( ref, theSocketRec );
        if length( theSocketRec.value ) > 0 then
           err( +"file already open" );
        elsif not error_found then
           r := ref;
        end if;
     end if;
  end if;
end ParseClosedSocket;

procedure ParseClosedFileOrSocket( return_ref : out reference; kind : out identifier ) is
  -- user file, must be closed (ie. value of variable not null)
  theFileRec : storage;
  ref : reference;
begin
  return_ref.id := eof_t; -- assume failure

  -- do not allow "current" or "standard" files to be closed.

  if token = standard_output_t then
     err( +"file already open" );
  elsif token = standard_error_t then
     err( +"file already open" );
  elsif token = current_input_t then
     err( +"file already open" );
  elsif token = current_output_t then
     err( +"file already open" );
  elsif token = current_error_t then
     err( +"file already open" );
  else
     ParseOutParameter( ref, file_type_t );

  -- Verify that the identifier is a file or socket.  Unless during a
  -- syntax check, verify that the file/socket is also open (they are
  -- not opened during a syntax check).

     kind := ref.kind;
     if kind /= file_type_t and kind /= socket_type_t then
        err( +"file_type or socket_type variable expected" );
     elsif not syntax_check then
        getParameterValue( ref, theFileRec );
        if length( theFileRec.value ) > 0 then
           err( +"file already open" );
        elsif not error_found then
           return_ref := ref;
        end if;
     end if;
  end if;
end ParseClosedFileOrSocket;


procedure ParseIsOpen( result : out storage ) is
  -- is_open( file )
  -- Ada.Text_IO.Is_Open
  openFileRef : reference;
  openFileKind : identifier;
  theOpenFileRec : storage;
  subprogramId : constant identifier := is_open_t;
begin
  result := storage'( identifiers( false_t ).store.value, noMetaLabel, sparMetaLabels );
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFileOrSocket( subprogramId, openFileRef, openFileKind );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     GetParameterValue( openFileRef, theOpenFileRec );
     if metaLabelOk( subprogramId, theOpenFileRec ) then
        -- The value of current_input, current_output, current_error are aliases
        -- to the actual file.
        if openFileRef.id = current_input_t then
           openFileRef.id := identifier( to_numeric( identifiers( current_input_t ).store.value ) );
        elsif openFileRef.id = current_output_t then
           openFileRef.id := identifier( to_numeric( identifiers( current_output_t ).store.value ) );
        elsif openFileRef.id = current_output_t then
           openFileRef.id := identifier( to_numeric( identifiers( current_error_t ).store.value ) );
        end if;
        -- No declare block with exception handler because we're not actually
        -- executing anything external to SparForte
        if uniTypesOk( openFileRef.kind, file_type_t ) then
           if length( theOpenFileRec.value ) > 0 then
              result := storage'( identifiers( true_t ).store.value, noMetaLabel, theOpenFileRec.policyMetaLabels );
           end if;
        end if;
     end if;
  end if;
end ParseIsOpen;

procedure ParseEndOfFile( result : out storage; kind : out identifier ) is
  -- Syntax: End_of_file( f )
  -- Source: Ada.Text_IO.End_Of_File
  openFileRef : reference;
  openFileKind : identifier;
  theOpenFileRec : storage;
  subprogramId : constant identifier := end_of_file_t;
begin
  kind := boolean_t;
  result := storage'( to_unbounded_string( boolean'image( false ) ), noMetaLabel, noMetaLabels );
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFileOrSocket( subprogramId, openFileRef, openFileKind );
  if isExecutingCommand then
     GetParameterValue( openFileRef, theOpenFileRec );
     if metaLabelOk( subprogramId, theOpenFileRec ) then
        begin
           if openFileKind = file_type_t then
              if identifier'value( to_string( stringField( openFileRef, mode_field ) ) ) /= in_file_t then
                 err( +"end_of_file only applies to " & em( "in_mode" ) & pl( " files" ) );
              end if;
           end if;
        exception when msg : others =>
           -- 'value can raise a constraint_error if there SparForte corrupts
           -- if the value at an earlier step.  This is unlikely so the catch-all
           -- handler sufficient to handle it and there is no constraint_error
           -- handler.
          err( contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while checking for the end of file" ),
             subject => subprogramId,
             reason => +"had an internal error because",
                obstructorNotes => pl( "an unexpected exception " ) &
                   em_value( to_unbounded_string( Exception_Message( msg ) ) )
          );
        end;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := storage'( stringField( openFileRef, eof_field ), noMetaLabel, theOpenFileRec.policyMetaLabels );
  end if;
end ParseEndOfFile;

procedure ParseEndOfLine( result : out storage; kind : out identifier ) is
  -- Syntax: end_of_line( open-file )
  -- Source: Ada.Text_IO.End_Of_Line
  fileRef    : reference;
  theFileRec : storage;
  subprogramId : constant identifier := end_of_line_t;
begin
  kind := boolean_t;
  result:= storage'( to_unbounded_string( integer'image( 0 ) ), noMetaLabel, noMetaLabels );
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFile( subprogramId, fileRef );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     GetParameterValue( fileRef, theFileRec );
     if metaLabelOk( subprogramId, theFileRec ) then
        result := storage'( stringField( fileRef, eol_field ), noMetaLabel, theFileRec.policyMetaLabels );
     end if;
  end if;
end ParseEndOfLine;

procedure ParseLine( result : out storage; kind : out identifier ) is
  -- Syntax: line( open-file )
  -- Source: Ada.Text_IO.Line
  file_ref : reference;
  theFileRec : storage;
  subprogramId : constant identifier := line_t;
begin
  kind := integer_t;   -- TODO: probably should be something more specific
  result := storage'( to_unbounded_string( integer'image( 0 ) ), noMetaLabel, noMetaLabels );
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFile( line_t, file_ref );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     GetParameterValue( file_ref, theFileRec );
     if metaLabelOk( subprogramId, theFileRec ) then
        result := storage'( stringField( file_ref, line_field ),
           noMetaLabel, theFileRec.policyMetaLabels );
     end if;
  end if;
end ParseLine;

procedure ParseName( result : out storage; kind : out identifier ) is
  -- Syntax: name( open-file )
  -- Source: Ada.Text_IO.Name
  file_ref : reference;
  theFileRec : storage;
  subprogramId : constant identifier := name_t;
begin
  kind := uni_string_t;
  result := nullStorage;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFile( subprogramId, file_ref );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     GetParameterValue( file_ref, theFileRec );
     if metaLabelOk( subprogramId, theFileRec ) then
        result := storage'( stringField( file_ref, name_field ),
           noMetaLabel, theFileRec.policyMetaLabels );
     end if;
  end if;
end ParseName;

procedure ParseMode( result : out storage; kind : out identifier ) is
  -- Syntax: mode( open-file )
  -- Source: Ada.Text_IO.Mode
  file_ref : reference;
  theFileRec : storage;
  subprogramId : constant identifier := mode_t;
begin
  kind := file_mode_t;
  result := nullStorage;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFile( subprogramId, file_ref );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     GetParameterValue( file_ref, theFileRec );
     if metaLabelOk( subprogramId, theFileRec) then
        begin
          result.value := stringField( file_ref, mode_field );
          result.unitMetaLabel := noMetaLabel;
          result.policyMetaLabels := theFileRec.policyMetaLabels;
          if identifier'value( to_string( result.value ) ) = in_file_t then
             result.value := identifiers( in_file_t ).store.value;
          elsif identifier'value( to_string( result.value ) ) = out_file_t then
             result.value := identifiers( out_file_t ).store.value;
          elsif identifier'value( to_string( result.value ) ) = append_file_t then
             result.value := identifiers( append_file_t ).store.value;
          else
             err( pl( Gnat.Source_Info.Source_Location & ": internal error: unable to determine file mode" ) );
          end if;
        exception when msg : others =>
           -- 'value can raise a constraint_error if there SparForte corrupts
           -- if the value at an earlier step.  This is unlikely so the catch-all
           -- handler sufficient to handle it and there is no constraint_error
           -- handler.
          err( contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while looking up the file mode" ),
             subject => subprogramId,
             reason => +"had an internal error because",
             obstructorNotes => pl( "an unexpected exception " ) &
                em_value( to_unbounded_string( Exception_Message( msg ) ) )

          );
       end;
     end if;
  end if;
end ParseMode;

procedure ParseInkey( result : out storage; kind : out identifier ) is
  -- Syntax: inkey
  -- Source: Ada.Text_IO.Inkey
  ch : character;
  subprogramId : constant identifier := inkey_t;
begin
  kind := character_t;
  expect( subprogramId );
  if isExecutingCommand then
     if metaLabelOk( subprogramId, identifiers( current_input_t ).store.all ) then
        getKey( ch );
        result := storage'( to_unbounded_string( ch & "" ), noMetaLabel, sparMetaLabels );
     end if;
  end if;
end ParseInkey;

procedure ParseGetLine( result : out storage; kind : out identifier ) is
  -- Syntax: get_line [ (open-file) ]
  -- Source: Ada.Text_IO.Get_Line
  -- Note: Gnat get_line can't be used here because it does something
  -- odd with file descriptor 0
  file_ref : reference;
  file_kind : identifier;
  ch   : character;
  fileInfo : storage;
  subprogramId : constant identifier := get_line_t;
begin
  kind := universal_t;
  file_ref.id := eof_t;
  result := nullstorage;
  expect( subprogramId );
  if token = symbol_t and then identifiers( Token ).store.value = "(" then
      expect( symbol_t, "(" );
      ParseOpenFileOrSocket( subprogramId, file_ref, file_kind );
      expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if file_ref.id /= eof_t then
        begin
           GetParameterValue( file_ref, fileInfo );
           if metaLabelOk( subprogramId, fileInfo ) then
              if trace then
                 put_trace( "Input from file descriptor" & to_string(
                    stringField( file_ref, fd_field ) ) );
              end if;
              if stringField(file_ref, eof_field ) = "1" then
                 err( +"end of file" );
                 return;
              end if;
              if file_kind = socket_type_t and then stringField( file_ref, doget_field ) = "1" then
                 DoGet( file_ref );
                 replaceField( file_ref, doget_field, boolean'image( false ) );
              end if;
              loop
                 GetParameterValue( file_ref, fileInfo );
                 ch := Element( fileInfo.value, 1 );
                 if stringField(file_ref, eof_field ) = "1" then
                    exit;
                 end if;
                 DoGet( file_ref );
                 exit when ch = ASCII.LF or error_found or wasSIGINT or wasSIGTERM;
                 result.value := result.value & ch;
              end loop;
              result.unitMetaLabel := noMetaLabel;
              result.policyMetaLabels := identifiers( file_ref.id ).store.policyMetaLabels;
           end if;
        exception when msg : others =>
          err( contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while looking up the file mode" ),
             subject => subprogramId,
             reason => +"had an internal error because",
             obstructorNotes => pl( "an unexpected exception " ) &
                em_value( to_unbounded_string( Exception_Message( msg ) ) )

          );
       end;
     else
        if metaLabelOk( subprogramId, identifiers( current_input_t ).store.all ) then
           pegasoft.user_io.getline.getLine( result.value );
           result.unitMetaLabel := noMetaLabel;
           result.policyMetaLabels := identifiers( current_input_t ).store.policyMetaLabels;
           if wasSIGINT or wasSIGTERM then
              new_line;  -- user didn't press enter
              -- wasSIGINT will be cleared later
           end if;
        end if;
     end if;
  end if;
end ParseGetLine;


-- TODO: "DO" procedures should be moved to parser_aux.



procedure ParseOpen( create : boolean := false ) is
  -- Syntax: open( closed-file, mode, name );
  -- Syntax: create( closed-file [,mode] [,name] );
  -- Source: Ada.Text_IO.Open
  -- Source: Ada.Text_IO.Create
  file_ref : reference;
  mode : identifier;
  fileName : storage;
  -- name : unbounded_string;
  -- fileMetaLabel : metaLabelID;
  kind : identifier;
  expr     : storage;
  exprType : identifier;
  subprogramId :  constant identifier := open_t;
begin
  if create then
     if rshOpt then
        err( +"create not allowed in a " & em( "restricted shell" ) );
     end if;
     expect( create_t );
     expect( symbol_t, "(" );
     ParseClosedFile( file_ref );
     kind := file_type_t;
     -- the mode is optional, default to out_file
     if token = symbol_t and identifiers( token ).store.value = ")" then
        mode := out_file_t;
     else
        expectParameterComma;
        ParseIdentifier( mode );
        if baseTypesOk( identifiers( mode ).kind, file_mode_t ) then
           if create and mode = in_file_t then
              err( +"cannot create an in_file" );
           end if;
        end if;
     end if;
     -- the name is optional, default to a temp file name
     if token = symbol_t and identifiers( token ).store.value = ")" then
        makeTempFile( fileName.value );
        fileName.unitMetaLabel := noMetaLabel;
        fileName.policyMetaLabels := sparMetaLabels;
     else
        expectParameterComma;
        ParseExpression( expr, exprType );
        if uniTypesOk( exprType, uni_string_t ) then
           fileName := expr;
           -- name := expr.value;
           --fileMetaLabel := expr.metaLabel;
           if length( fileName.value ) = 0 and then not syntax_check then
              err( +"pathname should not be null" );
           end if;
        end if;
     end if;
     expect( symbol_t, ")" );
  else
     expect( open_t );
     expect( symbol_t, "(" );
     ParseClosedFileOrSocket( file_ref, kind );
     if kind = file_type_t then
        expectParameterComma;
        ParseIdentifier( mode );
        if baseTypesOk( identifiers( mode ).kind, file_mode_t ) then
           if boolean(rshOpt) and mode = out_file_t then
              err( +"out_file mode not allowed in a " & em( "restricted shell" ) );
           end if;
        end if;
     end if;
     if not error_found then
        -- not error_found because file must be legit in here
        expectParameterComma;
        -- At this point, it may be the second or third parameter, depending on
        -- whether or not it is a socket.  The third parameter is an expression
        -- and the expression could contain keywords like $1.
        if kind = socket_type_t then
           -- now host + optional port in name.  host may contain keywords like $1
           if identifiers( token ).kind /= keyword_t then
              if getBaseType( identifiers( token ).kind ) = file_mode_t then
                 err( +"sockets don't have a mode" );
               end if;
           end if;
           ParseExpression( expr, exprType );
           if uniTypesOk( exprType, uni_string_t ) then
              fileName := expr;
              --name := expr.value;
              --fileMetaLabel := expr.metaLabel;
              if length( fileName.value ) = 0 and then not syntax_check then
                 err( +"hostname should not be null" );
              end if;
              expect( symbol_t, ")" );
           end if;
        else
           ParseExpression( expr, exprType );
           if uniTypesOk( exprType, uni_string_t ) then
              fileName := expr;
              --name := expr.value;
              --fileMetaLabel := expr.metaLabel;
              if length( fileName.value ) = 0 and then not syntax_check then
                 err( +"pathname should not be null" );
              end if;
              expect( symbol_t, ")" );
           end if;
        end if;
     end if; -- if mode and file OK
  end if; -- is open
  -- do it
  if isExecutingCommand then -- should use umask for permissions
     if metaLabelOk( subprogramId, fileName ) then
        begin
           if kind = file_type_t then
              DoFileOpen( file_ref, mode, create, to_string( fileName.value ), fileName.policyMetaLabels );
           else
              DoSocketOpen( file_ref, fileName.value, fileName.policyMetaLabels );
           end if;
        exception when msg : others =>
           err( contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while opening the file" ),
                subject => subprogramId,
                reason => +"had an internal error because",
                obstructorNotes => pl( "an unexpected exception " ) &
                   em_value( to_unbounded_string( Exception_Message( msg ) ) )

           );
        end;
     end if;
  end if;
end ParseOpen;

procedure ParseReset is
  -- Syntax: reset( open-file [,mode] )
  -- Source: Ada.Text_IO.Reset
  file_ref: reference;
  mode    : identifier := eof_t;
  name    : unbounded_string;
  fileMetaLabels : metaLabelHashedSet.Set;
  modestr : unbounded_string;
  fd      : aFileDescriptor;
  closeResult : int;
  theFileRec : storage;
  subprogramId :  constant identifier := reset_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFile( subprogramId, file_ref );
  if token = symbol_t and identifiers( token ).store.value = "," then
     getNextToken;
     if baseTypesOk( identifiers( token ).kind, file_mode_t ) then
        mode := token;
        getNextToken;
        if boolean(rshOpt) and mode = out_file_t then
           err( +"out_file mode not allowed in a " & em( "restricted shell" ) );
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     GetParameterValue( file_ref, theFileRec );
     if metaLabelOk( subprogramId, theFileRec ) then
        begin
           fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
           name := stringField( file_ref, name_field );
           fileMetaLabels := theFileRec.policyMetaLabels;
           if mode = eof_t then
              modestr := stringField( file_ref, mode_field );
              if to_string( modestr ) = in_file_t'img then
                 mode := in_file_t;
              elsif to_string( modestr ) = out_file_t'img then
                 mode := out_file_t;
              elsif to_string( modestr ) = append_file_t'img then
                 mode := append_file_t;
              else
                 err( pl( Gnat.Source_Info.Source_Location & ": internal error: unable to determine file mode " &
                      to_string( modestr ) ) );
              end if;
           end if;
<<retry>> closeResult := close( fd );
           if closeResult < 0 then
              if C_errno = EINTR then
                 goto retry;
              end if;
           end if;
           DoFileOpen( file_ref, mode, false, to_string( name ), fileMetaLabels );

        exception when msg : others =>
           err( contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while resetting the file" ),
                subject => subprogramId,
                reason => +"had an internal error because",
                obstructorNotes => pl( "an unexpected exception " ) &
                   em_value( to_unbounded_string( Exception_Message( msg ) ) )
        );
     end;
   end if;
  end if;
end ParseReset;

procedure ParseClose is
  -- Syntax: close( open-file )
  -- Source: Ada.Text_IO.Close
  file_ref : reference;
  fd   : aFileDescriptor;
  kind : identifier;
  theFileRec : storage;
  closeResult : int;
  subprogramId :  constant identifier := close_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFileOrSocket( subprogramId, file_ref, kind );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     getParameterValue( file_ref, theFileRec );
     if metaLabelOk( subprogramId, theFileRec ) then
        begin
           fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
           if fd = currentStandardInput then
              err( +"this file is the current input file" );
           elsif fd = currentStandardInput then
              err( +"this file is the current output file" );
           elsif fd = currentStandardInput then
              err( +"this file is the current error file" );
           else
<<retry>>
              closeResult := close( fd );
              if closeResult < 0 then
                 if C_errno = EINTR then
                    goto retry;
                 end if;
              end if;
              if trace then
                 put_trace( "Closed file descriptor" & to_string( stringField( file_ref, fd_field ) ) );
              end if;
              identifiers( file_ref.id ).store.value := null_unbounded_string;
           end if;
        exception when msg : others =>
           err( contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while closing the file" ),
                subject => subprogramId,
                reason => +"had an internal error because",
                obstructorNotes => pl( "an unexpected exception " ) &
                   em_value( to_unbounded_string( Exception_Message( msg ) ) )

           );
        end;
     end if;
  end if;
end ParseClose;

procedure ParseDelete is
  -- Syntax: delete( open-file )
  -- Source: Ada.Text_IO.Delete
  file_ref : reference;
  name : unbounded_string;
  fd   : aFileDescriptor;
  result : integer;
  closeResult : int;
  theFileRec : storage;
  subprogramId :  constant identifier := delete_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFile( subprogramId, file_ref );
  expect( symbol_t, ")" );
  if rshOpt then
     err( +"delete is not allowed in a " & em( "restricted shell" ) );
  end if;
  if isExecutingCommand then
     GetParameterValue( file_ref, theFileRec );
     if metaLabelOk( subprogramId, theFileRec ) then
        begin
          fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
          if fd = currentStandardInput then
             err( +"this file is the current input file" );
          elsif fd = currentStandardInput then
             err( +"this file is the current output file" );
          elsif fd = currentStandardInput then
             err( +"this file is the current error file" );
          else
             name := stringField( file_ref, name_field );
<<retry>> closeResult := close( fd );
             if closeResult < 0 then
                if C_errno = EINTR then
                   goto retry;
                end if;
             end if;
             identifiers( file_ref.id ).store.value := null_unbounded_string;
             result := integer( unlink( to_string( name ) & ASCII.NUL ) );
             if result /= 0 then
                err( pl( "unable to delete file: " & OSerror( C_errno ) ) );
             end if;
             if trace then
                put_trace( "delete file " & to_string( name ) );
             end if;
          end if;
        exception when msg : others =>
           err( contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while deleting the file" ),
                subject => subprogramId,
                reason => +"had an internal error because",
                obstructorNotes => pl( "an unexpected exception " ) &
                   em_value( to_unbounded_string( Exception_Message( msg ) ) )

           );
        end;
     end if;
  end if;
end ParseDelete;

procedure ParseSkipLine is
  -- Syntax: skip_line [ (open-file) ]
  -- Source: Ada.Text_IO.Skip_Line
  file_ref : reference;
  ch     : character;
  result : size_t;
  kind   : identifier;
  str    : unbounded_string;
  theFileRec : storage;
  subprogramId :  constant identifier := skip_line_t;
begin
  file_ref.id := eof_t;
  expect( subprogramId );
  if token = symbol_t and then identifiers( Token ).store.value = "(" then
      getNextToken;
      ParseOpenFileOrSocket( subprogramId, file_ref, kind );
      expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
        if trace then
           put_trace( "Input from file descriptor" & to_string( stringField( file_ref, fd_field ) ) );
        end if;
        if file_ref.id /= eof_t then
           getParameterValue( file_ref, theFileRec );
           if metaLabelOk( subprogramId, theFileRec ) then
              if kind = socket_type_t and then stringField( file_ref, doget_field ) = "1" then
                 DoGet( file_ref );
                 replaceField( file_ref, doget_field, "0" );
              end if;
              loop
                 ch := Element( identifiers( file_ref.id ).store.value, 1 );
                 if stringField(file_ref, eof_field ) = "1" then
                    err( +"end of file" );
                    exit;
                 end if;
                 DoGet( file_ref );
                 exit when ch = ASCII.LF or error_found;
                 str := str & ch;
              end loop;
           end if;
        else
           -- stdin (I don't like this)
           if metaLabelOk( subprogramId, identifiers( standard_input_t ).store.all ) then
              loop
                 readchar( result, stdin, ch, 1 );
 -- KB: 2012/02/15: see spar_os-tty for an explaination of this kludge
                 if result < 0 or result = size_t'last then
                    if C_errno /= EAGAIN and C_errno /= EINTR then
                       err( pl( "unable to read file:" & OSerror( C_errno ) ) );
                       exit;
                    end if;
                 elsif result = 0 then
                    err( +"skipped past the end of the file" );
                    exit;
                 else
                    if ch = ASCII.LF then
                       exit;
                    end if;
                 end if;
                 str := str & ch;
              end loop;
           end if;
           if trace then
              Put_Trace( "Skipped '" & to_string( str ) & "'" );
           end if;
        end if;
     exception when msg : others =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
              " while skipping lines" ),
             subject => subprogramId,
             reason => +"had an internal error because",
             obstructorNotes => pl( "an unexpected exception " ) &
                em_value( to_unbounded_string( Exception_Message( msg ) ) )

        );
     end;
  end if;
end ParseSkipLine;

procedure ParseGet is
  -- Syntax: get [ (open-file), ch ]
  -- Source: Ada.Text_IO.Get
  -- Note: Gnat get can't be used here because it does something
  -- odd with file descriptor 0
  file_ref  : reference;
  kind      : identifier;
  fd        : aFileDescriptor;
  ch        : character;
  id_ref    : reference;
  result    : size_t;
  fileInfo  : storage;
  theFileRec: storage;
  subprogramId : constant identifier := get_t;
begin
  file_ref.id := eof_t;
  expect( subprogramId );
  fd := stdin;
  expect( symbol_t, "(" );
  if identifiers( token ).kind /= keyword_t then
     ParseOpenFileOrSocket( subprogramId, file_ref, kind );
     expectParameterComma;
  else
     file_ref.id := standard_input_t;
  end if;
  ParseOutParameter( id_ref, character_t );
  if baseTypesOk( id_ref.kind, character_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
        if trace then
           put_trace( "Using file descriptor " & to_string( stringField( file_ref, fd_field ) ) );
        end if;
        if file_ref.id /= eof_t then
           getParameterValue( file_ref, theFileRec );
           if metaLabelOk( subprogramId, theFileRec ) then
              GetParameterValue( file_ref, fileInfo );
              if kind = socket_type_t and then stringField( fileInfo.value, recSep, doget_field ) = "1" then
                 -- First get must update the 1 char look-ahead in the file_info
                 DoGet( file_ref );
                 GetParameterValue( file_ref, fileInfo );
                 replaceField( fileInfo.value, recSep, doget_field, boolean'image(false));
              end if;
              if stringField( fileInfo.value, recSep, eof_field ) = "1" then
                 err( +"end of file" );
              else
                 ch := Element( fileInfo.value, 1 );
                 AssignParameter( id_ref,
                    storage'( to_unbounded_string( "" & ch ),
                    unitMetaLabel => noMetaLabel,
                    policyMetaLabels => identifiers( file_ref.id ).store.policyMetaLabels )
                 );
                 AssignParameter( file_ref, fileInfo );
                 DoGet( file_ref );
              end if;
           end if;
        else
           if metaLabelOk( subprogramId, identifiers( standard_input_t ).store.all ) then
              fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
   <<reread>>    readchar( result, fd, ch, 1 );
 -- KB: 2012/02/15: see spar_os-tty for an explaination of this kludge
              if result < 0 or result = size_t'last then
                 if C_errno = EAGAIN  or C_errno = EINTR then
                    goto reread;
                 end if;
                 err( pl( "unable to read file:" & OSerror( C_errno ) ) );
              elsif result = 0 then
                 err( +"end of file" );
              else
                 AssignParameter( id_ref,
                    storage'(to_unbounded_string( "" & ch ),
                    unitMetaLabel => noMetaLabel,
                    policyMetaLabels => identifiers( standard_input_t ).store.policyMetaLabels )
                 );
              end if;
              if ch = ASCII.LF then -- not stdin (or error)?
                 replaceField( file_ref, line_field,
                    long_integer'image( long_integer'value(
                    to_string( stringField( file_ref, line_field ) ) ) + 1 ) );
                 replaceField( file_ref, eol_field, "1" );
             else
                 replaceField( file_ref, eol_field, "0" );
             end if;
           end if;
        end if;
     exception when msg : others =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
              " while reading" ),
             subject => subprogramId,
             reason => +"had an internal error because",
             obstructorNotes => pl( "an unexpected exception " ) &
                em_value( to_unbounded_string( Exception_Message( msg ) ) )

        );
     end;
  end if;
end ParseGet;

procedure ParsePutLine is
  -- Syntax: put_line( [file,] expression )
  -- Source: Ada.Text_IO.Put_Line
  target_ref: reference;
  kind      : identifier := file_type_t;
  expr      : storage;
  expr_type : identifier;
  temp      : unbounded_string;
  result    : size_t;
  ch        : character;
  fd        : aFileDescriptor;
  retry     : boolean;
  theFileRec: storage;
  subprogramId : constant identifier := put_line_t;
begin
  target_ref.index := 0;
  expect( subprogramId );
  expect( symbol_t, "(" );
  if identifiers( token ).kind /= keyword_t then
     kind := getUniType( token );
     if kind = file_type_t then
        ParseOpenFile( subprogramId, target_ref );
        if isExecutingCommand then
           if to_string( stringField(target_ref, mode_field)) = in_file_t'img then
              err( +"This is a in_mode file" );
           end if;
        end if;
        expectParameterComma;
     elsif kind = socket_type_t then
        ParseOpenSocket( subprogramId, target_ref );
        expectParameterComma;
     else
        target_ref.id := standard_output_t;
     end if;
  else
     target_ref.id := standard_output_t;
  end if;
  ParseExpression( expr, expr_type );
  -- this sould be moved to an image function
  if getUniType( expr_type ) = root_enumerated_t then
     -- In newer versions of GCC Ada, expr_val cannot be used twice
     findEnumImage( expr.value, expr_type, temp );
     expr.value := temp;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
        if target_ref.id = standard_error_t then
           if metaLabelOk( subprogramId, identifiers( standard_error_t ).store.all, expr ) then
              -- Ada doesn't handle interrupted system calls properly.
              -- maybe a more elegant way to do this...
              loop
                retry := false;
                begin
                  Put_Line( standard_error, expr.value );
                exception when msg: device_error =>
                  if exception_message( msg ) = "interrupted system call" then
                     retry := true;
                  else
                     err( pl( exception_message( msg ) ) );
                  end if;
                end;
                exit when not retry;
              end loop;
           end if;
        elsif target_ref.id = standard_output_t then
           if metaLabelOk( subprogramId, identifiers( standard_output_t ).store.all, expr ) then
              -- Ada doesn't handle interrupted system calls properly.
              -- maybe a more elegant way to do this...
              loop
                retry := false;
                begin
                  Put_Line( expr.value );
                exception when msg: device_error =>
                  if exception_message( msg ) = "interrupted system call" then
                     retry := true;
                  else
                     err( pl( exception_message( msg ) ) );
                  end if;
                end;
                exit when not retry;
             end loop;
              last_output := expr;
              last_output_type := expr_type;
           end if;
        else
           getParameterValue( target_ref, theFileRec );
           if metaLabelOk( subprogramId, theFileRec, expr ) then
              fd := aFileDescriptor'value( to_string( stringField( target_ref, fd_field ) ) );
              for i in 1..length( expr.value ) loop
                  ch := Element( expr.value, i );
<<rewrite>>       writechar( result, fd, ch, 1 );
                  if result < 0 or result = size_t'last then
                     if C_errno = EAGAIN or C_errno = EINTR then
                        goto rewrite;
                    end if;
                    err( pl( "unable to write: " & OSerror( C_errno ) ) );
                    exit;
                  end if;
              end loop;
              ch := ASCII.LF;
<<rewrite2>>
              writechar( result, fd, ch, 1 ); -- add a line feed
              if result < 0 or result = size_t'last then
                 if C_errno = EAGAIN or C_errno = EINTR then
                    goto rewrite2;
                 end if;
                 err( pl( "unable to write: " & OSerror( C_errno ) ) );
              else
                replaceField( target_ref, line_field,
                   long_integer'image( long_integer'value(
                      to_string( stringField( target_ref, line_field ) ) ) + 1 ) );
              end if;
           end if;
        end if;
     exception when msg : others =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
              " while writing" ),
             subject => subprogramId,
             reason => +"had an internal error because",
             obstructorNotes => pl( "an unexpected exception " ) &
                em_value( to_unbounded_string( Exception_Message( msg ) ) )

        );
    end;
  end if;
end ParsePutLine;

procedure ParseQuestion is
  -- Syntax: "?" expression
  -- Source: SparForte built-in
  expr      : storage;
  expr_type : identifier;
  temp      : unbounded_string;
  uni_type  : identifier;
  retry     : boolean;
-- There is no subprogram id because question mark is a symbol token
begin
  expect( symbol_t );
  if onlyAda95 then
      err( em( "pragma ada_95" ) & pl( " doesn't allow ?" ) );
      return;
  end if;
  ParseExpression( expr, expr_type );
  -- If an error occurred when parsing the expression, the expression and
  -- type may not be valid.  There's nothing to print.
  if error_found then
     null;
  elsif getUniType( expr_type ) = root_enumerated_t then
     -- this will work during the syntax check but we don't need it
     if isExecutingCommand then
        -- In newer versions of GCC Ada, expr_val cannot be used twice
        findEnumImage( expr.value, expr_type, temp );
        expr.value := temp;
     end if;
  -- pretty formating for ? and time values
  elsif getBaseType( expr_type ) = cal_time_t then
     declare
        year    : year_number;
        month   : month_number;
        day     : day_number;
        seconds : day_duration;
        hours   : day_duration;
        minutes : day_duration;

        function get_2_digits( s : string ) return unbounded_string is
           tempStr : unbounded_string;
           dotPos  : natural;
        begin
           tempStr := to_unbounded_string( s );
           delete( tempStr, 1, 1 );
           dotPos := index( tempStr, "." );
           if dotPos > 0 then
              tempStr := head( tempStr, dotPos-1 );
           end if;
           if length( tempStr ) < 2 then
              tempStr := "0" & tempStr;
           end if;
           return tempStr;
        end get_2_digits;

        function get_4_digits( s : string ) return unbounded_string is
           tempStr : unbounded_string;
           dotPos  : natural;
        begin
           tempStr := to_unbounded_string( s );
           delete( tempStr, 1, 1 );
           dotPos := index( tempStr, "." );
           if dotPos > 0 then
              tempStr := head( tempStr, dotPos-1 );
           end if;
           if length( tempStr ) < 4 then
              tempStr := "0" & tempStr;
           end if;
           return tempStr;
        end get_4_digits;

        function drop_leading_space( s : string ) return unbounded_string is
           tempStr : unbounded_string;
        begin
           tempStr := to_unbounded_string( s );
           delete( tempStr, 1, 1 );
           return tempStr;
        end drop_leading_space;

     begin
        if isExecutingCommand then
           Split( time( to_numeric( expr.value ) ), year, month, day, seconds );

           hours := duration( float'truncation( float( seconds / (60 * 60) ) ) );
           seconds := seconds - hours * (60* 60);
           minutes := duration( float'truncation( float( seconds / ( 60 ) ) ) );
           seconds := seconds - (minutes * 60);
           expr.value := get_4_digits( year'img ) &  "/";
           expr.value := expr.value & get_2_digits( month'img ) &  "/";
           expr.value := expr.value & get_2_digits( day'img ) & " ";
           expr.value := expr.value & get_2_digits( hours'img ) & ":";
           expr.value := expr.value & get_2_digits( minutes'img ) & ":";
           expr.value := expr.value & drop_leading_space( seconds'img );
        end if;
     end;
  end if;
  if isExecutingCommand then -- fix this for no output on error!
     begin
        -- this sould be moved to an image function
        uni_type := getUniType( expr_type );
        if uni_type = root_enumerated_t then
           -- In newer versions of GCC Ada, expr_val cannot be used twice.
           findEnumImage( expr.value, expr_type, temp );
           expr.value := temp;
        elsif uni_type = uni_numeric_t then
           -- For universal numeric, represent it as an integer string if possible
           -- to make it human-readable.
           declare
              val : numericValue; -- := to_numeric( expr_val );
           begin
              val := to_numeric( expr.value );
              -- Within the range of a SparForte integer and no decimal (e.g.
              -- casting results in the same value?
              if val >= numericValue( integerOutputType'first+0.9 ) and
                 val <= maxInteger then
                 if val = numericValue'floor( val ) then
                    expr.value := to_unbounded_string( val );
                 end if;
              end if;
           exception when ada.strings.index_error =>
              -- since this is an expression, this could be something else but
              -- it is almost always this
              err( +"numeric variable has no value" );
           when constraint_error =>
              err( pl( "constraint_error in question command - value " &
                to_string( toEscaped( expr.value ) ) & -- KB: 25/03/30 - should be protected
                " may not be numeric" ) );
           end;
        elsif uni_type = root_record_t then
           err( pl( "full records cannot be printed with ?.  Try env" ) );
        end if;
        -- If an just error occurred, don't print anything further.
        if metaLabelOk( "?", identifiers( standard_output_t ).store.all, expr ) then
           if not error_found then
              -- Ada doesn't handle interrupted system calls properly.
              -- maybe a more elegant way to do this...
              loop
                 retry := false;
                 begin
                   Put_Line( expr.value );
                 exception when msg: device_error =>
                   if exception_message( msg ) = "interrupted system call" then
                      retry := true;
                   else
                      err( pl( exception_message( msg ) ) );
                   end if;
                 end;
              exit when not retry;
              end loop;
           end if;
           last_output := expr;
           last_output_type := expr_type;
           replaceField( standard_output_t, line_field,
              long_integer'image( long_integer'value(
               to_string( stringField( standard_output_t, line_field ) ) ) + 1 ) );
        end if;
     exception when msg : others =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
              " while writing" ),
             subjectNotes => em( "?" ),
             reason => +"had an internal error because",
             obstructorNotes => pl( "an unexpected exception " ) &
                em_value( to_unbounded_string( Exception_Message( msg ) ) )

        );
    end;
  end if;
end ParseQuestion;

procedure ParsePut is
  -- Syntax: put( [open-file,] expression [, picture] )
  -- Source: Ada.Text_IO.Editing.Put
  target_ref: reference;
  kind      : identifier;
  expr      : storage;
  expr_type : identifier;
  result    : size_t;
  ch        : character;
  fd        : aFileDescriptor;
  pic       : Picture;
  picExpr   : storage;
  pic_type  : identifier;
  retry     : boolean;
  temp      : unbounded_string;
  theFileRec: storage;
  subprogramId : constant identifier := put_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  if identifiers( token ).kind /= keyword_t then
     kind := getUniType( token );
     if kind = file_type_t then
        ParseOpenFile( subprogramId, target_ref );
        if isExecutingCommand then
           if to_string( stringField(target_ref, mode_field)) = in_file_t'img then
              err( +"This is a in_mode file" );
           end if;
        end if;
        expectParameterComma;
     elsif kind = socket_type_t then
        ParseOpenSocket( subprogramId, target_ref );
        expectParameterComma;
     else
        target_ref.id := standard_output_t;
     end if;
  else
     target_ref.id := standard_output_t;
  end if;
  ParseExpression( expr, expr_type );
  if getUniType( expr_type ) = root_enumerated_t then
     -- newer versions of GCC Ada do not like two expr_val params
     findEnumImage( expr.value, expr_type, temp );
     expr.value := temp;
  end if;
  -- apply optional numeric formatting
  if token = symbol_t and identifiers( token ).store.value = "," then
     getNextToken;
     ParseExpression( picExpr, pic_type );
     if getUniType( pic_type ) /= uni_string_t then
        err( +"number format picture string expected" );
     elsif not valid( to_string( picExpr.value ) ) then
        err( +"number not a valid format picture" );
     elsif getUniType( expr_type ) /= uni_numeric_t then
        err( +"only numeric types can use a format picture" );
     else
        if isExecutingCommand then
           pic := to_picture( to_string( picExpr.value) );
           begin
              expr.value := to_unbounded_string( image( decimal_output_type( to_numeric( expr.value ) ), pic ) );
           exception when LAYOUT_ERROR =>
              err( pl( "incorrect image picture " & ASCII.Quotation & to_string( toEscaped( picExpr.value) ) & ASCII.Quotation ) );
           end;
        end if;
        last_output_type := uni_string_t;
     end if;
  else
     last_output_type := expr_type;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
        if target_ref.id = standard_error_t then
           if metaLabelOk( subprogramId, identifiers( standard_error_t ).store.all, expr ) then
              -- Ada doesn't handle interrupted system calls properly.
              -- maybe a more elegant way to do this...
              loop
                 retry := false;
                 begin
                   Put( standard_error, expr.value );
                 exception when msg: device_error =>
                   if exception_message( msg ) = "interrupted system call" then
                      retry := true;
                   else
                      err( pl( exception_message( msg ) ) );
                   end if;
                 end;
              exit when not retry;
              end loop;
           end if;
        elsif target_ref.id = standard_output_t then
           if metaLabelOk( subprogramId, identifiers( standard_output_t ).store.all, expr ) then
              -- Ada doesn't handle interrupted system calls properly.
              -- maybe a more elegant way to do this...
              loop
                 retry := false;
                 begin
                   Put( expr.value );
                 exception when msg: device_error =>
                   if exception_message( msg ) = "interrupted system call" then
                      retry := true;
                   else
                      err( pl( exception_message( msg ) ) );
                   end if;
                 end;
              exit when not retry;
              end loop;
              last_output := expr;
           end if;
        else
           getParameterValue( target_ref, theFileRec );
           if metaLabelOk( subprogramId, theFileRec, expr ) then
              fd := aFileDescriptor'value( to_string( stringField( target_ref, fd_field ) ) );
              for i in 1..length( expr.value ) loop
                  ch := Element( expr.value, i );
                  writechar( result, fd, ch, 1 );
<<rewrite>>       if result < 0 or result = size_t'last then
                     if C_errno = EAGAIN or C_errno = EINTR then
                        goto rewrite;
                     end if;
                     err( pl( "unable to write: " & OSerror( C_errno ) ) );
                     exit;
                  end if;
              end loop;
           end if;
        end if;
     exception when msg : others =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
              " while writing" ),
             subject => subprogramId,
             reason => +"had an internal error because",
             obstructorNotes => pl( "an unexpected exception " ) &
                em_value( to_unbounded_string( Exception_Message( msg ) ) )
        );
    end;
  end if;
end ParsePut;

procedure ParseNewLine is
  -- Syntax: new_line
  -- Source: Ada.Text_IO.New_Line
  target_ref : reference;
  kind   : identifier;
  fd     : aFileDescriptor;         -- Linux file descriptor of output file
  ch     : character;
  theFileRec : storage;
  result : size_t;
  subprogramId : constant identifier := new_line_t;
begin
  expect( subprogramId );
  if token = symbol_t and identifiers( token ).store.value = "(" then
     expect( symbol_t, "(" );
     ParseOpenFileOrSocket( subprogramId, target_ref, kind );
     expect( symbol_t, ")" );
  else
     target_ref.id := standard_output_t;
  end if;
  if isExecutingCommand then
     begin
        if target_ref.id = standard_error_t then
           if metaLabelOk( subprogramId, identifiers( standard_error_t ).store.all ) then
              New_Line( standard_error );
           end if;
        elsif target_ref.id = standard_output_t then
           if metaLabelOk( subprogramId, identifiers( standard_output_t ).store.all ) then
              New_Line;
           end if;
        else
           getParameterValue( target_ref, theFileRec );
           if metaLabelOk( subprogramId, theFileRec ) then
              fd := aFileDescriptor'value( to_string( stringField( target_ref, fd_field ) ) );
              ch := ASCII.LF;
<<rewrite>>   writechar( result, fd, ch, 1 );
              if result < 0 or result = size_t'last then
                 if C_errno = EAGAIN or C_errno = EINTR then
                    goto rewrite;
                 end if;
                 err( pl( "unable to write: " & OSerror( C_errno ) ) );
              end if;
           end if;
        end if;
     exception when msg : others =>
        err( contextNotes => pl( "At " & gnat.source_info.source_location &
              " while writing" ),
             subject => subprogramId,
             reason => +"had an internal error because",
             obstructorNotes => pl( "an unexpected exception " ) &
                em_value( to_unbounded_string( Exception_Message( msg ) ) )
        );
    end;
  end if;
end ParseNewLine;

procedure ParseSetInput is
  -- Syntax: set_input( open-file )
  -- Source: Ada.Text_IO.Set_Input
  inputFile_ref : reference;            -- open file to read input from
  fd            : aFileDescriptor;      -- Linux file descriptor of input file
  result        : aFileDescriptor := 0; -- result of dup2
  theInputFile  : storage;              -- Input file_type value
  subprogramId  : constant identifier := set_input_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFile( subprogramId, inputFile_ref );

  -- Check for standard files that cannot have input read from them
  -- A file provided for input must open in a read mode (in_mode)
  --
  -- Standard input, output and error are scalar file_type variables.  They
  -- are declared in this package but their initial values are assigned by
  -- the parser.  Their values are in the format as other file_type variables.
  --
  -- Current input, output and error are an alias for the appropriate file.
  -- They only contain the identifier of the file to use.

  if inputFile_ref.id = standard_output_t then
     err( em( "standard_output" ) & pl( " cannot be assigned for " ) &
          em( "input" ) );
  elsif inputFile_ref.id = standard_error_t then
     err( em( "standard_error" ) & pl( " cannot be assigned for " ) &
          em( "input" ) );
  elsif inputFile_ref.id = current_output_t then
     err( em( "current_output" ) & pl( " cannot be assigned for " ) &
          em( "input" ) );
  elsif inputFile_ref.id = current_error_t then
     err( em( "current_error" ) & pl( " cannot be assigned for " ) &
          em( "input" ) );
  elsif not syntax_check then
    if to_string(stringField(inputFile_ref, mode_field)) /= in_file_t'img then
       err( +"not an in_file file" );
    end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     getParameterValue( inputFile_ref, theInputFile );
     if metaLabelOk( subprogramId, theInputFile ) then
        -- setting it to itself is a no-op
        if inputFile_ref.id /= current_input_t then
           begin
              fd := aFileDescriptor'value( to_string( stringField( inputFile_ref, fd_field ) ) );
              result := dup2( fd, stdin );
              if result < 0 then
                 err( pl( "unable to set input: " & OSerror( C_errno ) ) );
              elsif not error_found then
                 currentStandardInput := fd;
                 assignRenamedFile( subprogramId, current_input_t, inputFile_ref );
                 if trace then
                    put_trace( "input is currently from file descriptor" &
                       currentStandardInput'img );
                 end if;
               end if;
           exception when msg : others =>
              err( contextNotes => pl( "At " & gnat.source_info.source_location &
                    " while writing" ),
                   subjectNotes => pl( qp( "current input" ) ),
                   reason => +"had an internal error because",
                   obstructorNotes => pl( "an unexpected exception " ) &
                   em_value( to_unbounded_string( Exception_Message( msg ) ) )
              );
           end;
        end if;
     end if;
  end if;
end ParseSetInput;

procedure ParseSetOutput is
  -- Syntax: set_output( open-file )
  -- Source: Ada.Text_IO.Set_Output
  outputFile_ref : reference;             -- open file to write output to
  fd             : aFileDescriptor;       -- Linux file descriptor of output file
  result         : aFileDescriptor := 0;  -- result of dup2
  theOutputFile  : storage;               -- Output file_type value
  subprogramId   : constant identifier := set_output_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFile( subprogramId, outputFile_ref );

  -- Check for standard files that cannot have output written to them
  -- A file provided for output must be open in a write mode
  --
  -- Standard input, output and error are scalar file_type variables.  They
  -- are declared in this package but their initial values are assigned by
  -- the parser.  They are the same format as other file_type variables.
  --
  -- Current input, output and error are an alias for the appropriate file.
  -- They only contain the identifier of the file to use.

  if outputFile_ref.id = standard_input_t then
     err( em( "standard_input" ) & pl( " cannot be assigned for " ) &
          em( "output" ) );
  elsif outputFile_ref.id = current_input_t then
     err( em( "current_input" ) & pl( " cannot be assigned for " ) &
          em( "output" ) );
  elsif not syntax_check then
     if to_string(stringField(outputFile_ref.id, mode_field)) = in_file_t'img then
        err( +"not an out_file or append_file file" );
     end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     getParameterValue( outputFile_ref, theOutputFile );
     if metaLabelOk( subprogramId, theOutputFile ) then
        -- setting it to itself is a no-op
        if outputFile_ref.id /= current_output_t then
           begin
              fd := aFileDescriptor'value( to_string( stringField( outputFile_ref, fd_field ) ) );
              result := dup2( fd, stdout );
              if result < 0 then
                 err( pl( "unable to set output: " & OSerror( C_errno ) ) );
              elsif not error_found then
                 currentStandardOutput := fd;
                 assignRenamedFile( subprogramId, current_output_t, outputFile_ref );
                 if trace then
                    put_trace( "output is currently to file descriptor" &
                       currentStandardOutput'img );
                 end if;
              end if;
           exception when msg : others =>
              err( contextNotes => pl( "At " & gnat.source_info.source_location &
                    " while redirecting" ),
                   subjectNotes => pl( qp( "current output" ) ),
                   reason => +"had an internal error because",
                   obstructorNotes => pl( "an unexpected exception " ) &
                   em_value( to_unbounded_string( Exception_Message( msg ) ) )
              );
           end;
        end if;
     end if;
  end if;
end ParseSetOutput;

procedure ParseSetError is
  -- Syntax: set_error( open-file )
  -- Source: Ada.Text_IO.Set_Error
  errorOutputFile_ref : reference;            -- open file to write error output to
  result              : aFileDescriptor := 0; -- Linux file descriptor of output file
  fd                  : aFileDescriptor;      -- result of dup2
  theErrorOutputFile  : storage;              -- Error output file_type value
  subprogramId : constant identifier := set_error_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseOpenFile( subprogramId, errorOutputFile_ref );

  -- Check for standard files that cannot have error output written to them
  -- A file provided for error output must be open in a write mode
  --
  -- Standard input, output and error are scalar file_type variables.  They
  -- are declared in this package but their initial values are assigned by
  -- the parser.  They are the same format as other file_type variables.
  --
  -- Current input, output and error are an alias for the appropriate file.
  -- They only contain the identifier of the file to use.

  if errorOutputFile_ref.id = standard_input_t then
     err( em( "standard_input" ) & pl( " cannot be assigned for " ) &
          em( "standard error" ) );
  elsif errorOutputFile_ref.id = current_input_t then
     err( em( "current_input" ) & pl( " cannot be assigned for " ) &
          em( "standard error" ) );
  elsif not syntax_check then
     if to_string(stringField(errorOutputFile_ref, mode_field)) = in_file_t'img then
        err( pl( "not an out_file or append_file file" ) );
     end if;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     getParameterValue( errorOutputFile_ref, theErrorOutputFile );
     if metaLabelOk( subprogramId, theErrorOutputFile ) then
        -- setting it to itself is a no-op
        if errorOutputFile_ref.id /= current_error_t then
           begin
              fd := aFileDescriptor'value( to_string( stringField( errorOutputFile_ref, fd_field ) ) );
              result := dup2( fd, stderr );
              if result < 0 then
                 err( pl( "unable to set error: " & OSerror( C_errno ) ) );
              elsif not error_found then
                 currentStandardError := fd;
                 assignRenamedFile( subprogramId, current_error_t, errorOutputFile_ref );
                 if trace then
                    put_trace( "error output is currently to file descriptor" &
                      currentStandardError'img );
                 end if;
              end if;
           exception when msg : others =>
              err( contextNotes => pl( "At " & gnat.source_info.source_location &
                    " while redirecting" ),
                    subjectNotes => pl( qp( "current error output" ) ),
                   reason => +"had an internal error because",
                   obstructorNotes => pl( "an unexpected exception " ) &
                   em_value( to_unbounded_string( Exception_Message( msg ) ) )
              );
           end;
        end if;
     end if;
  end if;
end ParseSetError;

procedure ParseGetImmediate is
  -- Syntax: get_immediate( [file,] ch [, bool] )
  -- Source: Ada.Text_IO.Get_Immediate
  file_ref  : reference;
  kind      : identifier;
  fd        : aFileDescriptor;
  ch        : character;
  id_ref    : reference;
  avail_ref : reference;
  hasAvail  : boolean := false;
  baseType  : identifier;
  theInputFile : storage;
  subprogramId : constant identifier := get_immediate_t;
begin
  file_ref.id := eof_t;
  expect( subprogramId );
  fd := stdin;
  expect( symbol_t, "(" );
  kind := identifiers( token ).kind;
  if kind /= new_t then
     baseType := getBaseType( kind );
     if baseType = file_type_t or baseType = socket_type_t then
        ParseOpenFileOrSocket( subprogramId, file_ref, kind );
        expectParameterComma;
     else
        -- default is standard input
        file_ref.id := standard_input_t;
     end if;
  else
     -- default is standard input
     file_ref.id := standard_input_t;
  end if;
  ParseOutParameter( id_ref, character_t );
  if token = symbol_t and identifiers( token ).store.value = "," then
     getNextToken;
     ParseOutParameter( avail_ref, boolean_t );
     hasAvail := true;
  end if;
  expect( symbol_t, ")" );

  if isExecutingCommand then
     getParameterValue( file_ref, theInputFile );
     if metaLabelOk( subprogramId, theInputFile ) then
        begin
          -- TODO: integration with text_io functions.  Currently, it's ignoring
          -- TextIO's double-buffer queue.
          if file_ref.id /= eof_t then
             if isatty( fd ) /= 0 then
                if hasAvail then
                   -- TODO: this can echo the keypress because echoing is only
                   -- turned off when it tries to get a key.  So if a user presses
                   -- a key before getKey runs, it will echo because getKey hasn't
                   -- run yet.
                   getKey( ch, nonblock => true );
                   -- when non-blocking, Control-D indicates nothing read
                   AssignParameter( avail_ref,
                      storage'(to_spar_boolean( ch = ASCII.EOT ),
                      unitMetaLabel => noMetaLabel,
                      policyMetaLabels => theInputFile.policyMetaLabels )
                   );
                 else
                   getKey( ch );
                 end if;
                 AssignParameter( id_ref,
                    storage'(to_unbounded_string( "" & ch ),
                    unitMetaLabel => noMetaLabel,
                    policyMetaLabels => theInputFile.policyMetaLabels )
                 );
              else
                 err( +"only implemented for a tty/terminal" );
              end if;
           else
              -- TODO: this is only doing the terminal/tty, not files/sockets.
              -- This will probably require a C select() to avoid blocking.
              err( +"get_immediate for files are not yet implemented" );
           end if;
        exception when msg : others =>
           err( contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while redirecting" ),
                 subjectNotes => pl( qp( "current error output" ) ),
                reason => +"had an internal error because",
                obstructorNotes => pl( "an unexpected exception " ) &
                   em_value( to_unbounded_string( Exception_Message( msg ) ) )
           );
        end;
     end if;
  end if;
end ParseGetImmediate;

-- TODO: this is incomplete
--procedure ParseLookAhead is
  -- Syntax: look_ahead( [file,] ch [, eol] )
  -- Source: Ada.Text_IO.Look_Ahead
  -- This is the equivalent of a Get that does not get the next character.
--   procedure Look_Ahead
--     (Item        : out Character;
--      End_Of_Line : out Boolean);
--  file_ref  : reference;
--  kind      : identifier;
--  fd        : aFileDescriptor;
--  ch        : character;
--  id_ref    : reference;
--  result    : size_t;
--  fileInfo  : unbounded_string;
--begin
--  file_ref.id := eof_t;
--  expect( look_ahead_t );
--  fd := stdin;
--  expect( symbol_t, "(" );
--  if identifiers( token ).kind /= keyword_t then
--     ParseOpenFileOrSocket( file_ref, kind );
--     expectParameterComma;
--  else
--     file_ref.id := standard_input_t;
--  end if;
--  ParseOutParameter( id_ref, character_t );
--  if baseTypesOk( id_ref.kind, character_t ) then
--     expect( symbol_t, ")" );
--  end if;
--  -- TODO: second parameter
--  if isExecutingCommand then
--     if trace then
--        put_trace( "Using file descriptor " & to_string( stringField( file_ref, fd_field ) ) );
--     end if;
--     -- reading from a file?
--     if file_ref.id /= eof_t then
--        GetParameterValue( file_ref, fileInfo );
--        if kind = socket_type_t and then stringField( fileInfo, recSep, doget_field ) = "1" then
--           -- First get must update the 1 char look-ahead in the file_info
--           DoGet( file_ref );
--           GetParameterValue( file_ref, fileInfo );
--           replaceField( fileInfo, recSep, doget_field, boolean'image(false));
--        end if;
--        if stringField( fileInfo, recSep, eof_field ) = "1" then
--           err( "end of file" );
--        else
--           ch := Element( fileInfo, 1 );
--           AssignParameter( id_ref, to_unbounded_string( "" & ch ) );
--           AssignParameter( file_ref, fileInfo );
--           -- DoGet( file_ref );
--        end if;
--     else
--        -- reading from default input
--        -- TODO: why no buffering here?
--        fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
--   <<reread>> readchar( result, fd, ch, 1 );
-- -- KB: 2012/02/15: see spar_os-tty for an explaination of this kludge
--         if result < 0 or result = size_t'last then
--              if C_errno = EAGAIN  or C_errno = EINTR then
--                 goto reread;
--              end if;
--              err( "unable to read file:" & OSerror( C_errno ) );
--         elsif result = 0 then
--            err( "end of file" );
--         else
--            AssignParameter( id_ref, to_unbounded_string( "" & ch ) );
--         end if;
--      end if;
--      if ch = ASCII.LF then -- not stdin (or error)?
--         replaceField( file_ref, line_field,
--            long_integer'image( long_integer'value(
--            to_string( stringField( file_ref, line_field ) ) ) + 1 ) );
--         replaceField( file_ref, eol_field, "1" );
--      else
--         replaceField( file_ref, eol_field, "0" );
--      end if;
--  end if;
--end ParseLookAhead;

procedure StartupTextIO is
begin
  -- Predefined Text_IO Types

  declareStandardConstant( in_file_t, "in_file", file_mode_t, "0" );
  declareStandardConstant( out_file_t, "out_file", file_mode_t, "1" );
  declareStandardConstant( append_file_t, "append_file", file_mode_t, "2" );

  -- Text I/O Identifiers

  declareProcedure( create_t, "create" );
  declareProcedure( open_t, "open" );
  declareProcedure( close_t, "close", ParseClose'access );
  declareProcedure( get_t, "get", ParseGet'access );
  declareFunction( get_line_t, "get_line", ParseGetLine'access );
  declareFunction( inkey_t, "inkey", ParseInkey'access );
  declareProcedure( put_t, "put", ParsePut'access );
  declareProcedure( put_line_t, "put_line", ParsePutLine'access );
  declareProcedure( new_line_t, "new_line", ParseNewLine'access );
  declareFunction( is_open_t, "is_open" );
  declareFunction( end_of_file_t, "end_of_file", ParseEndOfFile'access );
  declareFunction( end_of_line_t, "end_of_line", ParseEndOfLine'access );
  declareFunction( name_t, "name", ParseName'access );
  declareFunction( mode_t, "mode", ParseMode'access );
  declareProcedure( skip_line_t, "skip_line", ParseSkipLine'access );
  declareFunction( line_t, "line", ParseLine'access );
  declareProcedure( set_input_t, "set_input", ParseSetInput'access );
  declareProcedure( set_output_t, "set_output", ParseSetOutput'access );
  declareProcedure( set_error_t, "set_error", ParseSetError'access );
  declareProcedure( reset_t, "reset", ParseReset'access );
  declareProcedure( get_immediate_t, "get_immediate", ParseGetImmediate'access );
  -- TODO: Look ahead doesn't work yet
  -- declareProcedure( look_ahead_t, "look_ahead", ParseLookAhead'access );

  -- declareProcedure( delete_t, "delete" );
  -- delete declared in scanner since it's also a SQL command

  -- Other Text I/O Constants

  declareStandardConstant( standard_input_t, "standard_input", file_type_t,
    "<defined by parser>" );
  declareStandardConstant( standard_output_t, "standard_output", file_type_t,
    "<defined by parser>" );
  declareStandardConstant( standard_error_t, "standard_error", file_type_t,
    "<defined by parser>" );
  -- these should not be constants, but we don't have a function type yet
  declareStandardConstant( current_input_t, "current_input", file_type_t,
    "<defined by parser>" );
  declareStandardConstant( current_output_t, "current_output", file_type_t,
    "<defined by parser>" );
  declareStandardConstant( current_error_t, "current_error", file_type_t,
    "<defined by parser>" );
end StartupTextIO;

procedure ShutdownTextIO is
begin
  null;
end ShutdownTextIO;

end parser_tio;

