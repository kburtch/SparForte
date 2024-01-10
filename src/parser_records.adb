------------------------------------------------------------------------------
-- Records Package Parser                                                   --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2023 Free Software Foundation              --
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

--with ada.text_io;use ada.text_io;

with gnat.source_info,
     ada.strings.unbounded,
     world,
     pegasoft.strings,
     scanner,
     scanner.communications,
     parser,
     parser_params;
use  ada.strings.unbounded,
     world,
     pegasoft.strings,
     scanner,
     scanner.communications,
     parser,
     parser_params;

package body parser_records is

------------------------------------------------------------------------------
-- Records package identifiers
------------------------------------------------------------------------------

records_to_json_t        : identifier;
records_to_record_t      : identifier;

procedure expectRecord( sub_id, id : identifier ) is
begin
  if identifiers( sub_id ).class = funcClass then
     err( context => sub_id,
          subject => id,
          subjectType => identifiers( id ).kind,
          reason => +"was used but the function expects",
          obstructorNotes => pl( "a " ) & em( "record" ),
          seeAlso => seeArrays
     );
  else
     err( context => sub_id,
          subject => id,
          subjectType => identifiers( id ).kind,
          reason => +"was used but the procedure expects",
          obstructorNotes => pl( "a " ) & em( "record" ),
          seeAlso => seeArrays
     );
  end if;
end expectRecord;

---------------------------------------------------------
-- PARSE THE RECORDS PACKAGE
---------------------------------------------------------

procedure ParseRecordsToJson is
  -- Syntax: records.to_json( str, rec );
  -- Source: N/A
  target_ref    : reference;
  source_var_id : identifier;
  jsonString    : unbounded_string;
  subprogramId  : constant identifier := records_to_json_t;
begin
  expect( records_to_json_t );
  expect( symbol_t, "(" );
  ParseOutParameter( target_ref, json_string_t );
  expectParameterComma;
  ParseIdentifier( source_var_id );
  if identifiers( getBaseType( identifiers( source_var_id ).kind ) ).kind /= root_record_t then
     expectRecord( subprogramId, source_var_id );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     DoRecordToJson( jsonString, source_var_id );
     assignParameter( target_ref, jsonString );
  end if;
end ParseRecordsToJson;

procedure ParseRecordsToRecord is
  -- Syntax: str := records.to_record( rec, str );
  -- Source: N/A
  --target_var_id : identifier;
  target_ref    : reference;
  sourceVal     : unbounded_string;
  sourceType    : identifier;
  baseType      : identifier;
  subprogramId  : constant identifier := records_to_record_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  -- TODO: refactor this.
  --
  -- Since this function accepts any record, we cannot use ParseOutParameter
  -- as we have no default.  ParseIdentifier will treat it as a read-but-not-
  -- written, which will throw off the constant tests.  So we have to use
  -- ParseInOutParameter.  Since we don't have nested record yet in
  -- SparForte, we're not using assignParameter..and DoJsonToRecord doesn't
  -- support it anyway.
  --
  -- As a side-effect, InOut is going to create local record fields as if
  -- this was a normal procedure.  We don't need these fields since we can
  -- write the original directly.  We can discard these with pushBlock/
  -- pullBLock
  --
  -- ParseIdentifier( target_var_id );
  -- ParseOutParameter( target_ref, json_string_t );
  pushBlock( newScope => true );
  ParseInOutParameter( target_ref );
  pullBlock;
  baseType := getBaseType( identifiers( target_ref.id ).kind );
  if identifiers( baseType ).kind /= root_record_t then
     expectRecord( subprogramId, target_ref.id  );
  end if;
  expectParameterComma;
  ParseExpression( sourceVal, sourceType );
  if baseTypesOK( sourceType, json_string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       DoJsonToRecord( target_ref.id, sourceVal );
     exception when constraint_error =>
       err( contextNotes => pl( "At " & gnat.source_info.source_location ) &
               contextAltText( sourceVal,"decoding the JSON string" ),
            subject => target_ref.id,
            subjectType => identifiers( target_ref.id ).kind,
            reason =>  +"the decoding failed because",
            obstructorNotes => +"a constraint error was raised"
       );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseRecordsToRecord;

-------------------------------------------------------------------------------
-- Housekeeping
-------------------------------------------------------------------------------

procedure StartupRecords is
begin
  declareNamespace( "records" );
  declareProcedure( records_to_json_t, "records.to_json", ParseRecordsToJSON'access );
  declareProcedure( records_to_record_t, "records.to_record", ParseRecordsToRecord'access );
  declareNamespaceClosed( "records" );
end StartupRecords;

procedure ShutdownRecords is
begin
  null;
end ShutdownRecords;

end parser_records;
