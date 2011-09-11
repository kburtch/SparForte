------------------------------------------------------------------------------
-- Records Package Parser                                                   --
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

with gnat.bubble_sort_a,
     gnat.heap_sort_a,
     ada.numerics.float_random,
     bush_os,
     string_util,
     user_io,
     world,
     parser,
     parser_aux;
use  bush_os,
     string_util,
     user_io,
     world,
     parser,
     parser_aux;
with ada.text_io; use ada.text_io;

package body parser_records is


---------------------------------------------------------
-- PARSE THE RECORDS PACKAGE
---------------------------------------------------------

procedure ParseRecordsToJson is
  -- Syntax: records.to_json( str, rec );
  -- Source: N/A
  target_ref    : reference;
  source_var_id : identifier;
  jsonString    : unbounded_string;
  firstField    : boolean := true;
begin
  expect( records_to_json_t );
  expect( symbol_t, "(" );
  ParseOutParameter( target_ref, json_string_t );
  expect( symbol_t, "," );
  ParseIdentifier( source_var_id );
  if identifiers( getBaseType( identifiers( source_var_id ).kind ) ).kind /= root_record_t then
     err( "Record type expected" );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     jsonString := to_unbounded_string( "{" );
     for i in 1..integer'value( to_string( identifiers( identifiers( source_var_id ).kind ).value ) ) loop
         for j in 1..identifiers_top-1 loop
             if identifiers( j ).field_of = identifiers( source_var_id ).kind then
                if integer'value( to_string( identifiers( j ).value )) = i then
                   declare
                      fieldName   : unbounded_string;
                      jsonFieldName   : unbounded_string;
                      --dont_care_t : identifier;
                      dotPos      : natural;
                      field_t     : identifier;
                      uniFieldType : identifier;
                      item        : unbounded_string;
                      ch          : character;
                   begin
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
                            jsonString := jsonString & ',';
                         end if;
                         jsonString := jsonString & '"' & jsonFieldName & '"' & ":";
                         -- json encode primitive types
                         uniFieldType := getUniType( identifiers( field_t ).kind );
                         if getBaseType( identifiers( field_t ).kind ) = boolean_t then
                            if integer( to_numeric( identifiers( field_t ).value ) ) = 0 then
                               jsonString := jsonString & "false";
                            else
                               jsonString := jsonString & "true";
                            end if;
                         elsif uniFieldType = uni_numeric_t then
                            jsonString := jsonString & identifiers( field_t ).value;
-- boolean
                         else
                            item := to_unbounded_string( """" );
                            item := item & ToJSONEscaped( identifiers( field_t ).value );
                            item := item & '"';
                            jsonString := jsonString & item;
                         end if;
                      end if;
                   end;
                end if;
             end if;
         end loop;
     end loop;
     jsonString := jsonString & "}";
     assignParameter( target_ref, jsonString );
  end if;
end ParseRecordsToJson;

procedure ParseRecordsToRecord is
  -- Syntax: str := records.to_record( rec, str );
  -- Source: N/A
  target_ref    : reference;
  sourceVal     : unbounded_string;
  sourceType    : identifier;
  jsonString    : unbounded_string;
  firstField    : boolean := true;
  k             : natural;
  item          : unbounded_string;
  decodedItem  : unbounded_string;
  decodedItemName  : unbounded_string;
  decodedItemValue  : unbounded_string;
  ch            : character;
  sourceLen     : long_integer;
  found         : boolean;
  searchName    : unbounded_string;
  inBackslash   : boolean;
  inQuotes      : boolean;
begin
  expect( records_to_record_t );
  expect( symbol_t, "(" );
  ParseInOutParameter( target_ref );
  if identifiers( getBaseType( identifiers( target_ref.id ).kind ) ).kind /= root_record_t then
     err( "Record type expected" );
  end if;
  expect( symbol_t, "," );
  ParseExpression( sourceVal, sourceType );
  if baseTypesOK( sourceType, json_string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     -- TODO: validate {
     k := 2; -- skip leading { in JSON
     item := sourceVal;

     -- Count the number of items in the JSON string
     sourceLen := 0;
     inBackslash := false;
     inQuotes := false;
     if length( item ) > 2 then -- length zero for {}
        for i in 2..length( item )-1 loop
            ch := element( item, i );
            if inBackslash then
               inBackslash := false;
            else
               if ch = '\' then
                  inBackslash := true;
               else
                  if not inBackslash and ch = '"' then
                     inQuotes := not inQuotes;
                  elsif not inQuotes and ch = ',' then
                     sourceLen := sourceLen + 1;
                  end if;
               end if;
            end if;
        end loop;
        sourceLen := sourceLen + 1;
    end if;

    -- The number of items in the JSON string should equal the size of the
    -- record.
    if sourceLen = long_integer'value( to_string( identifiers( identifiers( target_ref.id ).kind ).value ) ) then

       -- for each of the items in the JSON string
       for i in 1..sourceLen loop

          decodedItemName := null_unbounded_string;
          decodedItemValue := null_unbounded_string;

          -- there should be a label and a string
          -- this should be smarter TODO
          -- z = 1 is the label, z = 2 is the value
          -- TODO: we can tell from JSON if something is numeric
          -- or string.  We can type-check against that.
          inQuotes := false;
          for z in 1..2 loop
             decodedItem := null_unbounded_string;
             loop
                exit when k > length(item)-1;
                ch := element( item, k );
                if ch = '\' then
                   k := k + 1;
                   ch := element( item, k );
                   if ch = '"' then
                      decodedItem := decodedItem & '"';
                   elsif ch = '\' then
                      decodedItem := decodedItem & '\';
                   elsif ch = '/' then
                      decodedItem := decodedItem & '/';
                   elsif ch =  'b' then
                      decodedItem := decodedItem & ASCII.BS;
                   elsif ch = 'f' then
                      decodedItem := decodedItem & ASCII.FF;
                   elsif ch = 'n' then
                      decodedItem := decodedItem & ASCII.LF;
                   elsif ch = 'r' then
                      decodedItem := decodedItem & ASCII.CR;
                   elsif ch = 't' then
                      decodedItem := decodedItem & ASCII.HT;
                   end if;
                elsif ch = '"' then
                   inQuotes := not inQuotes;
                elsif not inQuotes and then ch = ':' then
                   k := k + 1;
                   exit;
                elsif not inQuotes and then ch = ',' then -- NOT RIGHT!
                   k := k + 1;
                   exit;
                else
                   decodedItem := decodedItem & ch;
                end if;
                k := k + 1;
             end loop;
             if z = 1 then
                decodedItemName := decodedItem;
             else
                decodedItemValue := decodedItem;
             end if;
          end loop; -- z

          -- we have a label and a value.  the record field is stored in the
          -- symbol table as rec.field.  Prepend the record name and search
          -- the symbol table for the record field.  When found, cast the
          -- value and assign it.  Otherwise, if the field is not found, it
          -- is an error.
          found := false;
          searchName := identifiers( target_ref.id ).name & "." & decodedItemName;
          for j in 1..identifiers_top-1 loop
              if identifiers( j ).name = searchName then
                 found := true;
                 decodedItemValue := castToType( decodedItemValue,
                     identifiers( j ).kind );
                 if not error_found then
                    identifiers( j ).value := decodedItemValue;
                 end if;
                 --assignParameter( arget_ref, jsonString );

              end if;
          end loop; -- j
          if not found then
             err( to_string( searchName ) & " not declared" );
          end if;
       end loop; -- i
    else
       err( "record has" &
            to_string( identifiers( identifiers( target_ref.id ).kind ).value ) &
            " field(s) but JSON string has" &
            sourceLen'img );
    end if;
  end if;
end ParseRecordsToRecord;

-------------------------------------------------------------------------------
-- Housekeeping
-------------------------------------------------------------------------------

procedure StartupRecords is
begin
  declareProcedure( records_to_json_t, "records.to_json" );
  declareProcedure( records_to_record_t, "records.to_record" );
end StartupRecords;

procedure ShutdownRecords is
begin
  null;
end ShutdownRecords;

end parser_records;
