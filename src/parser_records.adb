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
-- trim?
                            jsonString := jsonString & identifiers( field_t ).value;
                         elsif uniFieldType = root_enumerated_t then
                            jsonString := jsonString & identifiers( field_t ).value;
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
  elementKind   : identifier;
  ch            : character;
  sourceLen     : long_integer;
  found         : boolean;
  searchName    : unbounded_string;
  inBackslash   : boolean;
  inQuotes      : boolean;
  jsonStringType : boolean;
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

     k := 2; -- skip leading { in JSON
     item := sourceVal;

     -- basic JSon validation.  Important to verify it isn't an array.
     if length( item ) > 0 then
        if element( item, 1 ) = '[' then
           err( "JSON object expected but found array" );
        elsif element( item, 1 ) /= '{' then
           err( "JSON object expected but found string " & optional_bold( to_string( toEscaped( item ) ) ) );
        elsif element( item, length( item ) ) /= '}' then
           err( "expected trailing }" );
        end if;
     end if;

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
          inQuotes := false;
          for z in 1..2 loop
             decodedItem := null_unbounded_string;
             -- if first character is a quote, it's a string value
             -- use this to validate later on
             ch := element( item, k );
             if z = 2 then
                jsonStringType := ch = '"';
             end if;
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
                elsif not inQuotes and then ch = ',' then -- TODO: NOT RIGHT!
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
                 if not error_found then
                    -- for booleans, it's true or false, not value
                    elementKind := getBaseType( identifiers( j ).kind );
                    if elementKind = boolean_t then
                       if decodedItemValue = "true" then
                          identifiers( j ).value := to_unbounded_string( "1" );
                       elsif decodedItemValue = "false" then
                          identifiers( j ).value :=  to_unbounded_string( "0" );
                       else
                          err( optional_bold( to_string( toEscaped( decodedItemName ) ) ) & " has a value of " & optional_bold( to_string( toEscaped( decodedItemValue ) ) ) & " but expected JSON true or false" );
                       end if;

-- range check the valuse for enumerateds
                    elsif getUniType( elementKind ) = root_enumerated_t then

                       -- i don't actually record the maximum value for an enumerated type
                       -- the only way to tell is to search the symbol table for a match.

                      declare
                        maxEnum : integer;
                        enumVal : integer;
                      begin
                        for i in reverse keywords_top..identifiers_top-1  loop
                            if identifiers( i ).kind = elementKind then
                               if identifiers( i ).class = constClass then
                                  if identifiers( i ).class = constClass then
                                     maxEnum := integer( to_numeric( identifiers( i ).value ) );
                                     exit;
                                  end if;
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
                        identifiers( j ).value := decodedItemValue;
                      end;
                    elsif getUniType( elementKind ) = uni_string_t then
-- HERE
                      if not jsonStringType then
                         err( "JSON string value expected" );
                      end if;
                      identifiers( j ).value := castToType( decodedItemValue,
                         identifiers( j ).kind );
                    else
                      if jsonStringType then
                         err( "JSON number value expected" );
                      end if;
                      identifiers( j ).value := castToType( decodedItemValue,
                         identifiers( j ).kind );
                    end if;
                 end if;
                 --assignParameter( arget_ref, jsonString );

              end if;
          end loop; -- j
          if not found then
             err( to_string( toEscaped( searchName ) ) & " not declared" );
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
