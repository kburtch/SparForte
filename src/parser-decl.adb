------------------------------------------------------------------------------
-- AdaScript Language Parser                                                --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2025 Free Software Foundation              --
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
pragma ada_2005;

with ada.text_io;
use ada.text_io;

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );

with gnat.source_info,
    gnat.io_aux,
    spar_os,
    pegasoft.strings,
    pegasoft.numerics,
    performance_monitoring,
    compiler,
    scanner,
    scanner.communications,
    scanner_res,
    scanner_restypes,
    parser.decl.as, -- circular relationship for ParseBlock
    parser_aux,
    parser_params,
    interpreter; -- circular relationship for breakout prompt
use gnat.io_aux,
    spar_os,
    pegasoft,
    pegasoft.strings,
    pegasoft.numerics,
    performance_monitoring,
    compiler,
    scanner,
    scanner.communications,
    scanner_res,
    scanner_restypes,
    parser.decl.as, -- circular relationship for ParseBlock
    parser_aux,
    parser_params,
    interpreter; -- circular relationship for breakout prompt

package body parser.decl is

-----------------------------------------------------------------------------
-- Declarations
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  MAYBE SECRET
--
-- True if value looks like it could be encrypted data.
-----------------------------------------------------------------------------

function maybe_secret( field_name, str_val : unbounded_string ) return boolean is
  score : numericValue := 0.0;
  could_be : boolean := false;
begin
  if length( str_val ) >= 8 then
     score := shannon_entropy_of( str_val );

     -- Heuristics: field name (English)

     if index( field_name, "auth" ) > 0 then
        score := score + 1.0;
     elsif index( field_name, "crypt" ) > 0 then
        score := score + 1.0;
     elsif index( field_name, "hash" ) > 0 then
        score := score + 1.0;
     elsif index( field_name, "pass" ) > 0 then
        score := score + 1.0;
     elsif index( field_name, "token" ) > 0 then
        score := score + 1.0;
     elsif tail( field_name, 3 ) = "_id" then
        score := score + 1.0;
     end if;

     -- Heuristics: SSH

     if head(str_val, 7) = "ssh-rsa" then
        score := score + 1.0;
     elsif head(str_val, 7) = "-----B" then
        score := score + 1.0;
     end if;

     -- Heuristics: multiple of 8 characters

     if length(str_val) / 8 = 0 then
        score := score + 1.0;
     end if;
     if score > 5.5 then
        could_be := true;
     end if;
  end if;
  return could_be;
end maybe_secret;


-----------------------------------------------------------------------------
--  PARSE TYPE USAGE QUALIFIERS
--
-- In a type declaration, parent types may have a usage qualifier.  Check
-- for a usage qualifier and apply it to the type declaration.
-- e.g. With "type const_int is constant integer", handle "constant".
-- Syntax: type-usage-qualifiers = [abstract | limited | constant]
-- Type usage qualifiers and variable usage qualifiers are not identical:
-- variables cannot be abstract.
-----------------------------------------------------------------------------

procedure ParseTypeUsageQualifiers( newtype_id : identifier ) is
begin
   -- abstract types

   if token = abstract_t then
      if onlyAda95 then
        err( contextNotes => +"While checking the type usage qualifiers",
             subject => newtype_id,
             reason => +"cannot be abstract because of",
             obstructorNotes => obstructorAda95,
             remedy => +"Ada does not support qualifiers",
             seeAlso => seeTypes
        );
      end if;
      identifiers( newtype_id ).usage := abstractUsage; -- vars not allowed
      identifiers( newtype_id ).wasReferenced := true;  -- treat as used
      identifiers( newtype_id ).wasApplied := true;     -- treat as applied
      expect( abstract_t );
      if token = abstract_t then
        err( contextNotes => +"While checking the type usage qualifiers",
             subject => newtype_id,
             reason => +"has a redundant qualifier",
             obstructorNotes => name_em(token)
        );
      elsif token = limited_t or token = constant_t then
        err( contextNotes => +"While checking the type usage qualifiers",
             subject => newtype_id,
             reason => +"can have only one qualifier but",
             obstructorNotes => name_em(token) & pl(" is a second one"),
             remedy => +"use only one of abstract, limited or constant",
             seeAlso => seeTypes
        );
      end if;

   -- limited types

   elsif token = limited_t then
      if onlyAda95 then
         err( contextNotes => +"While checking the type usage qualifiers",
             subject => newtype_id,
             reason => +"cannot be limited because of",
             obstructorNotes => obstructorAda95,
             remedy => +"Ada does not support qualifiers",
             seeAlso => seeTypes
        );
      end if;
      identifiers( newtype_id ).usage := limitedUsage;  -- assign not allowed
      expect( limited_t );
      if token = limited_t then
        err( contextNotes => +"While checking the type usage qualifiers",
             subject => newtype_id,
             reason => +"has a redundant qualifier",
             obstructorNotes => name_em(token)
        );
      elsif token = abstract_t or token = constant_t then
         err( contextNotes => +"While checking the type usage qualifiers",
              subject => newtype_id,
              reason => +"can have only one qualifier but",
              obstructorNotes => name_em(token) & pl(" is a second one"),
              remedy => +"use only one of abstract, limited or constant",
              seeAlso => seeTypes
         );
      end if;

   -- constant types

   elsif token = constant_t then
      if onlyAda95 then
         err( contextNotes => +"While checking the type usage qualifiers",
             subject => newtype_id,
             reason => +"cannot be constant because of",
             obstructorNotes => obstructorAda95,
             remedy => +"Ada does not support qualifiers",
             seeAlso => seeTypes
        );
      end if;
      identifiers( newtype_id ).usage := constantUsage;  -- read-only
      expect( constant_t );
      if token = constant_t then
        err( contextNotes => +"While checking the type usage qualifiers",
             subject => newtype_id,
             reason => +"has a redundant qualifier",
             obstructorNotes => name_em(token)
        );
      elsif token = abstract_t or token = limited_t then
         err( contextNotes => +"While checking the type usage qualifiers",
              subject => newtype_id,
              reason => +"can have only one qualifier but",
              obstructorNotes => name_em(token) & pl(" is a second one"),
              remedy => +"use only one of abstract, limited or constant",
              seeAlso => seeTypes
         );
      end if;

   -- default same as parent

   end if;
end ParseTypeUsageQualifiers;


-----------------------------------------------------------------------------
--  PARSE VAR USAGE QUALIFIERS
--
-- In a variable declaration, data types may have a usage qualifier.  Check
-- for a usage qualifier and apply it to the variable declaration.
-- e.g. With "x : constant integer", handle "constant".
-- If a constant, expr_expected is set to true to alert the caller
-- that a constant declaration may need a value assigned.
-- Syntax: var-usage-qualifiers = [limited | constant]
-- Type usage qualifiers and variable usage qualifiers are not identical:
-- variables cannot be abstract.
-----------------------------------------------------------------------------

procedure ParseVarUsageQualifiers( id : identifier; expr_expected : out boolean ) is
begin
  expr_expected := false;                              -- usually false

  if token = aliased_t then                            -- aliased not supported
     err( +"aliased not implemented" );

  elsif token = constant_t then                        -- handle constant
     identifiers( id ).usage := constantUsage;         -- as a constant and
     expr_expected := true;                            -- must assign value
     expect( constant_t );                             -- by flagging variable
     if token = constant_t then
        err( contextNotes => +"While checking the variable usage qualifiers",
             subject => id,
             reason => +"has a redundant qualifier",
             obstructorNotes => name_em(token)
        );
     elsif token = abstract_t or token = limited_t then
         err( contextNotes => +"While checking the variable usage qualifiers",
              subject => id,
              reason => +"can have only one qualifier but",
              obstructorNotes => name_em(token) & pl(" is a second one"),
              remedy => +"use only one of limited or constant",
              seeAlso => seeTypes
         );
     end if;

  elsif token = abstract_t then                        -- abstract only makes sense
     err( contextNotes => +"While checking the variable usage qualifiers",
          subject => id,
          reason => +"cannot be abstract because",
          obstructorNotes => +"abstract usage is not conrete and cannot create variables",
          seeAlso => seeTypes
     );

  elsif token = limited_t then                         -- limited access?
     identifiers( id ).usage := limitedUsage;
     expect( limited_t );
     if token = limited_t then
        err( contextNotes => +"While checking the variable usage qualifiers",
             subject => id,
             reason => +"has a redundant qualifier",
             obstructorNotes => name_em(token)
        );
     elsif token = abstract_t or token = constant_t then
         err( contextNotes => +"While checking the variable usage qualifiers",
              subject => id,
              reason => +"can have only one qualifier but",
              obstructorNotes => name_em(token) & pl(" is a second one"),
              remedy => +"use only one of limited or constant",
              seeAlso => seeTypes
         );
     end if;
  end if;
end ParseVarUsageQualifiers;


-----------------------------------------------------------------------------
--  PARSE GENERIC PARAMETERS PART
--
-- Generic types require one or two parameters to complete them.  Parse these
-- type parameters, saving them in the variable created from the type.
-- Example: "x : doubly_linked_list.list( string )", "( string )" is the
-- parameter and it is saved in variable x.
-- Syntax: generic-parameters = ( gen1 [,gen2] )
-- Currently, there are no user generic types, only built-in ones.
-----------------------------------------------------------------------------

procedure ParseGenericParametersPart( varId, genTypeId : identifier ) is
  genKind : identifier;
begin
  if token /= symbol_t or identifiers( token ).svalue /= "(" then

     err( contextNotes => +"While checking the generic type",
          subject => varId,
          subjectType => genTypeId,
          reason => +"uses a type that needs parameters",
          obstructorNotes => nullMessageStrings,
          remedy => +"a generic type needs to know what types it is working with to be complete and usable"
     );
  end if;
  expect( symbol_t, "(" );
  ParseIdentifier( genKind );
  if type_checks_done or else class_ok( genKind, typeClass, subClass ) then
     identifiers( varId ).genKind := genKind;
     if token = symbol_t and identifiers( token ).svalue = "," then
        getNextToken;
        ParseIdentifier( genKind );
        if type_checks_done or else class_ok( genKind, typeClass, subClass ) then
           identifiers( varId ).genKind2 := genKind;
        end if;
     else
        identifiers( varId ).genKind2 := eof_t;
     end if;
  end if;
  expect( symbol_t, ")" );
end ParseGenericParametersPart;


-----------------------------------------------------------------------------
--  PARSE RENAMES PART
--
-- Handle a renames clause in a variable declaration.  canonicalRef is the
-- source of the renaming.  new_id and new_type_id is the alias that is
-- created.  This also validates the usage qualifiers, ensuring that the
-- renaming has equal or more restrictive usage than the source, or duplicating
-- the usage if none is specified for the alias.
-- Syntax: renames-part = renames ident ...
-- The caller must setup the value pointer for the renaming
-----------------------------------------------------------------------------

procedure ParseRenamesPart( canonicalRef : out renamingReference;
  new_id, new_type_id : identifier ) is
begin
  expect( renames_t );
  canonicalRef.id := token;
  -- To support array element renaming, we need a reference not an identifier.

  ParseRenamingReference( canonicalRef, new_type_id );

  -- only copy attributes if no error because copying attributes will
  -- declare the identifier as a side-effect
  -- if isExecutingCommand then
  if not error_found then
     begin
       declareRenaming( new_id, canonicalRef );
       identifiers( new_id ).declaredAt := getLineNo;
       identifiers( new_id ).declaredFile := getSourceFileName;

       -- check to see that the usage qualifier isn't less restrictive
       -- compared to the canonical identifier being renamed

       -- KB: 20/03/18 - these are tested for elsewhere
       --case identifiers( canonicalRef.id ).usage is
       --when fullUsage =>
       --   null; -- always good
       --when constantUsage =>
       --   if identifiers( new_id ).usage = fullUsage then
       --      err( "no qualifier is less restrictive than constant" );
       --   end if;
       --when limitedUsage =>
       --   if identifiers( new_id ).usage = fullUsage then
       --      err( "no qualifier is less restrictive than limited" );
       --   elsif identifiers( new_id ).usage = constantUsage then
       --      err( "constant is less restrictive than limited" );
       --   end if;
       --when abstractUsage =>
       --   err( gnat.source_info.source_location &
       --        "internal error: abstract usage qualifier not expected" );
       --when others =>
       --   err( gnat.source_info.source_location &
       --        "internal error: unexpected usage qualifier" );
       --end case;
     end;
  end if;
end ParseRenamesPart;


-----------------------------------------------------------------------------
--  PARSE COPIES PART
--
-- Handle a copies clause in a variable declaration.  canonicalRef is the
-- source of the copy.  new_id and new_type_id is the copy that is
-- created.  This also validates the usage qualifiers, ensuring that the
-- copy has equal or more restrictive usage than the source, or duplicating
-- the usage if none is specified for the copy.
-- Syntax: copies-part = copies ident ...
-- The caller must setup the value pointer for the copy
-----------------------------------------------------------------------------

procedure ParseCopiesPart( canonicalRef : out renamingReference;
  new_id, new_type_id : identifier ) is
begin
  expect( copies_t );
  canonicalRef.id := token;
  -- To support array element renaming, we need a reference not an identifier.

  -- This is the same syntax as a renaming
  ParseRenamingReference( canonicalRef, new_type_id );

  -- only copy attributes if no error because copying attributes will
  -- declare the identifier as a side-effect
  if not error_found then
     begin
       --identifiers( new_id ).usage := identifiers( canonicalRef.id ).usage;
       identifiers( new_id ).value := identifiers( new_id ).svalue'access;

       -- For a volatile, update the value before copying.
       if isExecutingCommand then
          if identifiers( canonicalRef.id ).volatile /= none then
             refreshVolatile( canonicalRef.id );
          end if;
       end if;

       if canonicalRef.index = 0 then
          identifiers( new_id ).svalue := identifiers( canonicalRef.id ).value.all;
       else
          identifiers( new_id ).svalue := identifiers( canonicalRef.id ).avalue( canonicalRef.index );
       end if;

       -- check to see that the usage qualifier isn't less restrictive
       -- compared to the canonical identifier being copied

       -- KB: 20/03/18 - these are tested for elsewhere
       -- case identifiers( canonicalRef.id ).usage is
       -- when fullUsage =>
       --    null; -- always good
       -- when constantUsage =>
       --    if identifiers( new_id ).usage = fullUsage then
       --       err( "no qualifier is less restrictive than constant" );
       --    end if;
       -- when limitedUsage =>
       --    err( "limited identifiers cannot be copied" );
       -- when abstractUsage =>
       --    err( gnat.source_info.source_location &
       --         "internal error: abstract usage qualifier not expected" );
       -- when others =>
       --    err( gnat.source_info.source_location &
       --         "internal error: unexpected usage qualifier" );
       -- end case;

       -- All of our resources are limited.  However, as a safety precaution:
       -- If an identifier has an external reference, we cannot copy it because
       -- we could inadvertently deallocate it in one place while keeping it open
       -- in another.

       if identifiers( canonicalRef.id ).resource then
          err( contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while copying the value" ),
             subjectNotes => subjectInterpreter,
             reason => +"had an internal error because",
             obstructorNotes => +"resource identifiers cannot be copied"
         );
       end if;

     end;
  end if;
end ParseCopiesPart;


-----------------------------------------------------------------------------
--  PARSE ASSIGN PART
--
-- Handle an expression used to assign a value.  The value and its type are
-- returned.
-- Example: "x : integer := 5", ":= 5" is the assign part.
-- Syntax: assign-part = " := default_value_expression"
-----------------------------------------------------------------------------

procedure ParseAssignPart( expr_value : out unbounded_string; expr_type : out identifier ) is
begin
  expect( symbol_t, ":=" );
  ParseExpression( expr_value, expr_type );
end ParseAssignPart;


-----------------------------------------------------------------------------
--  PARSE ARRAY ASSIGN PART
--
-- Handle a list of expressions to be assigned to an array as initial values.
-- Syntax: array-assign-part = " := ( expr, expr, ... )|second-array"
-- Note that ":= (5)" is ambiguous, since Ada syntax uses round brackets for
-- both subexpressions and lists of values.  SparForte treats any outer
-- round brackets as a list of values.  More recent versions of Gnat require
-- (1 => 5) positional syntax which is not currently supported in SparForte.
-- Also note that others is not yet supported
-----------------------------------------------------------------------------

procedure ParseArrayAssignPart( array_id : identifier ) is
-- procedure ParseArrayAssignPart( array_id : identifier; array_id2: arrayID ) is
  expr_value : unbounded_string;
  expr_type  : identifier;
  arrayIndex : long_integer;
  lastIndex  : long_integer;
  second_array_id  : identifier;
  base_type  : identifier;
begin

  -- Note: Array ID will not be valid at syntax check time

  expect( symbol_t, ":=" );
  if token = symbol_t then                                     -- assign (..)?
     expect( symbol_t, "(" );                                  -- read constant
     if isExecutingCommand then
        base_type := getBaseType( identifiers( array_id ).kind );
        arrayIndex := identifiers( base_type ).firstBound;
        lastIndex := identifiers( base_type ).lastBound;
     end if;
     loop                                                      -- read values
       ParseExpression( expr_value, expr_type );               -- next element
       if isExecutingCommand then                              -- not on synchk
             begin
               identifiers( array_id ).avalue( arrayIndex ) := expr_value;
             exception when CONSTRAINT_ERROR =>
               err( contextNotes => pl( "assigning the array values" ),
                  subjectNotes => em( "array position" & arrayIndex'img ),
                  reason => +"cannot be assigned because",
                  obstructorNotes => pl( "the array bounds have range" &
                       identifiers( array_id ).avalue'first'img & " .." & identifiers( array_id ).avalue'last'img )
               );
             when STORAGE_ERROR =>
               err( contextNotes => pl( "At " & gnat.source_info.source_location &
                  " while assigning the array values" ),
                  subjectNotes => subjectInterpreter,
                  reason => +"had an internal error because",
                  obstructorNotes => +"a storage_error was raised"
               );
             end;
          --end if;
       end if;
       if arrayIndex = long_integer'last then                  -- should never
          err( contextNotes => pl( "assigning the array values" ),
               subjectNotes => subjectInterpreter,
               reason => +"cannot handle an array positions >=",
               obstructorNotes => em( arrayIndex'img )
          );
       else                                                    -- check anyway
          arrayIndex := arrayIndex+1;                          -- next element
       end if;                                                 -- stop on err
       exit when error_found or identifiers( token ).value.all /= ","; -- more?
       expectParameterComma;                                   -- continue
     end loop;
     arrayIndex := arrayIndex - 1;                             -- last added
     if trace then
        put_trace(
            to_string( identifiers( array_id ).name ) & " :=" &
            arrayIndex'img & " elements" );
     end if;
     if isExecutingCommand then                                -- not on synchk
        if arrayIndex < lastIndex then                         -- check sizes
           err( contextNotes => pl( "assigning the array values" ),
              subjectNotes => em( "the last value is at position" & arrayIndex'img ),
              reason => +"and that is too few items to fill",
              obstructorNotes => pl( "the array bounds of range" &
                 identifiers( array_id ).avalue'first'img & " .." & identifiers( array_id ).avalue'last'img )
           );
         end if;
     end if;
     expect( symbol_t, ")" );
  else                                                         -- copying a
     ParseIdentifier( second_array_id );                       -- second array?
     if not type_checks_done then
        -- must be an array variable of an acceptable type
        if class_ok( second_array_id, varClass ) then
           if baseTypesOK( identifiers( array_id ).kind, identifiers( second_array_id ).kind ) then
              null;
           end if;
        end if;
     end if;

     if isExecutingCommand then
        begin
          base_type := getBaseType( identifiers( array_id ).kind );
          arrayIndex := identifiers( base_type ).firstBound;
          lastIndex := identifiers( base_type ).lastBound;
           if identifiers( array_id ).avalue = null then
              err( contextNotes => pl( "At " & gnat.source_info.source_location ),
                   subjectNotes => pl( qp( "creating the array" ) ),
                   reason => +"had an internal error because",
                   obstructorNotes => em( "the target array storage was unexpectedly null" )
              );
           elsif identifiers( array_id ).avalue'first /= arrayIndex then
              err( contextNotes => pl( "At " & gnat.source_info.source_location ),
                   subjectNotes => pl( qp( "creating the array" ) ),
                   reason => +"had an internal error because",
                   obstructorNotes => pl( "the target array first bound doesn't match: " &
                      identifiers( array_id ).avalue'first'img & " vs " & arrayIndex'img  )
              );
           elsif identifiers( array_id ).avalue'last /= lastIndex then
              err( contextNotes => pl( "At " & gnat.source_info.source_location ),
                   subjectNotes => pl( qp( "creating the array" ) ),
                   reason => +"had an internal error because",
                   obstructorNotes => pl( "the target array last bound doesn't match: " &
                      identifiers( array_id ).avalue'first'img & " vs " & arrayIndex'img )
              );
           elsif not error_found then
              identifiers( array_id ).avalue.all := identifiers( second_array_id ).avalue.all;
           end if;
        exception when CONSTRAINT_ERROR =>
           err( contextNotes => pl( "At " & gnat.source_info.source_location ),
                subjectNotes => pl( qp( "creating the array" ) ),
                reason => +"had an internal error because",
                obstructorNotes => pl( "a constraint_error was raised on index out of range " &
                   identifiers( array_id ).avalue'first'img & " .." & identifiers( array_id ).avalue'last'img )
           );
        when STORAGE_ERROR =>
           err( contextNotes => pl( "At " & gnat.source_info.source_location ),
                subjectNotes => pl( qp( "creating the array" ) ),
                reason => +"had an internal error because",
                obstructorNotes => pl( "a storage error raised when copying arrays" )
           );
        end;
        if trace then
           put_trace(
             to_string( identifiers( array_id ).name ) & " := " &
             to_string( identifiers( second_array_id ).name ) );
        end if;
     end if;
  end if;
end ParseArrayAssignPart;


-----------------------------------------------------------------------------
--  PARSE ANONYMOUS ARRAY
--
-- Handle an anonymous array declaration.  An anonymous array is an array
-- that is not predefined as it's own type.  A type declaration will be
-- created to represent the array type.  If limit is true, the type
-- will be limited.
-- Syntax: anon-array = " array(expr..expr) of ident [array-assn]
-- ParseDeclarationPart was getting complicated so this procedure
-- was declared separately.
-----------------------------------------------------------------------------

procedure ParseAnonymousArray( id : identifier; limit : boolean ) is
  -- array_id    : arrayID;           -- array table index for array variable
  -- type_id     : arrayID;           -- array table index for anon array type
  ab1         : unbounded_string;  -- first array bound
  kind1       : identifier;        -- type of first array bound
  ab2         : unbounded_string;  -- last array bound
  kind2       : identifier;        -- type of last array bound
  elementType : identifier;        -- array elements type
  elementBaseType : identifier;        -- base type of array elements type
  anonType    : identifier := eof_t;   -- identifier for anonymous array type
  b : boolean;
begin
  -- To create an anonymous array, we have to add a fake array type
  -- called "an anonymous array" to the symbol table and array table.

  expect( array_t );
  expectSymbol( expectedValue => "(",
     contextNotes => +"in the anonymous array declaration",
     subjectNotes => +"the start of the index range"
  );
  ParseExpression( ab1, kind1 );                           -- low bound
  -- should really be a constant expression but we can't handle that
  if getUniType( kind1 ) = uni_string_t then                 -- must be scalar
     err( contextNotes => +"in the anonymous array declaration",
          subject => id,
          reason => +"array indexes cannot be a string or character type like",
          obstructor => kind1,
          remedy => +"use a numeric or enumerated type",
          seeAlso => docArrays
     );
  elsif getUniType( kind1 ) = root_record_t then                 -- must be scalar
     -- this only occurs when a record variable is used in an index name.  A
     -- record type name is caught elsewhere.
     err( contextNotes => +"in the anonymous array declaration",
          subject => id,
          reason => +"array indexes cannot be a record type like",
          obstructor => kind1,
          remedy => +"use a numeric or enumerated type",
          seeAlso => docArrays
     );
     --err( pl( "array indexes cannot be a record type like " ) &
     --     name_em( kind1 ) );
  -- this is currently impossible: parseExpression will demand an
  -- array element, not the whole array
  -- elsif identifiers( getBaseType( kind1 ) ).list then
  --    err( "array indexes cannot be an array type like " &
  --         optional_yellow( to_string( identifiers( kind1 ).name ) ) );
  else
     expectSymbol( expectedValue => "..",
        contextNotes => +"in the anonymous array declaration",
        subjectNotes => +"the index range"
     );
     ParseExpression( ab2, kind2 );                            -- high bound
     if token = symbol_t and identifiers( token ).value.all = "," then
       featureNotYetImplemented( subjectNotes => "array of multiple dimensions",
          remedy => "encode a dimension to a JSON string as a workaround until support is written" );
     elsif type_checks_done or else baseTypesOK( kind1, kind2 ) then -- indexes good?
        if isExecutingCommand then                             -- not on synchk
           if ab1 = null_unbounded_string then
              err( contextNotes => +"in the anonymous array declaration",
                   subjectNotes => em( "the low bound of the array index range" ),
                   reason => em( "has no value" ),
                   obstructorNotes => nullMessageStrings,
                   remedy => +"a numeric variable was not assigned a value",
                   seeAlso => docArrays
              );

           elsif ab2 = null_unbounded_string then
              err( contextNotes => +"in the anonymous array declaration",
                   subjectNotes => em( "the high bound of the array index range" ),
                   reason => em( "has no value" ),
                   obstructorNotes => nullMessageStrings,
                   remedy => +"a numeric variable was not assigned a value",
                   seeAlso => docArrays
              );
           elsif to_numeric( ab1 ) > to_numeric( ab2 ) then    -- bound backwd?
              if long_integer( to_numeric( ab1 ) ) /= 1 and    -- only 1..0
                 long_integer( to_numeric( ab2 ) ) /= 0 then   -- allowed
                 err( contextNotes => +"in the anonymous array declaration",
                      subjectNotes => em( "the low bound of the array index range" ),
                      reason => em( "is greater than" ),
                      obstructorNotes => em( "the high bound"),
                      remedy => +"the bounds are incorrect, reversed or you mean 1..0 for an empty array",
                      seeAlso => docArrays
                 );
              end if;
           end if;
        end if;
     end if;
  end if;
  expectSymbol( expectedValue => ")",
     contextNotes => +"in the anonymous array declaration",
     subjectNotes => +"the end of index range"
  );
  expect( of_t );
  if token = exception_t then
     err( contextNotes => +"in the anonymous array declaration",
          subjectNotes => em( "an array" ),
          reason => em( "cannot contain" ),
          obstructorNotes => em( "exceptions"),
          seeAlso => docArrays
     );
  end if;
  ParseIdentifier( elementType );

  -- Declare anonymous type in symbol table and array table
  --
  -- Note: Bounds are expressions and may not be defined during syntax check
  -- (Constant assignments, etc. occur only when actually running a script)

  if not error_found then     -- syntax OK, but if execution failed, no
     elementBaseType := getBaseType( elementType );
     if identifiers( elementBaseType ).list  then
        featureNotYetImplemented( subjectNotes => "an array of arrays",
          remedy => "encode " & to_string( identifiers( elementType ).name ) &
            " as a JSON string as a workaround until support is written" );
     else
        declareIdent( anonType, to_unbounded_string( "an anonymous array" ),
           elementType, typeClass );
        identifiers( anonType ).list := true;
        identifiers( anonType ).wasReferenced := true; -- only referenced when declared
        identifiers( anonType ).declaredAt := getLineNo;
        identifiers( anonType ).declaredFile := getSourceFileName;
        --identifiers( anonType ).referencedByFlow := getDataFlowName;
        -- mark as limited, if necessary
        if limit then
           identifiers( anonType ).usage := limitedUsage;
        end if;
        if syntax_check then
           -- treat the anonymous type as applied (i.e. no need to be abstr.)
           -- for an anonymous array, the element type must be applied also
           identifiers( anonType ).wasApplied := true;
           identifiers( elementType ).wasApplied := true;
        end if;
        if type_checks_done or else class_ok( elementType, typeClass, subClass ) then     -- item type OK?
           if isExecutingCommand and not syntax_check then
              identifiers( anonType ).value.all := null_unbounded_string;
              identifiers( anonType ).firstBound := long_integer( to_numeric( ab1 ) );
              identifiers( anonType ).lastBound  := long_integer( to_numeric( ab2 ) );
              identifiers( anonType ).genKind    := kind1;
           end if;
        end if;
     end if;
  end if;

  -- Declare array variable in array table
  --
  -- Note: Bounds are expressions and may not be defined during syntax check
  -- (Constant assignments, etc. occur only when actually running a script)

  if isExecutingCommand then
     identifiers( id ).value.all := null_unbounded_string;
     identifiers( id ).avalue := findStorage( long_integer( to_numeric( ab1 ) ), long_integer( to_numeric( ab2 ) ) );
     identifiers( id ).genKind := kind1;
  end if;

  -- Change variable into an array

  if not error_found then     -- syntax OK, but if execution failed, no
     identifiers( id ).list := true;                           -- var is an array
     identifiers( id ).kind := anonType;
  end if;

  -- Any initial assignment?  Then do it.
  --
  -- Note: Array ID will not be valid at syntax check time

  if token = symbol_t and identifiers( token ).value.all = ":=" then
     ParseArrayAssignPart( id );
  end if;

  -- If the anonymous array declaration failed, delete the array variable
  -- and the hidden anonymous type.

  if error_found then
     b := deleteIdent( id );
     -- If there's an error during assignment, anonType may exist.
     -- Otherwise, it should not exist.  If it was declared,
     -- delete it on an error.
     if anonType /= eof_t then
        b := deleteIdent( anonType );
     end if;
  end if;

end ParseAnonymousArray;


-----------------------------------------------------------------------------
--  PARSE ARRAY DECLARATION
--
-- Handle the creation of an array variable and any renaming or default value
-- assignment.
-- Syntax: array-declaration = " := array_assign" | renames oldarray
-- ParseDeclarationPart was getting complicated so this procedure
-- was declared separately.
-- This should be renamed to make its function clearer.
-----------------------------------------------------------------------------

procedure ParseArrayDeclaration( id : identifier; arrayType : identifier ) is
  base_type_id : identifier;
  canonicalRef : renamingReference;
  b : boolean;
begin

  -- Renames clause
  -- if it appears, one can only rename...cannot assign.
  -- Renames part will copy the properties from the canonical array.

  if token = renames_t then
     -- Full array renaming
     ParseRenamesPart( canonicalRef, id, arrayType );
     FixRenamedArray( canonicalRef, id );
  else

     -- ParseDeclarationPart detected an array type, so let's set up the
     -- array variable.
     --
     -- Note: Bounds are expressions and may not be defined during syntax check
     -- (Constant assignments, etc. occur only when actually running a script)

     if isExecutingCommand then
        -- get the base type because this may be a subtype of another type
        -- subtypes are just renamings right now and they have no values/bounds
        if identifiers( arrayType ).class = subClass then
           base_type_id := getBaseType( arrayType );
        else
           base_type_id := arrayType;
        end if;
        identifiers( id ).value.all := null_unbounded_string;
        identifiers( id ).avalue := findStorage( identifiers( base_type_id ).firstBound, identifiers( base_type_id ).lastBound );
        identifiers( id ).genKind := identifiers( base_type_id ).genKind;
     end if;

     -- Change variable into an array

     identifiers( id ).list := true;                           -- var is an array
     identifiers( id ).kind := arrayType;

     -- CONST SPECS
     -- Constant Array Specification

     if token = symbol_t and identifiers( token ).value.all = ";" then
        if identifiers( id ).usage = constantUsage then
           identifiers( id ).specFile := getSourceFileName;
           identifiers( id ).specAt := getLineNo;
        end if;
     end if;

     -- Any initial assignment?  Then do it.
     --
     -- Note: Array ID will not be valid at syntax check time

     if token = symbol_t and identifiers( token ).value.all = ":=" then
       -- TODO: the recursion problem may exist here, where defining x but x may be
       -- in the assignment list.
        ParseArrayAssignPart( id );
     elsif token = symbol_t and identifiers( token ).svalue = "(" then
         err( name_em( arrayType ) & pl( " is not a generic type but has parameters" ) );
     end if;

     -- The element type of the array has been used.  Check for an error
     -- to ensure the arrayType is valid before setting was applied.

     if not error_found then
       identifiers( identifiers( arrayType ).kind ).wasApplied := true;
     end if;

     -- If the array declaration failed, delete the array variable.

     if error_found then
        b := deleteIdent( id );
     end if;

  end if;
end ParseArrayDeclaration;


-----------------------------------------------------------------------------
--  PARSE RECORD ASSIGN PART
--
-- Handle assigning default values to a record variable, or copying default
-- values from another record.
-- Syntax ... := (expr[,expr])|record_var
-----------------------------------------------------------------------------

procedure ParseRecordAssignPart( id : identifier; recType : identifier ) is
  field_no : integer;
  expr_value : unbounded_string;
  expr_type  : identifier;
  found      : boolean;
  expected_fields : integer;
  second_record_id : identifier;
begin
--put_line( "REC ASSIGN PART " & to_string( identifiers( id ).name ) ); -- DEBUG
  expect( symbol_t, ":=" );
  if token = symbol_t then                                     -- assign (..)?
     expect( symbol_t, "(" );                                  -- read constant
     field_no := 1;
     begin
       expected_fields := integer'value( to_string( identifiers( recType ).value.all ) );
     exception when others =>
       expected_fields := 0;
     end;
     loop                                                      -- read values
       ParseExpression( expr_value, expr_type );               -- next element
       found := false;
       for j in 1..identifiers_top-1 loop
           if identifiers( j ).field_of = recType then
              if integer'value( to_string( identifiers( j ).value.all )) = field_no then
                 found := true;
                 declare
                    fieldName : unbounded_string;
                    field_t : identifier;
                    p : natural;
                 begin
                    fieldName := identifiers( j ).name;
                    -- it is possible to have multiple periods in the name
                    -- search backwards for the field name.
                    p := length( fieldName );
                    while p > 0 loop
                       exit when element( fieldName, p ) = '.';
                       p := p - 1;
                    end loop;
                    if p = 0 then
                       field_t := eof_t;
                    else
                       fieldName := delete( fieldName, 1, p );
                       fieldName := identifiers( id ).name & "." & fieldName;
                    end if;
                    findIdent( fieldName, field_t );
                    if field_t = eof_t then
                       err( contextNotes => pl( "At " & gnat.source_info.source_location &
                            " while assigning a value" ),
                            subjectNotes => subjectInterpreter,
                            reason => +"had an internal error because",
                            obstructorNotes => +"it cound not find record field " &
                               unb_em( fieldName )
                       );
                    else
                       if type_checks_done or else baseTypesOK( identifiers( field_t ).kind, expr_type ) then
                          if isExecutingCommand then
                             identifiers( field_t ).value.all := expr_value;
                             if trace then
                                put_trace(
                                  to_string( fieldName ) & " := " &
                                  toSecureData( to_string( toEscaped( expr_value ) ) ) );
                             end if;
                          end if;
                       end if;
                    end if;
                 end;
           end if;
       end if;
       end loop; -- for
       if not found then
          err( contextNotes => pl( "assigning values to " &
                  to_string( identifiers( id ).name ) ),
               subjectNotes => pl( "record field" ) & em( field_no'img ),
               reason => +"cannot be assigned because the record only has",
               obstructorNotes => unb_em( trim( to_unbounded_string( expected_fields'img ), ada.strings.left ) ) &
               pl( " field(s)" )
          );
       end if;
       exit when error_found or identifiers( token ).value.all /= ","; -- more?
       expectParameterComma;
       field_no := field_no + 1;
     end loop;
     expect( symbol_t, ")" );
     if expected_fields /= field_no then
          err( contextNotes => pl( "assigning values to " &
                  to_string( identifiers( id ).name ) ),
               subjectNotes => pl( "record field" ) & em( field_no'img ),
               reason => +"cannot be assigned because the record has",
               obstructorNotes => unb_em( trim( to_unbounded_string( expected_fields'img ), ada.strings.left ) ) &
               pl( " field(s)" )
          );
     end if;
  else
     ParseIdentifier( second_record_id );                      -- second rec?
     if not type_checks_done then
        -- it must be a record variable of a compatible type
        if class_ok( second_record_id, varClass ) then
           if baseTypesOK( identifiers( id ).kind, identifiers( second_record_id ).kind ) then
              null;
           end if;
         end if;
     end if;

     if isExecutingCommand then
        begin
          expected_fields := integer'value( to_string( identifiers( recType ).value.all ) );
        exception when others =>
          expected_fields := 0;
        end;
        declare
           sourceFieldName : unbounded_string;
           targetFieldName : unbounded_string;
           source_field_t : identifier;
           target_field_t : identifier;
        begin
           for field_no in 1..expected_fields loop
              for j in 1..identifiers_top-1 loop
                  if identifiers( j ).field_of = recType then
                     if integer'value( to_string( identifiers( j ).value.all )) = field_no then
                        -- find source field
                        sourceFieldName := identifiers( j ).name;
                        sourceFieldName := delete( sourceFieldName, 1, index( sourceFieldName, "." ) );
                        sourceFieldName := identifiers( second_record_id ).name & "." & sourceFieldName;
                        findIdent( sourceFieldName, source_field_t );
                        if source_field_t = eof_t then
                           err( contextNotes => pl( "At " & gnat.source_info.source_location &
                                   " while assigning a value" ),
                                subjectNotes => subjectInterpreter,
                                reason => +"had an internal error because",
                                obstructorNotes => +"it cound not find source record field " &
                                   unb_em( sourceFieldName )
                           );
                           exit;
                        end if;
                        -- find target field
                        targetFieldName := identifiers( j ).name;
                        targetFieldName := delete( targetFieldName, 1, index( targetFieldName, "." ) );
                        targetFieldName := identifiers( id ).name & "." & targetFieldName;
                        findIdent( targetFieldName, target_field_t );
                        if target_field_t = eof_t then
                           err( contextNotes => pl( "At " & gnat.source_info.source_location &
                                   " while assigning a value" ),
                                subjectNotes => subjectInterpreter,
                                reason => +"had an internal error because",
                                obstructorNotes => +"it cound not find target record field " &
                                   unb_em( targetFieldName )
                           );
                           exit;
                        end if;
                        -- copy it
                        identifiers( target_field_t ).value.all := identifiers( source_field_t ).value.all;
                        if trace then
                          put_trace(
                            to_string( targetFieldName ) & " := " &
                            toSecureData( to_string( toEscaped( identifiers( target_field_t ).value.all ) ) ) );
                        end if;
                     end if; -- right number
                  end if; -- field member
              end loop; -- search loop
           end loop; -- fields
        end;
     end if;
  end if;
end ParseRecordAssignPart;


-----------------------------------------------------------------------------
--  PARSE RECORD DECLARATION
--
-- Handle a record variable declaration, including default values or renaming.
-- Syntax: rec-declaration = " := record_assign"
-- Syntax: rec-declaration renames canonical-rec
-----------------------------------------------------------------------------

procedure ParseRecordDeclaration( id : identifier; recType : identifier; canAssign : boolean := true ) is
  -- Syntax: rec-declaration = " := record_assign"
  -- Syntax: rec-declaration renames canonical-rec
  canonicalRef : renamingReference;
  numFields    : natural;
  baseRecType  : identifier;
  j            : identifier;
begin
  identifiers( id ).kind := recType;

  -- Declare the record's fields.  This must be done whether syntax checking
  -- or running for real.

  if not error_found then

     -- Determine the number of fields for the record, as stored in the record
     -- type's value.all (that is, svalue).  If recType is a derived type, get
     -- the base type which contains the number of fields because it is not
     -- stored in the value of the derived type.
     --
     -- TODO: perhaps the svalue SHOULD be copied by the declaration...but then
     -- it must also create the field identifiers as well...

     baseRecType := getBaseType( recType );
     begin
       numFields := natural'value( to_string( identifiers( baseRecType ).value.all ) );
     exception when constraint_error =>
       err( contextNotes => pl( "At " & gnat.source_info.source_location &
               " while assigning a value" ),
            subjectNotes => subjectInterpreter,
            reason => +"had an internal error because it was unable to determine number of fields in record type " &
               name_em( recType ) & pl( " for " ),
            obstructor => id
       );
       numFields := 0;
     end;

  -- Change variable into an record
  -- Fill record value with ASCII.NUL delimited fields
  --
  -- To do this, search for the i-th field in the formal record declaration
  -- (the identifier value for the field has the field number).  The field name
  -- contains the full dot qualified name.  Get the base field name by removing
  -- everything except the name after the final dot.  Then prefix the name of
  -- the record being declared (so that "rec_type.f" becomes "my_rec.f").

     j := baseRecType + 1;
     for i in 1..numFields loop

            -- brutal search was...
            -- for j in 1..identifiers_top-1 loop
            --
            -- As an optimization, the fields are likely located immediately
            -- after the record itself is defined.  Also assumes they are
            -- stored sequentially.  In the future, records will be stored
            -- differently: as a precaution a while loop was used.

            while j < identifiers_top loop
              if identifiers( j ).field_of = baseRecType then
                 if integer'value( to_string( identifiers( j ).value.all )) = i then
                    exit;
                 end if;
              end if;
              -- should not run: this is a precaution
              j := identifier( integer( j ) + 1 );
            end loop;

            -- no more identifiers means we didn't find it.
            if j = identifiers_top then
               err( contextNotes => pl( "At " & gnat.source_info.source_location &
                    " while declaring the record" ),
                    subjectNotes => subjectInterpreter,
                    reason => pl( "had an internal error because the record field" & i'img & "was not found" ),
                    obstructorNotes => nullMessageStrings
               );
               exit;
            end if;

            -- Given the fields in the record type, create fields for the
            -- record variable by copying the properties of the type.

            declare
               fieldName   : unbounded_string;
               dont_care_t : identifier;
               dotPos      : natural;
            begin
               -- construct the record field name
               fieldName := identifiers( j ).name;
               dotPos := length( fieldName );
               while dotPos > 1 loop
                  exit when element( fieldName, dotPos ) = '.';
                  dotPos := dotPos - 1;
               end loop;
               fieldName := delete( fieldName, 1, dotPos );
               fieldName := identifiers( id ).name & "." & fieldName;
               -- create the variable
               declareIdent( dont_care_t, fieldName, identifiers( j ).kind, varClass );
               identifiers( dont_care_t ).declaredAt := getLineNo;
               identifiers( dont_care_t ).declaredFile := getSourceFileName;
               -- fields have not been marked as children of the parent
               -- record.  However, to make sure the record is used, it
               -- is convenient to track the field.
               identifiers( dont_care_t ).field_of := id;
               -- apply abstract and limited
               identifiers( dont_care_t ).usage := identifiers( j ).usage;
               -- CONST SPECS
               -- By default, constant record fields are specifications:
               -- They have not been assigned a value yet and cannot be
               -- used.
               if identifiers( dont_care_t ).usage = constantUsage then
                  identifiers( dont_care_t ).specFile := getSourceFileName;
                  identifiers( dont_care_t ).specAt := getLineNo;
               end if;
               -- at least, for now, don't worry if record fields are
               -- declared but not accessed.  We'll just check the
               -- main record identifier.
               if syntax_check and then not error_found then
                  identifiers( dont_care_t ).wasReferenced := true;
                  identifiers( dont_care_t ).wasWritten := true;
                  identifiers( dont_care_t ).wasFactor := true;
               end if;
            end;
         j := identifier( integer( j ) + 1 );
     end loop;
  end if;

  -- Renames clause
  -- if it appears, one can only rename...cannot assign.

  if token = renames_t then
     -- Full Record Renaming
     ParseRenamesPart( canonicalRef, id, recType );
     FixRenamedRecordFields( canonicalRef, id );

  -- Assignment when declaring a record variable

  elsif token = symbol_t and identifiers( token ).value.all = ":=" then
     if canAssign then
        ParseRecordAssignPart( id, recType );
     -- elsif identifiers( id ).usage = constantUsage then
        -- CONST SPECS
        -- if it is a constant record and there was no assignment, the full
        -- record variable is a specification.
     --    identifiers( id ).specFile := getSourceFileName;
     --    identifiers( id ).specAt := getLineNo;
     end if;

  -- Parenthesis?  It looks like a Generic type.  Show an error.

  elsif token = symbol_t and identifiers( token ).svalue = "(" then
     err( contextNotes => pl( "in record declaration for " &
             to_string( identifiers( id ).name ) ),
          subject => recType,
          subjectType => identifiers( recType ).kind,
          reason => +"is a concrete type but",
          obstructorNotes => pl( "has parameters for a generic type" ),
          seeAlso => docTypeDecl
     );

  -- No Assignment?  If the new record variable is a constant, than
  -- it's a constant specification.

  elsif identifiers( id ).usage = constantUsage then
        -- CONST SPECS
        -- if it is a constant record and there was no assignment, the full
        -- record variable is a specification.
        identifiers( id ).specFile := getSourceFileName;
        identifiers( id ).specAt := getLineNo;
  end if;

end ParseRecordDeclaration;


-----------------------------------------------------------------------------
--  PARSE EXCEPTION DECLARATION PART
--
-- Handle exception declaration and declare the exception.
-- Syntax: exception-declaration-part := exception [ with msg [use status] ]
-- Run by ParseDeclarationPart
-----------------------------------------------------------------------------

procedure ParseExceptionDeclarationPart( id : in out identifier ) is
  var_name : unbounded_string;
  default_message : unbounded_string;
  exception_status : unbounded_string;
  exception_status_code : anExceptionStatusCode := 1;
  messageType : identifier;
  statusType  : identifier;
begin
   expect( exception_t );
   var_name := identifiers( id ).name;                 -- remember name
   discardUnusedIdentifier( id );                      -- discard variable
   if token = with_t then
      expectAdaScript( contextNotes => +"in the exception declaration", subject => with_t );
      if token = use_t then
         err( contextNotes => +"in the exception declaration",
              subjectNotes => em( "the exception message" ),
              reason => em( "is missing after the with" ),
              obstructorNotes => nullMessageStrings,
              seeAlso => docSubprograms
         );
      end if;
      ParseExpression( default_message, messageType );
      if type_checks_done or else uniTypesOK( messageType, uni_string_t ) then
         expect( use_t );
         ParseExpression( exception_status, statusType );
         if type_checks_done or else baseTypesOK( statusType, natural_t ) then
            null;
         end if;
      end if;
      -- expression value has no meaning except as run-time
      if isExecutingCommand then
         begin
           exception_status_code := anExceptionStatusCode'value( to_string( exception_status ) );
         exception when others =>
           err( contextNotes => +"in the exception declaration",
                subjectNotes => pl( "the exception status code " ) &
                   unb_em( trim( exception_status, ada.strings.both ) ),
                reason => pl( "is not in the range" ),
                obstructorNotes => em( "0..255" ),
                seeAlso => docSubprograms
           );
         end;
      end if;
   elsif token = renames_t then
      err( contextNotes => +"in the exception declaration",
           subjectNotes => pl( "an exception" ),
           reason => em( "cannot be renamed" ),
           obstructorNotes => nullMessageStrings,
           seeAlso => docSubprograms
      );
   elsif token = copies_t then
      -- TODO: we could make this happen
      featureNotYetImplemented( subjectNotes => "copying an exception",
         remedy => "declare the second exception separately" );
   elsif token /= symbol_t and identifiers( token ).value.all /= ";" then
      err( contextNotes => +"in the exception declaration",
           subjectNotes => em( "with or ;" ),
           reason => +"is expected",
           obstructorNotes => nullMessageStrings,
           seeAlso => docSubprograms
      );
   end if;
   -- Do not declare the exception if an error occurred while defining it or
   -- the details may be incorrect.
   if not error_found then
      findException( var_name, id );
      if id = eof_t then
         declareException( id, var_name, default_message, exception_status_code ); -- declare var
         identifiers( id ).declaredAt := getLineNo;
         identifiers( id ).declaredFile := getSourceFileName;
      else
         err( contextNotes => +"in the exception declaration",
              subject => id,
              subjectType => identifiers( id ).kind,
              subjectLocation => pl( to_string( identifiers( id ).specFile) & ":" &
                          identifiers( id ).specAt'img ),
              reason => em( "is already declared in a greater scope" ),
              obstructorNotes => nullMessageStrings,
              remedy => +"make exception names unique so that errors are not confusing",
              seeAlso => docSubprograms
         );
      end if;
   end if;
end ParseExceptionDeclarationPart;


-----------------------------------------------------------------------------
--  CHECK GENERIC PARAMETER TYPE
--
-- Type checks for the generic parameters
-- Currently there are no user-defined generic types: they are all built-in
-- types which are hard-coded here.
--
-- TODO: As a temporary situation, the generic type checks are hard-
-- coded here.  There is no field in an identifier to set the number
-- of expected parameters to a generic type.
--
-- TODO: I am permitting subtypes of generic types, but there's no
-- function currently in the scanner to track down type derived type of
-- generic type.  If I allowed new types from a generic type, the
-- hard-coded functionality will break.
-----------------------------------------------------------------------------

procedure CheckGenericParameterType( id, type_token : identifier ) is

  procedure err_array_element( genTypeId, elementTypeId : identifier ) is
  begin
    err(
       contextNotes => +"While checking the parameters of " &
          name_em( genTypeId ),
       subject => elementTypeId,
       reason => +"is an array type but",
       obstructorNotes => +"elements should be scalar",
       remedy => +"convert it to a JSON string as a workaround until support for arrays and records is written"
   );
  end err_array_element;

  procedure err_record_element( genTypeId, elementTypeId : identifier ) is
  begin
    err(
       contextNotes => +"While checking the parameters of " &
          name_em( genTypeId ),
       subject => elementTypeId,
       reason => +"is a record type but",
       obstructorNotes => +"elements should be scalar",
       remedy => +"convert it to a JSON string as a workaround until support for arrays and records is written"
   );
  end err_record_element;

  procedure err_one_element_not_two( id, type_token : identifier ) is
  begin
     err( contextNotes => +"While checking the generic parameters for " &
             name_em( id ),
          subject => type_token,
          reason => +"was given two element types but",
          obstructorNotes => em( "has only one type" )
     );
  end err_one_element_not_two;

  procedure err_two_elements_not_one( id, type_token : identifier ) is
  begin
     err( contextNotes => +"While checking the generic parameters for " &
              name_em( id ),
          subject => type_token,
          reason => +"was given one element type but",
          obstructorNotes => em( "has two types" )
      );
  end err_two_elements_not_one;

  procedure err_key_is_array( id, genKindId : identifier ) is
  begin
      err( contextNotes => +"While checking the key type for " &
              name_em( id ),
           subject => genKindId,
           reason => +"is an array but",
           obstructorNotes => em( "should be a discrete scalar type" )
      );
  end err_key_is_array;

  procedure err_key_is_record( id, genKindId : identifier ) is
  begin
      err( contextNotes => +"While checking the key type for " &
              name_em( id ),
           subject => genKindId,
           reason => +"is a record but",
           obstructorNotes => em( "should be a discrete scalar type" )
      );
  end err_key_is_record;

  uniType  : constant identifier := getUniType( type_token );
begin
   -- Treat the generic type parameters as used types
   identifiers( identifiers( id ).genKind ).wasApplied := true;
   identifiers( identifiers( id ).genKind2 ).wasApplied := true;
   if uniType = doubly_list_t then
      if identifiers( id ).genKind2 /= eof_t then
         err_one_element_not_two( id, type_token );
      else
         declare
            genKindId : identifier renames identifiers( id ).genKind;
         begin
            if class_ok( genKindId, typeClass, subClass ) then
               if identifiers( genKindId ).list then
                  err_array_element( uniType, genKindId );
               elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
                  err_record_element( uniType, genKindId );
               end if;
            end if;
         end;
      end if;
   elsif uniType = doubly_cursor_t then
      if identifiers( id ).genKind2 /= eof_t then
         err_one_element_not_two( id, type_token );
      end if;
   elsif uniType = btree_file_t then
      if identifiers( id ).genKind2 /= eof_t then
         err_one_element_not_two( id, type_token );
      end if;
   elsif uniType = btree_cursor_t then
      if identifiers( id ).genKind2 /= eof_t then
         err_one_element_not_two( id, type_token );
      end if;
   elsif uniType = hash_file_t then
      if identifiers( id ).genKind2 /= eof_t then
         err_one_element_not_two( id, type_token );
      end if;
   elsif uniType = hash_cursor_t then
      if identifiers( id ).genKind2 /= eof_t then
         err_one_element_not_two( id, type_token );
      end if;
   elsif uniType = dht_table_t then
      declare
         genKindId : identifier renames identifiers( id ).genKind;
      begin
         if class_ok( genKindId, typeClass, subClass ) then
            if identifiers( genKindId ).list then
               err_array_element( uniType, genKindId );
            elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
               err_record_element( uniType, genKindId );
            end if;
         end if;
      end;
   elsif uniType = vectors_vector_t then
      -- vector index must be scalar
      -- TODO
      declare
         genKindId : identifier renames identifiers( id ).genKind;
      begin
         if class_ok( genKindId, typeClass, subClass ) then
            if identifiers( genKindId ).list then
               err_key_is_array( id, genKindId );
            elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
               err_key_is_record( id, genKindId );
            elsif genKindId = natural_t or
               genKindId = positive_t or
               genKindId = natural_t or
               genKindId = integer_t then
               -- short_short_integer_t, short_integer_t, long_integer_t
               -- long_long_integer_t
               null;
             -- TODO: enumerated
            elsif getUniType( genKindId ) = root_enumerated_t then
               null;
            else
               err( contextNotes => +"While checking the key type for " &
                    name_em( id ),
                    subject => genKindId,
                    reason => em( "should be a discrete scalar type" ),
                    obstructorNotes => nullMessageStrings
               );
            end if;
         end if;
      end;
      -- vector values (for now) must be scalar
      if identifiers( id ).genKind2 = eof_t then
         err_two_elements_not_one( id, type_token );
      else
         declare
            genKindId : identifier renames identifiers( id ).genKind2;
         begin
            if class_ok( genKindId, typeClass, subClass ) then
               if identifiers( genKindId ).list then
                  err_array_element( uniType, genKindId );
               elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
                  err_record_element( unitype, genKindId );
               end if;
            end if;
         end;
      end if;
   elsif uniType = vectors_cursor_t then
      -- vector cursor index must be scalar
      -- TODO: THIS IS NOT FINISHED
      declare
         genKindId : identifier renames identifiers( id ).genKind;
      begin
         if class_ok( genKindId, typeClass, subClass ) then
            if identifiers( genKindId ).list then
               err_key_is_array( id, genKindId );
            elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
               err_key_is_record( id, genKindId );
            elsif genKindId = natural_t or
               genKindId = positive_t or
               genKindId = natural_t or
               genKindId = integer_t then
               -- short_short_integer_t, short_integer_t, long_integer_t
               -- long_long_integer_t
               null;
             -- TODO: enumerated
            elsif getUniType( genKindId ) = root_enumerated_t then
               null;
            else
               err( contextNotes => +"While checking the key type for " &
                    name_em( id ),
                    subject => genKindId,
                    reason => em( "should be a discrete scalar type" ),
                    obstructorNotes => nullMessageStrings
               );
            end if;
         end if;
      end;
      -- vector cursor values (for now) must be scalar
      if identifiers( id ).genKind2 = eof_t then
         err_two_elements_not_one( id, type_token );
      else
         declare
            genKindId : identifier renames identifiers( id ).genKind2;
         begin
            if class_ok( genKindId, typeClass, subClass ) then
               if identifiers( genKindId ).list then
                  err_array_element( uniType, genKindId );
               elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
                  err_record_element( uniType, genKindId );
               end if;
            end if;
         end;
      end if;
   elsif uniType = hashed_maps_map_t then
      -- hashed map key) must be scalar
      declare
         genKindId : identifier renames identifiers( id ).genKind;
      begin
         if class_ok( genKindId, typeClass, subClass ) then
            if identifiers( genKindId ).list then
               err_key_is_array( id, genKindId );
            elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
               err_key_is_record( id, genKindId );
            end if;
         end if;
      end;
      -- hashed map values (for now) must be scalar
      if identifiers( id ).genKind2 = eof_t then
         err_two_elements_not_one( id, type_token );
      else
         declare
            genKindId : identifier renames identifiers( id ).genKind2;
         begin
            if class_ok( genKindId, typeClass, subClass ) then
               if identifiers( genKindId ).list then
                  err_array_element( uniType, genKindId );
               elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
                  err_record_element( uniType, genKindId );
               end if;
            end if;
         end;
      end if;
   elsif uniType = hashed_maps_cursor_t then
      -- hashed map key) must be scalar
      declare
         genKindId : identifier renames identifiers( id ).genKind;
      begin
         if class_ok( genKindId, typeClass, subClass ) then
            if identifiers( genKindId ).list then
               err_key_is_array( id, genKindId );
            elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
               err_key_is_record( id, genKindId );
            end if;
         end if;
      end;
      -- hashed map values (for now) must be scalar
      if identifiers( id ).genKind2 = eof_t then
         err_two_elements_not_one( id, type_token );
      else
         declare
            genKindId : identifier renames identifiers( id ).genKind2;
         begin
            if class_ok( genKindId, typeClass, subClass ) then
               if identifiers( genKindId ).list then
                  err_array_element( uniType, genKindId );
               elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
                  err_record_element( uniType, genKindId );
               end if;
            end if;
         end;
      end if;
   else
     -- TODO: implement generic types
     featureNotYetImplemented( subjectNotes => "non-generic parameterized types",
        remedy => "" );
   end if; -- base types
end CheckGenericParameterType;


-----------------------------------------------------------------------------
--  PARSE DECLARATION PART
--
-- This is the umbrella function to handle finish almost any kind of
-- declaration.  Parse the colon and the rest of the declaration.  The
-- more complex declarations such as arrays and records are handled in
-- other "part" functions.
-- Assigns type of identifier and value (if assignment part)
-- Syntax: declaration-part = " : [aliased|constant] ident assign-part"
-- Syntax: declaration-part = " : anonymous-array
-- Syntax: declaration-part = " : array-declaration
-- Syntax: declaration-part = " : record-declaration
-- Syntax: declaration-part = " : exception [with message use status]
-- Syntax: declaration-part = " : renames x
-- Syntax: declaration-part = " : copies x
-- Note: in some cases, the variable id may change.
-- TODO: THIS PROCEDURE IS STILL TOO LONG AND SHOULD BE BROKEN DOWN EVEN MORE
-----------------------------------------------------------------------------

procedure ParseDeclarationPart( id : in out identifier; anon_arrays : boolean; exceptions : boolean ) is


  --  ATTACH GENERIC PARAMETER RESOURCE
  --
  -- This sub-procedure declares resources for storing generic types.  Currently
  -- there are no user-defined generic types: they are built-in and hard-coded.
  -- TODO: getStorageType
  ---------------------------------------------------------------------------

  procedure AttachGenericParameterResource( id, type_token : identifier ) is
     uniType   : constant identifier := getUniType( type_token );
     resId     : resHandleId;
  begin
     if uniType = doubly_list_t then
        declareResource( resId, doubly_linked_string_list, getIdentifierBlock( id ) );
     elsif uniType = doubly_cursor_t then
        declareResource( resId, doubly_linked_string_list_cursor, getIdentifierBlock( id ) );
     elsif uniType = btree_file_t then
        declareResource( resId, btree_file, getIdentifierBlock( id ) );
     elsif uniType = btree_cursor_t then
        declareResource( resId, btree_cursor, getIdentifierBlock( id ) );
     elsif uniType = hash_file_t then
        declareResource( resId, hash_file, getIdentifierBlock( id ) );
     elsif uniType = hash_cursor_t then
        declareResource( resId, hash_cursor, getIdentifierBlock( id ) );
     elsif uniType = dht_table_t then
        declareResource( resId, dynamic_string_hash_table, getIdentifierBlock( id ) );
     elsif uniType = vectors_vector_t then
        declareResource( resId, vector_string_list, getIdentifierBlock( id ) );
     elsif uniType = vectors_cursor_t then
        declareResource( resId, vector_string_list_cursor, getIdentifierBlock( id ) );
     elsif uniType = hashed_maps_map_t then
        declareResource( resId, string_hashed_map, getIdentifierBlock( id ) );
     elsif uniType = hashed_maps_cursor_t then
        declareResource( resId, string_hashed_map_cursor, getIdentifierBlock( id ) );
     else
        -- TODO: implement generic types
        err( contextNotes => pl( "declaring " ) & unb_em( identifiers( id ).name ),
             subject => type_token,
             subjectType => identifiers( type_token ).kind,
             reason => +"is a concrete type derived from",
             obstructor => uniType,
             remedy => +"user generic types are not yet implemented",
             seeAlso => docTypeDecl
        );
     end if;
     if isExecutingCommand then
        identifiers( id ).svalue := to_unbounded_string( resId );
        identifiers( id ).value := identifiers( id ).svalue'access;
        identifiers( id ).resource := true;
     end if;
  end AttachGenericParameterResource;


  --  VERIFY CONSTANT SPEC
  --
  -- When completing a constant specification (i.e. a forward constant),
  -- this function confirms parses the complete constant and checks that it
  -- conforms to the earlier specification.
  ---------------------------------------------------------------------------

  procedure VerifyConstantSpec( const_id : identifier ) is
    -- : constant type assign-part
    -- CONST SPECS
    type_token    : identifier;
    right_type    : identifier;
    expr_value    : unbounded_string;
    new_const_id  : identifier;
    oldSpec       : declaration;

    -- Verify that the type is the same as the previous declaration.
    -- TODO: test anonymous types

    procedure VerifyTypesAreSame is
    begin
       if identifiers( const_id ).kind /= type_token then
          err( contextNotes => +"verifying the constant specification",
               subject => const_id,
               subjectType => identifiers( const_id ).kind,
               subjectLocation => pl( to_string( identifiers( const_id ).specFile) & ":" &
          identifiers( const_id ).specAt'img ),
               reason => +"has a different type from this completed constant with type",
               obstructor => type_token
          );
       end if;
   end VerifyTypesAreSame;

  begin
    --put_line("VERIFY CONST SPEC: " & to_string( identifiers( const_id ).name ) ); --DEBUG

    -- This should not happen.
    --
    -- If the constant specification is at a different nesting
    -- level, it's the declaration of a new constant.A

    if not isLocal( const_id ) then
       err( contextNotes => pl( "At " & gnat.source_info.source_location &
                    " while fulfilling the constant specification" ),
            subject => const_id,
            subjectType => identifiers( const_id ).kind,
            subjectLocation => pl( to_string( identifiers( const_id ).specFile) & ":" &
                identifiers( const_id ).specAt'img ),
            reason => pl( "had an internal error because the specification is not in" ),
            obstructorNotes => pl( "the local scope" )
       );
    end if;

    expect( symbol_t, ":" );

    --  Get the type.
    --
    -- The variable must be a constant.  If it is not a constant, then the
    -- type must be constant usage.  If the specification type and the
    -- fulfillment type are not the same, there's no point in checking the
    -- type for constant usage as it may be the wrong type anyway.

    if token = constant_t then
       expect( constant_t );
       ParseIdentifier( type_token );                    -- identify type
       VerifyTypesAreSame;
    else
       ParseIdentifier( type_token );                    -- identify type
       VerifyTypesAreSame;
       -- This may be impossible.  If it's not a constant, it will
       -- be identifier not declared or a similar error before we
       -- get here.
       if identifiers( type_token ).usage /= constantUsage then
          err( contextNotes => +"fulfilling the constant specification",
               subject => const_id,
               subjectType => identifiers( const_id ).kind,
               subjectLocation => pl( to_string( identifiers( const_id ).specFile) & ":" &
                  identifiers( const_id ).specAt'img ),
               reason => pl( "requires a variable or type of constant usage but has type" ),
               obstructor => type_token
          );
       end if;
    end if;

    -- The type has been applied to make a variable.  Mark it as used.

    if syntax_check then                              -- mark that type was
       identifiers( type_token ).wasApplied := true;  -- used
    end if;

    -- The Tricky Part

    -- Temporarily destroy identifer so that i : constant integer := i
    -- isn't circular.  Set avalue to null to prevent releasing storage
    -- pointed to by oldSpec.  Backup the entire entry.
    --  For records, which are currently a collection of variables,
    -- it is not possible to destroy the parent record variable.  If we
    -- did, recreating it may change its position in the symbol table,
    -- breaking the linkage to its fields.  Also, the value is the number
    -- of fields and we don't want to clear and lose that.

    oldSpec := identifiers( const_id );

    -- unlike a regular declaration, the constant specification id exists
    -- and has a type (not new_t )

    if getUniType( oldSpec.kind ) /= root_record_t then
       identifiers( const_id ).avalue := null;
       identifiers( const_id ).kind := new_t;           -- make it discardable
       discardUnusedIdentifier( const_id );             -- discard variable
    end if;

    -- Calculate the assignment (i.e. using any previous variable i)
    -- if not an aggregate.  The assign part is done here while the variable
    -- does not exist.
    --   For a record, this will erase the number of fields in the record's
    -- parent variable.

    if not oldSpec.list and getUniType( oldSpec.kind ) /= root_record_t then
--put_line( "verify: AssignPart should not run for aggregates"); -- DEBUG
       ParseAssignPart( expr_value, right_type );          -- do := part
       baseTypesOK( type_token, right_type );
    end if;

    -- Redeclare temporarily destroyed identifier (i.e. declare new i)
    -- and recover old properties.  Clear the spec and fix the avalue
    -- (if necessary).  The spec is now fulfilled.
    --  For records, we didn't destroy the original.

    if getUniType( oldSpec.kind ) /= root_record_t then
--put_line( "verify: New ident" ); -- DEBUG
       declareIdent( new_const_id, oldSpec.name, type_token, varClass );
       identifiers( new_const_id ).declaredAt := getLineNo;
       identifiers( new_const_id ).declaredFile := getSourceFileName;
    else
--put_line( "verify: No new ident" ); -- DEBUG
       new_const_id := const_id;
    end if;
    identifiers( new_const_id ) := oldSpec;
    identifiers( new_const_id ).specAt := noSpec;

    -- For a record, mark it as used if it's a constant
    identifiers( new_const_id ).wasReferenced := true;

    -- Aggregate assignments
    --
    -- TODO: the recursion problem exists here, where defining x but x may be
    -- in the assignment list.

    if identifiers( new_const_id ).list then
--put_line( "verify: array assign part" ); -- DEBUG
       ParseArrayAssignPart( new_const_id );
    elsif getUniType( identifiers( new_const_id ).kind ) = root_record_t then
--put_line( "verify: record assign part" ); -- DEBUG
       ParseRecordAssignPart( new_const_id, type_token );
    end if;

    -- mark the type that was targeted by the cast
    if syntax_check then
       identifiers( type_token ).wasCastTo := true;
    end if;
    if isExecutingCommand then
       if trace then
          put_trace( "Completing constant specification for " & to_string( oldSpec.name ) );
       end if;
       expr_value := castToType( expr_value, type_token );
       -- Contracts only run on scalars currently
       if not oldSpec.list and getUniType( oldSpec.kind ) /= root_record_t then
          if type_token /= right_type then
             DoContracts( identifiers( new_const_id ).kind, expr_value );
          end if;
       end if;
       identifiers( new_const_id ).value.all := expr_value;
       if trace then
           put_trace(
              to_string( identifiers( new_const_id ).name ) & " := """ &
              toSecureData( to_string( ToEscaped( expr_value ) ) ) & """" );
       end if;
    end if;
  end VerifyConstantSpec;

  type_token    : identifier;
  expr_value    : unbounded_string;
  right_type    : identifier;
  expr_expected : boolean := false;
  canonicalRef : renamingReference;
begin
  -- CONST SPECS

  -- If it's a specification, verify and fulfill it.

  if identifiers( id ).specAt /= noSpec then
        VerifyConstantSpec( id );
        return;
  end if;

  expect( symbol_t, ":" );

  -- Overriding

  --if syntax_check then
  --   if token /= overriding_t then
  --      if identifiers( id ).kind /= new_t then
  --         err( optional_yellow( "overriding" ) &
  --              " expected because " &
  --              optional_yellow( to_string( identifiers( id ).name ) ) &
  --              " exists at a different scope" );
  --      end if;
  --   else
  --      if identifiers( id ).kind = new_t then
  --         err( optional_yellow( "overriding" ) &
  --              " not expected because " &
  --              optional_yellow( to_string( identifiers( id ).name ) ) &
  --              " does not exist at a different scope" );
  --      end if;
  --   end if;
  --end if;
  --if token = overriding_t then
  --   getNextToken;
  --end if;

  -- Exceptions

  if token = exception_t then                          -- handle exception
     if not exceptions then                            --  not permitted?
        err( contextNotes => pl( "in declaration for " &
                to_string( identifiers( id ).name ) ),
             subject => token,
             reason => +"is not allowed",
             obstructorNotes => nullMessageStrings
        );
     else
        ParseExceptionDeclarationPart( id );
     end if;
     return; -- nothing more to do
  end if;

  -- Check for constant, limited qualifiers

  ParseVarUsageQualifiers( id, expr_expected );

  -- Anonymous Array?  Handled elsewhere.

  -- TODO: sort out limit...is it on types, variables or both.  use it
  -- consistently.

  if token = array_t then                              -- anonymous array?
     if not anon_arrays then
        err( contextNotes => pl( "in declaration for " &
                to_string( identifiers( id ).name ) ),
             subjectNotes => +"anonymous arrays",
             reason => +"are not allowed",
             obstructorNotes => nullMessageStrings
        );
     end if;
     ParseAnonymousArray( id, identifiers( id ).usage = limitedUsage );  -- handle it
     return;                                           --  and nothing more
  end if;

  --  Get the type.

  ParseIdentifier( type_token );                            -- identify type
  if syntax_check then                                 -- mark that type was
     identifiers( type_token ).wasApplied := true;     -- used
  end if;

  -- Variable vs. Type Qualifiers
  --
  -- If the variable has an explicit qualifier, it will have been applied
  -- above.  So if it's full usage, check the type and inherit any qualifier
  -- from the type.  The variable is allowed to be more constrained than
  -- the type, but it must not reduce constraint.

  case identifiers( id ).usage is
  when fullUsage =>
        case identifiers( type_token ).usage is
        when fullUsage =>
           null;
        when constantUsage =>
           identifiers( id ).usage := constantUsage;
        when limitedUsage =>
           identifiers( id ).usage := limitedUsage;
        when abstractUsage =>
           err( contextNotes => pl( "in declaration for " &
                   to_string( identifiers( id ).name ) ),
                subject => type_token,
                reason => +"cannot be used to declare variables because the usage is",
                obstructorNotes => em( "abstract" )
           );
        when others =>
           err( contextNotes => pl( "At " & gnat.source_info.source_location &
                    " while declaring the variable" ),
                subject => id,
                subjectType => identifiers( id ).kind,
                reason => pl( "had an internal error because of unexpected var qualifier" ),
                obstructorNotes => em( identifiers( type_token ).usage'img )
           );
        end case;
  when constantUsage =>
       if identifiers( type_token ).usage = limitedUsage then
          err( contextNotes => pl( "in declaration for " &
                  to_string( identifiers( id ).name ) ),
               subjectNotes =>  unb_em( identifiers( type_token ).name ) & pl( " with " ) &
                  em( "limited" ) & pl( " usage qualifier" ),
               reason => +"cannot be assigned to less restrictive",
               obstructorNotes => em( "constant" ) & pl( " usage qualifer" )
          );
          -- TODO: should abstract be allowed for constants?
       end if;
  when limitedUsage =>
       null; -- this is the most constrained
  when abstractUsage =>
       err( contextNotes => pl( "At " & gnat.source_info.source_location &
                " while declaring the variable" ),
            subject => id,
            subjectType => identifiers( id ).kind,
            reason => pl( "had an internal error because of abstract type qualifier" ),
            obstructorNotes => em( identifiers( type_token ).usage'img )
       );
  when others =>
       err( contextNotes => pl( "At " & gnat.source_info.source_location &
                " while declaring the variable" ),
            subject => id,
            subjectType => identifiers( id ).kind,
            reason => pl( "had an internal error because of unexpected type qualifier" ),
            obstructorNotes => em( identifiers( type_token ).usage'img )
       );
  end case;

  if token = private_t then                             -- private access?
     featureNotYetImplemented( subjectNotes => "private access modifer",
        remedy => "" );
  end if;

  -- Array type?  Handled elsewhere.

  if identifiers( getBaseType( type_token ) ).list then       -- array type?
     if not anon_arrays then
        featureNotYetImplemented( subjectNotes => "nested arrays",
           remedy => "encode a dimension to a JSON string as a workaround until support is written" );
     else
        ParseArrayDeclaration( id, type_token );                -- handle it
     end if;
     return;                                            --  and nothing more
  end if;

  -- Record type?  Handled elsewhere.

  if identifiers( getBaseType( type_token ) ).kind = root_record_t then  -- record type?
     if not anon_arrays then
        featureNotYetImplemented( subjectNotes => "nested records",
           remedy => "encode a record to a JSON string as a workaround until support is written" );
     --elsif identifiers( type_token ).usage = abstractUsage then
     --   err( "constants and variables cannot be declared as " &
     --     optional_yellow( to_string( identifiers( type_token ).name ) ) &
     --     " because it is " & optional_yellow( "abstract" ) );
     else
        ParseRecordDeclaration( id, type_token );          -- handle it
     end if;
     return;                                           --  and nothing more
  end if;

  -- Not an array or record?
  -- Verify that the type token is a type and check for types
  -- not allowed with certain pragmas.
  -- We cannot use type_checks_done here unless we restructure because of
  -- expr_expected.

  if not type_checks_done then
     if not class_ok( type_token, typeClass, subClass, genericTypeClass ) then
        null;
     elsif onlyAda95 and (type_token = uni_string_t or type_token =
        uni_numeric_t or type_token = universal_t) then
        expectAdaScript( subject => type_token,
           remedy => pl( "it is a universal type" ) );
     elsif getBaseType( type_token ) = command_t then
        if onlyAda95 then
           expectAdaScript( subject => type_token,
             remedy => pl( "it is a command derived type" ) );
        -- Special case: command type qualifiers
        elsif identifiers( id ).usage /= limitedUsage and
           identifiers( id ).usage /= constantUsage then
           -- I'm assuming only full usage is possible because abstract is
           -- handled elsewhere
           err(  contextNotes => +"declaring the variable",
                 subject => id,
                 reason => pl( "is a command type with no" ) &
                   pl( " usage qualifier but should be restricted to either" ),
                 obstructorNotes => em( "limited or constant" )
           );
        end if;
     end if;
  end if;

  -- An extra call to getBaseType.  Should probably cache.

  if getBaseType( type_token ) = command_t then
      expr_expected := true;
  end if;

  -- Generic Parameters
  --
  -- These only apply to built-in types and they are not arrays or records.
  -- It excludes assignment and renaming (only because renaming requires
  -- modification to check the generic parameters).

  if identifiers( type_token ).class = genericTypeClass then
     ParseGenericParametersPart( id, type_token  );
     if not type_checks_done then
        CheckGenericParameterType( id, type_token );
     end if;
     if isExecutingCommand then
        AttachGenericParameterResource( id, type_token );
     end if;
     identifiers( id ).kind := type_token;
     identifiers( id ).usage := identifiers( type_token ).usage;

  elsif token = symbol_t and identifiers( token ).svalue = "(" then
     err( contextNotes => pl( "in declaration for " &
             to_string( identifiers( id ).name ) ),
          subject => type_token,
          subjectType => identifiers( type_token ).kind,
          reason => +"is a concrete type but",
          obstructorNotes => pl( "has parameters for a generic type" ),
          seeAlso => docTypeDecl
     );

   -- We need to attach a resource for the generic-based type
   -- (i.e. type x is generic(...), this will be x)
   -- on an error, the token may not be something with a gen type

  elsif (type_token /= eof_t and not error_found ) and then identifiers( type_token ).genKind /= eof_t then

      if token /= renames_t then
         if isExecutingCommand then
            AttachGenericParameterResource( id, type_token );
         end if;
         identifiers( id ).genKind := identifiers( type_token ).genKind;
         identifiers( id ).genKind2 := identifiers( type_token ).genKind2;
         identifiers( id ).kind := type_token;
      else
        -- identifiers( id ).svalue := to_unbounded_string( resId );
        -- identifiers( id ).value := identifiers( id ).svalue'access;
        -- identifiers( id ).resource := true;
         ParseRenamesPart( canonicalRef, id, type_token );
         identifiers( id ).genKind := identifiers( type_token ).genKind;
         identifiers( id ).genKind2 := identifiers( type_token ).genKind2;
         identifiers( id ).kind := type_token;
      end if;

  -- Renames clause
  -- if it appears, one can only rename...cannot assign.

  elsif token = renames_t then

     declare
        originalFieldOf : constant identifier := identifiers( id ).field_of;
        -- TODO: refactor these Booleans
        wasLimited : constant boolean := identifiers( id ).usage = limitedUsage;
        wasConstant : constant boolean := identifiers( id ).usage = constantUsage;
     begin
        -- Variable or Constant renaming
        ParseRenamesPart( canonicalRef, id, type_token );
        -- Prevent a constant from being turned into a variable by a renaming
        -- It must be renamed as a constant or a limited.
        if identifiers( canonicalRef.id).usage = constantUsage and
           not wasConstant and not wasLimited then
           err(  contextNotes => +"declaring the variable " &
                    unb_em( identifiers( id ).name ),
                 subject => canonicalRef.id,
                 subjectType => identifiers( canonicalRef.id ).kind,
                 reason => pl( "is a constant and should be renamed to either" ),
                 obstructorNotes => em( "limited or constant" )
           );
        elsif identifiers( canonicalRef.id ).class = enumClass then
           -- TODO: I could probably get this to work but it's a weird edge
           -- case.
           err(  contextNotes => +"declaring the variable " &
                    unb_em( identifiers( id ).name ),
                 subject => canonicalRef.id,
                 subjectType => identifiers( canonicalRef.id ).kind,
                 reason => pl( "is an item of a enumerated type and it cannot be renamed" ),
                 obstructorNotes => nullMessageStrings
           );
        elsif identifiers( canonicalRef.id ).usage = limitedUsage and not wasLimited then
           err(  contextNotes => +"declaring the variable " &
                    unb_em( identifiers( id ).name ),
                 subject => canonicalRef.id,
                 subjectType => identifiers( canonicalRef.id ).kind,
                 reason => pl( "is a limited and should be renamed to a" ),
                 obstructorNotes => em( "limited" )
           );
        elsif identifiers( canonicalRef.id ).field_of /= eof_t then
           if identifiers( identifiers( canonicalRef.id ).field_of ).usage = limitedUsage and not wasLimited then
              err(  contextNotes => +"declaring the variable " &
                       unb_em( identifiers( id ).name ),
                    subject => canonicalRef.id,
                    subjectType => identifiers( canonicalRef.id ).kind,
                    reason => pl( "is a field of a limited record and should be renamed to a" ),
                    obstructorNotes => em( "limited" )
              );
           end if;
        end if;
        -- If the identifier is a record field, it must refer to the
        -- renaming record, not the canonical record.
        identifiers( id ).field_of := originalFieldOf;
        if wasLimited then
           identifiers( id ).usage := limitedUsage;
        end if;
     end;

     -- Complete the declaration
     identifiers( id ).kind := type_token;

     if identifiers( canonicalRef.id ).list then
        if canonicalRef.hasIndex then
           -- don't do this on an error or an exception may be thrown
           if isExecutingCommand then
              begin
                 identifiers( id ).value := identifiers( canonicalRef.id ).avalue( canonicalRef.index )'access;
              exception when storage_error =>
                 err( contextNotes => pl( "At " & gnat.source_info.source_location &
                         " while declaring the renaming" ),
                      subject => id,
                      subjectType => identifiers( id ).kind,
                      reason => pl( "had an internal error " &
                         "while renaming because of "),
                      obstructorNotes => +"a storage_error exception"
                 );
              when others =>
                 err( contextNotes => pl( "At " & gnat.source_info.source_location &
                         " while declaring the renaming" ),
                      subject => id,
                      subjectType => identifiers( id ).kind,
                      reason => pl( "had an internal error " &
                         "while renaming because of "),
                      obstructorNotes => +"an unexpected exception"
                 );
              end;
           end if;
        end if;
     end if;

  elsif token = copies_t then

     declare
        originalFieldOf : constant identifier := identifiers( id ).field_of;
        -- TODO: refactor these Booleans
        wasLimited : constant boolean := identifiers( id ).usage = limitedUsage;
        wasConstant : constant boolean := identifiers( id ).usage = constantUsage;
     begin
        -- Variable or Constant renaming
        ParseCopiesPart( canonicalRef, id, type_token );
        -- Prevent a constant from being turned into a variable by a renaming
        -- It must be renamed as a constant or a limited.
        if identifiers( canonicalRef.id).usage = constantUsage and
           not wasConstant and not wasLimited then
           err(  contextNotes => +"declaring the variable " &
                    unb_em( identifiers( id ).name ),
                 subject => canonicalRef.id,
                 subjectType => identifiers( canonicalRef.id ).kind,
                 reason => pl( "is a constant and should be copied to either" ),
                 obstructorNotes => em( "limited or constant" )
           );
        elsif identifiers( canonicalRef.id ).class = enumClass then
           -- TODO: I could probably get this to work but it's a weird edge
           -- case.
           err(  contextNotes => +"declaring the variable " &
                    unb_em( identifiers( id ).name ),
                 subject => canonicalRef.id,
                 subjectType => identifiers( canonicalRef.id ).kind,
                 reason => pl( "is an item of a enumerated type and it cannot be copied" ),
                 obstructorNotes => nullMessageStrings
           );
        elsif identifiers( canonicalRef.id ).usage = limitedUsage and not wasLimited then
           err(  contextNotes => +"declaring the variable " &
                    unb_em( identifiers( id ).name ),
                 subject => canonicalRef.id,
                 subjectType => identifiers( canonicalRef.id ).kind,
                 reason => pl( "is a limited should be copied to a" ),
                 obstructorNotes => em( "limited" )
           );
        elsif identifiers( canonicalRef.id ).field_of /= eof_t then
           if identifiers( identifiers( canonicalRef.id ).field_of ).usage = limitedUsage and not wasLimited then
              err(  contextNotes => +"declaring the variable " &
                       unb_em( identifiers( id ).name ),
                    subject => canonicalRef.id,
                    subjectType => identifiers( canonicalRef.id ).kind,
                    reason => pl( "is a field of a limited record and should be copied to a" ),
                    obstructorNotes => em( "limited" )
              );
           end if;
        end if;
        -- If the identifier is a record field, it must refer to the
        -- renaming record, not the canonical record.
        identifiers( id ).field_of := originalFieldOf;
        if wasLimited then
           identifiers( id ).usage := limitedUsage;
        end if;
        identifiers( id ).writtenOn := perfStats.lineCnt;
     end;

     -- Complete the declaration
     identifiers( id ).kind := type_token;

     -- Volatiles hack
     --
     -- Volatiles are a special case:
     -- they cannot be limited but must not be used an an expression because
     -- they can cause side effects.  But copying a volatile requires
     -- a non-limited.  We exempt volatiles here by pretending they were
     -- used in an expression, and SparForte will not require them to be
     -- limited.

     --if syntax_check then
     --   if identifiers( canonicalRef.id ).volatile /= none then
     --      identifiers( canonicalRef.id ).wasFactor := true;
     --   end if;
     --end if;

     if identifiers( canonicalRef.id ).list then
        if canonicalRef.hasIndex then
           -- don't do this on an error or an exception may be thrown
           if isExecutingCommand then
              begin
                 identifiers( id ).value := identifiers( canonicalRef.id ).avalue( canonicalRef.index )'access;
              exception when storage_error =>
                 err( contextNotes => pl( "At " & gnat.source_info.source_location &
                         " while declaring the renaming" ),
                      subject => id,
                      subjectType => identifiers( id ).kind,
                      reason => pl( "had an internal error " &
                         "while renaming because of "),
                      obstructorNotes => +"a storage_error exception"
                 );

              when others =>
                 err( contextNotes => pl( "At " & gnat.source_info.source_location &
                         " while declaring the renaming" ),
                      subject => id,
                      subjectType => identifiers( id ).kind,
                      reason => pl( "had an internal error " &
                         "while renaming because of "),
                      obstructorNotes => +"an unexpected exception"
                 );
              end;
           end if;
        end if;
     end if;

  -- CONST SPECS
  -- constant specification?  Record the location of the specification
  -- and assign the data type to the identifier.

  elsif (token = symbol_t and identifiers( token ).value.all = ";") and
     expr_expected then
     identifiers( id ).kind := type_token;
     identifiers( id ).specFile := getSourceFileName;
     identifiers( id ).specAt := getLineNo;
     -- We assume here that abstracts can make constant specifications.
     -- limiteds will be disallowed elsewhere

  -- Check for optional assignment

  elsif (token = symbol_t and identifiers( token ).value.all = ":=") or
     expr_expected then

     -- Tricky bit: what about "i : integer := i"?
     --   Dropping the top of the stack temporarily isn't good enough: if
     -- the assignment contains backquotes, the name of the command will
     -- overwrite the hidden variable.  The variable must be deleted and
     -- redeclared later.

     declare
        is_constant : boolean := false;
        var_name    : unbounded_string;
        wasLimited  : constant boolean := identifiers( id ).usage = limitedUsage;
     begin

       -- Temporarily destroy identifer so that i : integer := i isn't circular

       var_name := identifiers( id ).name;                 -- remember name
       if identifiers( id ).usage = constantUsage then     -- a constant?
          is_constant := true;                             -- remember it
       end if;
       discardUnusedIdentifier( id );                      -- discard variable

       -- Calculate the assignment (i.e. using any previous variable i)

       ParseAssignPart( expr_value, right_type );          -- do := part

       -- Redeclare temporarily destroyed identifier (i.e. declare new i)
       -- and assign its type

       declareIdent( id, var_name, type_token, varClass );  -- declare var
       identifiers( id ).declaredAt := getLineNo;
       identifiers( id ).declaredFile := getSourceFileName;
       -- TODO: refactor this
       if is_constant then                                  -- a constant?
          identifiers( id ).usage := constantUsage;
       end if;
       if wasLimited then
          identifiers( id ).usage := limitedUsage;
       end if;
     end;

     -- exceptions are a special case because they are a keyword
     -- if the identifier is an exception, the missing semi-colon is detected
     -- first, but here someone is trying to assign an exception TO a normal
     -- typed variable.

     if right_type = exception_t then
        err( contextNotes => pl( "declaring the variable" ),
             subject => id,
             subjectType => identifiers( id ).kind,
             reason => pl( "cannot be assigned an initial value" ),
             obstructorNotes => nullMessageStrings
        );

     -- command types have special limitations

     elsif getBaseType( type_token ) = command_t then
       declare
         scriptDir : unbounded_string;
       begin
          if baseTypesOK( uni_string_t, right_type ) then
             type_token := uni_string_t; -- pretend it's a string
             scriptDir := dirname( expr_value );
             if not C_is_secure_dir( to_string( scriptDir ) & ASCII.NUL ) then
                err( contextNotes => +"declarating the command " &
                        unb_em( identifiers( id ).name ),
                     subjectNotes => pl( qp( "the parent directory " ) ) & unb_em( scriptDir ),
                     reason => +"cannot be read or is not secure because",
                     obstructorNotes => +"is either not readable, is world writable, is not a directory"
                );
             end if;
             if not File_Exists( to_string( expr_value ) ) then
                err( contextNotes => +"declarating the command " &
                        unb_em( identifiers( id ).name ),
                     subjectNotes => em( '"' & to_string( toEscaped( expr_value ) ) & '"' ),
                     reason => +"is not a path to an",
                     obstructorNotes => em( "executable file" ),
                     remedy => +"the path is wrong or the file is missing"
                );
             elsif not C_is_executable_file( to_string( expr_value ) & ASCII.NUL ) then
                err( contextNotes => +"declarating the command " &
                        unb_em( identifiers( id ).name ),
                     subjectNotes => em( '"' & to_string( toEscaped( expr_value ) ) & '"' ),
                     reason => +"is not a path to an",
                     obstructorNotes => em( "executable file" ),
                     remedy => +"the path is not a regular file or does not have execute permission"
                );
             end if;
          end if;
       end;

     elsif type_checks_done or else baseTypesOK( type_token, right_type ) then
        null;
     end if;

     -- mark the type that was targeted by the cast
     if syntax_check then
        identifiers( type_token ).wasCastTo := true;
     end if;

     -- perform assignment

     if isExecutingCommand then
        expr_value := castToType( expr_value, type_token );
        if type_token /= right_type then
           DoContracts( identifiers( id ).kind, expr_value );
        end if;
        identifiers( id ).value.all := expr_value;
        if trace then
            put_trace(
               to_string( identifiers( id ).name ) & " := """ &
               toSecureData( to_string( ToEscaped( expr_value ) ) ) & """" );
        end if;
     else
        -- At compile time, constants may have assigned values.
        -- However, we do not have static expression support (yet).
        -- A variable assigned to a constant may not be defined
        -- until run-time.
        -- KLUDGE: For constants, we may at compile-time and the
        -- variables are not defined.  Contracts cannot be run.
        if identifiers( id ).usage = constantUsage then
           if expr_value /= null_unbounded_string then
              expr_value := castToType( expr_value, type_token );
              if getUniType( type_token ) = uni_string_t then
                 -- put_line(to_string( identifiers(id).name));
                 -- put_line(maybe_secret( identifiers(id).name, expr_value )'img );
                 if maybe_secret( identifiers(id).name, expr_value ) then
                    err(
                      contextNotes => +"In your declaration",
                      subject => id,
                      reason => +"is being assigned a value that could be",
                      obstructorNotes => em( "secret data" ),
                      remedy => +"hard-coded secrets could accidentally be stored in version control software.  Store them in an untracked data file"
                 );
                 end if;
              end if;
              identifiers( id ).value.all := expr_value;
           end if;
        end if;
     end if;
  elsif (token = symbol_t and identifiers( token ).value.all = ";") then
     identifiers( id ).kind := type_token;
  else
     -- neither an ending ; or a :=?  destory the variable.  A syntax
     -- error will occur when expect semi-colon runs
     identifiers( id ).kind := new_t;
     discardUnusedIdentifier( id );
  end if;
  -- failed somewhere to set a real type?
  -- blow away half-declared variable
  if error_found then
     identifiers( id ).kind := new_t;
     discardUnusedIdentifier( id );
  end if;
end ParseDeclarationPart;


-----------------------------------------------------------------------------
--  PARSE RECORD FIELDS
--
-- Handle the declaration of record type's fields.
-- Syntax: field = declaration [; declaration ... ]
-- Used by ParseRecordTypePart.
-----------------------------------------------------------------------------

procedure ParseRecordFields( record_id : identifier; field_no : in out integer ) is
   field_id : identifier;
   b : boolean;
begin
  -- ParseNewIdentifier( field_id );
  ParseFieldIdentifier( record_id, field_id );
  ParseDeclarationPart( field_id, anon_arrays => false, exceptions => false );
  identifiers( field_id ).class := subClass;        -- it is a subtype
  identifiers( field_id ).field_of := record_id;    -- it is a field
  identifiers( field_id ).value.all := to_unbounded_string( field_no'img );
  if syntax_check then
     identifiers( field_id ).wasReferenced := true;
     --identifiers( field_id ).referencedByFlow := getDataFlowName;
  end if;
  expectDeclarationSemicolon( context => field_id );
  if not error_found and  token /= eof_t and token /= end_t then
     field_no := field_no + 1;
     ParseRecordFields( record_id, field_no );
     -- the symbol table will overflow before field_no does
  end if;
  if error_found then
     b := deleteIdent( field_id );
  end if;
end ParseRecordFields;


-----------------------------------------------------------------------------
--  PARSE RECORD TYPE PART
--
-- Parse a new record type.  The fields are parsed in ParseRecordFields.
-- It also creates the record.
-- Syntax: record-type-part = "[type-usage] record record-fields end record"
-----------------------------------------------------------------------------

procedure ParseRecordTypePart( newtype_id : identifier ) is
   field_no : integer := 1;
   b : boolean;
begin
   ParseTypeUsageQualifiers( newtype_id );
   expect( record_t );
   ParseRecordFields( newtype_id, field_no );
   -- end record (or end name)
   expect( end_t );
   if token = record_t then
      expect( record_t );
   elsif token = newtype_id then
      if not onlyAda95 then
         expect( newtype_id );
      else
         err( contextNotes => pl( "declaring the record type " ) &
                 unb_em( identifiers( newtype_id ).name ),
              subjectNotes => +"the type definition",
              reason => pl( "should end with " ) &
                 em( "end record" ) & pl( " because of" ),
              obstructorNotes => em( "pragma ada_95" )
        );
      end if;
  else
     err( contextNotes => pl( "declaring the record type " ) &
             unb_em( identifiers( newtype_id ).name ),
          subjectNotes => +"the type definition",
          reason => pl( "should end with" ),
          obstructorNotes => em( "end record" ) & pl( " or " ) &
             unb_em( "end " & identifiers( newtype_id ).name )
        );
   end if;
   -- if isExecutingCommand then
   if not error_found then
      identifiers( newtype_id ).kind := root_record_t;      -- a record
      identifiers( newtype_id ).list := false;              -- it isn't an array
      identifiers( newtype_id ).field_of := eof_t;          -- it isn't a field
      identifiers( newtype_id ).class := typeClass;         -- it is a type
      identifiers( newtype_id ).import := false;            -- never import
      identifiers( newtype_id ).export := false;            -- never export
      identifiers( newtype_id ).value.all := to_unbounded_string( field_no'img );
      -- number of fields in a record variable
   else                                                     -- otherwise
     b := deleteIdent( newtype_id );                        -- discard bad type
   end if;
end ParseRecordTypePart;


-----------------------------------------------------------------------------
--  PARSE ARRAY TYPE PART
--
-- Parse the declaration of a new array type, starting from "array".
-- Syntax: array-type = "[type-usage] array(exp1..exp2) of element-type"
-----------------------------------------------------------------------------

procedure ParseArrayTypePart( newtype_id : identifier ) is
   --type_id     : arrayID;
   ab1         : unbounded_string; -- low array bound (array bound 1)
   kind1       : identifier;
   ab1_as_longint : long_integer;
   ab2         : unbounded_string; -- high array bound (array bound 2)
   kind2       : identifier;
   ab2_as_longint : long_integer;
   elementType : identifier;
   elementBaseType : identifier;        -- base type of array elements type
   b           : boolean;
begin
   ParseTypeUsageQualifiers( newtype_id );

   -- Check the Array Declaration

   expect( array_t );
   expect( symbol_t, "(" );
   ParseExpression( ab1, kind1 );
   -- should be constant expression but we can't handle those yet
   if getUniType( kind1 ) = uni_string_t or
      identifiers( kind1 ).list then
      err( contextNotes => pl( "declaring the array type " ) &
              unb_em( identifiers( newtype_id ).name ),
           subjectNotes => +"the array type indexes",
           reason => pl( "should be not be a string nor an array like type" ),
           obstructor => kind1
      );
   end if;
   expect( symbol_t, ".." );
   ParseExpression( ab2, kind2 );
   if token = symbol_t and identifiers( token ).value.all = "," then
       featureNotYetImplemented( subjectNotes => "array of multiple dimensions",
          remedy => "encode a dimension to a JSON string as a workaround until support is written" );
   elsif ab1 = null_unbounded_string then
      err( contextNotes => pl( "declaring the array type " ) &
              unb_em( identifiers( newtype_id ).name ),
              subjectNotes => em( "the low bound of the array index range" ),
              reason => em( "has no value" ),
              obstructorNotes => nullMessageStrings,
              remedy => +"a numeric variable was not assigned a value",
              seeAlso => docArrays
      );
   elsif ab2 = null_unbounded_string then
      err( contextNotes => pl( "declaring the array type " ) &
              unb_em( identifiers( newtype_id ).name ),
              subjectNotes => em( "the high bound of the array index range" ),
              reason => em( "has no value" ),
              obstructorNotes => nullMessageStrings,
              remedy => +"a numeric variable was not assigned a value",
              seeAlso => docArrays
      );
   elsif type_checks_done or else baseTypesOK(kind1, kind2 ) then
      if isExecutingCommand and not syntax_check then  -- ab1/2 undef on synchk
         begin
            ab1_as_longint := long_integer( to_numeric( ab1 ) );
         exception when constraint_error =>
            err( contextNotes => pl( "declaring the array type " ) &
                    unb_em( identifiers( newtype_id ).name ),
                 subjectNotes => em( "the low bound of the array index range" ),
                 reason => +"has a value that is too large",
                 obstructorNotes => em_value( ab1 ),
                 obstructorType => kind1,
                 seeAlso => seeTypes
            );
            ab1_as_longint := 0;
         end;
         begin
            ab2_as_longint := long_integer( to_numeric( ab2 ) );
         exception when constraint_error =>
            err( contextNotes => pl( "declaring the array type " ) &
                    unb_em( identifiers( newtype_id ).name ),
                 subjectNotes => em( "the high bound of the array index range" ),
                 reason => +"has a value that is too large",
                 obstructorNotes => em_value( ab2 ),
                 obstructorType => kind2,
                 seeAlso => seeTypes
            );
            ab2_as_longint := 0;
         end;
         if ab1_as_longint > ab2_as_longint then
            if ab1_as_longint /= 1 and ab2_as_longint /= 0 then
               err( contextNotes => pl( "declaring the array type " ) &
                       unb_em( identifiers( newtype_id ).name ),
                    subjectNotes => em( "the high bound of the array index range" ),
                    reason => em( "is greater than" ),
                    obstructorNotes => em( "the high bound"),
                    remedy => +"the bounds are incorrect, reversed or you mean 1..0 for an empty array",
                    seeAlso => docArrays
               );
            end if;
         end if;
      end if;
   end if;
   expect( symbol_t, ")" );
   expect( of_t );
   if token = exception_t then
     err( contextNotes => pl( "declaring the array type " ) &
              unb_em( identifiers( newtype_id ).name ),
          subjectNotes => em( "an array" ),
          reason => em( "cannot contain" ),
          obstructorNotes => em( "exceptions"),
          seeAlso => docArrays
     );
   end if;
   ParseIdentifier( elementType );                       -- parent type name

   -- Finish declaring the array
   --
   -- Note: Bounds are expressions and may not be defined during syntax check
  -- (Constant assignments, etc. occur only when actually running a script)

   elementBaseType := getBaseType( elementType );
   if token = symbol_t and identifiers( token ).value.all = ":=" then
      err( contextNotes => pl( "declaring the array type " ) &
                       unb_em( identifiers( newtype_id ).name ),
          subjectNotes => em( "an array type" ),
          reason => em( "cannot chave an initial assignment" ),
          obstructorNotes => nullMessageStrings,
          seeAlso => docArrays
      );
      b := deleteIdent( newtype_id );                       -- discard bad type
   elsif identifiers( elementBaseType ).list  then
      featureNotYetImplemented( subjectNotes => "an array of arrays",
         remedy => "encode " & to_string( identifiers( elementType ).name ) &
            " as a JSON string as a workaround until support is written" );
      b := deleteIdent( newtype_id );                       -- discard bad type
   elsif type_checks_done or else class_ok( elementType, typeClass, subClass ) then  -- item type OK?
      if isExecutingCommand and not syntax_check then       -- not on synchk
         identifiers( newtype_id ).firstBound := ab1_as_longint;
         identifiers( newtype_id ).lastBound := ab2_as_longint;
      end if;
      identifiers( newtype_id ).kind := elementType;        -- element type
      identifiers( newtype_id ).genKind := kind1;           -- index type
      identifiers( newtype_id ).list := true;               -- it is an array
      identifiers( newtype_id ).class := typeClass;         -- it is a type
      identifiers( newtype_id ).import := false;            -- never import
      identifiers( newtype_id ).export := false;            -- never export
   else                                                     -- otherwise
     b := deleteIdent( newtype_id );                        -- discard bad type
   end if;

end ParseArrayTypePart;


-----------------------------------------------------------------------------
--  PARSE AFFIRM BLOCK
--
-- Handle an contract affirm block.  Parse Affirm Clause handles the
-- setup for this procedure.
-- Syntax: affirm ... begin ... end affirm;
-- I decided not to have an exception handler since the purpose of the
-- accept block is to raise exceptions.  However, they could be added later.
-- I've included commented out statements for this.
-----------------------------------------------------------------------------

procedure ParseAffirmBlock is

  --errorOnEntry : boolean := error_found;
begin
   -- Verify context
   expect( affirm_t );
   ParseBlock;
   --if token = exception_t then
   --   ParseExceptionHandler( errorOnEntry );
   --end if;
   expect( end_t );
   expect( affirm_t );
end ParseAffirmBlock;


-----------------------------------------------------------------------------
--  PARSE AFFIRM CLAUSE
--
-- Setup an affirm block.  This happens at compile-time.  Create a new block
-- scope, declare the identifier representing the value, parse the affirm,
-- save the byte code into the data type's contract field.
-- To execute a contract, we cannot use a function since we cannot
-- define one without knowing the data type of type_value.
-- TODO: handle backquoted affirm clause
-----------------------------------------------------------------------------

procedure ParseAffirmClause( newtype_id : identifier ) is
   type_value_id : identifier;
   blockStart    : natural;
   blockEnd      : natural;
   old_syntax_check : constant boolean := syntax_check;
   oldRshOpt : constant commandLineOption := rshOpt;
begin
   -- declare type_value
   if onlyAda95 then
     err(
        context => newtype_id,
        subjectNotes => em( "case affirm block" ),
        reason => +"is not compatible with",
        obstructorNotes => obstructorAda95
     );
   else
      pushBlock(
        newScope => true,
        newName => affirm_clause_str,
        newFlow => identifiers( newtype_id ).name & " affirm"
      );
      declareIdent( type_value_id, identifiers( newtype_id ).name, newtype_id );
      identifiers( type_value_id ).declaredAt := getLineNo;
      identifiers( type_value_id ).declaredFile := getSourceFileName;
      -- The type variable may or may not be written to and we don't want
      -- a warning that it should be limited, constant, etc.
      identifiers( type_value_id ).wasWritten := true;
      -- for now, treat as a restricted shell to reduce the risk of side-effects.
      rshOpt := true;

      blockStart := firstPos;
      syntax_check := true;
      ParseAffirmBlock;

      rshOpt := oldRshOpt;
      syntax_check := old_syntax_check;
      blockEnd := lastPos+1; -- include EOL ASCII.NUL
      if not syntax_check then
         -- TODO: copyByteCodeLines to be fixed
         identifiers( newtype_id ).contract := to_unbounded_string( copyByteCodeLines( blockStart, blockEnd ) );
      end if;
      pullBlock;
   end if;
end ParseAffirmClause;


-----------------------------------------------------------------------------
--  PARSE CASE AFFIRM BLOCK
--
-- Handle an contract affirm block.  Parse Case Affirm Clause handles the
-- setup for this procedure.
-- Syntax: case affirm ... begin ... end affirm;
-----------------------------------------------------------------------------

procedure ParseCaseAffirmBlock is
   case_id : identifier;
begin
   -- Verify context
   expect( case_t );
   expect( affirm_t );

  -- this will have an error during compilation.
  -- Case #1: case procedure when ... =>    ;

   if token /= when_t then                                 -- first when missing?
      expect( when_t );                                    -- force error
   end if;

   while token = when_t loop
      expect( when_t );
      if token = symbol_t and identifiers( token ).value.all = "=>" then
         err(                                                 -- "=>"
            contextNotes => +"in the case when part",
            subjectNotes => pl( qp( "the when conditions" ) ),
            reason => +"are",
            obstructorNotes => +"missing"
         );
      end if;
   exit when token = others_t;
      ParseIdentifier( case_id );
      if identifiers( case_id ).class /= typeClass and
         identifiers( case_id ).class /= subClass then
         -- TODO: fix me for proper format
         err(                                                 -- "=>"
            contextNotes => +"in the case affirm when part",
            subjectNotes => pl( qp( "the when conditions" ) ),
            reason => +"should be",
            obstructorNotes => +"a type or subtype"
         );
      else
         while token = symbol_t and identifiers( token ).value.all = "|" loop
            expect( symbol_t, "|" );
            ParseIdentifier( case_id );
            if identifiers( case_id ).class /= typeClass and
               identifiers( case_id ).class /= subClass then
               -- TODO: fix me for proper format
               err(                                                 -- "=>"
                  contextNotes => +"in the case affirm when part",
                  subjectNotes => pl( qp( "the when conditions" ) ),
                  reason => +"should be",
                  obstructorNotes => +"a type or subtype"
               );
            end if;
            -- exit when error_found or token = others_t;
            exit when error_found;
         end loop;
      end if;

      if token /= symbol_t or identifiers( token ).value.all /= "=>" then
         err(                                                 -- "=>"
            contextNotes => +"in case affirm when part",
            subjectNotes => pl( qp( "the list of input values" ) ),
            reason => +"should end with a '=>' but found a",
            obstructor => token
         );
      else
         getNextToken; -- expectSymbol( "=>" );
      end if;
      ParseBlock( when_t );
   end loop;

   -- others part

   if token /= others_t then                                -- a little clearer
      err(
         contextNotes => +"in the case procedure",
         subjectNotes => pl( qp( "when others" ) ),
         reason => +"is",
         obstructorNotes => +"missing"
      );                        -- if pointing at
   end if;                                                  -- end case
   expect( others_t );                                      -- "others"
   if token /= symbol_t or identifiers( token ).value.all /= "=>" then
      err(                                                 -- "=>"
         contextNotes => +"in case affirm when part",
         subjectNotes => pl( qp( "the list of input values" ) ),
         reason => +"should end with a '=>' but found a",
         obstructor => token
      );
   else
      getNextToken; -- expectSymbol( "=>" );
   end if;
   ParseBlock;
   expect( end_t );
   expect( affirm_t );
end ParseCaseAffirmBlock;


-----------------------------------------------------------------------------
--  PARSE CASE AFFIRM CLAUSE
--
-- Setup an case affirm block.  This happens at compile-time.  Create a new block
-- scope, declare the identifier representing the value, parse the affirm,
-- save the byte code into the data type's contract field.
-- To execute a contract, we cannot use a function since we cannot
-- define one without knowing the data type of type_value.
-- TODO: handle backquoted affirm clause
-----------------------------------------------------------------------------

procedure ParseCaseAffirmClause( newtype_id : identifier ) is
   type_value_id : identifier;
   blockStart    : natural;
   blockEnd      : natural;
   old_syntax_check : constant boolean := syntax_check;
   oldRshOpt : constant commandLineOption := rshOpt;
begin
   -- declare type_value
   if onlyAda95 then
     err(
        context => newtype_id,
        subjectNotes => em( "case affirm block" ),
        reason => +"is not compatible with",
        obstructorNotes => obstructorAda95
     );
   else
      pushBlock(
        newScope => true,
        newName => case_affirm_clause_str,
        newFlow => identifiers( newtype_id ).name & " affirm"
      );
      declareIdent( type_value_id, identifiers( newtype_id ).name, newtype_id );
      -- The type variable may or may not be written to and we don't want
      -- a warning that it should be limited, constant, etc.
      identifiers( type_value_id ).wasReferenced := true;
      identifiers( type_value_id ).wasWritten := true;
      -- for now, treat as a restricted shell to reduce the risk of side-effects.
      rshOpt := true;

      blockStart := firstPos;
      syntax_check := true;
      ParseCaseAffirmBlock;

      rshOpt := oldRshOpt;
      syntax_check := old_syntax_check;
      blockEnd := lastPos+1; -- include EOL ASCII.NUL
      if not syntax_check then
         -- TODO: copyByteCodeLines to be fixed
         identifiers( newtype_id ).contract := to_unbounded_string( copyByteCodeLines( blockStart, blockEnd ) );
      end if;
      pullBlock;
   end if;
end ParseCaseAffirmClause;


-----------------------------------------------------------------------------
--  PARSE TYPE
--
-- Handle a user-defined type declaration.
-- Syntax: type = "type newtype is new [type-usage] oldtype [affirm clause]"
--         type = "type arraytype is array-type-part"
--         type = "type rectype is record-type-part"
-- NOTE: enumerated types cannot be overloaded (yet)
-----------------------------------------------------------------------------

procedure ParseType is
   newtype_id  : identifier;
   parent_id   : identifier;
   enum_index  : integer := 0;
   b : boolean;
begin
   expect( type_t );                                       -- "type"
   ParseNewIdentifier( newtype_id );                       -- typename
   expect( is_t );                                         -- is

   if Token = symbol_t and identifiers( token ).value.all = "(" then

      -- enumerated
      --
      -- If an error happens during the parsing, some enumerated items
      -- may be left declared.  Should use recursion for parsing the
      -- items so they can be properly "rolled back".

      identifiers( newtype_id ).kind := root_enumerated_t; -- the parent is
      identifiers( newtype_id ).class := typeClass;        -- type based on
      identifiers( newtype_id ).wasApplied := true;        -- can't be abstract
      parent_id := newtype_id;                             -- root enumerated
      -- The enum type name may not be referenced as much
      -- as items are mentioned.  (e.g. draco_ii doesn't
      -- use the type name anywhere).
      if syntax_check and not restriction_no_unused_identifiers then
         identifiers( parent_id ).wasReferenced := true;
         --identifiers( parent_id ).referencedByFlow := getDataFlowName;
      end if;
      expectSymbol(                                        -- should always be
         contextNotes => +"in the enumerated type",        -- true. open enum
         expectedValue => "(",
         subject => parent_id,
         reason => +"items list seems to be missing because the list"
      );
      while token /= eof_t loop                            -- name [,name]
         ParseNewIdentifier( newtype_id );                 -- enumerated item
         -- always execute declarations when syntax checking
         -- because they are needed to test types and interpret
         -- other statements
         if isExecutingCommand or syntax_check then        -- OK to do it?
            -- identifiers( newtype_id ).class := constClass; -- it's a type
            identifiers( newtype_id ).class := enumClass;  -- it's a type
            -- normally, treat them as values and thus we don't care
            -- if they are used or not.  Unless user explicitly requests
            -- that they are tested.
            if syntax_check and not restriction_no_unused_identifiers then
               --identifiers( newtype_id ).referencedByFlow := getDataFlowName;
               identifiers( newtype_id ).wasReferenced := true;
            end if;
            declare
              s : constant string := enum_index'img;
            begin
              -- drop leading space
              --identifiers( newtype_id ).value := to_unbounded_string( s(2..s'last) );
              identifiers( newtype_id ).value.all := to_unbounded_string( s );
            end;
            identifiers( newtype_id ).kind := parent_id;   -- based on parent
         else                                              -- otherwise
            b := deleteIdent( newtype_id );                -- discard item
         end if;
         enum_index := enum_index + 1;                     -- next item number
         exit when error_found or identifiers( token ).value.all /= ",";      -- quit when no ","
         expectParameterComma;                             -- ","
      end loop;
      expectSymbol(                                        -- close enum
         contextNotes => +"in the enumerated type",
         expectedValue => ")",
         subject => parent_id,
         reason => +"items list looks incomplete because the list"
      );
      if error_found or exit_block then                    -- problems?
         b := deleteIdent( parent_id );                    -- discard parent
     end if;

   -- "abstract" appears before record or array, but after "new"
   -- so there's extra logic to handle abstract and non-abstract
   -- cases.
   --
   -- type ... abstract record... or abstract array...

   elsif token = abstract_t or token = limited_t or token = constant_t then
      ParseTypeUsageQualifiers( newtype_id );
      if token = array_t then
         ParseArrayTypePart( newtype_id );
      elsif token = record_t then
         ParseRecordTypePart( newtype_id );
      elsif token = new_t then
        err( contextNotes => +"in the declaration of type " &
                unb_em( identifiers( newtype_id ).name ),
             subjectNotes => em( "abstract" ) & pl( " or " ) &
                em( "constant" ) & pl( " or " ) &
                em( "limited" ),
             reason => +"should follow after the keyword",
             obstructor => new_t,
             seeAlso => docTypeDecl
        );
      else
        -- this only occurs if a usage qualifier is present
        err( contextNotes => +"in the declaration of type " &
                unb_em( identifiers( newtype_id ).name ),
             subjectNotes => +"a missing " & em( "new" ),
             reason => +"instead expects the keywords",
             obstructorNotes => em( "array") & pl( " or " ) & em( "record" ),
             remedy => +"add new before abstract, limited or constant",
             seeAlso => docTypeDecl
        );
      end if;

   -- type ... is array...

   elsif token = array_t then
      ParseArrayTypePart( newtype_id );
      -- for now, assignment is with a scalar so we don't have an affirm
      -- block for an array.

   -- type ... is record...

   elsif token = record_t then
      ParseRecordTypePart( newtype_id );
      -- for now, assignment is with a scalar so we don't have an affirm
      -- block for a record.
   else

     -- type ... is new [abstract] ...

     expect( new_t );                                      -- "new"

     ParseTypeUsageQualifiers( newtype_id );

     -- Standard Ada syntax, but if we could extend arrays or records, would
     -- "new" be appropriate?

     if token = array_t or token = record_t then
        err( contextNotes => +"in the declaration of type " &
                unb_em( identifiers( newtype_id ).name ),
             subject => new_t,
             reason => +"should be omitted because",
             obstructorNotes => em( "the type is not derived from another type" ),
             remedy => +"records and arrays do not use the keyword new",
             seeAlso => docTypeDecl
        );
     end if;

     ParseIdentifier( parent_id );                         -- parent type name

     if not type_checks_done then
        if class_ok( parent_id, typeClass, subClass, genericTypeClass ) then    -- not a type?
           if identifiers( getBaseType( parent_id ) ).kind = root_record_t then
              -- TODO: we would have to generate all the field identifiers
              -- for the record, renamed for the new type, which is not done
              -- yet.  I will need this for objects later.
              featureNotYetImplemented( subjectNotes => "new types based on records",
                  remedy => "" );
            end if;
        end if;
     end if;

     -- Types are generally allowed to change the type usage in any way they want.
     -- As a special case, resources must always be limited (or abstract)
     -- because the value is a resource id.

    if identifiers( parent_id ).resource then
        if identifiers( newtype_id ).usage /= abstractUsage and
           identifiers( newtype_id ).usage /= limitedUsage then
           err( contextNotes => +"in the declaration of type " &
                   unb_em( identifiers( newtype_id ).name ),
                subjectNotes => +"types that represent " & em( "resources" ),
                reason => +"should be " & em( "abstract" ) & pl( " or " ) &
                    em( "limited" ) & pl( " and not" ),
                obstructorNotes => pl( "readable"),
                remedy => +"this type contains a reference or id number " &
                    pl( "that should not be modified or read" ),
                seeAlso => docTypeDecl
           );
        end if;
        identifiers( newtype_id ).resource := true;
     end if;

     -- For a generic/parameterized type, if type checks need to be performed,
     -- check the supplied parameters and ensure they are compatible with the
     -- built-in generic type.  For example, assure a doubly_linked_list has
     -- only one parameter.
     --
     -- If the parent type has a defined parameter, then it is an instantiated
     -- generic.  It has no new parameters.  Copy the instantiated parameters
     -- of the parent to the new type.

--put_line( "*** ParseType: " & to_string( identifiers( newtype_id ).name )  ); -- DEBUG

     if identifiers( parent_id ).class = genericTypeClass then
        ParseGenericParametersPart( newtype_id, parent_id );
        if not type_checks_done then
           CheckGenericParameterType( newtype_id, parent_id );
--put_line( "ParseType: Generics done" ); -- DEBUG
--put_identifier( identifiers( newtype_id ).genKind ); -- DEBUG
        end if;
        -- currently, it will always be limitedUsage
        identifiers( newtype_id ).usage := identifiers( parent_id ).usage;
     elsif token = symbol_t and identifiers( token ).value.all = "(" then
           err( contextNotes => +"in the declaration of type " &
                unb_em( identifiers( newtype_id ).name ),
             subject => parent_id,
             subjectType => identifiers( parent_id ).kind,
             reason => +"is a concrete type but",
             obstructorNotes => em( "has parameters for a generic type"),
             seeAlso => docTypeDecl
        );
     end if;

     if isExecutingCommand then                         -- OK to do it?
        identifiers( newtype_id ).kind := parent_id;    -- define the type
        identifiers( newtype_id ).class := typeClass;

        if identifiers( parent_id ).list then           -- an array?
           identifiers( newtype_id ).list := true;      -- this also array
           identifiers( newtype_id ).firstBound :=      -- copy first bnd
              identifiers( parent_id ).firstBound;
           identifiers( newtype_id ).lastBound :=       -- copy last bnd
              identifiers( parent_id ).lastBound;
        end if;

        -- If the parent type has a defined parameter, then it is an instantiated
        -- generic.  It has no new parameters.  Copy the instantiated parameters
        -- of the parent to the new type.

        if identifiers( parent_id ).genKind /= eof_t then
           identifiers( newtype_id ).genKind :=         -- copy index type
           identifiers( parent_id ).genKind;          -- / generic type
           identifiers( newtype_id ).genKind2 :=        -- copy index type
           identifiers( parent_id ).genKind2;         -- / generic type
        end if;
     elsif syntax_check then                            -- syntax check?
        identifiers( newtype_id ).kind := parent_id;    -- assign subtype
        identifiers( newtype_id ).class := typeClass;   -- subtype class
        if identifiers( parent_id ).list then           -- an array?
           identifiers( newtype_id ).list := true;      -- this also array
        end if;

        -- If the parent type has a defined parameter, then it is an instantiated
        -- generic.  It has no new parameters.  Copy the instantiated parameters
        -- of the parent to the new type.

        if identifiers( parent_id ).genKind /= eof_t then
           identifiers( newtype_id ).genKind :=         -- copy index type
           identifiers( parent_id ).genKind;          -- / generic type
           identifiers( newtype_id ).genKind2 :=        -- copy index type
           identifiers( parent_id ).genKind2;         -- / generic type
        end if;
     else                                               -- otherwise
       b := deleteIdent( newtype_id );                  -- discard new type
     end if;

     -- Programming-by-contract (affirm clause)

     if token = case_t then
        ParseCaseAffirmClause( newtype_id );
     elsif token = affirm_t then
        ParseAffirmClause( newtype_id );
     elsif token /= symbol_t and identifiers( token ).value.all /= ";" then
        err( contextNotes => +"in the declaration of type " &
                unb_em( identifiers( newtype_id ).name ),
             subjectNotes => +"the declaration",
             reason => +"should end with an affirm block or ';' and not",
             obstructor => token,
             seeAlso => docTypeDecl
        );
     end if;
   end if;
end ParseType;


-----------------------------------------------------------------------------
--  PARSE SUBTYPE
--
-- Handle a user-defined subtype declaration.
-- Syntax: subtype = "subtype newtype is [type-usage] oldtype [affirm clause]"
-- NOTE: enumerateds aren't overloadable (yet)
-----------------------------------------------------------------------------

procedure ParseSubtype is
   newtype_id : identifier;
   parent_id : identifier;
   b : boolean;
begin
   expect( subtype_t );                                    -- "subtype"
   ParseNewIdentifier( newtype_id );                       -- type name
   expect( is_t );                                         -- "is"
   ParseTypeUsageQualifiers( newtype_id );                 -- limited, etc.
   ParseIdentifier( parent_id );                           -- old type

     -- Subypes are generally allowed to change the type usage in any way they want.
     -- As a special case, resources must always be limited (or abstract)
     -- because the value is a resource id.

    if identifiers( parent_id ).resource then
        if identifiers( newtype_id ).usage /= abstractUsage and
           identifiers( newtype_id ).usage /= limitedUsage then
           err( contextNotes => +"in the declaration of subtype " &
                   unb_em( identifiers( newtype_id ).name ),
                subjectNotes => +"subtype that represent " & em( "resources" ),
                reason => +"should be " & em( "abstract" ) & pl( " or " ) &
                    em( "limited" ) & pl( " and not" ),
                obstructorNotes => pl( "readable"),
                remedy => +"this subtype contains a reference or id number " &
                    pl( "that should not be modified or read" ),
                seeAlso => docTypeDecl
           );
        end if;
        identifiers( newtype_id ).resource := true;
     end if;

   if identifiers( parent_id ).class = genericTypeClass then
      err( contextNotes => +"in the declaration of subtype " &
              unb_em( identifiers( newtype_id ).name ),
           subject => parent_id,
           subjectType => identifiers( parent_id ).kind,
           reason => +"should be an instantiated generic type but",
           obstructorNotes => +"it is not instantiated",
           remedy => +"declare it as a new type not a subtype",
           seeAlso => docTypeDecl
      );
   elsif token = symbol_t and identifiers( token ).value.all = "(" then
      err( contextNotes => +"in the declaration of subtype " &
              unb_em( identifiers( newtype_id ).name ),
           subject => parent_id,
           subjectType => identifiers( parent_id ).kind,
           reason => +"is a concrete type but",
           obstructorNotes => em( "has parameters for a generic type"),
           seeAlso => docTypeDecl
      );
   elsif type_checks_done or else class_ok( parent_id, typeClass,
               subClass ) then                             -- not a type?
      if isExecutingCommand then                           -- OK to execute?
         identifiers( newtype_id ).kind := parent_id;      -- assign subtype
         identifiers( newtype_id ).class := subClass;      -- subtype class
         identifiers( newtype_id ).genKind :=              -- copy index type
             identifiers( parent_id ).genKind;             -- / generic type
         if identifiers( parent_id ).list then             -- an array?
            identifiers( newtype_id ).list := true;        -- this also array
            identifiers( newtype_id ).firstBound :=        -- copy first bnd
               identifiers( parent_id ).firstBound;
            identifiers( newtype_id ).lastBound :=         -- copy last bnd
               identifiers( parent_id ).lastBound;
         end if;

        -- If the parent type has a defined parameter, then it is an instantiated
        -- generic.  It has no new parameters.  Copy the instantiated parameters
        -- of the parent to the new type.

        if identifiers( parent_id ).genKind /= eof_t then
           identifiers( newtype_id ).genKind :=         -- copy index type
           identifiers( parent_id ).genKind;          -- / generic type
           identifiers( newtype_id ).genKind2 :=        -- copy index type
           identifiers( parent_id ).genKind2;         -- / generic type
        end if;
      elsif syntax_check then                              -- syntax check?
         identifiers( newtype_id ).kind := parent_id;      -- assign subtype
         identifiers( newtype_id ).class := subClass;      -- subtype class
         if identifiers( parent_id ).list then             -- an array?
            identifiers( newtype_id ).list := true;        -- this also array
         end if;

        -- If the parent type has a defined parameter, then it is an instantiated
        -- generic.  It has no new parameters.  Copy the instantiated parameters
        -- of the parent to the new type.

        if identifiers( parent_id ).genKind /= eof_t then
           identifiers( newtype_id ).genKind :=         -- copy index type
           identifiers( parent_id ).genKind;          -- / generic type
           identifiers( newtype_id ).genKind2 :=        -- copy index type
           identifiers( parent_id ).genKind2;         -- / generic type
        end if;
      else                                                 -- otherwise
         b := deleteIdent( newtype_id );                   -- discard subtype
      end if;

      -- Programming-by-contract (affirm clause)
      if token = case_t then
         ParseCaseAffirmClause( newtype_id );
      elsif token = affirm_t then
         ParseAffirmClause( newtype_id );
      elsif token /= symbol_t and identifiers( token ).value.all /= ";" then
        err( contextNotes => +"in the declaration of subtype " &
                unb_em( identifiers( newtype_id ).name ),
             subjectNotes => +"the declaration",
             reason => +"should end with an affirm block or ';' and not",
             obstructor => token,
             seeAlso => docTypeDecl
        );
      end if;
   end if;
end ParseSubtype;

end parser.decl;
