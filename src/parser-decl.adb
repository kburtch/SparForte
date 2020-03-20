------------------------------------------------------------------------------
-- AdaScript Language Parser                                                --
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
pragma ada_2005;

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );

with gnat.source_info,
    spar_os,
    string_util,
    user_io,
    performance_monitoring,
    compiler,
    scanner,
    scanner_res,
    scanner_restypes,
    parser.decl.as, -- circular relationship for ParseBlock
    parser_aux,
    parser_params,
    interpreter; -- circular relationship for breakout prompt
use spar_os,
    user_io,
    string_util,
    performance_monitoring,
    compiler,
    scanner,
    scanner_res,
    scanner_restypes,
    parser.decl.as, -- circular relationship for ParseBlock
    parser_aux,
    parser_params,
    interpreter; -- circular relationship for breakout prompt

-- with ada.text_io;
-- use ada.text_io;

package body parser.decl is

-----------------------------------------------------------------------------
-- Declarations
-----------------------------------------------------------------------------


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
        err( "abstract types not allowed with " &
            optional_bold( "pragam ada_95" ) );
      end if;
      identifiers( newtype_id ).usage := abstractUsage; -- vars not allowed
      identifiers( newtype_id ).wasReferenced := true;  -- treat as used
      identifiers( newtype_id ).wasApplied := true;     -- treat as applied
      expect( abstract_t );
      if token = abstract_t or token = limited_t or token = constant_t then
         err( "only one of abstract, limited or constant allowed" );
      end if;

   -- limited types

   elsif token = limited_t then
      if onlyAda95 then
         err( "limited types are not allowed with " & optional_bold( "pragma ada_95" ) );
      end if;
      identifiers( newtype_id ).usage := limitedUsage;  -- assign not allowed
      expect( limited_t );
      if token = abstract_t or token = limited_t or token = constant_t then
         err( "only one of abstract, limited or constant allowed" );
      end if;

   -- constant types

   elsif token = constant_t then
      if onlyAda95 then
         err( "constant types are not allowed with " & optional_bold( "pragma ada_95" ) );
      end if;
      identifiers( newtype_id ).usage := constantUsage;  -- read-only
      expect( constant_t );
      if token = abstract_t or token = limited_t or token = constant_t then
         err( "only one of abstract, limited or constant allowed" );
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
-- that a constant declaration may need a value asigned.
-- Syntax: var-usage-qualifiers = [limited | constant]
-- Type usage qualifiers and variable usage qualifiers are not identical:
-- variables cannot be abstract.
-----------------------------------------------------------------------------

procedure ParseVarUsageQualifiers( id : identifier; expr_expected : out boolean ) is
begin
  expr_expected := false;                              -- usually false

  if token = aliased_t then                            -- aliased not supported
     err( "aliased isn't supported" );

  elsif token = constant_t then                        -- handle constant
     identifiers( id ).usage := constantUsage;         -- as a constant and
     expr_expected := true;                            -- must assign value
     expect( constant_t );                             -- by flagging variable
     if token = abstract_t or token = limited_t or token = constant_t then
        err( "only one of abstract, limited or constant allowed" );
     end if;

  elsif token = abstract_t then                        -- abstract only makes sense
     err( optional_bold( "abstract" ) &                -- in type declarations since
        " can only be used in type declarations" );    -- it's a no-use quality.

  elsif token = limited_t then                         -- limited access?
     identifiers( id ).usage := limitedUsage;
     expect( limited_t );
     if token = abstract_t or token = limited_t or token = constant_t then
        err( "only one of abstract, limited or constant allowed" );
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

procedure ParseGenericParametersPart( varId : identifier ) is
  genKind : identifier;
begin
  if token /= symbol_t or identifiers( token ).svalue /= "(" then
     err( "generic types must have element type parameters" );
  end if;
  expect( symbol_t, "(" );
  ParseIdentifier( genKind );
  if type_checks_done or else class_ok( genKind, typeClass, subClass ) then
     identifiers( varId ).genKind := genKind;
     if token = symbol_t and identifiers( token ).svalue = "," then
        expect( symbol_t, "," );
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

       -- For a volatile, update the value before copying
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
       -- we could inadvertantly deallocate it in one place while keeping it open
       -- in another.

       if identifiers( canonicalRef.id ).resource then
          err( gnat.source_info.source_location &
               "internal error: resource identifiers cannot be copied" );
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
               err( "assigning " & optional_bold( arrayIndex'img ) &
                    " elements but the array is range " &
                    identifiers( array_id ).avalue'first'img & " .." & identifiers( array_id ).avalue'last'img );
             when STORAGE_ERROR =>
               err( gnat.source_info.source_location &
                 ": internal error : storage error raised in ParseAssignmentPart" );
             end;
          --end if;
       end if;
       if arrayIndex = long_integer'last then                  -- shound never
          err( "array is too large" );                         -- happen but
       else                                                    -- check anyway
          arrayIndex := arrayIndex+1;                          -- next element
       end if;                                                 -- stop on err
       exit when error_found or identifiers( token ).value.all /= ","; -- more?
       expect( symbol_t, "," );                                -- continue
     end loop;
     arrayIndex := arrayIndex - 1;                             -- last added
     if trace then
        put_trace(
            to_string( identifiers( array_id ).name ) & " :=" &
            arrayIndex'img & " elements" );
     end if;
     if isExecutingCommand then                                -- not on synchk
        if arrayIndex < lastIndex then                         -- check sizes
           err( "assigning only " & optional_bold( arrayIndex'img ) &
                " elements but the array is range " &
                identifiers( array_id ).avalue'first'img & " .." & identifiers( array_id ).avalue'last'img );
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
              err( gnat.source_info.source_location &
                ": internal error: target array storage unexpectedly null" );
           elsif identifiers( array_id ).avalue'first /= arrayIndex then
              err( gnat.source_info.source_location &
                ": internal error: target array first bound doesn't match: " & identifiers( array_id ).avalue'first'img & " vs " & arrayIndex'img );
           elsif identifiers( array_id ).avalue'last /= lastIndex then
              err( gnat.source_info.source_location &
                ": internal error: target array last bound doesn't match: " & identifiers( array_id ).avalue'last'img & " vs " &  lastIndex'img );
           elsif not error_found then
              identifiers( array_id ).avalue.all := identifiers( second_array_id ).avalue.all;
           end if;
        exception when CONSTRAINT_ERROR =>
           err( "constraint_error : index out of range " & identifiers( array_id ).avalue'first'img & " .." & identifiers( array_id ).avalue'last'img );
        when STORAGE_ERROR =>
           err( gnat.source_info.source_location &
              ": internal error : storage error raised when copying arrays" );
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
-- that is not pre-defined as it's own type.  A type declaration will be
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
  expect( symbol_t, "(" );
  ParseExpression( ab1, kind1 );                           -- low bound
  -- should really be a constant expression but we can't handle that
  if getUniType( kind1 ) = uni_string_t then                 -- must be scalar
     err( "array indexes cannot be a string or character type like " &
          optional_bold( to_string( identifiers( kind1 ).name ) ) );
  elsif getUniType( kind1 ) = root_record_t then                 -- must be scalar
     err( "array indexes cannot be a record type like " &
          optional_bold( to_string( identifiers( kind1 ).name ) ) );
  -- this is currently impossible: parseExpression will demand an
  -- array element, not the whole array
  -- elsif identifiers( getBaseType( kind1 ) ).list then
  --    err( "array indexes cannot be an array type like " &
  --         optional_bold( to_string( identifiers( kind1 ).name ) ) );
  else
     expect( symbol_t, ".." );
     ParseExpression( ab2, kind2 );                            -- high bound
     if token = symbol_t and identifiers( token ).value.all = "," then
        err( "array of arrays not yet supported" );
     elsif type_checks_done or else baseTypesOK( kind1, kind2 ) then -- indexes good?
        if isExecutingCommand then                             -- not on synchk
           if ab1 = null_unbounded_string then
              err( "first array index expression has no value" );
           elsif ab2 = null_unbounded_string then
              err( "last array index expression has no value" );
           elsif to_numeric( ab1 ) > to_numeric( ab2 ) then    -- bound backwd?
              if long_integer( to_numeric( ab1 ) ) /= 1 and    -- only 1..0
                 long_integer( to_numeric( ab2 ) ) /= 0 then   -- allowed
                 err( "first array bound is higher than last array bound" );
              end if;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );                                  -- finished ind
  expect( of_t );
  if token = exception_t then
     err( "arrays of exceptions are not allowed" );
  end if;
  ParseIdentifier( elementType );

  -- Declare anonymous type in symbol table and array table
  --
  -- Note: Bounds are expressions and may not be defined during syntax check
  -- (Constant assignments, etc. occur only when actually running a script)

  if not error_found then     -- syntax OK, but if execution failed, no
     elementBaseType := getBaseType( elementType );
     if identifiers( elementBaseType ).list  then
        err( "array of arrays not yet supported" );
     else
        declareIdent( anonType, to_unbounded_string( "an anonymous array" ),
           elementType, typeClass );
        identifiers( anonType ).list := true;
        identifiers( anonType ).wasReferenced := true; -- only referenced when declared
        --identifiers( anonType ).referencedByThread := getThreadName;
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
-- Handle the creation of an array varaible and any renaming or default value
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
         err( optional_bold( to_string( identifiers( arrayType ).name ) ) & " is not a generic type but has parameters" );
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
                       err( "unable to find record field " &
                          optional_bold( to_string( fieldName ) ) );
                    else
                       if type_checks_done or else baseTypesOK( identifiers( field_t ).kind, expr_type ) then
                          if isExecutingCommand then
                             identifiers( field_t ).value.all := expr_value;
                             if trace then
                                put_trace(
                                  to_string( fieldName ) & " := " &
                                  to_string( expr_value ) );
                             end if;
                          end if;
                       end if;
                    end if;
                 end;
           end if;
       end if;
       end loop; -- for
       if not found then
          err( "assigning" & optional_bold( field_no'img ) &
               " fields but the record has only" & optional_bold( expected_fields'img ) );
       end if;
       exit when error_found or identifiers( token ).value.all /= ","; -- more?
       expect( symbol_t, "," );
       field_no := field_no + 1;
     end loop;
     expect( symbol_t, ")" );
     if expected_fields /= field_no then
        err( "assigning only" & optional_bold( field_no'img ) &
             " fields but the record has" & optional_bold( expected_fields'img ) );
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
                           err( gnat.source_info.source_location &
                              ": internal error: mismatched source field" );
                           exit;
                        end if;
                        -- find target field
                        targetFieldName := identifiers( j ).name;
                        targetFieldName := delete( targetFieldName, 1, index( targetFieldName, "." ) );
                        targetFieldName := identifiers( id ).name & "." & targetFieldName;
                        findIdent( targetFieldName, target_field_t );
                        if target_field_t = eof_t then
                           err( gnat.source_info.source_location &
                              ": internal error: mismatched target field" );
                           exit;
                        end if;
                        -- copy it
                        identifiers( target_field_t ).value.all := identifiers( source_field_t ).value.all;
                        if trace then
                          put_trace(
                            to_string( targetFieldName ) & " := " &
                            to_string( identifiers( target_field_t ).value.all ) );
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
       err( gnat.source_info.source_location &
          ": internal error: unable to determine number of fields in record " &
          "type " & optional_bold( to_string( identifiers( recType ).name ) ) &
          " for " & optional_bold( to_string( identifiers( id ).name ) ) );
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
            -- As an optimization, the fields are likely located immediately after
            -- the record itself is defined.  Also assumes they are stored
            -- sequentially.  In the future, records will be stored differently.

            while j < identifiers_top loop
              if identifiers( j ).field_of = baseRecType then
                 if integer'value( to_string( identifiers( j ).value.all )) = i then
                    exit;
                 end if;
              end if;
              j := identifier( integer( j ) + 1 );
            end loop;

            -- no more identifiers means we didn't find it.
            if j = identifiers_top then
               err( gnat.source_info.source_location &
                 "internal error: record field not found" );
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
               -- fields have not been marked as children of the parent
               -- record.  However, to make sure the record is used, it
               -- is convenient to track the field.
               identifiers( dont_care_t ).field_of := id;
               -- apply abtract and limited
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

  -- Assignment?
  -- if it appears, one can only rename...cannot assign.

  elsif token = symbol_t and identifiers( token ).value.all = ":=" then
     if canAssign then
        ParseRecordAssignPart( id, recType );
     elsif identifiers( id ).usage = constantUsage then
        -- CONST SPECS
        -- if it is a constant record and there was no assignment, the full
        -- record variable is a specification.
        identifiers( id ).specFile := getSourceFileName;
        identifiers( id ).specAt := getLineNo;
     end if;

  -- Generic type?  Check for parameters.

  elsif token = symbol_t and identifiers( token ).svalue = "(" then
     err( optional_bold( to_string( identifiers( recType ).name ) ) & " is not a generic type but has parameters" );

  -- No Assignment?  If it's a constant, than it's a constant specification.

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
      if onlyAda95 then
         err( "exception with not allowed with " &
            optional_bold( "pragam ada_95" ) );
      end if;
      expect( with_t );
      if token = use_t then
         err( "with message missing" );
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
           err( "exception status code " & optional_bold( to_string( trim( exception_status, ada.strings.both ) ) ) & " is out-of-range 0..255" );
         end;
      end if;
   elsif token = renames_t then
      err( "exceptions cannot be renamed" );
   elsif token = copies_t then
      -- TODO: we could make this happen
      err( "exceptions cannot be copied" );
   elsif token /= symbol_t and identifiers( token ).value.all /= ";" then
      err( "with or ';' expected" );
   end if;
   -- Do not declare the exception if an error occurred while defining it or
   -- the details may be incorrect.
   if not error_found then
      findException( var_name, id );
      if id = eof_t then
         declareException( id, var_name, default_message, exception_status_code ); -- declare var
      else
         err( "exception " & optional_bold( to_string( var_name ) ) &
              " already exists in a greater scope" );
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
   uniType  : constant identifier := getUniType( type_token );
begin
   if uniType = doubly_list_t then
      if identifiers( id ).genKind2 /= eof_t then
         err( optional_bold( to_string( identifiers( type_token ).name ) ) & " should have one element type" );
      else
         declare
            genKindId : identifier renames identifiers( id ).genKind;
         begin
            if class_ok( genKindId, typeClass, subClass ) then
               if identifiers( genKindId ).list then
                  err( "element type should be a scalar type" );
               elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
                  err( "element type should be a scalar type" );
               end if;
            end if;
         end;
      end if;
   elsif uniType = doubly_cursor_t then
      if identifiers( id ).genKind2 /= eof_t then
         err( optional_bold( to_string( identifiers( type_token ).name ) ) & " should have one element type" );
      end if;
   elsif uniType = btree_file_t then
      if identifiers( id ).genKind2 /= eof_t then
         err( optional_bold( to_string( identifiers( type_token ).name ) ) & " should have one element type" );
      end if;
   elsif uniType = btree_cursor_t then
      if identifiers( id ).genKind2 /= eof_t then
         err( optional_bold( to_string( identifiers( type_token ).name ) ) & " should have one element type" );
      end if;
   elsif uniType = hash_file_t then
      if identifiers( id ).genKind2 /= eof_t then
         err( optional_bold( to_string( identifiers( type_token ).name ) ) & " should have one element type" );
      end if;
   elsif uniType = hash_cursor_t then
      if identifiers( id ).genKind2 /= eof_t then
         err( optional_bold( to_string( identifiers( type_token ).name ) ) & " should have one element type" );
      end if;
   elsif uniType = dht_table_t then
      declare
         genKindId : identifier renames identifiers( id ).genKind;
      begin
         if class_ok( genKindId, typeClass, subClass ) then
            if identifiers( genKindId ).list then
               err( "element type should be a scalar type" );
            elsif identifiers( getBaseType( genKindId ) ).kind = root_record_t then
               err( "element type should be a scalar type" );
            end if;
         end if;
      end;
   else
     -- TODO: implement generic types
      err( "expected a generic type" );
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
     else
        -- TODO: implement generic types
        err( optional_bold( to_string( identifiers( type_token ).name ) ) &
             " is derived from " &
             optional_bold( to_string( identifiers( uniType ).name ) ) &
             " which is not a generic type" );
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
          err( "constant type " &
              optional_bold( to_string( identifiers( type_token ).name ) ) &
              " was " & optional_bold( to_string( identifiers( identifiers( const_id ).kind ).name )) &
              " in the earlier specification (at " &
               to_string( identifiers( const_id ).specFile) & ":" &
          identifiers( const_id ).specAt'img & ")" );
       end if;
   end VerifyTypesAreSame;

  begin
    --put_line("VERIFY CONST SPEC: " & to_string( identifiers( const_id ).name ) ); --DEBUG

    -- This should not happen.
    --
    -- If the constant specification is at a different nesting
    -- level, it's the declaration of a new constant.A

    if not isLocal( const_id ) then
       err( "internal error: constant specification was in a different scope " );
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
       if identifiers( type_token ).usage /= constantUsage then
          err( "fulfilling the constant specification " &
               optional_bold( to_string( identifiers( const_id ).name ) ) &
               " requires a variable of constant usage " &
               "but type " &
               optional_bold( to_string( identifiers( type_token ).name ) ) &
               " is not constant usage nor is the variable " &
               "declared as constant" );
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

    -- Calculate the assignment (ie. using any previous variable i)
    -- if not an aggregate.  The assign part is done here while the variable
    -- does not exist.
    --   For a record, this will erase the number of fields in the record's
    -- parent variable.

    if not oldSpec.list and getUniType( oldSpec.kind ) /= root_record_t then
--put_line( "verify: AssignPart should not run for aggregates"); -- DEBUG
       ParseAssignPart( expr_value, right_type );          -- do := part
    end if;

    -- Redeclare temporarily destroyed identifier (ie. declare new i)
    -- and recover old properties.  Clear the spec and fix the avalue
    -- (if necessary).  The spec is now fulfilled.
    --  For records, we didn't destroy the original.

    if getUniType( oldSpec.kind ) /= root_record_t then
--put_line( "verify: New ident" ); -- DEBUG
       declareIdent( new_const_id, oldSpec.name, type_token, varClass );
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

    -- mark the type that was targetted by the cast
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
              to_string( ToEscaped( expr_value ) ) & """" );
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
  --         err( optional_bold( "overriding" ) &
  --              " expected because " &
  --              optional_bold( to_string( identifiers( id ).name ) ) &
  --              " exists at a different scope" );
  --      end if;
  --   else
  --      if identifiers( id ).kind = new_t then
  --         err( optional_bold( "overriding" ) &
  --              " not expected because " &
  --              optional_bold( to_string( identifiers( id ).name ) ) &
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
        err( "exceptions are not allowed" );
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
        err( "anonymous arrays are not allowed" );
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
           err( gnat.source_info.source_location &
                "internal error: variables should not have abstract types" );
        when others =>
           err( gnat.source_info.source_location &
                "internal error: unknown var qualifier" );
        end case;
  when constantUsage =>
       if identifiers( type_token ).usage = limitedUsage then
          err( "constant is less restrictive than " & optional_bold( "limited" ) );
       end if;
  when limitedUsage =>
       null; -- this is the most constrained
  when abstractUsage =>
       err( "variables cannot be declared as type " &
         optional_bold( to_string( identifiers( type_token ).name ) ) &
         " because it is " & optional_bold( "abstract" ) );
  when others =>
      err( gnat.source_info.source_location &
           "internal error: unknown var qualifier" );
  end case;

  if token = private_t then                             -- private access?
     err( "not yet implemented" );
  end if;

  -- Array type?  Handled elsewhere.

  if identifiers( getBaseType( type_token ) ).list then       -- array type?
     if not anon_arrays then
        err( "nested arrays not yet supported" );
     else
        ParseArrayDeclaration( id, type_token );                -- handle it
     end if;
     return;                                            --  and nothing more
  end if;

  -- Record type?  Handled elsewhere.

  if identifiers( getBaseType( type_token ) ).kind = root_record_t then  -- record type?
     if not anon_arrays then
        err( "nested records not yet supported" );
     --elsif identifiers( type_token ).usage = abstractUsage then
     --   err( "constants and variables cannot be declared as " &
     --     optional_bold( to_string( identifiers( type_token ).name ) ) &
     --     " because it is " & optional_bold( "abstract" ) );
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
        err( "universal/typeless types not allowed with " &
             optional_bold( "pragam ada_95" ) );
     elsif getBaseType( type_token ) = command_t then
        if onlyAda95 then
           err( "command types not allowed with " & optional_bold( "pragma ada_95" ) );
        -- Special case: command type qualifiers
        elsif identifiers( id ).usage /= limitedUsage and
           identifiers( id ).usage /= constantUsage then
           err( "command variables must be " & optional_bold( "limited" ) & " or " & optional_bold( "constant" ) );
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
     ParseGenericParametersPart( id  );
     if not type_checks_done then
        CheckGenericParameterType( id, type_token );
     end if;
     if isExecutingCommand then
        AttachGenericParameterResource( id, type_token );
     end if;
     identifiers( id ).kind := type_token;
     identifiers( id ).usage := identifiers( type_token ).usage;

  elsif token = symbol_t and identifiers( token ).svalue = "(" then
     err( optional_bold( to_string( identifiers( type_token ).name ) ) & " is not a generic type but has parameters" );

   -- We need to attach a resource for the generic-based type
   -- (i.e. type x is generic(...), this will be x)
   -- on an error, the token may not be something with a gen type

  elsif (type_token /= eof_t and not error_found ) and then identifiers( type_token ).genKind /= eof_t then
      if isExecutingCommand then
         AttachGenericParameterResource( id, type_token );
      end if;
      identifiers( id ).genKind := identifiers( type_token ).genKind;
      identifiers( id ).genKind2 := identifiers( type_token ).genKind2;
      identifiers( id ).kind := type_token;

  -- Renames clause
  -- if it appears, one can only rename...cannot assign.

  elsif token = renames_t then

     declare
        originalFieldOf : constant identifier := identifiers( id ).field_of;
        -- TODO: refactor these booleans
        wasLimited : constant boolean := identifiers( id ).usage = limitedUsage;
        wasConstant : constant boolean := identifiers( id ).usage = constantUsage;
     begin
        -- Variable or Constant renaming
        ParseRenamesPart( canonicalRef, id, type_token );
        -- Prevent a constant from being turned into a variable by a renaming
        -- It must be renamed as a constant or a limited.
        if identifiers( canonicalRef.id).usage = constantUsage and
           not wasConstant and not wasLimited then
           err( "a " & optional_bold( "constant" ) & " must be renamed by a constant or a limited" );
        elsif identifiers( canonicalRef.id ).class = enumClass then
           -- TODO: I could probably get this to work but it's a weird edge
           -- case.
           err( "enumerated items cannot be renamed" );
        elsif identifiers( canonicalRef.id ).usage = limitedUsage and not wasLimited then
           err( "a " & optional_bold( "limited" ) & " must be renamed by a limited" );
        elsif identifiers( canonicalRef.id ).field_of /= eof_t then
           if identifiers( identifiers( canonicalRef.id ).field_of ).usage = limitedUsage and not wasLimited then
              err( "limited record fields must be renamed by a limited identifier" );
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
           -- don't do this on an error or an excepion may be thrown
           if isExecutingCommand then
              begin
                 identifiers( id ).value := identifiers( canonicalRef.id ).avalue( canonicalRef.index )'access;
              exception when storage_error =>
                 err( gnat.source_info.source_location &
                    ": internal error: storage_error exception raised" );
              when others =>
                 err( gnat.source_info.source_location &
                    ": internal error: exception raised" );
              end;
           end if;
        end if;
     end if;

  elsif token = copies_t then

     declare
        originalFieldOf : constant identifier := identifiers( id ).field_of;
        -- TODO: refactor these booleans
        wasLimited : constant boolean := identifiers( id ).usage = limitedUsage;
        wasConstant : constant boolean := identifiers( id ).usage = constantUsage;
     begin
        -- Variable or Constant renaming
        ParseCopiesPart( canonicalRef, id, type_token );
        -- Prevent a constant from being turned into a variable by a renaming
        -- It must be renamed as a constant or a limited.
        if identifiers( canonicalRef.id).usage = constantUsage and
           not wasConstant and not wasLimited then
           err( "a " & optional_bold( "constant" ) & " must be copied by a constant or a limited" );
        elsif identifiers( canonicalRef.id ).class = enumClass then
           -- TODO: I could probably get this to work but it's a weird edge
           -- case.
           err( "enumerated items cannot be copied" );
        elsif identifiers( canonicalRef.id ).usage = limitedUsage and not wasLimited then
           err( "a " & optional_bold( "limited" ) & " must be copied by a limited" );
        elsif identifiers( canonicalRef.id ).field_of /= eof_t then
           if identifiers( identifiers( canonicalRef.id ).field_of ).usage = limitedUsage and not wasLimited then
              err( "limited record fields must be copied by a limited identifier" );
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
           -- don't do this on an error or an excepion may be thrown
           if isExecutingCommand then
              begin
                 identifiers( id ).value := identifiers( canonicalRef.id ).avalue( canonicalRef.index )'access;
              exception when storage_error =>
                 err( gnat.source_info.source_location &
                    ": internal error: storage_error exception raised" );
              when others =>
                 err( gnat.source_info.source_location &
                    ": internal error: exception raised" );
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

       -- Calculate the assignment (ie. using any previous variable i)

       ParseAssignPart( expr_value, right_type );          -- do := part

       -- Redeclare temporarily destroyed identifier (ie. declare new i)
       -- and assign its type

       declareIdent( id, var_name, type_token, varClass );  -- declare var
       -- TODO: refactor this
       if is_constant then                                  -- a constant?
          identifiers( id ).usage := constantUsage;
       end if;
       if wasLimited then
          identifiers( id ).usage := limitedUsage;
       end if;
     end;

     -- exceptions are a special case because they are a keyword

     if right_type = exception_t then
        err( "exceptions cannot be assigned" );

     -- command types have special limitations

     elsif getBaseType( type_token ) = command_t then
       if baseTypesOK( uni_string_t, right_type ) then
          type_token := uni_string_t; -- pretend it's a string
          if not C_is_executable_file( to_string( expr_value ) & ASCII.NUL ) then
             err( '"' & to_string( expr_value) & '"' &
                " is not an executable command" );
          end if;
       end if;

     elsif type_checks_done or else baseTypesOK( type_token, right_type ) then
        null;
     end if;

     -- mark the type that was targetted by the cast
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
               to_string( ToEscaped( expr_value ) ) & """" );
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
     --identifiers( field_id ).referencedByThread := getThreadName;
  end if;
  expectSemicolon;
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
         err( "end record required with " & optional_bold( "pragma ada_95" ) );
      end if;
  else
      err( optional_bold( "end record" ) &
          " or " &
              optional_bold( "end " & to_string( identifiers( newtype_id ).name ) ) &
              " expected" );
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
   ab1         : unbounded_string; -- low array bound
   kind1       : identifier;
   ab2         : unbounded_string; -- high array bound
   kind2       : identifier;
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
       err( "array indexes must be scalar types" );
   end if;
   expect( symbol_t, ".." );
   ParseExpression( ab2, kind2 );
   if token = symbol_t and identifiers( token ).value.all = "," then
      err( "array of arrays not yet supported" );
   elsif ab1 = null_unbounded_string then
      err( "array index has no value" );
   elsif ab2 = null_unbounded_string then
      err( "array index has no value" );
   elsif type_checks_done or else baseTypesOK(kind1, kind2 ) then
      if isExecutingCommand and not syntax_check then  -- ab1/2 undef on synchk
         if to_numeric( ab1 ) > to_numeric( ab2 ) then
            if long_integer( to_numeric( ab1 ) ) /= 1 and
               long_integer( to_numeric( ab2 ) ) /= 0 then
               err( "first array bound is higher than last array bound" );
            end if;
         end if;
      end if;
   end if;
   expect( symbol_t, ")" );
   expect( of_t );
   if token = exception_t then
      err( "arrays of exceptions are not allowed" );
   end if;
   ParseIdentifier( elementType );                       -- parent type name

   -- Finish declaring the array
   --
   -- Note: Bounds are expressions and may not be defined during syntax check
  -- (Constant assignments, etc. occur only when actually running a script)

   elementBaseType := getBaseType( elementType );
   if token = symbol_t and identifiers( token ).value.all = ":=" then
      err( "assignment not allowed in an array type declaration" );
      b := deleteIdent( newtype_id );                       -- discard bad type
   elsif identifiers( elementBaseType ).list  then
      err( "array of arrays not yet supported" );
      b := deleteIdent( newtype_id );                       -- discard bad type
   elsif type_checks_done or else class_ok( elementType, typeClass, subClass ) then  -- item type OK?
      if isExecutingCommand and not syntax_check then       -- not on synchk
         identifiers( newtype_id ).firstBound := long_integer( to_numeric( ab1 ) );
         identifiers( newtype_id ).lastBound := long_integer( to_numeric( ab2 ) );
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
begin
   -- declare type_value
   if onlyAda95 then
      err( "affirm clauses are not allowed with " & optional_bold( "pragma ada_95" ) );
   else
      pushBlock(
        newScope => true,
        newName => affirm_clause_str,
        newThread => identifiers( newtype_id ).name & " affirm"
      );
      declareIdent( type_value_id, identifiers( newtype_id ).name, newtype_id );
      blockStart := firstPos;
      syntax_check := true;

      ParseAffirmBlock;

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
--  PARSE TYPE
--
-- Handle a user-defined type declaration.
-- Syntax: type = "type newtype is new [type-usage] oldtype [affirm clause]"
--         type = "type arraytype is array-type-part"
--         type = "type rectype is record-type-part"
-- NOTE: enumerateds aren't overloadable (yet)
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
         --identifiers( parent_id ).referencedByThread := getThreadName;
      end if;
      expect( symbol_t, "(" );                             -- "("
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
               --identifiers( newtype_id ).referencedByThread := getThreadName;
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
         expect( symbol_t, "," );                          -- ","
      end loop;
      expect( symbol_t, ")" );                             -- closing ")"
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
        err( optional_bold( "abstract" ) & " or " &
             optional_bold( "constant" ) & " or " &
             optional_bold( "limited" ) &  " goes after " &
             optional_bold( "new" ) );
      else
        err( "record or array expected" );
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

     if token = array_t then
        err( "omit " & optional_bold( "new" ) & " since array is not derived from another type" );
     elsif token = record_t then
        err( "omit " & optional_bold( "new" ) & " since record is not derived from another type" );
     end if;

     ParseIdentifier( parent_id );                         -- parent type name

     if not type_checks_done then
        if class_ok( parent_id, typeClass, subClass, genericTypeClass ) then    -- not a type?
           if identifiers( getBaseType( parent_id ) ).kind = root_record_t then
              -- TODO: we would have to generate all the field identifiers
              -- for the record, renamed for the new type, which is not done
              -- yet.  I will need this for objects later.
              err( "new types based on records not supported yet" );
            end if;
        end if;
     end if;

     -- Types are generally allowed to change the type usage in any way they want.
     -- As a special case, resources must always be limited.

     if identifiers( parent_id ).resource then
        if identifiers( newtype_id ).usage /= limitedUsage then
           err("resource identifiers must always be limited" );
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
        ParseGenericParametersPart( newtype_id );
        if not type_checks_done then
           CheckGenericParameterType( newtype_id, parent_id );
--put_line( "ParseType: Generics done" ); -- DEBUG
--put_identifier( identifiers( newtype_id ).genKind ); -- DEBUG
        end if;
        -- currently, it will always be limitedUsage
        identifiers( newtype_id ).usage := identifiers( parent_id ).usage;
     elsif token = symbol_t and identifiers( token ).value.all = "(" then
        err( "parameters were supplied but " &
             optional_bold( to_string( identifiers( parent_id ).name ) ) &
             " is not a generic type" );
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

     if token = affirm_t then
        ParseAffirmClause( newtype_id );
     elsif token /= symbol_t and identifiers( token ).value.all /= ";" then
        err( "affirm or ';' expected" );
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

   -- Types are generally allowed to change the type usage in any way they want.
   -- As a special case, resources must always be limited.

   if identifiers( parent_id ).resource then
      if identifiers( newtype_id ).usage /= limitedUsage then
         err("resource identifiers must always be limited" );
      end if;
      identifiers( newtype_id ).resource := true;
   end if;

   if identifiers( parent_id ).class = genericTypeClass then
      err( "subtypes require an instantiated generic type but " &
           optional_bold( to_string( identifiers( parent_id ).name ) ) &
           " is not instantiated" );
   elsif token = symbol_t and identifiers( token ).value.all = "(" then
      err( "parameters were supplied but " &
           optional_bold( to_string( identifiers( parent_id ).name ) ) &
           " is not a generic type" );
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
      if token = affirm_t then
         ParseAffirmClause( newtype_id );
      elsif token /= symbol_t and identifiers( token ).value.all /= ";" then
         err( "accept or ';' expected" );
      end if;
   end if;
end ParseSubtype;

end parser.decl;
