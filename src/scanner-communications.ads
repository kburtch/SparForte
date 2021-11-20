------------------------------------------------------------------------------
-- Communications and Errors                                                --
--                                                                          --
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

with
  ada.strings.unbounded,
  world;
use ada.strings.unbounded,
  world;

package scanner.communications is

------------------------------------------------------------------------------
-- Old-style Errors
--
------------------------------------------------------------------------------

function get_script_execution_position( msg : string ) return unbounded_string;
-- get the script position but do not cause an error

procedure err_shell( msg : string; wordOffset : natural );
-- a shell error message with an offset into the token (shell word)

procedure err( msg : string );
-- if this is the first error encountered, display the message
-- set the token to eof_t to abort the parsing and set the
-- error_found flag to indicate that an error was encountered

procedure err_symbol_table_overflow;
-- fatal error.  the symbol table overflowed

procedure err_style( msg : string );
-- display a style error.  It is not an error if the script is unstructured.

procedure err_exception_raised;
-- generic error for when others => exceptions

procedure err_renaming( ident : identifier );
-- Show an error message for something disallowed for renaming

procedure err_test_result;
-- error for pragma test_result failure

procedure raise_exception( msg : string );

procedure warn( msg : string );

------------------------------------------------------------------------------
-- New-style Errors
--
------------------------------------------------------------------------------

type rootUserLanguage is abstract tagged null record;

procedure err(
    userLanguage    : rootUserLanguage;
    context         : identifier := eof_t;     -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    contextNotes    : string := "";            -- english description
    subject         : identifier := eof_t;     -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    subjectNotes    : string := "";            -- if not type issue
    reason          : string := "";            -- problem description
    obstructor      : identifier := eof_t;     -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    obstructorNotes : string := "";            -- if not type issue
    remedy          : string := "" ) is abstract; -- suggested solutions
-- Report an error but structure is based on the "reporter questions", to
-- describe the context around the error as briefly as possible.

type anyUserLanguagePtr is access all rootUserLanguage'class;


------------------------------------------------------------------------------
-- New-style Errors: English handling
------------------------------------------------------------------------------


type englishUserLanguage is new rootUserLanguage with private;


------------------------------------------------------------------------------
-- User Language Selection
------------------------------------------------------------------------------


userLanguage : anyUserLanguagePtr;
-- this is the error message language selection.  Only English is
-- currently defined.


------------------------------------------------------------------------------
-- New-style Errors: Error Functions
------------------------------------------------------------------------------


procedure err(
    context         : identifier := eof_t;     -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    contextNotes    : string := "";            -- notes
    subject         : identifier := eof_t;     -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    subjectNotes    : string := "";            -- if not type issue
    reason          : string := "";            -- problem description
    obstructor      : identifier := eof_t;     -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    obstructorNotes : string := "";            -- if not type issue
    remedy          : string := "" );          -- suggested solutions
-- Report an error but structure is based on the "reporter questions", to
-- describe the context around the error as briefly as possible.

procedure expect( expected_token : identifier );
-- check for the specified identifier.  If the current token matches,
-- get the next token otherwise show an error

procedure expect( expected_token : identifier; value : string );
-- check for the specified identifier and value.  If the current token
-- and its value matches, get the next token otherwise show an error

procedure expectSymbol(
    expectedValue   : string;
    expectPlural    : boolean := false;
    context         : identifier := eof_t;
    contextNotes    : string := "";
    subject         : identifier := eof_t;
    subjectType     : identifier := eof_t;
    subjectLocation : string := "";
    subjectNotes    : string := "";
    reason          : string := "";
    remedy          : string := "" );
-- Expect that structures the message like "reporter questions", with the
-- why (expects 'x') being provided.

procedure expectIdentifier( what, receivedDescription : string );

pragma inline( expectIdentifier );

-----------------------------------------------------------------------------
-- Missing Round Bracket / Paranthesis
-----------------------------------------------------------------------------

procedure expectStatementSemicolon( context : identifier := eof_t; contextNotes : string := "" );

procedure expectDeclarationSemicolon( context : identifier := eof_t; contextNotes : string := "" );

procedure expectReturnSemicolon;

pragma inline( expectStatementSemicolon );
pragma inline( expectDeclarationSemicolon );
pragma inline( expectReturnSemicolon );

-----------------------------------------------------------------------------
-- Missing Commas
-----------------------------------------------------------------------------

procedure expectParameterComma( subprogram : identifier := eof_t );
procedure expectPragmaComma;
-- expect a comma, check for semi-colon

pragma inline( expectParameterComma );

-----------------------------------------------------------------------------
-- Missing Round Bracket / Paranthesis
-----------------------------------------------------------------------------

procedure expectParameterOpen( subprogram : identifier := eof_t );

procedure expectParameterClose( subprogram : identifier := eof_t );

procedure expectPragmaParameterOpen( pragmaKind : string );

procedure expectPragmaParameterClose( pragmaKind : string );

pragma inline( expectParameterOpen );
pragma inline( expectParameterClose );

pragma inline( expect );

-----------------------------------------------------------------------------
-- Use of Non-Ada 95 Feature under Pragma Ada 95
-----------------------------------------------------------------------------

procedure expectSparForte(
    context         : identifier := eof_t;
    contextType     : identifier := eof_t;
    contextNotes    : string := "";
    subject         : identifier := eof_t;
    --subjectType     : identifier := eof_t;
    --subjectLocation : string := "";
    --subjectNotes    : string := "";
    remedy          : string := "" );

PRIVATE

type englishUserLanguage is new rootUserLanguage with null record;


-----------------------------------------------------------------------------
-- ERR
--
-- Report an error but structure is based on the "reporter questions", to
-- describe the context around the error as briefly as possible.
--
-- Parameter         Meaning                   Example
-- [subject]         The token in question     -
-- [subjectKind]     The type of token         -
-- [where]           Token alternate location  "(" declared at ")"
-- [what]            The situation             "For the in / not in range"
-- [whatKind]        The situation data type   universal_numeric
-- [why]             What the language wants   the range expects '..'
-- [how (to remedy)] Possible solutions        (Perhaps ...) -
--
-- See also expect functions.
--
-- It is a difficult balance to provide context without making the error
-- message too long.  I've noticed some other languages have tried to create
-- more descriptive error messages, or spend a lot of effort on the "why"
-- (how to correct the ere).  My goal here is to provide more context of
-- why a certain token is expected, rather than the correction.
-----------------------------------------------------------------------------

procedure err(
    userLanguage    : englishUserLanguage;
    context         : identifier := eof_t;     -- the parent or situation
    contextType     : identifier := eof_t;     -- associated type (if any)
    contextNotes    : string := "";            -- notes
    subject         : identifier := eof_t;     -- which
    subjectType     : identifier := eof_t;     -- which kind
    subjectLocation : string := "";            -- where it was
    subjectNotes    : string := "";            -- if not type issue
    reason          : string := "";            -- problem description
    obstructor      : identifier := eof_t;     -- ident causing problem
    obstructorType  : identifier := eof_t;     -- its type
    obstructorNotes : string := "";            -- if not type issue
    remedy          : string := "" );          -- suggested solutions
-- Report an error but structure is based on the "reporter questions", to
-- describe the context around the error as briefly as possible.
end scanner.communications;

