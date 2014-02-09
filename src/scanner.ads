
------------------------------------------------------------------------------
-- Lexical Scanner (the thing that reads your source code)                  --
-- Also, the semantic checking such as type mismatch checking.              --
--                                                                          --
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

with system,
  ada.unchecked_deallocation,
  ada.strings.unbounded,
  script_io,
  world,
  scanner_res;
use ada.strings.unbounded,
  script_io,
  world,
  scanner_res;

package scanner is

------------------------------------------------------------------------------
-- Scanner Global Variables / Flags
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Command Line Parameters
------------------------------------------------------------------------------

optionOffset: natural := 0;     -- offset to script parameters


------------------------------------------------------------------------------
-- Scripts
------------------------------------------------------------------------------

type scriptPtr is access all string;
procedure free is new ada.unchecked_deallocation( string, scriptPtr );
-- A script is a dynamically allocated fixed string for speed.

subtype aByteCodePosition is natural;
-- Index into a script

script     : scriptPtr := null;          -- current script / command

cmdpos     : aByteCodePosition := 0;     -- next char to read
firstpos   : aByteCodePosition := 0;     -- deliniates the last token
lastpos    : aByteCodePosition := 0;     -- deliniates the last token

firstScriptCommandOffset : constant aByteCodePosition := 8;
--firstScriptCommandOffset : constant aByteCodePosition := 13;
-- this is the first character of the first command.  however, we want
-- the end of line of the header or else it won't trigger the
-- trace output if trace is on.
nextScriptCommandOffset  : constant aByteCodePosition := 5;
-- position of first command is always 5 in from the top of the
-- byte code (for now)


------------------------------------------------------------------------------
-- Predefined identifiers
--
-- keywords, predefined types, virtual machine instructions and other global
-- identifiers are in the world package to make their values easily accessible
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Source_Info package identifiers
--
-- These will eventually be moved to the Source_Info parser
--
-- Most of these are constants known at startup, so they don't
-- have an identifier variable because we never have to refer to
-- them again.
------------------------------------------------------------------------------

source_info_file_t     : identifier; -- Gnat.Source_Info.File
source_info_line_t     : identifier; -- Gnat.Source_Info.Line
source_info_src_loc_t  : identifier; -- Gnat.Source_Info.Source_Location
source_info_enc_ent_t  : identifier; -- Gnat.Source_Info.Enclosing_Entity
source_info_script_size_t   : identifier; -- Bush-specific.  byte-code size
source_info_symbol_table_size_t : identifier; -- Bush-specific


------------------------------------------------------------------------------
-- Sound package identifiers
--
-- These will eventually be moved to the Sound parser
------------------------------------------------------------------------------


sound_play_t       : identifier;
sound_playcd_t     : identifier;
sound_stopcd_t     : identifier;
sound_mute_t       : identifier;
sound_unmute_t     : identifier;


------------------------------------------------------------------------------
-- BUSH O/S package identifiers
--
-- These will eventually be moved to the BUSH_OS parser
------------------------------------------------------------------------------

os_system_t       : identifier;
os_status_t       : identifier;

-- End of Identifier Declarations

------------------------------------------------------------------------------
-- Virtual Machine (VM)
--
-- VMNR - Virtual Machine Numeric Registers (universal_numeric)
-- VMSR - Virtual Machine String Registers (universal_string)
-- VMIR - Virtual Machine Index Registers (holding identifier id's)
--
-- This is under development
------------------------------------------------------------------------------

subtype aVMRegister is integer range 0..64;
noRegister : constant aVMRegister := aVMRegister'last;
-- 64 registers numbered 0 to 63, 64 indicates bad reg number

type aVMNRNumber is new aVMRegister;
type aVMSRNumber is new aVMRegister;
type aVMIRNumber is new aVMRegister;
-- register id numbers

type aVMNRBank is array( aVMNRNumber ) of unbounded_string;
type aVMSRBank is array( aVMSRNumber ) of unbounded_string;
type aVMIRBank is array( aVMIRNumber ) of identifier;
-- banks of registers

-- The Virutal Machine Registers

VMNR : aVMNRBank; -- the numeric registers
VMSR : aVMSRBank; -- the string registers
VMIR : aVMIRBank; -- the index registers


------------------------------------------------------------------------------
-- Scanner Subprograms
--
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Errors
--
------------------------------------------------------------------------------

procedure err( msg : string );
-- if this is the first error encountered, display the message
-- set the token to eof_t to abort the parsing and set the
-- error_found flag to indicate that an error was encountered

procedure raise_exception( msg : string );

procedure warn( msg : string );

------------------------------------------------------------------------------
-- Saving/Restoring Positions/Scripts
--
------------------------------------------------------------------------------

type aScannerState is private;

procedure markScanner( scannerState : out aScannerState );
-- mark the current position of the scanner, including the token

procedure resumeScanning( scannerState : aScannerState );
-- resume at a previously marked position, restoring the token

pragma inline( markScanner );
pragma inline( resumeScanning );

type aScriptState is private;

procedure saveScript( scriptState : out aScriptState );
-- Save scanner state plus the current script so that a new
-- script can be executed.  The error flag, syntax check flag,
-- etc. are not saved.

procedure restoreScript( scriptState : in out aScriptState );
-- Restore a previously saved script, destroying the current one
-- (if any).  Execution will continue where it previously left
-- off.

-- no need to inline these--OK if they are slower than mark/resume

------------------------------------------------------------------------------
-- Scope / Block Statement Control
--
------------------------------------------------------------------------------

type blockDeclaration is private;      -- Ident Scope: eg. for a for loop
blocks_top : block := block'first;     -- scope stack next position

procedure pushBlock( newScope : boolean := false;
  newName : string := "" );
-- start a new identifier scope, remember where we "parked"
-- if newScope is false, the new block is the start of a
-- multi-line statement (eg. an "if"); if true, the new
-- block is a nested declaration scope (eg. a "declare")

procedure checkIdentifiersForSimpleScripts;
-- check for unused variables and tally presence of software model req's
-- this is normally called automatically by pullBlock but is exposed
-- here for cases where pullBlock doesn't get run, such as simple scripts
-- with no blocks.

procedure completeSoftwareModelRequirements;
-- Check pre-defined identifiers for software requirements.  Then evaluate
-- if the requirements were met for the software model.  This should only be
-- run during the syntax check.

procedure pullBlock;
-- restore the previous identifier scope

procedure topOfBlock;
-- return to the top of the current block, read that line

procedure GetFullParentUnitName( fullUnitName : out unbounded_string );
-- return the full (dotted) name of the parent subprogram (e.g. proc )

function isLocal( id : identifier ) return boolean;
-- true if identifier is local to the current scope

function getBlockName( b : block ) return unbounded_string;
-- return the name of the given block

procedure dumpSymbolTable;
-- Debugging routine to display the top of the symbol table.

------------------------------------------------------------------------------
-- Type Checking / Verification
--
------------------------------------------------------------------------------

function getUniType( original : identifier ) return identifier;
-- dereference identifier until we find the universal type that this type
-- is based on.  quit if a circular relationship is suspected
-- errors result in type universal

function getBaseType( original : identifier ) return identifier;
-- dereference identifier until we find the original, parent root type
-- (ie for types declared with "subtype", roll them back to their
-- parent types)
-- quit if a circular relationship is suspected
-- errors result in type universal

function class_ok( id : identifier; class : anIdentifierClass ) return boolean;
function class_ok( id : identifier; c1,c2 : anIdentifierClass ) return boolean;
function class_ok( id : identifier; c1,c2,c3 : anIdentifierClass ) return boolean;
-- check identifier is of a certain class

function uniTypesOk( leftType, rightType : identifier ) return boolean;
-- check that the two types are extended from a common universal type

function baseTypesOk( leftType, rightType : identifier ) return boolean;
-- check that the two types are logically compatible

procedure baseTypesOk( leftType, rightType : identifier );
-- same as a procedure

function genTypesOk( leftType, rightType : identifier ) return boolean;
-- same as base types OK, but clearer error message for generic items

procedure genTypesOk( leftType, rightType : identifier );
-- same as a procedure


-- Type Casting
-----------------------------------------------------------------------------

function castToType( val : long_float; kind : identifier ) return unbounded_string;
function castToType( val : unbounded_string; kind : identifier ) return unbounded_string;
-- If a value is an integer type (i.e. positive, natural or integer),
-- round the value.  Otherwise do not round the value.  Return the
-- result as a string value.

function deleteIdent( id : identifier ) return boolean;
-- delete an identifier, true if successful

-----------------------------------------------------------------------------
-- JSON
-----------------------------------------------------------------------------

procedure DoJsonToString( result : out unbounded_string; expr_val : unbounded_string );
-- Convert a JSON string and return the string

procedure DoArrayToJson( result : out unbounded_string; source_var_id : identifier );
-- Convert an array to a JSON string.

procedure DoJsonToArray( target_var_id : identifier; source_val : unbounded_string );
-- Convert a JSON string and store in an array.

procedure DoRecordToJson( result : out unbounded_string; source_var_id : identifier );
-- Convert a record to a JSON string.

procedure DoJsonToRecord( target_var_id : identifier; sourceVal : unbounded_string );
-- Convert a JSON string and store in a record.

procedure DoJsonToNumber( jsonString : unbounded_string; expr_val : out unbounded_string );
-- convert a Json string to a numeric value

-----------------------------------------------------------------------------

function DoStringToJson( val : unbounded_string ) return unbounded_string;
-- Convert a string to JSON and return the string.

------------------------------------------------------------------------------
-- Scanning Tokens
--
------------------------------------------------------------------------------

procedure getNextToken;
-- the main scanner procedure, interpret the input text and
-- summarize it as a token.  Declare new, unknown identifiers
-- on the identifier stack.

procedure expect( expected_token : identifier );
-- check for the specified identifier.  If the current token matches,
-- get the next token otherwise show an error

procedure expect( expected_token : identifier; value : string );
-- check for the specified identifier and value.  If the current token
-- and its value matches, get the next token otherwise show an error

procedure expectSemicolon;
-- expect a semi-colon, check for colon

pragma inline( expect );
pragma inline( expectSemicolon );

------------------------------------------------------------------------------
-- Bourne Shell "Word" Processing
--
------------------------------------------------------------------------------

procedure skipWhiteSpace;
-- advance to first non-white space token

------------------------------------------------------------------------------
-- Housekeeping
--
------------------------------------------------------------------------------

procedure startScanner;
-- set up keywords, constants, and environment variables

procedure resetScanner;
-- restart the scanner, discarding all declarations

procedure shutdownScanner;
-- stop the scanner


------------------------------------------------------------------------------
-- Other Subprograms
--
------------------------------------------------------------------------------

procedure Put_Token;
-- for debugging, show the current token, its value, type and properities

procedure Put_Identifier( id : identifier );
-- show an identifier's name, value and attributes on standard output

procedure Put_All_Identifiers;
-- show all identifiers' name, value and attributes on standard output
-- in tabular format (used by env)

procedure Put_Trace( msg : string );
-- put this string if trace is true (on)

function inEnvironment( id : identifier ) return boolean;
-- true if this identifier is also in the O/S environment

procedure refreshVolatile( id : identifier );
-- reload this identifier's value from the environment

function getSourceFileName return unbounded_string;
-- Determine the current source file as stored against the byte code line.

function getLineNo return natural;
-- Determine the current line number as stored against the byte code line.

function getCommandLine return unbounded_string;
procedure getCommandLine ( cmdline : out unbounded_string;
  token_firstpos, token_lastpos, line_number, file_number : out natural );
-- de-tokenize and return the original command string

procedure compileTemplate( command : unbounded_string; lineno : natural );
-- compile the template tag command(s) into byte code

procedure compileCommand( command : unbounded_string;  firstLineNo : natural := 1 );
-- compile the command into byte code

procedure compileScript( firstLine : unbounded_string );
-- compile a script into byte code

function copyByteCodeLines( point1, point2 : natural ) return string;
-- copy the byte code lines containing point1 through point2

-- function createUserDefinedByteCode( byteCode : string ) return
--  unbounded_string;
-- Take byte code compiled in the script for a user-defined procedure or
-- function and add the necessary header/trailer code to make it a complete,
-- stand-alone script.

procedure replaceScript( bytecode : unbounded_string );
-- like compileCommand, but command is already compiled

procedure replaceScriptWithFragment( bytecode : unbounded_string );
-- like compileCommand, but command is already compiled (but is a fragment
-- out of another script)

procedure insertInclude( includeName : unbounded_string );
-- insert an include file or separate subunit into the byte code after the current position.


------------------------------------------------------------------------------
PRIVATE
------------------------------------------------------------------------------

type aScannerState is record
     token   : identifier;       -- the current token
     value   : unbounded_string; -- value (if symbol token)
     first   : natural;          -- firstpos value
     last    : natural;          -- lastpos value
     cmdpos  : natural;          -- cmdpos value
     lineno  : aLineNumber;      -- line number
     itself  : unbounded_string;   -- copy of the identifier declaration
     itself_type : identifier;         -- type of @ or procedure identifier
     last_output : unbounded_string;   -- result of last output
     last_output_type : identifier;    -- type of last output
     err_exception : declaration;      -- exception occurrence (if any)
end record;

type aScriptState is record
  scannerState : aScannerState;     -- scanning state of script
  script       : scriptPtr := null; -- the saved script
  size         : unbounded_string;  -- value of System.Script_Size
  inputMode    : anInputMode;       -- was interactive or not
end record;

type blockDeclaration is record
  startpos        : long_integer;     -- where in script file block starts
  identifiers_top : identifier;       -- where block declarations begin
  newScope        : boolean := false; -- true if new identifier scope
  blockName       : unbounded_string := Null_Unbounded_String;
  state           : aScannerState;    -- the position on the line
end record;
type blocksArray is array( block ) of blockDeclaration;
-- this arrangement means the last array element is never accessed

blocks : blocksArray; -- stack of scoping blocks / compound statements

end scanner;
