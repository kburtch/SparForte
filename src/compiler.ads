------------------------------------------------------------------------------
-- Byte Code Compiler                                                       --
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

with ada.unchecked_deallocation,
  ada.strings.unbounded,
  world;
use ada.strings.unbounded,
  world;

package compiler is

-----------------------------------------------------------------------------
-- HIGH ASCII CHARACTERS
--
-- These are the one-byte encoded characters for special VM opcodes.  They
-- are published so the scanner's getNextToken.
-----------------------------------------------------------------------------

immediate_word_delimiter     : character;
immediate_sql_word_delimiter : character;
immediate_symbol_delimiter   : character;
high_ascii_escape            : character;
eof_character                : character;

------------------------------------------------------------------------------
-- Scanner Global Variables / Flags
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Command Line Parameters
------------------------------------------------------------------------------

--optionOffset: natural := 0;     -- offset to script parameters


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


-----------------------------------------------------------------------------
-- Current Source File Location
-----------------------------------------------------------------------------

sourceFileNo   : natural := 0;
sourceLineNoLo : natural := 0;
sourceLineNoHi : natural := 0;


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


-- End of Identifier Declarations

-- List of reserved words for checking identifier names

reserved_words : unbounded_string;

------------------------------------------------------------------------------
-- Housekeeping
--
------------------------------------------------------------------------------

procedure startCompiler;

procedure resetCompiler;

procedure shutdownCompiler;


------------------------------------------------------------------------------
-- Information about the current script
--
------------------------------------------------------------------------------

function getSourceFileName return unbounded_string;
-- Determine the current source file as stored against the byte code line.

function getLineNo return natural;
-- Determine the current line number as stored against the byte code line.

function getCommandLine return unbounded_string;
procedure getCommandLine ( cmdline : out unbounded_string;
  token_firstpos, token_lastpos, line_number, file_number : out natural );
-- de-tokenize and return the original command string

------------------------------------------------------------------------------
-- Virtual Machine Registers
--
-- This is under development.  It is not used.
--
-- VMNR - Virtual Machine Numeric Registers (universal_numeric)
-- VMSR - Virtual Machine String Registers (universal_string)
-- VMIR - Virtual Machine Index Registers (holding identifier id's)
--
------------------------------------------------------------------------------

subtype aVMRegister is integer range 0..64;
noRegister : constant aVMRegister := aVMRegister'last;
-- 64 registers numbered 0 to 63, 64 indicates bad reg number

type aVMNRNumber is new aVMRegister;
type aVMSRNumber is new aVMRegister;
type aVMIRNumber is new aVMRegister;
-- register id numbers

------------------------------------------------------------------------------
-- Compiling into Byte Code
--
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Contexts
--
-- Commands like "cd bush-0.9.1", if threated as AdaScript, will report an
-- error: the compiler, not knowing this is a shell command, will see 0.9.1
-- as a malformed numeric literal.
--
-- The compiler needs to have some minimal context info to determine if it's
-- looking Bourne shell parameters, AdaScript, SQL, etc.  To avoid writing a
-- recursive version of line2ByteCode, we'll use an enumerated variable to
-- represent the the parsing history that we need.  The history must be
-- carried from one byte code line to the next.
--
-- startOfStatement  - at start of script or after last command
-- startOfParameters - a command and we're looking for parameters
-- shellParameters   - no '(' so it's Bourne shell parameters
-- etc.
------------------------------------------------------

type compressionContext is (
   startOfStatement,
   startOfParameters,
   startOfDeleteParameters,
   isPart,
   adaScriptStatement,
   shellStatement,
   SQLStatement );

------------------------------------------------------
-- General Purpose Register Assignment
--
-- not implemented
------------------------------------------------------------------------------

type aVMNRMapping is array( aVMNRNumber ) of unbounded_string;
type aVMSRMapping is array( aVMSRNumber ) of unbounded_string;
type aVMIRMapping is array( aVMIRNumber ) of identifier;
-- the association of which variable with a register

type compressionInfo is record
     compressedScript : unbounded_string;

     VMNRmap  : aVMNRMapping;     -- not implemented
     nextVMNR : aVMNRNumber := 0;

     VMSRmap  : aVMSRMapping;     -- not implemented
     nextVMSR : aVMSRNumber := 0;

     VMIRmap  : aVMIRMapping;     -- not implemented
     nextVMIR : aVMIRNumber := 0;

     context : compressionContext := startOfStatement;
end record;

procedure beginByteCode( ci : in out compressionInfo );
-- start compiling: create the byte code header

procedure endByteCode( ci : in out compressionInfo );
-- finish compiling: create the byte code footer

procedure resetLineNo;
-- set the line number back to zero for compiling a new file

procedure compileTemplate( command : unbounded_string; lineno : natural );
-- compile the template tag command(s) into byte code

procedure compileCommand( command : unbounded_string;  firstLineNo : natural := 1 );
-- compile the command into byte code

procedure compileScript( firstLine : unbounded_string );
-- compile a script into byte code

procedure compileInclude( command : unbounded_string );
-- Compile into byte code a command typed interactively at the command prompt
-- or backquotes or templates.

function copyByteCodeLines( point1, point2 : natural ) return string;
-- copy the byte code lines containing point1 through point2

procedure staticByteCodeAnalysis;
-- perform systatic byte code analysis during --perf option

------------------------------------------------------------------------------
PRIVATE
------------------------------------------------------------------------------

-- not yet implemented

type aVMNRBank is array( aVMNRNumber ) of unbounded_string;
type aVMSRBank is array( aVMSRNumber ) of unbounded_string;
type aVMIRBank is array( aVMIRNumber ) of identifier;
-- banks of registers

-- The Virutal Machine Registers

VMNR : aVMNRBank; -- the numeric registers
VMSR : aVMSRBank; -- the string registers
VMIR : aVMIRBank; -- the index registers

end compiler;
