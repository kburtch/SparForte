------------------------------------------------------------------------------
-- The Symbol Table.  The identifier type is in the world package.          --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2025 Free Software Foundation            --
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

with ada.numerics.float_random,
  ada.unchecked_deallocation,
  ada.strings.unbounded,
  ada.calendar,
  pegasoft.gen_list,
  software_models,
  pegasock.memcache,
  spar_os,
  world;
use ada.strings.unbounded,
  ada.calendar,
  software_models,
  pegasoft,
  pegasock.memcache,
  spar_os,
  world;

package symbol_table is

------------------------------------------------------------------------------
--
-- Import/Export Methods
-- Note: http_cgi to avoid ambiguity over "cgi" package name

type anImportMethod is ( none, shell, http_cgi, local_memcache, memcache, session );
type anImportMapping is ( none, json );

------------------------------------------------------------------------------
--
-- Usage Qualifier
--
-- Limits on how an identifier may be used.
--
-- fullUsage     - read/write
-- abstractUsage - no declarations (legal only for types)
-- limitedUsage  - no expressions or assignment
-- constUsage    - read only (a constant)

type aUsageQualifier is ( fullUsage, abstractUsage, limitedUsage, constantUsage );

-- Each identifier has a single-dimension array of strings to hold the
-- identifier's value(s)

------------------------------------------------------------------------------
--
-- Callbacks to execute a build-in procedure or function (such as put_line).
------------------------------------------------------------------------------

type aBuiltinProcedureCallback is access procedure;
type aBuiltinFunctionCallback is access procedure( value : out storage; kind : out identifier );

------------------------------------------------------------------------------

-- Types of volatility

type aVolatileMode is (none, unchecked, checked);

------------------------------------------------------------------------------
-- Identifier Declarations
--
-- Optimization note: at least as of GNAT 4.6 on 64-bit Intel, using pointers
-- is about 4 times slower on sequential searches than using array indexes.
------------------------------------------------------------------------------
type declaration;

type declaration is record
     name            : unbounded_string        -- identifier's name
                := Null_Unbounded_String;
     value           : valuePtr := null;       -- identifier's value
     --value    : unbounded_string                -- identifier's value
     --           := Null_Unbounded_String;
     kind            : identifier;              -- identifier's type
     class           : anIdentifierClass        -- identifier's class
                       := otherClass;
     import          : boolean := false;        -- marked by pragma import
     method          : anImportMethod := none;  -- where to import values
     mapping         : anImportMapping := none; -- how to translate on import
     export          : boolean := false;        -- marked by pragma export
     volatile        : aVolatileMode := none;   -- marked by pragma volatile
                                                --   or unchecked_volatile
     volatileTTL     : duration := 0.0;         -- period assumed safe
     volatileExpire  : time;                    -- time of next reload
                                                -- (may be undefined)
     static          : boolean := false;        -- if value is known at syn chk
     usage           : aUsageQualifier := fullUsage; -- limits on use
     list            : boolean := false;        -- array or array type
     resource        : boolean := false;        -- resource type
     field_of        : identifier;              -- record superclass
     inspect         : boolean := false;        -- show value on breakout
     deleted         : boolean := false;        -- marked for deletion
     declaredAt      : natural := noSpec;       -- line where declared
     declaredFile    : unbounded_string;        -- file of declared
     specAt          : natural := noSpec;       -- line where forward spec
     specFile        : unbounded_string         -- file of forward spec
                       := Null_Unbounded_String;
     wasReferenced   : boolean := false;        -- true if ref'd in syn chk
     wasWritten      : boolean := false;        -- true if written in syn chk
     writtenByFlow   : aDataFlowName;
     writtenOn       : line_count;              -- side-effect protection
                                                --   when last written to
     wasApplied      : boolean := false;        -- true if type was used
     wasFactor       : boolean := false;        -- true if used in expression
     wasCastTo       : boolean := false;        -- true if used in typecast
     renaming_of     : identifier := identifier'first; -- renaming or dereference
     -- TODO: renaming should be a reference
     -- TODO: renaming should be about storage, not dereference whole ident
     renamed_count   : natural := 0;            -- number of times renamed
     passingMode     : aParameterPassingMode    -- in, out, in out, none
                       := none;

     -- New fields for symbol table

     procCB   : aBuiltinProcedureCallback := null;  -- built-in proc CB
     funcCB   : aBuiltinFunctionCallback := null;  -- built-in func CB
     genKind         : identifier;              -- generic type (if any)
     genKind2        : identifier;              -- generic type (if any)
     firstBound      : long_integer := 1;       -- first bound (array type only)
     lastBound       : long_integer := 0;       -- last bound (array type only)

     -- Programming by Contract

     contract : unbounded_string;               -- executable block

     -- Storage

     astorage    : storageGroupPtr := null;      -- array value (array variable)
     sstorage  : aliased storage;                -- identifier's value storage

     -- Namespaces

     openNamespace : identifier := identifier'first;       -- corresponding open (or null)
     nextNamespace : identifier := identifier'first;       -- next tag (descending)
     parentNamespace : identifier := identifier'first;     -- parent open tag
end record;

------------------------------------------------------------------------------
-- Symbol Table
--
-- The stack of all identifiers.
-- This arrangement means the last array element is never accessed
------------------------------------------------------------------------------

type identifiersArray is array ( identifier ) of declaration;

identifiers     : identifiersArray;
identifiers_top : identifier := identifier'first;
keywords_top    : identifier := identifier'last; -- last Ada keyword (xor)
reserved_top    : identifier := identifier'last; -- last keyword any kind
predefined_top  : identifier := identifier'last; -- last predefined ident
                                                 -- (like last built-in fn)
-- Too many symbols/identifiers.

symbol_table_overflow : exception;

------------------------------------------------------------------------------
-- Identifier References
--
-- These are for working with aggregate types, such as params passed into
-- pre-defined functions.
------------------------------------------------------------------------------

-- a scalar reference

type reference is record
     id    : identifier;             -- the identifier
     index : long_integer := 0;      -- the array index (if an array)
     kind  : identifier;             -- the type name
end record;

-- a scalar or aggregate reference
--
-- This is done separately to avoid feature creep.  I don't want to
-- re-engineer references at this time.

type renamingReference is record
     id    : identifier;             -- the identifier
     hasIndex : boolean := false;    -- true if index
     index : long_integer := 0;      -- the array index (if an array)
     kind  : identifier;             -- the type name
end record;

------------------------------------------------------------------------------
-- Predefined Identifiers (Global)
--
-- These identifiers always have meaning and are accessible from all other
-- packages.  They represent keywords and other unchangeable declarations
-- in the symbol table.
--
-- Keyword is the root of all identifiers in the symbol table tree.  It is
-- the type for any AdaScript keyword.
------------------------------------------------------------------------------

keyword_t : identifier;


------------------------------------------------------------------------------
-- Keywords
--
-- All Ada95 keywords are reserved even if they are not used by AdaScript
------------------------------------------------------------------------------

abort_t    : identifier;
abs_t      : identifier;
abstract_t : identifier;
accept_t   : identifier;
access_t   : identifier;
aliased_t  : identifier;
all_t      : identifier;
and_t      : identifier;
array_t    : identifier;
at_t       : identifier;
begin_t    : identifier;
body_t     : identifier;
case_t     : identifier;
constant_t : identifier;
declare_t  : identifier;
delay_t    : identifier;
delta_t    : identifier;
digits_t   : identifier;
do_t       : identifier;
else_t     : identifier;
elsif_t    : identifier;
end_t      : identifier;
entry_t    : identifier;
exception_t : identifier;
exit_t     : identifier;
for_t      : identifier;
function_t : identifier;
generic_t  : identifier;
goto_t     : identifier;
if_t       : identifier;
in_t       : identifier;
is_t       : identifier;
limited_t  : identifier;
loop_t     : identifier;
mod_t      : identifier;
new_t      : identifier;
not_t      : identifier;
null_t     : identifier;
of_t       : identifier;
or_t       : identifier;
others_t   : identifier;
out_t      : identifier;
package_t  : identifier;
pragma_t   : identifier;
private_t  : identifier;
procedure_t : identifier;
protected_t : identifier;
raise_t    : identifier;
range_t    : identifier;
record_t   : identifier;
rem_t      : identifier;
renames_t  : identifier;
requeue_t  : identifier;
return_t   : identifier;
reverse_t  : identifier;
select_t   : identifier;
separate_t : identifier;
subtype_t  : identifier;
tagged_t   : identifier;
task_t     : identifier;
terminate_t : identifier;
then_t     : identifier;
type_t     : identifier;
until_t    : identifier;
use_t      : identifier;
when_t     : identifier;
while_t    : identifier;
with_t     : identifier;
xor_t      : identifier;

------------------------------------------------------------------------------
--  AdaScript-specific keywords
------------------------------------------------------------------------------

affirm_t   : identifier;
copies_t   : identifier;
configuration_t : identifier;
meta_t     : identifier;
policy_t   : identifier;

------------------------------------------------------------------------------
--  Built-in Bourne shell commands
--
-- AdaScript shell commands that extend Ada 95 are also reserved words
------------------------------------------------------------------------------

env_t      : identifier;
typeset_t  : identifier;
umask_t    : identifier;
unset_t    : identifier;
trace_t    : identifier;
help_t     : identifier;
clear_t    : identifier;
jobs_t     : identifier;
logout_t   : identifier;
pwd_t      : identifier;  -- built-in pwd
cd_t       : identifier;  -- built-in cd
wait_t     : identifier;  -- built-in wait
step_t     : identifier;
-- template_t : identifier;
history_t  : identifier;  -- built-in history

------------------------------------------------------------------------------
--  Built-in SQL commands
--
------------------------------------------------------------------------------

alter_t  : identifier;
insert_t : identifier;
delete_t : identifier; -- (shared with Ada.Text_IO delete)
--select_t : identifier; -- (shared with Ada select declared above)
update_t : identifier;

-- Additional keywords not used by SparForte but are in Ada

interface_t    : identifier; -- Ada 2005
overriding_t   : identifier; -- Ada 2005
synchronized_t : identifier; -- Ada 2005
some_t         : identifier; -- Ada 2012

------------------------------------------------------------------------------
-- Other internal identifiers
--
-- EOF token, literals and virtual machine instructions.  Users should never
-- see these but they are all defined in the symbol table.
------------------------------------------------------------------------------

eof_t      : identifier;  -- end of file / abort script
symbol_t   : identifier;  -- punctuation/etc., value = string of punctuation
backlit_t  : identifier;  -- back quoted literal, value = the literal
strlit_t   : identifier;  -- string literal, value = the literal
charlit_t  : identifier;  -- character literal, value = the literal
number_t   : identifier;  -- numeric literal, value = the literal
imm_delim_t: identifier;  -- immediate word delimiter / identifier terminator
imm_sql_delim_t: identifier;  -- same for SQL word
imm_symbol_delim_t : identifier; -- same for shell symbols
word_t     : identifier;  -- immediate word value
sql_word_t : identifier;  -- a SQL word (not to be escaped)
char_escape_t : identifier; -- character escape
shell_symbol_t : identifier; -- shell symbol like "2>&1"


------------------------------------------------------------------------------
-- Predefined types
--
-- All Ada 95 fundamental types are declared, as well as AdaScript extensions.
--
-- variable_t is the root type of all variables and
-- is only used to mark the fundamental types.
--
-- the fundamental types are universal number, universal
-- string and universal (typeless).  All have .kind =
-- variable_t.
--
-- the basic Ada types are derived from the universal
-- types and have a .kind = some universal type.
--
-- user types are, of course, derived from the basic
-- Ada types and have a .kind = some basic Ada type
------------------------------------------------------------------------------

variable_t            : identifier;

uni_numeric_t         : identifier;
uni_string_t          : identifier;
universal_t           : identifier;
root_enumerated_t     : identifier;
root_record_t         : identifier;
command_t             : identifier;

file_type_t           : identifier;
socket_type_t         : identifier;
integer_t             : identifier;
natural_t             : identifier;
positive_t            : identifier;
short_short_integer_t : identifier;
short_integer_t       : identifier;
long_integer_t        : identifier;
long_long_integer_t   : identifier;
character_t           : identifier;
float_t               : identifier;
short_float_t         : identifier;
long_float_t          : identifier;
boolean_t             : identifier;
string_t              : identifier;
duration_t            : identifier;
file_mode_t           : identifier;
unbounded_string_t    : identifier;
complex_t             : identifier;
complex_real_t        : identifier;
complex_imaginary_t   : identifier;

false_t               : identifier; -- Boolean.false
true_t                : identifier; -- Boolean.true

json_string_t         : identifier;

------------------------------------------------------------------------------
-- Shortcut operands
--
-- The reflexive operand, itself, "@":
--
--   If itself's class is otherClass, it refers to a variable.
--     e.g. total := @+1;  itself is the value, itself_type is the type
--   If itself's class is procClass, it refers to a procedure.
--     e.g. put( "hello" ) @ ( "!" )  itself_type is the procedure id and
--     itself is unused
--   If itself_type is new_t, then itself is undefined.
--   To extend itself's capabilities, beware of side-effects.
------------------------------------------------------------------------------

itself      : unbounded_string;   -- copy of the identifier declaration
itself_type : identifier;         -- type of @ or procedure identifier

-- The last output operand, %

last_output : storage;   -- result of last output
last_output_type : identifier;    -- type of last output

-----------------------------------------------------------------------------
-- TINY HASH CACHE
--
-- A small identifier cache to speed up identifier symbol table lookups.
-----------------------------------------------------------------------------

--type tinyHashCacheEntry is record
--     key : unbounded_string;
--     id  : identifier;
--     cnt : natural;
--end record;
--
--type effectiveHash is new natural range 0..37;
--subtype actualHash is effectiveHash range 1..37;
--
--type aTinyHashCache is array( actualHash ) of tinyHashCacheEntry;
--
--tinyHashCache : aTinyHashCache;
--currentTinyHashCacheCnt : natural := 1;
--
--procedure resetTinyHashCache;
--procedure getTinyHashCache( s : unbounded_string; id : out identifier ; h : out actualHash );


------------------------------------------------------------------------------
-- Declarations
------------------------------------------------------------------------------

procedure declareKeyword( id : out identifier; s : string );
-- initialize a new keyword / internal identifier

procedure declareFunction( id : out identifier; s : string; cb : aBuiltinFunctionCallback := null );
-- Initialize a built-in function identifier in the symbol table

procedure declareProcedure( id : out identifier; s : string; cb : aBuiltinProcedureCallback := null );
-- Initialize a built-in procedure identifier in the symbol table

--function deleteIdent( id : identifier ) return boolean;
-- delete an identifier, true if successful
-- moved to scanner because this has become a complex operation

procedure declareIdent( id : out identifier; name : unbounded_string;
  kind : identifier; class : anIdentifierClass := varClass );
-- Declare an identifier in the symbol table, specifying name, kind.
-- and (optionally) symbol class.  The id is returned.

procedure declareIdent( id : out identifier; name : string;
  kind : identifier; class : anIdentifierClass := varClass );
-- Alternate version: use fixed string type for name

procedure declareStandardConstant( id : out identifier;
   name : string; kind : identifier; value : string );
-- Declare a standard constant in the symbol table.  The id is not
-- returned since we don't change with constants once they are set.

procedure declareStandardConstant( name : string; kind : identifier;
  value : string );
-- Alternative version: don't return the symbol table id

procedure declareStandardEnum( id : out identifier;
   name : string; kind : identifier; value : string );

procedure updateFormalParameter( id : identifier; kind : identifier;
  proc_id : identifier; parameterNumber : integer; passingMode :
  aParameterPassingMode );
-- Update a formal parameter (i.e. proc.param).  The id is not
-- returned since we don't change the formal parameters once they are set.

procedure declareUsableFormalParameter( id : out identifier;
proc_id : identifier; parameterNumber : integer;
value : unbounded_string; startingAt : in out identifier  );
-- Declare a usable formal parameter (i.e. param for proc.param).

procedure declareReturnResult( id : out identifier; func_id : identifier );
-- Declare space for the function return result.

type anExceptionStatusCode is new natural range 0..255;

procedure findException( name : unbounded_string; id : out identifier );
-- search for a pre-existing exception with the same name

procedure declareException( id : out identifier; name : unbounded_string;
   default_message : unbounded_string; exception_status_code : anExceptionStatusCode );
-- Declare an exception.  Check with findException first.

procedure declareRenaming( new_id : identifier; canonicalRef :
  renamingReference );
-- recast a new identifier as a renaming of the given reference

procedure declareRecordFields( parentRecordOfFieldsId, recordBaseTypeId : identifier );

procedure declareNamespace( name : string );
procedure declareGlobalNamespace;
procedure declareNamespaceClosed( name : string );
-- Declare a namespace

procedure findIdent( name : unbounded_string; id : out identifier );
-- find an identifier, eof_t if failed

procedure findEnumImage( val : unbounded_string; kind : identifier; name : out unbounded_string );
-- Find the name of the enumerated item of enumerated type kind with value
-- val.

procedure fixUsableParametersInAbstractSubprogram( sub_id : identifier );
-- Mark usable parameters as read/written in a null abstract subprogram

procedure init_env_ident( s : string );
-- initialize an environment variable

function is_keyword( id : identifier ) return boolean;
-- TRUE if the identifier is a keyword

procedure copyValue( to_decl : in out declaration; from_decl : declaration );
procedure copyValue( to_decl : in out declaration; from_id : identifier );
procedure copyValue( to_id, from_id : identifier );
-- Copy the value and type of one identifier to another

-----------------------------------------------------------------------------
-- Namespaces
--
-- These are initialized in startScanner
-----------------------------------------------------------------------------

currentNamespace    : unbounded_string;
currentNamespaceId  : identifier;
lastNamespaceId     : identifier;


-- Find fieldNumber'th field of a record variable.

procedure findField( recordVar : identifier; fieldNumber: natural;
  fieldVar : out identifier );

------------------------------------------------------------------------------
-- Error Handling
------------------------------------------------------------------------------

-- error_type    : anExceptionType;                       -- type of exception
err_message      : unbounded_string;                     -- last error message
fullErrorMessage : messageStrings;                  -- same with location info
err_exception    : declaration;           -- the exception else eof_t for none
-- err_exception must be a declaration because it can be propagated out of
-- the exception declaration scope

end symbol_table;
