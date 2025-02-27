------------------------------------------------------------------------------
-- Strings Package Parser                                                   --
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
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

--with text_io;

with interfaces.c,
    ada.strings.maps,
    ada.strings.unbounded.text_io,
    ada.streams.stream_io,
    ada.text_io,
    gnat.regexp,
    gnat.regpat,
    base64,
    spar_os,
    pegasoft.strings,
    world,
    scanner.communications,
    parser_aux,
    parser_params,
    parser,
    parser_strings_pcre;
use interfaces.c,
    ada.strings.maps,
    ada.strings.unbounded,
    ada.strings.unbounded.text_io,
    ada.streams.stream_io,
    gnat.regexp,
    gnat.regpat,
    base64,
    spar_os,
    pegasoft,
    pegasoft.strings,
    world,
    scanner,
    scanner.communications,
    parser_params,
    parser_aux,
    parser;

package body parser_strings is

defaultDelimiter : constant character := ASCII.CR;
-- default delimiter for string lookup functions

------------------------------------------------------------------------------
-- Strings package identifiers
--
-- These will eventually be moved to the Strings parser
------------------------------------------------------------------------------

base64_string_t  : identifier;
null_unbounded_string_t : identifier;

strings_alignment_t        : identifier; -- strings.alignment enumerated
strings_alignment_left_t   : identifier;
strings_alignment_right_t  : identifier;
strings_alignment_center_t : identifier;
strings_truncation_t       : identifier; -- strings.truncation enumerated
strings_truncation_left_t  : identifier;
strings_truncation_right_t : identifier;
strings_truncation_error_t : identifier;
strings_membership_t         : identifier; -- strings.membership enumerated
strings_membership_inside_t  : identifier;
strings_membership_outside_t : identifier;
strings_direction_t          : identifier; -- strings.direction enumerated
strings_direction_forward_t  : identifier;
strings_direction_backward_t : identifier;
strings_trim_end_t         : identifier; -- strings.trim_end enumerated
strings_trim_end_left_t    : identifier;
strings_trim_end_right_t   : identifier;
strings_trim_end_both_t    : identifier;
strings_sensitivity_t      : identifier;
strings_sensitivity_insensitive_t      : identifier;
strings_sensitivity_sensitive_t      : identifier;

val_t        : identifier;
image_t      : identifier;
glob_t      : identifier;  -- built-in gnat.regexp.match
match_t     : identifier;  -- built-in gnat.regpat.match
element_t    : identifier; -- ada.strings.unbounded
slice_t      : identifier;
index_t      : identifier;
index_non_blank_t : identifier;
count_t      : identifier;
replace_slice_t : identifier;
strings_insert_t : identifier;
overwrite_t  : identifier;
sdelete_t    : identifier;
trim_t       : identifier;
length_t     : identifier;
head_t       : identifier;
tail_t       : identifier;
to_string_t  : identifier;
to_u_string_t: identifier;
to_upper_t   : identifier; -- ada.character.handling
to_lower_t   : identifier;
to_proper_t  : identifier;
to_basic_t   : identifier;
to_escaped_t : identifier;
is_control_t : identifier;
is_graphic_t : identifier;
is_letter_t  : identifier;
is_upper_t   : identifier;
is_lower_t   : identifier;
is_basic_t   : identifier;
is_digit_t   : identifier;
is_hex_digit_t : identifier;
is_alphanumeric_t : identifier;
is_special_t : identifier;
is_slashed_date_t : identifier; -- other
is_fixed_t : identifier;
field_t      : identifier;
csv_field_t  : identifier;
lookup_t     : identifier;
replace_t    : identifier;
csv_replace_t : identifier;
split_t      : identifier;
mktemp_t     : identifier;
is_typo_of_t : identifier;
set_unbounded_string_t  : identifier;
unbounded_slice_t  : identifier;
strings_to_json_t : identifier;
to_base64_t     : identifier;
to_hex_digits_t : identifier;
levenshtein_t   : identifier;
soundex_t       : identifier;
replace_all_t   : identifier;
starts_with_t   : identifier;
ends_with_t     : identifier;
word_count_t    : identifier;
compare_t       : identifier;
index_set_t     : identifier;


------------------------------------------------------------------------------
--  PARSE STRINGS GLOB
--
-- Syntax: glob( expr, string )
-- Source: GNAT.RegExp.Match
------------------------------------------------------------------------------

procedure ParseStringsGlob( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
  pat_val   : unbounded_string;
  pat_type  : identifier;
  re        : regexp;
  b         : boolean;
begin
  kind := boolean_t;
  result := null_unbounded_string;
  expect( glob_t );
  ParseFirstStringParameter( glob_t, pat_val, pat_type );
  ParseLastStringParameter( glob_t, expr_val, expr_type );
  if isExecutingCommand then
     begin
       re := compile( to_string( pat_val ), glob => true,
             case_sensitive => true );
       b := match( to_string( expr_val ), re );
     exception when expression_error =>
       err( pl( "bad globbing expression '" & to_string( pat_val ) & "'" ) );
       b := false;
     when storage_error =>
       err( +"formula too complex (storage_error exception)" );
       b := false;
     when others =>
       err( +"exception raised in gnat.regexp.match" );
       b := false;
     end;
     if not error_found then
        if b then
           result := to_unbounded_string( "1" );
        else
           result := to_unbounded_string( "0" );
        end if;
     end if;
  end if;
end ParseStringsGlob;


------------------------------------------------------------------------------
--  PARSE STRINGS MATCH
--
-- Syntax: match( pat, expr )
-- Source: GNAT.RegPat.Match
------------------------------------------------------------------------------

procedure ParseStringsMatch( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
  pat_val   : unbounded_string;
  pat_type  : identifier;
  b         : boolean;
begin
  kind := boolean_t;
  result := null_unbounded_string;
  expect( match_t ); --getNextToken;
  ParseFirstStringParameter( match_t, pat_val, pat_type );
  ParseLastStringParameter( match_t, expr_val, expr_type );
  if isExecutingCommand then
     begin
       b := match( to_string( pat_val ), to_string( expr_val ) );
     exception when expression_error =>
       err( pl( "bad regular expression '" & to_string( pat_val ) & "'" ) );
       b := false;
     when storage_error =>
       err( pl( "formula too complex (storage_error exception)" ) );
       b := false;
     when program_error =>
       err( pl( "program_error exception raised gnat.regpat.match" ) );
       b := false;
     when others =>
       err( pl( "exception raised in gnat.regpat.match" ) );
       b := false;
     end;
     if not error_found then
        if b then
           result := to_unbounded_string( "1" );
        else
           result := to_unbounded_string( "0" );
        end if;
        if trace then
           if b then
              put_trace( "'" & toSecureData( to_string( toEscaped( pat_val ) ) ) &
                         "' pattern matches string '" &
                         toSecureData( to_string( toEscaped( expr_val ) ) ) &
                         "'" );
           else
              put_trace( "'" & toSecureData( to_string( toEscaped( pat_val ) ) ) &
                         "' pattern does not match string '" &
                         toSecureData( to_string( toEscaped( expr_val ) ) ) &
                         "'" );
           end if;
        end if;
     end if;
  end if;
end ParseStringsMatch;


------------------------------------------------------------------------------
--  PARSE STRINGS ELEMENT
--
-- Syntax: element( s, i )
-- Source: Ada.Strings.Unbounded.Element
------------------------------------------------------------------------------

procedure ParseStringsElement( result : out unbounded_string; kind : out identifier ) is
  str_val : unbounded_string;
  str_type : identifier;
  index_val : unbounded_string;
  index_type : identifier;
begin
  kind := character_t;
  expect( element_t );
  ParseFirstStringParameter( element_t, str_val, str_type, Uni_String_T );
  ParseLastNumericParameter( element_t, index_val, index_type, positive_t );
  begin
     if isExecutingCommand then
        result := to_unbounded_string( "" &
           Element( str_val, positive( to_numeric( index_val ) ) ) );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsElement;


------------------------------------------------------------------------------
--  PARSE STRINGS SLICE
--
-- Syntax: slice( s, l, h )
-- Source: Ada.Strings.Unbounded.Slice
------------------------------------------------------------------------------

procedure ParseStringsSlice( result : out unbounded_string; kind : out identifier ) is
  str_val  : unbounded_string;
  str_type : identifier;
  low_val  : unbounded_string;
  low_type : identifier;
  hi_val   : unbounded_string;
  hi_type  : identifier;
begin
  kind := uni_string_t;
  expect( slice_t );
  ParseFirstStringParameter( slice_t, str_val, str_type );
  ParseNextNumericParameter( slice_t, low_val, low_type, positive_t );
  ParseLastNumericParameter( slice_t, hi_val,  hi_type,  natural_t );
  begin
     if isExecutingCommand then
        result := to_unbounded_string(
           Slice( str_val,
             positive( to_numeric( low_val ) ),
             natural( to_numeric( hi_val ) )
           ) );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsSlice;


------------------------------------------------------------------------------
--  PARSE STRINGS INDEX
--
-- Syntax: strings.index( s, p [,d] )
-- Source: Ada.Strings.Unbounded.Index
------------------------------------------------------------------------------

procedure ParseStringsIndex( result : out unbounded_string; kind : out identifier ) is
  use ada.strings;
  str_val : unbounded_string;
  str_type : identifier;
  pat_val : unbounded_string;
  pat_type : identifier;
  dir_val : unbounded_string;
  dir_type : identifier;
  dir    : direction := forward;
begin
  kind := natural_t;
  expect( index_t );
  ParseFirstStringParameter( index_t, str_val, str_type );
  ParseNextStringParameter( index_t, pat_val, pat_type, Uni_String_T );
  -- no value if syntax check
  if isExecutingCommand then
     if length( pat_val ) = 0 then
        err( +"search string is empty" );
     end if;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( index_t, dir_val, dir_type, strings_direction_t );
     if isExecutingCommand then
        case natural( to_numeric( dir_val ) ) is
        when 0 => dir := forward;
        when 1 => dir := backward;
        when others =>
             err_exception_raised;
        end case;
     end if;
  else
    expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
        result := to_unbounded_string( Index( str_val, to_string( pat_val ), Going => dir )'img );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsIndex;


------------------------------------------------------------------------------
--  PARSE STRINGS INDEX NON BLANK
--
-- Syntax: strings.index_non_blank( s [, d] )
-- Source: Ada.Strings.Unbounded.Index_Non_Blank
------------------------------------------------------------------------------

procedure ParseStringsIndexNonBlank( result : out unbounded_string; kind : out identifier ) is
  use ada.strings;
  str_val : unbounded_string;
  str_type : identifier;
  dir_val : unbounded_string;
  dir_type : identifier;
  dir    : direction := forward;
begin
  kind := natural_t;
  expect( index_non_blank_t );
  ParseFirstStringParameter( index_non_blank_t, str_val, str_type );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( index_non_blank_t, dir_val, dir_type, strings_direction_t );
     if isExecutingCommand then
        case natural( to_numeric( dir_val ) ) is
        when 0 => dir := forward;
        when 1 => dir := backward;
        when others =>
             err_exception_raised;
         end case;
     end if;
  else
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
        result := to_unbounded_string( Index_Non_Blank( str_val, dir )'img );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsIndexNonBlank;


------------------------------------------------------------------------------
--  PARSE STRINGS INDEX SET
--
-- Syntax: strings.index_set( s, t [, f [, m [, d]]] )
-- Source: Ada.Strings.Unbounded.Index
------------------------------------------------------------------------------

procedure ParseStringsIndexSet( result : out unbounded_string; kind : out identifier ) is
  use ada.strings;
  str_val    : unbounded_string;
  str_type   : identifier;
  set_val    : unbounded_string;
  set_type   : identifier;
  first_val  : unbounded_string;
  first_type : identifier;
  test_val   : unbounded_string;
  test_type  : identifier;
  test       : membership := inside;
  dir_val    : unbounded_string;
  dir_type   : identifier;
  dir        : direction := forward;
begin
  kind := natural_t;
  expect( index_set_t );
  ParseFirstStringParameter( index_set_t, str_val, str_type );
  ParseNextStringParameter( index_set_t, set_val, set_type, uni_string_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseNextStringParameter( index_set_t, first_val, first_type, positive_t );
  else
     first_val := to_unbounded_string("1" );
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseNextStringParameter( index_set_t, test_val, test_type, strings_membership_t );
     if isExecutingCommand then
        if to_string( test_val ) = "1" then -- 1 is outside; TODO: cleaner way?
           test := outside;
        end if;
     end if;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( index_set_t, dir_val, dir_type, strings_direction_t );
     if isExecutingCommand then
        case natural( to_numeric( dir_val ) ) is
        when 0 => dir := forward;
        when 1 => dir := backward;
        when others =>
             err_exception_raised;
        end case;
     end if;
  else
    expect( symbol_t, ")" );
  end if;
  declare
     map : character_set;
     first : positive;
  begin
     map := to_set( to_string( set_val ) );
     first := positive( to_numeric( first_val ) );
     if isExecutingCommand then
        result := to_unbounded_string( Index( str_val, map, first, test, Going => dir )'img );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsIndexSet;


------------------------------------------------------------------------------
--  PARSE STRINGS COUNT
--
-- Syntax: strings.count( s, p )
-- Source: Ada.Strings.Unbounded.Count
------------------------------------------------------------------------------

procedure ParseStringsCount( result : out unbounded_string; kind : out identifier ) is
  str_val : unbounded_string;
  str_type : identifier;
  pat_val : unbounded_string;
  pat_type : identifier;
begin
  kind := natural_t;
  expect( count_t );
  ParseFirstStringParameter( count_t, str_val, str_type );
  ParseLastStringParameter( count_t, pat_val, pat_type, Uni_String_T );
  begin
     if isExecutingCommand then
        result := to_unbounded_string( Ada.Strings.Unbounded.Count( str_val,
           to_string( pat_val ) )'img );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsCount;


------------------------------------------------------------------------------
--  PARSE STRINGS REPLACE SLICE
--
-- Syntax: strings.replace_slice( s, l, h, b )
-- Source: Ada.Strings.Unbounded.Replace_Slice
------------------------------------------------------------------------------

procedure ParseStringsReplaceSlice( result : out unbounded_string; kind : out identifier ) is
  str_val : unbounded_string;
  str_type : identifier;
  low_val : unbounded_string;
  low_type : identifier;
  hi_val : unbounded_string;
  hi_type : identifier;
  by_val : unbounded_string;
  by_type : identifier;
begin
  kind := uni_string_t;
  expect( replace_slice_t );
  ParseFirstStringParameter( replace_slice_t, str_val, str_type );
  ParseNextNumericParameter( replace_slice_t, low_val, low_type, positive_t );
  ParseNextNumericParameter( replace_slice_t, hi_val,  hi_type,  natural_t );
  ParseLastStringParameter(  replace_slice_t, by_val,  by_type, Uni_String_T );
  begin
     if isExecutingCommand then
        result := Replace_Slice( str_val,
           positive( to_numeric( low_val ) ),
           natural( to_numeric( hi_val ) ),
           to_string( by_val )
        );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsReplaceSlice;


------------------------------------------------------------------------------
--  PARSE STRINGS REPLACE SLICE
--
-- Syntax: strings.replace_all( s, n, r, b )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsReplaceAll( result : out unbounded_string; kind : out identifier ) is
  str_val : unbounded_string;
  str_type : identifier;
  needle_val : unbounded_string;
  needle_type : identifier;
  newstr_val : unbounded_string;
  newstr_type : identifier;
  sensitivity_val : unbounded_string;
  sensitivity_type : identifier;
begin
  kind := uni_string_t;
  expectAdaScript( subject => replace_all_t );
  ParseFirstStringParameter( replace_all_t, str_val, str_type );
  ParseNextStringParameter( replace_all_t, needle_val, needle_type );
  ParseNextStringParameter( replace_all_t, newstr_val, newstr_type );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( replace_all_t, sensitivity_val,  sensitivity_type, strings_sensitivity_t );
  else
    expect( symbol_t, ")" );
    sensitivity_val := to_unbounded_string( "1" );
  end if;
  begin
     if isExecutingCommand then
        if length( needle_val ) = 0 then
           err( +"search substring is an empty string" );
        else
           result := replaceAll( str_val, needle_val, newstr_val, sensitivity_val = "1" );
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsReplaceAll;


------------------------------------------------------------------------------
--  PARSE STRINGS INSERT
--
-- Syntax: strings.insert( s, b, n )
-- Source: Ada.Strings.Unbounded.Replace_Slice
------------------------------------------------------------------------------

procedure ParseStringsInsert( result : out unbounded_string; kind : out identifier ) is
  str_val : unbounded_string;
  str_type : identifier;
  before_val : unbounded_string;
  before_type : identifier;
  new_val : unbounded_string;
  new_type : identifier;
begin
  kind := uni_string_t;
  expect( strings_insert_t );
  ParseFirstStringParameter( strings_insert_t, str_val, str_type );
  ParseNextNumericParameter( strings_insert_t, before_val, before_type, positive_t );
  ParseLastStringParameter( strings_insert_t, new_val, new_type, uni_string_T );
  begin
     if isExecutingCommand then
        result := Insert( str_val,
           positive( to_numeric( before_val ) ),
           to_string( new_val )
        );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsInsert;


------------------------------------------------------------------------------
--  PARSE STRINGS OVERWRITE
--
-- Syntax: strings.overwrite( s, p, n )
-- Source: Ada.Strings.Unbounded.Overwrite
------------------------------------------------------------------------------

procedure ParseStringsOverwrite( result : out unbounded_string; kind : out identifier ) is
  str_val  : unbounded_string;
  str_type : identifier;
  pos_val  : unbounded_string;
  pos_type : identifier;
  new_val  : unbounded_string;
  new_type : identifier;
begin
  kind := uni_string_t;
  expect( overwrite_t );
  ParseFirstStringParameter( overwrite_t, str_val, str_type );
  ParseNextNumericParameter( overwrite_t, pos_val, pos_type, positive_t );
  ParseLastStringParameter( overwrite_t, new_val, new_type, Uni_String_T );
  begin
     if isExecutingCommand then
        result := Overwrite( str_val,
           positive( to_numeric( pos_val ) ),
           to_string( new_val )
        );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsOverwrite;


------------------------------------------------------------------------------
--  PARSE STRINGS DELETE
--
-- Syntax: strings.delete( s, l, h )
-- Source: Ada.Strings.Unbounded.Delete
------------------------------------------------------------------------------

procedure ParseStringsDelete( result : out unbounded_string; kind : out identifier ) is
  str_val  : unbounded_string;
  str_type : identifier;
  low_val  : unbounded_string;
  low_type : identifier;
  hi_val   : unbounded_string;
  hi_type  : identifier;
begin
  kind := uni_string_t;
  expect( sdelete_t );
  ParseFirstStringParameter( sdelete_t, str_val, str_type );
  ParseNextNumericParameter( sdelete_t, low_val, low_type, positive_t );
  ParseLastNumericParameter( sdelete_t, hi_val,  hi_type,  natural_t );
  if isExecutingCommand then
     begin
        result := Delete( str_val,
           positive( to_numeric( low_val ) ),
           natural( to_numeric( hi_val ) )
        );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseStringsDelete;


------------------------------------------------------------------------------
--  PARSE STRINGS TRIM
--
-- Syntax: strings.trim( s , e )
-- Source: Ada.Strings.Unbounded.Trim
------------------------------------------------------------------------------

procedure ParseStringsTrim( result : out unbounded_string; kind : out identifier ) is
  use ada.strings;
  str_val : unbounded_string;
  str_type : identifier;
  trim_end_val : unbounded_string;
  trim_end_type : identifier;
  the_trim_end : trim_end := both;
  has_end : boolean := false;
begin
  kind := uni_string_t;
  expect( trim_t );
  ParseFirstStringParameter( trim_t, str_val, str_type );
  if token = symbol_t and identifiers( token ).value.all = "," then
     has_end := true;
     ParseLastEnumParameter( trim_t, trim_end_val, trim_end_type, strings_trim_end_t );
  else
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if has_end then
        case natural( to_numeric( trim_end_val ) ) is
        when 0 => the_trim_end := left;
        when 1 => the_trim_end := right;
        when 2 => the_trim_end := both;
        when others =>
           err_exception_raised;
        end case;
      end if;
  end if;
  begin
     if isExecutingCommand then
        result := trim( str_val, the_trim_end );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsTrim;


------------------------------------------------------------------------------
--  PARSE STRINGS TRIM
--
-- Syntax: strings.trim( s )
-- Source: Ada.Strings.Unbounded.Trim
------------------------------------------------------------------------------

procedure ParseStringsLength( result : out unbounded_string; kind : out identifier ) is
  str_val : unbounded_string;
  str_type : identifier;
begin
  kind := natural_t;
  expect( length_t );
  ParseSingleStringParameter( length_t, str_val, str_type );
  begin
     if isExecutingCommand then
        result := to_unbounded_string( length( str_val )'img );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsLength;


------------------------------------------------------------------------------
--  PARSE STRINGS HEAD
--
-- Syntax: strings.head( s, c [,p] )
-- Source: Ada.Strings.Unbounded.Head
------------------------------------------------------------------------------

procedure ParseStringsHead( result : out unbounded_string; kind : out identifier ) is
  str_val  : unbounded_string;
  str_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  pad_val  : unbounded_string;
  pad_type : identifier;
  pad_char : character := ' ';
begin
  kind := uni_string_t;
  expect( head_t );
  ParseFirstStringParameter( head_t, str_val, str_type );
  ParseNextNumericParameter( head_t, cnt_val, cnt_type, natural_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastStringParameter( head_t, pad_val, pad_type, character_t );
     begin
        pad_char := element( pad_val, 1 );
     exception when others =>
        err_exception_raised;
     end;
  else
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
        result := head( str_val, natural( to_numeric( cnt_val ) ),
            pad_char );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsHead;


------------------------------------------------------------------------------
--  PARSE STRINGS TAIL
--
-- Syntax: strings.tail( s, c [, p] )
-- Source: Ada.Strings.Unbounded.Tail
------------------------------------------------------------------------------

procedure ParseStringsTail( result : out unbounded_string; kind : out identifier ) is
  str_val  : unbounded_string;
  str_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  pad_val  : unbounded_string;
  pad_type : identifier;
  pad_char : character := ' ';
begin
  kind := uni_string_t;
  expect( tail_t );
  ParseFirstStringParameter( tail_t, str_val, str_type );
  ParseNextNumericParameter( tail_t, cnt_val, cnt_type, natural_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastStringParameter( tail_t, pad_val, pad_type, character_t );
     begin
        pad_char := element( pad_val, 1 );
     exception when others =>
        err_exception_raised;
     end;
  else
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
        result := tail( str_val, natural( to_numeric( cnt_val ) ),
          pad_char );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsTail;


------------------------------------------------------------------------------
--  PARSE STRINGS STARTS WITH
--
-- Syntax: strings.starts_with( s, t [,c] )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsStartsWith( result : out unbounded_string; kind : out identifier ) is
  str_val   : unbounded_string;
  str_type  : identifier;
  head_val  : unbounded_string;
  head_type : identifier;
  sensitivity_val  : unbounded_string;
  sensitivity_type : identifier;
begin
  kind := boolean_t;
  expectAdaScript( subject => starts_with_t );
  ParseFirstStringParameter( starts_with_t, str_val, str_type );
  ParseNextStringParameter( starts_with_t, head_val, head_type );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( starts_with_t, sensitivity_val, sensitivity_type, strings_sensitivity_t );
  else
    expect( symbol_t, ")" );
    sensitivity_val := to_unbounded_string( "1" );
  end if;
  begin
     if isExecutingCommand then
        if length( head_val ) = 0 then
           err( +"head substring is an empty string" );
        else
           if sensitivity_val = "1"  then
              result := to_spar_boolean( head( str_val, length( head_val ) ) = head_val );
           else
              result := to_spar_boolean( head( ToUpper( str_val ), length( head_val ) ) = ToUpper( head_val ) );
           end if;
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsStartsWith;


------------------------------------------------------------------------------
--  PARSE STRINGS ENDS WITH
--
-- Syntax: strings.ends_with( s, t [,c] )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsEndsWith( result : out unbounded_string; kind : out identifier ) is
  str_val   : unbounded_string;
  str_type  : identifier;
  tail_val  : unbounded_string;
  tail_type : identifier;
  sensitivity_val  : unbounded_string;
  sensitivity_type : identifier;
begin
  kind := boolean_t;
  expectAdaScript( subject => ends_with_t );
  ParseFirstStringParameter( ends_with_t, str_val, str_type );
  ParseNextStringParameter( ends_with_t, tail_val, tail_type );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( ends_with_t, sensitivity_val, sensitivity_type, strings_sensitivity_t );
  else
    expect( symbol_t, ")" );
    sensitivity_val := to_unbounded_string( "1" );
  end if;
  begin
     if isExecutingCommand then
        if length( tail_val ) = 0 then
           err( +"tail substring is an empty string" );
        else
           if sensitivity_val = "1"  then
              result := to_spar_boolean( tail( str_val, length( tail_val ) ) = tail_val );
           else
              result := to_spar_boolean( tail( ToUpper( str_val ), length( tail_val ) ) = ToUpper( tail_val ) );
           end if;
        end if;
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsEndsWith;


------------------------------------------------------------------------------
--  PARSE STRINGS FIELD
--
-- Syntax: strings.field( s, c [, d] )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsField( result : out unbounded_string; kind : out identifier ) is
  str_val  : unbounded_string;
  str_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  del_val  : unbounded_string;
  del_type : identifier;
  delim    : character := defaultDelimiter;
begin
  kind := uni_string_t;
  expectAdaScript( subject => field_t );
  ParseFirstStringParameter( field_t, str_val, str_type );
  ParseNextNumericParameter( field_t, cnt_val, cnt_type, natural_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastStringParameter( field_t, del_val, del_type, character_t );
     if isExecutingCommand then
        begin
          delim := element( del_val, 1 );
        exception when others =>
          err_exception_raised;
        end;
     end if;
  else
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
        result := stringField( str_val, delim, natural( to_numeric( cnt_val ) ) );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsField;


------------------------------------------------------------------------------
--  PARSE STRINGS CSV FIELD
--
-- Syntax: strings.csv_field( s, c [, d [, q]] )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsCSVField( result : out unbounded_string; kind : out identifier ) is
  str_val  : unbounded_string;
  str_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  del_val  : unbounded_string;
  del_type : identifier;
  delim    : character := ',';
  squotes_val  : unbounded_string;
  squotes_type : identifier;
  squotes : boolean := false;
begin
  kind := uni_string_t;
  expectAdaScript( subject => csv_field_t );
  ParseFirstStringParameter( csv_field_t, str_val, str_type );
  ParseNextNumericParameter( csv_field_t, cnt_val, cnt_type, natural_t );
  -- Optional delimiter
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseNextStringParameter( csv_field_t, del_val, del_type, character_t );
     if isExecutingCommand then
        begin
          delim := element( del_val, 1 );
        exception when others =>
          err_exception_raised;
        end;
     end if;
     -- Optional single quotes flag
     if token = symbol_t and identifiers( token ).value.all = "," then
        ParseLastEnumParameter( csv_field_t, squotes_val, squotes_type, boolean_t );
        if isExecutingCommand then
           begin
             squotes := to_string( squotes_val ) = "1";
           exception when others =>
             err_exception_raised;
           end;
        end if;
     else
        expect( symbol_t, ")" );
     end if;
  else
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
        result := stringCSVField( str_val, delim, natural( to_numeric( cnt_val ) ), squotes );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsCSVField;


------------------------------------------------------------------------------
--  PARSE STRINGS MKTEMP
--
-- Syntax: strings.mktemp
-- Source: SparForte builtin
------------------------------------------------------------------------------

procedure ParseStringsMkTemp( result : out unbounded_string; kind : out identifier ) is
  str_val : unbounded_string;
  str_type : identifier;
  mkstemp_result : aFileDescriptor;
  closeResult : int;
begin
  kind := uni_string_t;
  expectAdaScript( subject => mktemp_t );
  ParseSingleStringParameter( mktemp_t, str_val, str_type );
  if isExecutingCommand then
     declare
       LinuxPath : string := to_string( str_val ) & "XXXXXX" & ASCII.NUL;
     begin
       result := null_unbounded_string;
       mkstemp( mkstemp_result, LinuxPath );
       if mkstemp_result < 0 then
          err( pl( "mkstemp failed " & OSError( C_errno ) ) );
       else
          -- not the best.  mkstemp is secure because it leaves the
          -- file open but we're closing it anyway.
<<retry>> closeResult := close( mkstemp_result );
          if closeResult < 0 then
             if C_errno = EINTR then
                goto retry;
             end if;
          end if;
          for i in aLinuxPath'range loop
              exit when LinuxPath( i ) = ASCII.NUL;
              result := result & LinuxPath( i );
          end loop;
       end if;
     end;
  end if;
end ParseStringsMkTemp;


------------------------------------------------------------------------------
--  PARSE STRINGS VAL
--
-- Syntax: strings.val( natural );
-- Source: Ada 'val attribute
------------------------------------------------------------------------------

procedure ParseStringsVal( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := character_t;
  expect( val_t );
  ParseSingleNumericParameter( val_t, expr_val, expr_type, natural_t );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( "" & character'val( natural( to_numeric( expr_val ) ) ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsVal;


------------------------------------------------------------------------------
--  PARSE STRINGS VAL
--
-- Syntax: strings.image( x );
-- Source: Ada 'image attribute
------------------------------------------------------------------------------

procedure ParseStringsImage( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
  temp       : unbounded_string;
  isEnum     : boolean := false;
begin
  kind := uni_string_t;
  expect( image_t );

  -- Parse the Expression

  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  expect( symbol_t, ")" );

  -- Determine if it's an enumerated, numeric or neither

  if identifiers( expr_type ).kind = root_enumerated_t then
     isEnum := true;
  elsif identifiers( getBaseType( expr_type ) ).kind = root_enumerated_t then
     isEnum := true;
  elsif getUniType( expr_type ) /= uni_numeric_t then
     err( name_em( expr_type ) &
          pl( " is not an enumerated or numeric type" ) );
  end if;

  -- If it's an enumerated, return the name, not the value.

  begin
    if isExecutingCommand then
       if isEnum then
          -- In newer versions of GCC Ada, can't use expr_val for both
          -- parameters.
          findEnumImage( expr_val, expr_type, temp );
          expr_val := temp;
       end if;
       result := expr_val;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsImage;


------------------------------------------------------------------------------
--  PARSE STRINGS TO STRING
--
-- Syntax: strings.to_string( x );
-- Source: Ada.Strings.Unbounded.To_String
------------------------------------------------------------------------------

procedure ParseStringsToString( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
  baseType   : identifier;

  procedure DoBase64ToString( result : out unbounded_string; expr_val : unbounded_string ) is
    rawFile : ada.streams.stream_io.file_type;
    base64file : ada.text_io.file_type;
  begin
    -- The AdaPower base64 package requires streams and/or files.  It
    -- doesn't do strings.
    --
    -- Barnes' documentation is a bit sketchy.  I am faking this.  There may
    -- be a cleaner and more efficient way to do this.
    ada.text_io.create( base64File );
    ada.text_io.put( base64File, to_string( expr_val ) );
    ada.text_io.reset( base64File, ada.text_io.in_file );
    ada.streams.stream_io.create( rawFile );
    Decode_Stream( From => base64File, To => rawFile  );
    ada.streams.stream_io.reset( rawFile, ada.streams.stream_io.in_file );
    declare
       rawStream : ada.streams.stream_io.stream_access;
       ch : character;
    begin
       rawStream := ada.streams.stream_io.Stream( rawFile );
       while not ada.streams.stream_io.end_of_file( rawFile ) loop
          character'read( rawStream, ch );
          result := result & ch;
       end loop;
     end;
     ada.streams.stream_io.delete( rawFile );
     ada.text_io.delete( base64File );
   exception when MODE_ERROR =>
     err( +"internal_error: file mode error" );
   when status_error =>
     err( +"internal_error: status error - cannot open file" );
   when name_error =>
     err( +"internal_error: name error - cannot open fie" );
   when end_error =>
     err( +"internal_error: end error - end of file reached" );
   when others =>
     err_exception_raised;
  end DoBase64ToString;

begin
  kind := string_t;
  expect( to_string_t );
  ParseSingleStringParameter( to_string_t, expr_val, expr_type );
  baseType := getBaseType( expr_type );
  if baseType /= unbounded_string_t and baseType /= json_string_t and
     baseType /= base64_string_t then
     err( +"unbounded_string, json_string or strings.base64_string expected" );
  end if;
  begin
    if isExecutingCommand then
       if baseType = unbounded_string_t then
          result := expr_val;
       elsif baseType = json_string_t then
          DoJsonToString( result, expr_val );
       else
          DoBase64ToString( result, expr_val );
       end if;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsToString;


------------------------------------------------------------------------------
--  PARSE STRINGS TO UNBOUNDED STRING
--
-- Syntax: strings.to_unbounded_string( x ); or strings.to_unbounded_string( n );
-- Source: Ada.Strings.Unbounded.To_Unbounded_String
------------------------------------------------------------------------------

procedure ParseStringsToUString( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := unbounded_string_t;
  expect( to_u_string_t );
  ParseSingleStringParameter( to_u_string_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := expr_val;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsToUString;


------------------------------------------------------------------------------
--  PARSE STRINGS LOOKUP
--
-- Syntax: strings.lookup( s, t [, d] );
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsLookup( result : out unbounded_string; kind : out identifier ) is
  src_val  : unbounded_string;
  src_type : identifier;
  tar_val  : unbounded_string;
  tar_type : identifier;
  del_val  : unbounded_string;
  del_type : identifier;
  delim    : character := defaultDelimiter;
begin
  kind := uni_string_t;
  expect( lookup_t );
  ParseFirstStringParameter( lookup_t, src_val, src_type );
  ParseNextStringParameter( lookup_t, tar_val, tar_type );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastStringParameter( lookup_t, del_val, del_type, character_t );
     if isExecutingCommand then
        begin
          delim := element( del_val, 1 );
        exception when others =>
          err_exception_raised;
        end;
     end if;
  else
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
        result := stringLookup( src_val, tar_val, delim );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsLookup;


------------------------------------------------------------------------------
--  PARSE STRINGS LOOKUP
--
-- Syntax: strings.replace( s, f, t, [,d] );
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsReplace is
  src_ref  : reference;
  tar_val  : unbounded_string;
  tar_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  del_val  : unbounded_string;
  del_type : identifier;
  delim    : character := defaultDelimiter;
  tempStr  : unbounded_string;
begin
  expect( replace_t );
  expect( symbol_t, "(" );
  ParseInOutParameter( src_ref );
  if uniTypesOk( src_ref.kind, Uni_String_T ) then
     ParseNextNumericParameter( replace_t, cnt_val, cnt_type, natural_t );
     ParseNextStringParameter( replace_t, tar_val, tar_type, Uni_String_T );
     if token = symbol_t and identifiers( token ).value.all = "," then
        ParseLastStringParameter( replace_t, del_val, del_type, character_t );
        if isExecutingCommand then
           begin
             delim := element( del_val, 1 );
           exception when others =>
             err_exception_raised;
           end;
        end if;
     else
        expect( symbol_t, ")" );
     end if;
  end if;
  begin
     if isExecutingCommand then
        getParameterValue( src_ref, tempStr );
        replaceField( tempStr,
           delim,
           natural( to_numeric( cnt_val ) ),
           to_string( tar_val ) );
        AssignParameter( src_ref, tempStr );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsReplace;


------------------------------------------------------------------------------
--  PARSE STRINGS CSV REPLACE
--
-- Syntax: strings.csv_replace( s, f, t, [,d] [,q] );
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsCSVReplace is
  src_ref  : reference;
  tar_val  : unbounded_string;
  tar_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  del_val  : unbounded_string;
  del_type : identifier;
  delim    : character := ',';
  squotes_val  : unbounded_string;
  squotes_type : identifier;
  squotes : boolean := false;
  tempStr  : unbounded_string;
begin
  expectAdaScript( subject => csv_replace_t );
  expect( symbol_t, "(" );
  ParseInOutParameter( src_ref );
  if uniTypesOk( src_ref.kind, Uni_String_t ) then
     ParseNextNumericParameter( csv_replace_t, cnt_val, cnt_type, natural_t );
     ParseNextStringParameter( csv_replace_t, tar_val, tar_type );
     if token = symbol_t and identifiers( token ).value.all = "," then
        ParseNextStringParameter( csv_replace_t, del_val, del_type, character_t );
        if isExecutingCommand then
           begin
             delim := element( del_val, 1 );
           exception when others =>
             err_exception_raised;
           end;
        end if;
        -- Optional single quotes flag
        if token = symbol_t and identifiers( token ).value.all = "," then
           ParseLastEnumParameter( csv_replace_t, squotes_val, squotes_type, boolean_t );
           if isExecutingCommand then
              begin
                squotes := to_string( squotes_val ) = "1";
              exception when others =>
                err_exception_raised;
              end;
           end if;
        else
           expect( symbol_t, ")" );
        end if;
     else
        expect( symbol_t, ")" );
     end if;
  end if;
  begin
     if isExecutingCommand then
        getParameterValue( src_ref, tempStr );
        replaceCSVField( tempStr,
           delim,
           natural( to_numeric( cnt_val ) ),
           to_string( tar_val ),
           squotes );
        assignParameter( src_ref, tempStr );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsCSVReplace;


------------------------------------------------------------------------------
--  PARSE STRINGS TO UPPER
--
-- Syntax: strings.to_upper( s );
-- Source: Ada.Characters.Handling.To_Upper
------------------------------------------------------------------------------

procedure ParseStringsToUpper( result : out unbounded_string; kind : out identifier ) is
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_upper_t );
  ParseSingleStringParameter( to_upper_t, src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToUpper( src_val );
  end if;
end ParseStringsToUpper;


------------------------------------------------------------------------------
--  PARSE STRINGS TO LOWER
--
-- Syntax: strings.to_lower( s );
-- Source: Ada.Characters.Handling.To_Lower
------------------------------------------------------------------------------

procedure ParseStringsToLower( result : out unbounded_string; kind : out identifier ) is
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_lower_t );
  ParseSingleStringParameter( to_lower_t, src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToLower( src_val );
  end if;
end ParseStringsToLower;


------------------------------------------------------------------------------
--  PARSE STRINGS TO PROPER
--
-- Syntax: strings.to_proper( s );
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsToProper( result : out unbounded_string; kind : out identifier ) is
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_proper_t );
  ParseSingleStringParameter( to_proper_t, src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToProper( src_val );
  end if;
end ParseStringsToProper;


------------------------------------------------------------------------------
--  PARSE STRINGS TO BASIC
--
-- Syntax: strings.to_basic( s );
-- Source: Ada.Characters.Handling.To_Basic
------------------------------------------------------------------------------

procedure ParseStringsToBasic( result : out unbounded_string; kind : out identifier ) is
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_basic_t );
  ParseSingleStringParameter( to_basic_t, src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToBasic( src_val );
  end if;
end ParseStringsToBasic;


------------------------------------------------------------------------------
--  PARSE STRINGS TO ESCAPED
--
-- Syntax: strings.to_escaped( s );
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsToEscaped( result : out unbounded_string; kind : out identifier ) is
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expectAdaScript( subject => to_escaped_t );
  ParseSingleStringParameter( to_escaped_t, src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     begin
        result := ToEscaped( src_val );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseStringsToEscaped;


------------------------------------------------------------------------------
--  PARSE STRINGS SPLIT
--
-- Syntax: strings.split( s, l, r [,d] )
-- Source: N/A (GNAT.Case_Util sort of)
------------------------------------------------------------------------------

procedure ParseStringsSplit is
  src_val  : unbounded_string;
  src_type : identifier;
  left_ref : reference;
  right_ref: reference;
  field_val: unbounded_string;
  field_type : identifier;
  leftStr  : unbounded_string;
  rightStr : unbounded_string;
begin
  expectAdaScript( subject => split_t );
  ParseFirstStringParameter( split_t, src_val, src_type );
  ParseNextOutParameter( split_t, left_ref, Uni_String_T );
  ParseNextOutParameter( split_t, right_ref, Uni_String_T );
  ParseLastNumericParameter( split_t, field_val, field_type, natural_t );
  begin
     if isExecutingCommand then
        split( src_val, leftStr, rightStr,
               natural( to_numeric( field_val ) ) );
        assignParameter( left_ref, leftStr );
        assignParameter( right_ref, rightStr );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsSplit;


------------------------------------------------------------------------------
--  PARSE STRINGS IS CONTROL
--
-- Syntax: strings.is_control( x );
-- Source: Ada.Characters.Handling.Is_Control (except for string)
------------------------------------------------------------------------------

procedure ParseStringsIsControl( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_control_t );
  ParseSingleStringParameter( is_control_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_control( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsControl;


------------------------------------------------------------------------------
--  PARSE STRINGS IS GRAPHIC
--
-- Syntax: strings.is_graphic( x );
-- Source: Ada.Characters.Handling.Is_Graphic (except for string)
------------------------------------------------------------------------------

procedure ParseStringsIsGraphic( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_graphic_t );
  ParseSingleStringParameter( is_graphic_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_graphic( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsGraphic;


------------------------------------------------------------------------------
--  PARSE STRINGS IS LETTER
--
-- Syntax: strings.is_letter( x );
-- Source: Ada.Characters.Handling.Is_Letter (except for string)
------------------------------------------------------------------------------

procedure ParseStringsIsLetter( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_letter_t );
  ParseSingleStringParameter( is_letter_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_letter( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsLetter;


------------------------------------------------------------------------------
--  PARSE STRINGS IS LOWER
--
-- Syntax: strings.is_lower( x );
-- Source: Ada.Characters.Handling.Is_Lower (except for string)
------------------------------------------------------------------------------

procedure ParseStringsIsLower( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_lower_t );
  ParseSingleStringParameter( is_lower_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_lower( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsLower;


------------------------------------------------------------------------------
--  PARSE STRINGS IS LOWER
--
-- Syntax: strings.is_upper( x );
-- Source: Ada.Characters.Handling.Is_Upper (except for string)
------------------------------------------------------------------------------

procedure ParseStringsIsUpper( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_upper_t );
  ParseSingleStringParameter( is_upper_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_upper( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsUpper;


------------------------------------------------------------------------------
--  PARSE STRINGS IS BASIC
--
-- Syntax: strings.is_basic( x );
-- Source: Ada.Characters.Handling.Is_Basic (except for string)
------------------------------------------------------------------------------

procedure ParseStringsIsBasic( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_basic_t );
  ParseSingleStringParameter( is_basic_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_basic( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsBasic;


------------------------------------------------------------------------------
--  PARSE STRINGS IS DIGIT
--
-- Syntax: strings.is_digit( x );
-- Source: Ada.Characters.Handling.Is_Digit (except for string)
------------------------------------------------------------------------------

procedure ParseStringsIsDigit( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_digit_t );
  ParseSingleStringParameter( is_digit_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_digit( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsDigit;


------------------------------------------------------------------------------
--  PARSE STRINGS IS HEXADECIMAL DIGIT
--
-- Syntax: strings.is_hexadecimal_digit( x );
-- Source: Ada.Characters.Handling.Is_Hexadecimal_Digit (except for string)
------------------------------------------------------------------------------

procedure ParseStringsIsHexDigit( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_hex_digit_t );
  ParseSingleStringParameter( is_hex_digit_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_hexadecimal_digit( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsHexDigit;


------------------------------------------------------------------------------
--  PARSE STRINGS IS ALPHANUMERIC
--
-- Syntax: strings.is_alphanumeric( x );
-- Source: Ada.Characters.Handling.Is_Alphanumeric (except for string)
------------------------------------------------------------------------------

procedure ParseStringsIsAlphanumeric( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_alphanumeric_t );
  ParseSingleStringParameter( is_alphanumeric_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_alphanumeric( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsAlphanumeric;


------------------------------------------------------------------------------
--  PARSE STRINGS IS SPECIAL
--
-- Syntax: strings.is_special( x );
-- Source: Ada.Characters.Handling.Is_Special (except for string)
------------------------------------------------------------------------------

procedure ParseStringsIsSpecial( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_special_t );
  ParseSingleStringParameter( is_special_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_special( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsSpecial;


------------------------------------------------------------------------------
--  PARSE STRINGS IS SLASHED DATE
--
-- Syntax: strings.is_slashed_date( x );
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsIsSlashedDate( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expectAdaScript( subject => is_slashed_date_t );
  ParseSingleStringParameter( is_slashed_date_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_date( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsSlashedDate;


------------------------------------------------------------------------------
--  PARSE STRINGS IS FIXED
--
-- Syntax: strings.is_fixed( x );
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsIsFixed( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_fixed_t );
  ParseSingleStringParameter( is_fixed_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( is_fixed( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsFixed;


------------------------------------------------------------------------------
--  PARSE STRINGS TO BASE64
--
-- Syntax: strings.to_base64( s )
-- Source: base64.encode_stream
------------------------------------------------------------------------------

procedure ParseStringsToBase64( result : out unbounded_string; kind : out identifier ) is
  rawFile : ada.streams.stream_io.file_type;
  base64file : ada.text_io.file_type;
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  kind := base64_string_t;
  expectAdaScript( subject => to_base64_t );
  ParseSingleStringParameter( to_base64_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       -- The AdaPower base64 package requires streams and/or files.  It
       -- doesn't do strings.
       --
       -- Barnes' documentation is a bit sketchy.  I am faking this.  There may
       -- be a cleaner and more efficient way to do this.
       ada.streams.stream_io.create( rawFile );
       declare
          rawStream : ada.streams.stream_io.stream_access;
          s : constant string := to_string( expr_val );
       begin
          rawStream := ada.streams.stream_io.Stream( rawFile );
          string'write( rawStream, s );
       end;
       ada.streams.stream_io.reset( rawFile, ada.streams.stream_io.in_file );
       ada.text_io.create( base64File );
       Encode_Stream( From => rawFile, To => base64File  );
       ada.text_io.reset( base64File, ada.text_io.in_file );
       -- TODO: loop
       while not ada.text_io.end_of_file( base64File ) loop
          result := result & get_line( base64File );
       end loop;
       ada.streams.stream_io.delete( rawFile );
       ada.text_io.delete( base64file );
    end if;
  exception when MODE_ERROR =>
    err( +"internal_error: file mode error" );
  when status_error =>
    err( +"internal_error: status error - cannot open file" );
  when name_error =>
    err( +"internal_error: name error - cannot open fie" );
  when end_error =>
    err( +"internal_error: end error - end of file reached" );
  when others =>
    err_exception_raised;
  end;
end ParseStringsToBase64;


------------------------------------------------------------------------------
--  PARSE STRINGS IS TYPO OF
--
-- Syntax: strings.is_typo_of( x, y );
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsIsTypoOf( result : out unbounded_string; kind : out identifier ) is
  expr1_val   : unbounded_string;
  expr1_type  : identifier;
  expr2_val   : unbounded_string;
  expr2_type  : identifier;
begin
  kind := boolean_t;
  expectAdaScript( subject => is_typo_of_t );
  ParseFirstStringParameter( is_typo_of_t, expr1_val, expr1_type );
  ParseLastStringParameter( is_typo_of_t, expr2_val, expr2_type );
  begin
    if isExecutingCommand then
       result := to_spar_boolean( typoOf( expr1_val, expr2_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsTypoOf;


------------------------------------------------------------------------------
--  PARSE STRINGS UNBOUNDED SLICE
--
-- Syntax: unbounded_slice( s, l, h )
-- Source: Ada.Strings.Unbounded.Unbounded_Slice
------------------------------------------------------------------------------

procedure ParseStringsUnboundedSlice( result : out unbounded_string; kind : out identifier ) is
  str_val  : unbounded_string;
  str_type : identifier;
  low_val  : unbounded_string;
  low_type : identifier;
  hi_val   : unbounded_string;
  hi_type  : identifier;
begin
  kind := unbounded_string_t;
  expect( unbounded_slice_t );
  ParseFirstStringParameter( unbounded_slice_t, str_val, str_type );
  ParseNextNumericParameter( unbounded_slice_t, low_val, low_type, positive_t );
  ParseLastNumericParameter( unbounded_slice_t, hi_val,  hi_type,  natural_t );
  begin
     if isExecutingCommand then
        result := to_unbounded_string(
           Slice( str_val,
             positive( to_numeric( low_val ) ),
             natural( to_numeric( hi_val ) )
           ) );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsUnboundedSlice;


------------------------------------------------------------------------------
--  PARSE STRINGS SET UNBOUNDED STRING
--
-- Syntax: strings.set_unbounded_string( u, s );
-- Source: ada.strings.unbounded.set_unbounded_string
-- Ada 2005
------------------------------------------------------------------------------

procedure ParseStringsSetUnboundedString is
  src_ref  : reference;
  str_val  : unbounded_string;
  str_type : identifier;
begin
  expect( set_unbounded_string_t );
  expect( symbol_t, "(" );
  ParseInOutParameter( src_ref );
  if uniTypesOk( src_ref.kind, unbounded_string_t ) then
     ParseLastStringParameter( set_unbounded_string_t, str_val, str_type );
  end if;
  begin
     if isExecutingCommand then
        AssignParameter( src_ref, str_val );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsSetUnboundedString;


------------------------------------------------------------------------------
--  PARSE STRINGS TO JSON
--
-- Syntax: strings.to_json( x );
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsToJSON( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := json_string_t;
  expectAdaScript( subject => strings_to_json_t );
  ParseSingleStringParameter( strings_to_json_t, expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := DoStringToJSON( expr_val  );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsToJSON;


------------------------------------------------------------------------------
--  PARSE STRINGS PERL MATCH
--
-- Syntax: perl_match( s, e )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsPerlMatch( result : out unbounded_string; kind : out identifier ) is
begin
  parser_strings_pcre.ParseStringsPerlMatch( result, kind );
end ParseStringsPerlMatch;


------------------------------------------------------------------------------
--  PARSE STRINGS TO HEX DIGITS
--
-- Syntax: to_hexadecimal_digits( n )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsToHexDigits( result : out unbounded_string; kind : out identifier ) is
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := string_t;
  expectAdaScript( subject => to_hex_digits_t );
  ParseSingleNumericParameter( to_hex_digits_t, expr_val, expr_type, natural_t );
  if baseTypesOK( expr_type, natural_t ) then
     declare
       expr  : natural;
       digit : natural;
     begin
       if isExecutingCommand then
          expr  := natural'value( to_string( expr_val ) );
          if expr = 0 then
             result := to_unbounded_string( "0" );
          else
             while expr > 0 loop
                digit := expr mod 16;
                if digit < 10 then
                   result := character'val( 48 + digit ) & result;
                else
                   result := character'val( 65 + digit - 10 ) & result;
                end if;
                expr  := expr / 16;
             end loop;
          end if;
       end if;
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseStringsToHexDigits;


------------------------------------------------------------------------------
--  PARSE STRINGS LEVENSHTEIN
--
-- Syntax: levenshtein( s1, s2 )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsLevenshtein( result : out unbounded_string; kind : out identifier ) is
   str1_val  : unbounded_string;
   str1_type : identifier;
   str2_val  : unbounded_string;
   str2_type : identifier;
begin
  kind := natural_t;
  expectAdaScript( subject => levenshtein_t );
  ParseFirstStringParameter( levenshtein_t, str1_val, str1_type );
  ParseLastStringParameter( levenshtein_t, str2_val, str2_type );
  if baseTypesOK( str1_type, str2_type ) then
     if isExecutingCommand then
        begin
           result := to_unbounded_string( natural'image( Levenshtein_Distance( to_string( str1_val ), to_string( str2_val ) ) ) );
        exception when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParseStringsLevenshtein;


------------------------------------------------------------------------------
--  PARSE STRINGS SOUNDEX
--
-- Syntax: soundex( s )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsSoundex( result : out unbounded_string; kind : out identifier ) is
   str_val  : unbounded_string;
   str_type : identifier;
begin
  kind := string_t;
  expectAdaScript( subject => soundex_t );
  ParseSingleStringParameter( soundex_t, str_val, str_type );
  if isExecutingCommand then
     begin
        result := to_unbounded_string( Soundex( to_string( str_val ) ) );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseStringsSoundex;


------------------------------------------------------------------------------
--  PARSE STRINGS WORD COUNT
--
-- Syntax: word_count( s )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsWordCount( result : out unbounded_string; kind : out identifier ) is
   str_val  : unbounded_string;
   str_type : identifier;
begin
  kind := natural_t;
  expectAdaScript( subject => word_count_t );
  ParseSingleStringParameter( word_count_t, str_val, str_type );
  if isExecutingCommand then
     begin
        result := to_unbounded_string( WordCount( str_val )'img );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseStringsWordCount;


------------------------------------------------------------------------------
--  PARSE STRINGS COMPARE
--
-- Syntax: i := compare( s, t [,c [,l] ] )
-- Source: N/A
------------------------------------------------------------------------------

procedure ParseStringsCompare( result : out unbounded_string; kind : out identifier ) is
  first_val  : unbounded_string;
  first_type : identifier;
  last_val   : unbounded_string;
  last_type  : identifier;
  sensitivity_val  : unbounded_string;
  sensitivity_type : identifier;
  len_val    : unbounded_string;
  len_type   : identifier;
  compare_len : natural;
begin
  kind := integer_t;
  compare_len := natural'last;
  expectAdaScript( subject => compare_t );
  ParseFirstStringParameter( compare_t, first_val, first_type );
  ParseNextStringParameter( compare_t, last_val, last_type );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseNextEnumParameter( compare_t, sensitivity_val,  sensitivity_type, strings_sensitivity_t );
  else
    sensitivity_val := to_unbounded_string( "1" );
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastNumericParameter( compare_t, len_val, len_type, natural_t );
  else
    expect( symbol_t, ")" );
    compare_len := natural'last;
  end if;
  if isExecutingCommand then
     -- get the length, if there is one
     if len_val /= null_unbounded_string then
        compare_len := natural( to_numeric( len_val ) );
     end if;
     -- If there is a maximum length, reduce the two strings to that length
     if compare_len < natural'last then
        if length( first_val ) > compare_len then
           first_val := head( first_val, compare_len );
        end if;
        if length( last_val ) > compare_len then
           last_val := head( last_val, compare_len );
        end if;
     end if;
     -- if the test is insensitive, convert the strings to lower case
     if sensitivity_val = to_unbounded_string( "1" ) then
        first_val := ToLower( first_val );
        last_val := ToLower( last_val );
     end if;
     -- The compare is less than, greater than or equals test
     if first_val < last_val then
        result := to_unbounded_string( "-1" );
     elsif first_val > last_val then
        result := to_unbounded_string( " 1" );
     else
        result := to_unbounded_string( " 0" );
     end if;
  end if;
end ParseStringsCompare;


------------------------------------------------------------------------------
--
-- Housekeeping
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  STARTUP STRINGS
------------------------------------------------------------------------------

procedure StartupStrings is
begin
  declareNamespace( "strings" );
  declareIdent( base64_string_t, "strings.base64_string", string_t, typeClass );
  declareIdent( null_unbounded_string_t, "strings.null_unbounded_string", string_t, varClass );
  identifiers( null_unbounded_string_t ).usage := constantUsage;

  declareFunction( glob_t, "strings.glob", ParseStringsGlob'access );
  declareFunction( match_t, "strings.match", ParseStringsMatch'access );
  declareFunction( element_t, "strings.element", ParseStringsElement'access );
  declareFunction( slice_t, "strings.slice", ParseStringsSlice'access );
  declareFunction( index_t, "strings.index", ParseStringsIndex'access );
  declareFunction( index_non_blank_t, "strings.index_non_blank", ParseStringsIndexNonBlank'access );
  declareFunction( count_t, "strings.count", ParseStringsCount'access );
  declareFunction( replace_slice_t, "strings.replace_slice", ParseStringsReplaceSlice'access );
  declareFunction( strings_insert_t, "strings.insert", ParseStringsInsert'access );
  declareFunction( overwrite_t, "strings.overwrite", ParseStringsOverwrite'access );
  declareFunction( sdelete_t, "strings.delete", ParseStringsDelete'access );
  declareFunction( trim_t, "strings.trim", ParseStringsTrim'access );
  declareFunction( length_t, "strings.length", ParseStringsLength'access );
  declareFunction( head_t, "strings.head", ParseStringsHead'access );
  declareFunction( tail_t, "strings.tail", ParseStringsTail'access );
  declareFunction( val_t, "strings.val", ParseStringsVal'access );
  declareFunction( image_t, "strings.image", ParseStringsImage'access );
  declareFunction( field_t, "strings.field", ParseStringsField'access );
  declareFunction( csv_field_t, "strings.csv_field", ParseStringsCSVField'access );
  declareFunction( lookup_t, "strings.lookup", ParseStringsLookup'access );
  declareProcedure( replace_t, "strings.replace", ParseStringsReplace'access );
  declareProcedure( csv_replace_t, "strings.csv_replace", ParseStringsCSVReplace'access );
  declareFunction( to_upper_t, "strings.to_upper", ParseStringsToUpper'access );
  declareFunction( to_lower_t, "strings.to_lower", ParseStringsToLower'access );
  declareFunction( to_proper_t, "strings.to_proper", ParseStringsToProper'access );
  declareFunction( to_basic_t, "strings.to_basic", ParseStringsToBasic'access );
  declareFunction( to_escaped_t, "strings.to_escaped", ParseStringsToEscaped'access );
  declareFunction( is_control_t, "strings.is_control", ParseStringsIsControl'access );
  declareFunction( is_graphic_t, "strings.is_graphic", ParseStringsIsGraphic'access );
  declareFunction( is_letter_t, "strings.is_letter", ParseStringsIsLetter'access );
  declareFunction( is_lower_t, "strings.is_lower", ParseStringsIsLower'access );
  declareFunction( is_upper_t, "strings.is_upper", ParseStringsIsUpper'access );
  declareFunction( is_basic_t, "strings.is_basic", ParseStringsIsBasic'access );
  declareFunction( is_digit_t, "strings.is_digit", ParseStringsIsDigit'access );
  declareFunction( is_hex_digit_t, "strings.is_hexadecimal_digit", ParseStringsIsHexDigit'access );
  declareFunction( is_alphanumeric_t, "strings.is_alphanumeric", ParseStringsIsAlphanumeric'access  );
  declareFunction( is_special_t, "strings.is_special", ParseStringsIsSpecial'access );
  declareFunction( is_slashed_date_t, "strings.is_slashed_date", ParseStringsIsSlashedDate'access );
  declareFunction( is_fixed_t, "strings.is_fixed", ParseStringsIsFixed'access );
  declareProcedure( split_t, "strings.split", ParseStringsSplit'access );
  declareFunction( mktemp_t, "strings.mktemp", ParseStringsMkTemp'access );
  declareFunction( to_string_t, "strings.to_string", ParseStringsToString'access );
  declareFunction( to_u_string_t, "strings.to_unbounded_string", ParseStringsToUString'access );
  declareFunction( is_typo_of_t, "strings.is_typo_of", ParseStringsIsTypoOf'access );
  declareProcedure( set_unbounded_string_t, "strings.set_unbounded_string", ParseStringsSetUnboundedString'access );
  declareFunction( unbounded_slice_t, "strings.unbounded_slice", ParseStringsUnboundedSlice'access );
  declareFunction( strings_to_json_t, "strings.to_json", ParseStringsToJSON'access );
  declareFunction( to_base64_t, "strings.to_base64", ParseStringsToBase64'access );
  declareFunction( parser_strings_pcre.perl_match_t, "strings.perl_match", ParseStringsPerlMatch'access );
  declareFunction( to_hex_digits_t, "strings.to_hexadecimal_digits", ParseStringsToHexDigits'access );
  declareFunction( levenshtein_t, "strings.levenshtein", ParseStringsLevenshtein'access );
  declareFunction( soundex_t, "strings.soundex", ParseStringsSoundex'access );
  declareFunction( replace_all_t, "strings.replace_all", ParseStringsReplaceAll'access );
  declareFunction( starts_with_t, "strings.starts_with", ParseStringsStartsWith'access );
  declareFunction( ends_with_t, "strings.ends_with", ParseStringsEndsWith'access );
  declareFunction( word_count_t, "strings.word_count", ParseStringsWordCount'access );
  declareFunction( compare_t, "strings.compare", ParseStringsCompare'access );
  declareFunction( index_set_t, "strings.index_set", ParseStringsIndexSet'access );

  -- enumerateds - values defined below
  declareIdent( strings_alignment_t, "strings.alignment",
    root_enumerated_t, typeClass );
  declareIdent( strings_truncation_t, "strings.truncation",
    root_enumerated_t, typeClass );
  declareIdent( strings_membership_t, "strings.membership",
    root_enumerated_t, typeClass );
  declareIdent( strings_direction_t, "strings.direction",
    root_enumerated_t, typeClass );
  declareIdent( strings_trim_end_t, "strings.trim_end",
    root_enumerated_t, typeClass );
  declareIdent( strings_sensitivity_t, "strings.sensitivity",
    root_enumerated_t, typeClass );

  declareNamespaceClosed( "strings" );

  declareNamespace( "alignment" );
  declareStandardConstant( strings_alignment_left_t, "alignment.left",
    strings_alignment_t, "0" );
  declareStandardConstant( strings_alignment_right_t, "alignment.right",
    strings_alignment_t, "1" );
  declareStandardConstant( strings_alignment_center_t, "alignment.center",
    strings_alignment_t, "2" );
  declareNamespaceClosed( "alignment" );

  declareNamespace( "truncation" );
  declareStandardConstant( strings_truncation_left_t, "truncation.left",
    strings_truncation_t, "0" );
  declareStandardConstant( strings_truncation_right_t, "truncation.right",
    strings_truncation_t, "1" );
  declareStandardConstant( strings_truncation_error_t, "truncation.error",
    strings_truncation_t, "2" );
  declareNamespaceClosed( "truncation" );

  declareNamespace( "membership" );
  declareStandardConstant( strings_membership_inside_t, "membership.inside",
    strings_membership_t, "0" );
  declareStandardConstant( strings_membership_outside_t, "membership.outside",
    strings_membership_t, "1" );
  declareNamespaceClosed( "membership" );

  declareNamespace( "direction" );
  declareStandardConstant( strings_direction_forward_t, "direction.forward",
    strings_direction_t, "0" );
  declareStandardConstant( strings_direction_backward_t, "direction.backward",
    strings_direction_t, "1" );
  declareNamespaceClosed( "direction" );

  declareNamespace( "trim_end" );
  declareStandardConstant( strings_trim_end_left_t, "trim_end.left",
    strings_trim_end_t, "0" );
  declareStandardConstant( strings_trim_end_right_t, "trim_end.right",
    strings_trim_end_t, "1" );
  declareStandardConstant( strings_trim_end_both_t, "trim_end.both",
    strings_trim_end_t, "2" );
  declareNamespaceClosed( "trim_end" );

  declareNamespace( "sensitivity" );
  declareStandardConstant( strings_sensitivity_insensitive_t, "sensitivity.insensitive",
    strings_sensitivity_t, "0" );
  declareStandardConstant( strings_sensitivity_sensitive_t, "sensitivity.sensitive",
    strings_sensitivity_t, "1" );
  declareNamespaceClosed( "sensitivity" );

end StartupStrings;


------------------------------------------------------------------------------
--  SHUTDOWN STRINGS
------------------------------------------------------------------------------

procedure ShutdownStrings is
begin
  null;
end ShutdownStrings;

end parser_strings;
