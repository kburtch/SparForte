------------------------------------------------------------------------------
-- Strings Package Parser                                                   --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2016 Free Software Foundation              --
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

--with text_io;use text_io;

with interfaces.c,
    ada.strings.unbounded.text_io,
    ada.streams.stream_io,
    ada.text_io,
    gnat.regexp,
    gnat.regpat,
    base64,
    spar_os,
    world,
    scanner,
    string_util,
    user_io,
    parser_aux,
    parser_params,
    parser;
use interfaces.c,
    ada.strings.unbounded,
    ada.strings.unbounded.text_io,
    ada.streams.stream_io,
    gnat.regexp,
    gnat.regpat,
    base64,
    spar_os,
    world,
    scanner,
    string_util,
    user_io,
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
to_base64_t  : identifier;

procedure ParseSingleStringExpression( expr_val : out unbounded_string;
  expr_type : out identifier ) is
begin
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
end ParseSingleStringExpression;

procedure ParseStringsGlob( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: glob( expr, string )
  -- Source: GNAT.RegExp.Match
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
  ParseFirstStringParameter( pat_val, pat_type );
  ParseLastStringParameter( expr_val, expr_type );
  if isExecutingCommand then
     begin
       re := compile( to_string( pat_val ), glob => true,
             case_sensitive => true );
       b := match( to_string( expr_val ), re );
     exception when expression_error =>
       err( "bad globbing expression '" & to_string( pat_val ) & "'" );
       b := false;
     when storage_error =>
       err( "formula too complex (storage_error exception)" );
       b := false;
     when others =>
       err( "exception raised in gnat.regexp.match" );
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

procedure ParseStringsMatch( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: match( pat, expr )
  -- Source: GNAT.RegPat.Match
  expr_val  : unbounded_string;
  expr_type : identifier;
  pat_val   : unbounded_string;
  pat_type  : identifier;
  b         : boolean;
begin
  kind := boolean_t;
  result := null_unbounded_string;
  expect( match_t ); --getNextToken;
  ParseFirstStringParameter( pat_val, pat_type );
  ParseLastStringParameter( expr_val, expr_type );
  if isExecutingCommand then
     begin
       b := match( to_string( pat_val ), to_string( expr_val ) );
     exception when expression_error =>
       err( "bad regular expression '" & to_string( pat_val ) & "'" );
       b := false;
     when storage_error =>
       err( "formula too complex (storage_error exception)" );
       b := false;
     when program_error =>
       err( "program_error exception raised gnat.regpat.match" );
       b := false;
     when others =>
       err( "exception raised in gnat.regpat.match" );
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
              put_trace( "'" & to_string( pat_val ) &
                         "' pattern matches string '" &
                         to_string( expr_val ) &
                         "'" );
           else
              put_trace( "'" & to_string( pat_val ) &
                         "' pattern does not match string '" &
                         to_string( expr_val ) &
                         "'" );
           end if;
        end if;
     end if;
  end if;
end ParseStringsMatch;

procedure ParseStringsElement( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: element( s, i )
  -- Source: Ada.Strings.Unbounded.Element
  str_val : unbounded_string;
  str_type : identifier;
  index_val : unbounded_string;
  index_type : identifier;
begin
  kind := character_t;
  expect( element_t );
  ParseFirstStringParameter( str_val, str_type, string_t );
  ParseLastNumericParameter( index_val, index_type, positive_t );
  begin
     if isExecutingCommand then
        result := to_unbounded_string( "" &
           Element( str_val, positive( to_numeric( index_val ) ) ) );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsElement;

procedure ParseStringsSlice( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: slice( s, l, h )
  -- Source: Ada.Strings.Unbounded.Slice
  str_val  : unbounded_string;
  str_type : identifier;
  low_val  : unbounded_string;
  low_type : identifier;
  hi_val   : unbounded_string;
  hi_type  : identifier;
begin
  kind := string_t;
  expect( slice_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseNextNumericParameter( low_val, low_type, positive_t );
  ParseLastNumericParameter( hi_val,  hi_type,  natural_t );
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

procedure ParseStringsIndex( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.index( s, p [,d] )
  -- Source: Ada.Strings.Unbounded.Index
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
  ParseFirstStringParameter( str_val, str_type );
  ParseNextStringParameter( pat_val, pat_type, string_t );
  -- no value if syntax check
  if isExecutingCommand then
     if length( pat_val ) = 0 then
        err( "search string is empty" );
     end if;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( dir_val, dir_type, strings_direction_t );
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

procedure ParseStringsIndexNonBlank( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.index_non_blank( s [, d] )
  -- Source: Ada.Strings.Unbounded.Index_Non_Blank
  use ada.strings;
  str_val : unbounded_string;
  str_type : identifier;
  dir_val : unbounded_string;
  dir_type : identifier;
  dir    : direction := forward;
begin
  kind := natural_t;
  expect( index_non_blank_t );
  ParseFirstStringParameter( str_val, str_type );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastEnumParameter( dir_val, dir_type, strings_direction_t );
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

procedure ParseStringsCount( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.count( s, p )
  -- Source: Ada.Strings.Unbounded.Count
  str_val : unbounded_string;
  str_type : identifier;
  pat_val : unbounded_string;
  pat_type : identifier;
begin
  kind := natural_t;
  expect( count_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseLastStringParameter( pat_val, pat_type, string_t );
  begin
     if isExecutingCommand then
        result := to_unbounded_string( Ada.Strings.Unbounded.Count( str_val,
           to_string( pat_val ) )'img );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsCount;

procedure ParseStringsReplaceSlice( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.replace_slice( s, l, h, b )
  -- Source: Ada.Strings.Unbounded.Replace_Slice
  str_val : unbounded_string;
  str_type : identifier;
  low_val : unbounded_string;
  low_type : identifier;
  hi_val : unbounded_string;
  hi_type : identifier;
  by_val : unbounded_string;
  by_type : identifier;
begin
  kind := string_t;
  expect( replace_slice_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseNextNumericParameter( low_val, low_type, positive_t );
  ParseNextNumericParameter( hi_val,  hi_type,  natural_t );
  ParseLastStringParameter(  by_val,  by_type, string_t );
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

procedure ParseStringsInsert( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.insert( s, b, n )
  -- Source: Ada.Strings.Unbounded.Replace_Slice
  str_val : unbounded_string;
  str_type : identifier;
  before_val : unbounded_string;
  before_type : identifier;
  new_val : unbounded_string;
  new_type : identifier;
begin
  kind := string_t;
  expect( strings_insert_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseNextNumericParameter( before_val, before_type, positive_t );
  ParseLastStringParameter( new_val, new_type, string_t );
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

procedure ParseStringsOverwrite( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.overwrite( s, p, n )
  -- Source: Ada.Strings.Unbounded.Overwrite
  str_val  : unbounded_string;
  str_type : identifier;
  pos_val  : unbounded_string;
  pos_type : identifier;
  new_val  : unbounded_string;
  new_type : identifier;
begin
  kind := string_t;
  expect( overwrite_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseNextNumericParameter( pos_val, pos_type, positive_t );
  ParseLastStringParameter( new_val, new_type, string_t );
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

procedure ParseStringsDelete( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.delete( s, l, h )
  -- Source: Ada.Strings.Unbounded.Delete
  str_val  : unbounded_string;
  str_type : identifier;
  low_val  : unbounded_string;
  low_type : identifier;
  hi_val   : unbounded_string;
  hi_type  : identifier;
begin
  kind := string_t;
  expect( sdelete_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseNextNumericParameter( low_val, low_type, positive_t );
  ParseLastNumericParameter( hi_val,  hi_type,  natural_t );
  begin
     if isExecutingCommand then
        result := Delete( str_val,
           positive( to_numeric( low_val ) ),
           natural( to_numeric( hi_val ) )
        );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsDelete;

procedure ParseStringsTrim( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.trim( s , e )
  -- Source: Ada.Strings.Unbounded.Trim
  use ada.strings;
  str_val : unbounded_string;
  str_type : identifier;
  trim_end_val : unbounded_string;
  trim_end_type : identifier;
  the_trim_end : trim_end := both;
  has_end : boolean := false;
begin
  kind := string_t;
  expect( trim_t );
  ParseFirstStringParameter( str_val, str_type );
  if token = symbol_t and identifiers( token ).value.all = "," then
     has_end := true;
     ParseLastEnumParameter( trim_end_val, trim_end_type, strings_trim_end_t );
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

procedure ParseStringsLength( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.trim( s )
  -- Source: Ada.Strings.Unbounded.Trim
  str_val : unbounded_string;
  str_type : identifier;
begin
  kind := natural_t;
  expect( length_t );
  ParseSingleStringParameter( str_val, str_type );
  begin
     if isExecutingCommand then
        result := to_unbounded_string( length( str_val )'img );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsLength;

procedure ParseStringsHead( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.head( s, c [,p] )
  -- Source: Ada.Strings.Unbounded.Head
  str_val  : unbounded_string;
  str_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  pad_val  : unbounded_string;
  pad_type : identifier;
  pad_char : character := ' ';
begin
  kind := string_t;
  expect( head_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseNextNumericParameter( cnt_val, cnt_type, natural_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastStringParameter( pad_val, pad_type, character_t );
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

procedure ParseStringsTail( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.tail( s, c [, p] )
  -- Source: Ada.Strings.Unbounded.Tail
  str_val  : unbounded_string;
  str_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  pad_val  : unbounded_string;
  pad_type : identifier;
  pad_char : character := ' ';
begin
  kind := string_t;
  expect( tail_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseNextNumericParameter( cnt_val, cnt_type, natural_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastStringParameter( pad_val, pad_type, character_t );
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

procedure ParseStringsField( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.field( s, c [, d] )
  -- Source: N/A
  str_val  : unbounded_string;
  str_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  del_val  : unbounded_string;
  del_type : identifier;
  delim    : character := defaultDelimiter;
begin
  kind := string_t;
  expect( field_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseNextNumericParameter( cnt_val, cnt_type, natural_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastStringParameter( del_val, del_type, character_t );
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

procedure ParseStringsCSVField( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.csv_field( s, c [, d] )
  -- Source: N/A
  str_val  : unbounded_string;
  str_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  del_val  : unbounded_string;
  del_type : identifier;
  delim    : character := ',';
begin
  kind := string_t;
  expect( csv_field_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseNextNumericParameter( cnt_val, cnt_type, natural_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastStringParameter( del_val, del_type, character_t );
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
        result := stringCSVField( str_val, delim, natural( to_numeric( cnt_val ) ) );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsCSVField;

procedure ParseStringsMkTemp( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.mktemp
  -- Source: SparForte builtin
  str_val : unbounded_string;
  str_type : identifier;
  mkstemp_result : aFileDescriptor;
  closeResult : int;
begin
  kind := string_t;
  expect( mktemp_t );
  ParseSingleStringParameter( str_val, str_type );
  if isExecutingCommand then
     declare
       LinuxPath : string := to_string( str_val ) & "XXXXXX" & ASCII.NUL;
     begin
       result := null_unbounded_string;
       mkstemp( mkstemp_result, LinuxPath );
       if mkstemp_result < 0 then
          err( "mkstemp failed " & OSError( C_errno ) );
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

procedure ParseStringsVal( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.val( natural );
  -- Source: Ada 'val attribute
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := character_t;
  expect( val_t );
  ParseSingleNumericParameter( expr_val, expr_type, natural_t );
  begin
    if isExecutingCommand then
       result := to_unbounded_string( "" & character'val( natural( to_numeric( expr_val ) ) ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsVal;

procedure ParseStringsImage( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.image( x );
  -- Source: Ada 'image attribute
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := string_t;
  expect( image_t );
  ParseSingleNumericParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := expr_val;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsImage;

procedure ParseStringsToString( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_string( x );
  -- Source: Ada.Strings.Unbounded.To_String
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
     err( "internal_error: file mode error" );
   when status_error =>
     err( "internal_error: status error - cannot open file" );
   when name_error =>
     err( "internal_error: name error - cannot open fie" );
   when end_error =>
     err( "internal_error: end error - end of file reached" );
   when others =>
     err_exception_raised;
  end DoBase64ToString;

begin
  kind := string_t;
  expect( to_string_t );
  ParseSingleStringParameter( expr_val, expr_type );
  baseType := getBaseType( expr_type );
  if baseType /= unbounded_string_t and baseType /= json_string_t and
     baseType /= base64_string_t then
     err( "unbounded_string, json_string or strings.base64_string expected" );
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

procedure ParseStringsToUString( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_unbounded_string( x );
  -- Source: Ada.Strings.Unbounded.To_Unbounded_String
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := unbounded_string_t;
  expect( to_u_string_t );
  ParseSingleStringParameter( expr_val, expr_type, string_t );
  begin
    if isExecutingCommand then
       result := expr_val;
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsToUString;

procedure ParseStringsLookup( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.lookup( s, t [, d] );
  -- Source: N/A
  src_val  : unbounded_string;
  src_type : identifier;
  tar_val  : unbounded_string;
  tar_type : identifier;
  del_val  : unbounded_string;
  del_type : identifier;
  delim    : character := defaultDelimiter;
begin
  kind := string_t;
  expect( lookup_t );
  ParseFirstStringParameter( src_val, src_type );
  ParseNextStringParameter( tar_val, tar_type );
  if token = symbol_t and identifiers( token ).value.all = "," then
     ParseLastStringParameter( del_val, del_type, character_t );
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

procedure ParseStringsReplace is
  -- Syntax: strings.replace( s, f, t, [,d] );
  -- Source: N/A
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
  if uniTypesOk( src_ref.kind, string_t ) then
     ParseNextNumericParameter( cnt_val, cnt_type, natural_t );
     ParseNextStringParameter( tar_val, tar_type, string_t );
     if token = symbol_t and identifiers( token ).value.all = "," then
        ParseLastStringParameter( del_val, del_type, character_t );
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

procedure ParseStringsCSVReplace is
  -- Syntax: strings.csv_replace( s, f, t, [,d] );
  -- Source: N/A
  src_ref  : reference;
  tar_val  : unbounded_string;
  tar_type : identifier;
  cnt_val  : unbounded_string;
  cnt_type : identifier;
  del_val  : unbounded_string;
  del_type : identifier;
  delim    : character := ',';
  tempStr  : unbounded_string;
begin
  expect( csv_replace_t );
  expect( symbol_t, "(" );
  ParseInOutParameter( src_ref );
  if uniTypesOk( src_ref.kind, string_t ) then
     ParseNextNumericParameter( cnt_val, cnt_type, natural_t );
     ParseNextStringParameter( tar_val, tar_type );
     if token = symbol_t and identifiers( token ).value.all = "," then
        ParseLastStringParameter( del_val, del_type, character_t );
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
        replaceCSVField( tempStr,
           delim,
           natural( to_numeric( cnt_val ) ),
           to_string( tar_val ) );
        assignParameter( src_ref, tempStr );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsCSVReplace;

procedure ParseStringsToUpper( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_upper( s );
  -- Source: Ada.Characters.Handling.To_Upper
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_upper_t );
  ParseSingleStringParameter( src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToUpper( src_val );
  end if;
end ParseStringsToUpper;

procedure ParseStringsToLower( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_lower( s );
  -- Source: Ada.Characters.Handling.To_Lower
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_lower_t );
  ParseSingleStringParameter( src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToLower( src_val );
  end if;
end ParseStringsToLower;

procedure ParseStringsToProper( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_proper( s );
  -- Source: N/A
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_proper_t );
  ParseSingleStringParameter( src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToProper( src_val );
  end if;
end ParseStringsToProper;

procedure ParseStringsToBasic( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_basic( s );
  -- Source: Ada.Characters.Handling.To_Basic
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_basic_t );
  ParseSingleStringParameter( src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToBasic( src_val );
  end if;
end ParseStringsToBasic;

procedure ParseStringsToEscaped( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_escaped( s );
  -- Source: N/A
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_escaped_t );
  ParseSingleStringParameter( src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToEscaped( src_val );
  end if;
end ParseStringsToEscaped;

procedure ParseStringsSplit is
  -- Syntax: strings.split( s, l, r [,d] )
  -- Source: N/A (GNAT.Case_Util sort of)
  src_val  : unbounded_string;
  src_type : identifier;
  left_ref : reference;
  right_ref: reference;
  field_val: unbounded_string;
  field_type : identifier;
  delim    : character := defaultDelimiter;
  leftStr  : unbounded_string;
  rightStr : unbounded_string;
begin
  expect( split_t );
  ParseFirstStringParameter( src_val, src_type );
  ParseNextOutParameter( left_ref, string_t );
  ParseNextOutParameter( right_ref, string_t );
  ParseLastNumericParameter( field_val, field_type, natural_t );
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

procedure ParseStringsIsControl( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_control( x );
  -- Source: Ada.Characters.Handling.Is_Control (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_control_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_control( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsControl;

procedure ParseStringsIsGraphic( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_graphic( x );
  -- Source: Ada.Characters.Handling.Is_Graphic (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_graphic_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_graphic( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsGraphic;

procedure ParseStringsIsLetter( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_letter( x );
  -- Source: Ada.Characters.Handling.Is_Letter (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_letter_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_letter( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsLetter;

procedure ParseStringsIsLower( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_lower( x );
  -- Source: Ada.Characters.Handling.Is_Lower (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_lower_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_lower( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsLower;

procedure ParseStringsIsUpper( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_upper( x );
  -- Source: Ada.Characters.Handling.Is_Upper (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_upper_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_upper( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsUpper;

procedure ParseStringsIsBasic( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_basic( x );
  -- Source: Ada.Characters.Handling.Is_Basic (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_basic_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_basic( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsBasic;

procedure ParseStringsIsDigit( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_digit( x );
  -- Source: Ada.Characters.Handling.Is_Digit (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_digit_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_digit( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsDigit;

procedure ParseStringsIsHexDigit( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_hexadecimal_digit( x );
  -- Source: Ada.Characters.Handling.Is_Hexadecimal_Digit (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_hex_digit_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_hexadecimal_digit( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsHexDigit;

procedure ParseStringsIsAlphanumeric( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_alphanumeric( x );
  -- Source: Ada.Characters.Handling.Is_Alphanumeric (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_alphanumeric_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_alphanumeric( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsAlphanumeric;

procedure ParseStringsIsSpecial( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_special( x );
  -- Source: Ada.Characters.Handling.Is_Special (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_special_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_special( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsSpecial;

procedure ParseStringsIsSlashedDate( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_slashed_date( x );
  -- Source: N/A
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_slashed_date_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_date( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsSlashedDate;

procedure ParseStringsIsFixed( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_fixed( x );
  -- Source: N/A
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := boolean_t;
  expect( is_fixed_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_fixed( expr_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsFixed;

procedure ParseStringsToBase64( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_base64( s )
  -- Source: base64.encode_stream
  rawFile : ada.streams.stream_io.file_type;
  base64file : ada.text_io.file_type;
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  expect( to_base64_t );
  kind := base64_string_t;
  ParseSingleStringExpression( expr_val, expr_type );
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
          s : string := to_string( expr_val );
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
    err( "internal_error: file mode error" );
  when status_error =>
    err( "internal_error: status error - cannot open file" );
  when name_error =>
    err( "internal_error: name error - cannot open fie" );
  when end_error =>
    err( "internal_error: end error - end of file reached" );
  when others =>
    err_exception_raised;
  end;
end ParseStringsToBase64;

procedure ParseStringsIsTypoOf( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.is_typo_of( x, y );
  -- Source: N/A
  expr1_val   : unbounded_string;
  expr1_type  : identifier;
  expr2_val   : unbounded_string;
  expr2_type  : identifier;
begin
  kind := boolean_t;
  expect( is_typo_of_t );
  ParseFirstStringParameter( expr1_val, expr1_type );
  ParseLastStringParameter( expr2_val, expr2_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( typoOf( expr1_val, expr2_val ) );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsIsTypoOf;

procedure ParseStringsUnboundedSlice( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: unbounded_slice( s, l, h )
  -- Source: Ada.Strings.Unbounded.Unbounded_Slice
  str_val  : unbounded_string;
  str_type : identifier;
  low_val  : unbounded_string;
  low_type : identifier;
  hi_val   : unbounded_string;
  hi_type  : identifier;
begin
  kind := unbounded_string_t;
  expect( unbounded_slice_t );
  ParseFirstStringParameter( str_val, str_type );
  ParseNextNumericParameter( low_val, low_type, positive_t );
  ParseLastNumericParameter( hi_val,  hi_type,  natural_t );
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

procedure ParseStringsSetUnboundedString is
  -- Syntax: strings.set_unbounded_string( u, s );
  -- Source: ada.strings.unbounded.set_unbounded_string
  src_ref  : reference;
  str_val  : unbounded_string;
  str_type : identifier;
begin
  expect( set_unbounded_string_t );
  expect( symbol_t, "(" );
  ParseInOutParameter( src_ref );
  if uniTypesOk( src_ref.kind, unbounded_string_t ) then
     ParseLastStringParameter( str_val, str_type );
  end if;
  begin
     if isExecutingCommand then
        AssignParameter( src_ref, str_val );
     end if;
  exception when others =>
     err_exception_raised;
  end;
end ParseStringsSetUnboundedString;

procedure ParseStringsToJSON( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_json( x );
  -- Source: N/A
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  kind := json_string_t;
  expect( strings_to_json_t );
  ParseSingleStringParameter( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := DoStringToJSON( expr_val  );
    end if;
  exception when others =>
    err_exception_raised;
  end;
end ParseStringsToJSON;

procedure StartupStrings is
begin
  declareNamespace( "strings" );
  declareIdent( base64_string_t, "strings.base64_string", string_t, typeClass );

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

end StartupStrings;

procedure ShutdownStrings is
begin
  null;
end ShutdownStrings;

end parser_strings;
