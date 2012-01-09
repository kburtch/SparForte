------------------------------------------------------------------------------
-- Strings Package Parser                                                   --
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

with text_io;use text_io;
with interfaces.c,
    gnat.regexp,
    gnat.regpat,
    base64,
    scanner,
    string_util,
    parser_aux,
    parser_params,
    parser,
    bush_os;
use interfaces.c,
    gnat.regexp,
    gnat.regpat,
    base64,
    scanner,
    string_util,
    parser_params,
    parser_aux,
    parser,
    bush_os;

package body parser_strings is

defaultDelimiter : constant character := ASCII.CR;
-- default delimiter for string lookup functions

procedure ParseSingleUniStringExpression( expr_val : out unbounded_string;
  expr_type : out identifier ) is
begin
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
end ParseSingleUniStringExpression;

procedure ParseSingleStringExpression( expr_val : out unbounded_string;
  expr_type : out identifier ) is
begin
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
end ParseSingleStringExpression;

procedure ParseStringsGlob( result : out unbounded_string ) is
  -- Syntax: glob( expr, string )
  -- Source: GNAT.RegExp.Match
  expr_val  : unbounded_string;
  expr_type : identifier;
  pat_val   : unbounded_string;
  pat_type  : identifier;
  re        : regexp;
  b         : boolean;
begin
  result := null_unbounded_string;
  expect( glob_t );
  expect( symbol_t, "(" );
  ParseExpression( pat_val, pat_type );
  if uniTypesOk( pat_type, uni_string_t ) then
     expect( symbol_t, "," );
     ParseExpression( expr_val, expr_type );
     if uniTypesOk( expr_type, uni_string_t ) then
        expect( symbol_t, ")" );
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
     end if;
  end if;
end ParseStringsGlob;

procedure ParseStringsMatch( result : out unbounded_string ) is
  -- Syntax: match( file )
  -- Source: GNAT.RegPat.Match
  expr_val  : unbounded_string;
  expr_type : identifier;
  pat_val   : unbounded_string;
  pat_type  : identifier;
  b         : boolean;
begin
  result := null_unbounded_string;
  getNextToken;
  expect( symbol_t, "(" );
  ParseExpression( pat_val, pat_type );
  if uniTypesOk( pat_type, uni_string_t ) then
     expect( symbol_t, "," );
     ParseExpression( expr_val, expr_type );
     if uniTypesOk( expr_type, uni_string_t ) then
        expect( symbol_t, ")" );
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
     end if;
  end if;
end ParseStringsMatch;

procedure ParseStringsElement( result : in out unbounded_string ) is
  -- Syntax: element( s, i )
  -- Source: Ada.Strings.Unbounded.Element
  str_val : unbounded_string;
  str_type : identifier;
  index_val : unbounded_string;
  index_type : identifier;
begin
  expect( element_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( index_val, index_type );
     if intTypesOk( index_type, positive_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  begin
     if isExecutingCommand then
        result := to_unbounded_string( "" &
           Element( str_val, positive( to_numeric( index_val ) ) ) );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsElement;

procedure ParseStringsSlice( result : in out unbounded_string ) is
  -- Syntax: slice( s, l, h )
  -- Source: Ada.Strings.Unbounded.Slice
  str_val  : unbounded_string;
  str_type : identifier;
  low_val  : unbounded_string;
  low_type : identifier;
  hi_val   : unbounded_string;
  hi_type  : identifier;
begin
  expect( slice_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( low_val, low_type );
     if intTypesOk( low_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( hi_val, hi_type );
        if intTypesOk( hi_type, natural_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  begin
     if isExecutingCommand then
        result := to_unbounded_string(
           Slice( str_val,
	     positive( to_numeric( low_val ) ),
	     natural( to_numeric( hi_val ) )
	   ) );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsSlice;

procedure ParseStringsIndex( result : in out unbounded_string ) is
  -- Syntax: strings.index( s, p )
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
  expect( index_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( pat_val, pat_type );
     if baseTypesOk( pat_type, string_t ) then
        if token = symbol_t and identifiers( token ).value = "," then
           expect( symbol_t, "," );
           ParseExpression( dir_val, dir_type );
           if baseTypesOk( dir_type, strings_direction_t ) then
              if isExecutingCommand then
                 case natural( to_numeric( dir_val ) ) is
                 when 0 => dir := forward;
                 when 1 => dir := backward;
                 when others =>
                    err( "exception raised" );
                 end case;
              end if;
           end if;
         end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  begin
     if isExecutingCommand then
        result := to_unbounded_string( Index( str_val, to_string( pat_val ), Going => dir )'img );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsIndex;

procedure ParseStringsIndexNonBlank( result : in out unbounded_string ) is
  -- Syntax: strings.index_non_blank( s [, d] )
  -- Source: Ada.Strings.Unbounded.Index_Non_Blank
  use ada.strings;
  str_val : unbounded_string;
  str_type : identifier;
  dir_val : unbounded_string;
  dir_type : identifier;
  dir    : direction := forward;
begin
  expect( index_non_blank_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     if token = symbol_t and identifiers( token ).value = "," then
        expect( symbol_t, "," );
        ParseExpression( dir_val, dir_type );
        if baseTypesOK( dir_type, strings_direction_t ) then
           if isExecutingCommand then
              case natural( to_numeric( dir_val ) ) is
              when 0 => dir := forward;
              when 1 => dir := backward;
              when others =>
                 err( "exception raised" );
              end case;
           end if;
        end if;
     end if;
     expect( symbol_t, ")" );
  end if;
  begin
     if isExecutingCommand then
        result := to_unbounded_string( Index_Non_Blank( str_val, dir )'img );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsIndexNonBlank;

procedure ParseStringsCount( result : in out unbounded_string ) is
  -- Syntax: strings.count( s, p )
  -- Source: Ada.Strings.Unbounded.Count
  str_val : unbounded_string;
  str_type : identifier;
  pat_val : unbounded_string;
  pat_type : identifier;
begin
  expect( count_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( pat_val, pat_type );
     if baseTypesOk( pat_type, string_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  begin
     if isExecutingCommand then
        result := to_unbounded_string( Ada.Strings.Unbounded.Count( str_val,
           to_string( pat_val ) )'img );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsCount;

procedure ParseStringsReplaceSlice( result : in out unbounded_string ) is
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
  expect( replace_slice_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( low_val, low_type );
     if intTypesOk( low_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( hi_val, hi_type );
        if intTypesOk( hi_type, natural_t ) then
           expect( symbol_t, "," );
           ParseExpression( by_val, by_type );
           if baseTypesOk( by_type, string_t ) then
              expect( symbol_t, ")" );
           end if;
        end if;
     end if;
  end if;
  begin
     if isExecutingCommand then
        result := Replace_Slice( str_val,
	   positive( to_numeric( low_val ) ),
	   natural( to_numeric( hi_val ) ),
	   to_string( by_val )
	);
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsReplaceSlice;

procedure ParseStringsInsert( result : in out unbounded_string ) is
  -- Syntax: strings.insert( s, b, n )
  -- Source: Ada.Strings.Unbounded.Replace_Slice
  str_val : unbounded_string;
  str_type : identifier;
  before_val : unbounded_string;
  before_type : identifier;
  new_val : unbounded_string;
  new_type : identifier;
begin
  expect( strings_insert_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( before_val, before_type );
     if intTypesOk( before_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( new_val, new_type );
        if baseTypesOk( new_type, string_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  begin
     if isExecutingCommand then
        result := Insert( str_val,
	   positive( to_numeric( before_val ) ),
	   to_string( new_val )
	);
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsInsert;

procedure ParseStringsOverwrite( result : in out unbounded_string ) is
  -- Syntax: strings.overwrite( s, p, n )
  -- Source: Ada.Strings.Unbounded.Overwrite
  str_val  : unbounded_string;
  str_type : identifier;
  pos_val  : unbounded_string;
  pos_type : identifier;
  new_val  : unbounded_string;
  new_type : identifier;
begin
  expect( overwrite_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( pos_val, pos_type );
     if intTypesOk( pos_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( new_val, new_type );
        if baseTypesOk( new_type, string_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  begin
     if isExecutingCommand then
        result := Overwrite( str_val,
	   positive( to_numeric( pos_val ) ),
	   to_string( new_val )
	);
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsOverwrite;

procedure ParseStringsDelete( result : in out unbounded_string ) is
  -- Syntax: strings.delete( s, l, h )
  -- Source: Ada.Strings.Unbounded.Delete
  str_val  : unbounded_string;
  str_type : identifier;
  low_val  : unbounded_string;
  low_type : identifier;
  hi_val   : unbounded_string;
  hi_type  : identifier;
begin
  expect( sdelete_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( low_val, low_type );
     if intTypesOk( low_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( hi_val, hi_type );
        if intTypesOk( hi_type, natural_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  begin
     if isExecutingCommand then
        result := Delete( str_val,
	   positive( to_numeric( low_val ) ),
	   natural( to_numeric( hi_val ) )
	);
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsDelete;

procedure ParseStringsTrim( result : in out unbounded_string ) is
  -- Syntax: strings.trim( s , e )
  -- Source: Ada.Strings.Unbounded.Trim
  use ada.strings;
  str_val : unbounded_string;
  str_type : identifier;
  trim_end_val : unbounded_string;
  trim_end_type : identifier;
  the_trim_end : trim_end := both;
begin
  expect( trim_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( trim_end_val, trim_end_type );
     if baseTypesOK( trim_end_type, strings_trim_end_t ) then
         if isExecutingCommand then
            case natural( to_numeric( trim_end_val ) ) is
            when 0 => the_trim_end := left;
            when 1 => the_trim_end := right;
            when 2 => the_trim_end := both;
            when others =>
               err( "exception raised" );
            end case;
         end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  begin
     if isExecutingCommand then
        result := trim( str_val, the_trim_end );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsTrim;

procedure ParseStringsLength( result : in out unbounded_string ) is
  -- Syntax: strings.trim( s )
  -- Source: Ada.Strings.Unbounded.Trim
  str_val : unbounded_string;
  str_type : identifier;
begin
  expect( length_t );
  ParseSingleUniStringExpression( str_val, str_type );
  begin
     if isExecutingCommand then
        result := to_unbounded_string( length( str_val )'img );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsLength;

procedure ParseStringsHead( result : in out unbounded_string ) is
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
  expect( head_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( cnt_val, cnt_type );
     if uniTypesOk( cnt_type, natural_t ) then
        if token = symbol_t and identifiers( token ).value = "," then
           expect( symbol_t, "," );
           ParseExpression( pad_val, pad_type );
           if baseTypesOk( pad_type, character_t ) then
           begin
              pad_char := element( pad_val, 1 );
           exception when others =>
              err( "exception raised" );
           end;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  begin
     if isExecutingCommand then
        result := head( str_val, natural( to_numeric( cnt_val ) ),
            pad_char );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsHead;

procedure ParseStringsTail( result : in out unbounded_string ) is
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
  expect( tail_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( cnt_val, cnt_type );
     if uniTypesOk( cnt_type, natural_t ) then
        if token = symbol_t and identifiers( token ).value = "," then
           expect( symbol_t, "," );
           ParseExpression( pad_val, pad_type );
           if baseTypesOk( pad_type, character_t ) then
           begin
              pad_char := element( pad_val, 1 );
           exception when others =>
              err( "exception raised" );
           end;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  begin
     if isExecutingCommand then
        result := tail( str_val, natural( to_numeric( cnt_val ) ),
          pad_char );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsTail;

procedure ParseStringsField( result : in out unbounded_string ) is
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
  expect( field_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( cnt_val, cnt_type );
     if intTypesOk( cnt_type, natural_t ) then
        if token = symbol_t and identifiers( token ).value = "," then
           expect( symbol_t, "," );
           ParseExpression( del_val, del_type );
           if baseTypesOk( del_type, character_t ) then
              if isExecutingCommand then
                 begin
                    delim := element( del_val, 1 );
                 exception when others =>
                    err( "exception raised" );
                 end;
              end if;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  begin
     if isExecutingCommand then
        result := stringField( str_val, delim, natural( to_numeric( cnt_val ) ) );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsField;

procedure ParseStringsCSVField( result : in out unbounded_string ) is
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
  expect( csv_field_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( cnt_val, cnt_type );
     if intTypesOk( cnt_type, natural_t ) then
        if token = symbol_t and identifiers( token ).value = "," then
           expect( symbol_t, "," );
           ParseExpression( del_val, del_type );
           if baseTypesOk( del_type, character_t ) then
              if isExecutingCommand then
                 begin
                    delim := element( del_val, 1 );
                 exception when others =>
                    err( "exception raised" );
                 end;
              end if;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  begin
     if isExecutingCommand then
        result := stringCSVField( str_val, delim, natural( to_numeric( cnt_val ) ) );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsCSVField;

procedure ParseStringsMkTemp( result : in out unbounded_string ) is
  -- Syntax: strings.mktemp
  -- Source: BUSH builtin
  str_val : unbounded_string;
  str_type : identifier;
  mkstemp_result : aFileDescriptor;
  closeResult : int;
begin
  expect( mktemp_t );
  ParseSingleStringExpression( str_val, str_type );
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

procedure ParseStringsVal( result : in out unbounded_string ) is
  -- Syntax: strings.val( natural );
  -- Source: Ada 'val attribute
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( val_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if intTypesOk( expr_type, natural_t ) then
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       result := to_unbounded_string( "" & character'val( natural( to_numeric( expr_val ) ) ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsVal;

procedure ParseStringsImage( result : in out unbounded_string ) is
  -- Syntax: strings.image( x );
  -- Source: Ada 'image attribute
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( image_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if uniTypesOk( expr_type, uni_numeric_t ) then
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       result := expr_val;
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsImage;

procedure ParseStringsToString( result : in out unbounded_string ) is
  -- Syntax: strings.to_string( x );
  -- Source: Ada.Strings.Unbounded.To_String
  expr_val   : unbounded_string;
  expr_type  : identifier;
  baseType   : identifier;
begin
  expect( to_string_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  baseType := getBaseType( expr_type );
  if baseType /= unbounded_string_t and baseType /= json_string_t then
     err( "unbounded_string or json_string expected" );
  end if;
  expect( symbol_t, ")" );
  begin
    if isExecutingCommand then
       if baseType = unbounded_string_t then
          result := expr_val;
       else
          DoJsonToString( result, expr_val );
       end if;
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsToString;

procedure ParseStringsToUString( result : in out unbounded_string ) is
  -- Syntax: strings.to_unbounded_string( x );
  -- Source: Ada.Strings.Unbounded.To_Unbounded_String
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( to_u_string_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, string_t ) then
     expect( symbol_t, ")" );
  end if;
  begin
    if isExecutingCommand then
       result := expr_val;
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsToUString;

procedure ParseStringsLookup( result : in out unbounded_string ) is
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
  expect( lookup_t );
  expect( symbol_t, "(" );
  ParseExpression( src_val, src_type );
  if uniTypesOk( src_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( tar_val, tar_type );
        if uniTypesOk( tar_type, string_t ) then
           if token = symbol_t and identifiers( token ).value = "," then
              expect( symbol_t, "," );
              ParseExpression( del_val, del_type );
              if baseTypesOk( del_type, character_t ) then
                 if isExecutingCommand then
                    begin
                       delim := element( del_val, 1 );
                    exception when others =>
                       err( "exception raised" );
                    end;
                 end if;
              end if;
           end if;
        end if;
  end if;
  expect( symbol_t, ")" );
  begin
     if isExecutingCommand then
        result := stringLookup( src_val, tar_val, delim );
     end if;
  exception when others =>
     err( "exception raised" );
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
     expect( symbol_t, "," );
     ParseExpression( cnt_val, cnt_type );
     if intTypesOk( cnt_type, natural_t ) then
        expect( symbol_t, "," );
        ParseExpression( tar_val, tar_type );
        if uniTypesOk( tar_type, string_t ) then
           if token = symbol_t and identifiers( token ).value = "," then
              expect( symbol_t, "," );
              ParseExpression( del_val, del_type );
              if baseTypesOk( del_type, character_t ) then
                 if isExecutingCommand then
                    begin
                       delim := element( del_val, 1 );
                    exception when others =>
                       err( "exception raised" );
                    end;
                 end if;
              end if;
           end if;
        end if;
     end if;
   end if;
  expect( symbol_t, ")" );
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
     err( "exception raised" );
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
     expect( symbol_t, "," );
     ParseExpression( cnt_val, cnt_type );
     if intTypesOk( cnt_type, natural_t ) then
        expect( symbol_t, "," );
        ParseExpression( tar_val, tar_type );
        if uniTypesOk( tar_type, string_t ) then
           if token = symbol_t and identifiers( token ).value = "," then
              expect( symbol_t, "," );
              ParseExpression( del_val, del_type );
              if baseTypesOk( del_type, character_t ) then
                 if isExecutingCommand then
                    begin
                       delim := element( del_val, 1 );
                    exception when others =>
                       err( "exception raised" );
                    end;
                 end if;
              end if;
           end if;
        end if;
     end if;
   end if;
  expect( symbol_t, ")" );
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
     err( "exception raised" );
  end;
end ParseStringsCSVReplace;

procedure ParseStringsToUpper( result : in out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_upper( s );
  -- Source: Ada.Characters.Handling.To_Upper
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_upper_t );
  ParseSingleUniStringExpression( src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToUpper( src_val );
  end if;
end ParseStringsToUpper;

procedure ParseStringsToLower( result : in out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_lower( s );
  -- Source: Ada.Characters.Handling.To_Lower
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_lower_t );
  ParseSingleUniStringExpression( src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToLower( src_val );
  end if;
end ParseStringsToLower;

procedure ParseStringsToProper( result : in out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_proper( s );
  -- Source: N/A
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_proper_t );
  ParseSingleUniStringExpression( src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToProper( src_val );
  end if;
end ParseStringsToProper;

procedure ParseStringsToBasic( result : in out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_basic( s );
  -- Source: Ada.Characters.Handling.To_Basic
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_basic_t );
  ParseSingleUniStringExpression( src_val, src_type );
  kind := src_type;
  if isExecutingCommand then
     result := ToBasic( src_val );
  end if;
end ParseStringsToBasic;

procedure ParseStringsToEscaped( result : in out unbounded_string; kind : out identifier ) is
  -- Syntax: strings.to_escaped( s );
  -- Source: N/A
  src_val  : unbounded_string;
  src_type : identifier;
begin
  expect( to_escaped_t );
  ParseSingleUniStringExpression( src_val, src_type );
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
  expect( symbol_t, "(" );
  ParseExpression( src_val, src_type );
  if uniTypesOk( src_type, string_t ) then
     expect( symbol_t, "," );
     ParseOutParameter( left_ref, string_t );
     if uniTypesOk( left_ref.kind, string_t ) then
        expect( symbol_t, "," );
        ParseOutParameter( right_ref, string_t );
        if uniTypesOk( right_ref.kind, string_t ) then
           expect( symbol_t, "," );
           ParseExpression( field_val, field_type );
           if intTypesOk( field_type, natural_t ) then
              expect( symbol_t, ")" );
           end if;
        end if;
     end if;
   end if;
  begin
     if isExecutingCommand then
        split( src_val, leftStr, rightStr,
               natural( to_numeric( field_val ) ) );
        assignParameter( left_ref, leftStr );
        assignParameter( right_ref, rightStr );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsSplit;

procedure ParseStringsIsControl( result : in out unbounded_string) is
  -- Syntax: strings.is_control( x );
  -- Source: Ada.Characters.Handling.Is_Control (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_control_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_control( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsControl;

procedure ParseStringsIsGraphic( result : in out unbounded_string) is
  -- Syntax: strings.is_graphic( x );
  -- Source: Ada.Characters.Handling.Is_Graphic (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_graphic_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_graphic( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsGraphic;

procedure ParseStringsIsLetter( result : in out unbounded_string) is
  -- Syntax: strings.is_letter( x );
  -- Source: Ada.Characters.Handling.Is_Letter (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_letter_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_letter( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsLetter;

procedure ParseStringsIsLower( result : in out unbounded_string) is
  -- Syntax: strings.is_lower( x );
  -- Source: Ada.Characters.Handling.Is_Lower (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_lower_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_lower( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsLower;

procedure ParseStringsIsUpper( result : in out unbounded_string) is
  -- Syntax: strings.is_upper( x );
  -- Source: Ada.Characters.Handling.Is_Upper (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_upper_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_upper( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsUpper;

procedure ParseStringsIsBasic( result : in out unbounded_string) is
  -- Syntax: strings.is_basic( x );
  -- Source: Ada.Characters.Handling.Is_Basic (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_basic_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_basic( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsBasic;

procedure ParseStringsIsDigit( result : in out unbounded_string) is
  -- Syntax: strings.is_digit( x );
  -- Source: Ada.Characters.Handling.Is_Digit (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_digit_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_digit( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsDigit;

procedure ParseStringsIsHexDigit( result : in out unbounded_string) is
  -- Syntax: strings.is_hexadecimal_digit( x );
  -- Source: Ada.Characters.Handling.Is_Hexadecimal_Digit (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_hex_digit_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_hexadecimal_digit( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsHexDigit;

procedure ParseStringsIsAlphanumeric( result : in out unbounded_string) is
  -- Syntax: strings.is_alphanumeric( x );
  -- Source: Ada.Characters.Handling.Is_Alphanumeric (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_alphanumeric_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_alphanumeric( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsAlphanumeric;

procedure ParseStringsIsSpecial( result : in out unbounded_string) is
  -- Syntax: strings.is_special( x );
  -- Source: Ada.Characters.Handling.Is_Special (except for string)
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_special_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_special( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsSpecial;

procedure ParseStringsIsSlashedDate( result : in out unbounded_string) is
  -- Syntax: strings.is_slashed_date( x );
  -- Source: N/A
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_slashed_date_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_date( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsSlashedDate;

procedure ParseStringsIsFixed( result : in out unbounded_string) is
  -- Syntax: strings.is_fixed( x );
  -- Source: N/A
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( is_fixed_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := to_bush_boolean( is_fixed( expr_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsFixed;

-- procedure ParseStringsToBase64( result : in out unbounded_string ) is
-- begin
--   expect( to_base64_t );
--   ParseSingleUniStringExpression( expr_val, expr_type );
--   begin
--     if isExecutingCommand then
--        Encode( to_string( expr_val ), Target, Last );
--        result := to_unbounded_string( Target );
--     end if;
--   exception when others =>
--     err( "exception raised" );
--   end;
-- end ParseStringsToBase64;

-- procedure ParseStringsFromBase64( result : in out unbounded_string ) is
-- begin
--   expect( from_base64_t );
--   ParseSingleUniStringExpression( expr_val, expr_type );
--   begin
--     if isExecutingCommand then
--        Decode( to_string( expr_val ), Target, Last );
--        result := to_unbounded_string( Target );
--     end if;
--   exception when others =>
--     err( "exception raised" );
--   end;
-- end ParseStringsFromBase64;

procedure ParseStringsIsTypoOf( result : in out unbounded_string) is
  -- Syntax: strings.is_typo_of( x, y );
  -- Source: N/A
  expr1_val   : unbounded_string;
  expr1_type  : identifier;
  expr2_val   : unbounded_string;
  expr2_type  : identifier;
begin
  expect( is_typo_of_t );
  expect( symbol_t, "(" );
  ParseExpression( expr1_val, expr1_type );
  if baseTypesOk( expr1_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( expr2_val, expr2_type );
     if baseTypesOk( expr2_type, string_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  begin
    if isExecutingCommand then
       result := to_bush_boolean( typoOf( expr1_val, expr2_val ) );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsIsTypoOf;

procedure ParseStringsUnboundedSlice( result : in out unbounded_string ) is
  -- Syntax: unbounded_slice( s, l, h )
  -- Source: Ada.Strings.Unbounded.Unbounded_Slice
  str_val  : unbounded_string;
  str_type : identifier;
  low_val  : unbounded_string;
  low_type : identifier;
  hi_val   : unbounded_string;
  hi_type  : identifier;
begin
  expect( unbounded_slice_t );
  expect( symbol_t, "(" );
  ParseExpression( str_val, str_type );
  if uniTypesOk( str_type, string_t ) then
     expect( symbol_t, "," );
     ParseExpression( low_val, low_type );
     if intTypesOk( low_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( hi_val, hi_type );
        if intTypesOk( hi_type, natural_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  begin
     if isExecutingCommand then
        result := to_unbounded_string(
           Slice( str_val,
	     positive( to_numeric( low_val ) ),
	     natural( to_numeric( hi_val ) )
	   ) );
     end if;
  exception when others =>
     err( "exception raised" );
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
     expect( symbol_t, "," );
     ParseExpression( str_val, str_type );
     if uniTypesOk( str_type, string_t ) then
        expect( symbol_t, ")" );
     end if;
   end if;
  begin
     if isExecutingCommand then
        AssignParameter( src_ref, str_val );
     end if;
  exception when others =>
     err( "exception raised" );
  end;
end ParseStringsSetUnboundedString;

procedure ParseStringsToJSON( result : in out unbounded_string) is
  -- Syntax: strings.to_json( x );
  -- Source: N/A
  expr_val   : unbounded_string;
  expr_type  : identifier;
begin
  expect( strings_to_json_t );
  ParseSingleUniStringExpression( expr_val, expr_type );
  begin
    if isExecutingCommand then
       result := DoStringToJSON( expr_val  );
    end if;
  exception when others =>
    err( "exception raised" );
  end;
end ParseStringsToJSON;

procedure StartupStrings is
begin
  declareIdent( strings_alignment_t, "strings.alignment",
    root_enumerated_t, typeClass );
  declareStandardConstant( strings_alignment_left_t, "alignment.left",
    strings_alignment_t, "0" );
  declareStandardConstant( strings_alignment_right_t, "alignment.right",
    strings_alignment_t, "1" );
  declareStandardConstant( strings_alignment_center_t, "alignment.center",
    strings_alignment_t, "2" );

  declareIdent( strings_truncation_t, "strings.truncation",
    root_enumerated_t, typeClass );
  declareStandardConstant( strings_truncation_left_t, "truncation.left",
    strings_truncation_t, "0" );
  declareStandardConstant( strings_truncation_right_t, "truncation.right",
    strings_truncation_t, "1" );
  declareStandardConstant( strings_truncation_error_t, "truncation.error",
    strings_truncation_t, "2" );

  declareIdent( strings_membership_t, "strings.membership",
    root_enumerated_t, typeClass );
  declareStandardConstant( strings_membership_inside_t, "membership.inside",
    strings_membership_t, "0" );
  declareStandardConstant( strings_membership_outside_t, "membership.outside",
    strings_membership_t, "1" );

  declareIdent( strings_direction_t, "strings.direction",
    root_enumerated_t, typeClass );
  declareStandardConstant( strings_direction_forward_t, "direction.forward",
    strings_direction_t, "0" );
  declareStandardConstant( strings_direction_backward_t, "direction.backward",
    strings_direction_t, "1" );

  declareIdent( strings_trim_end_t, "strings.trim_end",
    root_enumerated_t, typeClass );
  declareStandardConstant( strings_trim_end_left_t, "trim_end.left",
    strings_trim_end_t, "0" );
  declareStandardConstant( strings_trim_end_right_t, "trim_end.right",
    strings_trim_end_t, "1" );
  declareStandardConstant( strings_trim_end_both_t, "trim_end.both",
    strings_trim_end_t, "2" );

  declareFunction( glob_t, "strings.glob" );
  declareFunction( match_t, "strings.match" );
  declareFunction( element_t, "strings.element" );
  declareFunction( slice_t, "strings.slice" );
  declareFunction( index_t, "strings.index" );
  declareFunction( index_non_blank_t, "strings.index_non_blank" );
  declareFunction( count_t, "strings.count" );
  declareFunction( replace_slice_t, "strings.replace_slice" );
  declareFunction( strings_insert_t, "strings.insert" );
  declareFunction( overwrite_t, "strings.overwrite" );
  declareFunction( sdelete_t, "strings.delete" );
  declareFunction( trim_t, "strings.trim" );
  declareFunction( length_t, "strings.length" );
  declareFunction( head_t, "strings.head" );
  declareFunction( tail_t, "strings.tail" );
  declareFunction( val_t, "strings.val" );
  declareFunction( image_t, "strings.image" );
  declareFunction( field_t, "strings.field" );
  declareFunction( csv_field_t, "strings.csv_field" );
  declareFunction( lookup_t, "strings.lookup" );
  declareProcedure( replace_t, "strings.replace" );
  declareProcedure( csv_replace_t, "strings.csv_replace" );
  declareFunction( to_upper_t, "strings.to_upper" );
  declareFunction( to_lower_t, "strings.to_lower" );
  declareFunction( to_proper_t, "strings.to_proper" );
  declareFunction( to_basic_t, "strings.to_basic" );
  declareFunction( to_escaped_t, "strings.to_escaped" );
  declareFunction( is_control_t, "strings.is_control" );
  declareFunction( is_graphic_t, "strings.is_graphic" );
  declareFunction( is_letter_t, "strings.is_letter" );
  declareFunction( is_lower_t, "strings.is_lower" );
  declareFunction( is_upper_t, "strings.is_upper" );
  declareFunction( is_basic_t, "strings.is_basic" );
  declareFunction( is_digit_t, "strings.is_digit" );
  declareFunction( is_hex_digit_t, "strings.is_hexadecimal_digit" );
  declareFunction( is_alphanumeric_t, "strings.is_alphanumeric" );
  declareFunction( is_special_t, "strings.is_special" );
  declareFunction( is_slashed_date_t, "strings.is_slashed_date" );
  declareFunction( is_fixed_t, "strings.is_fixed" );
  declareFunction( split_t, "strings.split" );
  declareFunction( mktemp_t, "strings.mktemp" );
  declareFunction( to_string_t, "strings.to_string" );
  declareFunction( to_u_string_t, "strings.to_unbounded_string" );
  declareFunction( is_typo_of_t, "strings.is_typo_of" );
  declareProcedure( set_unbounded_string_t, "strings.set_unbounded_string" );
  declareFunction( unbounded_slice_t, "strings.unbounded_slice" );
  declareFunction( strings_to_json_t, "strings.to_json" );
end StartupStrings;

procedure ShutdownStrings is
begin
  null;
end ShutdownStrings;

end parser_strings;
