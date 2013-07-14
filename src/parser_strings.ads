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

with ada.strings.unbounded;
use  ada.strings.unbounded;

with world, scanner;
use  world, scanner;

package parser_strings is

------------------------------------------------------------------------------
-- Strings package identifiers
--
-- These will eventually be moved to the Strings parser
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupStrings;
procedure ShutdownStrings;

------------------------------------------------------------------------------
-- PARSE THE STRINGS PACKAGE
------------------------------------------------------------------------------

procedure ParseStringsGlob( result : out unbounded_string; kind : out identifier );
procedure ParseStringsMatch( result : out unbounded_string; kind : out identifier );
procedure ParseStringsElement( result : out unbounded_string; kind : out identifier );
procedure ParseStringsSlice( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIndex( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIndexNonBlank( result : out unbounded_string; kind : out identifier );
procedure ParseStringsCount( result : out unbounded_string; kind : out identifier );
procedure ParseStringsReplaceSlice( result : out unbounded_string; kind : out identifier );
procedure ParseStringsInsert( result : out unbounded_string; kind : out identifier );
procedure ParseStringsOverwrite( result : out unbounded_string; kind : out identifier );
procedure ParseStringsDelete( result : out unbounded_string; kind : out identifier );
procedure ParseStringsTrim( result : out unbounded_string; kind : out identifier );
procedure ParseStringsLength( result : out unbounded_string; kind : out identifier );
procedure ParseStringsHead( result : out unbounded_string; kind : out identifier );
procedure ParseStringsTail( result : out unbounded_string; kind : out identifier );
procedure ParseStringsField( result : out unbounded_string; kind : out identifier );
procedure ParseStringsCSVField( result : out unbounded_string; kind : out identifier );
procedure ParseStringsMkTemp( result : out unbounded_string; kind : out identifier );
procedure ParseStringsVal( result : out unbounded_string; kind : out identifier );
procedure ParseStringsImage( result : out unbounded_string; kind : out identifier );
procedure ParseStringsToString( result : out unbounded_string; kind : out identifier );
procedure ParseStringsToUString( result : out unbounded_string; kind : out identifier );
procedure ParseStringsLookup( result : out unbounded_string; kind : out identifier );
procedure ParseStringsReplace;
procedure ParseStringsCSVReplace;
procedure ParseStringsToUpper( result : out unbounded_string; kind : out identifier );
procedure ParseStringsToLower( result : out unbounded_string; kind : out identifier );
procedure ParseStringsToProper( result : out unbounded_string; kind : out identifier );
procedure ParseStringsToBasic( result : out unbounded_string; kind : out identifier );
procedure ParseStringsToEscaped( result : out unbounded_string; kind : out identifier );
procedure ParseStringsSplit;
procedure ParseStringsIsControl( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsGraphic( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsLetter( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsLower( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsUpper( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsBasic( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsDigit( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsHexDigit( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsAlphanumeric( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsSpecial( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsSlashedDate( result : out unbounded_string; kind : out identifier );
procedure ParseStringsIsFixed( result : out unbounded_string; kind : out identifier );
-- procedure ParseStringsToBase64( result : in out unbounded_string );
-- procedure ParseStringsFromBase64( result : in out unbounded_string );
procedure ParseStringsIsTypoOf( result : out unbounded_string; kind : out identifier );
procedure ParseStringsSetUnboundedString;
procedure ParseStringsUnboundedSlice( result : out unbounded_string; kind : out identifier);
procedure ParseStringsToJSON( result : out unbounded_string; kind : out identifier );

end parser_strings;
