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

procedure ParseStringsGlob( result : out unbounded_string );
procedure ParseStringsMatch( result : out unbounded_string );
procedure ParseStringsElement( result : in out unbounded_string );
procedure ParseStringsSlice( result : in out unbounded_string );
procedure ParseStringsIndex( result : in out unbounded_string );
procedure ParseStringsIndexNonBlank( result : in out unbounded_string );
procedure ParseStringsCount( result : in out unbounded_string );
procedure ParseStringsReplaceSlice( result : in out unbounded_string );
procedure ParseStringsInsert( result : in out unbounded_string );
procedure ParseStringsOverwrite( result : in out unbounded_string );
procedure ParseStringsDelete( result : in out unbounded_string );
procedure ParseStringsTrim( result : in out unbounded_string );
procedure ParseStringsLength( result : in out unbounded_string );
procedure ParseStringsHead( result : in out unbounded_string );
procedure ParseStringsTail( result : in out unbounded_string );
procedure ParseStringsField( result : in out unbounded_string );
procedure ParseStringsCSVField( result : in out unbounded_string );
procedure ParseStringsMkTemp( result : in out unbounded_string );
procedure ParseStringsVal( result : in out unbounded_string );
procedure ParseStringsImage( result : in out unbounded_string );
procedure ParseStringsToString( result : in out unbounded_string );
procedure ParseStringsToUString( result : in out unbounded_string );
procedure ParseStringsLookup( result : in out unbounded_string );
procedure ParseStringsReplace;
procedure ParseStringsCSVReplace;
procedure ParseStringsToUpper( result : in out unbounded_string; kind : out identifier );
procedure ParseStringsToLower( result : in out unbounded_string; kind : out identifier );
procedure ParseStringsToProper( result : in out unbounded_string; kind : out identifier );
procedure ParseStringsToBasic( result : in out unbounded_string; kind : out identifier );
procedure ParseStringsToEscaped( result : in out unbounded_string; kind : out identifier );
procedure ParseStringsSplit;
procedure ParseStringsIsControl( result : in out unbounded_string);
procedure ParseStringsIsGraphic( result : in out unbounded_string);
procedure ParseStringsIsLetter( result : in out unbounded_string);
procedure ParseStringsIsLower( result : in out unbounded_string);
procedure ParseStringsIsUpper( result : in out unbounded_string);
procedure ParseStringsIsBasic( result : in out unbounded_string);
procedure ParseStringsIsDigit( result : in out unbounded_string);
procedure ParseStringsIsHexDigit( result : in out unbounded_string);
procedure ParseStringsIsAlphanumeric( result : in out unbounded_string);
procedure ParseStringsIsSpecial( result : in out unbounded_string);
procedure ParseStringsIsSlashedDate( result : in out unbounded_string);
procedure ParseStringsIsFixed( result : in out unbounded_string);
-- procedure ParseStringsToBase64( result : in out unbounded_string );
-- procedure ParseStringsFromBase64( result : in out unbounded_string );
procedure ParseStringsIsTypoOf( result : in out unbounded_string);
procedure ParseStringsSetUnboundedString;
procedure ParseStringsUnboundedSlice( result : in out unbounded_string);
procedure ParseStringsToJSON( result : in out unbounded_string);

end parser_strings;
