#!/usr/local/bin/spar

pragma annotate( summary, "mapping" );
pragma annotate( description, "The task is to write a function/subroutine/... that takes" );
pragma annotate( description, "two ranges and a real number, and returns the mapping of" );
pragma annotate( description, "the real number from the first to the second range. Use" );
pragma annotate( description, "this function to map values from the range [0, 10] to the" );
pragma annotate( description, "range [-1, 0]." );
pragma annotate( see_also, "http://rosettacode.org/wiki/Map_range" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure mapping is
   type first_range  is new float;
   type second_range is new float;
   -- Spar doesn't implement ranges so we'll use constants
   first_range_first : constant first_range := 0.0;
   first_range_last : constant first_range := 10.0;
   second_range_first : constant second_range := -1.0;
   second_range_last : constant second_range := 0.0;

   function translate (first_range_value : first_range) return second_range is
      b1 : float := float( second_range_first );
      b2 : float := float( second_range_last );
      a1 : float := float( first_range_first );
      a2 : float := float( first_range_last );
      result : float;
   begin
      result := b1 + (float (first_range_value) - a1) * (b2 - b1) / (a2 - a1);
      return second_range(result);
   end translate;

   function translate_back (second_range_value : second_range) return first_range is
      b1 : float := float (first_range_first);
      b2 : float := float (first_range_last);
      a1 : float := float (second_range_first);
      a2 : float := float (second_range_last);
      result : float;
   begin
      result := b1 + (float (second_range_value) - a1) * (b2 - b1) / (a2 - a1);
      return first_range (result);
   end translate_back;

   test_value            : first_range := first_range_first;
   translated_value      : second_range;
   translated_back_value : first_range;
begin
   loop
      translated_value := translate( test_value );
      translated_back_value := translate_back( translated_value );

      ? strings.image(test_value) & " maps to: "
                          & strings.image (translated_value);
      ? strings.image(translated_value) & " maps back to: "
                          & strings.image (translated_back_value);
      exit when test_value = first_range_last;
      test_value := @ + 1.0;
   end loop;
end mapping;

-- VIM editor formatting instructions
-- vim: ft=spar

