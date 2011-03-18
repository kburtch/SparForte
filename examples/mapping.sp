#!/usr/local/bin/bush

pragma annotate( "mapping" );
pragma annotate( "" );
pragma annotate( "The task is to write a function/subroutine/... that takes" );
pragma annotate( "two ranges and a real number, and returns the mapping of" );
pragma annotate( "the real number from the first to the second range. Use" );
pragma annotate( "this function to map values from the range [0, 10] to the" );
pragma annotate( "range [-1, 0]." );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Map_range" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure mapping is
   type first_range  is new float;
   type second_range is new float;
   -- Bush doesn't implement ranges so we'll use constants
   first_range_first : constant first_range := 0.0;
   first_range_last : constant first_range := 10.0;
   second_range_first : constant second_range := -1.0;
   second_range_last : constant second_range := 0.0;

   function translate (value : first_range) return second_range is
      b1 : float := float( second_range_first );
      b2 : float := float( second_range_last );
      a1 : float := float( first_range_first );
      a2 : float := float( first_range_last );
      result : float;
   begin
      result := b1 + (float (value) - a1) * (b2 - b1) / (a2 - a1);
      return second_range(result);
   end translate;

   function translate_back (value : second_range) return first_range is
      b1 : float := float (first_range_first);
      b2 : float := float (first_range_last);
      a1 : float := float (second_range_first);
      a2 : float := float (second_range_last);
      result : float;
   begin
      result := b1 + (float (value) - a1) * (b2 - b1) / (a2 - a1);
      return first_range (result);
   end translate_back;
   test_value : first_range := first_range_first;
begin
   loop
      ? strings.image(test_value) & " maps to: "
                          & strings.image (translate (test_value));
      exit when test_value = first_range_last;
      test_value := @ + 1.0;
   end loop;
end mapping;

