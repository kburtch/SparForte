#!/usr/local/bin/spar

pragma annotate( summary, "gray" );
pragma annotate( description, "Gray code is a form of binary encoding where " );
pragma annotate( description, "transitions between consecutive numbers differ by" );
pragma annotate( description, "only one bit. Create functions to encode a number" );
pragma annotate( description, "to and decode a number from Gray code. Display the" );
pragma annotate( description, "normal binary representations, Gray code" );
pragma annotate( description, "representations, and decoded Gray code values for all" );
pragma annotate( description, "5-bit binary numbers (0-31 inclusive, leading 0's not" );
pragma annotate( description, "necessary).  There are many possible Gray codes. The" );
pragma annotate( description, "following encodes what is called 'binary reflected" );
pragma annotate( description, "Gray code.'"  );
pragma annotate( category, "algorithms" );
pragma annotate( see_also, "http://rosettacode.org/wiki/Gray_code" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure gray is

   bits : constant natural := 5;
   subtype nat_values is natural;

   function encode (binary : nat_values) return nat_values is
   begin
      return binary xor numerics.shift_right (binary, 1);
   end encode;

   -- SparForte 1.3 cannot print to numbers to different bases but we
   -- we can write a function

   function intToBin( nat_value : nat_values ) return string is
     result : string;
     v      : nat_values := nat_value;
   begin
     if v = 0 then
        result := '0';
     else
       while v > 0 loop
         if (v and 1) = 1 then
            result := '1' & @;
         else
            result := '0' & @;
         end if;
         v := numerics.shift_right( @, 1 );
       end loop;
     end if;
     return "2#" & result & "#";
   end intToBin;

   function decode (gray : nat_values) return nat_values is
      binary : nat_values;
      bit    : nat_values;
      mask   : nat_values := 2 ** (bits - 1);
   begin
      bit    := gray and mask;
      binary := bit;
      for i in 2 .. bits loop
         bit    := numerics.shift_right (@, 1);
         mask   := numerics.shift_right (mask, 1);
         bit    := (gray and mask) xor @;
         binary := @ + bit;
      end loop;
      return binary;
   end decode;

   j       : nat_values;
   ibinstr : string;
   jbinstr : string;

begin
   put_line ("Number   Binary     Gray Decoded");
   for i in 0..31 loop
      j := encode (i);
      -- convert i and j to base 2 representation
      ibinstr := intToBin(i);
      jbinstr := intToBin(j);
      -- for binary strings, right-justify
      put (i, "ZZZZZ9" ) @
          (' ' & strings.insert( ibinstr, 1, (8-strings.length(ibinstr)) * ' ' ) ) @
          (' ' & strings.insert( jbinstr, 1, (8-strings.length(jbinstr)) * ' ' ) ) @
          ( "  " ) @ (decode (j), "ZZZZZ9" );
      new_line;
   end loop;
end gray;

-- VIM editor formatting instructions
-- vim: ft=spar

