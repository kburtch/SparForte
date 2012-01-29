#!/usr/local/bin/spar

pragma annotate( "gray" );
pragma annotate( "" );
pragma annotate( "Gray code is a form of binary encoding where " );
pragma annotate( "transitions between consecutive numbers differ by" );
pragma annotate( "only one bit. Create functions to encode a number" );
pragma annotate( "to and decode a number from Gray code. Display the" );
pragma annotate( "normal binary representations, Gray code" );
pragma annotate( "representations, and decoded Gray code values for all" );
pragma annotate( "5-bit binary numbers (0-31 inclusive, leading 0's not" );
pragma annotate( "necessary).  There are many possible Gray codes. The" );
pragma annotate( "following encodes what is called 'binary reflected" );
pragma annotate( "Gray code.'"  );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Gray_code" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure gray is

   bits : constant natural := 5;
   subtype values is natural;

   function encode (binary : values) return values is
   begin
      return binary xor numerics.shift_right (binary, 1);
   end encode;

   -- SparForte 1.3 cannot print to numbers to different bases but we
   -- we can write a function

   function intToBin( value : values ) return string is
     result : string;
     v      : values := value;
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

   function decode (gray : values) return values is
      binary : values;
      bit    : values;
      mask   : values := 2 ** (bits - 1);
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

   j       : values;
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

