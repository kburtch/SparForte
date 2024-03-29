#!/usr/local/bin/spar

pragma annotate( summary, "caesar" )
              @( description, "Implement a Caesar cipher, both encoding and ")
              @( description, "decoding.  The key is an integer from 1 to " )
              @( description, "25.  This cipher rotates (either towards left ")
              @( description, "or right) the letters of the alphabet (A to " )
              @( description, "Z). " )
              @( see_also, "http://rosettacode.org/wiki/Caesar_cipher" )
              @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure caesar is

   type cipher_value is new natural;

   function to_cipher_value(c: character; offset: character) return cipher_value is
      cv : integer;
   begin
      cv := ( numerics.pos(c)-numerics.pos(offset) ) mod 26;
      return cipher_value(cv);
   end to_cipher_value;

   function to_character(cv: cipher_value; offset: character) return character is
   begin
      return strings.val( integer(cv) + numerics.pos(offset) );
   end to_character;

   function encrypt( plain: string; key: cipher_value) return string is
      cv : cipher_value;
      cipher_char : character;
      cipher_text : string;
      plain_char  : character;
   begin
      for i in 1..strings.length( plain ) loop
         plain_char := strings.element( plain, i);
         if plain_char >= 'A' and plain_char <= 'Z' then
            cv := ( to_cipher_value( plain_char, 'A')+key ) mod 26;
            cipher_char := to_character( cv, 'A' );
         elsif plain_char >= 'a' and plain_char <= 'z' then
            cv := ( to_cipher_value( plain_char, 'a')+key ) mod 26;
            cipher_char := to_character( cv, 'a' );
         else
            cipher_char := plain_char;
         end if;
         cipher_text := strings.overwrite( @, i, string( cipher_char ) );
      end loop;
      return cipher_text;
   end encrypt;

   text:  string := get_line;
   key: constant cipher_value := 3; -- Default key from "Commentarii de Bello Gallico"

begin
   put_line("Plaintext ------------>" & text);
   text := encrypt(text, key);
   put_line("Ciphertext ----------->" & text);
   text := encrypt(text, -key);
   put_line("Decrypted Ciphertext ->" & text);
end caesar;

-- VIM editor formatting instructions
-- vim: ft=spar

