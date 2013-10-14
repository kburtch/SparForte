#!/usr/local/bin/spar

pragma annotate( summary, "eliza" );
pragma annotate( description, "The psychologist imitation program" );
pragma annotate( description, "Original author: Joseph Weizenbaum" );
pragma annotate( description, "Usage: eliza" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure eliza is

-- Keywords
--
-- Keywords and responses are loaded into the keywords
-- variable.  Keywords are prefixed with the keyword_tag
-- character, responses with the response_tag character.

delimiter  : constant character := '~';
nokey_marker : constant string := delimiter & "NOKEYFOUND";

-- keyword lists are pairs of a keyword and first reponse field
-- reponses are lists of responses terminated with an empty field

single_keywords : string := "";
num_single      : natural := 0;
multi_keywords  : string := "";
num_multi       : natural := 0;
responses       : string := "";
num_responses   : natural := 0;

-- User Input
--
-- i is the what the user types.  previous is the
-- previous value of i, used to check if the user
-- repeats his/herself.

i : string := "";
previous : string := "";

begin

-- initialization

put_line( "Eliza" );
new_line;

declare
  type data_type is ( keyword, replies );
  data_file : file_type;
  current_type : data_type := keyword;
  s : string;
  terminate_response_list : boolean := false;
begin
  put_line( "Please wait...loading keywords/responses..." );
  open( data_file, in_file, "eliza.data" );
  while not end_of_file( data_file ) loop
       s := get_line( data_file );
       if s = "." then
	  current_type := keyword;
       elsif s = "!" then
	  current_type := replies;
       elsif current_type = keyword then
	  if terminate_response_list then
	     responses := @ & delimiter;
	     num_responses := @+1;
	     terminate_response_list := false;
          end if;
	  if strings.index( s, ' ' ) = 0 then
	     single_keywords := @ & s & delimiter;
	     single_keywords := @ & strings.image( num_responses+1 ) & delimiter;
	     num_single := @+2;
          else
	     multi_keywords := @ & s & delimiter;
	     multi_keywords := @ & strings.image( num_responses+1 ) & delimiter;
	     num_multi := @+2;
          end if;
       else
	  responses := @ & s & delimiter;
	  num_responses := @+1;
	  terminate_response_list;
       end if;
       put( "." );
  end loop;
  responses := @ & delimiter;
  close( data_file );
  new_line;
end;

put_line( "HI!  I'M ELIZA.  LET'S TALK.  TYPE `BYE' TO END THIS SESSION." );

-- get next line of input

loop

-- read user's input

i := "";
while i = "" loop
  put( ">" );
  i := get_line;
end loop;
i := ' ' & i & ' ';

-- clean up input

declare
  c : character;
  new_i : string := "";
  last_was_space : boolean := false;
begin
  for l in 1..strings.length( i ) loop
    c := strings.element( i, l );
    if c = ' ' then
       if last_was_space then
          null;
       else
          new_i := @ & c;
          last_was_space := true;
       end if;
    else
       last_was_space := false;
       if c >= "a" and c <= "z" then
          new_i := @ & strings.val(  numerics.pos( c ) - 32 );
       elsif c >= '0' and c <= '9' then
          new_i := @ & c;
       elsif c >= "A" and c <= 'Z' then
          new_i := @ & c;
       end if;
    end if;
  end loop;
  i := new_i;
end;

-- test for the basics

if i = previous then
   put_line( "PLEASE DON'T REPEAT YOURSELF!" );
elsif i = " BYE " or i = " LOGOUT " then
   put_line( "TALK TO YOU LATER!  BYE!" );
   exit;
else
   previous := i;

   -- look for keyword(s)

   declare
      remains : string := "";

      eliza_reply : string := "";
      testword    : string := "";
      response_pos: natural := 0;
   begin

      -- look for single keywords

      declare
        k : positive; -- keyword in the user's input
        keyword : string;
      begin
        k := 2; -- skip first null "field"
        loop
          testword := strings.field( i, k, ' ' );
          exit when testword = "";
          -- not 100% since doesn't take into account leading delimiter
          -- since first single keyword has no leading delimiter
	  if strings.index( single_keywords, testword & delimiter ) > 0 then
             for sk in 1..num_single loop
                 keyword := strings.field( single_keywords, sk, delimiter );
                 if keyword = testword then
	            response_pos := numerics.value( strings.field( single_keywords, sk+1, delimiter ) );
                    exit;
                 end if;
             end loop;
          end if;
          exit when response_pos > 0;
	  k := @+1;
        end loop;
      end;

      -- no match? look for multiple keywords

      if response_pos = 0 then
         declare
           k  : positive;
         begin
           k := 1;
           while k < positive( num_multi ) loop
	       testword := strings.field( multi_keywords, k, delimiter ); 
	       if strings.index( i, ' ' & testword & ' ' ) > 0 then
	          response_pos := numerics.value( strings.field( multi_keywords, k+1, delimiter ) );
	          exit;
               end if;
	       k := @+1;
           end loop;
         end;
      end if;

      -- still no match? use fallback responses else get remainder
      -- of user input after the keyword(s)

      if response_pos = 0 then
	 testword := nokey_marker;
	 response_pos := numerics.value( strings.field( single_keywords, num_single, delimiter ) );
      else
         remains := strings.slice( i, positive( strings.index( i, testword ) +
            strings.length( testword ) ), strings.length( i ) );
      end if;

      -- rewrite the remainder of the input

      declare
        c : natural;
        p : positive;
      begin
        c := strings.index( remains, " ARE " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+4, " AM+ " );
        end if;
        c := strings.index( remains, " AM " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+3, " ARE+ " );
        end if;
        c := strings.index( remains, " WERE " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+5, " WAS+ " );
        end if;
        c := strings.index( remains, " WAS " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+4, " WERE+ " );
        end if;
        c := strings.index( remains, " YOU " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+4, " I+ " );
        end if;
        c := strings.index( remains, " I " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+2, " YOU+ " );
        end if;
        c := strings.index( remains, " YOUR " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+5, " MY+ " );
        end if;
        c := strings.index( remains, " MY " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+3, " YOUR+ " );
        end if;
        c := strings.index( remains, " IVE " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+4, " YOUVE+ " );
        end if;
        c := strings.index( remains, " YOUVE " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+6, " IVE+ " );
        end if;
        c := strings.index( remains, " IM " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+3, " YOURE+ " );
        end if;
        c := strings.index( remains, " ME " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+3, " YOU+ " );
        end if;
        c := strings.index( remains, " US " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+3, " YOU+ " );
        end if;
        c := strings.index( remains, " WE " );
        if c > 0 then
           p := positive( c );
           remains := strings.replace_slice( remains, p, c+3, " YOU+ " );
        end if;
        loop
           c := strings.index( remains, "+" );
           exit when c = 0;
           remains := strings.delete( remains, positive( c ), c );
        end loop;
        if strings.tail( remains, 3 ) = " I " then
           remains := strings.slice( remains, 1,
              strings.length( remains ) - 2 )
              & "ME ";
	end if;
      end;
--put_line( "Remains (after conjugation): " & remains );

	 -- attach reply

      declare
        last_pos    : natural := 0;
        reply_cnt   : natural := 0;
        reply       : natural := 0;
        ch          : character;
        response    : string;
      begin

        -- count replies

        last_pos := response_pos+1;
        loop
           response := strings.field( responses, last_pos, delimiter );
           exit when response = "";
           last_pos := @+1;
        end loop;
        reply_cnt := last_pos - response_pos;

	reply := numerics.truncation(numerics.random * float(reply_cnt) );
        eliza_reply := strings.field( responses, response_pos+reply,
          delimiter );

        ch := strings.element( eliza_reply, positive( strings.length( eliza_reply ) ) );
        if ch = '*' then
           eliza_reply := strings.head( eliza_reply,
             strings.length( eliza_reply )-1 ) & remains;
        end if;
      end;
      put_line( eliza_reply );
   end;
end if;

end loop;

end eliza;

-- VIM editor formatting instructions
-- vim: ft=spar

