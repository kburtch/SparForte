#!/usr/local/bin/spar

pragma annotate( summary, "write_ownership" )
       @( description, "A demonstration of the ownership of global variables" )
       @( description, "shared by functions in the same expression." )
       @( errors, "This script will fail" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure write_ownership is
  global_var : natural := 1;

  -- these procedures are OK

  procedure first_writer is
  begin
    global_var := @ + 2;
  end first_writer;

  procedure second_writer is
  begin
   global_var := @ * 3;
  end second_writer;

  -- these functions will cause an error if they appear in the
  -- same expression.

  function third_writer return natural is
  begin
    global_var := @ + 2;
    return 1;
  end third_writer;

  function fourth_writer return natural is
  begin
   global_var := @ * 3;
   return 1;
  end fourth_writer;

begin

  first_writer;
  second_writer;
  ? third_writer;
  ? fourth_writer;

  -- ERROR: although not an expression side-effect, two different
  -- functions are updating the same global in one expression.
  -- These are not the same because the value of global_var
  -- may be different:
  --
  -- third_writer + fourth_writer
  -- fourth_writer + third_writer
  --
  -- There should be one function that "owns" (is responsible for)
  -- changes to a global variable that is used in an expression.

  ? third_writer + fourth_writer;
end write_ownership;

-- VIM editor formatting instructions
-- vim: ft=spar

