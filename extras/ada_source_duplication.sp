#!/usr/local/bin/spar

pragma annotate( summary, "ada_source_duplication" );
pragma annotate( description, "Read through the package body files in the Sparforte src directory" );
pragma annotate( description, "and check for duplicate lines of code.  This is for refactoring" );
pragma annotate( description, "purposes.  This takes a while to run." );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

procedure ada_source_duplication is

match_limit : constant natural := 5;
src_dir     : constant string := "../src";

d : directory_operations.dir_type_id;

f : string;

sources : doubly_linked_lists.list;

source_fname  : string;
source_file   : file_type;
source_lines  : doubly_linked_lists.list;
source_cursor : doubly_linked_lists.cursor;
source_lineno : positive;

target_fname  : string;
target_file   : file_type;
target_lines  : doubly_linked_lists.list;
target_cursor : doubly_linked_lists.cursor;
target        : doubly_linked_lists.cursor;

matches       : doubly_linked_lists.list;
lines_match   : boolean;
count : natural;
biggest_count : natural;

first_line    : string;
has_first_line : boolean;

comment_start : natural;
temp          : string;

begin

  -- Get the list of source files to compare.  Read them into a linked
  -- list for reference.

  doubly_linked_lists.new_list( sources, string );
  directory_operations.open( d, src_dir );
  loop
    directory_operations.read( d, f );
    exit when f = "";
    if strings.tail( f, 4 ) = ".adb" then
       doubly_linked_lists.append( sources, f );
    end if;
  end loop;
  directory_operations.close( d );

  -- Create the linked lists that will be used to assess the files

  doubly_linked_lists.new_list( source_lines, string )
                             @( target_lines, string )
                             @( matches, string );
  doubly_linked_lists.new_cursor( source_cursor, string )
                               @( target_cursor, string )
                               @( target, string );

  while not doubly_linked_lists.is_empty( sources ) loop

      -- Read next source file into a linked list

      source_fname := doubly_linked_lists.first_element( sources );
new_line; -- to compensate for blip line
? "checking " & source_fname;

      doubly_linked_lists.clear( source_lines );
      open( source_file, in_file, src_dir & "/" & source_fname );
      doubly_linked_lists.clear( source_lines );
      while not end_of_file( source_file ) loop
         doubly_linked_lists.append( source_lines, get_line( source_file ) );
      end loop;
      close( source_file );
      doubly_linked_lists.delete_first( sources );

      -- Compare the lines of this source file to the rest of the source
      -- files.  Since we eliminate files we've already examined, there's
      -- no need to test the same two files twice.  Also don't examine
      -- current file against itself.

      doubly_linked_lists.first( sources, target );
-- TODO: matching against the file itself should be possible
      doubly_linked_lists.next(  target );

      while doubly_linked_lists.has_element( target ) loop

         target_fname := doubly_linked_lists.element( target );
 --? "checking against " & target_fname;
put( "." );

         -- Read target file into a linked list

         doubly_linked_lists.clear( target_lines );
         open( target_file, in_file, src_dir & "/" & target_fname );
         while not end_of_file( target_file ) loop
            doubly_linked_lists.append( target_lines, get_line( target_file ) );
         end loop;
         close( target_file );

         -- Now compare the lines of the two files.  Display the set of
         -- lines that match.

         doubly_linked_lists.first( source_lines, source_cursor );
         source_lineno := 1;

         while doubly_linked_lists.has_element( source_cursor ) loop
            biggest_count := 0;
            first_line := doubly_linked_lists.element( source_cursor );
--? strings.image( source_lineno ) & ": '" & first_line & "'";

         -- Do a quick search.  Don't bother with a slow line-by-line search
         -- if the line never appears in the target file.

            has_first_line := false;
            temp := first_line;
            -- remove any comment
            comment_start := strings.index( temp, "--" );
            if comment_start  > 0 then
               temp := strings.delete( temp, comment_start, strings.length( temp ) );
            end if;
            -- ignore block ending statements
            comment_start := strings.index( temp, "end if;" );
            if comment_start  > 0 then
               temp := strings.delete( temp, comment_start, comment_start + 6 );
            end if;
            comment_start := strings.index( temp, "end loop;" );
            if comment_start  > 0 then
               temp := strings.delete( temp, comment_start, comment_start + 8 );
            end if;
            comment_start := strings.index( temp, "end case;" );
            if comment_start  > 0 then
               temp := strings.delete( temp, comment_start, comment_start + 8 );
            end if;
            comment_start := strings.index( temp, "end;" );
            if comment_start  > 0 then
               temp := strings.delete( temp, comment_start, comment_start + 3 );
            end if;
            -- remove leading spaces
            while strings.length( temp ) > 0 loop
               exit when strings.element( temp, 1 ) /= ' ';
               temp := strings.delete( temp, 1, 1 );
            end loop;
            -- remove trailing spaces
            while strings.length( temp ) > 0 loop
               exit when strings.element( temp, strings.length( temp ) ) /= ' ';
               temp := strings.delete( temp, strings.length( temp ), strings.length( temp ) );
            end loop;
            -- ignore line if it is empty.  Otherwise, search for it.  If it
            -- exists, then do the line-by-line search.
            if temp /= "" then
               if doubly_linked_lists.contains( target_lines, first_line ) then
                  has_first_line;
               end if;
            end if;
            if has_first_line then

               --doubly_linked_lists.find( target_lines, first_line, find_cursor );
               --if doubly_linked_lists.has_element( find_cursor ) then

               -- walk through the target file line-by-line

               doubly_linked_lists.first( target_lines, target_cursor );

               while doubly_linked_lists.has_element( target_cursor ) loop
-- TODO: matches should ignore leading whitespace
                 if first_line = doubly_linked_lists.element( target_cursor ) then
                    count := 1;
                    doubly_linked_lists.clear( matches );
                    loop
                      doubly_linked_lists.append( matches,
                         doubly_linked_lists.element( source_cursor ) );
                      doubly_linked_lists.next( target_cursor ) @ ( source_cursor );
                      -- Do the two lines match?  Fail if at end-of-file.
                      lines_match := false;
                      if doubly_linked_lists.has_element( source_cursor ) and
                         doubly_linked_lists.has_element( target_cursor ) then
                            if doubly_linked_lists.element( source_cursor ) =
                               doubly_linked_lists.element( target_cursor ) then
                               lines_match;
                            end if;
                       end if;
                       -- if lines match don't match, but we have at least 5 lines
                       -- previously that do, show the lines.  Track the biggest
                       -- number that match...we will need to skip that many lines
                       -- in the source file to avoid showing the same lines again.
                       if not lines_match then
                         if count >= match_limit then
                            ? source_fname & ":" &
                              strings.image( source_lineno ) &
                              " matched " &
                              target_fname &
                              " for" &
                              strings.image( count ) &
                              " lines:";
                            while not doubly_linked_lists.is_empty( matches ) loop
                              ? doubly_linked_lists.first_element( matches );
                              doubly_linked_lists.delete_first( matches );
                            end loop;
                            if count > biggest_count then
                               biggest_count := count;
                            end if;
                         end if;
                         -- Roll back source lines to the first line in the
                         -- consecutive lines that matched.  If we read past the
                         -- end of the file, move the cursor back to the end.
                         if not doubly_linked_lists.has_element( source_cursor ) then
                             doubly_linked_lists.last( source_lines, source_cursor );
                             count := @ - 1;
                         end if;
                         for i in 1..count loop
                            doubly_linked_lists.previous( source_cursor );
                         --   source_lineno := @ - 1;
   --? "reset to " & doubly_linked_lists.element( source_cursor );
                         end loop;
                         exit;
                       end if; -- consecutive lines don't match
                       count := @+1;
                    end loop;
                 end if;
                 doubly_linked_lists.next( target_cursor );
               end loop;
            end if; -- if first line exists
            for i in 1..biggest_count+1 loop
                doubly_linked_lists.next( source_cursor );
                source_lineno := @ + 1;
            end loop;
            exit when not doubly_linked_lists.has_element( source_cursor );
         end loop;

      doubly_linked_lists.next( target );
      end loop; -- all targets
   end loop;

end ada_source_duplication;

-- VIM editor formatting instructions
-- vim: ft=spar

