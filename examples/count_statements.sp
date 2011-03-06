#!/usr/local/bin/bush

-- Estimate the number of SparForte or Ada statements in a source code file

pragma annotate( "Count Statements" );
pragma annotate( "" );
pragma annotate( "Estimate the number of SparForte or Ada statements in a source code file" );
pragma annotate( "by William A. Whitaker  WIS JPMO   3 March 1984" );
pragma annotate( "adapted to SparForte by Ken O. Burtch" );

pragma restriction( no_external_commands );

procedure count_statements is 

  FILE_NAME_TO_CHECK  : string;

  function LOC (FILE_NAME  : string) return integer is
  --  This function calculates the "lines of code" of a valid Ada fragment
  --  specified by a FILE_NAME string parameter
  --  It need not be a complete compilation unit
  --  but it must have closed all open parentheses and string brackets
  --  The number of lines of code is returned as an INTEGER

  --  The line of code is defined by a semicolon terminator
  --  outside of comments, parentheses, or string or character literals
  --  This definition is insensitive to formatting or layout of the source

  --  This source code of function LOC has 31 lines by this definition
  --  It has 107 physical lines in its initial formatted form
  --  including 39 lines containing only comment and 18 blank lines

  --  There are exotic cases for which this will give the wrong answer

    INPUT   : file_type;
    C       : character := ' ';
    LINES   : integer := 0;
    LEVEL   : integer := 0;

  begin

    open (INPUT, in_file, FILE_NAME);

    loop
      exit when end_of_file( INPUT );
      get (INPUT, C);

      --  Check for comment on the line
      if C = '-' then
        exit when end_of_file( INPUT );
        get (INPUT, C);
        --  Which is signaled by the '-' following a '-'
        if C = '-' then
          --  Then just skip the rest of the line and go to the next
          skip_line (INPUT);
        end if;
      end if;

      --  Check for one of the characters which introduce code constructs
      --  like string or character literal or formal parameter list
      --  within which a ';' does not terminate a "line of code"
      if C = '(' or C = '"' or C = '%' or C = ''' or C = '`' then

        --  Check for opening parentheses
        --  Every ';' within is in a formal parameter list
        if C = '(' then
          --  Count the number of levels of parentheses
          LEVEL := @ + 1;
          --  Read ahead until the whole construct is closed, LEVEL = 0
          while LEVEL > 0 loop
            exit when end_of_file( INPUT );
            get (INPUT, C);
            if C = '(' then
              --  Increase the level if another '(' is found
              LEVEL := @ + 1;
            elsif C = ')' then
              --  Decrease the level if a ')' is found
              LEVEL := @ - 1;
            end if;
          end loop;

        --  Now check for string brackets of either kind, " or %
        elsif C = '"' or C = '%' then
          --  Treat them in parallel, one must lead off
          if C = '"' then
            loop
              exit when end_of_file( INPUT );
              get (INPUT, C);
              --  Loop until the close comes
              --  If there is a doubled character it just starts again
              exit when C = '"';
            end loop;
          --  The '%' is handled exactly the same way as '"'
          elsif C = '`' then
            loop
              exit when end_of_file( INPUT );
              get (INPUT, C);
              --  Loop until the close comes
              --  If there is a doubled character it just starts again
              exit when C = '`';
            end loop;
          --  The '%' is handled exactly the same way as '"'
          elsif C = '%' then
            loop
              exit when end_of_file( INPUT );
              get (INPUT, C);
              exit when C = '%';
            end loop;
          elsif C = ''' then
            loop
              exit when end_of_file( INPUT );
              get (INPUT, C);
              --  Loop until the close comes
              --  If there is a doubled character it just starts again
              exit when C = ''';
            end loop;
          --  The '%' is handled exactly the same way as '"'
          end if;

        end if;

      --  Any ';' that can be found at this point after all exclusions
      --  must be a valid "line of code" terminator
      elsif C = ';' then
        LINES := @ + 1;

      end if;

    end loop;

    return LINES;

  end LOC;


begin
  put("Input file name terminated by <RETURN> => ");
  FILE_NAME_TO_CHECK := get_line;
  new_line; new_line;
  put (LOC (FILE_NAME_TO_CHECK));
  put (" STATEMENTS IN SCRIPT " ) @ (FILE_NAME_TO_CHECK); 
  new_line; 
end count_statements;

-- VIM editor formatting instructions -- vim: ft=bush

