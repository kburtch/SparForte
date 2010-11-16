with gen_list,
     interfaces.C,
     ada.text_io,
     ada.command_line,
     ada.strings.unbounded;
use  interfaces.C,
     ada.text_io,
     ada.command_line,
     ada.strings.unbounded;

procedure coverage is

  -- Standard C library system() function

  function linux_system( s : string ) return int;
  pragma import( C, linux_system, "system" );

  type lineCoverage is record
        lineNumber : long_integer;                          -- the line number
        lineCount  : long_integer;                           -- times executed
        text       : unbounded_string;                     -- source code line
  end record;

  -- coverageList is a linked list

  function ">=" ( left, right : lineCoverage ) return boolean is
  begin
    return left.lineNumber > right.lineNumber;
  end ">=";

  function "=" ( left, right : lineCoverage ) return boolean is
  begin
    return left.lineNumber = right.lineNumber;
  end "=";
  
  package coverageList is new gen_list( lineCoverage, "=", ">=" );
  
  source_path : unbounded_string;
  coverage    : coverageList.List;
  num_lines   : long_integer;

begin

  if argument_count = 0 then
     put_line( "usage: coverage bush_script" );
     set_exit_status( 0 );
     return;
  end if;

  source_path := to_unbounded_string( argument( 1 ) );
 
  -- Read the source code into the coverage list. Count the lines

  declare
    source_file : file_type;
    path : string := to_string( source_path );
    line_info : lineCoverage;
    line_number : long_integer := 0;
  begin
    Open( source_file, in_file, to_string( source_path ) );
    while not end_of_file( source_file ) loop
       line_number := line_number + 1;
       line_info.lineNumber := line_number;
       line_info.lineCount := 0;
       line_info.text := to_unbounded_string( get_line( source_file ) );
       coverageList.Queue( coverage, line_info );
    end loop;
    Close( source_file );
    num_lines := coverageList.Length( coverage );
  end;
  
  -- Run the script in trace mode

  declare
    res : int;
  begin
     res := linux_system( "bush --trace """ & to_string( source_path ) & """ 2>t.t >/dev/null" & ASCII.NUL);
     if res /= 0 then
        put_line( "running command failed, status code" & res'img );
        set_exit_status( 255 );
        return;
     end if;
  end;

  -- Open the trace output and tally the lines that executed

  declare
    trace_file : file_type;
    temp : unbounded_string;
  begin
    Open( trace_file, in_file, "t.t" );
    while not end_of_file( trace_file ) loop
       temp := to_unbounded_string( get_line( trace_file ) );
       -- the line number is at the end of the trace line in square brackets
       if length( temp ) > 4 then
          if head( temp, 4 ) = "=> """ then
             if element( temp, length( temp ) ) = ']' then
                declare
                   i : integer := length( temp ) - 1;
                   old_count : lineCoverage;
                   location : coverageList.aListIndex := 0;
                   lineNumber : long_integer;
                begin
                   while element( temp, i ) /= '[' loop
                      i := i - 1;
                   end loop;
                   lineNumber := long_integer'value( slice( temp, i+1, length( temp ) -1 ) );
                   old_count.lineNumber := lineNumber;
                   coverageList.Find( coverage, old_count, foundAt => location );
                   if location > 0 then
                      coverageList.Find( coverage, location, old_count );
                      old_count.lineCount := old_count.lineCount + 1;
                      coverageList.Replace( coverage, location, old_count );
                   else
                      old_count.lineNumber := lineNumber;
                      old_count.lineCount := 1;
                      coverageList.Queue( coverage, old_count );
                   end if;

                end;
             end if;
          end if;
       end if;
    end loop;
    Delete( trace_file );
  end;

  -- Display the line coverage

  declare
     line_info : lineCoverage;
     source_line : unbounded_string;
     lines_covered : long_integer := 0;
     percent : long_integer;
  begin
      coverageList.Pull( coverage, line_info );
      while not coverageList.isEmpty( coverage ) loop
         if line_info.lineCount = 0 then
           -- Bush's byte code compiler discards any leading #! line so we
           -- won't have trace output for it
           if line_info.lineNumber = 1 and element( line_info.text, 1) = '#' then
              put( " 1" );
              lines_covered := lines_covered + 1;
           else
              put( " -" );
           end if;
         else
           put( line_info.lineCount'img );
           lines_covered := lines_covered + 1;
         end if;
         put( line_info.lineNumber'img & " " );
         put_line( to_string( line_info.text ) );
         coverageList.Pull( coverage, line_info );
      end loop;
      coverageList.clear( coverage );

      -- Compute line coverage
      percent := 100 * lines_covered / num_lines;
      put( "Line coverage is" );
      put( lines_covered'img & "/" & num_lines'img );
      put_line( " (" & percent'img & "%)" );
  end;
      
end coverage;

