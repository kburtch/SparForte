#!/usr/local/bin/spar

pragma annotate( summary, "web_summary" )
       @( description, "Simple web summary of an Apache server log" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

procedure web_summary is
  error_status  : constant short_short_integer := 192;
  log_path      : constant string := directory_operations.format_pathname( "/var/log/apache2" );
  log_file_name : constant string := "access_log";
  log_file      : constant string := log_path & directory_operations.dir_separator & log_file_name;
begin

  -- Verify the log directory exists and that the log file is readable

  if not files.is_directory( log_path ) then
     put( standard_error, source_info.file )
         @( standard_error, ": log path is not a directory" );
     command_line.set_exit_status( error_status );
     return;
  end if;
  if not files.is_readable_file( log_file ) then
     put( standard_error, source_info.file )
         @( standard_error, ": log file does not exist or is not readable" );
     command_line.set_exit_status( error_status );
     return;
  end if;

  declare
    this_month : constant calendar.month_number := calendar.month( calendar.clock );
    current_month : string;
    today : string;

    month_total_hits : natural;
    month_users_hits : natural;
    day_total_hits : natural;
    day_users_hits : natural;
  begin

    -- Create today and current_month strings for grepping the log file
    -- This may be dependent on your Apache configuration.

    today := strings.trim( strings.image( calendar.year( calendar.clock ) ),
             trim_end.left );
    today := "/" & @;
    case this_month is
    when  1 => today := "Jan" & @;
    when  2 => today := "Feb" & @;
    when  3 => today := "Mar" & @;
    when  4 => today := "Apr" & @;
    when  5 => today := "May" & @;
    when  6 => today := "Jun" & @;
    when  7 => today := "Jul" & @;
    when  8 => today := "Aug" & @;
    when  9 => today := "Sep" & @;
    when 10 => today := "Oct" & @;
    when 11 => today := "Nov" & @;
    when 12 => today := "Dec" & @;
    when others => put_line( standard_error, "bad month" );
    end case;
    current_month := today;
    today := strings.trim( strings.image( calendar.day( calendar.clock ) ),
             trim_end.left ) & "/" & @;
    if strings.length( today ) < 11 then
       today := "0" & @;
    end if;

    -- Collect the statistics
    --
    -- Select only GET lines.  Of those lines, select only the current day or
    -- month.  For uses, examine the first field of the log and count unique
    -- visitors.

    month_total_hits := numerics.value( `grep GET /var/log/httpd/access_log | grep "$current_month" | wc -l ;` );
    month_users_hits := numerics.value( `grep GET /var/log/httpd/access_log | grep "$current_month" | cut -d\  -f 1 | sort -u | wc -l ;` );
    day_total_hits := numerics.value( `grep GET /var/log/httpd/access_log | grep "$current_month" | wc -l ;` );
    day_users_hits := numerics.value( `grep GET /var/log/httpd/access_log | grep "$current_month" | cut -d\  -f 1 | sort -u | wc -l ;` );

    -- Display the report

    put_line( "Month to Date - " & current_month );
    new_line;

    put( "Total hits:                 " ) @ ( month_total_hits, "ZZZZZZZ" );
    new_line;
    put( "Total users:                " ) @ ( month_users_hits, "ZZZZZZZ" );
    new_line;

    new_line;
    put_line( "Today (so far) - " & today );
    new_line;
    put( "Total hits:                 " ) @ ( day_total_hits, "ZZZZZZZ" );
    new_line;
    put( "Total users:                " ) @ ( day_users_hits, "ZZZZZZZ" );
    new_line;

  end;

  command_line.set_exit_status( 0 );

end web_summary;

-- VIM editor formatting instructions
-- vim: ft=spar

