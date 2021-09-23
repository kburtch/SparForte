#!/usr/local/bin/spar

pragma annotate( summary, "scheduler" )
       @( description, "An example of a simple non-preemptive scheduler" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure scheduler is

-- EXCEPTIONS

  constraint_error : exception;
  process_error    : exception;

-- SCHEDULE NUMBER and PRIORITY
--
-- a_schedule_number is the iteration number of the scheduler
-- a_priority is added to the schedule number to determine when
-- word is rescheduled

  type a_schedule_number is new natural;

  min_priority : constant a_schedule_number := 0;
  max_priority : constant a_schedule_number := 5;

  subtype a_priority is a_schedule_number
  affirm
     raise constraint_error when a_priority < min_priority or a_priority > max_priority;
  end affirm;


-- WORK TYPES
--
-- For this example, idle means there is no work item.  run means there
-- is a work item.

  type work_types is (idle, run_once, repeat);


-- WORK PROCESS ITEM
--
-- This is an entry in the the process table.

  type work_process_item is record
       title            : string;
       work_type        : work_types;
       priority         : a_priority;
       schedule_number  : a_schedule_number;
       start_date_year  : calendar.year_number;
       start_date_month : calendar.month_number;
       start_date_day   : calendar.day_number;
       start_date_dur   : calendar.day_duration;
       due_date_year    : calendar.year_number;
       due_date_month   : calendar.month_number;
       due_date_day     : calendar.day_number;
       due_date_dur     : calendar.day_duration;
  end record;


-- PROCESSES
--
-- The process table.  The process id is the index in the table.  Because the
-- current version of SparForte does not support an array of records, records
-- will be stored as JSON strings.

  min_process : constant natural := 1;
  max_process : constant natural := 10;

  subtype a_process_id is natural
  affirm
    raise constraint_error when a_process_id < min_process or a_process_id > max_process;
  end affirm;

  work_processes : array(min_process..max_process) of json_string;


-----------------------------------------------------------------------------
--
-- SCHEDULER
--
-----------------------------------------------------------------------------


--  WAIT SCHEDULE
--
-- The schedule period is how often the scheduler will run.  This procedure
-- waits until it is time to run the scheduler again, unless the scheduler
-- is already late.
-----------------------------------------------------------------------------

  schedule_period : constant duration := 0.5;

  last_clock : calendar.time := calendar.clock;

  procedure wait_scheduler is
    elapsed_secs : duration;
    now : constant calendar.time := calendar.clock;
  begin
    elapsed_secs := now - last_clock;
    if elapsed_secs < schedule_period then
       delay schedule_period - elapsed_secs;
    end if;
    last_clock := now;
  end wait_scheduler;


--  RESET WORK
--
-- Initializes the process table.
-----------------------------------------------------------------------------

  procedure reset_work is
    item : work_process_item;
    js : json_string;
  begin
    item.title := "untitled";
    item.work_type := idle;
    item.priority := 5;
    item.schedule_number := 1;
    calendar.split( calendar.clock, item.due_date_year, item.due_date_month, item.due_date_day, item.due_date_dur );
    item.start_date_year  := item.due_date_year;
    item.start_date_month := item.due_date_month;
    item.start_date_day   := item.due_date_day;
    item.start_date_dur   := item.due_date_dur;
    records.to_json( js, item );

    for pid in min_process..max_process loop
        records.to_json( js, item );
        work_processes( pid ) := js;
    end loop;
    logs.info( "work is reset" );
  end reset_work;


--  SCHEDULE WORK
--
-- Add work to an empty position in the process table.  Process error is
-- raised if the work was not added.
-----------------------------------------------------------------------------

  procedure schedule_work( title : string; priority : a_priority; work_type : work_types; date_year : calendar.year_number; date_month : calendar.month_number; date_day : calendar.day_number; date_dur : calendar.day_duration; schedule_number : a_schedule_number ) is
    old_item : work_process_item;
    item : work_process_item;
    js : json_string;
    added : boolean := false;
  begin
    item.title := title;
    item.work_type := work_type;
    item.priority := priority;
    item.schedule_number := schedule_number;
    calendar.split( calendar.clock, item.start_date_year, item.start_date_month, item.start_date_day, item.start_date_dur );
    item.due_date_year  := date_year;
    item.due_date_month := date_month;
    item.due_date_day   := date_day;
    item.due_date_dur   := date_dur;
    records.to_json( js, item );

    for pid in min_process..max_process loop
        records.to_record( old_item, work_processes( pid ) );
        if old_item.work_type = idle then
           work_processes( pid ) := js;
           logs.info( "pid" & strings.image( pid ) & " - " & item.title & " - added" );
           added;
           exit;
        end if;
    end loop;
    if not added then
       logs.error( item.title & " - not added" );
       raise process_error;
    end if;
  end schedule_work;


--  EXEC WORK
--
-- Run a task if it is scheduled or if the scheduler is late for due date.
-----------------------------------------------------------------------------

  procedure exec_work( pid : a_process_id; schedule_number : a_schedule_number ) is
    item : work_process_item;
    js   : json_string;
  begin
    records.to_record( item, work_processes( pid ) );
    case item.work_type is
    when run_once =>
       if item.schedule_number = schedule_number then
          logs.ok( "pid" & strings.image( pid ) & " - " & item.title & " - exec" );
          -- the next run is rescheduled by the priority

          if item.priority = 0 then
             item.schedule_number := schedule_number + 1;
          else
             item.schedule_number := schedule_number + item.priority;
          end if;

          -- Remove the entry from the process table

          item.work_type := idle;
          records.to_json( js, item );
          work_processes( pid ) := js;
       elsif calendar.clock > calendar.time_of( item.due_date_year, item.due_date_month, item.due_date_day, item.due_date_dur ) then

          -- If it's not time to run but the run is late, schedule the task
          -- for the next schedule iteration

          item.schedule_number := schedule_number + 1;
          records.to_json( js, item );
          work_processes( pid ) := js;
          logs.warning( "pid" & strings.image( pid ) & " - " & item.title & " - overdue" );
       end if;
    when repeat =>
       if item.schedule_number = schedule_number then
          logs.ok( "pid" & strings.image( pid ) & " - " & item.title & " - exec" );
          -- the next run is rescheduled by the priority

          if item.priority = 0 then
             item.schedule_number := schedule_number + 1;
          else
             item.schedule_number := schedule_number + item.priority;
          end if;
          records.to_json( js, item );
          work_processes( pid ) := js;
          logs.info( "pid" & strings.image( pid ) & " - " & item.title & " - rescheduled" );
       end if;
    when others => null;
    end case;
  end exec_work;


-- SCHEDULER

  schedule_number : a_schedule_number := 1;
  -- the iteration of the scheduler

  current_process : a_process_id := 1;
  -- the current process being checked

  stop_time : calendar.time;
  -- the time when this example should stop

  done : boolean := false;
  -- true if the scheduler should stop

  due_date_year : calendar.year_number;
  due_date_month : calendar.month_number;
  due_date_day : calendar.day_number;
  due_date_dur : calendar.day_duration;
  -- used for passing the due date to schedule_work

begin

  -- Initialize the scheduler

  reset_work;
  stop_time := calendar.clock + 30;

  -- Schedule some tasks

  calendar.split( calendar.clock + 5, due_date_year, due_date_month, due_date_day, due_date_dur );
  schedule_work( "run me once", 2, run_once, due_date_year, due_date_month, due_date_day, due_date_dur, schedule_number );

  calendar.split( calendar.clock + 7, due_date_year, due_date_month, due_date_day, due_date_dur );
  schedule_work( "run me late", 3, run_once, due_date_year, due_date_month, due_date_day, due_date_dur, schedule_number + 50 );

  calendar.split( calendar.clock + 7, due_date_year, due_date_month, due_date_day, due_date_dur );
  schedule_work( "run me again", 5, repeat, due_date_year, due_date_month, due_date_day, due_date_dur, schedule_number );

  -- Scheduler loop

  while not done loop

     -- At the start of the process table, wait for the schedule period
     -- then run the scheduler

     if current_process = min_process then
        wait_scheduler;
        logs.info( "running scheduler" );
     end if;

     -- Check and run the work task (if necessary)

     exec_work( current_process, schedule_number );

     -- Move on to the next work task in the process table

     if current_process = max_process then
        current_process := min_process;
        schedule_number := @+1;
     else
        current_process := @+1;
     end if;

     -- Check to see if this example is over

     if calendar.clock > stop_time then
        logs.info( "time is up - exiting" );
        done;
     end if;
  end loop;

end scheduler;

-- VIM editor formatting instructions
-- vim: ft=spar

