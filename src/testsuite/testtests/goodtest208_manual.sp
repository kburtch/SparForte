procedure good_manual_test_case is
   ken : limited teams.member;
begin
  pragma test_report( text );

  pragma manual_test(
    ken,                                -- owner
    "Login Test",                       -- name
    "Ensure that users can login",      -- objective
    "Attempt to log into the website",  -- description
    "QA",                               -- environment
    "Functional",                       -- category
    "The user should not be logged in", -- preconditions
      "1) Enter the user name.  " &     -- steps
      "2) Enter the password" &
      "3) User home page should appear",
    "Log out of the website",           -- postconditions
    work_measure.size,                  -- unit of work
    "s",                                -- work estimate
    work_priority.level,                -- unit of priority
    'h',                                -- priority
    "ID1234"                            -- user story
  );

  pragma manual_test_result(
    ken,                                -- the tester
    "Sept 12/20xx",                     -- the date
    "No notes",                         -- any notes
    "no screenshots",                   -- screenshots
    true,                               -- test result (pass = true)
    "DE1234"
  );
end good_manual_test_case;

-- vim: ft=spar

