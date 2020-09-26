#!/usr/local/bin/spar

pragma annotate( summary, "git_prompt_script" );
pragma annotate( description, "This can be run from your SparForte profile with:" );
pragma annotate( description, "pragma prompt_script( `spar ~/bin/prompt_script.sp;` );" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

type profile_string is new string;

LOGNAME : constant profile_string := "unknown";
pragma unchecked_import( shell, LOGNAME );

HOSTNAME : constant profile_string := `hostname`;

pwd_tmp : string;
git_changes : string;
git_changes_staged : string;
git_branch  : string;

-- Blank line

new_line;

-- Current directory
--
-- Convert home directory to tilde to save space

pwd_tmp := PWD;
if strings.head( pwd_tmp, strings.length( HOME ) ) = HOME then
   pwd_tmp := strings.delete( @, 1, strings.length( HOME ) );
   pwd_tmp := "~" & pwd_tmp;
end if;
put(pwd_tmp);

-- Git Branch
--
-- If in a git project, show the branch.  Highlight the branch if it has
-- unsaved changes.

-- Git Branch
--
-- If in a git project, show the branch.  Highlight the branch if it has
-- unsaved changes.

git_branch := `git branch 2>&1`;
if $? = 0 then
  git_branch := `echo "$git_branch" | fgrep '*'`;
  git_branch := strings.delete( @, 1, 2 );
  git_changes := `git status | fgrep "not staged";`;
  git_changes_staged := git_changes & `git status | fgrep "to be committed";`;
  put( " [" );
  if git_changes /= "" then
     tput smso;
  elsif git_changes_staged /= "" then
     tput bold;
  end if;
  put( git_branch );
  if git_changes /= "" then
     tput rmso;
  elsif git_changes_staged /= "" then
     tput sgr0;
  end if;
  put( "]" );
end if;
new_line;

-- Identity and big arrow

put(LOGNAME) @ ('@') @ (HOSTNAME) @ (" => ");

-- VIM editor formatting instructions
-- vim: ft=spar

