# SparForte README File

Ken O. Burtch is the author of "Linux Shell Scripting with Bash" and
former IT Architect with the "Webkinz" brand websites. With nearly 20
years experience in the IT business, including many years with
multi-billion dollar companies, Ken was concerned over hard to scale,
hard to maintain scripting languages, he created SparForte as a tool
to solve real business problems. Based a ISO standard proven effective
for large, mission-critical projects, SparForte is designed for fast
development of large projects while, at the same time, providing
easier maintenance and bug removal.


## Licensing

COPYING contains information about the GPL licence.
The SparForte documentation is located in the doc/ directory.

AdaCGI is released under the GPL licence.  The only changes were to
add the "-q" flag in the makefile for quieter makes.

APQ is released under the ACL or GPL licence.


## Installation

If you have downloaded the binary version, you should have the spar
binary and the SparForte manual in the doc directory and sample scripts in
the examples directory.

To install SparForte as your login shell, copy the "spar" file to the
/usr/local/bin directory (or type "make install" to do this for you).
Then change the /etc/passwd file accordingly.  Alternatively, change
/etc/shells to allow users to select spar as a shell with chsh.
You will need to be the superuser to copy or edit these files.

If you have downloaded the SparForte sources, the INSTALL file contains
instructions for compiling the sources.

"Have a lot of fun!"

