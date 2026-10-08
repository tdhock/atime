## Bisect run
## If you have a script that can tell if the current source code is good
## or bad, you can bisect by issuing the command:
## $ git bisect run my_script arguments
## Note that the script (my_script in the above example) should exit with
## code 0 if the current source code is good/old, and exit with a code
## between 1 and 127 (inclusive), except 125, if the current source code
## is bad/new.
## The special exit code 125 should be used when the current source code
## cannot be tested. If the script exits with this code, the current
## revision will be skipped (see git bisect skip above).
## Bisect skip
## Instead of choosing a nearby commit by yourself, you can ask Git to do
## it for you by issuing the command:
## $ git bisect skip                 # Current version cannot be tested
## However, if you skip a commit adjacent to the one you are looking for,
## Git will be unable to tell exactly which of those commits was the first
## bad one.
