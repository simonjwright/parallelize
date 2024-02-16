## Description ##

Since modern processors have multiple cores, it can make sense to split a job into parts which can be run in parallel on the different cores, using separate processes. Existing tools which support this include _gprbuild_ with its `-j<num>` switch (`-j0` means "use as many processors as there are cores") and _make_, which also uses `-j<num>`, but note that `-j` on its own lets _make_ "place no limit on the number of jobs that can run simultaneously", which can be a Bad Idea.

_parallelize_ has been built and tested on macOS.

It runs the jobs defined in standard input (one per line).

The switches are

  >`-v`, `--verbose`: report as each job is started and finished.
  >`-j<num>`, `--jobs=<num>`: the number of jobs to run in parallel (default = number of cores)

A job must be executable (a program or a shell script). If there's any shell substitution to do, you can say e.g. `sh <script> arg=$<env var>` (I haven't found a way to do this with programs).

## Example ##

Given a set of jobs in `simple.dat`,
```
sleep 5
foo
cat alire.toml
loc src/parallelize.adb
sh loc src/*.ad?
```
where [`loc`](#loc) is an executable script that reports the number of lines of Ada code in Ada source files, the run `bin/parallelize -v <simple.dat` produces the output
```none
$ bin/parallelize -v <simple.dat
read 'sleep 5'
read 'foo'
read 'cat alire.toml'
read 'loc src/parallelize.adb'
read 'sh loc src/*.ad?'
starting 'sleep 5'
'foo' not executable
starting 'cat alire.toml'
starting 'loc src/parallelize.adb'
starting 'sh loc src/*.ad?'
finished 'cat alire.toml'
finished 'sh loc src/*.ad?'
finished 'loc src/parallelize.adb'
finished 'sleep 5'
completed 'sleep 5'
name = "parallelize"
description = "Execute multiple commands in parallel"
version = "1.0.0"

authors = ["Simon Wright"]
maintainers = ["Simon Wright <simon@pushface.org>"]
maintainers-logins = ["simonjwright"]
licenses = "Apache-2.0"
website = ""
tags = ["parallel", "execution"]

executables = ["parallelize"]

[build-switches]
development.contracts = "yes"
completed 'cat alire.toml'
src/parallelize.adb     111
total loc 111
completed 'loc src/parallelize.adb'
src/commands.adb      13
src/commands.ads       3
src/parallelize.adb     111
total loc 127
completed 'sh loc src/*.ad?'
```
----
#### <a name="loc">LOC script</a> ####

```shell
#!/bin/bash -f
t=0
for f in $*; do
    n=`sed -e "s/\".*\"//" -e "s/--.*//" <$f | tr -Cd \; | wc -c`
    t=`expr $t + $n`
    echo "$f$n"
done
echo "total loc $t"
```
