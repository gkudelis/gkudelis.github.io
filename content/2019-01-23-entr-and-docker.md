+++
title = "Using entr and Docker together for development"
[taxonomies]
tags = ["ag", "entr", "docker"]
[extra]
updated = 2019-01-28
+++

Lately I've found myself using Docker more and more. There's something quite
intoxicating about quick and easy creation of identical environments and the
portability that comes from it. However, for me that often looks as vim in
one tmux pane and a shell in another where I run something like
```sh
docker run --rm -it $(docker build -q .)
```
then look at the running application, change something, switch back into the
shell window, kill Docker, run same command again. This is pretty tedious and
there's no reason it can't be automated.

Up until recently the only way to achieve this I knew of was the inotify system
and the incron service, which makes it easier to manage, especially if you're
familiar to setting up cron events. However, I've resently come across [a tool
called entr](http://eradman.com/entrproject/) that helps achieve a similar result, but without depending on the
inotify system, making it a cross-platform solution. In simple terms what entr
does is it takes a list of filenames via standard input and a command to run
as a command line argument. When it notices that any of the files has changed
it runs the given command. For example, if you're working on a Python project
and you want to clear the screen (passing the `-c` flag to entr makes it clear
the screen) and re-run tests any time you make changes you might run it as
```sh
ag -l | entr -c python -m unittest
```
Here, ag is responsible for finding interesting files for entr to watch. You
could use find, but ag pays attention to your .gitignore and has some other
useful defaults. Note however that this wouldn't detect and start tracking
files created after starting the command. This can be changed by using the `-d`
flag of entr, which makes entr watch any directories containing the files it's
tracking and exit when it detects there are new files. This is intentional as
it's not the responsibility of entr to figure out which files it should track.
The intended way to use this functionality is by wrapping both commands in a
shell while-true loop, this way the file listing command is used to tell entr
which of the new files it's supposed to be tracking.
```sh
while sleep 1; do
ag -l | entr -cd python -m unittest
done
```
So can we use a similar setup to build and run a Docker container? Of course!
If your container runs some tests and exits or does some other task that
quickly finishes the only change you have to do is wrap the `docker` command in
an `sh -c` or if you're using an up to date version of entr you can use the
`-s` flag which effectively does the same (my distribution shipped version 3.4
and the `-s` flag was not available).
```sh
while sleep 1; do
ag -l | entr -cds 'docker run --rm $(docker build -q .)'
done
```
Note that whether you're using the `-s` flag or the `sh -c` technique the
command now needs to be quoted to avoid premature shell variable expansion.

However, if you plan to run a development server this way the workflow is a
little different - now entr should ask the Docker container to terminate, wait
until that's done and only then build and run a new container. Luckily, entr
has the `-r` flag that it send a signal to the proccess it started, wait for the
process to terminate and only then start a new one. Though if you simply add
the flag to the above command you'll run into issues - the container will
refuse to stop. It's not an issue with entr or Docker, it's simply an outcome
of how the Linux kernel treats signals sent to processes. For regular processes
if you don't supply a signal handler you basically get a default one assigned
and as a developer you don't need to worry about correctly handling all the
different signals your application might receive. Normally these default signal
handlers do the right thing. However, the way the process with PID 1 is treated
is a little different - the default handlers for the "please exit" signals
don't actually terminate the process. If you want your process to listen to
these signals and exit you have to implement these handlers yourself. Of
course, that's a bit of a hassle, so since Docker version 1.13 it ships with a
small and simple init manager (originally called tini) that can be used to wrap
the application so that the init manager runs as PID 1 and executes your
application as a different PID. It also knows how to handle different signals,
making it behave as entr would expect. To run your application in a container
using the init manager you can use
```sh
while sleep 1; do
ag -l | entr -cdrs 'docker run --rm --init $(docker build -q .)'
done
```
This allows you to have your application built and executed in a container
whenever any changes are made without the developer having to restart it
manually making it a very nice and slick development experience.

One issue I've noticed while using this with Python was that I wasn't getting
any output from the container and since it's a development environment that's
often what you want. Turns out the issue in my case was simply that Python
output is buffered by default. Normally you would run Docker with the `-it`
flag to have a pseudo-TTY attached and this debuffers the output, but this will
interfere with entr. The solution is simply to pass the `-u` flag to Python in
the Dockerfile disabling the default buffering. The above command then works as
a charm!

I hope you found this post useful. If you have any questions or comments -
please let me know. I'd also like to thank the author of entr, Eric Radman, for
his feedback and help with making this post as useful and accurate as possible.
