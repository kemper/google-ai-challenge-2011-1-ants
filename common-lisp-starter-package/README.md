# Common Lisp Starter Package for Ant Wars

See the section after "Google AI Challenge Blurb" for information
specific to this Common Lisp starter package.

## Dependencies

The bot depends on a couple of 3rd party libraries that should have
come with the starter package (in the `3rd-party` directory).


## Google AI Challenge Blurb

The files in this package are part of a starter package from the
Google AI Challenge. The Google AI Challenge is an Artificial
Intelligence programming contest. You can get more information by
visiting [www.ai-contest.com](http://www.ai-contest.com/).

The entire contents of this starter package are released under the
Apache license as is all code related to the Google AI Challenge. See
[code.google.com/p/ai-contest/](http://code.google.com/p/ai-contest/)
for more details.

There are a bunch of tutorials on the
[ai-contest.com](http://ai-contest.com/) website that tell you what to
do with the contents of this starter package. For the impatient, here
is a brief summary.

* In the root directory, there are a bunch of code files. These are a
  simple working contest entry that employs a basic strategy. These
  are meant to be used as a starting point for you to start writing
  your own entry.  Alternatively, you can just package up the starter
  package as-is and submit it on the website.

* The tools directory contains a game engine and visualizer. This is
  meant to be used to test your bot. See the relevant tutorials on the
  website for information about how to use the tools.

* The example_bots directory contains some sample bots for you to test
  your own bot against.


## Common Lisp Starter Package Specific Information

This rest of this file contains specific information about the Common
Lisp (CL) starter package for the [Ant Wars Google AI Challenge](http://ai-contest.com/).

It is assumed you are using [SBCL](http://www.sbcl.org/), since that
is what the challenge server will be using as well.  (Ubuntu Natty AMD64?)


## Status

This bot is still in development and has only been tested locally with
data on standard input.  Nevertheless, patches and improvements are
welcome.


## Usage

Run `bin/run-ants-bot.sbcl` and paste the sample input from the [Ants Game Specification](http://github.com/aichallenge/aichallenge/wiki/Ants-Game-Specification).

There are also a few initial unit tests which can be run by issuing
`bin/run-tests.sh`.

### Submission Errors

If SBCL does any output on standard error (stderr / \*error-output*)
it will count as a compilation error to the server.  So even innocuous
compiler notes or warnings will cause a compilation error.

If you're running into this, redirect \*error-output* to
\*standard-output* like so: `(setf *error-output* *standard-output*)`
at the top of MyBot.lisp.

**However**, this will also hide genuine compilation errors that would
otherwise be shown on your profile page!  So if your bot still fails
compiling on the server, your best best is resubmitting with the
redirection disabled.

In the near future the proper incantation to disable warnings and
notes on stderr will hopefully be added.


### Tools

*point out [tools](http://aichallengebeta.hypertriangle.com/using_the_tools.php)*

### CC=gcc

If running or compiling `proxy-bot` or `play-game` fails you need to
put `CC=gcc` in front of the command: `CC=gcc make proxy-bot`. Where
`gcc` is whatever C compiler you have installed on your system.

### play-game

*needs new description*

#### Why another game engine?

I'm planning to build a bot in this challenge using genetic
programming and for several reasons (all of which can be argued
against) I wanted a native game engine:

* ease of deployment on multiple servers
* ease of programming
* performance

There's another reason which is that I will probably be using a lot of
this engine's code for the next challenge's game suggestion.  I want
to have a running prototype of a game then.

### Windows / MSYS Note

You're probably best of putting a symbolic link `sbcl` in /usr/bin
pointing to wherever SBCL is installed on your system.


## Platforms

The code has been tested on the following platforms:

* x86: SBCL 1.0.45.debian
* x86: [Experimental SBCL 1.0.45 with threads](https://sites.google.com/site/dmitryvksite/sbcl-distr) for Windows using [MSYS](http://www.mingw.org/node/18)
* x86: SBCL 1.0.40 on an [Ubuntu 10.10 VirtualBox image](http://virtualboxes.org/images/ubuntu/)

### Windows Note

If you're running Windows it is assumed you are running [MSYS](http://www.mingw.org/node/18).

At the time of this writing if you're using Windows you *must* use the
Clon package that comes with this example bot since it contains some
fixes to make it work on SBCL on Windows.  You could ofcourse check to
see whether Clon has been updated in the meantime.
