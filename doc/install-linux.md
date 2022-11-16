# Installing kons-9: Linux

This document describes how to install and configure kons-9 for local development and experimentation on Linux. These instructions assume you're using Ubuntu 20.04. Other Linux versions may require some changes to the installation.

## II. Prerequisites

This section describes the prerequisites for installing and configuring kons-9 on Ubuntu 20,04; in other words, it describes what should already be installed and configured before you start. Please check over the list of prerequisites and make sure they're all installed and properly configred before you try to install and use knos-9.

### git

These instructions assume that you have the git revision-control system installed and configured. In most cases, Ubuntu 20.04 installs git as part of the OS installation. You can test whether it's installed by opening a terminal window and typing

``` bash
$ git --version
```

If git is installed then you should see output similar to the following:

``` bash
git version 2.25.1
```

If instead you see a message telling you that git is not installed then you must install it before proceeding.

First, use apt to update your system's local package index:

``` bash
$ sudo apt update
```

Then use apt to install git:

``` bash
$ sudo apt install git
```

### SBCL

kons-9 is developed and tested with Stell Bank Common Lisp (SBCL). For best results working with it, install and configure a recent version of SBCL on your Linux system. These instructions were tested with SBCL version 2.0.1.debian, but any version of SBCL from 2.0 onward should work well.

You can install SBCL using apt. First, use apt to update your system's local package index:

``` bash
$ sudo apt update
```

Then use apt to install SBCL:

``` bash
$ sudo apt install sbcl
```

If you prefer, you can instead install SBCL by downloading a binary distribution from the SBCL 

[home page](https://www.sbcl.org/platform-table.html). Follow the link to the SBCL Downloads page and choose the binary for your operating system and hardware platform. Then follow the instructions on the [getting started page](https://www.sbcl.org/getting.html) to install the SBCL software.

To test whether the install succeeded, open a new terminal window and try running `sbcl`:

``` bash
$ sbcl
```

If the install was successful then you should see a message similar to the following:



``` bash
$ sbcl
This is SBCL 2.2.10, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* 

```

SBCL prints its banner message waits for you to enter expressions to evaluate. The asterisk ('*') is the SBCL prompt.

### quicklisp

kons-9 relies on [quicklisp](https://www.quicklisp.org/beta/) to find and load dependencies, and these instructions assume that you have it installed on your local system. The easiest way to install quicklisp is to follow the [installation instructions](https://www.quicklisp.org/beta/#installation) at the quicklisp home page.

### GNU Emacs

There are several editors that provide support for working with Common Lisp, but by far the most commonly-used one is GNU Emacs. These instructions assume that you have a recent version of GNU Emacs installed and configured to your liking.

Emacs may not necessarily be installed on your Ubuntu system by default, but it's easy to install it using apt.

First, use apt to update your system's local package index:

``` bash
$ sudo apt update
```

Then use apt to install GNU Emacs:

``` bash
$ sudo apt install emacs27
```

Here we assume you want to install `emacs27`, a recent version of GNU Emacs that isn't the very most recent one. To find out which versions of Emacs your system can install, first update the package index as shown above, then use apt-cache search to list the available versions of Emacs:

``` bash
$ sudo apt-cache search emacs
```

apt-cache responds with a list of installable packages it knows about that are related to Emacs. Choose the one you prefer and use apt to install it, as shown above for emacs27.

### SLIME

Most Lispers want more than just text-editing support for working with Lisp. The Superior Lisp Interaction Mode for Emacs (SLIME) is an Emacs package that provides comprehensive support for running a Common Lisp implementation, sending forms to it for evaluation, controlling its debugger, and generally making life easier for Lisp programmers.

The easiest way to set up SLIME so that it works with quicklisp is to use quicklisp to install it. These instructions assume that you followed the instructions for installing quicklisp, as we discussed above.

Open a terminal window and run sbcl:



``` bash
$ sbcl
This is SBCL 2.2.10, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* 

```

SBCL prints its banner and waits for input. Now use qucklisp to install **swank**, the Lisp side of the SLIME package:



``` lisp
* (ql:quickload :swank)
To load "swank":
  Load 1 ASDF system:
    swank
; Loading "swank"
.
(:SWANK)
*

```

SBCL fetches SLIME and loads the SWANK package.

SLIME consists of two parts:

1. The SLIME package loads in Emacs and provides a user interface for Lisp.
2. The SWANK package loads into your Common Lisp implementation and provides communication features that enable SLIME to talk to it and control it.



### GLFW

kons-9 currently depends on a development version of the GLFW library to create windows and render scenes. GLFW may not be installed on your Ubuntu system by default.

To install the needed version of GLFW using apt, first update your system's local package index:

``` bash
$ sudo apt update
```

Then use apt to install libglfw3-dev:

``` bash
$ sudo apt install libglfw3-dev
```

If apt signals that it has completed successfully then you're ready to install and configure kons-9.

## II. Installation and configuration

Open a terminal window and change to a directory where you want to keep the kons-9 project.

Then use git to clone the development repo into the chosen directory:

``` bash
$ git clone https://github.com/kaveh808/kons-9.git
```



## III. Smoke test

## IV. Contacts and sources of help
