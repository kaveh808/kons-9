# Installing kons-9: Linux

This document describes how to install and configure kons-9 for local development and experimentation on Linux. These instructions assume you're using Ubuntu 20.04. Other Linux versions may require some changes to the installation procedure.

We assume that you're familiar with installing software on Ubuntu systems, and that you're comfortable with the Linux terminal and command-line tools.

## I. Prerequisites

This section describes the prerequisites for installing and configuring kons-9 on Ubuntu 20,04. In other words, it describes what should already be installed and configured before you start. Please check over the  prerequisites listed in this section and make sure they're all installed and  configured before you try to install and use kons-9.

### git

We assume that you have the git revision-control system installed and configured. In most cases, Ubuntu 20.04 installs git as part of installing the OS. You can test whether it's installed by opening a terminal window and typing

``` bash
$ git --version
```

If git is installed then you should see output similar to the following:

``` bash
git version 2.25.1
```

If instead you see a message telling you that git is not found or not installed then you must install it before proceeding.

First, use apt to update your system's local package index:

``` bash
$ sudo apt update
```

Then use apt to install git:

``` bash
$ sudo apt install git
```

### SBCL

kons-9 is developed and tested with Steel Bank Common Lisp (SBCL). For best results working with it, install and configure a recent version of SBCL. These instructions were tested with SBCL version 2.0.1.debian, but any version of SBCL from 2.0 onward should work well.

You can install SBCL using apt. First, use apt to update your system's local package index:

``` bash
$ sudo apt update
```

Then use apt to install SBCL:

``` bash
$ sudo apt install sbcl
```

If you prefer, you can instead install SBCL by downloading a binary distribution from the SBCL [downloads page](https://www.sbcl.org/platform-table.html). Follow the link and choose the binary for your operating system and hardware platform. Then follow the instructions on the [getting started page](https://www.sbcl.org/getting.html) to install the SBCL software.

To test whether the install succeeded, open a new terminal window and try running `sbcl`:

``` bash
$ sbcl
```

If the install was successful then you should see a message similar to the following:



``` bash
$ sbcl
This is SBCL 2.0.1.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* 

```

SBCL prints its banner message and then waits for you to enter expressions to evaluate. The asterisk ('*') is the SBCL prompt.

### GLFW

The kons-9 code relies on the GLFW library for rendering graphics to windows. To ensure that GLFW is installed and configured for development, use apt to install it.

First, use apt to update your system's local package index:

``` bash
$ sudo apt update
```

Then use apt to install the GLFW library:

``` bash
$ sudo apt install libglfw3
```

FInally, use apt to install GLFW's development-support files:

``` bash
$ sudo apt install libglfw3-dev
```



### quicklisp

kons-9 relies on [quicklisp](https://www.quicklisp.org/beta/) to find and load dependencies, and these instructions assume that you have it installed on your local system. The easiest way to install quicklisp is to follow the [installation instructions](https://www.quicklisp.org/beta/#installation) at the quicklisp home page.

### GNU Emacs

There are several editors that provide support for working with Common Lisp, but by far the most commonly-used one is GNU Emacs. We assume that you have a recent version of GNU Emacs installed and configured to your liking.

You can use a different editor to work with SBCL and kons-9, but some details of the process will differ from our description. Those details are outside the scope of this document. You might want to use GNU Emacs to get started, even if you intend to later use a different editor.

Emacs may not necessarily be installed on your Ubuntu system by default, but it's easy to install it using apt.

First, use apt to update your system's local package index:

``` bash
$ sudo apt update
```

Then use apt to install GNU Emacs:

``` bash
$ sudo apt install emacs27
```

Here we assume you want to install `emacs27`, a recent version of GNU Emacs, but not the  most recent one. To find out which versions of Emacs your system can install, first update the package index as shown above, then use apt-cache search to list the available versions of Emacs:

``` bash
$ sudo apt-cache search emacs
```

apt-cache responds with a list of installable packages it knows about that are related to Emacs. Choose the one you prefer and use apt to install it, as shown above for emacs27.

### SLIME

Most Lispers want more than just text-editing support for working with Lisp. The Superior Lisp Interaction Mode for Emacs (SLIME) is an Emacs package that provides comprehensive support for running a Common Lisp implementation, sending forms to it for evaluation, controlling its debugger, and generally making life easier for Lisp programmers. 

SLIME consists of two parts: (1) an Emacs package that provides a user interface inside the text editor; and (2) a Common Lisp package that provides features that Emacs uses to communicate with the Lisp system.

The easiest way to set up SLIME so that it works with quicklisp is to use quicklisp to install it. These instructions assume that you followed the instructions above for installing quicklisp.

Open a terminal window and run sbcl:

``` bash
$ sbcl
This is SBCL 2.0.1.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* 

```

SBCL prints its banner and waits for input. Now use quicklisp to install **swank**, the Lisp side of the SLIME package:



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

To use SLIME with GNU Emacs, you must first tell Emacs to load the SLIME package. Quicklisp installs the code that Emacs uses for this purpose, but you must tell Emacs where to find it. You can do that by adding a few lines of Lisp code to your Emacs init file.

Using your favorite editor, open or create the file `~/.emacs`. At the bottom of the file, add the following text:

```lisp
(add-to-list 'load-path "/home/<USERNAME>/quicklisp/dists/quicklisp/software/slime-v2.27")
(require 'slime-autoloads)
(setq inferior-lisp-program "/bin/sbcl")
```

Edit the first line to correct it for your system. Replace `<USERNAME>` with your username. Replace `slime-v2.27` with the name of the SLIME directory that quicklisp installed.

If you're installing on Ubuntu 20.04, and if you installed SBCL using apt, then the name of the SBCL executable in the last line should be correct. If you're using a different version of Linux, or if you installed SBCL manually to a different location, then you'll need to change that last line to reflect where the SBCL executable is.

Now run Emacs and confirm that installation and configuration were successfuly. From a terminal window, run Emacs:

``` bash
$ emacs
```

A new GNU Emacs window should appear. You should see the Emacs splash screen unless you've disabled it in your Emacs init file.

If there's a problem with your Emacs configuration, emacs pops up a buffer containing text that describes the problem and offers advice on how to debug it. If you see only the Emacs splash screen or an empty scratch buffer then Emacs is configured correctly.

To test whether SLIME is properly configured, type M-x sbcl RETURN (in other words, Press and release the ESC key, then type x; when Emacs prompts for a command in the minibuffer at the bottom of the window, type "sbcl" and then hit the Return key).

Emacs launches SBCL and creates a **repl buffer** that you can use to communicate with it. If SLIME is configured properly then you should see the SBCL prompt looking something like this:

```lisp
; SLIME 2.0.1
CL-USER> 
```

You're ready to install and run kons-9.



## II. Installation and configuration

If you successfully followed the instructions in the previous sections then you should be ready to install and use kons-9. 

Open a terminal window and change to a directory where you want to keep the kons-9 project.

Use git to clone the development repo into the chosen directory:

``` bash
$ git clone https://github.com/kaveh808/kons-9.git
```

Now run GNU Emacs:

``` bash
$ emacs
```

After the Emacs window appears, run SBCL from Emacs as we did in the previous section: type M-x sbcl RETURN (in other words, Press and release the ESC key, then type x; when Emacs prompts for a command in the minibuffer at the bottom of the window, type "sbcl" and then hit the Return key).

Emacs launches SBCL and creates a **repl buffer** that you can use to communicate with it.

Now open the file "kons-9.asd" in Emacs. Type C-x C-f RETURN to open Dired (Hold down the Control key and type 'x' 'f', then, when a prompt appears in the Emacs minibuffer, type Return). Emacs starts Dired, a mode for browsing and navigating directories. You can select a file or directory by using the arrow keys to move the text cursor up and down, and you can open it by hitting Return. Selecting and opening the directory at the top of the list named ".." navigates up to the enclosing directory.

Use Dired to navigate to the directory where you installed kons-9 and find the file named "kons-9.asd". Select that file in Dired and hit Return to open it.

You should now have an Emacs window with two text buffers displayed. The top buffer shows the contents of the "kons-9.asd" file; the bottom one shows the SLIME repl buffer for interacting with Lisp.

### Installing ASDF dependencies

kons-9 depends on several other Common Lisp libraries that are available from Quicklisp. To avoid confusing errors that can arise during loading, we'll install each of these dependencies individually before we try to load kons-9.

We'll need to do this only one time: the first time we load kons-9. After that firsst time, the library dependencies are available locally and Quicklisp will automatically load them as-needed.

First, let's see how to manually install a dependency from Quicklisp. Click anywhere in the Lisp repl buffer to make it active, then type the following expression at the Lisp prompt and hit Return:

```lisp
 CL-USER> (ql:quickload :closer-mop)
```

Quicklisp locates the `:closer-mop` library, downloading it if necessary, and loads it into the running Lisp. After it's loaded, it prints `(:CLOSER-MOP)` to the repl buffer to signal that it's now installed.

Now find the line in the "kons-9.asd" file that reads `:depends-on`. Following `:depends-on` is the list of libraries that kons-9 needs in order to work. To ensure that they're loaded, use `ql:quickload` as shown above to install all of the named libraries.

Alternatively, you could try just loading kons-9 without first loading all of the dependencies manually. ASDF will signal an error naming any library that it can't find on its own, and you can then use Quicklisp as shown above to manually install it and then try loading again. If you'd prefer to avoid the loading errors, though, your best course is to manually preload all of the dependencies. Remember: you'll need to do this only once. You won't have to repeat it once the libraries have been downloaded.

### Loading kons-9

You can load kons-9 into the running Lisp by first loading the "kons-9.asd" file, then evaluating a command to compile and load the kons-9 system.

kons-9 uses a library named ASDF to define Lisp **systems**. A **system** is a collection of Lisp source files and other assets that may be compiled and loaded into the running Lisp to add new features to it. The "kons-9.asd" defines a system that knows how to load and configure kons-9 for your use.

First, load the system definition. Click anywhere in the buffer that contains the "kons-9.asd" file to make that buffer active. Next, type C-x C-l RETURN to load the file into Lisp (Hold down the Control key and hit the 'X' key, then then 'L' key, then hit the Return key). Emacs sends the contents of the "kons-9.asd" file to SBCL for evaluation. After SBCL reads and evaluates the fle's contents, it displays T (Common Lisp's canonical true value) in the minibuffer.

The running Lisp now contains the definition of the kons-9 system. You can use ASDF commands to compile and load it.

Click anywhere in the repl buffer to make it active, then type the following expression at the Lisp prompt:

```lisp
 CL-USER> (asdf:load-system :kons-9)
```

Hit Return and SBCL runs the ASDF command, loading the files defined by the kons-9 system.

## III. Smoke test



## IV. Contacts and sources of help
