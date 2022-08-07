# kons-9
Common Lisp 3D Graphics System

This code is definitely not ready for primetime. It is very much a work in progress and a playground for my ideas.

More info about the thoughts behind this project at my blog:

https://kaveh808.medium.com

Invitation to contribute to the project:

https://old.reddit.com/r/lisp/comments/weocc5/new_open_source_common_lisp_3d_graphics_project/

There is no documentation beyond this file.

This code currently only runs in CCL on MacOS.

Download the code and point your asdf to it:

    (push #p"<your directory>/kons-9/" asdf:*central-registry*)

Load the system:

    (ql:quickload "kons-9")

Run the code:

    (in-package :kons-9)
    (run)

You should see a graphics window appear.

Type 'h' in the window to see the available key bindings.

Open `demo.lisp` and start evaluating the blocks of code. Things should appear in the graphics window.

Have fun.


