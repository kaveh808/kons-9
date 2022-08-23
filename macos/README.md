# kons-9
## Common Lisp 3D Graphics System

## What kons-9 Is

**kons-9** is a new 3D graphics and animation system written in Common Lisp. The intention is to develop a flexible and extensible system on which can be built a wide variety of application and domain specific tools and packages. The system is designed and implemented to allow exploratory development in a seamless way. All user extensions have the same power and functionality as the built-in code.

More info about the thoughts (and random musings) behind this project at my blog:

https://kaveh808.medium.com

Demo of an early version of the software:

https://youtu.be/NJe4isZ7NHI

## How To Join The kons-9 Team

**kons-9** is an open source project under the MIT license. We welcome those wishing to contribute their time and skills.

If you wish to do so, please:

- Watch the project.
- Turn on Notifications so you are aware of the Discussions postings.
- Read the Introductions thread in Discussions.
- Post your own introduction on the thread.
- Expect an invitation to join the team.

## How To Run kons-9

These are early days of this project, so bear with us as we find our feet.

There is no documentation beyond this file.

This code currently only runs in CCL on MacOS, though we are working on porting it to sbcl/Linux. We need developers to do ports for sbcl/Windows and sbcl/MacOS. The system currently used OpenGL as a graphics engine.

Download the code and load the local directory:

    (push (uiop:getcwd) ql:*local-project-directories*)

Load the system:

    (ql:quickload "kons-9")

Run the code:

    (in-package :kons-9)
    (run)

You should see a graphics window appear.

Type 'h' in the window to see the available key bindings.

Open `demo.lisp` and start evaluating the blocks of code. Things should appear in the graphics window.

Have fun.


