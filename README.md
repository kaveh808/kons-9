# kons-9

## An IDE For 3D Production

Teaser trailer for the project:

https://www.youtube.com/watch?v=THMzaVDaZP8

**kons-9** is a new 3D computer graphics and animation software system being developed as an **open source project** under the MIT license. It broadly falls under the category of a **3D digital content creation tool**, while providing interesting and unique features not found in other systems. The intention is to develop a **flexible and extensible system** in which can be built a wide variety of application and domain specific tools and packages.

The unique differentiating aspect of **kons-9** is that it combines the power of a **software development IDE** with the visual tools of **3D graphics authoring system**. It does this by being implemented in **Common Lisp**, an **object-oriented dynamic language** which provides powerful facilities for **exploratory development and rapid prototyping** within a live interactive software environment.

This allows for **unlimited extensibility** of the system by both developers and users. In fact, this unique aspect of **kons-9** erases the distinction between developers and end users. Code developed within the kons-9 framework is always first class, not limited to some arbitrary API or scripting language. The source code for the system is available for extension, customization and exploration, and development in new directions, all within a **REPL-based** integrated development environment operating on a **live image of a 3D scene**.

Every user of **kons-9** has at their disposal the same full range of facilities and tools as the original developers of the system. Therefore code developed by them is first class in the sense that it has all the capabilities of code written by the developers of the system. There is no concept of a limited scripting language separate from the source code of the software.

Users of **kons-9** are able to modify 3D classes, subclass and extend them, and add new behaviors as desired. The system is **highly extensible and customizable** to suit different application domains and workflow needs.

**kons-9** can be used as a traditional **user interface driven 3D application** by general artists, or as a **REPL-based development environment** by technical artists and software developers. These two approaches can be seamlessly combined into a flexible and powerful workflow, where non-technical users can immediately benefit from software tools and extensions developed by technical users.

Developers work in a live image of their 3D scene, able to immediately see the results of their code in action. There is no need for a separate compile, link, and load process. Nor does the system have to be restarted to be updated. The REPL-based development experience is **highly interactive** with a **continuous and tight feedback loop**. Class and function definitions can be **modified on the fly** and the **results seen immediately** in the 3D scene. Incremental and exploratory development is facilitated and encouraged by the nature of the system.



Demo of an early version of the software:

https://youtu.be/NJe4isZ7NHI

## How To Join The kons-9 Team

**kons-9** is an open source project under the MIT license. We welcome those wishing to contribute their time and skills.

If you wish to do so, please:

- Watch the project.
- Turn on Notifications so you are aware of the Discussions postings.
- Read the Introductions thread in [Discussions](https://github.com/kaveh808/kons-9/discussions).
- Post your own introduction on the thread.
- Join the Discussions and look at the open Issues.

## How To Run kons-9

This code currently runs in SBCL on MacOS, Linux, and Windows. The system currently uses OpenGL as a graphics engine, though we are working on moving to Vulkan/Metal.

Download the code and load the local directory:

    (push (uiop:getcwd) ql:*local-project-directories*)

Load the system:

    (ql:quickload "kons-9")

Run the following code to open a 3D view window:

    (in-package :kons-9)
    (run)

Open `test/demo-kernel.lisp` and start evaluating the blocks of code for the demos. Things should appear in the graphics window. Try the other demo files as well.

Have fun.

## Run the Testsuite as a Batch Job

Use development script `development/testsuite` to run the testsuite as
a batch job. Specific tests can be requested by adding one argument to
the command line, such as

    development/testsuite exercise-clamp
    development/testsuite testsuite-utils

The following command lists all available tests

    development/testsuite list-all-available-tests

## Run the Testsuite from the REPL

Load the system:

    (ql:quickload "kons-9/testsuite")

List all available tests with

    (kons-9/testsuite:list-available-tests)

Tests are implemented as regular functions and can be run with
statements similar to

    (kons-9/testsuite:run-all-tests)

or

    (kons-9/testsuite:exercise-clamp)
    (kons-9/testsuite:testsuite-utils)

Users not familiar with [Confidence][confidence-home] may want to
review the [quick introduction to Confidence][confidence-intro].

  [confidence-home]: https://github.com/melusina-org/cl-confidence
  [confidence-intro]: https://github.com/melusina-org/cl-confidence/blob/main/example/example.lisp
