# Run kons-9 with SBCL on macOS

2022-08-18 by mikel evins (mikel@evins.net)



Following are the steps that enable me to compile and load the **merge-sbcl-macos** branch of kons-9 under SBCL 2.2.6 with GLFW 3.3.8 on an Intel mac running macOS Monterey v. 12.5:

[These steps assume your Mac has Homebrew installed. For information about installing Homebrew, see https://brew.sh/. It also assumes that you have git, GNU Emacs, SLIME, and quicklisp installed and configured.]

1. Install GLFW;
   `brew install glfw`
   At the time of this writing, this command installs GLFW 3.3.8.
2. Install SBCL:
   `brew install sbcl`
   or see the SBCL download page at https://www.sbcl.org/platform-table.html.
3. Clone kons-9 into some convenient directory on your system, such as `~/lisp`.
   `git clone git@github.com:kaveh808/kons-9.git`
4. Using git, check out the **merge-sbcl-macos** branch:
   `cd kons-9`
   `git checkout merge-sbcl-macos`
5. Start sbcl from Emacs. If you've configured SLIME to start SBCL automatically, you can just do:
   `M-x slime`
   For more information about configuring SLIME, see the SLIME manual:
   https://slime.common-lisp.dev/doc/html/
   ... or the SLIME HOTO:
   https://www.cliki.net/slime-howto
6. From Emacs, open the file `kons-9/kons-9.asd`
7. Load the buffer; click anywhere inside the buffer and then use these Emacs key commands:
   `C-c C-l RET`
8. Load the kons-9 system; in the SLIME repl buffer, evaluate the following expression:
   `(asdf:load-system :kons-9)`
9. Show the kons-9 GLFW window:
   `(trivial-main-thread:call-in-main-thread
      (lambda ()
       (sb-int:set-floating-point-modes :traps nil)
       (kons-9::show-window kons-9::*scene*)))`
   [The two previous Lisp expressions are at the bottom of the kons-9.asd file. You can execute them by placing the cursor at the end of each expression and using the key command `C-x C-e`.]
10. Open the file `kons-9/demo.lisp` and evaluate the Lisp forms in it one-by-one. Each form should cause the GLFW window to render one or more figures. Some as-yet unresolved porting issues cause expressions past about halfway down the file to fail. We'll address these failures in due course.