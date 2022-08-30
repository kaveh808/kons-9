# Run kons-9 with SBCL on Windows 64-bit

2022-08-28 by mikel evins (mikel@evins.net)


Following are the steps that enable me to compile and load the **sbcl-win64-tests** branch of kons-9 under SBCL 2.1.0 with GLFW 3.3.8 on a Microsoft Surface Pro 8 running Windows 11 Pro, version 21H2.

The TL;DR version is that you just need to:

1. Install the 64-bit version of glfw
2. Push the directory containing the glfw DLL onto cffi:*foreign-library-directories*. For example, you can open kons-9.asd and find the following text near the top of the file:
```
#+(and windows x86-64) 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew "c:/Program Files/glfw-3.3.8/lib/"
           cffi:*foreign-library-directories*))

```
Change the pathname `"c:/Program Files/glfw-3.3.8/lib/"` to the path that contains the GLFW DLL you want to use.

3. Modify the definition of print-scene-view-help (in kons-9/src/graphics/glfw/minimal-ui.lisp) so that it doesn't try to use "~%~" to end lines in the message string; this does not work well across filesystems. For testing purposes, I simply commented out the FORMAT call with a "#-windows", which means that printing help doesn't currnetly work on Windows, for now.

Here's a fuller version of the explanation:

1. Install GLFW 3.3.8
   I downloaded glfw-3.3.8.bin.WIN64.zip from:
     https://sourceforge.net/projects/glfw/files/glfw/3.3.8/
   I unzipped it and copied the contents of the inner folder to
     C:\Program Files\glfw-3.3.8
   In other words, after I copied the files, the directory
     C:\Program Files\glfw-3.3.8
   contained subdirectories titled docs, include, and so on from the downloaded archive.

2. Install SBCL:
   see the SBCL download page at https://www.sbcl.org/platform-table.html.
   I used SBCL 2.1.0 for my tests.
3. Clone kons-9 into some convenient directory on your system.
   `git clone git@github.com:kaveh808/kons-9.git`
4. Using git, check out the **sbcl-win64-tests** branch:
   `cd kons-9`
   `git checkout sbcl-win64-tests`
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
```
   (trivial-main-thread:call-in-main-thread
      (lambda ()
       (sb-int:set-floating-point-modes :traps nil)
       (kons-9::show-window kons-9::*scene*)))
 ```
       
   [The two previous Lisp expressions are at the bottom of the kons-9.asd file. You can execute them by placing the cursor at the end of each expression and using the key command `C-x C-e`.]
   
10. Open the file `kons-9/test/demo-kernel.lisp` and evaluate the Lisp forms in it one-by-one. Each form should cause the GLFW window to render one or more figures. Some as-yet unresolved porting issues cause expressions past about halfway down the file to fail. We'll address these failures in due course.

