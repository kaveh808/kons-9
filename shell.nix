{ pkgs ? (import <nixpkgs> {}) }:

# Define a lisp distribution with all the libraries that we depend on
# (including their foreign dependencies e.g. OpenGL libraries.)
let lisp = pkgs.lispPackages_new.sbclWithPackages (p:
      with p; [ closer-mop
                trivial-main-thread
                trivial-backtrace
                cffi
                cl-opengl
                cl-glu
                cl-glfw3
              ]); in

# Define a development environment that auto-starts this Lisp.
pkgs.mkShell {
  nativeBuildInputs = [ lisp ];
  shellHook = ''
    # Start SBCL ready to run:
    #   (require :kons-9)
    exec sbcl --eval "(require 'asdf)" --load "kons-9.asd"
    # (comment line above to get a bash shell with this sbcl in path.)
  '';
}
