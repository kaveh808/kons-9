{ pkgs ? (import <nixpkgs> {}) }:

# Define a lisp distribution with all the libraries that we depend on
# (including their foreign dependencies e.g. OpenGL libraries.)
let
  # sbcl custom initialization
  sbcl-initrc = pkgs.writeText "sbcl-initrc.lisp"
  ''
    ;; CL-OpenGL default behaviour is to make prohibitively expensive
    ;; calls to mask floating point traps.
    ;; These are already set globally in kons-9 and redundant.
    ;; See: https://github.com/3b/cl-opengl#readme
    (pushnew :cl-opengl-no-masked-traps *features*)
    (pushnew :cl-opengl-no-check-error *features*)
  '';
  # sbcl customized startup script
  sbcl-script = "${pkgs.sbcl}/bin/sbcl --load ${sbcl-initrc} --script";
  # sbcl customized distribution with dependencies
  sbcl = pkgs.lispPackages_new.lispWithPackages sbcl-script (p:
    with p; [ closer-mop
              trivial-main-thread
              trivial-backtrace
              cffi
              cl-opengl
              cl-glu
              cl-glfw3
              cl-vulkan
            ]);
in

# Define a development environment that auto-starts this Lisp.
pkgs.mkShell {
  nativeBuildInputs = [ sbcl ];
  shellHook = ''
    # Start SBCL ready to run:
    #   (require :kons-9)
    exec sbcl --load ${sbcl-initrc} \
              --eval "(require 'asdf)" \
              --load "kons-9.asd"
    # (comment line above to get a bash shell with this sbcl in path.)
  '';
}
