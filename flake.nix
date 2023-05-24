# This file defines a Nix package definition ("flake") for Kons-9.
#
# Nix is a package manager that can install a broad range of software on Linux
# and macOS (but not Windows.)
#
# Nix can install Lisp packages from Quicklisp (and other sources) and
# automatically include their native dependencies (OpenGL libraries, etc).
#
# Nix installs all software in a separate /nix directory so that it doesn't
# interfere with other software on the machine.

{
  description = "Kons-9 3D IDE";
  inputs.nixpkgs.url = "nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Standard "nixpkgs" software distribution.
        #
        # Includes SBCL and a snapshot of Quicklisp projects (with extensions.)
        pkgs = nixpkgs.legacyPackages.${system};
        # Nix package definition for kons-9 and its dependencies.
        #
        # This definition also exists in the standard upstream nixpkgs
        # distribution but defining it separately here is handy for updates and
        # experiments during development.
        kons-9 = pkgs.sbcl.buildASDFSystem rec {
          pname = "kons-9";
          version = "trunk";
          src = ./.;
          systems = [ "kons-9" "kons-9/testsuite"
                      "kons-9/api-types" "kons-9/api-docs" ];
          lispLibs = with pkgs.sbcl.pkgs; [
            closer-mop trivial-main-thread trivial-backtrace cffi cl-opengl cl-glu
            cl-glfw3 cl-paths-ttf zpb-ttf cl-vectors origin clobber
            org_dot_melusina_dot_confidence
            mgl-pax
          ];
        };
        # Extended SBCL with kons-9 on the menu of available packages.
        sbcl' = pkgs.sbcl.withOverrides (self: super: {
          inherit kons-9;
        });
        # SBCL "distribution" including kons-9 and dependencies (both Lisp and native.)
        lisp = sbcl'.withPackages (ps: ps.kons-9);
        run-kons-9 = pkgs.lib.writeScript "kons-9" ''
          ${lisp}/bin/sbcl --eval '(kons-9::run)'
        '';
      in {
        apps.default = { type = "app"; program = }
      }
    );
}

