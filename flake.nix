# This file allows the Nix package manager to operate on Kons-9.
#
# This is primarily intended to support automated build/documentation workflows.
# You can use it manually if you want to, though.
#
# *** Quick usage for a local checkout ***
#
#   nix run .
#     run kons-9 built from local sources
#   nix run .#test
#     run kons-9 test suite
#   nix build .#api-html
#     build kons-9 API documentation in ./result/
#
# *** Running directly on sources in Github ***
#
#   nix run github:kaveh808/kons-9
#   nix run github:kaveh808/kons-9#test
#   nix build github:kaveh808/kons-9#api-html
#
# *** Prerequisites ***
#
#   1. Install the Nix package manager:
#      https://nixos.org/download.html
#   2. Enable features "flakes" and "nix-command":
#      https://nixos.wiki/wiki/Flakes#Enable_flakes

{
  description = "Kons-9 3D IDE";
  # nixpkgs branch including mgl-pax package fixes that are still being upstreamed.
  inputs.nixpkgs.url = "github:lukego/nixpkgs/mgl-pax";
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
        kons-9 = pkgs.sbcl.buildASDFSystem {
          pname = "kons-9";
          version = "dev";
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
        lisp = sbcl'.withPackages (ps: [ ps.kons-9 ]);
        # Script to run kons-9.
        run-kons-9 = pkgs.writeScriptBin "run-kons-9" ''
          ${lisp}/bin/sbcl --eval '(require :asdf)' \
                           --eval '(require :kons-9)' \
                           --eval '(kons-9::run)'
        '';
        # Script to run the kons-9 test suite.
        test-kons-9 = pkgs.writeScriptBin "test-kons-9" ''
          ${lisp}/bin/sbcl --non-interactive \
                           --eval '(require :asdf)' \
                           --eval '(require :kons-9/testsuite)' \
                           --eval '(kons-9/testsuite:run-all-tests)'
        '';
        # API documentation in HTML format (including CSS/JS.)
        api-html = pkgs.runCommand "api-html" {} ''
          mkdir $out
          ${lisp}/bin/sbcl --eval '(require :asdf)' \
                           --eval '(require :kons-9/api-docs)' \
                           --eval "(pax:update-asdf-system-html-docs kons-9::@api :kons-9 :target-dir \"$out/\")"
          cd $out
          ln -s api.html index.html
        '';
      in {
        apps.default = { type = "app"; program = "${run-kons-9}/bin/run-kons-9"; };
        apps.test    = { type = "app"; program = "${test-kons-9}/bin/test-kons-9"; };
        packages.api-html = api-html;
      }
    );
}

