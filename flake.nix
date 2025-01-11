{
  description = "A flake for rssbridge";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ flake-parts.flakeModules.easyOverlay ];
      systems = nixpkgs.lib.platforms.all;
      perSystem = { config, pkgs, lib, system, ... }:
        let
          ############ Settings ############
          ## Project name
          pname = "rssbridge";
          ## Source directory
          src = ./.;
          ## Dependencies
          lispLibs = lisp: with lisp.pkgs; [
	    # Utils
	    alexandria
	    cl-utilities
	    local-time
	    cl-ppcre

	    # Charsets
	    babel

	    # URL
	    quri

	    # JSON
	    shasht

	    # Websocket
	    websocket-driver-client

	    # KV Store
	    # cl-store

	    # HTTP Request
	    dexador

	    # cryptography
	    ironclad
	    
            arrows

            # rove 
	    fiveam

	    slynk
	    swank
	  ];

	  # devLibs = lisp: with lisp.pkgs; [ 
	  #   slynk
	  #   swank
	  # ];

          ## Non-Lisp dependencies
          nativeLibs = with pkgs; [ ];
          ## Supported Lisp implementations
          lispImpls = [
            "sbcl"
            "ecl"
            "ccl"
            # "abcl"
            # "mkcl"
            # "clisp"
            # "cmucl_binary"
            # "clasp-common-lisp"
          ];
          ##################################
          systems = let
            asd = builtins.readFile "${src}/${pname}.asd";
            res = builtins.split ''defsystem[[:space:]]*([^[:space:]]*)'' asd;
            odd = n: lib.trivial.mod n 2 == 1;
            sys1 = lib.lists.flatten (lib.lists.ifilter0 (i: v: odd i) res);
            sys2 = builtins.map (s: builtins.replaceStrings [''"'' "#:" ":"] ["" "" ""] s) sys1;
          in sys2;
          versions = let
            asd = builtins.readFile "${src}/${pname}.asd";
            res = builtins.split '':version[[:space:]]*([^[:space:]]*)'' asd;
            odd = n: lib.trivial.mod n 2 == 1;
            ver1 = lib.lists.flatten (lib.lists.ifilter0 (i: v: odd i) res);
            ver2 = builtins.map (s: builtins.replaceStrings [''"''] [""] s) ver1;
          in ver2;
          version = builtins.head versions;
          isAvailable = impl:
            let lisp = pkgs.${impl};
            in (builtins.tryEval lisp).success
            && (builtins.elem system lisp.meta.platforms)
            && (!lisp.meta.broken);
          availableLispImpls = builtins.filter isAvailable lispImpls;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeLibs;
          unbundledPackage = { lisp, evalFlag, extraArgs }: rec {
            mainLib = lisp.buildASDFSystem {
              inherit pname version src systems nativeLibs;
              lispLibs = lispLibs lisp;
            };
            lisp' = lisp.withPackages (ps: [ mainLib ]) // {
              inherit (lisp) meta;
            };
            mainExe = pkgs.writeShellScriptBin pname ''
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${lisp'}/bin/${lisp'.meta.mainProgram} ${extraArgs} ${evalFlag} '(require "asdf")' ${evalFlag} "$(cat <<EOF
                (let* ((_ (asdf:load-system :${pname}))
                       (component (asdf:find-system :${pname}))
                       (entry-point (asdf/system:component-entry-point component))
                       (function (uiop:ensure-function entry-point)))
                  (funcall function)
                  (quit))
              EOF
              )" -- "$@"
            '';
            testExe = pkgs.writeShellScriptBin "${pname}-test" ''
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${lisp'}/bin/${lisp'.meta.mainProgram} ${extraArgs} ${evalFlag} '(require "asdf")' ${evalFlag} "$(cat <<EOF
                (progn
                  (asdf:test-system :${pname})
                  (quit))
              EOF
              )" -- "$@"
            '';
          };
          bundledPackage = { lisp, evalFlag, extraArgs }: rec {
            mainLib = lisp.buildASDFSystem {
              inherit pname version src systems nativeLibs;
              lispLibs = lispLibs lisp;
            };
            lisp' = lisp.withPackages (ps: [ mainLib ]) // {
              inherit (lisp) meta;
            };
            mainRaw = pkgs.stdenv.mkDerivation {
              inherit pname version src;
              meta.mainProgram = pname;
              dontStrip = true;
              installPhase = ''
                export HOME=$TMPDIR
                export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                ${lisp'}/bin/${lisp'.meta.mainProgram} ${extraArgs} ${evalFlag} '(require "asdf")' ${evalFlag} "$(cat <<EOF
                  (let ((system (asdf:find-system :${pname})))
                    (setf (asdf/system:component-build-pathname system) #p"$out/bin/${pname}")
                    (asdf:make :${pname})
                    (quit))
                EOF
                )" -- "$@"
              '';
            };
            mainExe = pkgs.writeShellScriptBin pname ''
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              exec ${mainRaw}/bin/${pname} "$@"
            '';
            testExe = pkgs.writeShellScriptBin "${pname}-test" ''
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${lisp'}/bin/${lisp'.meta.mainProgram} ${extraArgs} ${evalFlag} '(require "asdf")' ${evalFlag} "$(cat <<EOF
                (progn
                  (asdf:test-system :${pname})
                  (quit))
              EOF
              )" -- "$@"
            '';
          };
          coverage-sbcl = let
            lisp = pkgs.sbcl.withPackages (ps: lispLibs pkgs.sbcl) // {
              inherit (pkgs.sbcl) meta;
            };
            program = pkgs.writeShellScriptBin "${pname}-coverage" ''
              export CL_SOURCE_REGISTRY=$PWD
              ${lisp}/bin/${lisp.meta.mainProgram} --noinform --disable-debugger <<EOF
                (require "asdf")
                (require :sb-cover)
                (declaim (optimize sb-cover:store-coverage-data))
                (asdf:compile-system :rssbridge :force t)
                (declaim (optimize (sb-cover:store-coverage-data 0)))
                (asdf:test-system :${pname})
                (sb-cover:report "coverage/")
              EOF
            '';
          in {
            type = "app";
            inherit program;
          };
          recipe = {
            sbcl = bundledPackage {
              lisp = pkgs.sbcl;
              evalFlag = "--eval";
              extraArgs = "--noinform --disable-debugger";
            };
            ccl = bundledPackage {
              lisp = pkgs.ccl;
              evalFlag = "--eval";
              extraArgs = "--quiet";
            };
            ecl = unbundledPackage {
              lisp = pkgs.ecl;
              evalFlag = "--eval";
              extraArgs = "";
            };
            # clisp = unbundledPackage {
            #   lisp = pkgs.clisp;
            #   evalFlag = "-x";
            #   extraArgs = "--quiet";
            # };
            # cmucl_binary = unbundledPackage {
            #   lisp = pkgs.cmucl_binary;
            #   evalFlag = "-eval";
            #   extraArgs = "-quiet";
            # };
            # abcl = unbundledPackage {
            #   lisp = pkgs.abcl;
            #   evalFlag = "--eval";
            #   extraArgs = "--noinform";
            # };
            # clasp-common-lisp = unbundledPackage {
            #   lisp = pkgs.clasp-common-lisp;
            #   evalFlag = "--eval";
            #   extraArgs = "--noinform";
            # };
            # mkcl = unbundledPackage {
            #   lisp = pkgs.mkcl;
            #   evalFlag = "-eval";
            #   extraArgs = "--quiet";
            # };
          };
          apps = impl: [
            {
              name = "main-" + impl;
              value = {
                type = "app";
                program = recipe.${impl}.mainExe;
              };
            }
            {
              name = "test-" + impl;
              value = {
                type = "app";
                program = recipe.${impl}.testExe;
              };
            }
          ];

          run-slynk = pkgs.writeShellScriptBin "run-slynk" ''
            sbcl --dynamic-space-size 1024 --eval "(require :asdf)" \
	                     --eval "(asdf:load-system :slynk)" \
	                     --eval "(slynk:create-server :dont-close t :style :spawn)"
          '';
          run-swank = pkgs.writeShellScriptBin "run-swank" ''
            sbcl --dynamic-space-size 1024 --eval "(require :asdf)" \
	                     --eval "(asdf:load-system :swank)" \
	                     --eval "(swank:create-server :dont-close t :style :spawn)"
          '';

          packages = impl: [
            {
              name = "main-" + impl;
              value = recipe.${impl}.mainExe;
            }
            {
              name = "lib-" + impl;
              value = recipe.${impl}.mainLib;
            }
          ];

          devPackages = impl:
            pkgs.${impl}.withPackages (ps: lispLibs pkgs.${impl});
          overlays = impl: {
            ${impl} = pkgs.${impl}.withOverrides
              (self: super: { ${pname} = config.packages."lib-${impl}"; });
            "${pname}-${impl}" = config.packages."main-${impl}";
          };
        in {
          overlayAttrs =
            builtins.listToAttrs (builtins.map overlays availableLispImpls);
          devShells.default = pkgs.mkShell {
            inherit LD_LIBRARY_PATH;
            shellHook = ''
              export CL_SOURCE_REGISTRY=$PWD
            '';
            packages = builtins.map devPackages availableLispImpls ++ [ pkgs.python3 ];
          };
          packages = builtins.listToAttrs
            (builtins.concatMap packages availableLispImpls);
          apps =
            builtins.listToAttrs (builtins.concatMap apps availableLispImpls)
            // lib.optionals (isAvailable "sbcl") {
              coverage-sbcl = coverage-sbcl;
            };
        };
    };
}
