{
  description = "Feed reader for Emacs backed by SQLite";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      # synaxis declares (keymap-popup "0.2.1") as its minimum; pin a
      # concrete release here so the test environment is reproducible.
      keymapPopupVersion = "0.4.0";

      # Build everything for one concrete Emacs.  Called once per
      # variant (the full build and emacs-nox) so the test matrix can
      # exercise both the GUI build and the headless build Debian ships.
      mkVariant = pkgs: emacs:
        let
          lib = pkgs.lib;
          emacsPackages = pkgs.emacsPackagesFor emacs;

          source = lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              let name = baseNameOf path;
              in !(name == ".test-results"
                   || lib.hasSuffix ".elc" name
                   || lib.hasSuffix "~" name);
          };

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = pkgs.fetchurl {
              url = "https://elpa.gnu.org/packages/keymap-popup-${keymapPopupVersion}.tar";
              hash = "sha256-ZySAozyALV4fSfqNtFd3YOtW7ZBSFpCr+hAdnPm9v0E=";
            };
            packageRequires = [ ];
          };

          emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
            keymapPopup
            epkgs.package-lint
            epkgs.relint
          ]);

          # The byte-compiled package, built straight from the working
          # tree.  Doubles as a compile check: a warning-free build of
          # every lisp/ file under the pinned Emacs.
          synaxis = emacsPackages.trivialBuild {
            pname = "synaxis";
            version = "0.1.0";
            src = lib.cleanSource ./lisp;
            packageRequires = [ keymapPopup ];
          };

          # Run a Makefile test target in a sandbox that mirrors a
          # clean buildd: empty HOME/XDG, keymap-popup from the pinned
          # Emacs, the suite already wrapped so `make' does not re-enter
          # `nix develop'.
          mkTests = { pname, target }: pkgs.stdenv.mkDerivation {
            inherit pname;
            version = "0.1.0";
            src = source;
            nativeBuildInputs = [ emacsWithPackages pkgs.gnumake ];
            dontConfigure = true;

            buildPhase = ''
              runHook preBuild
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/share"
              export XDG_STATE_HOME="$TMPDIR/state"
              mkdir -p "$HOME" "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" \
                "$XDG_DATA_HOME" "$XDG_STATE_HOME"
              EMACS_CMD=emacs SYNAXIS_ENV_WRAPPED=1 make ${target}
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              touch $out/tests-passed
              runHook postInstall
            '';
          };
        in {
          inherit emacs emacsWithPackages keymapPopup synaxis;
          # Per-file: one Emacs per test file (fast, good isolation).
          tests = mkTests { pname = "synaxis-tests"; target = "test"; };
          # Combined: every file in one Emacs, suite run twice -- mirrors
          # dh_elpa_test and catches cross-test state pollution.
          testsOneshot = mkTests { pname = "synaxis-tests-oneshot"; target = "test-oneshot"; };
        };

      mkSynaxis = system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          inherit pkgs;
          full = mkVariant pkgs pkgs.emacs;
          # emacs-nox preloads fewer libraries and has no image support;
          # this is what Debian ships, so it catches headless-only bugs
          # the full build hides.
          nox = mkVariant pkgs pkgs.emacs-nox;
        };
    in {
      packages = forAllSystems (system:
        let s = mkSynaxis system;
        in {
          default = s.full.synaxis;
          synaxis = s.full.synaxis;
        });

      checks = forAllSystems (system:
        let s = mkSynaxis system;
        in {
          # Test matrix: {full, nox} x {per-file, combined-twice}.
          test = s.full.tests;
          test-nox = s.nox.tests;
          test-oneshot = s.full.testsOneshot;
          test-oneshot-nox = s.nox.testsOneshot;
        });

      devShells = forAllSystems (system:
        let s = mkSynaxis system;
        in {
          default = s.pkgs.mkShell {
            packages = with s.pkgs; [
              git
              gnumake
              s.full.emacsWithPackages
            ];

            shellHook = ''
              export EMACS_CMD=emacs
            '';
          };
        });
    };
}
