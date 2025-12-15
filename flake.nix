{
  description = "Authentic terminal primitives enabling autonomous agents to operate interactive applications.";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (s: f (import nixpkgs {
        system = s;
        config.allowUnfreePredicate = p: nixpkgs.lib.getName p == "specter";
      }));
    in {
      packages = forAll (pkgs: let sys = pkgs.stdenv.hostPlatform.system; specter = pkgs.haskell.lib.overrideCabal
          (pkgs.haskellPackages.callCabal2nix "specter" ./. {}) (_: { license = pkgs.lib.licenses.cc-by-nc-sa-40; }); in {
        default = specter;
        nethack = pkgs.writeShellScriptBin "specter-nethack" ''
          exec ${specter}/bin/specter ${pkgs.nethack}/bin/nethack "Play NetHack. You're seeing an authentic terminal - the same view a human would see. The @ is you. Top line shows messages (read them!), bottom two lines show your stats. Take your time, stay alive, descend when ready. Have fun exploring."
        '';
      });

      apps = forAll (pkgs: let sys = pkgs.stdenv.hostPlatform.system; in {
        nethack = { type = "app"; program = "${self.packages.${sys}.nethack}/bin/specter-nethack"; };
      });

      devShells = forAll (pkgs: let sys = pkgs.stdenv.hostPlatform.system; in {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ self.packages.${sys}.default ];
          buildInputs = [ pkgs.cabal-install pkgs.opencode ];
          shellHook = ''
            [ -f specter.cabal ] && cat > opencode.json <<EOF
            {"mcp":{"specter":{"type":"local","command":["${self.packages.${sys}.default}/bin/specter"]}}}
            EOF
          '';
        };
      });
    };
}
