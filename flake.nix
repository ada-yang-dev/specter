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
          export PATH="${pkgs.nethack}/bin:$PATH"
          exec ${specter}/bin/specter "Play a session of NetHack (it's in your PATH). When your adventure ends, reflect on your experience and exit the shell."
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
