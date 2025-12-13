{
  description = "Authentic terminal primitives enabling autonomous agents to operate interactive applications.";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAll = f: nixpkgs.lib.genAttrs systems (s: f (import nixpkgs {
        system = s;
        config.allowUnfreePredicate = p: nixpkgs.lib.getName p == "specter";
      }));
    in {
      packages = forAll (pkgs: {
        default = pkgs.haskell.lib.overrideCabal
          (pkgs.haskellPackages.callCabal2nix "specter" ./. {})
          (_: { license = pkgs.lib.licenses.cc-by-nc-sa-40; });
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
