{
  description = "Authentic terminal primitives enabling autonomous agents to operate interactive applications.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    opencode.url = "github:ada-yang-dev/opencode";
  };

  outputs = { self, nixpkgs, opencode }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (s: f (import nixpkgs {
        system = s;
        config.allowUnfreePredicate = p: nixpkgs.lib.getName p == "specter";
      }));
    in {
      packages = forAll (pkgs: let 
        sys = pkgs.stdenv.hostPlatform.system;
        specter = pkgs.haskell.lib.overrideCabal
          (pkgs.haskellPackages.callCabal2nix "specter" ./. {}) (_: { license = pkgs.lib.licenses.cc-by-nc-sa-40; });
        oc = opencode.packages.${sys}.default;
      in {
        default = pkgs.writeShellScriptBin "specter" ''
          dir=$(mktemp -d)
          cat > "$dir/opencode.json" <<CONF
          {
            "mcp":{"specter":{"type":"local","command":["${specter}/bin/specter"]}},
            "permission":{"edit":"allow","bash":{"*":"allow"},"mcp":{"*":"allow"}}
          }
          CONF
          cd "$dir" && exec ${oc}/bin/opencode "$@"
        '';
        nethack = pkgs.writeShellScriptBin "specter-nethack" ''
          dir=$(mktemp -d)
          cat > "$dir/opencode.json" <<CONF
          {
            "mcp":{"specter":{"type":"local","command":["${specter}/bin/specter","${pkgs.nethack}/bin/nethack"]}},
            "tools":{"*":false,"specter_read":true,"specter_write":true}
          }
          CONF
          cd "$dir" && exec ${oc}/bin/opencode "$@"
        '';
        mcp = specter;
        opencode = oc;
      });

      devShells = forAll (pkgs: let sys = pkgs.stdenv.hostPlatform.system; in {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ self.packages.${sys}.mcp ];
          buildInputs = [ pkgs.cabal-install self.packages.${sys}.opencode ];
          shellHook = ''
            cat > opencode.json <<EOF
            {"mcp":{"specter":{"type":"local","command":["${self.packages.${sys}.mcp}/bin/specter"]}}}
            EOF
          '';
        };
      });
    };
}
