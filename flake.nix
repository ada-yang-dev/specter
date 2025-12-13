{
  description = "Authentic terminal primitives enabling autonomous agents to operate interactive applications.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [ "specter" "claude-code" ];
        };
        haskellPackages = pkgs.haskellPackages;

        specter = pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "specter" ./. {}) (_: {
          license = pkgs.lib.licenses.cc-by-nc-sa-40;
        });
      in {
        packages = {
          default = specter;
          specter = specter;
        };

        devShells = {
          default = haskellPackages.shellFor {
            packages = p: [ specter ];
            buildInputs = with haskellPackages; [
              cabal-install
              ghcid
              haskell-language-server
              hlint
              ormolu
            ];
          };

          demo = pkgs.mkShell {
            buildInputs = [ specter pkgs.claude-code ];
            shellHook = ''
              claude mcp add --scope project --transport stdio specter -- ${specter}/bin/specter 2>/dev/null || true
              echo "Specter MCP configured. Run 'claude' to start."
            '';
          };
        };

        apps = {
          default = {
            type = "app";
            program = "${specter}/bin/specter";
          };
          daemon = {
            type = "app";
            program = "${specter}/bin/specterd";
          };
        };
      }
    );
}
