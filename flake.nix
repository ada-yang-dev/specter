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
      packages = forAll (pkgs: let 
        sys = pkgs.stdenv.hostPlatform.system;
        specter = pkgs.haskell.lib.overrideCabal
          (pkgs.haskellPackages.callCabal2nix "specter" ./. {}) (_: { license = pkgs.lib.licenses.cc-by-nc-sa-40; });
        opencode = pkgs.writeShellApplication {
          name = "opencode";
          runtimeInputs = [ pkgs.opencode pkgs.jq ];
          text = ''
            nl=
            ${pkgs.opencode}/bin/opencode run --format json "$@" | while IFS= read -r line; do
              case $(jq -r '.type' <<< "$line") in
                text) jq -rj '.part.text' <<< "$line"; nl=1 ;;
                tool_use)
                  [[ $nl ]] && echo; nl=
                  jq -r '"\u001b[38;5;205m\(.part.tool)\(if .part.state.input == {} then "" else ": \(.part.state.input)" end)\u001b[0m\(.part.state.output // "" | if startswith("\n") or . == "" then . else "\n\(.)" end)\n"' <<< "$line"
                  ;;
              esac
            done
          '';
        };
        mkSpecter = name: packages: let
          shell = pkgs.writeShellScript "${name}-shell" ''
            export PATH="${pkgs.lib.makeBinPath packages}:$PATH"
            exec ${pkgs.fish}/bin/fish
          '';
        in pkgs.writeShellScriptBin "specter-${name}" ''
          dir=$(mktemp -d)
          export HOME="$dir" XDG_CONFIG_HOME="$dir"
          cat > "$dir/opencode.json" <<CONF
          {"provider":{"anthropic":{"models":{"claude-opus-4-5":{"options":{"thinking":{"type":"enabled","budgetTokens":16000}}}}}},"mcp":{"specter":{"type":"local","command":["${specter}/bin/specter","${shell}"]}},"tools":{"*":false,"specter_read":true,"specter_write":true}}
          CONF
          cd "$dir" && exec ${opencode}/bin/opencode "$@"
        '';
      in {
        default = mkSpecter "default" [];
        nethack = mkSpecter "nethack" [ pkgs.nethack ];
        lynx = mkSpecter "lynx" [ pkgs.lynx ];
        vimgolf = mkSpecter "vimgolf" [ pkgs.vimgolf pkgs.vim ];
        mcp = specter;
        inherit opencode;
      });

      devShells = forAll (pkgs: let sys = pkgs.stdenv.hostPlatform.system; in {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ self.packages.${sys}.mcp ];
          buildInputs = [ pkgs.cabal-install ];
        };
      });
    };
}
