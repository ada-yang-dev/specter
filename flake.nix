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
            ${pkgs.opencode}/bin/opencode run --format json "$@" | while IFS= read -r line; do
              type=$(jq -r '.type' <<< "$line")
              case "$type" in
                text)
                  jq -r '.part.text' <<< "$line"
                  ;;
                tool_use)
                  tool=$(jq -r '.part.tool' <<< "$line")
                  args=$(jq -r '.part.state.input | if . == {} then "" else tostring end' <<< "$line")
                  if [[ -n "$args" ]]; then
                    printf '\e[48;5;229;30m %s: %s \e[0m\n' "$tool" "$args"
                  else
                    printf '\e[48;5;229;30m %s \e[0m\n' "$tool"
                  fi
                  printf '%s\n' "$(jq -r '.part.state.output // empty' <<< "$line")"
                  ;;
              esac
            done
          '';
        };
        mkSpecter = name: packages: let
          shell = pkgs.writeShellScript "${name}-shell" ''
            export PATH="${pkgs.lib.makeBinPath packages}:$PATH"
            exec ${pkgs.bash}/bin/bash
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
