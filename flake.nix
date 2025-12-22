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
        specter = pkgs.haskell.lib.overrideCabal
          (pkgs.haskellPackages.callCabal2nix "specter" ./. {}) (_: { license = pkgs.lib.licenses.cc-by-nc-sa-40; });
        opencode = pkgs.writeShellApplication {
          name = "opencode";
          text = ''
            ${pkgs.opencode}/bin/opencode run --format json "$@" | ${pkgs.jq}/bin/jq -n --unbuffered -rj '
              def pink: "\u001b[38;5;205m"; def rst: "\u001b[0m";
              def tool: pink + .part.tool + ((.part.state.input | if . == {} then "" else ": \(.)" end)) + rst +
                        ((.part.state.output // "") | if . == "" or startswith("\n") then . else "\n\(.)" end) + "\n";
              foreach inputs as $x ({nl:false};
                if $x.type == "text" then {nl:true, out:$x.part.text}
                elif $x.type == "tool_use" then {nl:false, out:((if .nl then "\n" else "" end) + ($x|tool))}
                else . end;
                .out // empty)
            '
          '';
        };
        mkSpecter = name: packages: let
          shell = pkgs.writeShellScript "${name}-shell" ''
            export PATH="${pkgs.lib.makeBinPath (packages ++ [ pkgs.coreutils ])}"
            exec ${pkgs.fish}/bin/fish
          '';
        in pkgs.writeShellScriptBin "specter-${name}" ''
          dir=$(${pkgs.coreutils}/bin/mktemp -d)
          export HOME="$dir" XDG_CONFIG_HOME="$dir"
          ${pkgs.coreutils}/bin/cat > "$dir/opencode.json" <<CONF
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
