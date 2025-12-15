{
  description = "NetHack demo for specter";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    specter.url = "../..";
  };

  outputs = { self, nixpkgs, specter }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (s: f (import nixpkgs { system = s; }) s);
    in {
      packages = forAll (pkgs: sys: {
        default = pkgs.writeShellScriptBin "specter-nethack" ''
          export PATH="${pkgs.nethack}/bin:$PATH"
          exec ${specter.packages.${sys}.default}/bin/specter "Play a session of NetHack (it's in your PATH). When your adventure ends, reflect on your experience and exit the shell."
        '';
      });

      devShells = forAll (pkgs: sys: {
        default = pkgs.mkShell {
          buildInputs = [ self.packages.${sys}.default pkgs.nethack specter.packages.${sys}.default ];
        };
      });
    };
}
