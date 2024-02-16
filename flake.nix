{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      package = "vmensa";
      system  = "x86_64-linux";
      pkgs    = nixpkgs.legacyPackages.${system};
      overlay = final: prev: {
        main = prev.callCabal2nix package ./. { };
        cmdline-util = prev.callCabal2nix "cmdline-util" (builtins.fetchGit {
          url = "https://github.com/slotThe/cmdline-util";
          rev = "c5d6e3832b38769be649b8dae2dd8f4659ead577";
        }) { };
      };
      haskellPackages = pkgs.haskellPackages.extend overlay;
    in {
      # nix build
      packages.${system}.default = haskellPackages.main;

      # nix develop
      devShells.${system}.default = haskellPackages.shellFor {
        packages = p: [ p.main ];
        buildInputs = with haskellPackages; [
          cabal-install
          haskell-language-server
          hpack
        ];
      };
    };
}
