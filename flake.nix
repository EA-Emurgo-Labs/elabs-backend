{
  description = "EMURGO Labs Backend";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" ];
    extra-substituters = [
      "https://ea-emurgo-labs.cachix.org"
    ];
    extra-trusted-public-keys = [
      "ea-emurgo-labs.cachix.org-1:C4qCB9BfOH+otex5Mr5YclmyXer66BA7Oe4H1BmOzII="
    ];
    allow-import-from-derivation = "true";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.CHaP = {
    url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, CHaP }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = system;
                inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake { };
      in
      flake // {
        legacyPackages = pkgs;
      });
}
